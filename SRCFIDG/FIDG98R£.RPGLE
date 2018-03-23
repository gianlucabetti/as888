000100070322     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500070502      *                     PARTENZA GEO                              *
000600031017      *                                                               *
000700031017      *---------------------------------------------------------------*
000800031017
000900070323     ffidg98d   cf   e             workstn sfile(dg98s01:nrr1)
001000070424     fazorg01l  uf   e           k disk    usropn
001100070323     ffnarb54l  if   e           k disk
001200070413     ffnevb05l  if   e           k disk
001300070419     ffnevb22l  if a e           k disk
001400070323     ffiar401l  if a e           k disk
001500070612     f**!!!fiarg02l  uf   e           k disk
001600070328     ftabel00f  if   e           k disk
001700070424     ftntbe01l  uf a e           k disk
001800070323     ffiarn01l  o    e             disk
001900070323     ffidg98p   o    e             printer oflind(*in10) usropn
002000070323     ffidg98p1  o    e             printer oflind(*in11) usropn
002100070323     ffidg98p2  o    e             printer oflind(*in12) usropn
002200070323     ffidg98p3  o    e             printer oflind(*in13) usropn
002300070328     ffidg98p4  o    e             printer oflind(*in14) usropn
002400070413     ffidg98p5  o    e             printer oflind(*in15) usropn
002500070323     fqsysprt   o    f  132        printer oflind(*inof) usropn
002600070308
002700070308      *------------------------------------------------------------------------*
002800070308      * riepilogo indicatori
002900070308      *------------------------------------------------------------------------*
003000070424      * 01 - ambiente di prova
003100070502      * 02 - utente EDP
003200070323      * 10 - salto pagina
003300070323      * 11 - salto pagina 1
003400070323      * 12 - salto pagina 2
003500070323      * 13 - salto pagina 3
003600070413      * 14 - salto pagina 4
003700070413      * 15 - salto pagina 5
003800070308      * 20 - gestione subfile
003900070308      * 21 - gestione subfile
004000070308      * 28 - messaggio errore
004100070308      * 31 - fine subfile
004200070424      * 40 - filiale
004300070323      * 99 - salto pagina
004400031030
004500070308      *------------------------------------------------------------------------*
004600070308      * campi di work
004700070308      *------------------------------------------------------------------------*
004800070323     d command         s             80
004900070503     d errore          s             20
005000070329     d ilv80newproc    s              1    inz('S')
005100070424     d ktbecod         s                   like(tbecod)
005200070424     d ktbeke1         s                   like(tbeke1)
005300070323     d ktrc            s                   like(ar4trc) inz('L')
005400070323     d lung            s             15  5 inz(80)
005500070323     d nrr1            s                   like(recsf1)
005600070525     d sav_w1cfil      s                   like(w1cfil)
005700070328     d wb4lbasa        s                   like(§b4lbas)
005800070328     d wb4lbass        s                   like(§b4lbas)
005900070323     d wb4lbge         s                   like(§b4lbge)
006000070323     d wb4ltca         s                   like(§b4ltca)
006100070323     d wb4ltfd         s                   like(§b4ltfd)
006200070323     d wdcm            s                   like(arbdcm) inz
006300070323     d wdesblc         s             10
006400070508     d wflag           s              1    inz(*off)
006500050914     d wtot            s              7  0 inz
006600070323     d wtot1           s              7  0 inz
006700070323     d wtot2           s              7  0 inz
006800070323     d wtot3           s              7  0 inz
006900070328     d wtot4           s              7  0 inz
007000070413     d wtot5           s              7  0 inz
007100070308     d w0030           s                   like(orgfil)
007200070424     d w0080           s              8  0 inz
007300070308     d xx              s              2  0 inz
007400070328
007500070323      *------------------------------------------------------------------------*
007600070323      * schiere
007700070323      *------------------------------------------------------------------------*
007800070424     d cmd             s             80    dim(5)  ctdata perrcd(1)
007900031030
008000070308      *------------------------------------------------------------------------*
008100070308      * ds interne/esterne
008200070308      *------------------------------------------------------------------------*
008300070525     d wdatainv        ds
008400070525     d waainv                  1      4
008500070525     d wmminv                  5      6
008600070525     d wgginv                  7      8
008700070525
008800070525     d wdata           ds
008900070525     d wgg                     1      2
009000070525     d wmm                     3      4
009100070525     d waa                     5      8
009200070525
009300070424     d azuteds       e ds                  extname(azute00f)
009400070424     d ddatiute      e ds
009500070502     d dgeo          e ds
009600070323     d dsbl4l        e ds
009700070328     d ds3a          e ds
009800070323     d fidn12ds      e ds
009900070323     d  skkey                 26    305    dim(20)
010000070323     d  skdca                326    485  0 dim(20)
010100070424     d fnlv55ds      e ds
010200070328     d fnlv80ds      e ds
010300070308     d kpjba         e ds
010400070424     d og148         e ds
010500070424     d tibs02ds      e ds
010600070424     d tibs34ds      e ds
010700070308     d trul06ds      e ds                  inz
010800070308     d  lin                    1     90    dim(30)
010900070308
011000070424      *------------------------------------------------------------------------*
011100070424      * costanti
011200070424      *------------------------------------------------------------------------*
011300070426     d password        c                   const('GIANNI    ')
011400070424
011500070308      *------------------------------------------------------------------------*
011600070528
011700070528     c     inizio        tag
011800070424
011900070525     c                   exsr      sr_carvideo1
012000070525
012100070424     c                   exsr      sr_video1
012200070424
012300070424     c                   exsr      sr_video2
012400070308
012500070308     c     fine          tag
012600070328
012700070308     c                   eval      *inlr = *on
012800070525
012900070525      *------------------------------------------------------------------------*
013000070525      * carico i dati nella prima videata
013100070525      *------------------------------------------------------------------------*
013200070525     c     sr_carvideo1  begsr
013300070525
013400070525     c                   clear                   w1fas1a
013500070525     c                   clear                   w1fas1b
013600070525     c                   clear                   w1fas2
013700070528     c                   clear                   w1dta1a
013800070528     c                   clear                   w1dta1b
013900070528     c                   clear                   w1dta2
014000070525     c                   clear                   w1cpsw
014100070525
014200070525      * aggancio la tabella GEO della filiale utente
014300070525     c                   clear                   dgeo
014400070525     c                   clear                   tibs02ds
014500070525     c                   eval      t02mod = 'C'
014600070525     c                   eval      t02sif = knsif
014700070525     c                   eval      t02cod = 'GEO'
014800070525     c                   movel     w1cfil        t02ke1
014900070525     c                   call      'TIBS02R'
015000070525     c                   parm                    kpjba
015100070525     c                   parm                    tibs02ds
015200070525     c                   if        t02err = *blanks
015300070525     c                   eval      dgeo = t02uni
015400070525     c                   endif
015500070525      * data fase 1 una tantum
015600070525     c                   if        d§geofas1a > *zeros
015700070525     c                   eval      wdatainv = d§geofas1a
015800070525     c                   eval      waa = waainv
015900070525     c                   eval      wmm = wmminv
016000070525     c                   eval      wgg = wgginv
016100070525     c                   move      wdata         w1dta1a
016200070525     c                   endif
016300070525      * data fase 1 giornaliera
016400070525     c                   if        d§geofas1b > *zeros
016500070525     c                   eval      wdatainv = d§geofas1b
016600070525     c                   eval      waa = waainv
016700070525     c                   eval      wmm = wmminv
016800070525     c                   eval      wgg = wgginv
016900070525     c                   move      wdata         w1dta1b
017000070525     c                   endif
017100070525      * data fase 2 simualazione
017200070525     c                   if        d§geofas2 > *zeros
017300070525     c                   eval      wdatainv = d§geofas2
017400070525     c                   eval      waa = waainv
017500070525     c                   eval      wmm = wmminv
017600070525     c                   eval      wgg = wgginv
017700070525     c                   move      wdata         w1dta2
017800070525     c                   endif
017900070525
018000070525     c                   eval      sav_w1cfil = w1cfil
018100070525
018200070525     c                   endsr
018300070424
018400070424      *------------------------------------------------------------------------*
018500070424      * gestione prima videata
018600070424      *------------------------------------------------------------------------*
018700070424     c     sr_video1     begsr
018800070424
018900070424     c                   do        *hival
019000070424     c                   exfmt     dg98d01
019100070424     c                   eval      *in28 = *off
019200070424     c                   clear                   w1cmsg
019300070502     c                   clear                   h1colo
019400070502     c                   clear                   h1riga
019500070424
019600070424      * f3=fine
019700070424     c                   if        *inkc
019800070424     c                   goto      fine
019900070424     c                   endif
020000070424
020100070424      * controllo
020200070424     c                   exsr      sr_ctrd01
020300070424
020400070424     c                   if        *in28
020500070424     c                   iter
020600070424     c                   endif
020700070424
020800070424      * f6=conferma
020900070424     c                   if        *inkf
021000070424     c                   leave
021100070424     c                   endif
021200070424
021300070424     c                   enddo
021400070424
021500070424     c                   endsr
021600070308
021700070424      *------------------------------------------------------------------------*
021800070424      * gestione seconda videata
021900070424      *------------------------------------------------------------------------*
022000070424     c     sr_video2     begsr
022100070424
022200070424      * cerco le filiali gestite dalla filiale da elaborare
022300070424     c                   clear                   trul06ds
022400070424     c                   eval      d06cod = '£6'
022500070424     c                   movel     w1cfil        d06key
022600070424     c                   eval      d06tla = 'L'
022700070424     c                   eval      kpjbu = trul06ds
022800070424     c                   call      'TRUL06R'
022900070424     c                   parm                    kpjba
023000070424     c                   eval      trul06ds = kpjbu
023100070424
023200070424      * visualizzo le linee di arrivo da abilitare
023300070424     c                   exsr      sr_pulsfl
023400070424     c                   exsr      sr_carsfl
023500070424     c                   exsr      sr_emisfl
023600070424
023700070424     c                   endsr
023800070424
023900070424      *------------------------------------------------------------------------*
024000070424      * controllo la prima videata
024100070424      *------------------------------------------------------------------------*
024200070424     c     sr_ctrd01     begsr
024300070424
024400070424      * la filiale inserita deve essere una filiale valida
024500070424     c                   clear                   w1dfil
024600070424     c                   clear                   og148
024700070424     c     w1cfil        chain(n)  azorg01l
024800070424     c                   if        not %found(azorg01l) or orgfva <> *blanks or
024900070424     c                             (orgfag <> 'A' and orgfag <> 'F')
025000070424     c                   eval      w1cmsg = 'Filiale errata'
025100070424     c                   eval      *in28 = *on
025200070424     c                   eval      *in40 = *on
025300070424     c                   leavesr
025400070424     c                   endif
025500070502
025600070502     c                   eval      w1dfil= orgdes
025700070502     c                   eval      og148 = orgde8
025800070502
025900070502      * non deve essere una filiale gestita da altra filiale
026000070502     c                   clear                   fnlv55ds
026100070502     c                   movel     '6'           d55tpt
026200070502     c                   z-add     w1cfil        d55lin
026300070525     c                   z-add     *date         d55drf
026400070502     c                   call      'FNLV55R'
026500070502     c                   parm                    fnlv55ds
026600070502     c                   if        d55lin <> d55tfa
026700070502     c                   eval      w1cmsg = 'Attenzione!! Filiale gestita'
026800070502     c                   eval      *in28 = *on
026900070502     c                   eval      *in40 = *on
027000070502     c                   leavesr
027100070502     c                   endif
027200070502
027300070525      * se variata la filiale riemetto la videata
027400070525     c                   if        w1cfil <> sav_w1cfil
027500070525     c                   goto      inizio
027600070525     c                   endif
027700070503
027800070525      * posso richiedere una fase alla volta
027900070525     c                   if        w1fas1a = 'S' and w1fas1b = 'S' and
028000070525     c                             w1fas2  = 'S'
028100070525     c                   eval      w1cmsg = 'Attivare una FASE alla volta'
028200070503     c                   eval      *in28 = *on
028300070525     c                   eval      h1riga = 07
028400070525     c                   eval      h1colo = 21
028500070503     c                   leavesr
028600070503     c                   endif
028700070503
028800070503      * controllo se filiale già attivata (tabella GEO)
028900070525if  3c                   if        d§geofas1a <> *blanks and
029000070525if  3c                             d§geofas1b <> *blanks and
029100070525if  3c                             d§geofas2  <> *blanks
029200070503     c                   eval      w1cmsg = 'Filiale già attivata'
029300070503     c                   eval      *in28 = *on
029400070503     c                   eval      *in40 = *on
029500070503     c                   leavesr
029600070503     c                   endif
029700070503
029800070503      * controllo se filiale già attivata (organigramma)
029900070525     c                   if        (§ogtlls = 'D' or §ogtlls = 'S') and
030000070525     c                              §oggeot = 'S'
030100070503     c                   eval      w1cmsg = 'Filiale già attivata'
030200070503     c                   eval      *in28 = *on
030300070503     c                   eval      *in40 = *on
030400070503     c                   leavesr
030500070503     c                   endif
030600070503
030700070525      * devo richiedere almeno una FASE
030800070525     c                   if        w1fas1a = *blanks and w1fas1b = *blanks and
030900070525     c                             w1fas2  = *blanks
031000070525     c                   eval      w1cmsg = 'Scegliere la FASE da attivare'
031100070503     c                   eval      *in28 = *on
031200070525     c                   eval      h1riga = 07
031300070525     c                   eval      h1colo = 21
031400070503     c                   leavesr
031500070503     c                   endif
031600070525
031700070525      * FASE 1a Una-Tantum
031800070525     c                   if        w1fas1a = 'S'
031900070525      * errore se già attivato (tabella GEO)
032000070525if  3c                   if        d§geofas1a <> *blanks
032100070525     c                   eval      w1cmsg = 'FASE 1 Una-Tantum già attivata'
032200070525     c                   eval      *in28 = *on
032300070525     c                   eval      h1riga = 07
032400070525     c                   eval      h1colo = 21
032500070525     c                   leavesr
032600070525     c                   endif
032700070525      * errore se già attivato (organigramma)
032800070525     c                   if        §ogtlls = 'D'
032900070525     c                   eval      w1cmsg = 'FASE 1 Una-Tantum già attivata'
033000070525     c                   eval      *in28 = *on
033100070525     c                   eval      h1riga = 07
033200070525     c                   eval      h1colo = 21
033300070525     c                   leavesr
033400070525     c                   endif
033500070525     c                   endif
033600070525
033700070525      * FASE 1b Giornaliera
033800070525     c                   if        w1fas1b = 'S'
033900070528      * errore se non ho attivato già la fase 1a
034000070528if  3c                   if        d§geofas1a = *blanks
034100070528     c                   eval      w1cmsg =
034200070528     c                             'Prima attivare la FASE 1 Una-Tantum'
034300070528     c                   eval      *in28 = *on
034400070528     c                   eval      h1riga = 08
034500070528     c                   eval      h1colo = 21
034600070528     c                   leavesr
034700070528     c                   endif
034800070525      * errore se già attivato (tabella GEO)
034900070525if  3c                   if        d§geofas1b <> *blanks
035000070525     c                   eval      w1cmsg = 'FASE 1 giornaliera già attivata'
035100070525     c                   eval      *in28 = *on
035200070525     c                   eval      h1riga = 08
035300070525     c                   eval      h1colo = 21
035400070525     c                   leavesr
035500070525     c                   endif
035600070525      * errore se già attivato (organigramma)
035700070525     c                   if        §ogtlls = 'S'
035800070525     c                   eval      w1cmsg = 'FASE 1 giornaliera già attivata'
035900070525     c                   eval      *in28 = *on
036000070525     c                   eval      h1riga = 08
036100070525     c                   eval      h1colo = 21
036200070525     c                   leavesr
036300070525     c                   endif
036400070525     c                   endif
036500070502
036600070525      * FASE 2
036700070525     c                   if        w1fas2 = 'S'
036800070525      * errore se non ho attivato già le fasi 1
036900070525if  3c                   if        d§geofas1a = *blanks
037000070525     c                   eval      w1cmsg =
037100070525     c                             'Prima attivare la FASE 1 Una-Tantum'
037200070503     c                   eval      *in28 = *on
037300070525     c                   eval      h1riga = 09
037400070525     c                   eval      h1colo = 21
037500070503     c                   leavesr
037600070503     c                   endif
037700070525if  3c                   if        d§geofas1b = *blanks
037800070525     c                   eval      w1cmsg =
037900070525     c                             'Prima attivare la FASE 1 giornaliera'
038000070525     c                   eval      *in28 = *on
038100070525     c                   eval      h1riga = 09
038200070525     c                   eval      h1colo = 21
038300070525     c                   leavesr
038400070525     c                   endif
038500070503      * errore se già attivato (tabella GEO)
038600070525if  3c                   if        d§geofas2  <> *blanks
038700070525     c                   eval      w1cmsg = 'FASE 2 già attivata'
038800070502     c                   eval      *in28 = *on
038900070525     c                   eval      h1riga = 09
039000070525     c                   eval      h1colo = 21
039100070502     c                   leavesr
039200070502     c                   endif
039300070503      * errore se già attivato (organigramma)
039400070502     c                   if        §oggeot = 'S'
039500070525     c                   eval      w1cmsg = 'FASE 2 già attivata'
039600070502     c                   eval      *in28 = *on
039700070525     c                   eval      h1riga = 09
039800070525     c                   eval      h1colo = 21
039900070502     c                   leavesr
040000070502     c                   endif
040100070502     c                   endif
040200070424
040300070502      * no utente EDP
040400070502     c                   if        not *in02
040500070424      * controllo la password
040600070424     c                   if        w1cpsw = *blanks
040700070424     c                   eval      w1cmsg = 'Immettere la password'
040800070424     c                   eval      *in28 = *on
040900070502     c                   eval      h1riga = 11
041000070525     c                   eval      h1colo = 11
041100070424     c                   leavesr
041200070424     c                   endif
041300070424      * controllo se esatta
041400070424     c                   if        w1cpsw <> password
041500070424     c                   clear                   w1cpsw
041600070424     c                   eval      w1cmsg = 'Password errata'
041700070424     c                   eval      *in28 = *on
041800070502     c                   eval      h1riga = 11
041900070525     c                   eval      h1colo = 11
042000070424     c                   leavesr
042100070424     c                   endif
042200070502     c                   endif
042300070424
042400070424     c                   endsr
042500070424
042600070308      *------------------------------------------------------------------------*
042700070308      * pulizia del subfile
042800070308      *------------------------------------------------------------------------*
042900070308     c     sr_pulsfl     begsr
043000070308
043100070323     c                   clear                   nrr1
043200070308     c                   eval      *in20 = *off
043300070308     c                   eval      *in21 = *off
043400070323     c                   write     dg98c01
043500070308     c                   eval      *in21 = *on
043600070308     c                   eval      *in20 = *on
043700070308
043800070323     c                   eval      recsf1 = 1
043900070308     c                   eval      *in31 = *off
044000070308
044100070308     c                   endsr
044200070308
044300070308      *------------------------------------------------------------------------*
044400070308      * carico subfile con le filiali
044500070308      *------------------------------------------------------------------------*
044600070308     c     sr_carsfl     begsr
044700070308
044800070308      * leggo la schiera delle filiali
044900070308     c                   do        30            xx
045000070308     c                   if        lin(xx) = *blanks or lin(xx) = *zeros
045100070308     c                   iter
045200070308     c                   endif
045300070424     c                   move      lin(xx)       w0030
045400070424     c     w0030         chain(n)  azorg01l
045500070424     c                   eval      w1sfil = w0030
045600070424     c                   eval      w1sdfil = orgdes
045700070424     c                   eval      w1ssce = '1'
045800070323     c                   eval      nrr1 = xx
045900070323     c                   write     dg98s01
046000070308     c                   enddo
046100070308
046200070308     c                   eval      *in31 = *on
046300070502
046400070525      * imposto la fase da attivare
046500070525     c                   if        w1fas1a = 'S'
046600070525     c                   eval      w1fase = 'FASE 1 Una-Tantum'
046700070502     c                   endif
046800070525     c                   if        w1fas1b = 'S'
046900070525     c                   eval      w1fase = 'FASE 1 Giornaliera'
047000070525     c                   endif
047100070525     c                   if        w1fas2 = 'S'
047200070525     c                   eval      w1fase = 'FASE 2 Simulazione'
047300070502     c                   endif
047400070308
047500070308     c                   endsr
047600070308
047700070308      *------------------------------------------------------------------------*
047800070308      * emetto il subfile
047900070308      *------------------------------------------------------------------------*
048000070308     c     sr_emisfl     begsr
048100070308
048200070308     c                   do        *hival
048300070308
048400070323     c                   if        recsf1 > nrr1
048500070323     c                   eval      recsf1 = 1
048600070308     c                   endif
048700070308
048800070323     c                   write     dg98z01
048900070323     c                   exfmt     dg98c01
049000070424
049100070308     c                   eval      *in28 = *off
049200070323     c                   clear                   w1cmsg
049300070308
049400070308      * f3=fine
049500070308     c                   if        *inkc
049600070308     c                   goto      fine
049700070308     c                   endif
049800070308
049900070308      * f6=conferma
050000070308     c                   if        *inkf
050100070308     c                   exsr      sr_contrsfl
050200070308     c                   leave
050300070308     c                   endif
050400070308
050500070308     c                   enddo
050600070308
050700070308     c                   endsr
050800070308
050900070308      *------------------------------------------------------------------------*
051000070308      *  controllo i dati del subfile
051100070308      *------------------------------------------------------------------------*
051200070308     c     sr_contrsfl   begsr
051300070308
051400070508     c                   eval      wflag = *off
051500070323     c                   clear                   nrr1
051600070308     c                   do        *hival
051700070323     c                   add       1             nrr1
051800070323     c     nrr1          chain     dg98s01                            32
051900070308      * esco se fine sfl
052000070308     c                   if        *in32
052100070308     c                   leave
052200070308     c                   endif
052300070502
052400070508      * scelta = 1
052500070508     c                   if        w1ssce = '1'
052600070508      *            imposto i flag per la filiale richiesta in prima videata
052700070508     c                   if        wflag = *off
052800070502     c                   exsr      sr_flag
052900070508     c                   eval      wflag = *on
053000070508     c                   endif
053100070508      *            imposto la tabella per tutte le filiali
053200070508     c                   exsr      sr_tabgeo
053300070502
053400070525      * fase 1 b = pulisco fiarg
053500070612     c**!!!              if        w1fas1b = 'S'
053600070612     c**!!!              exsr      sr_fiarg
053700070612     c**!!!              endif
053800070308
053900070525      * fase 2 = blocco le spedizioni della linea di arrivo scelta
054000070525     c                   if        w1fas2 = 'S'
054100070323     c                   exsr      sr_blocca
054200070502     c                   endif
054300070502
054400070323     c                   eval      recsf1 = nrr1
054500070424     c                   clear                   w1ssce
054600070424     c                   endif
054700070323     c                   update    dg98s01
054800070308     c                   enddo
054900070308
055000070308     c                   endsr
055100070424
055200070424      *------------------------------------------------------------------------*
055300070508      *  imposta flag in organigramma
055400070424      *------------------------------------------------------------------------*
055500070424     c     sr_flag       begsr
055600070424
055700070424      * chiudo il file aperto
055800070424     c                   if        %open(azorg01l)
055900070424     c                   close     azorg01l
056000070424     c                   endif
056100070424
056200070424      * ovrdbf azorg01l di filiale
056300070424     c                   clear                   command
056400070424     c  n01              movea     cmd(2)        command
056500070424     c   01              movea     cmd(4)        command
056600070424     c                   call      'QCMDEXC'
056700070424     c                   parm                    command
056800070424     c                   parm                    lung
056900070424     c                   open      azorg01l
057000070424
057100070424      * imposto subito il flag in organigramma di filiale
057200070424     c                   clear                   og148
057300070508     c     w1cfil        chain     azorg01l
057400070424     c                   if        %found(azorg01l)
057500070424     c                   eval      og148 = orgde8
057600070525      * fase 1a
057700070525     c                   if        w1fas1a = 'S'
057800070525     c                   eval      §ogtlls = 'D'
057900070525     c                   endif
058000070525      * fase 1b
058100070525     c                   if        w1fas1b = 'S'
058200070502     c                   eval      §ogtlls = 'S'
058300070502     c                   endif
058400070525      * fase 2
058500070525     c                   if        w1fas2 = 'S'
058600070424     c                   eval      §oggeot = 'S'
058700070502     c                   endif
058800070424     c                   eval      orgde8 = og148
058900070424     c                   update    azorg
059000070424     c                   endif
059100070424
059200070424      * chiudo il file aperto
059300070424     c                   if        %open(azorg01l)
059400070424     c                   close     azorg01l
059500070424     c                   endif
059600070424
059700070424      * ovrdbf azorg01l di sede
059800070424     c                   clear                   command
059900070424     c  n01              movea     cmd(3)        command
060000070424     c   01              movea     cmd(5)        command
060100070424     c                   call      'QCMDEXC'
060200070424     c                   parm                    command
060300070424     c                   parm                    lung
060400070424     c                   open      azorg01l
060500070424
060600070424      * imposto subito il flag in organigramma di filiale
060700070424     c                   clear                   og148
060800070508     c     w1cfil        chain     azorg01l
060900070424     c                   if        %found(azorg01l)
061000070424     c                   eval      og148 = orgde8
061100070525      * fase 1a
061200070525     c                   if        w1fas1a = 'S'
061300070525     c                   eval      §ogtlls = 'D'
061400070525     c                   endif
061500070525      * fase 1b
061600070525     c                   if        w1fas1b = 'S'
061700070502     c                   eval      §ogtlls = 'S'
061800070502     c                   endif
061900070525      * fase 2
062000070525     c                   if        w1fas2 = 'S'
062100070424     c                   eval      §oggeot = 'S'
062200070502     c                   endif
062300070424     c                   eval      orgde8 = og148
062400070424     c                   update    azorg
062500070424     c                   endif
062600070424
062700070424     c                   endsr
062800070508
062900070508      *------------------------------------------------------------------------*
063000070508      *  aggiorna/scrive la tebella GEO
063100070508      *------------------------------------------------------------------------*
063200070508     c     sr_tabgeo     begsr
063300070508
063400070508      * aggiorno la tabella con tutte le filiali attivate
063500070508     c                   eval      ktbecod = 'GEO'
063600070508     c                   movel(p)  w1sfil        ktbeke1
063700070508     c     ktntbe        chain     tntbe01l
063800070525     c                   z-add     *date         w0080
063900070508      * scrivo
064000070508     c                   if        not %found(tntbe01l)
064100070508     c                   clear                   tntbe000
064200070508     c                   clear                   dgeo
064300070508     c                   eval      tbecod = ktbecod
064400070508     c                   eval      tbeke1 = ktbeke1
064500070525      * fase 1a
064600070525     c                   if        w1fas1a = 'S'
064700070525     c                   movel     w0080         d§geofas1a
064800070525     c                   endif
064900070525      * fase 1b
065000070525     c                   if        w1fas1b = 'S'
065100070525     c                   movel     w0080         d§geofas1b
065200070508     c                   endif
065300070525      * fase 2
065400070525     c                   if        w1fas2 = 'S'
065500070525     c                   movel     w0080         d§geofas2
065600070508     c                   endif
065700070508     c                   eval      tbeuni = dgeo
065800070508     c                   write     tntbe000
065900070508      * aggiorno
066000070508     c                   else
066100070508     c                   eval      dgeo = tbeuni
066200070525      * fase 1a
066300070525     c                   if        w1fas1a = 'S'
066400070525     c                   movel     w0080         d§geofas1a
066500070525     c                   endif
066600070525      * fase 1b
066700070525     c                   if        w1fas1b = 'S'
066800070525     c                   movel     w0080         d§geofas1b
066900070508     c                   endif
067000070525      * fase 2
067100070525     c                   if        w1fas2 = 'S'
067200070525     c                   movel     w0080         d§geofas2
067300070508     c                   endif
067400070508     c                   eval      tbeuni = dgeo
067500070508     c                   update    tntbe000
067600070508     c                   endif
067700070508
067800070508     c                   endsr
067900070502
068000070502      *------------------------------------------------------------------------*
068100070502      *  pulisco fiarg00f
068200070502      *------------------------------------------------------------------------*
068300070612     c**!!!sr_fiarg      begsr
068400070502
068500070612     c**!!!w1sfil        setll     fiarg02l
068600070612     c**!!!              do        *hival
068700070612     c**!!!w1sfil        reade     fiarg02l
068800070502      * fine file
068900070612     c**!!!              if        %eof(fiarg02l)
069000070612     c**!!!              leave
069100070612     c**!!!              endif
069200070502      * pulisco i campi necessari
069300070612     c**!!!              clear                   argatb
069400070612     c**!!!              clear                   argidb
069500070612     c**!!!              clear                   argttl
069600070612     c**!!!              clear                   argtgi
069700070612     c**!!!              clear                   argcgi
069800070612     c**!!!              clear                   argsqg
069900070612     c**!!!              clear                   argnst
070000070612     c**!!!              clear                   argxlo
070100070612     c**!!!              clear                   argyla
070200070612     c**!!!              clear                   arglno
070300070612     c**!!!              clear                   argerr
070400070612     c**!!!              clear                   argsco
070500070612     c**!!!              clear                   argpdc
070600070612     c**!!!              clear                   argfgs
070700070612     c**!!!              clear                   argndc
070800070612     c**!!!              clear                   argddc
070900070612     c**!!!              clear                   argstp
071000070612     c**!!!              clear                   argnftl
071100070612     c**!!!              clear                   argslb
071200070612     c**!!!              clear                   argdtvdis
071300070612     c**!!!              clear                   arghvdis
071400070612     c**!!!              clear                   argdtvtel
071500070612     c**!!!              clear                   arghvtel
071600070502
071700070502      * aggiorno
071800070612     c**!!!              update    fiarg000
071900070612     c**!!!              enddo
072000070502
072100070612     c**!!!              endsr
072200070308
072300070308      *------------------------------------------------------------------------*
072400070328      *  blocco le spedizioni
072500070308      *------------------------------------------------------------------------*
072600070323     c     sr_blocca     begsr
072700070308
072800070323     c                   eval      *in99 = *off
072900070308     c                   clear                   wtot
073000070323     c                   clear                   wtot1
073100070323     c                   clear                   wtot2
073200070323     c                   clear                   wtot3
073300070328     c                   clear                   wtot4
073400070413     c                   clear                   wtot5
073500031017
073600070328      * leggo bolle arrivo
073700070323     c     karb          setll     fnarb54l
073800070329    1c                   do        *hival
073900070323     c     karb          reade     fnarb54l
074000070323
074100070323      * fine file
074200070323     c                   if        %eof(fnarb54l)
074300070323     c                   leave
074400070323     c                   endif
074500070328
074600070328      * non elaboro se bolla di recupero
074700070328     c                   eval      tblkut = 1
074800070328     c                   eval      tblcod = '3A'
074900070328     c                   eval      tblkey = arbcbo
075000070328     c                   clear                   ds3a
075100070328     c     ktab          chain     tabel00f
075200070328     c                   if        %found(tabel00f) and tblflg = *blanks
075300070328     c                   eval      ds3a = tbluni
075400070328     c                   endif
075500070328     c                   if           §3arbl = 'R'
075600070328     c                   iter
075700070328     c                   endif
075800070323
075900070413      * pulisco i dati di work
076000070413     c                   clear                   errore
076100070507     c                   clear                   causa
076200070328     c                   clear                   wb4lbasa
076300070328     c                   clear                   wb4lbass
076400070323     c                   clear                   wb4lbge
076500070323     c                   clear                   wb4ltca
076600070323     c                   clear                   wb4ltfd
076700070503
076800070503      * --> per prima cosa controllo se ha già il rcd L su FIAR400F
076900070503     c     kar4          chain     fiar401l
077000070507    2c                   if        %found(fiar401l)
077100070503     c                   eval      dsbl4l = ar4not
077200070503      *    non elaboro se ha già il blocco sospensione
077300070503     c                   if        §b4lbge = 'S'
077400070503     c                   iter
077500070503     c                   endif
077600070507      *    non elaboro ma stampo errore se ha altri blocchi
077700070503     c                   if        §b4lbas <> *blanks or §b4ltca <> *blanks or
077800070503     c                             §b4ltfd <> *blanks
077900070503     c                   eval      errore = 'ALTRI BLOCCHI'
078000070503     c                   exsr      sr_staerr
078100070503     c                   iter
078200070503     c                   endif
078300070507    2c                   endif
078400070503      *     se non c'è rcd L su FIAR400F vado avanti
078500070328
078600070507      * --> blocco in sospensione se bolla con almeno una C.A.
078700070328     c                   clear                   fidn12ds
078800070328     c                   eval      i12aas = arbaas
078900070328     c                   eval      i12lnp = arblnp
079000070328     c                   eval      i12nrs = arbnrs
079100070328     c                   eval      i12nsp = arbnsp
079200070328     c                   eval      i12fel = 'E'
079300070328     c                   eval      i12fan = 'E'
079400070328     c                   call      'FIDN12R'
079500070328     c                   parm                    fidn12ds
079600070507      *     se non ci sono errori e trova almeno una c.a.
079700070507    2c                   if        o12err = ' ' and o12nca > *zeros
079800070507      *     blocco sospensione se solo c.a. aperte
079900070507     c                   if        o12cch = *blanks
080000070328     c                   eval      wb4lbge = 'S'
080100070507     c                   eval      causa = 'SOLO C.A. APERTE'
080200070507     c                   goto      scriviar4
080300070507     c                   endif
080400070507      *     blocco sospensione se anche solo una c.a. chiusa ma no lasciato avviso o giacenza
080500070507      *     (arbfbc = ' ')
080600070507     c                   if        arbfbc = *blanks
080700070507     c                   eval      wb4lbge = 'S'
080800070507     c                   eval      causa = 'ALMENO UNA C.A. CHIUSA'
080900070507     c                   goto      scriviar4
081000070507     c                   endif
081100070507      *     se arrivo qua vuol dire che anche se c'è una c.a. su questa bolla devo continuare
081200070507      *     con gli altri blocchi e non la metto in sospensione
081300070507    2c                   endif
081400070507
081500070507      * --> blocco per lasciato avviso
081600070507    2c                   if        arbfbc = 'A'
081700070507     c     kevb          setgt     fnevb05l
081800070507     c     kevb          readpe    fnevb05l
081900070507    3c                   if        not %eof(fnevb05l)
082000070507      * se c'è già il rcd su EVB2 stampo errore
082100070507     c     kevb2         chain     fnevb22l
082200070507    4c                   if        %found(fnevb22l)
082300070507     c                   eval      errore = 'FNEVB20F'
082400070507     c                   exsr      sr_staerr
082500070507     c                   else
082600070507      * scrivo fnevb2
082700070507     c                   clear                   fnevb200
082800070507     c                   eval      evb2aas = evbaas
082900070507     c                   eval      evb2lnp = evblnp
083000070507     c                   eval      evb2nrs = evbnrs
083100070507     c                   eval      evb2nsp = evbnsp
083200070507     c                   eval      evb2cev = evbcev
083300070507     c                   eval      evb2dtv = evbdtv
083400070507     c                   eval      evb2orv = evborv
083500070507     c                   eval      evb2fle = evbfle
083600070507     c                   eval      evb2dev = evbdev
083700070507     c                   eval      evb2hev = evbhev
083800070507     c                   eval      evb2tel = 'S'
083900070507     c                   write     fnevb200
084000070507      * stampo
084100070507     c                   exsr      sr_stampa5
084200070507      * sommo
084300070507     c                   add       1             wtot5
084400070507    4c                   endif
084500070507    3c                   endif
084600070507    2c                   endif
084700070328
084800070323      * per bolle non in distinta
084900070507    2c                   if        arbndc = *zeros
085000070328
085100070323      * imposto i seguenti blocchi:
085200070328
085300070420      * --> consegna particolare appuntamento (No Giacenza e No data cons.rich.)
085400070420     c                   if        (arbtc1 = 'A' or arbtc2 = 'A') and
085500070420     c                              arbfbc <> 'G' and arbdcr <= *zeros
085600070420     c                   eval      wb4lbasa = 'S'
085700070420     c                   endif
085800070420
085900070420      * --> consegna particolare supermercato (non tassativa e No Giacenza)
086000070420     c                   if        (arbtc1 = 'S' or arbtc2 = 'S') and
086100070420     c                              arbfbc <> 'G'
086200070329     c                   clear                   fnlv80ds
086300070329      *     se data consegna richiesta IL e la data è <= oggi BLOCCO
086400070525     c                   if        arbdcr <= *date and arbtcr = *blanks
086500070329     c                   else
086600070329      *     controllo se tassativa
086700070329     c                   if        arbdcr <> *zeros
086800070329     c                   eval      ilv80aas = arbaas
086900070329     c                   eval      ilv80lnp = arblnp
087000070329     c                   eval      ilv80nrs = arbnrs
087100070329     c                   eval      ilv80nsp = arbnsp
087200070329     c                   eval      ilv80mgs = arbmgs
087300070329     c                   eval      ilv80tc1 = arbtc1
087400070329     c                   eval      ilv80tc2 = arbtc2
087500070329     c                   eval      ilv80dcr = arbdcr
087600070329     c                   eval      ilv80hcr = arbhcr
087700070329     c                   eval      ilv80tcr = arbtcr
087800070329     c                   call      'FNLV80R'
087900070329     c                   parm                    kpjba
088000070329     c                   parm                    fnlv80ds
088100070329     c                   parm                    ilv80newproc
088200070329     c                   endif
088300070329     c                   endif
088400070329      *     blocco se consegna NO tassativa
088500070329     c                   if        olv80tat <> 'S'
088600070329     c                   eval      wb4lbass = 'S'
088700070329     c                   endif
088800070329     c                   endif
088900070328
089000070323      * --> contrassegno non abilitato
089100070323     c                   if        arbacc = *blanks
089200070323     c                   eval      wb4ltca = 'S'
089300070323     c                   endif
089400070328
089500070420      * --> fermo deposito (No Giacenza)
089600070420     c                   if        arbffd <> *blanks and arbfbc <> 'G'
089700070323     c                   eval      wb4ltfd = 'S'
089800070323     c                   endif
089900070329    2c                   endif
090000070507
090100070507     c     scriviar4     tag
090200070328
090300070328      * scrivo record L di fiar4 per la gestione dei blocchi
090400070329    2c                   if        wb4lbasa <> *blanks or
090500070328     c                             wb4lbass <> *blanks or
090600070328     c                             wb4lbge <> *blanks or
090700070328     c                             wb4ltca <> *blanks or
090800070328     c                             wb4ltfd <> *blanks
090900070328
091000070328      * se esiste già il rcd L su fiar4 stampo errore e torno a leggere una nuova bolla
091100070328     c     kar4          chain     fiar401l
091200070328     c                   if        %found(fiar401l)
091300070413     c                   eval      errore = 'FIAR400F'
091400070328     c                   exsr      sr_staerr
091500070328     c                   iter
091600070328     c                   endif
091700070328
091800070328      * scrivo rcd L su fiar4 e stampo
091900070328     c                   clear                   dsbl4l
092000070328     c                   clear                   fiar4000
092100070328     c                   eval      ar4aas = arbaas
092200070328     c                   eval      ar4lnp = arblnp
092300070328     c                   eval      ar4nrs = arbnrs
092400070328     c                   eval      ar4nsp = arbnsp
092500070328     c                   eval      ar4trc = 'L'
092600070328      * se blocco sospensione non imposto gli altri blocci
092700070329     c                   if        wb4lbge <> *blanks
092800070328     c                   eval      §b4lbge = 'S'
092900070329     c                   else
093000070328     c                   if        wb4lbasa <> *blanks or wb4lbass <> *blanks
093100070328     c                   eval      §b4lbas = 'S'
093200070328     c                   endif
093300070328     c                   eval      §b4ltca = wb4ltca
093400070328     c                   eval      §b4ltfd = wb4ltfd
093500070329     c                   endif
093600070328     c                   eval      ar4not = dsbl4l
093700070525     c                   eval      ar4duv = *date
093800070328     c                   eval      ar4ftr = 'R'
093900070525     c                   eval      ar4dtr = *date
094000070328      * scrivo
094100070328     c                   write     fiar4000
094200070328
094300070328      * stampo blocco e sommo
094400070328      * --> appuntamento
094500070328     c                   if        wb4lbasa <> *blanks
094600070328     c                   exsr      sr_stampa
094700070328      * sommo
094800070328     c                   add       1             wtot
094900070328     c                   endif
095000070328      * --> supermercato
095100070328     c                   if        wb4lbass <> *blanks
095200070328     c                   exsr      sr_stampa4
095300070328      * sommo
095400070328     c                   add       1             wtot4
095500070328     c                   endif
095600070328      * --> sospensione
095700070328     c                   if        wb4lbge <> *blanks
095800070328      * scrivo la nota
095900070328     c                   exsr      sr_nota
096000070328     c                   exsr      sr_stampa1
096100070328      * sommo
096200070328     c                   add       1             wtot1
096300070328     c                   endif
096400070328      * --> contrassegno
096500070328     c                   if        wb4ltca <> *blanks
096600070328     c                   exsr      sr_stampa2
096700070328      * sommo
096800070328     c                   add       1             wtot2
096900070328     c                   endif
097000070328      * --> fermo deposito
097100070328     c                   if        wb4ltfd <> *blanks
097200070328     c                   exsr      sr_stampa3
097300070328      * sommo
097400070328     c                   add       1             wtot3
097500070328     c                   endif
097600070329    2c                   endif
097700070329
097800070329    1c                   enddo
097900070328
098000070328      * stampo il totale delle bolle bloccate
098100070328     c                   if        wtot > *zeros
098200070328     c                   exsr      sr_tot
098300070328     c                   clear                   wtot
098400070328     c                   endif
098500070328     c                   if        wtot4 > *zeros
098600070328     c                   exsr      sr_tot4
098700070328     c                   clear                   wtot4
098800070328     c                   endif
098900070328     c                   if        wtot1 > *zeros
099000070328     c                   exsr      sr_tot1
099100070328     c                   clear                   wtot1
099200070328     c                   endif
099300070328     c                   if        wtot2 > *zeros
099400070328     c                   exsr      sr_tot2
099500070328     c                   clear                   wtot2
099600070328     c                   endif
099700070328     c                   if        wtot3 > *zeros
099800070328     c                   exsr      sr_tot3
099900070328     c                   clear                   wtot3
100000070328     c                   endif
100100070413     c                   if        wtot5 > *zeros
100200070413     c                   exsr      sr_tot5
100300070413     c                   clear                   wtot5
100400070413     c                   endif
100500070328
100600070328     c                   endsr
100700070323
100800070323      *------------------------------------------------------------------------*
100900070323      * stampa errore
101000070323      *------------------------------------------------------------------------*
101100070323     c     sr_staerr     begsr
101200070323
101300070323      * apro il file
101400070323     c                   if        not %open(qsysprt)
101500070323     c                   open      qsysprt
101600070323     c                   endif
101700070323      * stampo
101800070323     c                   if        not *in99 or *inof
101900070323     c                   except    testa
102000070323     c  n99              eval      *in99 = *on
102100070323     c                   endif
102200070323      * errore
102300070323     c                   except    rigaerr
102400070323
102500070323     c                   endsr
102600070323
102700070323      *------------------------------------------------------------------------*
102800070328      * stampa blocco appuntamento
102900070323      *------------------------------------------------------------------------*
103000070323     c     sr_stampa     begsr
103100070323
103200070323      * apro il file
103300070323     c                   if        not %open(fidg98p)
103400070323     c                   open      fidg98p
103500070323     c                   eval      *in10 = *on
103600070323     c                   endif
103700070323
103800070323      * stampo
103900070323     c                   if        *in10
104000070323     c                   write     dg98p01
104100070323     c                   eval      *in10 = *off
104200070323     c                   endif
104300070323     c                   write     dg98p02
104400070323
104500070323     c                   endsr
104600070328
104700070328      *------------------------------------------------------------------------*
104800070328      * stampa blocco supermercato
104900070328      *------------------------------------------------------------------------*
105000070328     c     sr_stampa4    begsr
105100070328
105200070328      * apro il file
105300070328     c                   if        not %open(fidg98p4)
105400070328     c                   open      fidg98p4
105500070328     c                   eval      *in14 = *on
105600070328     c                   endif
105700070328
105800070328      * stampo
105900070328     c                   if        *in14
106000070328     c                   write     dg98p401
106100070328     c                   eval      *in14 = *off
106200070328     c                   endif
106300070328     c                   write     dg98p402
106400070328
106500070328     c                   endsr
106600070323
106700070323      *------------------------------------------------------------------------*
106800070328      * stampa blocco sospensione
106900070323      *------------------------------------------------------------------------*
107000070323     c     sr_stampa1    begsr
107100070323
107200070323      * apro il file
107300070323     c                   if        not %open(fidg98p1)
107400070323     c                   open      fidg98p1
107500070323     c                   eval      *in11 = *on
107600070323     c                   endif
107700070323
107800070323      * stampo
107900070323     c                   if        *in11
108000070323     c                   write     dg98p101
108100070323     c                   eval      *in11 = *off
108200070323     c                   endif
108300070323     c                   write     dg98p102
108400070323
108500070323     c                   endsr
108600070323
108700070323      *------------------------------------------------------------------------*
108800070323      * stampa blocco contrassegno
108900070323      *------------------------------------------------------------------------*
109000070323     c     sr_stampa2    begsr
109100070323
109200070323      * apro il file
109300070323     c                   if        not %open(fidg98p2)
109400070323     c                   open      fidg98p2
109500070323     c                   eval      *in12 = *on
109600070323     c                   endif
109700070323
109800070323      * stampo
109900070323     c                   if        *in12
110000070323     c                   write     dg98p201
110100070323     c                   eval      *in12 = *off
110200070323     c                   endif
110300070323     c                   write     dg98p202
110400070323
110500070323     c                   endsr
110600070323
110700070323      *------------------------------------------------------------------------*
110800070323      * stampa blocco fermo deposito
110900070323      *------------------------------------------------------------------------*
111000070323     c     sr_stampa3    begsr
111100070323
111200070323      * apro il file
111300070323     c                   if        not %open(fidg98p3)
111400070323     c                   open      fidg98p3
111500070323     c                   eval      *in13 = *on
111600070323     c                   endif
111700070323
111800070323      * stampo
111900070323     c                   if        *in13
112000070323     c                   write     dg98p301
112100070323     c                   eval      *in13 = *off
112200070323     c                   endif
112300070323     c                   write     dg98p302
112400070323
112500070323     c                   endsr
112600070413
112700070413      *------------------------------------------------------------------------*
112800070413      * stampa blocco lasciato avviso
112900070413      *------------------------------------------------------------------------*
113000070413     c     sr_stampa5    begsr
113100070413
113200070413      * apro il file
113300070413     c                   if        not %open(fidg98p5)
113400070413     c                   open      fidg98p5
113500070413     c                   eval      *in15 = *on
113600070413     c                   endif
113700070413
113800070413      * stampo
113900070413     c                   if        *in15
114000070413     c                   write     dg98p501
114100070413     c                   eval      *in15 = *off
114200070413     c                   endif
114300070413     c                   write     dg98p502
114400070413
114500070413     c                   endsr
114600070323
114700070323      *------------------------------------------------------------------------*
114800070323      * creo nota per blocco generico
114900070323      *------------------------------------------------------------------------*
115000070323     c     sr_nota       begsr
115100070323
115200070323     c                   clear                   fiarn000
115300070323     c                   eval      arnaas = arbaas
115400070323     c                   eval      arnlnp = arblnp
115500070323     c                   eval      arnnrs = arbnrs
115600070323     c                   eval      arnnsp = arbnsp
115700070323     c                   eval      arncdn = 'BGE'
115800070323     c                   eval      arnpno = 1
115900070323     c                   eval      arnnob = 'Impostata in AUTOMATICO'
116000070525     c                   eval      arndim = *date
116100070323     c                   time                    arnoim
116200070323     c                   eval      arnpru = 'EDPCED'
116300070323     c                   eval      arnpgm = 'FIDG98R'
116400070416     c                   eval      arnfl1 = 'S'
116500070323     c                   write     fiarn000
116600070323
116700070323     c                   endsr
116800070323
116900070323      *------------------------------------------------------------------------*
117000070328      * stampa totale blocco appuntamento
117100070323      *------------------------------------------------------------------------*
117200070323     c     sr_tot        begsr
117300070323
117400070323     c                   if        *in10
117500070323     c                   write     dg98p01
117600070323     c                   eval      *in10 = *off
117700070323     c                   endif
117800070323
117900070323     c                   write     dg98p03
118000070323     c                   write     dg98pf
118100070323     c                   close     fidg98p
118200070323
118300070323     c                   endsr
118400070328
118500070328      *------------------------------------------------------------------------*
118600070328      * stampa totale blocco supermercato
118700070328      *------------------------------------------------------------------------*
118800070328     c     sr_tot4       begsr
118900070328
119000070328     c                   if        *in14
119100070328     c                   write     dg98p401
119200070328     c                   eval      *in14 = *off
119300070328     c                   endif
119400070328
119500070328     c                   write     dg98p403
119600070328     c                   write     dg98p4f
119700070328     c                   close     fidg98p4
119800070328
119900070328     c                   endsr
120000070323
120100070323      *------------------------------------------------------------------------*
120200070323      * stampa totale blocco generale
120300070323      *------------------------------------------------------------------------*
120400070323     c     sr_tot1       begsr
120500070323
120600070323     c                   if        *in11
120700070323     c                   write     dg98p101
120800070323     c                   eval      *in11 = *off
120900070323     c                   endif
121000070323
121100070323     c                   write     dg98p103
121200070323     c                   write     dg98p1f
121300070323     c                   close     fidg98p1
121400070323
121500070323     c                   endsr
121600070323
121700070323      *------------------------------------------------------------------------*
121800070323      * stampa totale blocco contrassegno
121900070323      *------------------------------------------------------------------------*
122000070323     c     sr_tot2       begsr
122100070323
122200070323     c                   if        *in12
122300070323     c                   write     dg98p201
122400070323     c                   eval      *in12 = *off
122500070323     c                   endif
122600070323
122700070323     c                   write     dg98p203
122800070323     c                   write     dg98p2f
122900070323     c                   close     fidg98p2
123000070323
123100070323     c                   endsr
123200070323
123300070323      *------------------------------------------------------------------------*
123400070323      * stampa totale blocco fermo deposito
123500070323      *------------------------------------------------------------------------*
123600070323     c     sr_tot3       begsr
123700070323
123800070323     c                   if        *in13
123900070323     c                   write     dg98p301
124000070323     c                   eval      *in13 = *off
124100070323     c                   endif
124200070323
124300070323     c                   write     dg98p303
124400070323     c                   write     dg98p3f
124500070323     c                   close     fidg98p3
124600070323
124700070323     c                   endsr
124800070413
124900070413      *------------------------------------------------------------------------*
125000070413      * stampa totale blocco lasciato avviso
125100070413      *------------------------------------------------------------------------*
125200070413     c     sr_tot5       begsr
125300070413
125400070413     c                   if        *in15
125500070413     c                   write     dg98p501
125600070413     c                   eval      *in15 = *off
125700070413     c                   endif
125800070413
125900070413     c                   write     dg98p503
126000070413     c                   write     dg98p5f
126100070413     c                   close     fidg98p5
126200070413
126300070413     c                   endsr
126400070308
126500070308      *------------------------------------------------------------------------*
126600070308      * routine iniziale
126700070308      *------------------------------------------------------------------------*
126800070308     c     *inzsr        begsr
126900070308
127000070308     c     *entry        plist
127100070424     c                   parm                    kpjba
127200070424
127300070424      * ambiente di prova
127400070424     c                   eval      *in01 = (%subst(knsif:7:1) = 'P')
127500070502
127600070502      * utente EDP
127700070502     c                   eval      *in02 = (%subst(knmus:1:3) = 'EDP')
127800070424
127900070424     c     *dtaara       define    §azute        azuteds
128000070424     c     *dtaara       define    §datiute      ddatiute
128100070424     c                   in(e)     *dtaara
128200070424     c                   if        %error or rsut = *blanks
128300070424     c                   clear                   tibs34ds
128400070424     c                   call      'TIBS34R'
128500070424     c                   parm                    tibs34ds
128600070424     c                   in        *dtaara
128700070424     c                   endif
128800070528
128900070528      * imposto la filiale utente a video
129000070528     c                   eval      w1cfil = dutpou
129100070528     c                   eval      w1dfil = dutdpo
129200070328
129300070328     c     ktab          klist
129400070328     c                   kfld                    tblkut
129500070328     c                   kfld                    tblcod
129600070328     c                   kfld                    tblkey
129700070323
129800070323     c     karb          klist
129900070424     c                   kfld                    w1sfil
130000070323     c                   kfld                    wdcm
130100070413
130200070323     c     kar4          klist
130300070323     c                   kfld                    arbaas
130400070323     c                   kfld                    arblnp
130500070323     c                   kfld                    arbnrs
130600070323     c                   kfld                    arbnsp
130700070323     c                   kfld                    ktrc
130800070413
130900070413     c     kevb          klist
131000070413     c                   kfld                    arbaas
131100070413     c                   kfld                    arblnp
131200070413     c                   kfld                    arbnrs
131300070413     c                   kfld                    arbnsp
131400070413
131500070413     c     kevb2         klist
131600070413     c                   kfld                    evbaas
131700070413     c                   kfld                    evblnp
131800070413     c                   kfld                    evbnrs
131900070413     c                   kfld                    evbnsp
132000070413     c                   kfld                    evbdtv
132100070413     c                   kfld                    evborv
132200070419     c                   kfld                    evbcev
132300070424
132400070424     c     ktntbe        klist
132500070424     c                   kfld                    ktbecod
132600070424     c                   kfld                    ktbeke1
132700070308
132800070424      * ovrprtf file di stampa
132900070323     c                   clear                   command
133000070323     c                   movea     cmd(1)        command
133100070308     c                   call      'QCMDEXC'
133200070308     c                   parm                    command
133300070308     c                   parm                    lung
133400070308
133500070424      * ovrdbf azorg01l di filiale
133600070424     c                   clear                   command
133700070424     c  n01              movea     cmd(2)        command
133800070424     c   01              movea     cmd(4)        command
133900070424     c                   call      'QCMDEXC'
134000070424     c                   parm                    command
134100070424     c                   parm                    lung
134200070424     c                   open      azorg01l
134300070424
134400070308     c                   endsr
134500070323
134600070323     oqsysprt   e            testa          2 02
134700070323     o                                         +  5 'Blocca spedizioni per'
134800070323     o                                         +  1 'attivazione telefonate'
134900070323     o                                         +  1 'GEO'
135000070424     o                       w1sfil            +  1
135100070424     o                       w1sdfil           +  1
135200070323     o                                           85 'FIDG98R'
135300070525     o                       *date         y    110
135400070323     o                                          128 'Pag.'
135500070323     o                       page          z    132
135600070323     o          e            rigaerr        1
135700070323     o                                           +1 'Spedizione:'
135800070323     o                       arbaas              +1
135900070323     o                       arblnp              +1
136000070323     o                       arbnrs        z     +1
136100070323     o                       arbnsp        z     +1
136200070413     o                       errore              +1
136300070323** CMD   (Lunga 80)                                                            *
136400070323OVRPRTF QSYSPRT SAVE(*YES)                                                       1
136500070424OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRU/AZORG01L)                                 2
136600070424OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRU/AZORG01L)                                 3
136700070424OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRPF/AZORG01L)                                4
136800070424OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRPS/AZORG01L)                                5
