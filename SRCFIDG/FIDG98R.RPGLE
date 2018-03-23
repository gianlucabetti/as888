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
006500080118     d wforza          s               n
006600080118     d w£6             s               n
006700050914     d wtot            s              7  0 inz
006800070323     d wtot1           s              7  0 inz
006900070323     d wtot2           s              7  0 inz
007000070323     d wtot3           s              7  0 inz
007100070328     d wtot4           s              7  0 inz
007200070413     d wtot5           s              7  0 inz
007300070308     d w0030           s                   like(orgfil)
007400070424     d w0080           s              8  0 inz
007500070308     d xx              s              2  0 inz
007600070328
007700070323      *------------------------------------------------------------------------*
007800070323      * schiere
007900070323      *------------------------------------------------------------------------*
008000070424     d cmd             s             80    dim(5)  ctdata perrcd(1)
008100031030
008200070308      *------------------------------------------------------------------------*
008300070308      * ds interne/esterne
008400070308      *------------------------------------------------------------------------*
008500070525     d wdatainv        ds
008600070525     d waainv                  1      4
008700070525     d wmminv                  5      6
008800070525     d wgginv                  7      8
008900070525
009000070525     d wdata           ds
009100070525     d wgg                     1      2
009200070525     d wmm                     3      4
009300070525     d waa                     5      8
009400070525
009500070424     d azuteds       e ds                  extname(azute00f)
009600070424     d ddatiute      e ds
009700070502     d dgeo          e ds
009800070323     d dsbl4l        e ds
009900070328     d ds3a          e ds
010000070323     d fidn12ds      e ds
010100070323     d  skkey                 26    305    dim(20)
010200070323     d  skdca                326    485  0 dim(20)
010300070424     d fnlv55ds      e ds
010400070328     d fnlv80ds      e ds
010500070308     d kpjba         e ds
010600070424     d og148         e ds
010700070424     d tibs02ds      e ds
010800070424     d tibs34ds      e ds
010900070308     d trul06ds      e ds                  inz
011000070308     d  lin                    1     90    dim(30)
011100070308
011200070424      *------------------------------------------------------------------------*
011300070424      * costanti
011400070424      *------------------------------------------------------------------------*
011500070426     d password        c                   const('GIANNI    ')
011600070424
011700070308      *------------------------------------------------------------------------*
011800070528
011900070528     c     inizio        tag
012000070424
012100070525     c                   exsr      sr_carvideo1
012200070525
012300070424     c                   exsr      sr_video1
012400070424
012500070424     c                   exsr      sr_video2
012600080118
012700080118     c                   eval      wforza = *off
012800070308
012900070308     c     fine          tag
013000070328
013100070308     c                   eval      *inlr = *on
013200070525
013300070525      *------------------------------------------------------------------------*
013400070525      * carico i dati nella prima videata
013500070525      *------------------------------------------------------------------------*
013600070525     c     sr_carvideo1  begsr
013700070525
013800070525     c                   clear                   w1fas1a
013900070525     c                   clear                   w1fas1b
014000070525     c                   clear                   w1fas2
014100070528     c                   clear                   w1dta1a
014200070528     c                   clear                   w1dta1b
014300070528     c                   clear                   w1dta2
014400070525     c                   clear                   w1cpsw
014500070525
014600070525      * aggancio la tabella GEO della filiale utente
014700070525     c                   clear                   dgeo
014800070525     c                   clear                   tibs02ds
014900070525     c                   eval      t02mod = 'C'
015000070525     c                   eval      t02sif = knsif
015100070525     c                   eval      t02cod = 'GEO'
015200070525     c                   movel     w1cfil        t02ke1
015300070525     c                   call      'TIBS02R'
015400070525     c                   parm                    kpjba
015500070525     c                   parm                    tibs02ds
015600070525     c                   if        t02err = *blanks
015700070525     c                   eval      dgeo = t02uni
015800070525     c                   endif
015900070525      * data fase 1 una tantum
016000070525     c                   if        d§geofas1a > *zeros
016100070525     c                   eval      wdatainv = d§geofas1a
016200070525     c                   eval      waa = waainv
016300070525     c                   eval      wmm = wmminv
016400070525     c                   eval      wgg = wgginv
016500070525     c                   move      wdata         w1dta1a
016600070525     c                   endif
016700070525      * data fase 1 giornaliera
016800070525     c                   if        d§geofas1b > *zeros
016900070525     c                   eval      wdatainv = d§geofas1b
017000070525     c                   eval      waa = waainv
017100070525     c                   eval      wmm = wmminv
017200070525     c                   eval      wgg = wgginv
017300070525     c                   move      wdata         w1dta1b
017400070525     c                   endif
017500070525      * data fase 2 simualazione
017600070525     c                   if        d§geofas2 > *zeros
017700070525     c                   eval      wdatainv = d§geofas2
017800070525     c                   eval      waa = waainv
017900070525     c                   eval      wmm = wmminv
018000070525     c                   eval      wgg = wgginv
018100070525     c                   move      wdata         w1dta2
018200070525     c                   endif
018300070525
018400070525     c                   eval      sav_w1cfil = w1cfil
018500070525
018600070525     c                   endsr
018700070424
018800070424      *------------------------------------------------------------------------*
018900070424      * gestione prima videata
019000070424      *------------------------------------------------------------------------*
019100070424     c     sr_video1     begsr
019200070424
019300070424     c                   do        *hival
019400070424     c                   exfmt     dg98d01
019500070424     c                   eval      *in28 = *off
019600070424     c                   clear                   w1cmsg
019700070502     c                   clear                   h1colo
019800070502     c                   clear                   h1riga
019900070424
020000070424      * f3=fine
020100070424     c                   if        *inkc
020200070424     c                   goto      fine
020300070424     c                   endif
020400070424
020500070424      * controllo
020600070424     c                   exsr      sr_ctrd01
020700070424
020800070424     c                   if        *in28
020900070424     c                   iter
021000070424     c                   endif
021100070424
021200070424      * f6=conferma
021300070424     c                   if        *inkf
021400070424     c                   leave
021500070424     c                   endif
021600070424
021700070424     c                   enddo
021800070424
021900070424     c                   endsr
022000070308
022100070424      *------------------------------------------------------------------------*
022200070424      * gestione seconda videata
022300070424      *------------------------------------------------------------------------*
022400070424     c     sr_video2     begsr
022500070424
022600070424      * cerco le filiali gestite dalla filiale da elaborare
022700070424     c                   clear                   trul06ds
022800070424     c                   eval      d06cod = '£6'
022900070424     c                   movel     w1cfil        d06key
023000070424     c                   eval      d06tla = 'L'
023100070424     c                   eval      kpjbu = trul06ds
023200070424     c                   call      'TRUL06R'
023300070424     c                   parm                    kpjba
023400070424     c                   eval      trul06ds = kpjbu
023500070424
023600070424      * visualizzo le linee di arrivo da abilitare
023700070424     c                   exsr      sr_pulsfl
023800070424     c                   exsr      sr_carsfl
023900070424     c                   exsr      sr_emisfl
024000070424
024100070424     c                   endsr
024200070424
024300070424      *------------------------------------------------------------------------*
024400070424      * controllo la prima videata
024500070424      *------------------------------------------------------------------------*
024600070424     c     sr_ctrd01     begsr
024700070424
024800070424      * la filiale inserita deve essere una filiale valida
024900070424     c                   clear                   w1dfil
025000070424     c                   clear                   og148
025100070424     c     w1cfil        chain(n)  azorg01l
025200070424     c                   if        not %found(azorg01l) or orgfva <> *blanks or
025300070424     c                             (orgfag <> 'A' and orgfag <> 'F')
025400070424     c                   eval      w1cmsg = 'Filiale errata'
025500070424     c                   eval      *in28 = *on
025600070424     c                   eval      *in40 = *on
025700070424     c                   leavesr
025800070424     c                   endif
025900070502
026000070502     c                   eval      w1dfil= orgdes
026100070502     c                   eval      og148 = orgde8
026200070502
026300070502      * non deve essere una filiale gestita da altra filiale
026400080118     c                   eval      w£6 = *off
026500070502     c                   clear                   fnlv55ds
026600070502     c                   movel     '6'           d55tpt
026700070502     c                   z-add     w1cfil        d55lin
026800070525     c                   z-add     *date         d55drf
026900070502     c                   call      'FNLV55R'
027000070502     c                   parm                    fnlv55ds
027100080118     c                   if        d55lin <> d55tfa
027200080118     c                   if        not wforza
027300080118     c                   eval      w1cmsg = 'Attenzione!! Filiale gestita ' +
027400080118     c                             'Enter forza!'
027500070502     c                   eval      *in28 = *on
027600070502     c                   eval      *in40 = *on
027700080118     c                   eval      wforza = *on
027800070502     c                   leavesr
027900080118     c                   endif
028000080118     c                   eval      w£6 = *on
028100070502     c                   endif
028200070502
028300070525      * se variata la filiale riemetto la videata
028400070525     c                   if        w1cfil <> sav_w1cfil
028500070525     c                   goto      inizio
028600070525     c                   endif
028700070503
028800070525      * posso richiedere una fase alla volta
028900070525     c                   if        w1fas1a = 'S' and w1fas1b = 'S' and
029000070525     c                             w1fas2  = 'S'
029100070525     c                   eval      w1cmsg = 'Attivare una FASE alla volta'
029200070503     c                   eval      *in28 = *on
029300070525     c                   eval      h1riga = 07
029400070525     c                   eval      h1colo = 21
029500070503     c                   leavesr
029600070503     c                   endif
029700070503
029800070503      * controllo se filiale già attivata (tabella GEO)
029900070525if  3c                   if        d§geofas1a <> *blanks and
030000070525if  3c                             d§geofas1b <> *blanks and
030100070525if  3c                             d§geofas2  <> *blanks
030200070503     c                   eval      w1cmsg = 'Filiale già attivata'
030300070503     c                   eval      *in28 = *on
030400070503     c                   eval      *in40 = *on
030500070503     c                   leavesr
030600070503     c                   endif
030700070503
030800070503      * controllo se filiale già attivata (organigramma)
030900070525     c                   if        (§ogtlls = 'D' or §ogtlls = 'S') and
031000070525     c                              §oggeot = 'S'
031100070503     c                   eval      w1cmsg = 'Filiale già attivata'
031200070503     c                   eval      *in28 = *on
031300070503     c                   eval      *in40 = *on
031400070503     c                   leavesr
031500070503     c                   endif
031600070503
031700070525      * devo richiedere almeno una FASE
031800070525     c                   if        w1fas1a = *blanks and w1fas1b = *blanks and
031900070525     c                             w1fas2  = *blanks
032000070525     c                   eval      w1cmsg = 'Scegliere la FASE da attivare'
032100070503     c                   eval      *in28 = *on
032200070525     c                   eval      h1riga = 07
032300070525     c                   eval      h1colo = 21
032400070503     c                   leavesr
032500070503     c                   endif
032600070525
032700070525      * FASE 1a Una-Tantum
032800070525     c                   if        w1fas1a = 'S'
032900070525      * errore se già attivato (tabella GEO)
033000070525if  3c                   if        d§geofas1a <> *blanks
033100070525     c                   eval      w1cmsg = 'FASE 1 Una-Tantum già attivata'
033200070525     c                   eval      *in28 = *on
033300070525     c                   eval      h1riga = 07
033400070525     c                   eval      h1colo = 21
033500070525     c                   leavesr
033600070525     c                   endif
033700070525      * errore se già attivato (organigramma)
033800070525     c                   if        §ogtlls = 'D'
033900070525     c                   eval      w1cmsg = 'FASE 1 Una-Tantum già attivata'
034000070525     c                   eval      *in28 = *on
034100070525     c                   eval      h1riga = 07
034200070525     c                   eval      h1colo = 21
034300070525     c                   leavesr
034400070525     c                   endif
034500070525     c                   endif
034600070525
034700070525      * FASE 1b Giornaliera
034800070525     c                   if        w1fas1b = 'S'
034900070528      * errore se non ho attivato già la fase 1a
035000070528if  3c                   if        d§geofas1a = *blanks
035100070528     c                   eval      w1cmsg =
035200070528     c                             'Prima attivare la FASE 1 Una-Tantum'
035300070528     c                   eval      *in28 = *on
035400070528     c                   eval      h1riga = 08
035500070528     c                   eval      h1colo = 21
035600070528     c                   leavesr
035700070528     c                   endif
035800070525      * errore se già attivato (tabella GEO)
035900070525if  3c                   if        d§geofas1b <> *blanks
036000070525     c                   eval      w1cmsg = 'FASE 1 giornaliera già attivata'
036100070525     c                   eval      *in28 = *on
036200070525     c                   eval      h1riga = 08
036300070525     c                   eval      h1colo = 21
036400070525     c                   leavesr
036500070525     c                   endif
036600070525      * errore se già attivato (organigramma)
036700070525     c                   if        §ogtlls = 'S'
036800070525     c                   eval      w1cmsg = 'FASE 1 giornaliera già attivata'
036900070525     c                   eval      *in28 = *on
037000070525     c                   eval      h1riga = 08
037100070525     c                   eval      h1colo = 21
037200070525     c                   leavesr
037300070525     c                   endif
037400070525     c                   endif
037500070502
037600070525      * FASE 2
037700070525     c                   if        w1fas2 = 'S'
037800070525      * errore se non ho attivato già le fasi 1
037900070525if  3c                   if        d§geofas1a = *blanks
038000070525     c                   eval      w1cmsg =
038100070525     c                             'Prima attivare la FASE 1 Una-Tantum'
038200070503     c                   eval      *in28 = *on
038300070525     c                   eval      h1riga = 09
038400070525     c                   eval      h1colo = 21
038500070503     c                   leavesr
038600070503     c                   endif
038700070525if  3c                   if        d§geofas1b = *blanks
038800070525     c                   eval      w1cmsg =
038900070525     c                             'Prima attivare la FASE 1 giornaliera'
039000070525     c                   eval      *in28 = *on
039100070525     c                   eval      h1riga = 09
039200070525     c                   eval      h1colo = 21
039300070525     c                   leavesr
039400070525     c                   endif
039500070503      * errore se già attivato (tabella GEO)
039600070525if  3c                   if        d§geofas2  <> *blanks
039700070525     c                   eval      w1cmsg = 'FASE 2 già attivata'
039800070502     c                   eval      *in28 = *on
039900070525     c                   eval      h1riga = 09
040000070525     c                   eval      h1colo = 21
040100070502     c                   leavesr
040200070502     c                   endif
040300070503      * errore se già attivato (organigramma)
040400070502     c                   if        §oggeot = 'S'
040500070525     c                   eval      w1cmsg = 'FASE 2 già attivata'
040600070502     c                   eval      *in28 = *on
040700070525     c                   eval      h1riga = 09
040800070525     c                   eval      h1colo = 21
040900070502     c                   leavesr
041000070502     c                   endif
041100070502     c                   endif
041200070424
041300070502      * no utente EDP
041400070502     c                   if        not *in02
041500070424      * controllo la password
041600070424     c                   if        w1cpsw = *blanks
041700070424     c                   eval      w1cmsg = 'Immettere la password'
041800070424     c                   eval      *in28 = *on
041900070502     c                   eval      h1riga = 11
042000070525     c                   eval      h1colo = 11
042100070424     c                   leavesr
042200070424     c                   endif
042300070424      * controllo se esatta
042400070424     c                   if        w1cpsw <> password
042500070424     c                   clear                   w1cpsw
042600070424     c                   eval      w1cmsg = 'Password errata'
042700070424     c                   eval      *in28 = *on
042800070502     c                   eval      h1riga = 11
042900070525     c                   eval      h1colo = 11
043000070424     c                   leavesr
043100070424     c                   endif
043200070502     c                   endif
043300070424
043400070424     c                   endsr
043500070424
043600070308      *------------------------------------------------------------------------*
043700070308      * pulizia del subfile
043800070308      *------------------------------------------------------------------------*
043900070308     c     sr_pulsfl     begsr
044000070308
044100070323     c                   clear                   nrr1
044200070308     c                   eval      *in20 = *off
044300070308     c                   eval      *in21 = *off
044400070323     c                   write     dg98c01
044500070308     c                   eval      *in21 = *on
044600070308     c                   eval      *in20 = *on
044700070308
044800070323     c                   eval      recsf1 = 1
044900070308     c                   eval      *in31 = *off
045000070308
045100070308     c                   endsr
045200070308
045300070308      *------------------------------------------------------------------------*
045400070308      * carico subfile con le filiali
045500070308      *------------------------------------------------------------------------*
045600070308     c     sr_carsfl     begsr
045700070308
045800070308      * leggo la schiera delle filiali
045900070308     c                   do        30            xx
046000070308     c                   if        lin(xx) = *blanks or lin(xx) = *zeros
046100070308     c                   iter
046200070308     c                   endif
046300070424     c                   move      lin(xx)       w0030
046400070424     c     w0030         chain(n)  azorg01l
046500070424     c                   eval      w1sfil = w0030
046600070424     c                   eval      w1sdfil = orgdes
046700070424     c                   eval      w1ssce = '1'
046800070323     c                   eval      nrr1 = xx
046900070323     c                   write     dg98s01
047000070308     c                   enddo
047100070308
047200070308     c                   eval      *in31 = *on
047300070502
047400070525      * imposto la fase da attivare
047500070525     c                   if        w1fas1a = 'S'
047600070525     c                   eval      w1fase = 'FASE 1 Una-Tantum'
047700070502     c                   endif
047800070525     c                   if        w1fas1b = 'S'
047900070525     c                   eval      w1fase = 'FASE 1 Giornaliera'
048000070525     c                   endif
048100070525     c                   if        w1fas2 = 'S'
048200070525     c                   eval      w1fase = 'FASE 2 Simulazione'
048300070502     c                   endif
048400070308
048500070308     c                   endsr
048600070308
048700070308      *------------------------------------------------------------------------*
048800070308      * emetto il subfile
048900070308      *------------------------------------------------------------------------*
049000070308     c     sr_emisfl     begsr
049100070308
049200070308     c                   do        *hival
049300070308
049400070323     c                   if        recsf1 > nrr1
049500070323     c                   eval      recsf1 = 1
049600070308     c                   endif
049700070308
049800070323     c                   write     dg98z01
049900070323     c                   exfmt     dg98c01
050000070424
050100070308     c                   eval      *in28 = *off
050200070323     c                   clear                   w1cmsg
050300070308
050400070308      * f3=fine
050500070308     c                   if        *inkc
050600070308     c                   goto      fine
050700070308     c                   endif
050800070308
050900070308      * f6=conferma
051000070308     c                   if        *inkf
051100070308     c                   exsr      sr_contrsfl
051200070308     c                   leave
051300070308     c                   endif
051400070308
051500070308     c                   enddo
051600070308
051700070308     c                   endsr
051800070308
051900070308      *------------------------------------------------------------------------*
052000070308      *  controllo i dati del subfile
052100070308      *------------------------------------------------------------------------*
052200070308     c     sr_contrsfl   begsr
052300070308
052400070508     c                   eval      wflag = *off
052500070323     c                   clear                   nrr1
052600070308     c                   do        *hival
052700070323     c                   add       1             nrr1
052800070323     c     nrr1          chain     dg98s01                            32
052900070308      * esco se fine sfl
053000070308     c                   if        *in32
053100070308     c                   leave
053200070308     c                   endif
053300070502
053400070508      * scelta = 1
053500070508     c                   if        w1ssce = '1'
053600070508      *            imposto i flag per la filiale richiesta in prima videata
053700070508     c                   if        wflag = *off
053800070502     c                   exsr      sr_flag
053900070508     c                   eval      wflag = *on
054000070508     c                   endif
054100070508      *            imposto la tabella per tutte le filiali
054200070508     c                   exsr      sr_tabgeo
054300070502
054400070525      * fase 1 b = pulisco fiarg
054500070612     c**!!!              if        w1fas1b = 'S'
054600070612     c**!!!              exsr      sr_fiarg
054700070612     c**!!!              endif
054800070308
054900070525      * fase 2 = blocco le spedizioni della linea di arrivo scelta
055000070525     c                   if        w1fas2 = 'S'
055100080118     c                             and not w£6
055200070323     c                   exsr      sr_blocca
055300070502     c                   endif
055400070502
055500070323     c                   eval      recsf1 = nrr1
055600070424     c                   clear                   w1ssce
055700070424     c                   endif
055800070323     c                   update    dg98s01
055900070308     c                   enddo
056000070308
056100070308     c                   endsr
056200070424
056300070424      *------------------------------------------------------------------------*
056400070508      *  imposta flag in organigramma
056500070424      *------------------------------------------------------------------------*
056600070424     c     sr_flag       begsr
056700070424
056800070424      * chiudo il file aperto
056900070424     c                   if        %open(azorg01l)
057000070424     c                   close     azorg01l
057100070424     c                   endif
057200070424
057300070424      * ovrdbf azorg01l di filiale
057400070424     c                   clear                   command
057500070424     c  n01              movea     cmd(2)        command
057600070424     c   01              movea     cmd(4)        command
057700070424     c                   call      'QCMDEXC'
057800070424     c                   parm                    command
057900070424     c                   parm                    lung
058000070424     c                   open      azorg01l
058100070424
058200070424      * imposto subito il flag in organigramma di filiale
058300070424     c                   clear                   og148
058400070508     c     w1cfil        chain     azorg01l
058500070424     c                   if        %found(azorg01l)
058600070424     c                   eval      og148 = orgde8
058700070525      * fase 1a
058800070525     c                   if        w1fas1a = 'S'
058900070525     c                   eval      §ogtlls = 'D'
059000070525     c                   endif
059100070525      * fase 1b
059200070525     c                   if        w1fas1b = 'S'
059300070502     c                   eval      §ogtlls = 'S'
059400070502     c                   endif
059500070525      * fase 2
059600070525     c                   if        w1fas2 = 'S'
059700070424     c                   eval      §oggeot = 'S'
059800070502     c                   endif
059900070424     c                   eval      orgde8 = og148
060000070424     c                   update    azorg
060100070424     c                   endif
060200070424
060300070424      * chiudo il file aperto
060400070424     c                   if        %open(azorg01l)
060500070424     c                   close     azorg01l
060600070424     c                   endif
060700070424
060800070424      * ovrdbf azorg01l di sede
060900070424     c                   clear                   command
061000070424     c  n01              movea     cmd(3)        command
061100070424     c   01              movea     cmd(5)        command
061200070424     c                   call      'QCMDEXC'
061300070424     c                   parm                    command
061400070424     c                   parm                    lung
061500070424     c                   open      azorg01l
061600070424
061700070424      * imposto subito il flag in organigramma di filiale
061800070424     c                   clear                   og148
061900070508     c     w1cfil        chain     azorg01l
062000070424     c                   if        %found(azorg01l)
062100070424     c                   eval      og148 = orgde8
062200070525      * fase 1a
062300070525     c                   if        w1fas1a = 'S'
062400070525     c                   eval      §ogtlls = 'D'
062500070525     c                   endif
062600070525      * fase 1b
062700070525     c                   if        w1fas1b = 'S'
062800070502     c                   eval      §ogtlls = 'S'
062900070502     c                   endif
063000070525      * fase 2
063100070525     c                   if        w1fas2 = 'S'
063200070424     c                   eval      §oggeot = 'S'
063300070502     c                   endif
063400070424     c                   eval      orgde8 = og148
063500070424     c                   update    azorg
063600070424     c                   endif
063700070424
063800070424     c                   endsr
063900070508
064000070508      *------------------------------------------------------------------------*
064100070508      *  aggiorna/scrive la tebella GEO
064200070508      *------------------------------------------------------------------------*
064300070508     c     sr_tabgeo     begsr
064400070508
064500070508      * aggiorno la tabella con tutte le filiali attivate
064600070508     c                   eval      ktbecod = 'GEO'
064700070508     c                   movel(p)  w1sfil        ktbeke1
064800070508     c     ktntbe        chain     tntbe01l
064900070525     c                   z-add     *date         w0080
065000070508      * scrivo
065100070508     c                   if        not %found(tntbe01l)
065200070508     c                   clear                   tntbe000
065300070508     c                   clear                   dgeo
065400070508     c                   eval      tbecod = ktbecod
065500070508     c                   eval      tbeke1 = ktbeke1
065600070525      * fase 1a
065700070525     c                   if        w1fas1a = 'S'
065800070525     c                   movel     w0080         d§geofas1a
065900070525     c                   endif
066000070525      * fase 1b
066100070525     c                   if        w1fas1b = 'S'
066200070525     c                   movel     w0080         d§geofas1b
066300070508     c                   endif
066400070525      * fase 2
066500070525     c                   if        w1fas2 = 'S'
066600070525     c                   movel     w0080         d§geofas2
066700070508     c                   endif
066800070508     c                   eval      tbeuni = dgeo
066900070508     c                   write     tntbe000
067000070508      * aggiorno
067100070508     c                   else
067200070508     c                   eval      dgeo = tbeuni
067300070525      * fase 1a
067400070525     c                   if        w1fas1a = 'S'
067500070525     c                   movel     w0080         d§geofas1a
067600070525     c                   endif
067700070525      * fase 1b
067800070525     c                   if        w1fas1b = 'S'
067900070525     c                   movel     w0080         d§geofas1b
068000070508     c                   endif
068100070525      * fase 2
068200070525     c                   if        w1fas2 = 'S'
068300070525     c                   movel     w0080         d§geofas2
068400070508     c                   endif
068500070508     c                   eval      tbeuni = dgeo
068600070508     c                   update    tntbe000
068700070508     c                   endif
068800070508
068900070508     c                   endsr
069000070502
069100070502      *------------------------------------------------------------------------*
069200070502      *  pulisco fiarg00f
069300070502      *------------------------------------------------------------------------*
069400070612     c**!!!sr_fiarg      begsr
069500070502
069600070612     c**!!!w1sfil        setll     fiarg02l
069700070612     c**!!!              do        *hival
069800070612     c**!!!w1sfil        reade     fiarg02l
069900070502      * fine file
070000070612     c**!!!              if        %eof(fiarg02l)
070100070612     c**!!!              leave
070200070612     c**!!!              endif
070300070502      * pulisco i campi necessari
070400070612     c**!!!              clear                   argatb
070500070612     c**!!!              clear                   argidb
070600070612     c**!!!              clear                   argttl
070700070612     c**!!!              clear                   argtgi
070800070612     c**!!!              clear                   argcgi
070900070612     c**!!!              clear                   argsqg
071000070612     c**!!!              clear                   argnst
071100070612     c**!!!              clear                   argxlo
071200070612     c**!!!              clear                   argyla
071300070612     c**!!!              clear                   arglno
071400070612     c**!!!              clear                   argerr
071500070612     c**!!!              clear                   argsco
071600070612     c**!!!              clear                   argpdc
071700070612     c**!!!              clear                   argfgs
071800070612     c**!!!              clear                   argndc
071900070612     c**!!!              clear                   argddc
072000070612     c**!!!              clear                   argstp
072100070612     c**!!!              clear                   argnftl
072200070612     c**!!!              clear                   argslb
072300070612     c**!!!              clear                   argdtvdis
072400070612     c**!!!              clear                   arghvdis
072500070612     c**!!!              clear                   argdtvtel
072600070612     c**!!!              clear                   arghvtel
072700070502
072800070502      * aggiorno
072900070612     c**!!!              update    fiarg000
073000070612     c**!!!              enddo
073100070502
073200070612     c**!!!              endsr
073300070308
073400070308      *------------------------------------------------------------------------*
073500070328      *  blocco le spedizioni
073600070308      *------------------------------------------------------------------------*
073700070323     c     sr_blocca     begsr
073800070308
073900070323     c                   eval      *in99 = *off
074000070308     c                   clear                   wtot
074100070323     c                   clear                   wtot1
074200070323     c                   clear                   wtot2
074300070323     c                   clear                   wtot3
074400070328     c                   clear                   wtot4
074500070413     c                   clear                   wtot5
074600031017
074700070328      * leggo bolle arrivo
074800070323     c     karb          setll     fnarb54l
074900070329    1c                   do        *hival
075000070323     c     karb          reade     fnarb54l
075100070323
075200070323      * fine file
075300070323     c                   if        %eof(fnarb54l)
075400070323     c                   leave
075500070323     c                   endif
075600070328
075700070328      * non elaboro se bolla di recupero
075800070328     c                   eval      tblkut = 1
075900070328     c                   eval      tblcod = '3A'
076000070328     c                   eval      tblkey = arbcbo
076100070328     c                   clear                   ds3a
076200070328     c     ktab          chain     tabel00f
076300070328     c                   if        %found(tabel00f) and tblflg = *blanks
076400070328     c                   eval      ds3a = tbluni
076500070328     c                   endif
076600070328     c                   if           §3arbl = 'R'
076700070328     c                   iter
076800070328     c                   endif
076900070323
077000070413      * pulisco i dati di work
077100070413     c                   clear                   errore
077200070507     c                   clear                   causa
077300070328     c                   clear                   wb4lbasa
077400070328     c                   clear                   wb4lbass
077500070323     c                   clear                   wb4lbge
077600070323     c                   clear                   wb4ltca
077700070323     c                   clear                   wb4ltfd
077800070503
077900070503      * --> per prima cosa controllo se ha già il rcd L su FIAR400F
078000070503     c     kar4          chain     fiar401l
078100070507    2c                   if        %found(fiar401l)
078200070503     c                   eval      dsbl4l = ar4not
078300070503      *    non elaboro se ha già il blocco sospensione
078400070503     c                   if        §b4lbge = 'S'
078500070503     c                   iter
078600070503     c                   endif
078700070507      *    non elaboro ma stampo errore se ha altri blocchi
078800070503     c                   if        §b4lbas <> *blanks or §b4ltca <> *blanks or
078900070503     c                             §b4ltfd <> *blanks
079000070503     c                   eval      errore = 'ALTRI BLOCCHI'
079100070503     c                   exsr      sr_staerr
079200070503     c                   iter
079300070503     c                   endif
079400070507    2c                   endif
079500070503      *     se non c'è rcd L su FIAR400F vado avanti
079600070328
079700070507      * --> blocco in sospensione se bolla con almeno una C.A.
079800070328     c                   clear                   fidn12ds
079900070328     c                   eval      i12aas = arbaas
080000070328     c                   eval      i12lnp = arblnp
080100070328     c                   eval      i12nrs = arbnrs
080200070328     c                   eval      i12nsp = arbnsp
080300070328     c                   eval      i12fel = 'E'
080400070328     c                   eval      i12fan = 'E'
080500070328     c                   call      'FIDN12R'
080600070328     c                   parm                    fidn12ds
080700070507      *     se non ci sono errori e trova almeno una c.a.
080800070507    2c                   if        o12err = ' ' and o12nca > *zeros
080900070507      *     blocco sospensione se solo c.a. aperte
081000070507     c                   if        o12cch = *blanks
081100070328     c                   eval      wb4lbge = 'S'
081200070507     c                   eval      causa = 'SOLO C.A. APERTE'
081300070507     c                   goto      scriviar4
081400070507     c                   endif
081500070507      *     blocco sospensione se anche solo una c.a. chiusa ma no lasciato avviso o giacenza
081600070507      *     (arbfbc = ' ')
081700070507     c                   if        arbfbc = *blanks
081800070507     c                   eval      wb4lbge = 'S'
081900070507     c                   eval      causa = 'ALMENO UNA C.A. CHIUSA'
082000070507     c                   goto      scriviar4
082100070507     c                   endif
082200070507      *     se arrivo qua vuol dire che anche se c'è una c.a. su questa bolla devo continuare
082300070507      *     con gli altri blocchi e non la metto in sospensione
082400070507    2c                   endif
082500070507
082600070507      * --> blocco per lasciato avviso
082700070507    2c                   if        arbfbc = 'A'
082800070507     c     kevb          setgt     fnevb05l
082900070507     c     kevb          readpe    fnevb05l
083000070507    3c                   if        not %eof(fnevb05l)
083100070507      * se c'è già il rcd su EVB2 stampo errore
083200070507     c     kevb2         chain     fnevb22l
083300070507    4c                   if        %found(fnevb22l)
083400070507     c                   eval      errore = 'FNEVB20F'
083500070507     c                   exsr      sr_staerr
083600070507     c                   else
083700070507      * scrivo fnevb2
083800070507     c                   clear                   fnevb200
083900070507     c                   eval      evb2aas = evbaas
084000070507     c                   eval      evb2lnp = evblnp
084100070507     c                   eval      evb2nrs = evbnrs
084200070507     c                   eval      evb2nsp = evbnsp
084300070507     c                   eval      evb2cev = evbcev
084400070507     c                   eval      evb2dtv = evbdtv
084500070507     c                   eval      evb2orv = evborv
084600070507     c                   eval      evb2fle = evbfle
084700070507     c                   eval      evb2dev = evbdev
084800070507     c                   eval      evb2hev = evbhev
084900070507     c                   eval      evb2tel = 'S'
085000070507     c                   write     fnevb200
085100070507      * stampo
085200070507     c                   exsr      sr_stampa5
085300070507      * sommo
085400070507     c                   add       1             wtot5
085500070507    4c                   endif
085600070507    3c                   endif
085700070507    2c                   endif
085800070328
085900070323      * per bolle non in distinta
086000070507    2c                   if        arbndc = *zeros
086100070328
086200070323      * imposto i seguenti blocchi:
086300070328
086400070420      * --> consegna particolare appuntamento (No Giacenza e No data cons.rich.)
086500070420     c                   if        (arbtc1 = 'A' or arbtc2 = 'A') and
086600070420     c                              arbfbc <> 'G' and arbdcr <= *zeros
086700070420     c                   eval      wb4lbasa = 'S'
086800070420     c                   endif
086900070420
087000070420      * --> consegna particolare supermercato (non tassativa e No Giacenza)
087100070420     c                   if        (arbtc1 = 'S' or arbtc2 = 'S') and
087200070420     c                              arbfbc <> 'G'
087300070329     c                   clear                   fnlv80ds
087400070329      *     se data consegna richiesta IL e la data è <= oggi BLOCCO
087500070525     c                   if        arbdcr <= *date and arbtcr = *blanks
087600070329     c                   else
087700070329      *     controllo se tassativa
087800070329     c                   if        arbdcr <> *zeros
087900070329     c                   eval      ilv80aas = arbaas
088000070329     c                   eval      ilv80lnp = arblnp
088100070329     c                   eval      ilv80nrs = arbnrs
088200070329     c                   eval      ilv80nsp = arbnsp
088300070329     c                   eval      ilv80mgs = arbmgs
088400070329     c                   eval      ilv80tc1 = arbtc1
088500070329     c                   eval      ilv80tc2 = arbtc2
088600070329     c                   eval      ilv80dcr = arbdcr
088700070329     c                   eval      ilv80hcr = arbhcr
088800070329     c                   eval      ilv80tcr = arbtcr
088900070329     c                   call      'FNLV80R'
089000070329     c                   parm                    kpjba
089100070329     c                   parm                    fnlv80ds
089200070329     c                   parm                    ilv80newproc
089300070329     c                   endif
089400070329     c                   endif
089500070329      *     blocco se consegna NO tassativa
089600070329     c                   if        olv80tat <> 'S'
089700070329     c                   eval      wb4lbass = 'S'
089800070329     c                   endif
089900070329     c                   endif
090000070328
090100070323      * --> contrassegno non abilitato
090200070323     c                   if        arbacc = *blanks
090300070323     c                   eval      wb4ltca = 'S'
090400070323     c                   endif
090500070328
090600070420      * --> fermo deposito (No Giacenza)
090700070420     c                   if        arbffd <> *blanks and arbfbc <> 'G'
090800070323     c                   eval      wb4ltfd = 'S'
090900070323     c                   endif
091000070329    2c                   endif
091100070507
091200070507     c     scriviar4     tag
091300070328
091400070328      * scrivo record L di fiar4 per la gestione dei blocchi
091500070329    2c                   if        wb4lbasa <> *blanks or
091600070328     c                             wb4lbass <> *blanks or
091700070328     c                             wb4lbge <> *blanks or
091800070328     c                             wb4ltca <> *blanks or
091900070328     c                             wb4ltfd <> *blanks
092000070328
092100070328      * se esiste già il rcd L su fiar4 stampo errore e torno a leggere una nuova bolla
092200070328     c     kar4          chain     fiar401l
092300070328     c                   if        %found(fiar401l)
092400070413     c                   eval      errore = 'FIAR400F'
092500070328     c                   exsr      sr_staerr
092600070328     c                   iter
092700070328     c                   endif
092800070328
092900070328      * scrivo rcd L su fiar4 e stampo
093000070328     c                   clear                   dsbl4l
093100070328     c                   clear                   fiar4000
093200070328     c                   eval      ar4aas = arbaas
093300070328     c                   eval      ar4lnp = arblnp
093400070328     c                   eval      ar4nrs = arbnrs
093500070328     c                   eval      ar4nsp = arbnsp
093600070328     c                   eval      ar4trc = 'L'
093700070328      * se blocco sospensione non imposto gli altri blocci
093800070329     c                   if        wb4lbge <> *blanks
093900070328     c                   eval      §b4lbge = 'S'
094000070329     c                   else
094100070328     c                   if        wb4lbasa <> *blanks or wb4lbass <> *blanks
094200070328     c                   eval      §b4lbas = 'S'
094300070328     c                   endif
094400070328     c                   eval      §b4ltca = wb4ltca
094500070328     c                   eval      §b4ltfd = wb4ltfd
094600070329     c                   endif
094700070328     c                   eval      ar4not = dsbl4l
094800070525     c                   eval      ar4duv = *date
094900070328     c                   eval      ar4ftr = 'R'
095000070525     c                   eval      ar4dtr = *date
095100070328      * scrivo
095200070328     c                   write     fiar4000
095300070328
095400070328      * stampo blocco e sommo
095500070328      * --> appuntamento
095600070328     c                   if        wb4lbasa <> *blanks
095700070328     c                   exsr      sr_stampa
095800070328      * sommo
095900070328     c                   add       1             wtot
096000070328     c                   endif
096100070328      * --> supermercato
096200070328     c                   if        wb4lbass <> *blanks
096300070328     c                   exsr      sr_stampa4
096400070328      * sommo
096500070328     c                   add       1             wtot4
096600070328     c                   endif
096700070328      * --> sospensione
096800070328     c                   if        wb4lbge <> *blanks
096900070328      * scrivo la nota
097000070328     c                   exsr      sr_nota
097100070328     c                   exsr      sr_stampa1
097200070328      * sommo
097300070328     c                   add       1             wtot1
097400070328     c                   endif
097500070328      * --> contrassegno
097600070328     c                   if        wb4ltca <> *blanks
097700070328     c                   exsr      sr_stampa2
097800070328      * sommo
097900070328     c                   add       1             wtot2
098000070328     c                   endif
098100070328      * --> fermo deposito
098200070328     c                   if        wb4ltfd <> *blanks
098300070328     c                   exsr      sr_stampa3
098400070328      * sommo
098500070328     c                   add       1             wtot3
098600070328     c                   endif
098700070329    2c                   endif
098800070329
098900070329    1c                   enddo
099000070328
099100070328      * stampo il totale delle bolle bloccate
099200070328     c                   if        wtot > *zeros
099300070328     c                   exsr      sr_tot
099400070328     c                   clear                   wtot
099500070328     c                   endif
099600070328     c                   if        wtot4 > *zeros
099700070328     c                   exsr      sr_tot4
099800070328     c                   clear                   wtot4
099900070328     c                   endif
100000070328     c                   if        wtot1 > *zeros
100100070328     c                   exsr      sr_tot1
100200070328     c                   clear                   wtot1
100300070328     c                   endif
100400070328     c                   if        wtot2 > *zeros
100500070328     c                   exsr      sr_tot2
100600070328     c                   clear                   wtot2
100700070328     c                   endif
100800070328     c                   if        wtot3 > *zeros
100900070328     c                   exsr      sr_tot3
101000070328     c                   clear                   wtot3
101100070328     c                   endif
101200070413     c                   if        wtot5 > *zeros
101300070413     c                   exsr      sr_tot5
101400070413     c                   clear                   wtot5
101500070413     c                   endif
101600070328
101700070328     c                   endsr
101800070323
101900070323      *------------------------------------------------------------------------*
102000070323      * stampa errore
102100070323      *------------------------------------------------------------------------*
102200070323     c     sr_staerr     begsr
102300070323
102400070323      * apro il file
102500070323     c                   if        not %open(qsysprt)
102600070323     c                   open      qsysprt
102700070323     c                   endif
102800070323      * stampo
102900070323     c                   if        not *in99 or *inof
103000070323     c                   except    testa
103100070323     c  n99              eval      *in99 = *on
103200070323     c                   endif
103300070323      * errore
103400070323     c                   except    rigaerr
103500070323
103600070323     c                   endsr
103700070323
103800070323      *------------------------------------------------------------------------*
103900070328      * stampa blocco appuntamento
104000070323      *------------------------------------------------------------------------*
104100070323     c     sr_stampa     begsr
104200070323
104300070323      * apro il file
104400070323     c                   if        not %open(fidg98p)
104500070323     c                   open      fidg98p
104600070323     c                   eval      *in10 = *on
104700070323     c                   endif
104800070323
104900070323      * stampo
105000070323     c                   if        *in10
105100070323     c                   write     dg98p01
105200070323     c                   eval      *in10 = *off
105300070323     c                   endif
105400070323     c                   write     dg98p02
105500070323
105600070323     c                   endsr
105700070328
105800070328      *------------------------------------------------------------------------*
105900070328      * stampa blocco supermercato
106000070328      *------------------------------------------------------------------------*
106100070328     c     sr_stampa4    begsr
106200070328
106300070328      * apro il file
106400070328     c                   if        not %open(fidg98p4)
106500070328     c                   open      fidg98p4
106600070328     c                   eval      *in14 = *on
106700070328     c                   endif
106800070328
106900070328      * stampo
107000070328     c                   if        *in14
107100070328     c                   write     dg98p401
107200070328     c                   eval      *in14 = *off
107300070328     c                   endif
107400070328     c                   write     dg98p402
107500070328
107600070328     c                   endsr
107700070323
107800070323      *------------------------------------------------------------------------*
107900070328      * stampa blocco sospensione
108000070323      *------------------------------------------------------------------------*
108100070323     c     sr_stampa1    begsr
108200070323
108300070323      * apro il file
108400070323     c                   if        not %open(fidg98p1)
108500070323     c                   open      fidg98p1
108600070323     c                   eval      *in11 = *on
108700070323     c                   endif
108800070323
108900070323      * stampo
109000070323     c                   if        *in11
109100070323     c                   write     dg98p101
109200070323     c                   eval      *in11 = *off
109300070323     c                   endif
109400070323     c                   write     dg98p102
109500070323
109600070323     c                   endsr
109700070323
109800070323      *------------------------------------------------------------------------*
109900070323      * stampa blocco contrassegno
110000070323      *------------------------------------------------------------------------*
110100070323     c     sr_stampa2    begsr
110200070323
110300070323      * apro il file
110400070323     c                   if        not %open(fidg98p2)
110500070323     c                   open      fidg98p2
110600070323     c                   eval      *in12 = *on
110700070323     c                   endif
110800070323
110900070323      * stampo
111000070323     c                   if        *in12
111100070323     c                   write     dg98p201
111200070323     c                   eval      *in12 = *off
111300070323     c                   endif
111400070323     c                   write     dg98p202
111500070323
111600070323     c                   endsr
111700070323
111800070323      *------------------------------------------------------------------------*
111900070323      * stampa blocco fermo deposito
112000070323      *------------------------------------------------------------------------*
112100070323     c     sr_stampa3    begsr
112200070323
112300070323      * apro il file
112400070323     c                   if        not %open(fidg98p3)
112500070323     c                   open      fidg98p3
112600070323     c                   eval      *in13 = *on
112700070323     c                   endif
112800070323
112900070323      * stampo
113000070323     c                   if        *in13
113100070323     c                   write     dg98p301
113200070323     c                   eval      *in13 = *off
113300070323     c                   endif
113400070323     c                   write     dg98p302
113500070323
113600070323     c                   endsr
113700070413
113800070413      *------------------------------------------------------------------------*
113900070413      * stampa blocco lasciato avviso
114000070413      *------------------------------------------------------------------------*
114100070413     c     sr_stampa5    begsr
114200070413
114300070413      * apro il file
114400070413     c                   if        not %open(fidg98p5)
114500070413     c                   open      fidg98p5
114600070413     c                   eval      *in15 = *on
114700070413     c                   endif
114800070413
114900070413      * stampo
115000070413     c                   if        *in15
115100070413     c                   write     dg98p501
115200070413     c                   eval      *in15 = *off
115300070413     c                   endif
115400070413     c                   write     dg98p502
115500070413
115600070413     c                   endsr
115700070323
115800070323      *------------------------------------------------------------------------*
115900070323      * creo nota per blocco generico
116000070323      *------------------------------------------------------------------------*
116100070323     c     sr_nota       begsr
116200070323
116300070323     c                   clear                   fiarn000
116400070323     c                   eval      arnaas = arbaas
116500070323     c                   eval      arnlnp = arblnp
116600070323     c                   eval      arnnrs = arbnrs
116700070323     c                   eval      arnnsp = arbnsp
116800070323     c                   eval      arncdn = 'BGE'
116900070323     c                   eval      arnpno = 1
117000070323     c                   eval      arnnob = 'Impostata in AUTOMATICO'
117100070525     c                   eval      arndim = *date
117200070323     c                   time                    arnoim
117300070323     c                   eval      arnpru = 'EDPCED'
117400070323     c                   eval      arnpgm = 'FIDG98R'
117500070416     c                   eval      arnfl1 = 'S'
117600070323     c                   write     fiarn000
117700070323
117800070323     c                   endsr
117900070323
118000070323      *------------------------------------------------------------------------*
118100070328      * stampa totale blocco appuntamento
118200070323      *------------------------------------------------------------------------*
118300070323     c     sr_tot        begsr
118400070323
118500070323     c                   if        *in10
118600070323     c                   write     dg98p01
118700070323     c                   eval      *in10 = *off
118800070323     c                   endif
118900070323
119000070323     c                   write     dg98p03
119100070323     c                   write     dg98pf
119200070323     c                   close     fidg98p
119300070323
119400070323     c                   endsr
119500070328
119600070328      *------------------------------------------------------------------------*
119700070328      * stampa totale blocco supermercato
119800070328      *------------------------------------------------------------------------*
119900070328     c     sr_tot4       begsr
120000070328
120100070328     c                   if        *in14
120200070328     c                   write     dg98p401
120300070328     c                   eval      *in14 = *off
120400070328     c                   endif
120500070328
120600070328     c                   write     dg98p403
120700070328     c                   write     dg98p4f
120800070328     c                   close     fidg98p4
120900070328
121000070328     c                   endsr
121100070323
121200070323      *------------------------------------------------------------------------*
121300070323      * stampa totale blocco generale
121400070323      *------------------------------------------------------------------------*
121500070323     c     sr_tot1       begsr
121600070323
121700070323     c                   if        *in11
121800070323     c                   write     dg98p101
121900070323     c                   eval      *in11 = *off
122000070323     c                   endif
122100070323
122200070323     c                   write     dg98p103
122300070323     c                   write     dg98p1f
122400070323     c                   close     fidg98p1
122500070323
122600070323     c                   endsr
122700070323
122800070323      *------------------------------------------------------------------------*
122900070323      * stampa totale blocco contrassegno
123000070323      *------------------------------------------------------------------------*
123100070323     c     sr_tot2       begsr
123200070323
123300070323     c                   if        *in12
123400070323     c                   write     dg98p201
123500070323     c                   eval      *in12 = *off
123600070323     c                   endif
123700070323
123800070323     c                   write     dg98p203
123900070323     c                   write     dg98p2f
124000070323     c                   close     fidg98p2
124100070323
124200070323     c                   endsr
124300070323
124400070323      *------------------------------------------------------------------------*
124500070323      * stampa totale blocco fermo deposito
124600070323      *------------------------------------------------------------------------*
124700070323     c     sr_tot3       begsr
124800070323
124900070323     c                   if        *in13
125000070323     c                   write     dg98p301
125100070323     c                   eval      *in13 = *off
125200070323     c                   endif
125300070323
125400070323     c                   write     dg98p303
125500070323     c                   write     dg98p3f
125600070323     c                   close     fidg98p3
125700070323
125800070323     c                   endsr
125900070413
126000070413      *------------------------------------------------------------------------*
126100070413      * stampa totale blocco lasciato avviso
126200070413      *------------------------------------------------------------------------*
126300070413     c     sr_tot5       begsr
126400070413
126500070413     c                   if        *in15
126600070413     c                   write     dg98p501
126700070413     c                   eval      *in15 = *off
126800070413     c                   endif
126900070413
127000070413     c                   write     dg98p503
127100070413     c                   write     dg98p5f
127200070413     c                   close     fidg98p5
127300070413
127400070413     c                   endsr
127500070308
127600070308      *------------------------------------------------------------------------*
127700070308      * routine iniziale
127800070308      *------------------------------------------------------------------------*
127900070308     c     *inzsr        begsr
128000070308
128100070308     c     *entry        plist
128200070424     c                   parm                    kpjba
128300070424
128400070424      * ambiente di prova
128500070424     c                   eval      *in01 = (%subst(knsif:7:1) = 'P')
128600070502
128700070502      * utente EDP
128800070502     c                   eval      *in02 = (%subst(knmus:1:3) = 'EDP')
128900070424
129000070424     c     *dtaara       define    §azute        azuteds
129100070424     c     *dtaara       define    §datiute      ddatiute
129200070424     c                   in(e)     *dtaara
129300070424     c                   if        %error or rsut = *blanks
129400070424     c                   clear                   tibs34ds
129500070424     c                   call      'TIBS34R'
129600070424     c                   parm                    tibs34ds
129700070424     c                   in        *dtaara
129800070424     c                   endif
129900070528
130000070528      * imposto la filiale utente a video
130100070528     c                   eval      w1cfil = dutpou
130200070528     c                   eval      w1dfil = dutdpo
130300070328
130400070328     c     ktab          klist
130500070328     c                   kfld                    tblkut
130600070328     c                   kfld                    tblcod
130700070328     c                   kfld                    tblkey
130800070323
130900070323     c     karb          klist
131000070424     c                   kfld                    w1sfil
131100070323     c                   kfld                    wdcm
131200070413
131300070323     c     kar4          klist
131400070323     c                   kfld                    arbaas
131500070323     c                   kfld                    arblnp
131600070323     c                   kfld                    arbnrs
131700070323     c                   kfld                    arbnsp
131800070323     c                   kfld                    ktrc
131900070413
132000070413     c     kevb          klist
132100070413     c                   kfld                    arbaas
132200070413     c                   kfld                    arblnp
132300070413     c                   kfld                    arbnrs
132400070413     c                   kfld                    arbnsp
132500070413
132600070413     c     kevb2         klist
132700070413     c                   kfld                    evbaas
132800070413     c                   kfld                    evblnp
132900070413     c                   kfld                    evbnrs
133000070413     c                   kfld                    evbnsp
133100070413     c                   kfld                    evbdtv
133200070413     c                   kfld                    evborv
133300070419     c                   kfld                    evbcev
133400070424
133500070424     c     ktntbe        klist
133600070424     c                   kfld                    ktbecod
133700070424     c                   kfld                    ktbeke1
133800070308
133900070424      * ovrprtf file di stampa
134000070323     c                   clear                   command
134100070323     c                   movea     cmd(1)        command
134200070308     c                   call      'QCMDEXC'
134300070308     c                   parm                    command
134400070308     c                   parm                    lung
134500070308
134600070424      * ovrdbf azorg01l di filiale
134700070424     c                   clear                   command
134800070424     c  n01              movea     cmd(2)        command
134900070424     c   01              movea     cmd(4)        command
135000070424     c                   call      'QCMDEXC'
135100070424     c                   parm                    command
135200070424     c                   parm                    lung
135300070424     c                   open      azorg01l
135400070424
135500070308     c                   endsr
135600070323
135700070323     oqsysprt   e            testa          2 02
135800070323     o                                         +  5 'Blocca spedizioni per'
135900070323     o                                         +  1 'attivazione telefonate'
136000070323     o                                         +  1 'GEO'
136100070424     o                       w1sfil            +  1
136200070424     o                       w1sdfil           +  1
136300070323     o                                           85 'FIDG98R'
136400070525     o                       *date         y    110
136500070323     o                                          128 'Pag.'
136600070323     o                       page          z    132
136700070323     o          e            rigaerr        1
136800070323     o                                           +1 'Spedizione:'
136900070323     o                       arbaas              +1
137000070323     o                       arblnp              +1
137100070323     o                       arbnrs        z     +1
137200070323     o                       arbnsp        z     +1
137300070413     o                       errore              +1
137400070323** CMD   (Lunga 80)                                                            *
137500070323OVRPRTF QSYSPRT SAVE(*YES)                                                       1
137600070424OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRU/AZORG01L)                                 2
137700070424OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRU/AZORG01L)                                 3
137800070424OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRPF/AZORG01L)                                4
137900070424OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRPS/AZORG01L)                                5
