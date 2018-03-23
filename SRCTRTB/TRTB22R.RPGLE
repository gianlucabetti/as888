000100060418     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060418
000300060418      *--------------------------------------------------------------*
000400060418      * GESTIONE TABELLA CODICI DI TASSAZIONE                        *
000500060418      *--------------------------------------------------------------*
000600060418
000700060427     fazlin01l  if   e           k disk
000800060418     ftabel00f  uf a e           k disk
000900060427     ftrtb22d   cf   e             workstn
001000060427     f                                     sfile(tb22s1:nrr)
001100060418
001200060418      *---------------------------------------------------------------*
001300060418      *  RIEPILOGO INDICATORI                                         *
001400060418      *---------------------------------------------------------------*
001500060427      * 01 - inserimento
001600060427      * 02 - variazione
001700060427      * 03 - annullato
001800060427      * 05 - visualizzo subfile lingue
001900060427      * 06 - ambiente di filiale
002000060427      * 20 - GESTIONE SUBFILE
002100060427      * 21 - GESTIONE SUBFILE
002200060427      * 22 - GESTIONE SUBFILE
002300060427      * 28 - visualizza messaggio errore
002400060427      * 30 - Fine file
002500060427      * 31 - comodo
002600060427      * 32 - comodo
002700060427      * 40 - codice tabella
002800060427      * 41 - descrizione bartolini
002900060427      * 42 - codice evento
003000060427      * 43 - errore subfile
003100060418      *--------------------------------------------------------------*
003200060418
003300060418      *--------------------------------------------------------------*
003400060418      * VARIABILI
003500060418      *--------------------------------------------------------------*
003600060418     d comando         s              1
003700060426     d kazlin          s                   like(lintabel)
003800060427     d kcod            s                   like(tblcod)
003900060420     d kkey            s                   like(tblkey)
004000060420     d kkut            s                   like(tblkut)
004100060418     d ordina          s              1
004200060531     d savtblftt       s                   like(tblftt)
004300060531     d savtblflt       s                   like(tblflt)
004400060418     d savtbluni       s                   like(tbluni)
004500060418     d nrr             s              4  0
004600060420     d savrec          s                   like(nrr)
004700060418     d wf16            s              1    inz('0')
004800060418     d wf5             s              1    inz('0')
004900060418     d wf6             s              1    inz('0')
005000060418
005100060418      *--------------------------------------------------------------*
005200060418      * SCHIERE
005300060418      *--------------------------------------------------------------*
005400060418     d msg             s             78    dim(10) ctdata perrcd(1)
005500060418
005600060418      *--------------------------------------------------------------*
005700060418      * STRUTTURE DATI
005800060418      *--------------------------------------------------------------*
005900060418     d datar           ds
006000060418     d  aar                    1      2
006100060418     d  mmr                    3      4
006200060418     d  ggr                    5      6
006300060418
006400060418     d datad           ds
006500060418     d  ggd                    1      2
006600060418     d  mmd                    3      4
006700060418     d  aad                    5      6
006800060420
006900060420     d ds_tnsd         ds
007000060420     d  ds_cod                        3
007100060420     d  ds_tip                        1
007200060420     d  ds_des                       25
007300060420     d  ds_pra                        3
007400060418
007500060418     d azuteds       e ds                  extname(azute00f)
007600060418     d ddatiute      e ds
007700060427     d dice          e ds
007800060427     d ds7o          e ds
007900060418     d kpjba         e ds
008000060427     d tibs02ds      e ds
008100060418     d tibs34ds      e ds
008200060427
008300060427     d                sds
008400060427     d  vtcpgm                 1     10
008500060427
008600060427      *--------------------------------------------------------------*
008700060427      *  COSTANTI
008800060418      *--------------------------------------------------------------*
008900060427      *  titolo videata (lunghezza massima 28)
009000060427     d vtctit          c                   const('* Tabella Consegne Anomale *')
009100060427
009200060427      *--------------------------------------------------------------*
009300060418
009400060418     c     inizio        tag
009500060427
009600060426     c                   clear                   v1ccod
009700060426     c                   eval      v1ccod = '?'
009800060418     c                   eval      wf16 = *off
009900060418     c                   eval      wf5 = *off
010000060418     c                   eval      wf6 = *off
010100060427     c                   clear                   vtcmod
010200060418
010300060427      * prima videata
010400060418     c     inz01         tag
010500060427     c                   write     tb22t1
010600060427     c                   exfmt     tb22d1
010700060418     c                   clear                   v1cmsg
010800060427     c                   setoff                                       2840
010900060418      * F3=Fine
011000060418     c   kc              goto      fine
011100060427      * controllo dati videata
011200060427     c                   exsr      sr_ctrd01
011300060418     c                   setoff                                       010203
011400060427     c                   exsr      sr_pulvid
011500060427      * aggancio la tabella scelta
011600060420     c                   eval      kkut = 1
011700060427     c                   eval      kcod = '7O'
011800060427     c                   eval      kkey = v1ccod
011900060420     c     ktab          chain     tabel00f
012000060418      * nuovo codice
012100060418     c                   if        not %found(tabel00f)
012200060418     c                   eval      *in01 = *on
012300060427     c                   eval      vtcmod = 'INSERIMENTO'
012400060418      * codice trovato
012500060418     c                   else
012600060427     c                   eval      ds7o = tbluni
012700060427      * imposto i dati a video
012800060427     c                   exsr      sr_movvid
012900060418      * codice annullato
013000060418     c                   if        tblflg = '*'
013100060418     c                   eval      *in03 = *on
013200060427     c                   eval      vtcmod = 'ANNULLATO  '
013300060418      * codice valido
013400060418     c                   else
013500060418     c                   eval      *in02 = *on
013600060427     c                   eval      vtcmod = 'MODIFICA   '
013700060418     c                   endif
013800060418     c                   endif
013900060418
014000060427      * seconda videata
014100060418     c     inz02         tag
014200060427     c                   write     tb22t1
014300060427     c                   exfmt     tb22d2
014400060418     c                   clear                   v1cmsg
014500060427     c                   setoff                                       28
014600060418      * F3=Fine
014700060418     c   kc              goto      fine
014800060418      * F12=Ritorno
014900060418     c   kl              goto      inz01
015000060426      * F9=Altri dati
015100060426     c                   if        *inki
015200060426     c                   eval      *in05 = *on
015300060426     c                   exsr      sr_lingua
015400060426     c                   eval      *in05 = *off
015500060426     c                   goto      inz02
015600060426     c                   endif
015700060418      * F16=Annullamento
015800060418     c                   if        *inkq
015900060418     c                   eval      wf16 = *on
016000060427     c                   exsr      sr_conferma
016100060418     c                   if        wf6 = *on
016200060418     c                   goto      inizio
016300060418     c                   else
016400060418     c                   eval      wf16 = *off
016500060418     c                   goto      inz02
016600060418     c                   endif
016700060418     c                   endif
016800060418      * F5=Ripristino
016900060418     c                   if        *inke
017000060418     c                   eval      wf5 = *on
017100060427     c                   exsr      sr_conferma
017200060418     c                   if        wf6 = *on
017300060418     c                   goto      inizio
017400060418     c                   else
017500060418     c                   eval      wf5 = *off
017600060418     c                   goto      inz02
017700060418     c                   endif
017800060418     c                   endif
017900060418      * controllo dati
018000060427     c                   exsr      sr_ctrd02
018100060427     c   28
018200060427     cornkf              goto      inz02
018300060418      * F6=Conferma
018400060418     c                   if        *inkf
018500060427     c                   exsr      sr_conferma
018600060418     c                   if        wf6 = *on
018700060418      * tabelle in lingua
018800060426     c                   eval      *in05 = *off
018900060418     c                   exsr      sr_lingua
019000060418     c                   goto      inizio
019100060418     c                   else
019200060418     c                   goto      inz02
019300060418     c                   endif
019400060418     c                   endif
019500060418
019600060418     c     fine          tag
019700060418
019800060418     c                   eval      *inlr = *on
019900060427
020000060427      *---------------------------------------------------------------*
020100060427      * controllo dati videata
020200060427      *---------------------------------------------------------------*
020300060427     c     sr_ctrd01     begsr
020400060427
020500060427      * codice obbligatorio
020600060427     c                   if        v1ccod = *blanks
020700060427     c                   eval      *in28 = *on
020800060427     c                   eval      *in40 = *on
020900060427     c                   eval      v1cmsg = msg(01)
021000060427     c                   goto      inz01
021100060427     c                   endif
021200060427
021300060427      * ricerca codice tassazione
021400060427     c     '?'           scan      v1ccod                                 31
021500060427     c                   if        *in31
021600060427     c                   clear                   v1ccod
021700060427     c                   eval      kcod = '7O'
021800060427     c                   clear                   kkey
021900060427     c                   call      'TRUL19R'
022000060427     c                   parm                    kcod
022100060427     c                   parm                    ordina
022200060427     c                   parm                    kkey
022300060427     c                   parm                    comando
022400060427     c                   eval      v1ccod = kkey
022500060427     c                   goto      inz01
022600060427     c                   endif
022700060427
022800060427     c                   endsr
022900060418
023000060418      *---------------------------------------------------------------*
023100060427      * pulizia campi videata
023200060418      *---------------------------------------------------------------*
023300060427     c     sr_pulvid     begsr
023400060418
023500060427     c                   clear                   v1cdei
023600160502     c                   clear                   v1cdeb
023700060427     c                   clear                   v1cvcp
023800060427     c                   clear                   v1cvca
023900060427     c                   clear                   v1cfdi
024000060427     c                   clear                   v1cfst
024100060427     c                   clear                   v1cres
024200060427     c                   clear                   v1cidd
024300060427     c                   clear                   v1cinc
024400060427     c                   clear                   v1cincp
024500060427     c                   clear                   v1cicv
024600060427     c                   clear                   v1ccev
024700060427     c                   clear                   v1cfpt
024800060427     c                   clear                   v1crms
024900060427     c                   clear                   v1c139
025000060427     c                   clear                   v1cpodimg
025100060427     c                   clear                   v1ctle
025200060427     c                   clear                   descev
025300060418
025400060418     c                   endsr
025500060427
025600060427      *---------------------------------------------------------------*
025700060427      * imposto i dati a video
025800060427      *---------------------------------------------------------------*
025900060427     c     sr_movvid     begsr
026000060427
026100060427     c                   eval      v1cdei = §7odei
026200160502     c                   eval      v1cdeb = §7odeb
026300060427     c                   eval      v1cvcp = §7ovcp
026400060427     c                   eval      v1cvca = §7ovca
026500060427     c                   eval      v1cfdi = §7ofdi
026600060427     c                   eval      v1cfst = §7ofst
026700060427     c                   eval      v1cres = §7ores
026800060427     c                   eval      v1cidd = §7oidd
026900060427     c                   eval      v1cinc = §7oinc
027000060427     c                   eval      v1cincp = §7oincp
027100060427     c                   eval      v1cicv = §7oicv
027200060427     c                   eval      v1ccev = §7ocev
027300060427     c                   eval      v1cfpt = §7ofpt
027400060427     c                   eval      v1crms = §7orms
027500060427     c                   eval      v1c139 = §7o139
027600060427     c                   eval      v1cpodimg = §7opodimg
027700060427     c                   eval      v1ctle = §7otle
027800060427      * descrizione codice evento
027900060427      * se sono in ambiente di sede
028000060427     c                   if        v1ccev <> *blanks and not *in06
028100060427     c                   clear                   descev
028200060427     c                   clear                   dice
028300060427     c                   clear                   tibs02ds
028400060427     c                   eval      t02mod = 'C'
028500060427     c                   eval      t02sif = knsif
028600060427     c                   eval      t02cod = 'ICE'
028700060427     c                   eval      t02ke1 = v1ccev
028800060427     c                   call      'TIBS02R'
028900060427     c                   parm                    kpjba
029000060427     c                   parm                    tibs02ds
029100060427     c                   if        t02err = *blanks
029200060427     c                   eval      dice = t02uni
029300060427     c                   endif
029400060427     c                   eval      descev = §icedei
029500060427     c                   endif
029600060427      * se ambiente di filiale
029700060427     c   06              eval      descev = 'Tabella solo di sede'
029800060427
029900060427     c                   endsr
030000060418
030100060418      *---------------------------------------------------------------*
030200060427      * controllo dati videata
030300060418      *---------------------------------------------------------------*
030400060427     c     sr_ctrd02     begsr
030500060427     c                   setoff                                       4142
030600060418
030700060427      * descrizione bartolini
030800060427     c                   if        v1cdei = *blanks
030900060418     c                   eval      *in28 = *on
031000060418     c                   eval      *in41 = *on
031100060418     c                   eval      v1cmsg = msg(02)
031200060418     c                   leavesr
031300060418     c                   endif
031400060427
031500060427      * codice evento
031600060427      * se sono in ambiente di sede
031700060427     c                   if        v1ccev <> *blanks and not *in06
031800060427      * ricerca
031900060427     c     '?'           scan      v1ccev                                 30
032000060427     c                   if        *in30
032100060427     c                   seton                                        42
032200060427     c                   clear                   descev
032300060427     c                   clear                   dice
032400060427     c                   clear                   tibs02ds
032500060427     c                   eval      t02mod = 'R'
032600060427     c                   eval      t02sif = knsif
032700060427     c                   eval      t02cod = 'ICE'
032800060427     c                   call      'TIBS02R'
032900060427     c                   parm                    kpjba
033000060427     c                   parm                    tibs02ds
033100060427     c                   eval      v1ccev = t02ke1
033200060427     c                   eval      dice = t02uni
033300060427     c                   eval      descev = §icedei
033400060427     c                   goto      inz02
033500060427     c                   endif
033600060427      * controllo
033700060427     c                   clear                   descev
033800060427     c                   clear                   dice
033900060427     c                   clear                   tibs02ds
034000060427     c                   eval      t02mod = 'C'
034100060427     c                   eval      t02sif = knsif
034200060427     c                   eval      t02cod = 'ICE'
034300060427     c                   eval      t02ke1 = v1ccev
034400060427     c                   call      'TIBS02R'
034500060427     c                   parm                    kpjba
034600060427     c                   parm                    tibs02ds
034700160502     c                   if        t02err <> *blanks and v1cinc='S'
034800060427     c                   eval      v1cmsg = msg(03)
034900060427     c                   seton                                        4228
035000060427     c                   leavesr
035100060427     c                   endif
035200060427     c                   eval      dice = t02uni
035300060427     c                   eval      descev = §icedei
035400060427     c                   endif
035500060427      * se ambiente di filiale
035600060427     c                   if        v1ccev <> *blanks and *in06
035700060427     c                   eval      descev = 'Tabella solo di sede'
035800060427     c                   endif
035900060418
036000060418     c                   endsr
036100060418
036200060418      *----------------------------------------------------------
036300060427      * conferma dati + dati di trasmissione
036400060418      *----------------------------------------------------------
036500060427     c     sr_conferma   begsr
036600060418
036700060420     c                   eval      kkut = 1
036800060427     c                   eval      kcod = '7O'
036900060426     c                   movel     v1ccod        kkey
037000060420     c     ktab          chain(n)  tabel00f
037100060418     c                   if        %found
037200060418     c                   eval      w1ftt = tblftt
037300060418     c                   eval      w1flt = tblflt
037400060418     c                   eval      w1ftr = tblftr
037500060418
037600060418     c                   move      tbldtr        dataR
037700060418     c                   eval      aad = aar
037800060418     c                   eval      mmd = mmr
037900060418     c                   eval      ggd = ggr
038000060418     c                   move      dataD         w1dtr
038100060418     c                   else
038200060418     c                   clear                   w1ftt
038300060418     c                   clear                   w1flt
038400060418     c                   clear                   w1ftr
038500060418     c                   clear                   w1dtr
038600060418     c                   endif
038700060418
038800060418     c                   do        *hival
038900060427     c                   exfmt     tb22w1
039000060418      * F12 - Ritorno
039100060418     c                   if        *inkl
039200060418     c                   leave
039300060418     c                   endif
039400060418
039500060418      * F6 - Conferma
039600060418     c                   if        *inkf
039700060427     c                   exsr      sr_scrivi
039800060418     c                   eval      wf6 = *on
039900060418     c                   leave
040000060418     c                   endif
040100060418
040200060418     c                   enddo
040300060418
040400060418     c                   endsr
040500060418
040600060418      *---------------------------------------------------------------*
040700060427      * aggiornamento/inserimento
040800060418      *---------------------------------------------------------------*
040900060427     c     sr_scrivi     begsr
041000060427
041100060427      * imposto i dati nel file
041200060427     c                   exsr      sr_movfile
041300060418
041400060420     c                   eval      kkut = 1
041500060427     c                   eval      kcod = '7O'
041600060426     c                   movel     v1ccod        kkey
041700060420     c     ktab          chain     tabel00f
041800060418
041900060427     c                   eval      tbluni = ds7o
042000060418
042100060418     c                   if        wf16 = *on
042200060418     c                   eval      tblflg = '*'
042300060418     c                   endif
042400060418
042500060418     c                   if        wf5 = *on
042600060418     c                   clear                   tblflg
042700060418     c                   endif
042800060418
042900060418     c                   eval      tblftt = w1ftt
043000060418     c                   clear                   tblftr
043100060418     c                   clear                   tbldtr
043200060418
043300060418      * scrivo
043400060418     c                   if        not %found(tabel00f)
043500060420     c                   eval      tblkut = kkut
043600060420     c                   eval      tblcod = kcod
043700060420     c                   eval      tblkey = kkey
043800060418     c                   write     tabel
043900060418
044000060418      * aggiorno
044100060418     c                   else
044200060418     c                   update    tabel
044300060418     c                   endif
044400060420
044500060420      * se annullamento o ripristino devo fare la stessa cosa anche
044600060420      * sulle tabelle in lingua
044700060420     c                   if        wf16 = *on or wf5 = *on
044800060509      * aggancio le lingue presenti su AZLIN
044900060426     c                   move      kkut          kazlin
045000060426     c     kazlin        setgt     azlin01l
045100060426     c                   do        *hival
045200060426     c                   read      azlin01l
045300060426     c                   if        %eof(azlin01l)
045400060426     c                   leave
045500060426     c                   endif
045600060509      * per ogni lingua aggiorno
045700060426     c                   move      lintabel      kkut
045800060427     c                   eval      kcod = '7O'
045900060426     c                   movel     v1ccod        kkey
046000060420     c     ktab          chain     tabel00f
046100060420     c                   if        %found(tabel00f)
046200060420     c                   if        wf16 = *on
046300060420     c                   eval      tblflg = '*'
046400060420     c                   endif
046500060420     c                   if        wf5 = *on
046600060420     c                   clear                   tblflg
046700060420     c                   endif
046800060420     c                   eval      tblftt = w1ftt
046900060420     c                   clear                   tblftr
047000060420     c                   clear                   tbldtr
047100060420     c                   update    tabel
047200060420     c                   endif
047300060420     c                   enddo
047400060420     c                   endif
047500060418
047600060418     c                   endsr
047700060427
047800060427      *---------------------------------------------------------------*
047900060427      * imposto i dati nel file
048000060427      *---------------------------------------------------------------*
048100060427     c     sr_movfile    begsr
048200060427
048300060427     c                   eval      §7odei = v1cdei
048400160502     c                   eval      §7odeb = v1cdeb
048500060427     c                   eval      §7ovcp = v1cvcp
048600060427     c                   eval      §7ovca = v1cvca
048700060427     c                   eval      §7ofdi = v1cfdi
048800060427     c                   eval      §7ofst = v1cfst
048900060427     c                   eval      §7ores = v1cres
049000060427     c                   eval      §7oidd = v1cidd
049100060427     c                   eval      §7oinc = v1cinc
049200060427     c                   eval      §7oincp = v1cincp
049300060427     c                   eval      §7oicv = v1cicv
049400060427     c                   eval      §7ocev = v1ccev
049500060427     c                   eval      §7ofpt = v1cfpt
049600060427     c                   eval      §7orms = v1crms
049700060427     c                   eval      §7o139 = v1c139
049800060427     c                   eval      §7opodimg = v1cpodimg
049900060427     c                   eval      §7otle = v1ctle
050000060427
050100060427     c                   endsr
050200060418
050300060418      *---------------------------------------------------------------*
050400060418      * gestione tabella in lingua
050500060418      *---------------------------------------------------------------*
050600060418     c     sr_lingua     begsr
050700060418
050800060418      * pulisco il subfile
050900060418     c                   exsr      sr_pulsfl
051000060418      * carico il subfile con i dati che trovo o vuoto
051100060418     c                   exsr      sr_carsfl
051200060420      * emetto il subfile
051300060420     c     emisfl        tag
051400060420     c                   eval      rec = 1
051500060420     c                   if        savrec > 0
051600060420     c                   eval      rec = savrec
051700060420     c                   endif
051800060427     c                   write     tb22t1
051900060427     c                   write     tb22z1
052000060427     c                   exfmt     tb22c1
052100060420
052200060420      * F12 - Ritorno
052300060427     c                   if        *inkl
052400060427     c                   clear                   v1cmsg
052500060427     c                   setoff                                       28
052600060427     c                   goto      inz02
052700060427     c                   endif
052800060420
052900060427     c                   setoff                                       2843
053000060420      * Controlli
053100060426     c  n05              exsr      sr_ctrsfl
053200060420     c   28
053300060420     cornkf              goto      emisfl
053400060420
053500060420      * F6 - Conferma
053600060420     c   kf              exsr      sr_confsfl
053700060418
053800060418     c                   endsr
053900060418      *---------------------------------------------------------------*
054000060418      * pulisco il subfile
054100060418      *---------------------------------------------------------------*
054200060418     c     sr_pulsfl     begsr
054300060418
054400060426     c                   clear                   nrr
054500060426     c                   clear                   savrec
054600060426     c                   eval      *in20 = *off
054700060426     c                   eval      *in21 = *off
054800060427     c                   write     tb22c1
054900060426     c                   eval      *in20 = *on
055000060426     c                   eval      *in21 = *on
055100060418
055200060418     c                   endsr
055300060418
055400060418      *---------------------------------------------------------------*
055500060418      * carico il subfile
055600060418      *---------------------------------------------------------------*
055700060418     c     sr_carsfl     begsr
055800060418
055900060426     c                   setoff                                       30
056000060426      * carico un record per ogni lingua inserite in AZLIN
056100060426     c                   move      '1'           kazlin
056200060426     c     kazlin        setgt     azlin01l
056300060426     c                   do        *hival
056400060426     c                   read      azlin01l
056500060426      * fine file
056600060426     c                   if        %eof(azlin01l)
056700060426     c                   leave
056800060426     c                   endif
056900060426     c                   eval      s1dlin = lindesita
057000060427     c                   clear                   ds7o
057100060426     c                   move      lintabel      kkut
057200060427     c                   eval      kcod = '7O'
057300060426     c                   eval      kkey = v1ccod
057400060420     c     ktab          chain(n)  tabel00f
057500060418     c                   if        not %found(tabel00f)
057600060427     c                   clear                   s1cdei
057700160502     c                   clear                   s1cdeb
057800060418     c                   else
057900060427     c                   eval      ds7o = tbluni
058000060427     c                   eval      s1cdei = §7odei
058100160502     c                   eval      s1cdeb = §7odeb
058200060418     c                   endif
058300060420     c                   eval      s1hcdlin = kkut
058400060418     c                   add       1             nrr
058500060427     c                   write     tb22s1
058600060426     c                   enddo
058700060426
058800060426     c                   seton                                        30
058900060418
059000060418     c                   endsr
059100060420
059200060420      *---------------------------------------------------------------*
059300060420      * controllo il subfile
059400060420      *---------------------------------------------------------------*
059500060420     c     sr_ctrsfl     begsr
059600060420
059700060427     c                   clear                   nrr
059800060427     c                   do        *hival
059900060427     c                   eval      nrr = nrr +1
060000060427     c     nrr           chain     tb22s1                             32
060100060427     c                   if        *in32
060200060427     c                   leave
060300060427     c                   endif
060400060427      * deve esserci la descrizione Bartolini
060500060427     c                   if        s1cdei = *blanks
060600060420     c                   eval      v1cmsg = msg(02)
060700060420     c                   eval      *in28 = *on
060800060427     c                   eval      *in43 = *on
060900060420     c                   eval      savrec = nrr
061000060427     c                   update    tb22s1
061100060420     c                   leavesr
061200060420     c                   endif
061300060427     c                   update    tb22s1
061400060420     c                   enddo
061500060420
061600060420     c                   endsr
061700060420
061800060420      *---------------------------------------------------------------*
061900060420      * confermo i dati del subfile
062000060420      *---------------------------------------------------------------*
062100060420     c     sr_confsfl    begsr
062200060420
062300060420      * recupero i dati della tabella in italiano
062400060420     c                   clear                   savtbluni
062500060420     c                   eval      kkut = 1
062600060427     c                   eval      kcod = '7O'
062700060426     c                   movel     v1ccod        kkey
062800060420     c     ktab          chain(n)  tabel00f
062900060420     c                   if        %found
063000060420     c                   eval      savtbluni = tbluni
063100060420     c                   endif
063200060420
063300060420      * leggo il subfile e aggiorno/scrivo i dati in lingua
063400060420     c                   z-add     1             nrr
063500060420     c                   dow       nrr <= 3
063600060427     c     nrr           chain     tb22s1                             32
063700060420     c                   if        not *in32
063800060420     c                   eval      kkut = s1hcdlin
063900060420     c     ktab          chain     tabel00f
064000060427     c                   eval      ds7o = savtbluni
064100060427     c                   eval      §7odei = s1cdei
064200160502     c                   eval      §7odeb = s1cdeb
064300060427     c                   eval      tbluni = ds7o
064400060420     c                   eval      tblftt = w1ftt
064500060420     c                   clear                   tblftr
064600060420     c                   clear                   tbldtr
064700060420      * scrivo
064800060420     c                   if        not %found(tabel00f)
064900060420     c                   eval      tblkut = kkut
065000060420     c                   eval      tblcod = kcod
065100060420     c                   eval      tblkey = kkey
065200060420     c                   write     tabel
065300060420      * aggiorno
065400060420     c                   else
065500060420     c                   update    tabel
065600060420     c                   endif
065700060420     c                   endif
065800060420     c                   add       1             nrr
065900060420     c                   enddo
066000060420
066100060420     c                   endsr
066200060427
066300060427      *---------------------------------------------------------------*
066400060427      * routine iniziale
066500060427      *---------------------------------------------------------------*
066600060427     c     *inzsr        begsr
066700060427
066800060427     c     *entry        plist
066900060427     c                   parm                    kpjba
067000060427
067100060427     c     *dtaara       define    §azute        azuteds
067200060427     c     *dtaara       define    §datiute      ddatiute
067300060427
067400060427     c                   in(e)     *dtaara
067500060427     c                   if        %error or rsut = *blanks
067600060427     c                   clear                   tibs34ds
067700060427     c                   call      'TIBS34R'
067800060427     c                   parm                    tibs34ds
067900060427     c                   in        *dtaara
068000060427     c                   endif
068100060427
068200060427      * Controllo se ambiente di sede o di filiale
068300060427     c                   select
068400060427     c                   when      %subst(knsif:7:3) = '201' and
068500060427     c                             %subst(knsif:1:1) = 'F'
068600060427     c                   seton                                        06
068700060427     c                   when      %subst(knsif:7:1) = 'P' and
068800060427     c                             %subst(knsif:1:1) = 'F'
068900060427     c                   seton                                        06
069000060427     c                   other
069100060427     c                   setoff                                       06
069200060427     c                   endsl
069300060427
069400060427     c     ktab          klist
069500060427     c                   kfld                    kkut
069600060427     c                   kfld                    kcod
069700060427     c                   kfld                    kkey
069800060418
069900060427     c                   endsr
070000060418
070100060418** -- MSG -------------------------------------------------------------------*
070200060418Immettere il codice                                                            1
070300060418Immettere la descrizione                                                       2
070400060427Codice evento errato                                                           3
