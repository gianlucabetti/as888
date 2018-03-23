000100060524     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060524
000300060524      *--------------------------------------------------------------*
000400060524      * GESTIONE TABELLA TIPI TARIFFA                                *
000500060524      *--------------------------------------------------------------*
000600060524
000700060524     ftabel00f  uf a e           k disk
000800060524     fazlin01l  if   e           k disk
000900060705     ftrtb4sd   cf   e             workstn
001000060524     f                                     sfile(tb11s1:nrr)
001100060524
001200060524      *---------------------------------------------------------------*
001300060524      *  RIEPILOGO INDICATORI                                         *
001400060524      *---------------------------------------------------------------*
001500060524      * 01 = inserimento
001600060524      * 02 = variazione
001700060524      * 03 = annullato
001800060524      * 05 = visualizzo subfile lingue
001900060524      * 20 = GESTIONE SUBFILE
002000060524      * 21 = GESTIONE SUBFILE
002100060524      * 22 = GESTIONE SUBFILE
002200060524      * 28 = visualizza messaggio errore
002300060524      * 30 = Fine file
002400060524      * 31 = comodo
002500060524      * 32 = comodo
002600060524      * 40 = codice tassazione
002700060524      * 41 = descrizione
002800060524      * 42 = provincia
002900060524      * 43 = punto operativo
003000060524      * 44 = nazione
003100060524      * 79 = profilo NO EDP
003200060524      *--------------------------------------------------------------*
003300060524
003400060524      *--------------------------------------------------------------*
003500060524      * VARIABILI
003600060524      *--------------------------------------------------------------*
003700060524     d comando         s              1
003800060524     d kazlin          s                   like(lintabel)
003900060524     d kcod            s                   like(tblcod)
004000060524     d kkey            s                   like(tblkey)
004100060524     d kkut            s                   like(tblkut)
004200060524     d ordina          s              1
004300060531     d savtblftt       s                   like(tblftt)
004400060531     d savtblflt       s                   like(tblflt)
004500060524     d savtbluni       s                   like(tbluni)
004600060524     d nrr             s              4  0
004700060524     d savrec          s                   like(nrr)
004800060524     d wf16            s              1    inz('0')
004900060524     d wf5             s              1    inz('0')
005000060524     d wf6             s              1    inz('0')
005100060524
005200060524      *--------------------------------------------------------------*
005300060524      * SCHIERE
005400060524      *--------------------------------------------------------------*
005500060524     d msg             s             78    dim(10) ctdata perrcd(1)
005600060524
005700060524      *--------------------------------------------------------------*
005800060524      * STRUTTURE DATI
005900060524      *--------------------------------------------------------------*
006000060524     d datar           ds
006100060524     d  aar                    1      2
006200060524     d  mmr                    3      4
006300060524     d  ggr                    5      6
006400060524
006500060524     d datad           ds
006600060524     d  ggd                    1      2
006700060524     d  mmd                    3      4
006800060524     d  aad                    5      6
006900060524
007000060524     d azuteds       e ds                  extname(azute00f)
007100060705     d ds4s          e ds
007200060524     d ddatiute      e ds
007300060524     d kpjba         e ds
007400060524     d tibs34ds      e ds
007500060524      *--------------------------------------------------------------*
007600060524
007700060524     c     ktab          klist
007800060524     c                   kfld                    kkut
007900060524     c                   kfld                    kcod
008000060524     c                   kfld                    kkey
008100060524
008200060524     c     *entry        plist
008300060524     c                   parm                    kpjba
008400060524
008500060524      * Reperisco dati job
008600060524     c                   exsr      datijob
008700060524
008800060524     c                   eval      *in79 = *off
008900060524     c                   if        %subst(knmus:1:3) <> 'EDP'
009000060524     c                   eval      *in79 = *on
009100060524     c                   endif
009200060524
009300060524     c     inizio        tag
009400060524     c                   clear                   v1ccod
009500060524     c                   eval      v1ccod = '?'
009600060524     c                   eval      wf16 = *off
009700060524     c                   eval      wf5 = *off
009800060524     c                   eval      wf6 = *off
009900060524     c                   clear                   vmod
010000060524
010100060524     c     inz01         tag
010200060524     c                   write     tb11t1
010300060524     c                   exfmt     tb11d1
010400060524     c                   clear                   v1cmsg
010500060524     c                   eval      *in28 = *off
010600060524     c                   eval      *in40 = *off
010700060524
010800060524      * F3=Fine
010900060524     c   kc              goto      fine
011000060524
011100060524      * codice obbligatorio
011200060524     c                   if        v1ccod = *blanks
011300060524     c                   eval      *in28 = *on
011400060524     c                   eval      *in40 = *on
011500060524     c                   eval      v1cmsg = msg(01)
011600060524     c                   goto      inz01
011700060524     c                   endif
011800060524
011900060524      * ricerca codice tassazione
012000060524     c     '?'           scan      v1ccod                                 31
012100060524     c                   if        *in31
012200060524     c                   clear                   v1ccod
012300060705     c                   eval      kcod = '4S'
012400060524     c                   clear                   kkey
012500060524     c                   call      'TRUL19R'
012600060524     c                   parm                    kcod
012700060524     c                   parm                    ordina
012800060524     c                   parm                    kkey
012900060524     c                   parm                    comando
013000060705     c                   move      kkey          v1ccod
013100060524     c                   goto      inz01
013200060524     c                   endif
013300060524
013400060524     c                   setoff                                       010203
013500060524     c                   exsr      pulvid
013600060524
013700060524     c                   eval      kkut = 1
013800060705     c                   eval      kcod = '4S'
013900060705     c                   move      v1ccod        kkey
014000060524     c     ktab          chain     tabel00f
014100060524      * nuovo codice
014200060524     c                   if        not %found(tabel00f)
014300060524     c                   eval      *in01 = *on
014400060524     c                   eval      vmod = 'INSERIMENTO'
014500060524      * codice trovato
014600060524     c                   else
014700060705     c                   eval      ds4s = tbluni
014800060530      * imposto i dati a video
014900060530     c                   exsr      impvid
015000060524      * codice annullato
015100060524     c                   if        tblflg = '*'
015200060524     c                   eval      *in03 = *on
015300060524     c                   eval      vmod = 'ANNULLATO  '
015400060524      * codice valido
015500060524     c                   else
015600060524     c                   eval      *in02 = *on
015700060524     c                   eval      vmod = 'MODIFICA   '
015800060524     c                   endif
015900060524     c                   endif
016000060524
016100060524     c     inz02         tag
016200060524
016300060524      * EMETTO FORMATO
016400060524     c                   write     tb11t1
016500060524     c                   exfmt     tb11d2
016600060524     c                   clear                   v1cmsg
016700060524     c                   eval      *in28 = *off
016800060524      * F3=Fine
016900060524     c   kc              goto      fine
017000060524      * F12=Ritorno
017100060524     c   kl              goto      inz01
017200060524
017300060524      * F9=Altri dati
017400060524     c                   if        *inki
017500060524     c                   eval      *in05 = *on
017600060524     c                   exsr      sr_lingua
017700060524     c                   eval      *in05 = *off
017800060524     c                   goto      inz02
017900060524     c                   endif
018000060524      * F16=Annullamento
018100060524     c                   if        *inkq
018200060524     c                   eval      wf16 = *on
018300060524     c                   exsr      conferma
018400060524     c                   if        wf6 = *on
018500060524     c                   goto      inizio
018600060524     c                   else
018700060524     c                   eval      wf16 = *off
018800060524     c                   goto      inz02
018900060524     c                   endif
019000060524     c                   endif
019100060524      * F5=Ripristino
019200060524     c                   if        *inke
019300060524     c                   eval      wf5 = *on
019400060524     c                   exsr      conferma
019500060524     c                   if        wf6 = *on
019600060524     c                   goto      inizio
019700060524     c                   else
019800060524     c                   eval      wf5 = *off
019900060524     c                   goto      inz02
020000060524     c                   endif
020100060524     c                   endif
020200060524      * controllo dati
020300060524     c                   exsr      ctrfmt
020400060524     c   28              goto      inz02
020500060524
020600060524     c  nkf              goto      inz02
020700060524      * F6=Conferma
020800060524     c                   if        *inkf
020900060524     c                   exsr      conferma
021000060524     c                   if        wf6 = *on
021100060524      * tabelle in lingua
021200060524     c                   eval      *in05 = *off
021300060524     c                   exsr      sr_lingua
021400060524     c                   goto      inizio
021500060524     c                   else
021600060524     c                   goto      inz02
021700060524     c                   endif
021800060524     c                   endif
021900060524
022000060524     c     fine          tag
022100060524
022200060524     c                   eval      *inlr = *on
022300060524
022400060524      *---------------------------------------------------------------*
022500060524      * PULIZIA CAMPI VIDEATA
022600060524      *---------------------------------------------------------------*
022700060524     c     pulvid        begsr
022800060524
022900060705     c                   clear                   ds4s
023000060524     c                   clear                   v1cdes
023100060705     c                   clear                   v1cfil
023200060705     c                   clear                   v1cmod
023300060705     c                   clear                   v1cdei
023400060705     c                   clear                   v1cdep
023500060530
023600060530     c                   endsr
023700060530
023800060530      *---------------------------------------------------------------*
023900060530      * IMPOSTA CAMPI VIDEATA
024000060530      *---------------------------------------------------------------*
024100060530     c     impvid        begsr
024200060530
024300060705     c                   movel     §4sdes        v1cdes
024400060705     c                   movel     §4sfil        v1cfil
024500060705     c                   movel     §4smod        v1cmod
024600060705     c                   movel     §4sdei        v1cdei
024700060705     c                   movel     §4sdep        v1cdep
024800060530
024900060530     c                   endsr
025000060524
025100060524      *---------------------------------------------------------------*
025200060524      * CONTROLLI FORMATO
025300060524      *---------------------------------------------------------------*
025400060524     c     ctrfmt        begsr
025500060524
025600060524     c                   setoff                                       41
025700060524
025800060524      * DESCRIZIONE
025900060524     c                   if        v1cdes = *blanks
026000060524     c                   eval      *in28 = *on
026100060524     c                   eval      *in41 = *on
026200060524     c                   eval      v1cmsg = msg(02)
026300060524     c                   leavesr
026400060524     c                   endif
026500060524
026600060524     c                   endsr
026700060524
026800060524      *----------------------------------------------------------
026900060524      *    Videata conferma + dati trasmissione
027000060524      *----------------------------------------------------------
027100060524     c     conferma      begsr
027200060524
027300060524     c                   eval      kkut = 1
027400060705     c                   eval      kcod = '4S'
027500060705     c                   move      v1ccod        kkey
027600060524     c     ktab          chain(n)  tabel00f
027700060524     c                   if        %found
027800060524     c                   eval      w1ftt = tblftt
027900060524     c                   eval      w1flt = tblflt
028000060524     c                   eval      w1ftr = tblftr
028100060524
028200060524     c                   move      tbldtr        datar
028300060524     c                   eval      aad = aar
028400060524     c                   eval      mmd = mmr
028500060524     c                   eval      ggd = ggr
028600060524     c                   move      datad         w1dtr
028700060524     c                   else
028800060524     c                   clear                   w1ftt
028900060524     c                   clear                   w1flt
029000060524     c                   clear                   w1ftr
029100060524     c                   clear                   w1dtr
029200060524     c                   endif
029300060524
029400060524     c                   do        *hival
029500060524     c                   exfmt     tb11w1
029600060524      * F12 - Ritorno
029700060524     c                   if        *inkl
029800060524     c                   leave
029900060524     c                   endif
030000060524
030100060524      * F6 - Conferma
030200060524     c                   if        *inkf
030300060524     c                   exsr      scrivi
030400060524     c                   eval      wf6 = *on
030500060524     c                   leave
030600060524     c                   endif
030700060524
030800060524     c                   enddo
030900060524
031000060524     c                   endsr
031100060524
031200060524      *---------------------------------------------------------------*
031300060524      * AGGIORNAMENTO / INSERIMENTO
031400060524      *---------------------------------------------------------------*
031500060524     c     scrivi        begsr
031600060524
031700060705     c                   eval      §4sdes = v1cdes
031800060705     c                   eval      §4sfil = v1cfil
031900060705     c                   eval      §4smod = v1cmod
032000060705     c                   eval      §4sdei = v1cdei
032100060705     c                   eval      §4sdep = v1cdep
032200060524
032300060524     c                   eval      kkut = 1
032400060705     c                   eval      kcod = '4S'
032500060705     c                   move      v1ccod        kkey
032600060524     c     ktab          chain     tabel00f
032700060524
032800060705     c                   eval      tbluni = ds4s
032900060524
033000060524     c                   if        wf16 = *on
033100060524     c                   eval      tblflg = '*'
033200060524     c                   endif
033300060524
033400060524     c                   if        wf5 = *on
033500060524     c                   clear                   tblflg
033600060524     c                   endif
033700060524
033800060524     c                   eval      tblftt = w1ftt
033900060524     c                   clear                   tblftr
034000060524     c                   clear                   tbldtr
034100060524
034200060524      * scrivo
034300060524     c                   if        not %found(tabel00f)
034400060524     c                   eval      tblkut = kkut
034500060524     c                   eval      tblcod = kcod
034600060524     c                   eval      tblkey = kkey
034700060524     c                   write     tabel
034800060524
034900060524      * aggiorno
035000060524     c                   else
035100060524     c                   update    tabel
035200060524     c                   endif
035300060524
035400060524      * se annullamento o ripristino devo fare la stessa cosa anche
035500060524      * sulle tabelle in lingua
035600060524     c                   if        wf16 = *on or wf5 = *on
035700060524      * aggancio le lingue presenti su AZLIN
035800060524     c                   move      kkut          kazlin
035900060524     c     kazlin        setgt     azlin01l
036000060524     c                   do        *hival
036100060524     c                   read      azlin01l
036200060524     c                   if        %eof(azlin01l)
036300060524     c                   leave
036400060524     c                   endif
036500060524      * per ogni lingua aggiorno
036600060524     c                   move      lintabel      kkut
036700060705     c                   eval      kcod = '4S'
036800060705     c                   move      v1ccod        kkey
036900060524     c     ktab          chain     tabel00f
037000060524     c                   if        %found(tabel00f)
037100060524     c                   if        wf16 = *on
037200060524     c                   eval      tblflg = '*'
037300060524     c                   endif
037400060524     c                   if        wf5 = *on
037500060524     c                   clear                   tblflg
037600060524     c                   endif
037700060524     c                   eval      tblftt = w1ftt
037800060524     c                   clear                   tblftr
037900060524     c                   clear                   tbldtr
038000060524     c                   update    tabel
038100060524     c                   endif
038200060524     c                   enddo
038300060524     c                   endif
038400060524
038500060524     c                   endsr
038600060524
038700060524      *---------------------------------------------------------------*
038800060524      * gestione tabella in lingua
038900060524      *---------------------------------------------------------------*
039000060524     c     sr_lingua     begsr
039100060524
039200060524      * per prima cosa devo creare le testate in lingua
039300060524      * se non esistono già
039400060524     c  n05              exsr      sr_teslin
039500060524      * pulisco il subfile
039600060524     c                   exsr      sr_pulsfl
039700060524      * carico il subfile con i dati che trovo o vuoto
039800060524     c                   exsr      sr_carsfl
039900060524      * emetto il subfile
040000060524     c     emisfl        tag
040100060524     c                   eval      rec = 1
040200060524     c                   if        savrec > 0
040300060524     c                   eval      rec = savrec
040400060524     c                   endif
040500060524     c                   write     tb11t1
040600060524     c                   write     tb11z1
040700060524     c                   exfmt     tb11c1
040800060524
040900060524      * F12 - Ritorno
041000060524     c                   if        *inkl
041100060524     c                   setoff                                       28
041200060524     c                   clear                   v1cmsg
041300060524     c                   goto      inz02
041400060524     c                   endif
041500060524
041600060524     c                   setoff                                       28
041700060524     c                   setoff                                       45
041800060524      * Controlli
041900060524     c  n05              exsr      sr_ctrsfl
042000060524     c   28
042100060524     cornkf              goto      emisfl
042200060524
042300060524      * F6 - Conferma
042400060524     c   kf              exsr      sr_confsfl
042500060524
042600060524     c                   endsr
042700060524
042800060524      *---------------------------------------------------------------*
042900060524      * scrivo le testate in lingua
043000060524      *---------------------------------------------------------------*
043100060524     c     sr_teslin     begsr
043200060524
043300060524      * recupero i dati dalla testata in italiano
043400060524     c                   clear                   savtbluni
043500060531     c                   clear                   savtblftt
043600060531     c                   clear                   savtblflt
043700060524     c                   eval      kkut = 1
043800060524     c                   clear                   kcod
043900060524     c                   eval      kkey = *all'0'
044000060705     c                   move      '4S'          kkey
044100060524     c     ktab          chain(n)  tabel00f
044200060524     c                   if        %found(tabel00f)
044300060524     c                   eval      savtbluni = tbluni
044400060531     c                   eval      savtblftt = tblftt
044500060531     c                   eval      savtblflt = tblflt
044600060524     c                   endif
044700060524      * scrivo la testata nelle varie lingue inserite in AZLIN
044800060524     c                   move      kkut          kazlin
044900060524     c     kazlin        setgt     azlin01l
045000060524     c                   do        *hival
045100060524     c                   read      azlin01l
045200060524      * fine file
045300060524     c                   if        %eof(azlin01l)
045400060524     c                   leave
045500060524     c                   endif
045600060524      * scrivo
045700060524     c                   move      lintabel      kkut
045800060524     c     ktab          chain     tabel00f
045900060524     c                   if        not %found(tabel00f)
046000060524     c                   eval      tblkut = kkut
046100060524     c                   eval      tblcod = kcod
046200060524     c                   eval      tblkey = kkey
046300060524     c                   eval      tbluni = savtbluni
046400060531     c                   eval      tblftt = savtblftt
046500060531     c                   eval      tblflt = savtblflt
046600060531     c                   clear                   tblftr
046700060531     c                   clear                   tbldtr
046800060524     c                   write     tabel
046900060524     c                   endif
047000060524     c                   enddo
047100060524
047200060524     c                   endsr
047300060524
047400060524      *---------------------------------------------------------------*
047500060524      * pulisco il subfile
047600060524      *---------------------------------------------------------------*
047700060524     c     sr_pulsfl     begsr
047800060524
047900060524     c                   clear                   nrr
048000060524     c                   clear                   savrec
048100060524     c                   eval      *in20 = *off
048200060524     c                   eval      *in21 = *off
048300060524     c                   write     tb11c1
048400060524     c                   eval      *in20 = *on
048500060524     c                   eval      *in21 = *on
048600060524
048700060524     c                   endsr
048800060524
048900060524      *---------------------------------------------------------------*
049000060524      * carico il subfile
049100060524      *---------------------------------------------------------------*
049200060524     c     sr_carsfl     begsr
049300060524
049400060524     c                   setoff                                       30
049500060524      * carico un record per ogni lingua inserite in AZLIN
049600060524     c                   move      '1'           kazlin
049700060524     c     kazlin        setgt     azlin01l
049800060524     c                   do        *hival
049900060524     c                   read      azlin01l
050000060524      * fine file
050100060524     c                   if        %eof(azlin01l)
050200060524     c                   leave
050300060524     c                   endif
050400060524     c                   eval      s1dlin = lindesita
050500060705     c                   clear                   ds4s
050600060524     c                   move      lintabel      kkut
050700060705     c                   eval      kcod = '4S'
050800060705     c                   evalr     kkey = v1ccod
050900060524     c     ktab          chain(n)  tabel00f
051000060524     c                   if        not %found(tabel00f)
051100060524     c                   clear                   s1cdes
051101060705     c                   clear                   s1cdei
051102060705     c                   clear                   s1cdep
051200060524     c                   else
051300060705     c                   eval      ds4s = tbluni
051400060705     c                   eval      s1cdes = §4sdes
051401060705     c                   eval      s1cdei = §4sdei
051402060705     c                   eval      s1cdep = §4sdep
051500060524     c                   endif
051600060524     c                   eval      s1hcdlin = kkut
051700060524     c                   add       1             nrr
051800060524     c                   write     tb11s1
051900060524     c                   enddo
052000060524
052100060524     c                   seton                                        30
052200060524
052300060524     c                   endsr
052400060524
052500060524      *---------------------------------------------------------------*
052600060524      * controllo il subfile
052700060524      *---------------------------------------------------------------*
052800060524     c     sr_ctrsfl     begsr
052900060524
053000060524     c                   clear                   nrr
053100060524     c                   do        *hival
053200060524     c                   eval      nrr = nrr +1
053300060524     c     nrr           chain     tb11s1                             32
053400060524     c                   if        *in32
053500060524     c                   leave
053600060524     c                   endif
053700060524      * deve esserci la descrizione
053800060524     c                   if        s1cdes = *blanks
053900060524     c                   eval      v1cmsg = msg(02)
054000060524     c                   eval      *in28 = *on
054100060524     c                   eval      *in45 = *on
054200060524     c                   eval      savrec = nrr
054300060524     c                   update    tb11s1
054400060524     c                   leavesr
054500060524     c                   endif
054600060524     c                   update    tb11s1
054700060524     c                   enddo
054800060524
054900060524     c                   endsr
055000060524
055100060524      *---------------------------------------------------------------*
055200060524      * confermo i dati del subfile
055300060524      *---------------------------------------------------------------*
055400060524     c     sr_confsfl    begsr
055500060524
055600060524      * recupero i dati della tabella in italiano
055700060524     c                   clear                   savtbluni
055800060524     c                   eval      kkut = 1
055900060705     c                   eval      kcod = '4S'
056000060705     c                   move      v1ccod        kkey
056100060524     c     ktab          chain(n)  tabel00f
056200060524     c                   if        %found
056300060524     c                   eval      savtbluni = tbluni
056400060524     c                   endif
056500060524
056600060524      * leggo il subfile e aggiorno/scrivo i dati in lingua
056700060524     c                   clear                   nrr
056800060524     c                   do        *hival
056900060524     c                   eval      nrr = nrr +1
057000060524     c     nrr           chain     tb11s1                             32
057100060524     c                   if        *in32
057200060524     c                   leave
057300060524     c                   endif
057400060524     c                   eval      kkut = s1hcdlin
057500060524     c     ktab          chain     tabel00f
057600060705     c                   eval      ds4s = savtbluni
057700060705     c                   eval      §4sdes = s1cdes
057701060705     c                   eval      §4sdei = s1cdei
057702060705     c                   eval      §4sdep = s1cdep
057800060705     c                   eval      tbluni = ds4s
057900060524     c                   eval      tblftt = w1ftt
058000060524     c                   clear                   tblftr
058100060524     c                   clear                   tbldtr
058200060524      * scrivo
058300060524     c                   if        not %found(tabel00f)
058400060524     c                   eval      tblkut = kkut
058500060524     c                   eval      tblcod = kcod
058600060524     c                   eval      tblkey = kkey
058700060524     c                   write     tabel
058800060524      * aggiorno
058900060524     c                   else
059000060524     c                   update    tabel
059100060524     c                   endif
059200060524
059300060524     c                   enddo
059400060524
059500060524     c                   endsr
059600060524
059700060524      *---------------------------------------------------------------*
059800060524      * Reperimento Dati del job (Utente/Operativi)                   *
059900060524      *---------------------------------------------------------------*
060000060524     c     datijob       begsr
060100060524      *
060200060524     c     *dtaara       define    §azute        azuteds
060300060524     c     *dtaara       define    §datiute      ddatiute
060400060524      *
060500060524     c                   in(e)     *dtaara
060600060524     c                   if        %error or rsut = *blanks
060700060524     c                   clear                   tibs34ds
060800060524     c                   call      'TIBS34R'
060900060524     c                   parm                    tibs34ds
061000060524     c                   in        *dtaara
061100060524     c                   endif
061200060524
061300060524     c                   endsr
061400060524** -- MSG -------------------------------------------------------------------*
061500060524Immettere il codice                                                            1
061600060524Immettere la descrizione                                                       2
