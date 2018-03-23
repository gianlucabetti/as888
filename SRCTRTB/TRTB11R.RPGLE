000100060524     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060524
000300060524      *--------------------------------------------------------------*
000400060524      * GESTIONE TABELLA TIPI TARIFFA                                *
000500060524      *--------------------------------------------------------------*
000600060524
000700060524     ftabel00f  uf a e           k disk
000800060524     fazlin01l  if   e           k disk
000900060524     ftrtb11d   cf   e             workstn
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
007100060524     d dstr          e ds
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
012300060524     c                   eval      kcod = 'TR'
012400060524     c                   clear                   kkey
012500060524     c                   call      'TRUL19R'
012600060524     c                   parm                    kcod
012700060524     c                   parm                    ordina
012800060524     c                   parm                    kkey
012900060524     c                   parm                    comando
013000060524     c                   movel     kkey          v1ccod
013100060524     c                   goto      inz01
013200060524     c                   endif
013300060524
013400060524     c                   setoff                                       010203
013500060524     c                   exsr      pulvid
013600060524
013700060524     c                   eval      kkut = 1
013800060524     c                   eval      kcod = 'TR'
013900060524     c                   movel     v1ccod        kkey
014000060524     c     ktab          chain     tabel00f
014100060524      * nuovo codice
014200060524     c                   if        not %found(tabel00f)
014300060524     c                   eval      *in01 = *on
014400060524     c                   eval      vmod = 'INSERIMENTO'
014500060524      * codice trovato
014600060524     c                   else
014700060524     c                   eval      dstr = tbluni
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
022900060524     c                   clear                   dstr
023000060524     c                   clear                   v1cdes
023100060524     c                   clear                   v1cde1
023200060524     c                   clear                   v1cde2
023300060524     c                   clear                   v1cde3
023400060524     c                   clear                   v1cdst
023500060530     c                   clear                   v1cvas
023600060530     c                   clear                   v1cutc
023700060530     c                   clear                   v1cutp
023800060530     c                   clear                   v1cata
023900060530     c                   clear                   v1ctap
024000060530     c                   clear                   v1csap
024100060530     c                   clear                   v1cute
024200060530     c                   clear                   v1cupa
024300060530     c                   clear                   v1cucv
024400060530     c                   clear                   v1carr
024500060530     c                   clear                   v1crpv
024600060530     c                   clear                   v1cutf
024700060530
024800060530     c                   endsr
024900060530
025000060530      *---------------------------------------------------------------*
025100060530      * IMPOSTA CAMPI VIDEATA
025200060530      *---------------------------------------------------------------*
025300060530     c     impvid        begsr
025400060530
025500060530     c                   movel     §trdes        v1cdes
025600060530     c                   movel     §trde1        v1cde1
025700060530     c                   movel     §trde2        v1cde2
025800060530     c                   movel     §trde3        v1cde3
025900060530     c                   movel     §trdst        v1cdst
026000060530     c                   movel     §trvas        v1cvas
026100060530     c                   movel     §trutc        v1cutc
026200060530     c                   movel     §trutp        v1cutp
026300060530     c                   movel     §trata        v1cata
026400060530     c                   movel     §trtap        v1ctap
026500060530     c                   movel     §trsap        v1csap
026600060530     c                   movel     §trute        v1cute
026700060530     c                   movel     §trupa        v1cupa
026800060530     c                   movel     §trucv        v1cucv
026900060530     c                   movel     §trarr        v1carr
027000060530     c                   movel     §trrpv        v1crpv
027100060530     c                   movel     §trutf        v1cutf
027200060530
027300060530     c                   endsr
027400060524
027500060524      *---------------------------------------------------------------*
027600060524      * CONTROLLI FORMATO
027700060524      *---------------------------------------------------------------*
027800060524     c     ctrfmt        begsr
027900060524
028000060524     c                   setoff                                       41
028100060524
028200060524      * DESCRIZIONE
028300060524     c                   if        v1cdes = *blanks
028400060524     c                   eval      *in28 = *on
028500060524     c                   eval      *in41 = *on
028600060524     c                   eval      v1cmsg = msg(02)
028700060524     c                   leavesr
028800060524     c                   endif
028900060524
029000060524     c                   endsr
029100060524
029200060524      *----------------------------------------------------------
029300060524      *    Videata conferma + dati trasmissione
029400060524      *----------------------------------------------------------
029500060524     c     conferma      begsr
029600060524
029700060524     c                   eval      kkut = 1
029800060524     c                   eval      kcod = 'TR'
029900060727     c                   movel(p)  v1ccod        kkey
030000060524     c     ktab          chain(n)  tabel00f
030100060524     c                   if        %found
030200060524     c                   eval      w1ftt = tblftt
030300060524     c                   eval      w1flt = tblflt
030400060524     c                   eval      w1ftr = tblftr
030500060524
030600060524     c                   move      tbldtr        datar
030700060524     c                   eval      aad = aar
030800060524     c                   eval      mmd = mmr
030900060524     c                   eval      ggd = ggr
031000060524     c                   move      datad         w1dtr
031100060524     c                   else
031200060524     c                   clear                   w1ftt
031300060524     c                   clear                   w1flt
031400060524     c                   clear                   w1ftr
031500060524     c                   clear                   w1dtr
031600060524     c                   endif
031700060524
031800060524     c                   do        *hival
031900060524     c                   exfmt     tb11w1
032000060524      * F12 - Ritorno
032100060524     c                   if        *inkl
032200060524     c                   leave
032300060524     c                   endif
032400060524
032500060524      * F6 - Conferma
032600060524     c                   if        *inkf
032700060524     c                   exsr      scrivi
032800060524     c                   eval      wf6 = *on
032900060524     c                   leave
033000060524     c                   endif
033100060524
033200060524     c                   enddo
033300060524
033400060524     c                   endsr
033500060524
033600060524      *---------------------------------------------------------------*
033700060524      * AGGIORNAMENTO / INSERIMENTO
033800060524      *---------------------------------------------------------------*
033900060524     c     scrivi        begsr
034000060524
034100060524     c                   eval      §trdes = v1cdes
034200060524     c                   eval      §trde1 = v1cde1
034300060524     c                   eval      §trde2 = v1cde2
034400060524     c                   eval      §trde3 = v1cde3
034500060524     c                   eval      §trdst = v1cdst
034600060530     c                   eval      §trvas = v1cvas
034700060530     c                   eval      §trutc = v1cutc
034800060530     c                   eval      §trutp = v1cutp
034900060530     c                   eval      §trata = v1cata
035000060530     c                   eval      §trtap = v1ctap
035100060530     c                   eval      §trsap = v1csap
035200060530     c                   eval      §trute = v1cute
035300060530     c                   eval      §trupa = v1cupa
035400060530     c                   eval      §trucv = v1cucv
035500060530     c                   eval      §trarr = v1carr
035600060530     c                   eval      §trrpv = v1crpv
035700060530     c                   eval      §trutf = v1cutf
035800060524
035900060524     c                   eval      kkut = 1
036000060524     c                   eval      kcod = 'TR'
036100060524     c                   movel     v1ccod        kkey
036200060524     c     ktab          chain     tabel00f
036300060524
036400060524     c                   eval      tbluni = dstr
036500060524
036600060524     c                   if        wf16 = *on
036700060524     c                   eval      tblflg = '*'
036800060524     c                   endif
036900060524
037000060524     c                   if        wf5 = *on
037100060524     c                   clear                   tblflg
037200060524     c                   endif
037300060524
037400060524     c                   eval      tblftt = w1ftt
037500060524     c                   clear                   tblftr
037600060524     c                   clear                   tbldtr
037700060524
037800060524      * scrivo
037900060524     c                   if        not %found(tabel00f)
038000060524     c                   eval      tblkut = kkut
038100060524     c                   eval      tblcod = kcod
038200060524     c                   eval      tblkey = kkey
038300060524     c                   write     tabel
038400060524
038500060524      * aggiorno
038600060524     c                   else
038700060524     c                   update    tabel
038800060524     c                   endif
038900060524
039000060524      * se annullamento o ripristino devo fare la stessa cosa anche
039100060524      * sulle tabelle in lingua
039200060524     c                   if        wf16 = *on or wf5 = *on
039300060524      * aggancio le lingue presenti su AZLIN
039400060524     c                   move      kkut          kazlin
039500060524     c     kazlin        setgt     azlin01l
039600060524     c                   do        *hival
039700060524     c                   read      azlin01l
039800060524     c                   if        %eof(azlin01l)
039900060524     c                   leave
040000060524     c                   endif
040100060524      * per ogni lingua aggiorno
040200060524     c                   move      lintabel      kkut
040300060524     c                   eval      kcod = 'TR'
040400060524     c                   movel     v1ccod        kkey
040500060524     c     ktab          chain     tabel00f
040600060524     c                   if        %found(tabel00f)
040700060524     c                   if        wf16 = *on
040800060524     c                   eval      tblflg = '*'
040900060524     c                   endif
041000060524     c                   if        wf5 = *on
041100060524     c                   clear                   tblflg
041200060524     c                   endif
041300060524     c                   eval      tblftt = w1ftt
041400060524     c                   clear                   tblftr
041500060524     c                   clear                   tbldtr
041600060524     c                   update    tabel
041700060524     c                   endif
041800060524     c                   enddo
041900060524     c                   endif
042000060524
042100060524     c                   endsr
042200060524
042300060524      *---------------------------------------------------------------*
042400060524      * gestione tabella in lingua
042500060524      *---------------------------------------------------------------*
042600060524     c     sr_lingua     begsr
042700060524
042800060524      * pulisco il subfile
042900060524     c                   exsr      sr_pulsfl
043000060524      * carico il subfile con i dati che trovo o vuoto
043100060524     c                   exsr      sr_carsfl
043200060524      * emetto il subfile
043300060524     c     emisfl        tag
043400060524     c                   eval      rec = 1
043500060524     c                   if        savrec > 0
043600060524     c                   eval      rec = savrec
043700060524     c                   endif
043800060524     c                   write     tb11t1
043900060524     c                   write     tb11z1
044000060524     c                   exfmt     tb11c1
044100060524
044200060524      * F12 - Ritorno
044300060524     c                   if        *inkl
044400060524     c                   setoff                                       28
044500060524     c                   clear                   v1cmsg
044600060524     c                   goto      inz02
044700060524     c                   endif
044800060524
044900060524     c                   setoff                                       28
045000060524     c                   setoff                                       45
045100060524      * Controlli
045200060524     c  n05              exsr      sr_ctrsfl
045300060524     c   28
045400060524     cornkf              goto      emisfl
045500060524
045600060524      * F6 - Conferma
045700060524     c   kf              exsr      sr_confsfl
045800060524
045900060524     c                   endsr
046000060524
046100060524      *---------------------------------------------------------------*
046200060524      * pulisco il subfile
046300060524      *---------------------------------------------------------------*
046400060524     c     sr_pulsfl     begsr
046500060524
046600060524     c                   clear                   nrr
046700060524     c                   clear                   savrec
046800060524     c                   eval      *in20 = *off
046900060524     c                   eval      *in21 = *off
047000060524     c                   write     tb11c1
047100060524     c                   eval      *in20 = *on
047200060524     c                   eval      *in21 = *on
047300060524
047400060524     c                   endsr
047500060524
047600060524      *---------------------------------------------------------------*
047700060524      * carico il subfile
047800060524      *---------------------------------------------------------------*
047900060524     c     sr_carsfl     begsr
048000060524
048100060524     c                   setoff                                       30
048200060524      * carico un record per ogni lingua inserite in AZLIN
048300060524     c                   move      '1'           kazlin
048400060524     c     kazlin        setgt     azlin01l
048500060524     c                   do        *hival
048600060524     c                   read      azlin01l
048700060524      * fine file
048800060524     c                   if        %eof(azlin01l)
048900060524     c                   leave
049000060524     c                   endif
049100060524     c                   eval      s1dlin = lindesita
049200060524     c                   clear                   dstr
049300060524     c                   move      lintabel      kkut
049400060524     c                   eval      kcod = 'TR'
049500060524     c                   eval      kkey = v1ccod
049600060524     c     ktab          chain(n)  tabel00f
049700060524     c                   if        not %found(tabel00f)
049800060524     c                   clear                   s1cdes
049900060524     c                   clear                   s1cde1
050000060524     c                   clear                   s1cde2
050100060524     c                   clear                   s1cde3
050200060524     c                   clear                   s1cdst
050300060524     c                   else
050400060524     c                   eval      dstr = tbluni
050500060524     c                   eval      s1cdes = §trdes
050600060524     c                   eval      s1cde1 = §trde1
050700060524     c                   eval      s1cde2 = §trde2
050800060524     c                   eval      s1cde3 = §trde3
050900060524     c                   eval      s1cdst = §trdst
051000060524     c                   endif
051100060524     c                   eval      s1hcdlin = kkut
051200060524     c                   add       1             nrr
051300060524     c                   write     tb11s1
051400060524     c                   enddo
051500060524
051600060524     c                   seton                                        30
051700060524
051800060524     c                   endsr
051900060524
052000060524      *---------------------------------------------------------------*
052100060524      * controllo il subfile
052200060524      *---------------------------------------------------------------*
052300060524     c     sr_ctrsfl     begsr
052400060524
052500060524     c                   clear                   nrr
052600060524     c                   do        *hival
052700060524     c                   eval      nrr = nrr +1
052800060524     c     nrr           chain     tb11s1                             32
052900060524     c                   if        *in32
053000060524     c                   leave
053100060524     c                   endif
053200060524      * deve esserci la descrizione
053300060524     c                   if        s1cdes = *blanks
053400060524     c                   eval      v1cmsg = msg(02)
053500060524     c                   eval      *in28 = *on
053600060524     c                   eval      *in45 = *on
053700060524     c                   eval      savrec = nrr
053800060524     c                   update    tb11s1
053900060524     c                   leavesr
054000060524     c                   endif
054100060524     c                   update    tb11s1
054200060524     c                   enddo
054300060524
054400060524     c                   endsr
054500060524
054600060524      *---------------------------------------------------------------*
054700060524      * confermo i dati del subfile
054800060524      *---------------------------------------------------------------*
054900060524     c     sr_confsfl    begsr
055000060524
055100060524      * recupero i dati della tabella in italiano
055200060524     c                   clear                   savtbluni
055300060524     c                   eval      kkut = 1
055400060524     c                   eval      kcod = 'TR'
055500060524     c                   movel     v1ccod        kkey
055600060524     c     ktab          chain(n)  tabel00f
055700060524     c                   if        %found
055800060524     c                   eval      savtbluni = tbluni
055900060524     c                   endif
056000060524
056100060524      * leggo il subfile e aggiorno/scrivo i dati in lingua
056200060524     c                   clear                   nrr
056300060524     c                   do        *hival
056400060524     c                   eval      nrr = nrr +1
056500060524     c     nrr           chain     tb11s1                             32
056600060524     c                   if        *in32
056700060524     c                   leave
056800060524     c                   endif
056900060524     c                   eval      kkut = s1hcdlin
057000060524     c     ktab          chain     tabel00f
057100060524     c                   eval      dstr = savtbluni
057200060524     c                   eval      §trdes = s1cdes
057300060524     c                   eval      §trde1 = s1cde1
057400060524     c                   eval      §trde2 = s1cde2
057500060524     c                   eval      §trde3 = s1cde3
057600060524     c                   eval      §trdst = s1cdst
057700060524     c                   eval      tbluni = dstr
057800060524     c                   eval      tblftt = w1ftt
057900060524     c                   clear                   tblftr
058000060524     c                   clear                   tbldtr
058100060524      * scrivo
058200060524     c                   if        not %found(tabel00f)
058300060524     c                   eval      tblkut = kkut
058400060524     c                   eval      tblcod = kcod
058500060524     c                   eval      tblkey = kkey
058600060524     c                   write     tabel
058700060524      * aggiorno
058800060524     c                   else
058900060524     c                   update    tabel
059000060524     c                   endif
059100060524
059200060524     c                   enddo
059300060524
059400060524     c                   endsr
059500060524
059600060524      *---------------------------------------------------------------*
059700060524      * Reperimento Dati del job (Utente/Operativi)                   *
059800060524      *---------------------------------------------------------------*
059900060524     c     datijob       begsr
060000060524      *
060100060524     c     *dtaara       define    §azute        azuteds
060200060524     c     *dtaara       define    §datiute      ddatiute
060300060524      *
060400060524     c                   in(e)     *dtaara
060500060524     c                   if        %error or rsut = *blanks
060600060524     c                   clear                   tibs34ds
060700060524     c                   call      'TIBS34R'
060800060524     c                   parm                    tibs34ds
060900060524     c                   in        *dtaara
061000060524     c                   endif
061100060524
061200060524     c                   endsr
061300060524** -- MSG -------------------------------------------------------------------*
061400060524Immettere il codice                                                            1
061500060524Immettere la descrizione                                                       2
