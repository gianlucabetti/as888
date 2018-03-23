000100060524     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060524
000300060524      *--------------------------------------------------------------*
000400060524      * GESTIONE TABELLA TIPI TARIFFA                                *
000500060524      *--------------------------------------------------------------*
000600060524
000700060524     ftabel00f  uf a e           k disk
000800060524     fazlin01l  if   e           k disk
000900060704     ftrtb2dd   cf   e             workstn
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
007100060704     d ds2d          e ds
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
012300060704     c                   eval      kcod = '2D'
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
013800060704     c                   eval      kcod = '2D'
013900060524     c                   movel     v1ccod        kkey
014000060524     c     ktab          chain     tabel00f
014100060524      * nuovo codice
014200060524     c                   if        not %found(tabel00f)
014300060524     c                   eval      *in01 = *on
014400060524     c                   eval      vmod = 'INSERIMENTO'
014500060524      * codice trovato
014600060524     c                   else
014700060704     c                   eval      ds2d = tbluni
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
022900060704     c                   clear                   ds2d
023000060524     c                   clear                   v1cdes
023100060704     c                   clear                   v1cutp
023200060704     c                   clear                   v1cuta
023300060530
023400060530     c                   endsr
023500060530
023600060530      *---------------------------------------------------------------*
023700060530      * IMPOSTA CAMPI VIDEATA
023800060530      *---------------------------------------------------------------*
023900060530     c     impvid        begsr
024000060530
024100060704     c                   movel     §2ddes        v1cdes
024200060704     c                   movel     §2dutp        v1cutp
024300060704     c                   movel     §2duta        v1cuta
024400060530
024500060530     c                   endsr
024600060524
024700060524      *---------------------------------------------------------------*
024800060524      * CONTROLLI FORMATO
024900060524      *---------------------------------------------------------------*
025000060524     c     ctrfmt        begsr
025100060524
025200060524     c                   setoff                                       41
025300060524
025400060524      * DESCRIZIONE
025500060524     c                   if        v1cdes = *blanks
025600060524     c                   eval      *in28 = *on
025700060524     c                   eval      *in41 = *on
025800060524     c                   eval      v1cmsg = msg(02)
025900060524     c                   leavesr
026000060524     c                   endif
026100060524
026200060524     c                   endsr
026300060524
026400060524      *----------------------------------------------------------
026500060524      *    Videata conferma + dati trasmissione
026600060524      *----------------------------------------------------------
026700060524     c     conferma      begsr
026800060524
026900060524     c                   eval      kkut = 1
027000060704     c                   eval      kcod = '2D'
027100060524     c                   movel     v1ccod        kkey
027200060524     c     ktab          chain(n)  tabel00f
027300060524     c                   if        %found
027400060524     c                   eval      w1ftt = tblftt
027500060524     c                   eval      w1flt = tblflt
027600060524     c                   eval      w1ftr = tblftr
027700060524
027800060524     c                   move      tbldtr        datar
027900060524     c                   eval      aad = aar
028000060524     c                   eval      mmd = mmr
028100060524     c                   eval      ggd = ggr
028200060524     c                   move      datad         w1dtr
028300060524     c                   else
028400060524     c                   clear                   w1ftt
028500060524     c                   clear                   w1flt
028600060524     c                   clear                   w1ftr
028700060524     c                   clear                   w1dtr
028800060524     c                   endif
028900060524
029000060524     c                   do        *hival
029100060524     c                   exfmt     tb11w1
029200060524      * F12 - Ritorno
029300060524     c                   if        *inkl
029400060524     c                   leave
029500060524     c                   endif
029600060524
029700060524      * F6 - Conferma
029800060524     c                   if        *inkf
029900060524     c                   exsr      scrivi
030000060524     c                   eval      wf6 = *on
030100060524     c                   leave
030200060524     c                   endif
030300060524
030400060524     c                   enddo
030500060524
030600060524     c                   endsr
030700060524
030800060524      *---------------------------------------------------------------*
030900060524      * AGGIORNAMENTO / INSERIMENTO
031000060524      *---------------------------------------------------------------*
031100060524     c     scrivi        begsr
031200060524
031300060704     c                   eval      §2ddes = v1cdes
031400060704     c                   eval      §2dutp = v1cutp
031500060704     c                   eval      §2duta = v1cuta
031600060524
031700060524     c                   eval      kkut = 1
031800060704     c                   eval      kcod = '2D'
031900060524     c                   movel     v1ccod        kkey
032000060524     c     ktab          chain     tabel00f
032100060524
032200060704     c                   eval      tbluni = ds2d
032300060524
032400060524     c                   if        wf16 = *on
032500060524     c                   eval      tblflg = '*'
032600060524     c                   endif
032700060524
032800060524     c                   if        wf5 = *on
032900060524     c                   clear                   tblflg
033000060524     c                   endif
033100060524
033200060524     c                   eval      tblftt = w1ftt
033300060524     c                   clear                   tblftr
033400060524     c                   clear                   tbldtr
033500060524
033600060524      * scrivo
033700060524     c                   if        not %found(tabel00f)
033800060524     c                   eval      tblkut = kkut
033900060524     c                   eval      tblcod = kcod
034000060524     c                   eval      tblkey = kkey
034100060524     c                   write     tabel
034200060524
034300060524      * aggiorno
034400060524     c                   else
034500060524     c                   update    tabel
034600060524     c                   endif
034700060524
034800060524      * se annullamento o ripristino devo fare la stessa cosa anche
034900060524      * sulle tabelle in lingua
035000060524     c                   if        wf16 = *on or wf5 = *on
035100060524      * aggancio le lingue presenti su AZLIN
035200060524     c                   move      kkut          kazlin
035300060524     c     kazlin        setgt     azlin01l
035400060524     c                   do        *hival
035500060524     c                   read      azlin01l
035600060524     c                   if        %eof(azlin01l)
035700060524     c                   leave
035800060524     c                   endif
035900060524      * per ogni lingua aggiorno
036000060524     c                   move      lintabel      kkut
036100060704     c                   eval      kcod = '2D'
036200060524     c                   movel     v1ccod        kkey
036300060524     c     ktab          chain     tabel00f
036400060524     c                   if        %found(tabel00f)
036500060524     c                   if        wf16 = *on
036600060524     c                   eval      tblflg = '*'
036700060524     c                   endif
036800060524     c                   if        wf5 = *on
036900060524     c                   clear                   tblflg
037000060524     c                   endif
037100060524     c                   eval      tblftt = w1ftt
037200060524     c                   clear                   tblftr
037300060524     c                   clear                   tbldtr
037400060524     c                   update    tabel
037500060524     c                   endif
037600060524     c                   enddo
037700060524     c                   endif
037800060524
037900060524     c                   endsr
038000060524
038100060524      *---------------------------------------------------------------*
038200060524      * gestione tabella in lingua
038300060524      *---------------------------------------------------------------*
038400060524     c     sr_lingua     begsr
038500060524
038600060524      * pulisco il subfile
038700060524     c                   exsr      sr_pulsfl
038800060524      * carico il subfile con i dati che trovo o vuoto
038900060524     c                   exsr      sr_carsfl
039000060524      * emetto il subfile
039100060524     c     emisfl        tag
039200060524     c                   eval      rec = 1
039300060524     c                   if        savrec > 0
039400060524     c                   eval      rec = savrec
039500060524     c                   endif
039600060524     c                   write     tb11t1
039700060524     c                   write     tb11z1
039800060524     c                   exfmt     tb11c1
039900060524
040000060524      * F12 - Ritorno
040100060524     c                   if        *inkl
040200060524     c                   setoff                                       28
040300060524     c                   clear                   v1cmsg
040400060524     c                   goto      inz02
040500060524     c                   endif
040600060524
040700060524     c                   setoff                                       28
040800060524     c                   setoff                                       45
040900060524      * Controlli
041000060524     c  n05              exsr      sr_ctrsfl
041100060524     c   28
041200060524     cornkf              goto      emisfl
041300060524
041400060524      * F6 - Conferma
041500060524     c   kf              exsr      sr_confsfl
041600060524
041700060524     c                   endsr
041800060524
041900060524      *---------------------------------------------------------------*
042000060524      * pulisco il subfile
042100060524      *---------------------------------------------------------------*
042200060524     c     sr_pulsfl     begsr
042300060524
042400060524     c                   clear                   nrr
042500060524     c                   clear                   savrec
042600060524     c                   eval      *in20 = *off
042700060524     c                   eval      *in21 = *off
042800060524     c                   write     tb11c1
042900060524     c                   eval      *in20 = *on
043000060524     c                   eval      *in21 = *on
043100060524
043200060524     c                   endsr
043300060524
043400060524      *---------------------------------------------------------------*
043500060524      * carico il subfile
043600060524      *---------------------------------------------------------------*
043700060524     c     sr_carsfl     begsr
043800060524
043900060524     c                   setoff                                       30
044000060524      * carico un record per ogni lingua inserite in AZLIN
044100060524     c                   move      '1'           kazlin
044200060524     c     kazlin        setgt     azlin01l
044300060524     c                   do        *hival
044400060524     c                   read      azlin01l
044500060524      * fine file
044600060524     c                   if        %eof(azlin01l)
044700060524     c                   leave
044800060524     c                   endif
044900060524     c                   eval      s1dlin = lindesita
045000060704     c                   clear                   ds2d
045100060524     c                   move      lintabel      kkut
045200060704     c                   eval      kcod = '2D'
045300060524     c                   eval      kkey = v1ccod
045400060524     c     ktab          chain(n)  tabel00f
045500060524     c                   if        not %found(tabel00f)
045600060524     c                   clear                   s1cdes
045700060524     c                   else
045800060704     c                   eval      ds2d = tbluni
045900060704     c                   eval      s1cdes = §2ddes
046000060524     c                   endif
046100060524     c                   eval      s1hcdlin = kkut
046200060524     c                   add       1             nrr
046300060524     c                   write     tb11s1
046400060524     c                   enddo
046500060524
046600060524     c                   seton                                        30
046700060524
046800060524     c                   endsr
046900060524
047000060524      *---------------------------------------------------------------*
047100060524      * controllo il subfile
047200060524      *---------------------------------------------------------------*
047300060524     c     sr_ctrsfl     begsr
047400060524
047500060524     c                   clear                   nrr
047600060524     c                   do        *hival
047700060524     c                   eval      nrr = nrr +1
047800060524     c     nrr           chain     tb11s1                             32
047900060524     c                   if        *in32
048000060524     c                   leave
048100060524     c                   endif
048200060524      * deve esserci la descrizione
048300060524     c                   if        s1cdes = *blanks
048400060524     c                   eval      v1cmsg = msg(02)
048500060524     c                   eval      *in28 = *on
048600060524     c                   eval      *in45 = *on
048700060524     c                   eval      savrec = nrr
048800060524     c                   update    tb11s1
048900060524     c                   leavesr
049000060524     c                   endif
049100060524     c                   update    tb11s1
049200060524     c                   enddo
049300060524
049400060524     c                   endsr
049500060524
049600060524      *---------------------------------------------------------------*
049700060524      * confermo i dati del subfile
049800060524      *---------------------------------------------------------------*
049900060524     c     sr_confsfl    begsr
050000060524
050100060524      * recupero i dati della tabella in italiano
050200060524     c                   clear                   savtbluni
050300060524     c                   eval      kkut = 1
050400060704     c                   eval      kcod = '2D'
050500060524     c                   movel     v1ccod        kkey
050600060524     c     ktab          chain(n)  tabel00f
050700060524     c                   if        %found
050800060524     c                   eval      savtbluni = tbluni
050900060524     c                   endif
051000060524
051100060524      * leggo il subfile e aggiorno/scrivo i dati in lingua
051200060524     c                   clear                   nrr
051300060524     c                   do        *hival
051400060524     c                   eval      nrr = nrr +1
051500060524     c     nrr           chain     tb11s1                             32
051600060524     c                   if        *in32
051700060524     c                   leave
051800060524     c                   endif
051900060524     c                   eval      kkut = s1hcdlin
052000060524     c     ktab          chain     tabel00f
052100060704     c                   eval      ds2d = savtbluni
052200060704     c                   eval      §2ddes = s1cdes
052300060704     c                   eval      tbluni = ds2d
052400060524     c                   eval      tblftt = w1ftt
052500060524     c                   clear                   tblftr
052600060524     c                   clear                   tbldtr
052700060524      * scrivo
052800060524     c                   if        not %found(tabel00f)
052900060524     c                   eval      tblkut = kkut
053000060524     c                   eval      tblcod = kcod
053100060524     c                   eval      tblkey = kkey
053200060524     c                   write     tabel
053300060524      * aggiorno
053400060524     c                   else
053500060524     c                   update    tabel
053600060524     c                   endif
053700060524
053800060524     c                   enddo
053900060524
054000060524     c                   endsr
054100060524
054200060524      *---------------------------------------------------------------*
054300060524      * Reperimento Dati del job (Utente/Operativi)                   *
054400060524      *---------------------------------------------------------------*
054500060524     c     datijob       begsr
054600060524      *
054700060524     c     *dtaara       define    §azute        azuteds
054800060524     c     *dtaara       define    §datiute      ddatiute
054900060524      *
055000060524     c                   in(e)     *dtaara
055100060524     c                   if        %error or rsut = *blanks
055200060524     c                   clear                   tibs34ds
055300060524     c                   call      'TIBS34R'
055400060524     c                   parm                    tibs34ds
055500060524     c                   in        *dtaara
055600060524     c                   endif
055700060524
055800060524     c                   endsr
055900060524** -- MSG -------------------------------------------------------------------*
056000060524Immettere il codice                                                            1
056100060524Immettere la descrizione                                                       2
