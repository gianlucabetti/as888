000100060526     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060526
000300060526      *--------------------------------------------------------------*
000400060526      * GESTIONE TABELLA TIPI DI PAGAMENTO                           *
000500060526      *--------------------------------------------------------------*
000600060526
000700060526     ftabel00f  uf a e           k disk
000800060526     fazlin01l  if   e           k disk
000900060526     ftrtb68d   cf   e             workstn
001000060526     f                                     sfile(tb68s1:nrr)
001100060526
001200060526      *---------------------------------------------------------------*
001300060526      *  RIEPILOGO INDICATORI                                         *
001400060526      *---------------------------------------------------------------*
001500060526      * 01 = inserimento
001600060526      * 02 = variazione
001700060526      * 03 = annullato
001800060526      * 05 = visualizzo subfile lingue
001900060526      * 20 = GESTIONE SUBFILE
002000060526      * 21 = GESTIONE SUBFILE
002100060526      * 22 = GESTIONE SUBFILE
002200060526      * 28 = visualizza messaggio errore
002300060526      * 30 = Fine file
002400060526      * 31 = comodo
002500060526      * 32 = comodo
002600060526      * 40 = codice tassazione
002700060526      * 41 = descrizione
002800060526      * 45 = errore subfile
002900060526      *--------------------------------------------------------------*
003000060526
003100060526      *--------------------------------------------------------------*
003200060526      * VARIABILI
003300060526      *--------------------------------------------------------------*
003400060526     d comando         s              1
003500060526     d kazlin          s                   like(lintabel)
003600060526     d kcod            s                   like(tblcod)
003700060526     d kkey            s                   like(tblkey)
003800060526     d kkut            s                   like(tblkut)
003900060526     d ordina          s              1
004000060531     d savtblftt       s                   like(tblftt)
004100060531     d savtblflt       s                   like(tblflt)
004200060526     d savtbluni       s                   like(tbluni)
004300060526     d nrr             s              4  0
004400060526     d savrec          s                   like(nrr)
004500060526     d wf16            s              1    inz('0')
004600060526     d wf5             s              1    inz('0')
004700060526     d wf6             s              1    inz('0')
004800060526
004900060526      *--------------------------------------------------------------*
005000060526      * SCHIERE
005100060526      *--------------------------------------------------------------*
005200060526     d msg             s             78    dim(10) ctdata perrcd(1)
005300060526
005400060526      *--------------------------------------------------------------*
005500060526      * STRUTTURE DATI
005600060526      *--------------------------------------------------------------*
005700060526     d datar           ds
005800060526     d  aar                    1      2
005900060526     d  mmr                    3      4
006000060526     d  ggr                    5      6
006100060526
006200060526     d datad           ds
006300060526     d  ggd                    1      2
006400060526     d  mmd                    3      4
006500060526     d  aad                    5      6
006600060526
006700060526     d azuteds       e ds                  extname(azute00f)
006800060526     d ddatiute      e ds
006900060526     d ds4u          e ds
007000060526     d kpjba         e ds
007100060526     d tibs34ds      e ds
007200060526     d trul12ds      e ds
007300060526      *--------------------------------------------------------------*
007400060526
007500060526     c     ktab          klist
007600060526     c                   kfld                    kkut
007700060526     c                   kfld                    kcod
007800060526     c                   kfld                    kkey
007900060526
008000060526     c     *entry        plist
008100060526     c                   parm                    kpjba
008200060526
008300060526      * Reperisco dati job
008400060526     c                   exsr      datijob
008500060526
008600060526      * Richiamo pgm per controllo modificabilità tabella
008700151030      * ma solo se non sono in ambiente di prova
008800151030     c                   IF        %subst(knsif:7:1) <> 'P'
008900060526     c                   clear                   trul12ds
009000060526     c                   eval      i12tla = 'L'
009100060526     c                   eval      i12nsi = knsif
009200060526     c                   eval      i12cod = '4U'
009300060526     c                   call      'TRUL12R'
009400060526     c                   parm                    trul12ds
009500060526     c                   if        o12err = '1'
009600060526     c                   goto      fine
009700060526     c                   endif
009800151030     c                   ENDIF
009900060526
010000060526     c     inizio        tag
010100060526     c                   clear                   v1ccod
010200060526     c                   eval      v1ccod = '?'
010300060526     c                   eval      wf16 = *off
010400060526     c                   eval      wf5 = *off
010500060526     c                   eval      wf6 = *off
010600060526     c                   clear                   vmod
010700060526
010800060526
010900060526     c     inz01         tag
011000060526     c                   write     tb68t1
011100060526     c                   exfmt     tb68d1
011200060526     c                   clear                   v1cmsg
011300060526     c                   eval      *in28 = *off
011400060526     c                   eval      *in40 = *off
011500060526      * F3=Fine
011600060526     c   kc              goto      fine
011700060526      * ricerca codice
011800060526     c     '?'           scan      v1ccod                                 31
011900060526     c                   if        *in31
012000060526     c                   clear                   v1ccod
012100060526     c                   eval      kcod = '4U'
012200060526     c                   clear                   kkey
012300060526     c                   call      'TRUL19R'
012400060526     c                   parm                    kcod
012500060526     c                   parm                    ordina
012600060526     c                   parm                    kkey
012700060526     c                   parm                    comando
012800060526     c                   movel     kkey          v1ccod
012900060526     c                   goto      inz01
013000060526     c                   endif
013100060526
013200060526     c                   setoff                                       010203
013300060526     c                   exsr      pulvid
013400060526
013500060526     c                   eval      kkut = 1
013600060526     c                   eval      kcod = '4U'
013700060526     c                   movel     v1ccod        kkey
013800060526     c     ktab          chain     tabel00f
013900060526      * nuovo codice
014000060526     c                   if        not %found(tabel00f)
014100060526     c                   eval      *in01 = *on
014200060526     c                   eval      vmod = 'INSERIMENTO'
014300060526      * codice trovato
014400060526     c                   else
014500060526     c                   eval      ds4u = tbluni
014600060530      * imposto i dati a video
014700060530     c                   exsr      impvid
014800060526      * codice annullato
014900060526     c                   if        tblflg = '*'
015000060526     c                   eval      *in03 = *on
015100060526     c                   eval      vmod = 'ANNULLATO  '
015200060526      * codice valido
015300060526     c                   else
015400060526     c                   eval      *in02 = *on
015500060526     c                   eval      vmod = 'MODIFICA   '
015600060526     c                   endif
015700060526     c                   endif
015800060526
015900060526     c     inz02         tag
016000060526
016100060526      * EMETTO FORMATO
016200060526     c                   write     tb68t1
016300060526     c                   exfmt     tb68d2
016400060526     c                   clear                   v1cmsg
016500060526     c                   eval      *in28 = *off
016600060526      * F3=Fine
016700060526     c   kc              goto      fine
016800060526      * F12=Ritorno
016900060526     c   kl              goto      inz01
017000060526      * F9=Altri dati
017100060526     c                   if        *inki
017200060526     c                   eval      *in05 = *on
017300060526     c                   exsr      sr_lingua
017400060526     c                   eval      *in05 = *off
017500060526     c                   goto      inz02
017600060526     c                   endif
017700060526      * F16=Annullamento
017800060526     c                   if        *inkq
017900060526     c                   eval      wf16 = *on
018000060526     c                   exsr      conferma
018100060526     c                   if        wf6 = *on
018200060526     c                   goto      inizio
018300060526     c                   else
018400060526     c                   eval      wf16 = *off
018500060526     c                   goto      inz02
018600060526     c                   endif
018700060526     c                   endif
018800060526      * F5=Ripristino
018900060526     c                   if        *inke
019000060526     c                   eval      wf5 = *on
019100060526     c                   exsr      conferma
019200060526     c                   if        wf6 = *on
019300060526     c                   goto      inizio
019400060526     c                   else
019500060526     c                   eval      wf5 = *off
019600060526     c                   goto      inz02
019700060526     c                   endif
019800060526     c                   endif
019900060526      * controllo dati
020000060526     c                   exsr      ctrfmt
020100060526     c   28              goto      inz02
020200060526
020300060526     c  nkf              goto      inz02
020400060526      * F6=Conferma
020500060526     c                   if        *inkf
020600060526     c                   exsr      conferma
020700060526     c                   if        wf6 = *on
020800060526      * tabelle in lingua
020900060526     c                   eval      *in05 = *off
021000060526     c                   exsr      sr_lingua
021100060526     c                   goto      inizio
021200060526     c                   else
021300060526     c                   goto      inz02
021400060526     c                   endif
021500060526     c                   endif
021600060526
021700060526     c     fine          tag
021800060526
021900060526     c                   eval      *inlr = *on
022000060526
022100060526      *---------------------------------------------------------------*
022200060526      * PULIZIA CAMPI VIDEATA
022300060526      *---------------------------------------------------------------*
022400060526     c     pulvid        begsr
022500060526
022600060526     c                   clear                   ds4u
022700060526     c                   clear                   v1cdes
022800060530     c                   clear                   v1cabi
022900060530     c                   clear                   v1cvar
023000060530     c                   clear                   v1ctip
023100060530     c                   clear                   v1ccoda
023200151030     c                   clear                   v1csede
023300060526
023400060526     c                   endsr
023500060530
023600060530      *---------------------------------------------------------------*
023700060530      * IMPOSTA CAMPI VIDEATA
023800060530      *---------------------------------------------------------------*
023900060530     c     impvid        begsr
024000060530
024100060530     c                   eval      v1cdes = §4udes
024200060530     c                   eval      v1cabi = §4uabi
024300060530     c                   eval      v1cvar = §4uvar
024400060530     c                   eval      v1ctip = §4utip
024500060530     c                   eval      v1ccoda = §4ucod
024600151030     c                   eval      v1csede = §4usede
024700060530
024800060530     c                   endsr
024900060530
025000060526      *---------------------------------------------------------------*
025100060526      * CONTROLLI FORMATO
025200060526      *---------------------------------------------------------------*
025300060526     c     ctrfmt        begsr
025400060526     c                   setoff                                       4142
025500060526
025600060526      * DESCRIZIONE
025700060526     c                   if        v1cdes = *blanks
025800060526     c                   eval      *in28 = *on
025900060526     c                   eval      *in41 = *on
026000060526     c                   eval      v1cmsg = msg(02)
026100060526     c                   leavesr
026200060526     c                   endif
026300060526
026400060526     c                   endsr
026500060526
026600060526      *----------------------------------------------------------
026700060526      *    Videata conferma + dati trasmissione
026800060526      *----------------------------------------------------------
026900060526     c     conferma      begsr
027000060526
027100060526     c                   eval      kkut = 1
027200060526     c                   eval      kcod = '4U'
027300060526     c                   movel     v1ccod        kkey
027400060526     c     ktab          chain(n)  tabel00f
027500060526     c                   if        %found
027600060526     c                   eval      w1ftt = tblftt
027700060526     c                   eval      w1flt = tblflt
027800060526     c                   eval      w1ftr = tblftr
027900060526
028000060526     c                   move      tbldtr        datar
028100060526     c                   eval      aad = aar
028200060526     c                   eval      mmd = mmr
028300060526     c                   eval      ggd = ggr
028400060526     c                   move      datad         w1dtr
028500060526     c                   else
028600060526     c                   clear                   w1ftt
028700060526     c                   clear                   w1flt
028800060526     c                   clear                   w1ftr
028900060526     c                   clear                   w1dtr
029000060526     c                   endif
029100060526
029200060526     c                   do        *hival
029300060526     c                   exfmt     tb68w1
029400060526      * F12 - Ritorno
029500060526     c                   if        *inkl
029600060526     c                   leave
029700060526     c                   endif
029800060526
029900060526      * F6 - Conferma
030000060526     c                   if        *inkf
030100060526     c                   exsr      scrivi
030200060526     c                   eval      wf6 = *on
030300060526     c                   leave
030400060526     c                   endif
030500060526
030600060526     c                   enddo
030700060526
030800060526     c                   endsr
030900060526
031000060526      *---------------------------------------------------------------*
031100060526      * AGGIORNAMENTO / INSERIMENTO
031200060526      *---------------------------------------------------------------*
031300060526     c     scrivi        begsr
031400060526
031500060530     c                   eval      §4udes = v1cdes
031600060530     c                   eval      §4uabi = v1cabi
031700060530     c                   eval      §4uvar = v1cvar
031800060530     c                   eval      §4utip = v1ctip
031900060530     c                   eval      §4ucod = v1ccoda
032000151030     c                   eval      §4usede = v1csede
032100060526
032200060526     c                   eval      kkut = 1
032300060526     c                   eval      kcod = '4U'
032400060526     c                   movel     v1ccod        kkey
032500060526     c     ktab          chain     tabel00f
032600060526
032700060526     c                   eval      tbluni = ds4u
032800060526
032900060526     c                   if        wf16 = *on
033000060526     c                   eval      tblflg = '*'
033100060526     c                   endif
033200060526
033300060526     c                   if        wf5 = *on
033400060526     c                   clear                   tblflg
033500060526     c                   endif
033600060526
033700060526     c                   eval      tblftt = w1ftt
033800060526     c                   clear                   tblftr
033900060526     c                   clear                   tbldtr
034000060526
034100060526      * scrivo
034200060526     c                   if        not %found(tabel00f)
034300060526     c                   eval      tblkut = kkut
034400060526     c                   eval      tblcod = kcod
034500060526     c                   eval      tblkey = kkey
034600060526     c                   write     tabel
034700060526
034800060526      * aggiorno
034900060526     c                   else
035000060526     c                   update    tabel
035100060526     c                   endif
035200060526
035300060526      * se annullamento o ripristino devo fare la stessa cosa anche
035400060526      * sulle tabelle in lingua
035500060526     c                   if        wf16 = *on or wf5 = *on
035600060526      * aggancio le lingue presenti su AZLIN
035700060526     c                   move      kkut          kazlin
035800060526     c     kazlin        setgt     azlin01l
035900060526     c                   do        *hival
036000060526     c                   read      azlin01l
036100060526     c                   if        %eof(azlin01l)
036200060526     c                   leave
036300060526     c                   endif
036400060526      * per ogni lingua aggiorno
036500060526     c                   move      lintabel      kkut
036600060526     c                   eval      kcod = '4U'
036700060526     c                   movel     v1ccod        kkey
036800060526     c     ktab          chain     tabel00f
036900060526     c                   if        %found(tabel00f)
037000060526     c                   if        wf16 = *on
037100060526     c                   eval      tblflg = '*'
037200060526     c                   endif
037300060526     c                   if        wf5 = *on
037400060526     c                   clear                   tblflg
037500060526     c                   endif
037600060526     c                   eval      tblftt = w1ftt
037700060526     c                   clear                   tblftr
037800060526     c                   clear                   tbldtr
037900060526     c                   update    tabel
038000060526     c                   endif
038100060526     c                   enddo
038200060526     c                   endif
038300060526
038400060526     c                   endsr
038500060526
038600060526      *---------------------------------------------------------------*
038700060526      * gestione tabella in lingua
038800060526      *---------------------------------------------------------------*
038900060526     c     sr_lingua     begsr
039000060526
039100060526      * pulisco il subfile
039200060526     c                   exsr      sr_pulsfl
039300060526      * carico il subfile con i dati che trovo o vuoto
039400060526     c                   exsr      sr_carsfl
039500060526      * emetto il subfile
039600060526     c     emisfl        tag
039700060526     c                   eval      rec = 1
039800060526     c                   if        savrec > 0
039900060526     c                   eval      rec = savrec
040000060526     c                   endif
040100060526     c                   write     tb68t1
040200060526     c                   write     tb68z1
040300060526     c                   exfmt     tb68c1
040400060526
040500060526      * F12 - Ritorno
040600060526     c                   if        *inkl
040700060526     c                   setoff                                       28
040800060526     c                   clear                   v1cmsg
040900060526     c                   goto      inz02
041000060526     c                   endif
041100060526
041200060526     c                   setoff                                       28
041300060526     c                   setoff                                       45
041400060526      * Controlli
041500060526     c  n05              exsr      sr_ctrsfl
041600060526     c   28
041700060526     cornkf              goto      emisfl
041800060526
041900060526      * F6 - Conferma
042000060526     c   kf              exsr      sr_confsfl
042100060526
042200060526     c                   endsr
042300060526
042400060526      *---------------------------------------------------------------*
042500060526      * pulisco il subfile
042600060526      *---------------------------------------------------------------*
042700060526     c     sr_pulsfl     begsr
042800060526
042900060526     c                   clear                   nrr
043000060526     c                   clear                   savrec
043100060526     c                   eval      *in20 = *off
043200060526     c                   eval      *in21 = *off
043300060526     c                   write     tb68c1
043400060526     c                   eval      *in20 = *on
043500060526     c                   eval      *in21 = *on
043600060526
043700060526     c                   endsr
043800060526
043900060526      *---------------------------------------------------------------*
044000060526      * carico il subfile
044100060526      *---------------------------------------------------------------*
044200060526     c     sr_carsfl     begsr
044300060526
044400060526     c                   setoff                                       30
044500060526      * carico un record per ogni lingua inserite in AZLIN
044600060526     c                   move      '1'           kazlin
044700060526     c     kazlin        setgt     azlin01l
044800060526     c                   do        *hival
044900060526     c                   read      azlin01l
045000060526      * fine file
045100060526     c                   if        %eof(azlin01l)
045200060526     c                   leave
045300060526     c                   endif
045400060526     c                   eval      s1dlin = lindesita
045500060526     c                   clear                   ds4u
045600060526     c                   move      lintabel      kkut
045700060526     c                   eval      kcod = '4U'
045800060526     c                   eval      kkey = v1ccod
045900060526     c     ktab          chain(n)  tabel00f
046000060526     c                   if        not %found(tabel00f)
046100060526     c                   clear                   s1cdes
046200060526     c                   else
046300060526     c                   eval      ds4u = tbluni
046400060526     c                   eval      s1cdes = §4udes
046500060526     c                   endif
046600060526     c                   eval      s1hcdlin = kkut
046700060526     c                   add       1             nrr
046800060526     c                   write     tb68s1
046900060526     c                   enddo
047000060526
047100060526     c                   seton                                        30
047200060526
047300060526     c                   endsr
047400060526
047500060526      *---------------------------------------------------------------*
047600060526      * controllo il subfile
047700060526      *---------------------------------------------------------------*
047800060526     c     sr_ctrsfl     begsr
047900060526
048000060526     c                   clear                   nrr
048100060526     c                   do        *hival
048200060526     c                   eval      nrr = nrr +1
048300060526     c     nrr           chain     tb68s1                             32
048400060526     c                   if        *in32
048500060526     c                   leave
048600060526     c                   endif
048700060526      * deve esserci la descrizione
048800060526     c                   if        s1cdes = *blanks
048900060526     c                   eval      v1cmsg = msg(02)
049000060526     c                   eval      *in28 = *on
049100060526     c                   eval      *in45 = *on
049200060526     c                   eval      savrec = nrr
049300060526     c                   update    tb68s1
049400060526     c                   leavesr
049500060526     c                   endif
049600060526     c                   update    tb68s1
049700060526     c                   enddo
049800060526
049900060526     c                   endsr
050000060526
050100060526      *---------------------------------------------------------------*
050200060526      * confermo i dati del subfile
050300060526      *---------------------------------------------------------------*
050400060526     c     sr_confsfl    begsr
050500060526
050600060526      * recupero i dati della tabella in italiano
050700060526     c                   clear                   savtbluni
050800060526     c                   eval      kkut = 1
050900060526     c                   eval      kcod = '4U'
051000060526     c                   movel     v1ccod        kkey
051100060526     c     ktab          chain(n)  tabel00f
051200060526     c                   if        %found
051300060526     c                   eval      savtbluni = tbluni
051400060526     c                   endif
051500060526
051600060526      * leggo il subfile e aggiorno/scrivo i dati in lingua
051700060526     c                   clear                   nrr
051800060526     c                   do        *hival
051900060526     c                   eval      nrr = nrr +1
052000060526     c     nrr           chain     tb68s1                             32
052100060526     c                   if        *in32
052200060526     c                   leave
052300060526     c                   endif
052400060526     c                   eval      kkut = s1hcdlin
052500060526     c     ktab          chain     tabel00f
052600060526     c                   eval      ds4u = savtbluni
052700060526     c                   eval      §4udes = s1cdes
052800060526     c                   eval      tbluni = ds4u
052900060526     c                   eval      tblftt = w1ftt
053000060526     c                   clear                   tblftr
053100060526     c                   clear                   tbldtr
053200060526      * scrivo
053300060526     c                   if        not %found(tabel00f)
053400060526     c                   eval      tblkut = kkut
053500060526     c                   eval      tblcod = kcod
053600060526     c                   eval      tblkey = kkey
053700060526     c                   write     tabel
053800060526      * aggiorno
053900060526     c                   else
054000060526     c                   update    tabel
054100060526     c                   endif
054200060526
054300060526     c                   enddo
054400060526
054500060526     c                   endsr
054600060526
054700060526      *---------------------------------------------------------------*
054800060526      * Reperimento Dati del job (Utente/Operativi)                   *
054900060526      *---------------------------------------------------------------*
055000060526     c     datijob       begsr
055100060526      *
055200060526     c     *dtaara       define    §azute        azuteds
055300060526     c     *dtaara       define    §datiute      ddatiute
055400060526      *
055500060526     c                   in(e)     *dtaara
055600060526     c                   if        %error or rsut = *blanks
055700060526     c                   clear                   tibs34ds
055800060526     c                   call      'TIBS34R'
055900060526     c                   parm                    tibs34ds
056000060526     c                   in        *dtaara
056100060526     c                   endif
056200060526
056300060526     c                   endsr
056400060526
056500060526
056600060526** -- MSG -------------------------------------------------------------------*
056700060526Immettere il codice                                                            1
056800060526Immettere la descrizione                                                       2
