000100060530     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060530
000300060530      *--------------------------------------------------------------*
000400060530      * GESTIONE TABELLA ORGANIGRAMMA DI FILIALE                     *
000500060530      *--------------------------------------------------------------*
000600060530
000700060530     ftabel00f  uf a e           k disk
000800060530     fazlin01l  if   e           k disk
000900060530     ftrtb17d   cf   e             workstn
001000060530     f                                     sfile(tb17s1:nrr)
001100060530
001200060530      *---------------------------------------------------------------*
001300060530      *  RIEPILOGO INDICATORI                                         *
001400060530      *---------------------------------------------------------------*
001500060530      * 01 = inserimento
001600060530      * 02 = variazione
001700060530      * 03 = annullato
001800060530      * 05 = visualizzo subfile lingue
001900060530      * 20 = GESTIONE SUBFILE
002000060530      * 21 = GESTIONE SUBFILE
002100060530      * 22 = GESTIONE SUBFILE
002200060530      * 28 = visualizza messaggio errore
002300060530      * 30 = Fine file
002400060530      * 31 = comodo
002500060530      * 32 = comodo
002600060530      * 40 = codice tassazione
002700060530      * 41 = descrizione
002800060530      * 79 = profilo NO EDP
002900060530      *--------------------------------------------------------------*
003000060530
003100060530      *--------------------------------------------------------------*
003200060530      * VARIABILI
003300060530      *--------------------------------------------------------------*
003400060530     d comando         s              1
003500060530     d kazlin          s                   like(lintabel)
003600060530     d kcod            s                   like(tblcod)
003700060530     d kkey            s                   like(tblkey)
003800060530     d kkut            s                   like(tblkut)
003900060530     d ordina          s              1
004000060531     d savtblftt       s                   like(tblftt)
004100060531     d savtblflt       s                   like(tblflt)
004200060530     d savtbluni       s                   like(tbluni)
004300060530     d nrr             s              4  0
004400060530     d savrec          s                   like(nrr)
004500060530     d wf16            s              1    inz('0')
004600060530     d wf5             s              1    inz('0')
004700060530     d wf6             s              1    inz('0')
004800060530
004900060530      *--------------------------------------------------------------*
005000060530      * SCHIERE
005100060530      *--------------------------------------------------------------*
005200060530     d msg             s             78    dim(10) ctdata perrcd(1)
005300060530
005400060530      *--------------------------------------------------------------*
005500060530      * STRUTTURE DATI
005600060530      *--------------------------------------------------------------*
005700060530     d datar           ds
005800060530     d  aar                    1      2
005900060530     d  mmr                    3      4
006000060530     d  ggr                    5      6
006100060530
006200060530     d datad           ds
006300060530     d  ggd                    1      2
006400060530     d  mmd                    3      4
006500060530     d  aad                    5      6
006600060530
006700060530     d azuteds       e ds                  extname(azute00f)
006800060530     d ddatiute      e ds
006900060530     d dsog          e ds
007000060530     d kpjba         e ds
007100060530     d tibs34ds      e ds
007200060530     d trul12ds      e ds
007300060530      *--------------------------------------------------------------*
007400060530
007500060530     c     ktab          klist
007600060530     c                   kfld                    kkut
007700060530     c                   kfld                    kcod
007800060530     c                   kfld                    kkey
007900060530
008000060530     c     *entry        plist
008100060530     c                   parm                    kpjba
008200060530
008300060530      * Reperisco dati job
008400060530     c                   exsr      datijob
008500060530
008600060530      * Richiamo pgm per controllo modificabilità tabella
008700060530     c                   clear                   trul12ds
008800060530     c                   eval      i12tla = 'L'
008900060530     c                   eval      i12nsi = knsif
009000060530     c                   eval      i12cod = '4U'
009100060530     c                   call      'TRUL12R'
009200060530     c                   parm                    trul12ds
009300060530     c                   if        o12err = '1'
009400060530     c                   goto      fine
009500060530     c                   endif
009600060530
009700060530     c                   eval      *in79 = *off
009800060530     c                   if        %subst(knmus:1:3) <> 'EDP'
009900060530     c                   eval      *in79 = *on
010000060530     c                   endif
010100060530
010200060530     c     inizio        tag
010300060530     c                   clear                   v1ccod
010400060530     c                   eval      v1ccod = '?'
010500060530     c                   eval      wf16 = *off
010600060530     c                   eval      wf5 = *off
010700060530     c                   eval      wf6 = *off
010800060530     c                   clear                   vmod
010900060530
011000060530
011100060530     c     inz01         tag
011200060530     c                   write     tb17t1
011300060530     c                   exfmt     tb17d1
011400060530     c                   clear                   v1cmsg
011500060530     c                   eval      *in28 = *off
011600060530     c                   eval      *in40 = *off
011700060530      * F3=Fine
011800060530     c   kc              goto      fine
011900060530      * codice obbligatorio
012000060530     c                   if        v1ccod = *blanks
012100060530     c                   eval      *in28 = *on
012200060530     c                   eval      *in40 = *on
012300060530     c                   eval      v1cmsg = msg(01)
012400060530     c                   goto      inz01
012500060530     c                   endif
012600060530      * ricerca codice tassazione
012700060530     c     '?'           scan      v1ccod                                 31
012800060530     c                   if        *in31
012900060530     c                   clear                   v1ccod
013000060530     c                   eval      kcod = 'OG'
013100060530     c                   clear                   kkey
013200060530     c                   call      'TRUL19R'
013300060530     c                   parm                    kcod
013400060530     c                   parm                    ordina
013500060530     c                   parm                    kkey
013600060530     c                   parm                    comando
013700060530     c                   movel     kkey          v1ccod
013800060530     c                   goto      inz01
013900060530     c                   endif
014000060530
014100060530     c                   setoff                                       010203
014200060530     c                   exsr      pulvid
014300060530
014400060530     c                   eval      kkut = 1
014500060530     c                   eval      kcod = 'OG'
014600060530     c                   movel     v1ccod        kkey
014700060530     c     ktab          chain     tabel00f
014800060530      * nuovo codice
014900060530     c                   if        not %found(tabel00f)
015000060530     c                   eval      *in01 = *on
015100060530     c                   eval      vmod = 'INSERIMENTO'
015200060530      * codice trovato
015300060530     c                   else
015400060530     c                   eval      dsog = tbluni
015500060530      * imposto i dati a video
015600060530     c                   exsr      impvid
015700060530      * codice annullato
015800060530     c                   if        tblflg = '*'
015900060530     c                   eval      *in03 = *on
016000060530     c                   eval      vmod = 'ANNULLATO  '
016100060530      * codice valido
016200060530     c                   else
016300060530     c                   eval      *in02 = *on
016400060530     c                   eval      vmod = 'MODIFICA   '
016500060530     c                   endif
016600060530     c                   endif
016700060530
016800060530     c     inz02         tag
016900060530
017000060530      * EMETTO FORMATO
017100060530     c                   write     tb17t1
017200060530     c                   exfmt     tb17d2
017300060530     c                   clear                   v1cmsg
017400060530     c                   eval      *in28 = *off
017500060530      * F3=Fine
017600060530     c   kc              goto      fine
017700060530      * F12=Ritorno
017800060530     c   kl              goto      inz01
017900060530      * F9=Altri dati
018000060530     c                   if        *inki
018100060530     c                   eval      *in05 = *on
018200060530     c                   exsr      sr_lingua
018300060530     c                   eval      *in05 = *off
018400060530     c                   goto      inz02
018500060530     c                   endif
018600060530      * F16=Annullamento
018700060530     c                   if        *inkq
018800060530     c                   eval      wf16 = *on
018900060530     c                   exsr      conferma
019000060530     c                   if        wf6 = *on
019100060530     c                   goto      inizio
019200060530     c                   else
019300060530     c                   eval      wf16 = *off
019400060530     c                   goto      inz02
019500060530     c                   endif
019600060530     c                   endif
019700060530      * F5=Ripristino
019800060530     c                   if        *inke
019900060530     c                   eval      wf5 = *on
020000060530     c                   exsr      conferma
020100060530     c                   if        wf6 = *on
020200060530     c                   goto      inizio
020300060530     c                   else
020400060530     c                   eval      wf5 = *off
020500060530     c                   goto      inz02
020600060530     c                   endif
020700060530     c                   endif
020800060530      * controllo dati
020900060530     c                   exsr      ctrfmt
021000060530     c   28              goto      inz02
021100060530
021200060530     c  nkf              goto      inz02
021300060530      * F6=Conferma
021400060530     c                   if        *inkf
021500060530     c                   exsr      conferma
021600060530     c                   if        wf6 = *on
021700060530      * tabelle in lingua
021800060530     c                   eval      *in05 = *off
021900060530     c                   exsr      sr_lingua
022000060530     c                   goto      inizio
022100060530     c                   else
022200060530     c                   goto      inz02
022300060530     c                   endif
022400060530     c                   endif
022500060530
022600060530     c     fine          tag
022700060530
022800060530     c                   eval      *inlr = *on
022900060530
023000060530      *---------------------------------------------------------------*
023100060530      * PULIZIA CAMPI VIDEATA
023200060530      *---------------------------------------------------------------*
023300060530     c     pulvid        begsr
023400060530
023500060530     c                   clear                   v1cdes
023600060530     c                   clear                   v1cfob
023700060530     c                   clear                   v1cfri
023800060530     c                   clear                   v1cffi
023900060530     c                   clear                   v1cfsf
024000060530     c                   clear                   v1cfsc
024100060530     c                   clear                   v1csig
024200060530     c                   clear                   v1cfov
024300060530     c                   clear                   v1cfrv
024400060530     c                   clear                   v1cff2
024500060530     c                   clear                   v1cfs1
024600060530     c                   clear                   v1cfs2
024700060530
024800060530     c                   endsr
024900060530
025000060530      *---------------------------------------------------------------*
025100060530      * IMPOSTO I CAMPI A VIDEO
025200060530      *---------------------------------------------------------------*
025300060530     c     impvid        begsr
025400060530
025500060530     c                   eval      v1cdes = §ogdes
025600060530     c                   eval      v1cfob = §ogfob
025700060530     c                   eval      v1cfri = §ogfri
025800060530     c                   eval      v1cffi = §ogffi
025900060530     c                   eval      v1cfsf = §ogfsf
026000060530     c                   eval      v1cfsc = §ogfsc
026100060530     c                   eval      v1csig = §ogsig
026200060530     c                   eval      v1cfov = §ogfov
026300060530     c                   eval      v1cfrv = §ogfrv
026400060530     c                   eval      v1cff2 = §ogff2
026500060530     c                   eval      v1cfs1 = §ogfs1
026600060530     c                   eval      v1cfs2 = §ogfs2
026700060530
026800060530     c                   endsr
026900060530
027000060530      *---------------------------------------------------------------*
027100060530      * CONTROLLI FORMATO
027200060530      *---------------------------------------------------------------*
027300060530     c     ctrfmt        begsr
027400060530     c                   setoff                                       414243
027500060530     c                   setoff                                       44
027600060530
027700060530      * DESCRIZIONE
027800060530     c                   if        v1cdes = *blanks
027900060530     c                   eval      *in28 = *on
028000060530     c                   eval      *in41 = *on
028100060530     c                   eval      v1cmsg = msg(02)
028200060530     c                   leavesr
028300060530     c                   endif
028400060530
028500060530     c                   endsr
028600060530
028700060530      *----------------------------------------------------------
028800060530      *    Videata conferma + dati trasmissione
028900060530      *----------------------------------------------------------
029000060530     c     conferma      begsr
029100060530
029200060530     c                   eval      kkut = 1
029300060530     c                   eval      kcod = 'OG'
029400060727     c                   movel(p)  v1ccod        kkey
029500060530     c     ktab          chain(n)  tabel00f
029600060530     c                   if        %found
029700060530     c                   eval      w1ftt = tblftt
029800060530     c                   eval      w1flt = tblflt
029900060530     c                   eval      w1ftr = tblftr
030000060530
030100060530     c                   move      tbldtr        datar
030200060530     c                   eval      aad = aar
030300060530     c                   eval      mmd = mmr
030400060530     c                   eval      ggd = ggr
030500060530     c                   move      datad         w1dtr
030600060530     c                   else
030700060530     c                   clear                   w1ftt
030800060530     c                   clear                   w1flt
030900060530     c                   clear                   w1ftr
031000060530     c                   clear                   w1dtr
031100060530     c                   endif
031200060530
031300060530     c                   do        *hival
031400060530     c                   exfmt     tb17w1
031500060530      * F12 - Ritorno
031600060530     c                   if        *inkl
031700060530     c                   leave
031800060530     c                   endif
031900060530
032000060530      * F6 - Conferma
032100060530     c                   if        *inkf
032200060530     c                   exsr      scrivi
032300060530     c                   eval      wf6 = *on
032400060530     c                   leave
032500060530     c                   endif
032600060530
032700060530     c                   enddo
032800060530
032900060530     c                   endsr
033000060530
033100060530      *---------------------------------------------------------------*
033200060530      * AGGIORNAMENTO / INSERIMENTO
033300060530      *---------------------------------------------------------------*
033400060530     c     scrivi        begsr
033500060530
033600060530     c                   eval      §ogdes = v1cdes
033700060530     c                   eval      §ogfob = v1cfob
033800060530     c                   eval      §ogfri = v1cfri
033900060530     c                   eval      §ogffi = v1cffi
034000060530     c                   eval      §ogfsf = v1cfsf
034100060530     c                   eval      §ogfsc = v1cfsc
034200060530     c                   eval      §ogsig = v1csig
034300060530     c                   eval      §ogfov = v1cfov
034400060530     c                   eval      §ogfrv = v1cfrv
034500060530     c                   eval      §ogff2 = v1cff2
034600060530     c                   eval      §ogfs1 = v1cfs1
034700060530     c                   eval      §ogfs2 = v1cfs2
034800060530
034900060530     c                   eval      kkut = 1
035000060530     c                   eval      kcod = 'OG'
035100060530     c                   movel     v1ccod        kkey
035200060530     c     ktab          chain     tabel00f
035300060530
035400060530     c                   eval      tbluni = dsog
035500060530
035600060530     c                   if        wf16 = *on
035700060530     c                   eval      tblflg = '*'
035800060530     c                   endif
035900060530
036000060530     c                   if        wf5 = *on
036100060530     c                   clear                   tblflg
036200060530     c                   endif
036300060530
036400060530     c                   eval      tblftt = w1ftt
036500060530     c                   clear                   tblftr
036600060530     c                   clear                   tbldtr
036700060530
036800060530      * scrivo
036900060530     c                   if        not %found(tabel00f)
037000060530     c                   eval      tblkut = kkut
037100060530     c                   eval      tblcod = kcod
037200060530     c                   eval      tblkey = kkey
037300060530     c                   write     tabel
037400060530
037500060530      * aggiorno
037600060530     c                   else
037700060530     c                   update    tabel
037800060530     c                   endif
037900060530
038000060530      * se annullamento o ripristino devo fare la stessa cosa anche
038100060530      * sulle tabelle in lingua
038200060530     c                   if        wf16 = *on or wf5 = *on
038300060530      * aggancio le lingue presenti su AZLIN
038400060530     c                   move      kkut          kazlin
038500060530     c     kazlin        setgt     azlin01l
038600060530     c                   do        *hival
038700060530     c                   read      azlin01l
038800060530     c                   if        %eof(azlin01l)
038900060530     c                   leave
039000060530     c                   endif
039100060530      * per ogni lingua aggiorno
039200060530     c                   move      lintabel      kkut
039300060530     c                   eval      kcod = 'OG'
039400060530     c                   movel     v1ccod        kkey
039500060530     c     ktab          chain     tabel00f
039600060530     c                   if        %found(tabel00f)
039700060530     c                   if        wf16 = *on
039800060530     c                   eval      tblflg = '*'
039900060530     c                   endif
040000060530     c                   if        wf5 = *on
040100060530     c                   clear                   tblflg
040200060530     c                   endif
040300060530     c                   eval      tblftt = w1ftt
040400060530     c                   clear                   tblftr
040500060530     c                   clear                   tbldtr
040600060530     c                   update    tabel
040700060530     c                   endif
040800060530     c                   enddo
040900060530     c                   endif
041000060530
041100060530     c                   endsr
041200060530
041300060530      *---------------------------------------------------------------*
041400060530      * gestione tabella in lingua
041500060530      *---------------------------------------------------------------*
041600060530     c     sr_lingua     begsr
041700060530
041800060530      * pulisco il subfile
041900060530     c                   exsr      sr_pulsfl
042000060530      * carico il subfile con i dati che trovo o vuoto
042100060530     c                   exsr      sr_carsfl
042200060530      * emetto il subfile
042300060530     c     emisfl        tag
042400060530     c                   eval      rec = 1
042500060530     c                   if        savrec > 0
042600060530     c                   eval      rec = savrec
042700060530     c                   endif
042800060530     c                   write     tb17t1
042900060530     c                   write     tb17z1
043000060530     c                   exfmt     tb17c1
043100060530
043200060530      * F12 - Ritorno
043300060530     c                   if        *inkl
043400060530     c                   setoff                                       28
043500060530     c                   clear                   v1cmsg
043600060530     c                   goto      inz02
043700060530     c                   endif
043800060530
043900060530     c                   setoff                                       28
044000060530     c                   setoff                                       45
044100060530      * Controlli
044200060530     c  n05              exsr      sr_ctrsfl
044300060530     c   28
044400060530     cornkf              goto      emisfl
044500060530
044600060530      * F6 - Conferma
044700060530     c   kf              exsr      sr_confsfl
044800060530
044900060530     c                   endsr
045000060530
045100060530      *---------------------------------------------------------------*
045200060530      * pulisco il subfile
045300060530      *---------------------------------------------------------------*
045400060530     c     sr_pulsfl     begsr
045500060530
045600060530     c                   clear                   nrr
045700060530     c                   clear                   savrec
045800060530     c                   eval      *in20 = *off
045900060530     c                   eval      *in21 = *off
046000060530     c                   write     tb17c1
046100060530     c                   eval      *in20 = *on
046200060530     c                   eval      *in21 = *on
046300060530
046400060530     c                   endsr
046500060530
046600060530      *---------------------------------------------------------------*
046700060530      * carico il subfile
046800060530      *---------------------------------------------------------------*
046900060530     c     sr_carsfl     begsr
047000060530
047100060530     c                   setoff                                       30
047200060530      * carico un record per ogni lingua inserite in AZLIN
047300060530     c                   move      '1'           kazlin
047400060530     c     kazlin        setgt     azlin01l
047500060530     c                   do        *hival
047600060530     c                   read      azlin01l
047700060530      * fine file
047800060530     c                   if        %eof(azlin01l)
047900060530     c                   leave
048000060530     c                   endif
048100060530     c                   eval      s1dlin = lindesita
048200060530     c                   clear                   dsog
048300060530     c                   move      lintabel      kkut
048400060530     c                   eval      kcod = 'OG'
048500060530     c                   eval      kkey = v1ccod
048600060530     c     ktab          chain(n)  tabel00f
048700060530     c                   if        not %found(tabel00f)
048800060530     c                   clear                   s1cdes
048900060530     c                   else
049000060530     c                   eval      dsog = tbluni
049100060530     c                   eval      s1cdes = §ogdes
049200060530     c                   endif
049300060530     c                   eval      s1hcdlin = kkut
049400060530     c                   add       1             nrr
049500060530     c                   write     tb17s1
049600060530     c                   enddo
049700060530
049800060530     c                   seton                                        30
049900060530
050000060530     c                   endsr
050100060530
050200060530      *---------------------------------------------------------------*
050300060530      * controllo il subfile
050400060530      *---------------------------------------------------------------*
050500060530     c     sr_ctrsfl     begsr
050600060530
050700060530     c                   clear                   nrr
050800060530     c                   do        *hival
050900060530     c                   eval      nrr = nrr +1
051000060530     c     nrr           chain     tb17s1                             32
051100060530     c                   if        *in32
051200060530     c                   leave
051300060530     c                   endif
051400060530      * deve esserci la descrizione
051500060530     c                   if        s1cdes = *blanks
051600060530     c                   eval      v1cmsg = msg(02)
051700060530     c                   eval      *in28 = *on
051800060530     c                   eval      *in45 = *on
051900060530     c                   eval      savrec = nrr
052000060530     c                   update    tb17s1
052100060530     c                   leavesr
052200060530     c                   endif
052300060530     c                   update    tb17s1
052400060530     c                   enddo
052500060530
052600060530     c                   endsr
052700060530
052800060530      *---------------------------------------------------------------*
052900060530      * confermo i dati del subfile
053000060530      *---------------------------------------------------------------*
053100060530     c     sr_confsfl    begsr
053200060530
053300060530      * recupero i dati della tabella in italiano
053400060530     c                   clear                   savtbluni
053500060530     c                   eval      kkut = 1
053600060530     c                   eval      kcod = 'OG'
053700060530     c                   movel     v1ccod        kkey
053800060530     c     ktab          chain(n)  tabel00f
053900060530     c                   if        %found
054000060530     c                   eval      savtbluni = tbluni
054100060530     c                   endif
054200060530
054300060530      * leggo il subfile e aggiorno/scrivo i dati in lingua
054400060530     c                   clear                   nrr
054500060530     c                   do        *hival
054600060530     c                   eval      nrr = nrr +1
054700060530     c     nrr           chain     tb17s1                             32
054800060530     c                   if        *in32
054900060530     c                   leave
055000060530     c                   endif
055100060530     c                   eval      kkut = s1hcdlin
055200060530     c     ktab          chain     tabel00f
055300060530     c                   eval      dsog = savtbluni
055400060530     c                   eval      §ogdes = s1cdes
055500060530     c                   eval      tbluni = dsog
055600060530     c                   eval      tblftt = w1ftt
055700060530     c                   clear                   tblftr
055800060530     c                   clear                   tbldtr
055900060530      * scrivo
056000060530     c                   if        not %found(tabel00f)
056100060530     c                   eval      tblkut = kkut
056200060530     c                   eval      tblcod = kcod
056300060530     c                   eval      tblkey = kkey
056400060530     c                   write     tabel
056500060530      * aggiorno
056600060530     c                   else
056700060530     c                   update    tabel
056800060530     c                   endif
056900060530
057000060530     c                   enddo
057100060530
057200060530     c                   endsr
057300060530
057400060530      *---------------------------------------------------------------*
057500060530      * Reperimento Dati del job (Utente/Operativi)                   *
057600060530      *---------------------------------------------------------------*
057700060530     c     datijob       begsr
057800060530      *
057900060530     c     *dtaara       define    §azute        azuteds
058000060530     c     *dtaara       define    §datiute      ddatiute
058100060530      *
058200060530     c                   in(e)     *dtaara
058300060530     c                   if        %error or rsut = *blanks
058400060530     c                   clear                   tibs34ds
058500060530     c                   call      'TIBS34R'
058600060530     c                   parm                    tibs34ds
058700060530     c                   in        *dtaara
058800060530     c                   endif
058900060530
059000060530     c                   endsr
059100060530
059200060530
059300060530** -- MSG -------------------------------------------------------------------*
059400060530Immettere il codice                                                            1
059500060530Immettere la descrizione                                                       2
