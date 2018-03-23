000100060525     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060525
000300060525      *--------------------------------------------------------------*
000400060525      * GESTIONE TABELLA CONDIZIONI DI PAGAMENTO                     *
000500060525      *--------------------------------------------------------------*
000600060525
000700060525     ftabel00f  uf a e           k disk
000800060525     fazlin01l  if   e           k disk
000900060525     fcnc810d   cf   e             workstn
001000060525     f                                     sfile(c810s1:nrr)
001100060525
001200060525      *---------------------------------------------------------------*
001300060525      *  RIEPILOGO INDICATORI                                         *
001400060525      *---------------------------------------------------------------*
001500060525      * 01 = inserimento
001600060525      * 02 = variazione
001700060525      * 03 = annullato
001800060525      * 05 = visualizzo subfile lingue
001900060526      * 12 = sede
002000060525      * 20 = GESTIONE SUBFILE
002100060525      * 21 = GESTIONE SUBFILE
002200060525      * 22 = GESTIONE SUBFILE
002300060525      * 28 = visualizza messaggio errore
002400060525      * 30 = Fine file
002500060525      * 31 = comodo
002600060525      * 32 = comodo
002700060525      * 40 = codice tassazione
002800060525      * 41 = descrizione
002900060526      * 42 = modalità pagamento
003000060526      * 43 = gg1
003100060526      * 44 = gg1
003200060526      * 45 = rate
003300060526      * 46 = almeno 2 rate
003400060526      * 47 = buchi nelle rate
003500060526      * 48 = errore sbfl
003600060525      *--------------------------------------------------------------*
003700060525
003800060525      *--------------------------------------------------------------*
003900060525      * VARIABILI
004000060525      *--------------------------------------------------------------*
004100060525     d kazlin          s                   like(lintabel)
004200060525     d kcod            s                   like(tblcod)
004300060525     d kkey            s                   like(tblkey)
004400060525     d kkut            s                   like(tblkut)
004500060526     d mdpcod          s              1
004600060526     d mdptip          s              1
004700060525     d pagcod          s              3
004800060525     d pagdes          s             30
004900060525     d savtbluni       s                   like(tbluni)
005000060525     d nrr             s              4  0
005100060525     d savrec          s                   like(nrr)
005200060525     d wf16            s              1    inz('0')
005300060525     d wf5             s              1    inz('0')
005400060525     d wf6             s              1    inz('0')
005500060526     d w0020           s              2  0 inz
005600060526     d xx              s              2  0
005700060526     d yy              s              2  0
005800060525
005900060525      *--------------------------------------------------------------*
006000060525      * SCHIERE
006100060525      *--------------------------------------------------------------*
006200060525     d msg             s             78    dim(10) ctdata perrcd(1)
006300060525
006400060525      *--------------------------------------------------------------*
006500060525      * STRUTTURE DATI
006600060525      *--------------------------------------------------------------*
006700060525     d datar           ds
006800060525     d  aar                    1      2
006900060525     d  mmr                    3      4
007000060525     d  ggr                    5      6
007100060525
007200060525     d datad           ds
007300060525     d  ggd                    1      2
007400060525     d  mmd                    3      4
007500060525     d  aad                    5      6
007600060525
007700060525     d parm81b         ds
007800060525     d  pcod                   1      3
007900060525     d  plin                   4      4  0
008000060525     d  pfil                   5      8
008100060525
008200060525     d dsrat           ds
008300060525     d  rt1                    1      3  0
008400060525     d  rt2                    4      6  0
008500060525     d  rt3                    7      9  0
008600060525     d  rt4                   10     12  0
008700060525     d  rt5                   13     15  0
008800060525     d  rt6                   16     18  0
008900060525     d  rt7                   19     21  0
009000060525     d  rt8                   22     24  0
009100060525     d  rt9                   25     27  0
009200060525     d  rt10                  28     30  0
009300060525     d  rat                    1     30  0 dim(10)
009400060525
009500060525     d azuteds       e ds                  extname(azute00f)
009600060525     d dsfa          e ds
009700060525     d ddatiute      e ds
009800060525     d kpjba         e ds
009900060525     d tibs34ds      e ds
010000060525     d trul12ds      e ds
010100060525      *--------------------------------------------------------------*
010200060525
010300060525     c     ktab          klist
010400060525     c                   kfld                    kkut
010500060525     c                   kfld                    kcod
010600060525     c                   kfld                    kkey
010700060525
010800060525     c     *entry        plist
010900060525     c                   parm                    kpjba
011000060525
011100060525      * Reperisco dati job
011200060525     c                   exsr      datijob
011300060525
011400060525      * Richiamo pgm per controllo modificabilità tabella
011500060525     c                   clear                   trul12ds
011600060525     c                   eval      i12tla = 'L'
011700060525     c                   eval      i12nsi = knsif
011800060525     c                   eval      i12cod = 'FA'
011900060525     c                   call      'TRUL12R'
012000060525     c                   parm                    trul12ds
012100060525     c                   if        o12err = '1'
012200060525     c                   goto      fine
012300060525     c                   endif
012400060525
012500060525     c     inizio        tag
012600060525     c                   clear                   v1ccod
012700060525     c                   eval      v1ccod = '?'
012800060525     c                   eval      wf16 = *off
012900060525     c                   eval      wf5 = *off
013000060525     c                   eval      wf6 = *off
013100060525     c                   clear                   vmod
013200060525
013300060525     c     inz01         tag
013400060525     c                   write     c810t1
013500060525     c                   exfmt     c810d1
013600060525     c                   clear                   v1cmsg
013700060525     c                   eval      *in28 = *off
013800060525     c                   eval      *in40 = *off
013900060525      * F3=Fine
014000060525     c   kc              goto      fine
014100060525
014200060525      * F8=Stampa
014300060525     c                   if        *inkh
014400060525     c                   clear                   parm81b
014500060525     c                   eval      plin = 1
014600060525     c                   eval      kpjbu = parm81b
014700060525     c                   call      'CNC81BR'
014800060525     c                   parm                    kpjba
014900060525     c                   goto      fine
015000060525     c                   endif
015100060525
015200060525      * codice obbligatorio
015300060525     c                   if        v1ccod = *blanks
015400060525     c                   eval      *in28 = *on
015500060525     c                   eval      *in40 = *on
015600060525     c                   eval      v1cmsg = msg(01)
015700060525     c                   goto      inz01
015800060525     c                   endif
015900060525
016000060525      * ricerca codice tassazione
016100060525     c     '?'           scan      v1ccod                                 31
016200060525     c                   if        *in31
016300060525     c                   clear                   v1ccod
016400060525     c                   call      'XCDPAGR'
016500060525     c                   parm                    pagcod
016600060525     c                   parm                    pagdes
016700060525     c                   movel     pagcod        v1ccod
016800060525     c                   goto      inz01
016900060525     c                   endif
017000060525
017100060525     c                   setoff                                       010203
017200060525     c                   exsr      pulvid
017300060525
017400060526      * il codice pagamento è composto dal codice + 1 che sarebbe la lingua, ma la
017500060526      * lingua la gestiamo con il tblkut quindi imposto fisso 1
017600060525     c                   eval      kkut = 1
017700060525     c                   eval      kcod = 'FA'
017800060526     c                   clear                   kkey
017900060526     c                   eval      %subst(kkey:1:3) = v1ccod
018000060526     c                   eval      %subst(kkey:4:4) = '1'
018100060525     c     ktab          chain     tabel00f
018200060525      * nuovo codice
018300060525     c                   if        not %found(tabel00f)
018400060525     c                   eval      *in01 = *on
018500060525     c                   eval      vmod = 'INSERIMENTO'
018600060525      * codice trovato
018700060525     c                   else
018800060525     c                   eval      dsfa = tbluni
018900060525      * imposto i dati a video
019000060525     c                   exsr      impvid
019100060525      * codice annullato
019200060525     c                   if        tblflg = '*'
019300060525     c                   eval      *in03 = *on
019400060525     c                   eval      vmod = 'ANNULLATO  '
019500060525      * codice valido
019600060525     c                   else
019700060525     c                   eval      *in02 = *on
019800060525     c                   eval      vmod = 'MODIFICA   '
019900060525     c                   endif
020000060525     c                   endif
020100060525
020200060525     c     inz02         tag
020300060525      * decodifico
020400060525     c                   exsr      ricdes
020500060525
020600060525      * EMETTO FORMATO
020700060525     c                   write     c810t1
020800060525     c                   exfmt     c810d2
020900060525     c                   clear                   v1cmsg
021000060525     c                   eval      *in28 = *off
021100060525      * F3=Fine
021200060525     c   kc              goto      fine
021300060525      * F12=Ritorno
021400060526     c   kl              goto      inizio
021500060525      * F9=Altri dati
021600060525     c                   if        *inki
021700060525     c                   eval      *in05 = *on
021800060525     c                   exsr      sr_lingua
021900060525     c                   eval      *in05 = *off
022000060525     c                   goto      inz02
022100060525     c                   endif
022200060525      * F16=Annullamento
022300060525     c                   if        *inkq
022400060525     c                   eval      wf16 = *on
022500060525     c                   exsr      conferma
022600060525     c                   if        wf6 = *on
022700060525     c                   goto      inizio
022800060525     c                   else
022900060525     c                   eval      wf16 = *off
023000060525     c                   goto      inz02
023100060525     c                   endif
023200060525     c                   endif
023300060525      * F5=Ripristino
023400060525     c                   if        *inke
023500060525     c                   eval      wf5 = *on
023600060525     c                   exsr      conferma
023700060525     c                   if        wf6 = *on
023800060525     c                   goto      inizio
023900060525     c                   else
024000060525     c                   eval      wf5 = *off
024100060525     c                   goto      inz02
024200060525     c                   endif
024300060525     c                   endif
024400060525      * controllo dati
024500060525     c                   exsr      ctrfmt
024600060525     c   28              goto      inz02
024700060525
024800060525     c  nkf              goto      inz02
024900060525      * F6=Conferma
025000060525     c                   if        *inkf
025100060525     c                   exsr      conferma
025200060525     c                   if        wf6 = *on
025300060525      * tabelle in lingua
025400060525     c                   eval      *in05 = *off
025500060525     c                   exsr      sr_lingua
025600060525     c                   goto      inizio
025700060525     c                   else
025800060525     c                   goto      inz02
025900060525     c                   endif
026000060525     c                   endif
026100060525
026200060525     c     fine          tag
026300060525
026400060525     c                   eval      *inlr = *on
026500060525
026600060525      *---------------------------------------------------------------*
026700060525      * PULIZIA CAMPI VIDEATA
026800060525      *---------------------------------------------------------------*
026900060525     c     pulvid        begsr
027000060525
027100060525     c                   clear                   dsfa
027200060525     c                   clear                   v1cdes
027300060525     c                   clear                   tipo
027400090416     c                   clear                   v1cuti
027500060525     c                   clear                   vidmdp
027600060525     c                   clear                   decmdp
027700060525     c                   clear                   eff
027800060525     c                   clear                   iva
027900060525     c                   clear                   sol
028000060525     c                   clear                   gg1
028100060525     c                   clear                   gg2
028200060525     c                   clear                   sconto
028300060525     c                   clear                   dsrat
028400060525     c                   clear                   nrat
028500060525
028600060525     c                   endsr
028700060525
028800060525      *---------------------------------------------------------------*
028900060525      * IMPOSTO I DATI A VIDEO
029000060525      *---------------------------------------------------------------*
029100060525     c     impvid        begsr
029200060525
029300060525     c                   eval      v1cdes = §fades
029400060526     c                   move      §fatpg        tipo
029500090416     c                   eval      v1cuti = §fauti
029600060525     c                   eval      vidmdp = §famdp
029700060525     c                   if        §fatra = '1'
029800060525     c                   eval      eff = 'SI'
029900060525     c                   else
030000060525     c                   eval      eff = 'NO'
030100060525     c                   endif
030200060525     c                   movel     §fatti        iva
030300060525     c                   eval      sol = §fasol
030400060525     c                   eval      gg1 = §faggi
030500060525     c                   eval      gg2 = §faggf
030600060525     c                   eval      sconto = §fasco
030700060525     c                   eval      dsrat = §farat
030800060525     c                   eval      nrat = §fantr
030900060525
031000060525     c                   endsr
031100060525
031200060525      *---------------------------------------------------------------*
031300060525      * RICERCA DESCRIZIONI
031400060525      *---------------------------------------------------------------*
031500060525     c     ricdes        begsr
031600060525
031700060525      * modalità pagamento
031800060526      * la tabella è solo di sede quindi decodifico solo se sono in ambiente di sede
031900060526      * ho fatto questo controllo perchè prima il vecchio programma dava la possibilità
032000060526      * di manutenzionare la tabella anche in filiale
032100060526      * ora non lo fa ma non capisco il perchè visto che richiamo sempre il trul12r
032200060526      * come faceva prima
032300060526     c                   if        vidmdp <> *blanks and *in12
032400060526     c                   clear                   decmdp
032500060525     c                   eval      kkut = 1
032600060525     c                   eval      kcod = '18'
032700060525     c                   eval      kkey = *all'0'
032800060526     c                   if        tipo = 1 or tipo = 2
032900060526     c                   eval      %subst(kkey:7:1) = vidmdp
033000060526     c                   eval      %subst(kkey:8:1) = *blanks
033100060525     c                   else
033200060526     c                   eval      %subst(kkey:7:1) = *blanks
033300060526     c                   eval      %subst(kkey:8:1) = vidmdp
033400060525     c                   endif
033500060525     c     ktab          chain(n)  tabel00f
033600060525     c                   if        %found(tabel00f)
033700060525     c                   eval      decmdp = tbluni
033800060525     c                   endif
033900060525     c                   endif
034000060525
034100060525     c                   endsr
034200060525
034300060525      *---------------------------------------------------------------*
034400060525      * CONTROLLI FORMATO
034500060525      *---------------------------------------------------------------*
034600060525     c     ctrfmt        begsr
034700060526
034800060526     c                   setoff                                       4142
034900060526     c                   setoff                                       4344
035000060526     c                   setoff                                       454647
035100060525
035200060526      * descrizione
035300060525     c                   if        v1cdes = *blanks
035400060525     c                   eval      *in28 = *on
035500060525     c                   eval      *in41 = *on
035600060525     c                   eval      v1cmsg = msg(02)
035700060525     c                   leavesr
035800060525     c                   endif
035900060526
036000060526      * modalità di pagamento
036100060526      * controllo solo se sono in sede, se sono in filiale il campo è protetto
036200060526      * dato che la tabella relativa alla modalità di pagamento è solo di sede
036300060526      * ho fatto questo controllo perchè prima il vecchio programma dava la possibilità
036400060526      * di manutenzionare la tabella anche in filiale
036500060526      * ora non lo fa ma non capisco il perchè visto che richiamo sempre il trul12r
036600060526      * come faceva prima
036700060526     c                   if        vidmdp <> *blanks and *in12
036800060526      * ricerca modalità di pagamento
036900060526     c                   if        vidmdp = '?'
037000060526     c                   call      'CNC018R1'
037100060526     c                   parm                    mdptip
037200060526     c                   parm                    mdpcod
037300060526     c                   parm                    rsut
037400060526     c                   parm                    knsif
037500060526     c                   parm                    w0020
037600060526     c                   parm                    w0020
037700060526     c                   eval      vidmdp = mdpcod
037800060526     c                   eval      *in42 = *on
037900060526     c                   goto      inz02
038000060526     c                   endif
038100060526      * controllo
038200060526     c                   clear                   decmdp
038300060526     c                   eval      kkut = 1
038400060526     c                   eval      kcod = '18'
038500060526     c                   eval      kkey = *all'0'
038600060526     c                   if        tipo = 1 or tipo = 2
038700060526     c                   eval      %subst(kkey:7:1) = vidmdp
038800060526     c                   eval      %subst(kkey:8:1) = *blanks
038900060526     c                   else
039000060526     c                   eval      %subst(kkey:7:1) = *blanks
039100060526     c                   eval      %subst(kkey:8:1) = vidmdp
039200060526     c                   endif
039300060526     c     ktab          chain(n)  tabel00f
039400060526     c                   if        not %found(tabel00f) or tblflg <> *blanks
039500060526     c                   eval      *in28 = *on
039600060526     c                   eval      *in42 = *on
039700060526     c                   eval      v1cmsg = msg(03)
039800060526     c                   leavesr
039900060526     c                   endif
040000060526     c                   eval      decmdp = tbluni
040100060526     c                   endif
040200060526
040300060526      * se gli effetti sono da stampare controllo i campi
040400060526      * gg inizio - sconto - rate
040500060526     c                   if        eff = 'SI'
040600060526      * gg (99 indica fine mese)
040700060526      * se ho immesso gg2 devo immettere anche gg1
040800060526     c                   if        gg2 <> *zeros and gg2 <> 90 and gg1 = *zeros
040900060526     c                   eval      *in28 = *on
041000060526     c                   eval      *in43 = *on
041100060526     c                   eval      v1cmsg = msg(04)
041200060526     c                   leavesr
041300060526     c                   endif
041400060526      * gg1 può essere solo 99 o tra 1 e 31
041500060526     c                   if        gg1 <> 99 and gg1 > 31
041600060526     c                   eval      *in28 = *on
041700060526     c                   eval      *in43 = *on
041800060526     c                   eval      v1cmsg = msg(05)
041900060526     c                   leavesr
042000060526     c                   endif
042100060526      * gg2 può essere solo 99/95/90 o tra 1 e 31
042200060526     c                   if        gg2 <> 99 and gg2 <> 95 and gg2 <> 90 and
042300060526     c                             gg2 > 31
042400060526     c                   eval      *in28 = *on
042500060526     c                   eval      *in44 = *on
042600060526     c                   eval      v1cmsg = msg(05)
042700060526     c                   leavesr
042800060526     c                   endif
042900060526      * rate
043000060526      * deve esserci la prima rata
043100060526     c                   if        rt1 = *zeros
043200060526     c                   eval      *in28 = *on
043300060526     c                   eval      *in45 = *on
043400060526     c                   eval      v1cmsg = msg(06)
043500060526     c                   leavesr
043600060526     c                   endif
043700060526      * se trattamento IVA = 2 deve esserci anche la seconda rata
043800060526     c                   if        iva = 2 and rt2 = *zeros
043900060526     c                   eval      *in28 = *on
044000060526     c                   eval      *in46 = *on
044100060526     c                   eval      v1cmsg = msg(07)
044200060526     c                   leavesr
044300060526     c                   endif
044400060526      * non ci possono essere buchi nelle rate
044500060526     c                   eval      xx = 1
044600060526     c                   for       xx by 1 to 10
044700060526     c                   if        rat(xx) = *zeros
044800060526     c                   leave
044900060526     c                   endif
045000060526     c                   endfor
045100060526     c                   eval      yy = xx
045200060526     c                   for       yy by 1 to 10
045300060526     c                   if        rat(yy) <> *zeros
045400060526     c                   eval      *in28 = *on
045500060526     c                   eval      *in47 = *on
045600060526     c                   eval      v1cmsg = msg(08)
045700060526     c                   leavesr
045800060526     c                   endif
045900060526     c                   endfor
046000060526      * imposto il nr. delle rate
046100060526     c                   eval      nrat = xx - 1
046200060526     c                   endif
046300060526      * se gli effetti non sono da stampare azzeri i campi
046400060526      * gg inizio - sconto - rate
046500060526     c                   if        eff = 'NO'
046600060526      * azzero le rate
046700060526     c                   eval      xx = 1
046800060526     c                   for       xx by 1 to 10
046900060526     c                   eval      rat(xx) = *zeros
047000060526     c                   endfor
047100060526      * azzero il nr. delle rate
047200060526     c                   clear                   nrat
047300060526      * azzero IVA
047400060526     c                   clear                   iva
047500060526      * azzero i gg
047600060526     c                   clear                   gg1
047700060526     c                   clear                   gg2
047800060526      * azzero lo sconto
047900060526     c                   clear                   sconto
048000060526     c                   endif
048100060525
048200060525     c                   endsr
048300060525
048400060525      *----------------------------------------------------------
048500060525      *    Videata conferma + dati trasmissione
048600060525      *----------------------------------------------------------
048700060525     c     conferma      begsr
048800060525
048900060525     c                   eval      kkut = 1
049000060526     c                   eval      kcod = 'FA'
049100060526     c                   clear                   kkey
049200060526     c                   eval      %subst(kkey:1:3) = v1ccod
049300060526     c                   eval      %subst(kkey:4:4) = '1'
049400060525     c     ktab          chain(n)  tabel00f
049500060525     c                   if        %found
049600060525     c                   eval      w1ftt = tblftt
049700060525     c                   eval      w1flt = tblflt
049800060525     c                   eval      w1ftr = tblftr
049900060525
050000060526     c                   move      tbldtr        datar
050100060525     c                   eval      aad = aar
050200060525     c                   eval      mmd = mmr
050300060525     c                   eval      ggd = ggr
050400060526     c                   move      datad         w1dtr
050500060525     c                   else
050600060525     c                   clear                   w1ftt
050700060525     c                   clear                   w1flt
050800060525     c                   clear                   w1ftr
050900060525     c                   clear                   w1dtr
051000060525     c                   endif
051100060525
051200060525     c                   do        *hival
051300060526     c                   exfmt     c810w1
051400060525      * F12 - Ritorno
051500060525     c                   if        *inkl
051600060525     c                   leave
051700060525     c                   endif
051800060525
051900060525      * F6 - Conferma
052000060525     c                   if        *inkf
052100060525     c                   exsr      scrivi
052200060525     c                   eval      wf6 = *on
052300060525     c                   leave
052400060525     c                   endif
052500060525
052600060525     c                   enddo
052700060525
052800060525     c                   endsr
052900060525
053000060525      *---------------------------------------------------------------*
053100060525      * AGGIORNAMENTO / INSERIMENTO
053200060525      *---------------------------------------------------------------*
053300060525     c     scrivi        begsr
053400060525
053500060526     c                   eval      §fades = v1cdes
053600060526     c                   movel     tipo          §fatpg
053700090416     c                   eval      §fauti = v1cuti
053800060526     c                   if        eff = 'SI'
053900060526     c                   eval      §fatra = '1'
054000060526     c                   else
054100060526     c                   eval      §fatra = '0'
054200060526     c                   endif
054300060526     c                   movel     iva           §fatti
054400060526     c                   eval      §faggi = gg1
054500060526     c                   eval      §faggf = gg2
054600060526     c                   movel     dsrat         §farat
054700060526     c                   eval      §fasco = sconto
054800060526     c                   eval      §fantr = nrat
054900060526     c                   eval      §famdp = vidmdp
055000060525
055100060525     c                   eval      kkut = 1
055200060526     c                   eval      kcod = 'FA'
055300060526     c                   clear                   kkey
055400060526     c                   eval      %subst(kkey:1:3) = v1ccod
055500060526     c                   eval      %subst(kkey:4:4) = '1'
055600060525     c     ktab          chain     tabel00f
055700060525
055800060526     c                   eval      tbluni = dsfa
055900060525
056000060525     c                   if        wf16 = *on
056100060525     c                   eval      tblflg = '*'
056200060525     c                   endif
056300060525
056400060525     c                   if        wf5 = *on
056500060525     c                   clear                   tblflg
056600060525     c                   endif
056700060525
056800060525     c                   eval      tblftt = w1ftt
056900060525     c                   clear                   tblftr
057000060525     c                   clear                   tbldtr
057100060525
057200060525      * scrivo
057300060525     c                   if        not %found(tabel00f)
057400060525     c                   eval      tblkut = kkut
057500060525     c                   eval      tblcod = kcod
057600060525     c                   eval      tblkey = kkey
057700060525     c                   write     tabel
057800060525
057900060525      * aggiorno
058000060525     c                   else
058100060525     c                   update    tabel
058200060525     c                   endif
058300060525
058400060525      * se annullamento o ripristino devo fare la stessa cosa anche
058500060525      * sulle tabelle in lingua
058600060525     c                   if        wf16 = *on or wf5 = *on
058700060525      * aggancio le lingue presenti su AZLIN
058800060525     c                   move      kkut          kazlin
058900060525     c     kazlin        setgt     azlin01l
059000060525     c                   do        *hival
059100060525     c                   read      azlin01l
059200060525     c                   if        %eof(azlin01l)
059300060525     c                   leave
059400060525     c                   endif
059500060525      * per ogni lingua aggiorno
059600060525     c                   move      lintabel      kkut
059700060526     c                   eval      kcod = 'FA'
059800060526     c                   clear                   kkey
059900060526     c                   eval      %subst(kkey:1:3) = v1ccod
060000060526     c                   eval      %subst(kkey:4:4) = '1'
060100060525     c     ktab          chain     tabel00f
060200060525     c                   if        %found(tabel00f)
060300060525     c                   if        wf16 = *on
060400060525     c                   eval      tblflg = '*'
060500060525     c                   endif
060600060525     c                   if        wf5 = *on
060700060525     c                   clear                   tblflg
060800060525     c                   endif
060900060525     c                   eval      tblftt = w1ftt
061000060525     c                   clear                   tblftr
061100060525     c                   clear                   tbldtr
061200060525     c                   update    tabel
061300060525     c                   endif
061400060525     c                   enddo
061500060525     c                   endif
061600060525
061700060525     c                   endsr
061800060525
061900060525      *---------------------------------------------------------------*
062000060525      * gestione tabella in lingua
062100060525      *---------------------------------------------------------------*
062200060525     c     sr_lingua     begsr
062300060525
062400060525      * pulisco il subfile
062500060525     c                   exsr      sr_pulsfl
062600060525      * carico il subfile con i dati che trovo o vuoto
062700060525     c                   exsr      sr_carsfl
062800060525      * emetto il subfile
062900060525     c     emisfl        tag
063000060525     c                   eval      rec = 1
063100060525     c                   if        savrec > 0
063200060525     c                   eval      rec = savrec
063300060525     c                   endif
063400060526     c                   write     c810t1
063500060526     c                   write     c810z1
063600060526     c                   exfmt     c810c1
063700060525
063800060525      * F12 - Ritorno
063900060525     c                   if        *inkl
064000060525     c                   setoff                                       28
064100060525     c                   clear                   v1cmsg
064200060525     c                   goto      inz02
064300060525     c                   endif
064400060525
064500060525     c                   setoff                                       28
064600060526     c                   setoff                                       48
064700060525      * Controlli
064800060525     c  n05              exsr      sr_ctrsfl
064900060525     c   28
065000060525     cornkf              goto      emisfl
065100060525
065200060525      * F6 - Conferma
065300060525     c   kf              exsr      sr_confsfl
065400060525
065500060525     c                   endsr
065600060525
065700060525      *---------------------------------------------------------------*
065800060525      * pulisco il subfile
065900060525      *---------------------------------------------------------------*
066000060525     c     sr_pulsfl     begsr
066100060525
066200060525     c                   clear                   nrr
066300060525     c                   clear                   savrec
066400060525     c                   eval      *in20 = *off
066500060525     c                   eval      *in21 = *off
066600060526     c                   write     c810c1
066700060525     c                   eval      *in20 = *on
066800060525     c                   eval      *in21 = *on
066900060525
067000060525     c                   endsr
067100060525
067200060525      *---------------------------------------------------------------*
067300060525      * carico il subfile
067400060525      *---------------------------------------------------------------*
067500060525     c     sr_carsfl     begsr
067600060525
067700060525     c                   setoff                                       30
067800060525      * carico un record per ogni lingua inserite in AZLIN
067900060525     c                   move      '1'           kazlin
068000060525     c     kazlin        setgt     azlin01l
068100060525     c                   do        *hival
068200060525     c                   read      azlin01l
068300060525      * fine file
068400060525     c                   if        %eof(azlin01l)
068500060525     c                   leave
068600060525     c                   endif
068700060525     c                   eval      s1dlin = lindesita
068800060526     c                   clear                   dsfa
068900060525     c                   move      lintabel      kkut
069000060526     c                   eval      kcod = 'FA'
069100060526     c                   clear                   kkey
069200060526     c                   eval      %subst(kkey:1:3) = v1ccod
069300060526     c                   eval      %subst(kkey:4:4) = '1'
069400060525     c     ktab          chain(n)  tabel00f
069500060525     c                   if        not %found(tabel00f)
069600060525     c                   clear                   s1cdes
069700060525     c                   else
069800060526     c                   eval      dsfa = tbluni
069900060526     c                   eval      s1cdes = §fades
070000060525     c                   endif
070100060525     c                   eval      s1hcdlin = kkut
070200060525     c                   add       1             nrr
070300060526     c                   write     c810s1
070400060525     c                   enddo
070500060525
070600060525     c                   seton                                        30
070700060525
070800060525     c                   endsr
070900060525
071000060525      *---------------------------------------------------------------*
071100060525      * controllo il subfile
071200060525      *---------------------------------------------------------------*
071300060525     c     sr_ctrsfl     begsr
071400060525
071500060525     c                   clear                   nrr
071600060525     c                   do        *hival
071700060525     c                   eval      nrr = nrr +1
071800060526     c     nrr           chain     c810s1                             32
071900060525     c                   if        *in32
072000060525     c                   leave
072100060525     c                   endif
072200060525      * deve esserci la descrizione
072300060525     c                   if        s1cdes = *blanks
072400060525     c                   eval      v1cmsg = msg(02)
072500060525     c                   eval      *in28 = *on
072600060526     c                   eval      *in48 = *on
072700060525     c                   eval      savrec = nrr
072800060526     c                   update    c810s1
072900060525     c                   leavesr
073000060525     c                   endif
073100060526     c                   update    c810s1
073200060525     c                   enddo
073300060525
073400060525     c                   endsr
073500060525
073600060525      *---------------------------------------------------------------*
073700060525      * confermo i dati del subfile
073800060525      *---------------------------------------------------------------*
073900060525     c     sr_confsfl    begsr
074000060525
074100060525      * recupero i dati della tabella in italiano
074200060525     c                   clear                   savtbluni
074300060525     c                   eval      kkut = 1
074400060526     c                   eval      kcod = 'FA'
074500060526     c                   clear                   kkey
074600060526     c                   eval      %subst(kkey:1:3) = v1ccod
074700060526     c                   eval      %subst(kkey:4:4) = '1'
074800060525     c     ktab          chain(n)  tabel00f
074900060525     c                   if        %found
075000060525     c                   eval      savtbluni = tbluni
075100060525     c                   endif
075200060525
075300060525      * leggo il subfile e aggiorno/scrivo i dati in lingua
075400060525     c                   clear                   nrr
075500060525     c                   do        *hival
075600060525     c                   eval      nrr = nrr +1
075700060526     c     nrr           chain     c810s1                             32
075800060525     c                   if        *in32
075900060525     c                   leave
076000060525     c                   endif
076100060525     c                   eval      kkut = s1hcdlin
076200060525     c     ktab          chain     tabel00f
076300060526     c                   eval      dsfa = savtbluni
076400060526     c                   eval      §fades = s1cdes
076500060526     c                   eval      tbluni = dsfa
076600060525     c                   eval      tblftt = w1ftt
076700060525     c                   clear                   tblftr
076800060525     c                   clear                   tbldtr
076900060525      * scrivo
077000060525     c                   if        not %found(tabel00f)
077100060525     c                   eval      tblkut = kkut
077200060525     c                   eval      tblcod = kcod
077300060525     c                   eval      tblkey = kkey
077400060525     c                   write     tabel
077500060525      * aggiorno
077600060525     c                   else
077700060525     c                   update    tabel
077800060525     c                   endif
077900060525
078000060525     c                   enddo
078100060525
078200060525     c                   endsr
078300060525
078400060525      *---------------------------------------------------------------*
078500060525      * Reperimento Dati del job (Utente/Operativi)                   *
078600060525      *---------------------------------------------------------------*
078700060525     c     datijob       begsr
078800060525      *
078900060525     c     *dtaara       define    §azute        azuteds
079000060525     c     *dtaara       define    §datiute      ddatiute
079100060525      *
079200060525     c                   in(e)     *dtaara
079300060525     c                   if        %error or rsut = *blanks
079400060525     c                   clear                   tibs34ds
079500060525     c                   call      'TIBS34R'
079600060525     c                   parm                    tibs34ds
079700060525     c                   in        *dtaara
079800060525     c                   endif
079900060526
080000060526      * controllo se sono in ambiente di sede
080100060526     c                   eval      *in12 = (simfel = *zeros)
080200060525
080300060525     c                   endsr
080400060525
080500060525** -- MSG -------------------------------------------------------------------*
080600060525Immettere il codice                                                            1
080700060525Immettere la descrizione                                                       2
080800060526Modalità di pagamento errata                                                   3
080900060526Immettere i giorni                                                             4
081000060526Immettere tra 0 e 31 o 99 = fine mese                                          5
081100060526Immettere Scadenze rate                                                        6
081200060526Immettere almeno 2 rate                                                        7
081300060526Esistono delle rate a 0                                                        8
