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
027400060525     c                   clear                   vidmdp
027500060525     c                   clear                   decmdp
027600060525     c                   clear                   eff
027700060525     c                   clear                   iva
027800060525     c                   clear                   sol
027900060525     c                   clear                   gg1
028000060525     c                   clear                   gg2
028100060525     c                   clear                   sconto
028200060525     c                   clear                   dsrat
028300060525     c                   clear                   nrat
028400060525
028500060525     c                   endsr
028600060525
028700060525      *---------------------------------------------------------------*
028800060525      * IMPOSTO I DATI A VIDEO
028900060525      *---------------------------------------------------------------*
029000060525     c     impvid        begsr
029100060525
029200060525     c                   eval      v1cdes = §fades
029300060526     c                   move      §fatpg        tipo
029400060525     c                   eval      vidmdp = §famdp
029500060525     c                   if        §fatra = '1'
029600060525     c                   eval      eff = 'SI'
029700060525     c                   else
029800060525     c                   eval      eff = 'NO'
029900060525     c                   endif
030000060525     c                   movel     §fatti        iva
030100060525     c                   eval      sol = §fasol
030200060525     c                   eval      gg1 = §faggi
030300060525     c                   eval      gg2 = §faggf
030400060525     c                   eval      sconto = §fasco
030500060525     c                   eval      dsrat = §farat
030600060525     c                   eval      nrat = §fantr
030700060525
030800060525     c                   endsr
030900060525
031000060525      *---------------------------------------------------------------*
031100060525      * RICERCA DESCRIZIONI
031200060525      *---------------------------------------------------------------*
031300060525     c     ricdes        begsr
031400060525
031500060525      * modalità pagamento
031600060526      * la tabella è solo di sede quindi decodifico solo se sono in ambiente di sede
031700060526      * ho fatto questo controllo perchè prima il vecchio programma dava la possibilità
031800060526      * di manutenzionare la tabella anche in filiale
031900060526      * ora non lo fa ma non capisco il perchè visto che richiamo sempre il trul12r
032000060526      * come faceva prima
032100060526     c                   if        vidmdp <> *blanks and *in12
032200060526     c                   clear                   decmdp
032300060525     c                   eval      kkut = 1
032400060525     c                   eval      kcod = '18'
032500060525     c                   eval      kkey = *all'0'
032600060526     c                   if        tipo = 1 or tipo = 2
032700060526     c                   eval      %subst(kkey:7:1) = vidmdp
032800060526     c                   eval      %subst(kkey:8:1) = *blanks
032900060525     c                   else
033000060526     c                   eval      %subst(kkey:7:1) = *blanks
033100060526     c                   eval      %subst(kkey:8:1) = vidmdp
033200060525     c                   endif
033300060525     c     ktab          chain(n)  tabel00f
033400060525     c                   if        %found(tabel00f)
033500060525     c                   eval      decmdp = tbluni
033600060525     c                   endif
033700060525     c                   endif
033800060525
033900060525     c                   endsr
034000060525
034100060525      *---------------------------------------------------------------*
034200060525      * CONTROLLI FORMATO
034300060525      *---------------------------------------------------------------*
034400060525     c     ctrfmt        begsr
034500060526
034600060526     c                   setoff                                       4142
034700060526     c                   setoff                                       4344
034800060526     c                   setoff                                       454647
034900060525
035000060526      * descrizione
035100060525     c                   if        v1cdes = *blanks
035200060525     c                   eval      *in28 = *on
035300060525     c                   eval      *in41 = *on
035400060525     c                   eval      v1cmsg = msg(02)
035500060525     c                   leavesr
035600060525     c                   endif
035700060526
035800060526      * modalità di pagamento
035900060526      * controllo solo se sono in sede, se sono in filiale il campo è protetto
036000060526      * dato che la tabella relativa alla modalità di pagamento è solo di sede
036100060526      * ho fatto questo controllo perchè prima il vecchio programma dava la possibilità
036200060526      * di manutenzionare la tabella anche in filiale
036300060526      * ora non lo fa ma non capisco il perchè visto che richiamo sempre il trul12r
036400060526      * come faceva prima
036500060526     c                   if        vidmdp <> *blanks and *in12
036600060526      * ricerca modalità di pagamento
036700060526     c                   if        vidmdp = '?'
036800060526     c                   call      'CNC018R1'
036900060526     c                   parm                    mdptip
037000060526     c                   parm                    mdpcod
037100060526     c                   parm                    rsut
037200060526     c                   parm                    knsif
037300060526     c                   parm                    w0020
037400060526     c                   parm                    w0020
037500060526     c                   eval      vidmdp = mdpcod
037600060526     c                   eval      *in42 = *on
037700060526     c                   goto      inz02
037800060526     c                   endif
037900060526      * controllo
038000060526     c                   clear                   decmdp
038100060526     c                   eval      kkut = 1
038200060526     c                   eval      kcod = '18'
038300060526     c                   eval      kkey = *all'0'
038400060526     c                   if        tipo = 1 or tipo = 2
038500060526     c                   eval      %subst(kkey:7:1) = vidmdp
038600060526     c                   eval      %subst(kkey:8:1) = *blanks
038700060526     c                   else
038800060526     c                   eval      %subst(kkey:7:1) = *blanks
038900060526     c                   eval      %subst(kkey:8:1) = vidmdp
039000060526     c                   endif
039100060526     c     ktab          chain(n)  tabel00f
039200060526     c                   if        not %found(tabel00f) or tblflg <> *blanks
039300060526     c                   eval      *in28 = *on
039400060526     c                   eval      *in42 = *on
039500060526     c                   eval      v1cmsg = msg(03)
039600060526     c                   leavesr
039700060526     c                   endif
039800060526     c                   eval      decmdp = tbluni
039900060526     c                   endif
040000060526
040100060526      * se gli effetti sono da stampare controllo i campi
040200060526      * gg inizio - sconto - rate
040300060526     c                   if        eff = 'SI'
040400060526      * gg (99 indica fine mese)
040500060526      * se ho immesso gg2 devo immettere anche gg1
040600060526     c                   if        gg2 <> *zeros and gg2 <> 90 and gg1 = *zeros
040700060526     c                   eval      *in28 = *on
040800060526     c                   eval      *in43 = *on
040900060526     c                   eval      v1cmsg = msg(04)
041000060526     c                   leavesr
041100060526     c                   endif
041200060526      * gg1 può essere solo 99 o tra 1 e 31
041300060526     c                   if        gg1 <> 99 and gg1 > 31
041400060526     c                   eval      *in28 = *on
041500060526     c                   eval      *in43 = *on
041600060526     c                   eval      v1cmsg = msg(05)
041700060526     c                   leavesr
041800060526     c                   endif
041900060526      * gg2 può essere solo 99/95/90 o tra 1 e 31
042000060526     c                   if        gg2 <> 99 and gg2 <> 95 and gg2 <> 90 and
042100060526     c                             gg2 > 31
042200060526     c                   eval      *in28 = *on
042300060526     c                   eval      *in44 = *on
042400060526     c                   eval      v1cmsg = msg(05)
042500060526     c                   leavesr
042600060526     c                   endif
042700060526      * rate
042800060526      * deve esserci la prima rata
042900060526     c                   if        rt1 = *zeros
043000060526     c                   eval      *in28 = *on
043100060526     c                   eval      *in45 = *on
043200060526     c                   eval      v1cmsg = msg(06)
043300060526     c                   leavesr
043400060526     c                   endif
043500060526      * se trattamento IVA = 2 deve esserci anche la seconda rata
043600060526     c                   if        iva = 2 and rt2 = *zeros
043700060526     c                   eval      *in28 = *on
043800060526     c                   eval      *in46 = *on
043900060526     c                   eval      v1cmsg = msg(07)
044000060526     c                   leavesr
044100060526     c                   endif
044200060526      * non ci possono essere buchi nelle rate
044300060526     c                   eval      xx = 1
044400060526     c                   for       xx by 1 to 10
044500060526     c                   if        rat(xx) = *zeros
044600060526     c                   leave
044700060526     c                   endif
044800060526     c                   endfor
044900060526     c                   eval      yy = xx
045000060526     c                   for       yy by 1 to 10
045100060526     c                   if        rat(yy) <> *zeros
045200060526     c                   eval      *in28 = *on
045300060526     c                   eval      *in47 = *on
045400060526     c                   eval      v1cmsg = msg(08)
045500060526     c                   leavesr
045600060526     c                   endif
045700060526     c                   endfor
045800060526      * imposto il nr. delle rate
045900060526     c                   eval      nrat = xx - 1
046000060526     c                   endif
046100060526      * se gli effetti non sono da stampare azzeri i campi
046200060526      * gg inizio - sconto - rate
046300060526     c                   if        eff = 'NO'
046400060526      * azzero le rate
046500060526     c                   eval      xx = 1
046600060526     c                   for       xx by 1 to 10
046700060526     c                   eval      rat(xx) = *zeros
046800060526     c                   endfor
046900060526      * azzero il nr. delle rate
047000060526     c                   clear                   nrat
047100060526      * azzero IVA
047200060526     c                   clear                   iva
047300060526      * azzero i gg
047400060526     c                   clear                   gg1
047500060526     c                   clear                   gg2
047600060526      * azzero lo sconto
047700060526     c                   clear                   sconto
047800060526     c                   endif
047900060525
048000060525     c                   endsr
048100060525
048200060525      *----------------------------------------------------------
048300060525      *    Videata conferma + dati trasmissione
048400060525      *----------------------------------------------------------
048500060525     c     conferma      begsr
048600060525
048700060525     c                   eval      kkut = 1
048800060526     c                   eval      kcod = 'FA'
048900060526     c                   clear                   kkey
049000060526     c                   eval      %subst(kkey:1:3) = v1ccod
049100060526     c                   eval      %subst(kkey:4:4) = '1'
049200060525     c     ktab          chain(n)  tabel00f
049300060525     c                   if        %found
049400060525     c                   eval      w1ftt = tblftt
049500060525     c                   eval      w1flt = tblflt
049600060525     c                   eval      w1ftr = tblftr
049700060525
049800060526     c                   move      tbldtr        datar
049900060525     c                   eval      aad = aar
050000060525     c                   eval      mmd = mmr
050100060525     c                   eval      ggd = ggr
050200060526     c                   move      datad         w1dtr
050300060525     c                   else
050400060525     c                   clear                   w1ftt
050500060525     c                   clear                   w1flt
050600060525     c                   clear                   w1ftr
050700060525     c                   clear                   w1dtr
050800060525     c                   endif
050900060525
051000060525     c                   do        *hival
051100060526     c                   exfmt     c810w1
051200060525      * F12 - Ritorno
051300060525     c                   if        *inkl
051400060525     c                   leave
051500060525     c                   endif
051600060525
051700060525      * F6 - Conferma
051800060525     c                   if        *inkf
051900060525     c                   exsr      scrivi
052000060525     c                   eval      wf6 = *on
052100060525     c                   leave
052200060525     c                   endif
052300060525
052400060525     c                   enddo
052500060525
052600060525     c                   endsr
052700060525
052800060525      *---------------------------------------------------------------*
052900060525      * AGGIORNAMENTO / INSERIMENTO
053000060525      *---------------------------------------------------------------*
053100060525     c     scrivi        begsr
053200060525
053300060526     c                   eval      §fades = v1cdes
053400060526     c                   movel     tipo          §fatpg
053500060526     c                   if        eff = 'SI'
053600060526     c                   eval      §fatra = '1'
053700060526     c                   else
053800060526     c                   eval      §fatra = '0'
053900060526     c                   endif
054000060526     c                   movel     iva           §fatti
054100060526     c                   eval      §faggi = gg1
054200060526     c                   eval      §faggf = gg2
054300060526     c                   movel     dsrat         §farat
054400060526     c                   eval      §fasco = sconto
054500060526     c                   eval      §fantr = nrat
054600060526     c                   eval      §famdp = vidmdp
054700060525
054800060525     c                   eval      kkut = 1
054900060526     c                   eval      kcod = 'FA'
055000060526     c                   clear                   kkey
055100060526     c                   eval      %subst(kkey:1:3) = v1ccod
055200060526     c                   eval      %subst(kkey:4:4) = '1'
055300060525     c     ktab          chain     tabel00f
055400060525
055500060526     c                   eval      tbluni = dsfa
055600060525
055700060525     c                   if        wf16 = *on
055800060525     c                   eval      tblflg = '*'
055900060525     c                   endif
056000060525
056100060525     c                   if        wf5 = *on
056200060525     c                   clear                   tblflg
056300060525     c                   endif
056400060525
056500060525     c                   eval      tblftt = w1ftt
056600060525     c                   clear                   tblftr
056700060525     c                   clear                   tbldtr
056800060525
056900060525      * scrivo
057000060525     c                   if        not %found(tabel00f)
057100060525     c                   eval      tblkut = kkut
057200060525     c                   eval      tblcod = kcod
057300060525     c                   eval      tblkey = kkey
057400060525     c                   write     tabel
057500060525
057600060525      * aggiorno
057700060525     c                   else
057800060525     c                   update    tabel
057900060525     c                   endif
058000060525
058100060525      * se annullamento o ripristino devo fare la stessa cosa anche
058200060525      * sulle tabelle in lingua
058300060525     c                   if        wf16 = *on or wf5 = *on
058400060525      * aggancio le lingue presenti su AZLIN
058500060525     c                   move      kkut          kazlin
058600060525     c     kazlin        setgt     azlin01l
058700060525     c                   do        *hival
058800060525     c                   read      azlin01l
058900060525     c                   if        %eof(azlin01l)
059000060525     c                   leave
059100060525     c                   endif
059200060525      * per ogni lingua aggiorno
059300060525     c                   move      lintabel      kkut
059400060526     c                   eval      kcod = 'FA'
059500060526     c                   clear                   kkey
059600060526     c                   eval      %subst(kkey:1:3) = v1ccod
059700060526     c                   eval      %subst(kkey:4:4) = '1'
059800060525     c     ktab          chain     tabel00f
059900060525     c                   if        %found(tabel00f)
060000060525     c                   if        wf16 = *on
060100060525     c                   eval      tblflg = '*'
060200060525     c                   endif
060300060525     c                   if        wf5 = *on
060400060525     c                   clear                   tblflg
060500060525     c                   endif
060600060525     c                   eval      tblftt = w1ftt
060700060525     c                   clear                   tblftr
060800060525     c                   clear                   tbldtr
060900060525     c                   update    tabel
061000060525     c                   endif
061100060525     c                   enddo
061200060525     c                   endif
061300060525
061400060525     c                   endsr
061500060525
061600060525      *---------------------------------------------------------------*
061700060525      * gestione tabella in lingua
061800060525      *---------------------------------------------------------------*
061900060525     c     sr_lingua     begsr
062000060525
062100060525      * pulisco il subfile
062200060525     c                   exsr      sr_pulsfl
062300060525      * carico il subfile con i dati che trovo o vuoto
062400060525     c                   exsr      sr_carsfl
062500060525      * emetto il subfile
062600060525     c     emisfl        tag
062700060525     c                   eval      rec = 1
062800060525     c                   if        savrec > 0
062900060525     c                   eval      rec = savrec
063000060525     c                   endif
063100060526     c                   write     c810t1
063200060526     c                   write     c810z1
063300060526     c                   exfmt     c810c1
063400060525
063500060525      * F12 - Ritorno
063600060525     c                   if        *inkl
063700060525     c                   setoff                                       28
063800060525     c                   clear                   v1cmsg
063900060525     c                   goto      inz02
064000060525     c                   endif
064100060525
064200060525     c                   setoff                                       28
064300060526     c                   setoff                                       48
064400060525      * Controlli
064500060525     c  n05              exsr      sr_ctrsfl
064600060525     c   28
064700060525     cornkf              goto      emisfl
064800060525
064900060525      * F6 - Conferma
065000060525     c   kf              exsr      sr_confsfl
065100060525
065200060525     c                   endsr
065300060525
065400060525      *---------------------------------------------------------------*
065500060525      * pulisco il subfile
065600060525      *---------------------------------------------------------------*
065700060525     c     sr_pulsfl     begsr
065800060525
065900060525     c                   clear                   nrr
066000060525     c                   clear                   savrec
066100060525     c                   eval      *in20 = *off
066200060525     c                   eval      *in21 = *off
066300060526     c                   write     c810c1
066400060525     c                   eval      *in20 = *on
066500060525     c                   eval      *in21 = *on
066600060525
066700060525     c                   endsr
066800060525
066900060525      *---------------------------------------------------------------*
067000060525      * carico il subfile
067100060525      *---------------------------------------------------------------*
067200060525     c     sr_carsfl     begsr
067300060525
067400060525     c                   setoff                                       30
067500060525      * carico un record per ogni lingua inserite in AZLIN
067600060525     c                   move      '1'           kazlin
067700060525     c     kazlin        setgt     azlin01l
067800060525     c                   do        *hival
067900060525     c                   read      azlin01l
068000060525      * fine file
068100060525     c                   if        %eof(azlin01l)
068200060525     c                   leave
068300060525     c                   endif
068400060525     c                   eval      s1dlin = lindesita
068500060526     c                   clear                   dsfa
068600060525     c                   move      lintabel      kkut
068700060526     c                   eval      kcod = 'FA'
068800060526     c                   clear                   kkey
068900060526     c                   eval      %subst(kkey:1:3) = v1ccod
069000060526     c                   eval      %subst(kkey:4:4) = '1'
069100060525     c     ktab          chain(n)  tabel00f
069200060525     c                   if        not %found(tabel00f)
069300060525     c                   clear                   s1cdes
069400060525     c                   else
069500060526     c                   eval      dsfa = tbluni
069600060526     c                   eval      s1cdes = §fades
069700060525     c                   endif
069800060525     c                   eval      s1hcdlin = kkut
069900060525     c                   add       1             nrr
070000060526     c                   write     c810s1
070100060525     c                   enddo
070200060525
070300060525     c                   seton                                        30
070400060525
070500060525     c                   endsr
070600060525
070700060525      *---------------------------------------------------------------*
070800060525      * controllo il subfile
070900060525      *---------------------------------------------------------------*
071000060525     c     sr_ctrsfl     begsr
071100060525
071200060525     c                   clear                   nrr
071300060525     c                   do        *hival
071400060525     c                   eval      nrr = nrr +1
071500060526     c     nrr           chain     c810s1                             32
071600060525     c                   if        *in32
071700060525     c                   leave
071800060525     c                   endif
071900060525      * deve esserci la descrizione
072000060525     c                   if        s1cdes = *blanks
072100060525     c                   eval      v1cmsg = msg(02)
072200060525     c                   eval      *in28 = *on
072300060526     c                   eval      *in48 = *on
072400060525     c                   eval      savrec = nrr
072500060526     c                   update    c810s1
072600060525     c                   leavesr
072700060525     c                   endif
072800060526     c                   update    c810s1
072900060525     c                   enddo
073000060525
073100060525     c                   endsr
073200060525
073300060525      *---------------------------------------------------------------*
073400060525      * confermo i dati del subfile
073500060525      *---------------------------------------------------------------*
073600060525     c     sr_confsfl    begsr
073700060525
073800060525      * recupero i dati della tabella in italiano
073900060525     c                   clear                   savtbluni
074000060525     c                   eval      kkut = 1
074100060526     c                   eval      kcod = 'FA'
074200060526     c                   clear                   kkey
074300060526     c                   eval      %subst(kkey:1:3) = v1ccod
074400060526     c                   eval      %subst(kkey:4:4) = '1'
074500060525     c     ktab          chain(n)  tabel00f
074600060525     c                   if        %found
074700060525     c                   eval      savtbluni = tbluni
074800060525     c                   endif
074900060525
075000060525      * leggo il subfile e aggiorno/scrivo i dati in lingua
075100060525     c                   clear                   nrr
075200060525     c                   do        *hival
075300060525     c                   eval      nrr = nrr +1
075400060526     c     nrr           chain     c810s1                             32
075500060525     c                   if        *in32
075600060525     c                   leave
075700060525     c                   endif
075800060525     c                   eval      kkut = s1hcdlin
075900060525     c     ktab          chain     tabel00f
076000060526     c                   eval      dsfa = savtbluni
076100060526     c                   eval      §fades = s1cdes
076200060526     c                   eval      tbluni = dsfa
076300060525     c                   eval      tblftt = w1ftt
076400060525     c                   clear                   tblftr
076500060525     c                   clear                   tbldtr
076600060525      * scrivo
076700060525     c                   if        not %found(tabel00f)
076800060525     c                   eval      tblkut = kkut
076900060525     c                   eval      tblcod = kcod
077000060525     c                   eval      tblkey = kkey
077100060525     c                   write     tabel
077200060525      * aggiorno
077300060525     c                   else
077400060525     c                   update    tabel
077500060525     c                   endif
077600060525
077700060525     c                   enddo
077800060525
077900060525     c                   endsr
078000060525
078100060525      *---------------------------------------------------------------*
078200060525      * Reperimento Dati del job (Utente/Operativi)                   *
078300060525      *---------------------------------------------------------------*
078400060525     c     datijob       begsr
078500060525      *
078600060525     c     *dtaara       define    §azute        azuteds
078700060525     c     *dtaara       define    §datiute      ddatiute
078800060525      *
078900060525     c                   in(e)     *dtaara
079000060525     c                   if        %error or rsut = *blanks
079100060525     c                   clear                   tibs34ds
079200060525     c                   call      'TIBS34R'
079300060525     c                   parm                    tibs34ds
079400060525     c                   in        *dtaara
079500060525     c                   endif
079600060526
079700060526      * controllo se sono in ambiente di sede
079800060526     c                   eval      *in12 = (simfel = *zeros)
079900060525
080000060525     c                   endsr
080100060525
080200060525** -- MSG -------------------------------------------------------------------*
080300060525Immettere il codice                                                            1
080400060525Immettere la descrizione                                                       2
080500060526Modalità di pagamento errata                                                   3
080600060526Immettere i giorni                                                             4
080700060526Immettere tra 0 e 31 o 99 = fine mese                                          5
080800060526Immettere Scadenze rate                                                        6
080900060526Immettere almeno 2 rate                                                        7
081000060526Esistono delle rate a 0                                                        8
