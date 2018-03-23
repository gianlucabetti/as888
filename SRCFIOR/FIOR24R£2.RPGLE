000100070214     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200070208
000300070208      *------------------------------------------------------------------------*
000400070208      *
000500070208      *                 STAMPA PROSPETTO ORM               ?
000600070208      *
000700070208      *------------------------------------------------------------------------*
000800070208     ffiplg01l  if   e           k disk
000900070208     ffiapd01l  if   e           k disk
001000070208     ffnorm01l  if   e           k disk
001100070208     ffior24p   o    e             printer oflind(*in90)
001200070212
001300070212      *------------------------------------------------------------------------*
001400070212      *  RIEPILOGO INDICATORI
001500070212      *------------------------------------------------------------------------*
001600070212      * 01 - distinta forzata nella quadratura
001700070214      * 02 - ORM manuale
001800070212      * 30 - lookup
001900070212      * 90 - salto pagina
002000070212      * 99 - comodo
002100070212      *------------------------------------------------------------------------*
002200070208
002300070212      *   V A R I A B I L I
002400070208     d kapdtip         s                   like(apdtip) inz('A')
002500070208     d ktiporcd        s                   like(plgtiporcd) inz('RES')
002600071214     d savdst          s                   like(plgndc)
002700070213     d wdatora         s             12
002800070213     d wdtorin         s             14  0
002900070213     d worm            s             14  0
003000070212     d xx              s              4  0
003100070213     d yy              s              4  0
003200070212
003300070212      *   S C H I E R E
003400070212     d skorm           s             14  0 dim(100)
003500070212     d skdatora        s             14  0 dim(100)
003600070213     d skdtorin        s             14  0 dim(100)
003700070213     d skcomodo        s             28    dim(100)
003800070208
003900070212      *   D S   I N T E R N E / E S T E R N E
004000070212     d dettaglio       ds                  occurs(100)
004100070212     d  pman                   1      1
004200070212     d  prsr                   2     36
004300070212     d  pinr                  37     71
004400070212     d  pncl                  72     76  0
004500070212     d  pbnc                  77     81  0
004600070213     d  pflgspc               82     82
004700070212
004800070212     d                 ds
004900070212     d  dsorm                  1     14  0
005000070213     d  wormpoe                1      3  0
005100070213     d  wormnsr                4      5  0
005200070213     d  wormnor                6     12  0
005300070213     d  wormnrv               13     14  0
005400070212
005500070208     d wlbdat          ds                  inz
005600070208     d  g02dat                 1      8  0
005700070208     d  g02inv                 9     16  0
005800070208     d  g02err                17     17
005900070208     d  g02tgi                18     22  0
006000070208
006100070208     d azuteds       e ds                  extname(azute00f)
006200070208     d ddatiute      e ds
006300070208     d fior24ds      e ds
006400070213     d fiprcresds    e ds
006500070208     d kpjba         e ds
006600070208     d tibs34ds      e ds
006700070208
006800070208      *------------------------------------------------------------------------*
006900070208
007000070208     c     kfiplg        setll     fiplg01l
007100070213     c                   do        *hival
007200071214     c     kfiplg1       reade     fiplg01l
007300070208
007400070208     c                   if        %eof(fiplg01l)
007500070208     c                   leave
007600070208     c                   endif
007700071214
007800071214     c                   if        plgndc > i24ndcal
007900071214     c                   leave
008000071214     c                   endif
008100071214
008200071214      * stampo a rottura distinta
008300071214     c                   if        plgndc <> savndc
008400071214     c                   if        savndc > 0
008500071214     c                   exsr      sr_stampa
008600071214     c                   endif
008700071214     c                   exsr      sr_pulisci
008800071214     c                   eval      savndc = plgndc
008900071214     c                   endif
009000070208
009100070208     c                   eval      fiprcresds = plgdati
009200070208
009300070208      * escludo i non fatti
009400070208     c                   if        §rescmr <> *blanks
009500070208     c                   iter
009600070208     c                   endif
009700070212
009800070212      * imposto i campi numerici dell'ORM
009900070213     c                   move      §respoe       wormpoe
010000070213     c                   move      §resnsr       wormnsr
010100070213     c                   move      §resnor       wormnor
010200070213     c                   move      §resnrv       wormnrv
010300070212
010400070212      * memorizzo l'ORM in schiera con anche la data/ora
010500070213      * ma tengo l'ORM con data/ora inserimento più alto
010600070212     c                   eval      xx = 1
010700070212     c     dsorm         lookup    skorm(xx)                              30
010800070213     c                   if        not *in30
010900070212     c                   eval      xx = 1
011000070213     c     *zeros        lookup    skorm(xx)                              30
011100070212     c                   eval      skorm(xx) = dsorm
011200070213     c                   move      §resdtorin    skdtorin(xx)
011300070213     c                   move      §resdatora    skdatora(xx)
011400070213     c                   exsr      sr_cardet
011500070212     c                   else
011600070213     c                   move      §resdtorin    wdtorin
011700070213     c                   if        wdtorin > skdtorin(xx)
011800070213     c                   move      §resdtorin    skdtorin(xx)
011900070213     c                   move      §resdatora    skdatora(xx)
012000070213     c                   exsr      sr_cardet
012100070213     c                   endif
012200070213     c                   endif
012300070213
012400070212     c                   enddo
012500071214
012600071214      * stampo ultima distinta
012700071214     c                   exsr      sr_stampa
012800071214
012900071214      * stampo fine
013000071214     c                   write     or24e1
013100071214
013200071214     c                   eval      *inlr = *on
013300071214
013400071214      *------------------------------------------------------------------------*
013500071214      * STAMPA
013600071214      *------------------------------------------------------------------------*
013700071214     c     sr_stampa     begsr
013800071214
013900071214      * data distinta
014000071214     c                   eval      g02inv = i24data
014100071214     c                   eval      g02err = '3'
014200071214     c                   call      'XSRDA8'
014300071214     c                   parm                    wlbdat
014400071214     c                   eval      pddc = g02dat
014500071214
014600071214      * descrizione AUT
014700071214     c     kfiapd        chain     fiapd01l
014800070213
014900070213      * carico una schiera di comodo con data/ora ritiro + key orm
015000070213     c                   eval      xx = 1
015100070213     c                   for       xx by 1 to 100
015200070213     c                   move      skorm(xx)     skcomodo(xx)
015300070213     c                   movel     skdatora(xx)  skcomodo(xx)
015400070213     c                   endfor
015500070213      * devo sortare la sk di comodo xchè la stampa è in ordine di data/ora ritiro dell'ORM
015600070213     c                   sorta     skcomodo
015700070208
015800070212      * S T A M P A
015900070213     c                   eval      xx = 1
016000070213      * leggo gli orm in ordine di data/ora ritiro
016100070213     c                   for       xx by 1 to 100
016200070214     c                   if        skcomodo(xx) <> *zeros
016300070213     c                   move      skcomodo(xx)  worm
016400070213      * cerco il relativo ORM
016500070213     c                   eval      yy = 1
016600070213     c     worm          lookup    skorm(yy)                              30
016700070213     c                   if        *in30
016800070213      * se quadratura forzata cerco il rcd RCH di forzatura
016900070213     c                   if        *in01 and not *in99
017000070213     c                   clear                   plgprfc
017100070213     c                   clear                   pfdata
017200070213     c                   clear                   pfora
017300070213     c                   eval      ktiporcd = 'RCH'
017400070213     c     kfiplg        chain     fiplg01l
017500070213     c                   if        %found (fiplg01l)
017600070213     c                   movel     plgdatora     wdatora
017700070213      * inverto la data
017800070213     c                   clear                   wlbdat
017900070213     c                   movel     wdatora       g02inv
018000070213     c                   eval      g02err = '3'
018100070213     c                   call      'XSRDA8'
018200070213     c                   parm                    wlbdat
018300070213     c                   eval      pfdata = g02dat
018400070213     c                   move      wdatora       pfora
018500070213     c                   endif
018600070213     c                   endif
018700070208      * stampo testata
018800070208     c                   if        not *in99
018900070208     c                   write     or24t1
019000070208     c                   write     or24t2
019100070208     c                   eval      *in99 = *on
019200070208     c                   endif
019300070213      * imposto il n. orm per la stampa
019400070213     c                   eval      dsorm = worm
019500070213     c                   eval      ppoe = wormpoe
019600070213     c                   eval      pnsr = wormnsr
019700070213     c                   eval      pnor = wormnor
019800070213     c                   eval      pnrv = wormnrv
019900070213      * recupero i dati di dettaglio
020000070213     c     yy            occur     dettaglio
020100070214      * controllo se ORM manuale
020200070214     c                   eval      *in02 = (pman = 'S')
020300070213      * imposto la data/ora ritiro per la stampa
020400070213     c                   movel     skdatora(yy)  wdatora
020500070213      * inverto la data
020600070213     c                   clear                   wlbdat
020700070213     c                   movel     wdatora       g02inv
020800070213     c                   eval      g02err = '3'
020900070213     c                   call      'XSRDA8'
021000070213     c                   parm                    wlbdat
021100070213     c                   eval      pdata = g02dat
021200070213     c                   move      wdatora       pora
021300070213
021400070208      * sommo colli/bancali
021500070208     c                   add       pncl          tncl
021600070208     c                   add       pbnc          tbnc
021700070208
021800070208      * stampo dettaglio
021900070208     c   90              write     or24t2
022000070208     c                   write     or24d1
022100070213
022200070213     c                   endif
022300070213     c                   endif
022400070213     c                   endfor
022500070208
022600070208      * stampo totale  colli/bancali
022700070208     c                   write     or24g1
022800071214
022900071214     c                   endsr
023000071214
023100071214      *------------------------------------------------------------------------*
023200071214      * PULISCO I DATI DI WORK
023300071214      *------------------------------------------------------------------------*
023400071214     c     sr_pulisci    begsr
023500071214
023600071214      * pulisco la ds multipla e le schiere
023700071214     c                   eval      xx = 1
023800071214     c                   for       xx by 1 to 100
023900071214     c     xx            occur     dettaglio
024000071214     c                   clear                   dettaglio
024100071214     c                   clear                   skorm(xx)
024200071214     c                   clear                   skdatora(xx)
024300071214     c                   clear                   skdtorin(xx)
024400071214     c                   clear                   skcomodo(xx)
024500071214     c                   endfor
024600071214
024700071214     c                   endsr
024800070213
024900070213      *------------------------------------------------------------------------*
025000070213      * CARICO I DATI DI DETTAGLIO PER LA STAMPA
025100070213      *------------------------------------------------------------------------*
025200070213     c     sr_cardet     begsr
025300070213
025400070213     c     xx            occur     dettaglio
025500070213
025600070213      * ORM manuale
025700070213     c                   if         %subst(plgdati:10:8) = *all'0'
025800070213     c                   eval      pman = 'S'
025900070213     c                   eval      prsr = §resrsr
026000070213     c                   eval      pinr = §resinr
026100070213      * ORM inserito da AS
026200070213     c                   else
026300070213     c                   clear                   pman
026400070213     c                   eval      ormpoe = wormpoe
026500070213     c                   eval      ormnsr = wormnsr
026600070213     c                   eval      ormnor = wormnor
026700070213     c                   eval      ormnrv = wormnrv
026800070213     c     kfnorm        chain     fnorm01l
026900070213     c                   if        not %found(fnorm01l)
027000070213     c                   clear                   prsr
027100070213     c                   clear                   pinr
027200070213     c                   endif
027300070213     c                   eval      prsr = ormrsr
027400070213     c                   eval      pinr = orminr
027500070213     c                   endif
027600070213
027700070213      * colli/bancali
027800070213     c                   move      §resncl       pncl
027900070213     c                   move      §resbnc       pbnc
028000070213
028100070213      * flag immesso da SPC
028200070213     c                   eval      pflgspc = §resflgspc
028300070213
028400070213     c                   endsr
028500070208
028600070208      *------------------------------------------------------------------------*
028700070208      * ROUTINE INIZIALE
028800070208      *------------------------------------------------------------------------*
028900070208     c     *inzsr        begsr
029000070208
029100070208     c     *entry        plist
029200070208     c                   parm                    kpjba
029300070208
029400070208     c     *dtaara       define    §azute        azuteds
029500070208     c     *dtaara       define    §datiute      ddatiute
029600070208     c                   in(e)     *dtaara
029700070208     c                   if        %error or rsut = *blanks
029800070208     c                   clear                   tibs34ds
029900070208     c                   call      'TIBS34R'
030000070208     c                   parm                    tibs34ds
030100070208     c                   in        *dtaara
030200070208     c                   endif
030300070208
030400070208     c                   eval      fior24ds = kpjbu
030500070212
030600070212      * se forzata la quadratura lo stampo
030700070213     c                   eval      *in01 = (i24forzaq = *on)
030800070214
030900070214     c                   time                    wtime
031000070214     c                   z-add     *date         wdate
031100070208
031200070208     c     kfiapd        klist
031300070208     c                   kfld                    kapdtip
031400071214     c                   kfld                    plgcodaut
031500070208
031600070208     c     kfiplg        klist
031700070208     c                   kfld                    ktiporcd
031800070208     c                   kfld                    i24fgs
031900071214     c                   kfld                    i24ndcda
032000071214
032100071214     c     kfiplg1       klist
032200071214     c                   kfld                    ktiporcd
032300071214     c                   kfld                    i24fgs
032400070208
032500070208     c     kfnorm        klist
032600070208     c                   kfld                    ormpoe
032700070208     c                   kfld                    ormnsr
032800070208     c                   kfld                    ormnor
032900070208     c                   kfld                    ormnrv
033000070208
033100070208     c                   endsr
