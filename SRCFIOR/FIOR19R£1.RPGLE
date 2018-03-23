000100040416     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200040326
000300040326      *------------------------------------------------------------------------*
000400040416      *                                                                        *
000500040416      *      AVANZAMENTO ORM ABBINATI A BOLLE IN SOSPENSIONE                   *
000600040326      *                                                                        *
000700040326      *------------------------------------------------------------------------*
000800040326
000900040416     fFnblp76l  if   e           k disk
001000060214     fFiar401l  if   e           k disk
001100040416     fFnfgv01l  if   e           k disk
001200040416     fFnfgw01l  if   e           k disk
001300040416     fFnorm01l  uf   e           k disk
001400070711     ffnorg01l  uf   e           k disk
001500040416     fFnorf00f  o    e             disk
001600051212     fFnorv00f  o    e             disk
001700060516     ffndst01l  if   e           k disk
001800040326
001900040326      *------------------------------------------------------------------------*
002000040326      *   V A R I A B I L I
002100040326      *------------------------------------------------------------------------*
002200060516     d kdstfgs         s                   like(dstfgs)
002300060516     d kdstnfv         s                   like(dstnfv)
002400040416     d wlna03          s                   Like(BlpLna)
002500040416     d xx              s              2  0
002600040416     d yy              s              3  0
002700040416
002800040326      *------------------------------------------------------------------------*
002900040326      *   D S   I N T E R N E / E S T E R N E
003000040326      *------------------------------------------------------------------------*
003100040416     d                 ds
003200040416     d  kAas                   1      4  0
003300040416     d  kMgs                   5      8  0
003400040416     d  FgvDfv                 1      8  0
003500051212
003600051212     D WLBDAT          DS                  INZ
003700051212     D  G02DAT                 1      8  0
003800051212     D  G02INV                 9     16  0
003900051212     D  G02ERR                17     17
004000051212     D  G02TGI                18     22  0
004100040416
004200040416     d                 ds
004300040416     d  FgvLna                 1      3  0
004400040416     d  FgvFfv                 4    243
004500040416     d  FgvFf2               244    450
004600040416     d  FgwFf3               451    690
004700040416     d  FgwFf4               691    900
004800040416     d  lna                    1    900    dim(300)
004900050428
005000050428     d                 ds
005100050428     d  blpaas                 1      4  0
005200050428     d  blpmgs                 5      8  0
005300050428     d  blpdsp                 1      8  0
005400040416
005500040416     d parm01          ds
005600040416     d  fgs                           3  0
005700040416     d  nfv                           5  0
005800040416
005900070711     d dfar          e ds
006000040416     d dsbl4m        e ds
006100040416
006200040416     d Trul06ds      e ds
006300040416     d  lin                    1     90  0 dim(30)
006400040416
006500040326     d Kpjba         e ds
006600060516     d fnlv55ds      e ds
006700070711     d tibs02ds      e ds
006800040326
006900040326      *------------------------------------------------------------------------*
007000001025
007100040617      * Leggo le bolle partenza  (per linea di partenza - £1)
007200040416do  1c                   Do        30            xx
007300040416      * non ho più linee di partenza da leggere esco
007400040416     c                   If        lin(xx) = *Zeros
007500040416     c                   Leave
007600040416     c                   EndIf
007700040416      * linee di arrivo
007800040416do  2c                   Do        300           yy
007900040416      * non ho più linee di arrivo da leggere esco
008000040617     c                   If        lna(yy) <= *Zeros
008100040416     c                   Leave
008200040416     c                   EndIf
008300040416
008400040416     c                   Move      lna(yy)       wlna03
008500040416
008600040617     c     kBlp          Setgt     Fnblp76l
008700040416do  3c                   Do        *Hival
008800040416     c     kBlp1         Reade     Fnblp76l
008900040330      * Fine file
009000040416     c                   If        %Eof(Fnblp76l)
009100040326     c                   Leave
009200040326     c                   EndIf
009300050429      * la bolla deve avere immissione <= data foglio
009400050428     c                   If        blpdim > fgvdfv
009500050428     c                   Iter
009600050428     c                   EndIf
009700050428      * la bolla deve avere data immissione < data spedizione
009800050428     c                   If        blpdim >= blpdsp
009900050428     c                   Iter
010000050428     c                   EndIf
010100040416      * Non deve essere borderizzata
010200040416     c                   If        BlpDbr > *Zeros
010300040416     c                   Iter
010400040416     c                   EndIf
010500040714      * Se non c'è l'autotrasportatore è una bolla prepagato non la elaboro
010600040714     c                   If        BlpPdr = *Zeros
010700040714     c                   Iter
010800040714     c                   EndIf
010900040416      * Controllo se bolla abbinata ad un ORM
011000060214     c     kAr4          Chain     Fiar401l
011100060214     c                   If        Not %Found(Fiar401l)
011200040416     c                   Iter
011300040416     c                   EndIf
011400040416
011500060214     c                   Eval      dsbl4m = ar4Not
011600040416     c     kOrm          Chain     Fnorm01l
011700040416     c                   If        %Found(Fnorm01l) and OrmFao <> 900
011800051212     c* Se il p.o. di ritiro non è uguale alla linea di partenza,
011900051212     c*  aggiorno in questo momento
012000060214    2c                   if        ormpor<>ar4lnp
012100051212     c                   exsr      sr_Storico
012200051212     c* Scrivo fase 200
012300051212     c                   z-add     200           ormfao
012400051212     C                   Z-ADD     FGVDFV        ORMDFO
012500070110     C                   Z-ADD     235859        ORMOFO
012600051212     c                   exsr      rieorf
012700051212     c                   write     fnorf000
012800051212     c
012900060214     c                   z-add     ar4lnp        ormpor
013000070711      * se ho aggiornato il p.o. ritiro dell'ORM devo aggiornare anche FNORG
013100070711     c     korm          chain     fnorg01l
013200070711if  1c                   if        %found(fnorg01l)
013300070711      * cambio il p.o. ritiro
013400070711     c                   eval      orgpor = ormpor
013500070711     c                   update    fnorg000
013600070711    2c                   endif
013700051212    2c                   endif
013800051212     c
013900040416     c                   ExSr      Sr_AggOrm
014000040416     c                   EndIf
014100040416e   3c                   EndDo
014200040416
014300040416e   2c                   EndDo
014400040415e   1c                   EndDo
014500040326
014600040326     c                   Eval      *InLr = *On
014700040416
014800051212      *------------------------------------------------------------------------*
014900051212     C* Riempio campi record fase O.R.M.
015000051212      *------------------------------------------------------------------------*
015100051212     C     RIEORF        BEGSR
015200051212     C                   clear                   fnorf000
015300051212     C                   eval      ORFpoe = ormpoe
015400051212     C                   eval      ORFnsr = ormnsr
015500051212     C                   eval      ORFnor = ormnor
015600051212     C                   eval      ORFnrv = ormnrv
015700051212     C                   eval      ORFpog = ormpor
015800051212     C                   eval      ORFdae = ormdfo
015900051212     C                   eval      ORFore = ormofo
016000051212     C                   eval      ORFfar = ormfao
016100051212     C                   eval      ORFpue = knmus
016200051212     c                   If        OrmNdc > *Zeros
016300060516      * devo trovare il p.o. gestione distinta giusto
016400060516     c**!!!              Eval      OrfFgs = OrmPor
016500060516     c                   move      ormndc        kdstnfv
016600060516      * prima provo con il p.o. ritiro
016700060516     c                   eval      kdstfgs = ormpor
016800060516     c     kdst          chain     fndst01l
016900060516      * se non lo trovo cerco se il p.o. ritiro è gestito
017000060516     c                   if        not %found(fndst01l)
017100060516     c                   clear                   fnlv55ds
017200060516     c                   eval      d55tpt = '6'
017300060516     c                   move      ormpor        d55lin
017400060516     c                   eval      d55drf = ormddc
017500060516     c                   call      'FNLV55R'
017600060516     c                   parm                    fnlv55ds
017700060516     c                   if        d55err = *blanks and d55tfa <> d55lin
017800060516     c                   move      d55tfa        kdstfgs
017900060516     c     kdst          chain     fndst01l
018000060516      * se non trovo neanche con il p.o. che gestisce metto il p.o. ritiro
018100060516      * teoricamente non dovrebbe succedere
018200060516     c                   if        not %found(fndst01l)
018300060516     c                   eval      kdstfgs = ormpor
018400060516     c                   endif
018500060516     c                   endif
018600060516     c                   endif
018700060516     c                   eval      orffgs = kdstfgs
018800051212     c                   Eval      OrfNdc = OrmNdc
018900051212     c                   Eval      OrfDdc = OrmDdc
019000051212     c                   EndIf
019100051212
019200051212     c                   ENDSR
019300040416      *------------------------------------------------------------------------*
019400040416      * - AGGIORNO L'ORM E SCRIVO LA FASE 900
019500040416      *------------------------------------------------------------------------*
019600040416     c     Sr_AggOrm     BegSr
019700040416
019800040416     c                   Eval      OrmFao = 900
019900040416     c                   Eval      OrmDfo = FgvDfv
020000070110     c                   Eval      OrmOfo = 235958
020100040908     c                   Z-add     *Date         OrmDtt
020200051212     c                   exsr      RIEORF
020300070711      * imposto ormeti
020400070711     c                   clear                   dfar
020500070711     c                   clear                   tibs02ds
020600070711     c                   eval      t02mod = 'C'
020700070711     c                   eval      t02sif = knsif
020800070711     c                   eval      t02cod = 'FAR'
020900070711     c                   movel(p)  ormfao        t02ke1
021000070711     c                   call      'TIBS02R'
021100070711     c                   parm                    kpjba
021200070711     c                   parm                    tibs02ds
021300070711     c                   eval      dfar = t02uni
021400070711     c                   eval      ormeti = d§farass
021500040416
021600040416     c                   Write     Fnorf000
021700040416
021800040416     c                   Update    Fnorm000
021900040416
022000040416     c                   EndSr
022100040326
022200051212      *------------------------------------------------------------------------*
022300051212     C* Scrivo lo storico orm
022400051212      *------------------------------------------------------------------------*
022500051212     C     SR_Storico    BEGSR
022600051212     C                   clear                   fnorv000
022700051212     C                   eval      ORVpoe = ORMpoe
022800051212     C                   eval      ORVnsr = ORMnsr
022900051212     C                   eval      ORVnor = ORMnor
023000051212     C                   eval      ORVnrv = ORMnrv
023100051212     C                   eval      ORVtor = ORMtor
023200051212     C                   eval      ORVcor = ORMcor
023300051212     C                   eval      ORVrso = ORMrso
023400051212     C                   eval      ORVino = ORMino
023500051212     C                   eval      ORVcao = ORMcao
023600051212     C                   eval      ORVloo = ORMloo
023700051212     C                   eval      ORVpro = ORMpro
023800051212     C                   eval      ORVnao = ORMnao
023900051212     C                   eval      ORVcra = ORMcra
024000051212     C                   eval      ORVrsr = ORMrsr
024100051212     C                   eval      ORVinr = ORMinr
024200051212     C                   eval      ORVcar = ORMcar
024300051212     C                   eval      ORVlor = ORMlor
024400051212     C                   eval      ORVprr = ORMprr
024500051212     C                   eval      ORVnar = ORMnar
024600051212     C                   eval      ORVrer = ORMrer
024700051212     C                   eval      ORVter = ORMter
024800051212     C                   eval      ORVcrc = ORMcrc
024900051212     C                   eval      ORVrsc = ORMrsc
025000051212     C                   eval      ORVinc = ORMinc
025100051212     C                   eval      ORVloc = ORMloc
025200051212     C                   eval      ORVcac = ORMcac
025300051212     C                   eval      ORVprc = ORMprc
025400051212     C                   eval      ORVnac = ORMnac
025500051212     C                   eval      ORVffd = ORMffd
025600051212     C                   eval      ORVdar = ORMdar
025700051212     C                   eval      ORVorr = ORMorr
025800051212     C                   eval      ORVnam = ORMnam
025900051212     C                   eval      ORVncl = ORMncl
026000051212     C                   eval      ORVpkg = ORMpkg
026100051212     C                   eval      ORVvlm = ORMvlm
026200051212     C                   eval      ORVbnc = ORMbnc
026300051212     C                   eval      ORVblc = ORMblc
026400051212     C                   eval      ORVatt = ORMatt
026500051212     C                   eval      ORVmtc = ORMmtc
026600051212     C                   eval      ORVpag = ORMpag
026700051212     C                   eval      ORVksc = ORMksc
026800051212     C                   eval      ORVctr = ORMctr
026900051212     C                   eval      ORVddt = ORMddt
027000051212     C                   eval      ORVpor = ORMpor
027100051212     C                   eval      ORVzor = ORMzor
027200051212     C                   eval      ORVpoc = ORMpoc
027300051212     C                   eval      ORVno1 = ORMno1
027400051212     C                   eval      ORVno2 = ORMno2
027500051212     C                   eval      ORVcst = ORMcst
027600051212     C                   eval      ORVvcs = ORMvcs
027700051212     C                   eval      ORVfcs = ORMfcs
027800051212     C                   eval      ORVdfo = ORMdfo
027900051212     C                   eval      ORVofo = ORMofo
028000051212     C                   eval      ORVfao = ORMfao
028100051212     C                   eval      ORVsto = ORMsto
028200051212     C                   eval      ORVdst = ORMdst
028300051212     C                   eval      ORVnpg = ORMnpg
028400051212     C                   eval      ORVndc = ORMndc
028500051212     C                   eval      ORVddc = ORMddc
028600051212     C                   eval      ORVstp = ORMstp
028700051212     C                   eval      ORVrfa = ORMrfa
028800051212     C                   eval      ORVsto = ORMsto
028900070711     c                   eval      orveti = ormeti
029000051212     C                   eval      ORVspi = ORMspi
029100051212     C                   eval      ORVflo = ORMflo
029200051212     C                   movel     knmus         ORVutv
029300051212     C                   TIME                    W0140            14 0
029400051212     C                   MOVEL     W0140         orvorv
029500051212     C                   CLEAR                   WLBDAT
029600051212     C                   MOVE      W0140         G02DAT
029700051212     C                   CALL      'XSRDA8'
029800051212     C                   PARM                    WLBDAT
029900051212     C                   Z-ADD     G02INV        orvdtv
030000051212     C                   Z-ADD     G02INV        orvdtt
030100051212     C                   write     fnorv000
030200051212
030300051212     c                   ENDSR
030400040326      *------------------------------------------------------------------------*
030500040326      * - ROUTINE INIZIALE
030600040326      *------------------------------------------------------------------------*
030700040326     c     *Inzsr        BegSr
030800040326
030900040326     c     *Entry        Plist
031000040326     c                   Parm                    Kpjba
031100040416
031200040416     c                   Eval      Parm01 = kpjbu
031300040416
031400040416     c     kFgv          klist
031500040416     c                   kfld                    nfv
031600040416     c                   kfld                    fgs
031700040416
031800040416     c     kBlp          klist
031900040416     c                   kfld                    lin(xx)
032000040416     c                   kfld                    wlna03
032100040416     c                   kfld                    kAas
032200040416     c                   kfld                    kMgs
032300040416
032400040416     c     kBlp1         klist
032500040416     c                   kfld                    lin(xx)
032600040416     c                   kfld                    wlna03
032700040416
032800060214     c     kAr4          klist
032900040416     c                   kfld                    BlpAas
033000040416     c                   kfld                    BlpLnp
033100040416     c                   kfld                    BlpNrs
033200040416     c                   kfld                    BlpNsp
033300060214     c                   kFld                    Ar4Trc
033400040416
033500040416     c     kOrm          klist
033600040416     c                   kfld                    §b4Poe
033700040416     c                   kfld                    §b4Nsr
033800040416     c                   kfld                    §b4Nor
033900040416     c                   kfld                    §B4Nrv
034000060516
034100060516     c     kdst          klist
034200060516     c                   kfld                    kdstnfv
034300060516     c                   kfld                    kdstfgs
034400040416
034500060214     c                   Eval      Ar4Trc = 'M'
034600040416
034700040416      * Aggancio foglio viaggio partenza
034800040416     c     kFgv          Chain     Fnfgv01l
034900040416     c     kFgv          Chain     Fnfgw01l
035000040416     c                   If        Not %Found(Fnfgw01l) or FgwAtb <> *Blanks
035100040416     c                   Clear                   FgwFl3
035200040416     c                   Clear                   FgwFl4
035300040416     c                   EndIf
035400040416
035500040416      * Carico la £1 del p.o. gestione foglio (è sempre il terminal di partenza)
035600040416     c                   Clear                   Trul06ds
035700040416     c                   Eval      D06Cod = '£1'
035800040416     c                   Movel     fgs           D06Key
035900040416     c                   Movel     Trul06ds      kpjbu
036000040416     c                   Call      'TRUL06R'
036100040416     c                   Parm                    kpjba
036200040416     c                   Movel     kpjbu         Trul06ds
036300040326
036400040326     c     EndInz        EndSr
