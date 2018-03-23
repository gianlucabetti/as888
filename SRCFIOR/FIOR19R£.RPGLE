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
001700120612     ffidst01l  if   e           k disk
001800040326
001900040326      *------------------------------------------------------------------------*
002000040326      *   V A R I A B I L I
002100040326      *------------------------------------------------------------------------*
002200120612     d kdstnpg         s                   like(dstnpg)
002300060516     d kdstfgs         s                   like(dstfgs)
002400060516     d kdstnfv         s                   like(dstnfv)
002500040416     d wlna03          s                   Like(BlpLna)
002600040416     d xx              s              2  0
002700040416     d yy              s              3  0
002800040416
002900040326      *------------------------------------------------------------------------*
003000040326      *   D S   I N T E R N E / E S T E R N E
003100040326      *------------------------------------------------------------------------*
003200040416     d                 ds
003300040416     d  kAas                   1      4  0
003400040416     d  kMgs                   5      8  0
003500040416     d  FgvDfv                 1      8  0
003600051212
003700051212     D WLBDAT          DS                  INZ
003800051212     D  G02DAT                 1      8  0
003900051212     D  G02INV                 9     16  0
004000051212     D  G02ERR                17     17
004100051212     D  G02TGI                18     22  0
004200040416
004300040416     d                 ds
004400040416     d  FgvLna                 1      3  0
004500040416     d  FgvFfv                 4    243
004600040416     d  FgvFf2               244    450
004700040416     d  FgwFf3               451    690
004800040416     d  FgwFf4               691    900
004900040416     d  lna                    1    900    dim(300)
005000050428
005100050428     d                 ds
005200050428     d  blpaas                 1      4  0
005300050428     d  blpmgs                 5      8  0
005400050428     d  blpdsp                 1      8  0
005500040416
005600040416     d parm01          ds
005700040416     d  fgs                           3  0
005800040416     d  nfv                           5  0
005900040416
006000070711     d dfar          e ds
006100040416     d dsbl4m        e ds
006200040416
006300040416     d Trul06ds      e ds
006400040416     d  lin                    1     90  0 dim(30)
006500040416
006600040326     d Kpjba         e ds
006700060516     d fnlv55ds      e ds
006800070711     d tibs02ds      e ds
006900040326
007000040326      *------------------------------------------------------------------------*
007100001025
007200040617      * Leggo le bolle partenza  (per linea di partenza - £1)
007300040416do  1c                   Do        30            xx
007400040416      * non ho più linee di partenza da leggere esco
007500040416     c                   If        lin(xx) = *Zeros
007600040416     c                   Leave
007700040416     c                   EndIf
007800040416      * linee di arrivo
007900040416do  2c                   Do        300           yy
008000040416      * non ho più linee di arrivo da leggere esco
008100040617     c                   If        lna(yy) <= *Zeros
008200040416     c                   Leave
008300040416     c                   EndIf
008400040416
008500040416     c                   Move      lna(yy)       wlna03
008600040416
008700040617     c     kBlp          Setgt     Fnblp76l
008800040416do  3c                   Do        *Hival
008900040416     c     kBlp1         Reade     Fnblp76l
009000040330      * Fine file
009100040416     c                   If        %Eof(Fnblp76l)
009200040326     c                   Leave
009300040326     c                   EndIf
009400050429      * la bolla deve avere immissione <= data foglio
009500050428     c                   If        blpdim > fgvdfv
009600050428     c                   Iter
009700050428     c                   EndIf
009800050428      * la bolla deve avere data immissione < data spedizione
009900050428     c                   If        blpdim >= blpdsp
010000050428     c                   Iter
010100050428     c                   EndIf
010200040416      * Non deve essere borderizzata
010300040416     c                   If        BlpDbr > *Zeros
010400040416     c                   Iter
010500040416     c                   EndIf
010600040714      * Se non c'è l'autotrasportatore è una bolla prepagato non la elaboro
010700040714     c                   If        BlpPdr = *Zeros
010800040714     c                   Iter
010900040714     c                   EndIf
011000040416      * Controllo se bolla abbinata ad un ORM
011100060214     c     kAr4          Chain     Fiar401l
011200060214     c                   If        Not %Found(Fiar401l)
011300040416     c                   Iter
011400040416     c                   EndIf
011500040416
011600060214     c                   Eval      dsbl4m = ar4Not
011700040416     c     kOrm          Chain     Fnorm01l
011800040416     c                   If        %Found(Fnorm01l) and OrmFao <> 900
011900051212     c* Se il p.o. di ritiro non è uguale alla linea di partenza,
012000051212     c*  aggiorno in questo momento
012100060214    2c                   if        ormpor<>ar4lnp
012200051212     c                   exsr      sr_Storico
012300051212     c* Scrivo fase 200
012400051212     c                   z-add     200           ormfao
012500051212     C                   Z-ADD     FGVDFV        ORMDFO
012600070110     C                   Z-ADD     235859        ORMOFO
012700051212     c                   exsr      rieorf
012800051212     c                   write     fnorf000
012900051212     c
013000060214     c                   z-add     ar4lnp        ormpor
013100070711      * se ho aggiornato il p.o. ritiro dell'ORM devo aggiornare anche FNORG
013200070711     c     korm          chain     fnorg01l
013300070711if  1c                   if        %found(fnorg01l)
013400070711      * cambio il p.o. ritiro
013500070711     c                   eval      orgpor = ormpor
013600070711     c                   update    fnorg000
013700070711    2c                   endif
013800051212    2c                   endif
013900051212     c
014000040416     c                   ExSr      Sr_AggOrm
014100040416     c                   EndIf
014200040416e   3c                   EndDo
014300040416
014400040416e   2c                   EndDo
014500040415e   1c                   EndDo
014600040326
014700040326     c                   Eval      *InLr = *On
014800040416
014900051212      *------------------------------------------------------------------------*
015000051212     C* Riempio campi record fase O.R.M.
015100051212      *------------------------------------------------------------------------*
015200051212     C     RIEORF        BEGSR
015300051212     C                   clear                   fnorf000
015400051212     C                   eval      ORFpoe = ormpoe
015500051212     C                   eval      ORFnsr = ormnsr
015600051212     C                   eval      ORFnor = ormnor
015700051212     C                   eval      ORFnrv = ormnrv
015800051212     C                   eval      ORFpog = ormpor
015900051212     C                   eval      ORFdae = ormdfo
016000051212     C                   eval      ORFore = ormofo
016100051212     C                   eval      ORFfar = ormfao
016200051212     C                   eval      ORFpue = knmus
016300051212     c                   If        OrmNdc > *Zeros
016400120612     c                   eval      kdstnpg = ormnpg
016500060516      * devo trovare il p.o. gestione distinta giusto
016600060516     c                   move      ormndc        kdstnfv
016700060516      * prima provo con il p.o. ritiro
016800060516     c                   eval      kdstfgs = ormpor
016900120612     c     kdst          chain     fidst01l
017000060516      * se non lo trovo cerco se il p.o. ritiro è gestito
017100120612     c                   if        not %found(fidst01l)
017200060516     c                   clear                   fnlv55ds
017300060516     c                   eval      d55tpt = '6'
017400060516     c                   move      ormpor        d55lin
017500060516     c                   eval      d55drf = ormddc
017600060516     c                   call      'FNLV55R'
017700060516     c                   parm                    fnlv55ds
017800060516     c                   if        d55err = *blanks and d55tfa <> d55lin
017900060516     c                   move      d55tfa        kdstfgs
018000120612     c     kdst          chain     fidst01l
018100060516      * se non trovo neanche con il p.o. che gestisce metto il p.o. ritiro
018200060516      * teoricamente non dovrebbe succedere
018300120612     c                   if        not %found(fidst01l)
018400060516     c                   eval      kdstfgs = ormpor
018500060516     c                   endif
018600060516     c                   endif
018700060516     c                   endif
018800060516     c                   eval      orffgs = kdstfgs
018900051212     c                   Eval      OrfNdc = OrmNdc
019000051212     c                   Eval      OrfDdc = OrmDdc
019100051212     c                   EndIf
019200051212
019300051212     c                   ENDSR
019400040416      *------------------------------------------------------------------------*
019500040416      * - AGGIORNO L'ORM E SCRIVO LA FASE 900
019600040416      *------------------------------------------------------------------------*
019700040416     c     Sr_AggOrm     BegSr
019800040416
019900040416     c                   Eval      OrmFao = 900
020000040416     c                   Eval      OrmDfo = FgvDfv
020100070110     c                   Eval      OrmOfo = 235958
020200040908     c                   Z-add     *Date         OrmDtt
020300051212     c                   exsr      RIEORF
020400070711      * imposto ormeti
020500070711     c                   clear                   dfar
020600070711     c                   clear                   tibs02ds
020700070711     c                   eval      t02mod = 'C'
020800070711     c                   eval      t02sif = knsif
020900070711     c                   eval      t02cod = 'FAR'
021000070711     c                   movel(p)  ormfao        t02ke1
021100070711     c                   call      'TIBS02R'
021200070711     c                   parm                    kpjba
021300070711     c                   parm                    tibs02ds
021400070711     c                   eval      dfar = t02uni
021500070711     c                   eval      ormeti = d§farass
021600040416
021700040416     c                   Write     Fnorf000
021800040416
021900040416     c                   Update    Fnorm000
022000040416
022100040416     c                   EndSr
022200040326
022300051212      *------------------------------------------------------------------------*
022400051212     C* Scrivo lo storico orm
022500051212      *------------------------------------------------------------------------*
022600051212     C     SR_Storico    BEGSR
022700051212     C                   clear                   fnorv000
022800051212     C                   eval      ORVpoe = ORMpoe
022900051212     C                   eval      ORVnsr = ORMnsr
023000051212     C                   eval      ORVnor = ORMnor
023100051212     C                   eval      ORVnrv = ORMnrv
023200051212     C                   eval      ORVtor = ORMtor
023300051212     C                   eval      ORVcor = ORMcor
023400051212     C                   eval      ORVrso = ORMrso
023500051212     C                   eval      ORVino = ORMino
023600051212     C                   eval      ORVcao = ORMcao
023700051212     C                   eval      ORVloo = ORMloo
023800051212     C                   eval      ORVpro = ORMpro
023900051212     C                   eval      ORVnao = ORMnao
024000051212     C                   eval      ORVcra = ORMcra
024100051212     C                   eval      ORVrsr = ORMrsr
024200051212     C                   eval      ORVinr = ORMinr
024300051212     C                   eval      ORVcar = ORMcar
024400051212     C                   eval      ORVlor = ORMlor
024500051212     C                   eval      ORVprr = ORMprr
024600051212     C                   eval      ORVnar = ORMnar
024700051212     C                   eval      ORVrer = ORMrer
024800051212     C                   eval      ORVter = ORMter
024900051212     C                   eval      ORVcrc = ORMcrc
025000051212     C                   eval      ORVrsc = ORMrsc
025100051212     C                   eval      ORVinc = ORMinc
025200051212     C                   eval      ORVloc = ORMloc
025300051212     C                   eval      ORVcac = ORMcac
025400051212     C                   eval      ORVprc = ORMprc
025500051212     C                   eval      ORVnac = ORMnac
025600051212     C                   eval      ORVffd = ORMffd
025700051212     C                   eval      ORVdar = ORMdar
025800051212     C                   eval      ORVorr = ORMorr
025900051212     C                   eval      ORVnam = ORMnam
026000051212     C                   eval      ORVncl = ORMncl
026100051212     C                   eval      ORVpkg = ORMpkg
026200051212     C                   eval      ORVvlm = ORMvlm
026300051212     C                   eval      ORVbnc = ORMbnc
026400051212     C                   eval      ORVblc = ORMblc
026500051212     C                   eval      ORVatt = ORMatt
026600051212     C                   eval      ORVmtc = ORMmtc
026700051212     C                   eval      ORVpag = ORMpag
026800051212     C                   eval      ORVksc = ORMksc
026900051212     C                   eval      ORVctr = ORMctr
027000051212     C                   eval      ORVddt = ORMddt
027100051212     C                   eval      ORVpor = ORMpor
027200051212     C                   eval      ORVzor = ORMzor
027300051212     C                   eval      ORVpoc = ORMpoc
027400051212     C                   eval      ORVno1 = ORMno1
027500051212     C                   eval      ORVno2 = ORMno2
027600051212     C                   eval      ORVcst = ORMcst
027700051212     C                   eval      ORVvcs = ORMvcs
027800051212     C                   eval      ORVfcs = ORMfcs
027900051212     C                   eval      ORVdfo = ORMdfo
028000051212     C                   eval      ORVofo = ORMofo
028100051212     C                   eval      ORVfao = ORMfao
028200051212     C                   eval      ORVsto = ORMsto
028300051212     C                   eval      ORVdst = ORMdst
028400051212     C                   eval      ORVnpg = ORMnpg
028500051212     C                   eval      ORVndc = ORMndc
028600051212     C                   eval      ORVddc = ORMddc
028700051212     C                   eval      ORVstp = ORMstp
028800051212     C                   eval      ORVrfa = ORMrfa
028900051212     C                   eval      ORVsto = ORMsto
029000070711     c                   eval      orveti = ormeti
029100051212     C                   eval      ORVspi = ORMspi
029200051212     C                   eval      ORVflo = ORMflo
029300051212     C                   movel     knmus         ORVutv
029400051212     C                   TIME                    W0140            14 0
029500051212     C                   MOVEL     W0140         orvorv
029600051212     C                   CLEAR                   WLBDAT
029700051212     C                   MOVE      W0140         G02DAT
029800051212     C                   CALL      'XSRDA8'
029900051212     C                   PARM                    WLBDAT
030000051212     C                   Z-ADD     G02INV        orvdtv
030100051212     C                   Z-ADD     G02INV        orvdtt
030200051212     C                   write     fnorv000
030300051212
030400051212     c                   ENDSR
030500040326      *------------------------------------------------------------------------*
030600040326      * - ROUTINE INIZIALE
030700040326      *------------------------------------------------------------------------*
030800040326     c     *Inzsr        BegSr
030900040326
031000040326     c     *Entry        Plist
031100040326     c                   Parm                    Kpjba
031200040416
031300040416     c                   Eval      Parm01 = kpjbu
031400040416
031500040416     c     kFgv          klist
031600040416     c                   kfld                    nfv
031700040416     c                   kfld                    fgs
031800040416
031900040416     c     kBlp          klist
032000040416     c                   kfld                    lin(xx)
032100040416     c                   kfld                    wlna03
032200040416     c                   kfld                    kAas
032300040416     c                   kfld                    kMgs
032400040416
032500040416     c     kBlp1         klist
032600040416     c                   kfld                    lin(xx)
032700040416     c                   kfld                    wlna03
032800040416
032900060214     c     kAr4          klist
033000040416     c                   kfld                    BlpAas
033100040416     c                   kfld                    BlpLnp
033200040416     c                   kfld                    BlpNrs
033300040416     c                   kfld                    BlpNsp
033400060214     c                   kFld                    Ar4Trc
033500040416
033600040416     c     kOrm          klist
033700040416     c                   kfld                    §b4Poe
033800040416     c                   kfld                    §b4Nsr
033900040416     c                   kfld                    §b4Nor
034000040416     c                   kfld                    §B4Nrv
034100060516
034200060516     c     kdst          klist
034300120612     c                   kfld                    kdstnpg
034400060516     c                   kfld                    kdstnfv
034500060516     c                   kfld                    kdstfgs
034600040416
034700060214     c                   Eval      Ar4Trc = 'M'
034800040416
034900040416      * Aggancio foglio viaggio partenza
035000040416     c     kFgv          Chain     Fnfgv01l
035100040416     c     kFgv          Chain     Fnfgw01l
035200040416     c                   If        Not %Found(Fnfgw01l) or FgwAtb <> *Blanks
035300040416     c                   Clear                   FgwFl3
035400040416     c                   Clear                   FgwFl4
035500040416     c                   EndIf
035600040416
035700040416      * Carico la £1 del p.o. gestione foglio (è sempre il terminal di partenza)
035800040416     c                   Clear                   Trul06ds
035900040416     c                   Eval      D06Cod = '£1'
036000040416     c                   Movel     fgs           D06Key
036100040416     c                   Movel     Trul06ds      kpjbu
036200040416     c                   Call      'TRUL06R'
036300040416     c                   Parm                    kpjba
036400040416     c                   Movel     kpjbu         Trul06ds
036500040326
036600040326     c     EndInz        EndSr
