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
001800160323     fazorg01l  if   e           k disk
001900160323     ftabel00f  if   e           k disk
002000040326
002100040326      *------------------------------------------------------------------------*
002200040326      *   V A R I A B I L I
002300040326      *------------------------------------------------------------------------*
002400120612     d kdstnpg         s                   like(dstnpg)
002500060516     d kdstfgs         s                   like(dstfgs)
002600060516     d kdstnfv         s                   like(dstnfv)
002700160323     d kTBLkut         s                   like(TBLkut) inz(1)
002800160323     d kTBLcod         s                   like(TBLcod) inz('3A')
002900160323     d kTBLkey         s                   like(TBLkey)
003000040416     d wlna03          s                   Like(BlpLna)
003100040416     d xx              s              2  0
003200040416     d yy              s              3  0
003300040416
003400040326      *------------------------------------------------------------------------*
003500040326      *   D S   I N T E R N E / E S T E R N E
003600040326      *------------------------------------------------------------------------*
003700040416     d                 ds
003800040416     d  kAas                   1      4  0
003900040416     d  kMgs                   5      8  0
004000040416     d  FgvDfv                 1      8  0
004100051212
004200051212     D WLBDAT          DS                  INZ
004300051212     D  G02DAT                 1      8  0
004400051212     D  G02INV                 9     16  0
004500051212     D  G02ERR                17     17
004600051212     D  G02TGI                18     22  0
004700040416
004800040416     d                 ds
004900040416     d  FgvLna                 1      3  0
005000040416     d  FgvFfv                 4    243
005100040416     d  FgvFf2               244    450
005200040416     d  FgwFf3               451    690
005300040416     d  FgwFf4               691    900
005400040416     d  lna                    1    900    dim(300)
005500050428
005600050428     d                 ds
005700050428     d  blpaas                 1      4  0
005800050428     d  blpmgs                 5      8  0
005900050428     d  blpdsp                 1      8  0
006000040416
006100040416     d parm01          ds
006200040416     d  fgs                           3  0
006300040416     d  nfv                           5  0
006400040416
006500070711     d dfar          e ds
006600040416     d dsbl4m        e ds
006700160323     d og143         e ds
006800160323     d ds3a          e ds
006900040416
007000040416     d Trul06ds      e ds
007100040416     d  lin                    1     90  0 dim(30)
007200040416
007300040326     d Kpjba         e ds
007400060516     d fnlv55ds      e ds
007500070711     d tibs02ds      e ds
007600040326
007700040326      *------------------------------------------------------------------------*
007800001025
007900040617      * Leggo le bolle partenza  (per linea di partenza - £1)
008000040416do  1c                   Do        30            xx
008100040416      * non ho più linee di partenza da leggere esco
008200040416     c                   If        lin(xx) = *Zeros
008300040416     c                   Leave
008400040416     c                   EndIf
008500040416      * linee di arrivo
008600040416do  2c                   Do        300           yy
008700040416      * non ho più linee di arrivo da leggere esco
008800040617     c                   If        lna(yy) <= *Zeros
008900040416     c                   Leave
009000040416     c                   EndIf
009100040416
009200040416     c                   Move      lna(yy)       wlna03
009300040416
009400040617     c     kBlp          Setgt     Fnblp76l
009500040416do  3c                   Do        *Hival
009600040416     c     kBlp1         Reade     Fnblp76l
009700040330      * Fine file
009800040416     c                   If        %Eof(Fnblp76l)
009900040326     c                   Leave
010000040326     c                   EndIf
010100050429      * la bolla deve avere immissione <= data foglio
010200050428     c                   If        blpdim > fgvdfv
010300050428     c                   Iter
010400050428     c                   EndIf
010500050428      * la bolla deve avere data immissione < data spedizione
010600050428     c                   If        blpdim >= blpdsp
010700050428     c                   Iter
010800050428     c                   EndIf
010900040416      * Non deve essere borderizzata
011000040416     c                   If        BlpDbr > *Zeros
011100040416     c                   Iter
011200040416     c                   EndIf
011300040714      * Se non c'è l'autotrasportatore è una bolla prepagato non la elaboro
011400040714     c                   If        BlpPdr = *Zeros
011500040714     c                   Iter
011600040714     c                   EndIf
011700040416      * Controllo se bolla abbinata ad un ORM
011800060214     c     kAr4          Chain     Fiar401l
011900060214     c                   If        Not %Found(Fiar401l)
012000040416     c                   Iter
012100040416     c                   EndIf
012200160323      /free
012300160323       //?non devo avanzare l'ORM se è un bolla import DPD o EEX in assegnato
012400160323         clear OG143;
012500160323         chain (BLPlnp) AZORG01L;
012600160323         IF  %found(AZORG01L);
012700160323           OG143 = ORGde3;
012800160323         ENDIF;
012900160323         clear ds3a;
013000160323         kTBLkey = BLPcbo;
013100160323         chain (kTBLkut:kTBLcod:kTBLkey) TABEL00F;
013200160323         IF  %found(TABEL00F);
013300160323           ds3A = TBLuni;
013400160323         ENDIF;
013500160401         IF  (§OGntw = 'EEX' or §OGntw = 'DPD') and
013600160401             %subst(§3Atb1:1:1) = 'A';
013700160323           iter;
013800160323         ENDIF;
013900160323      /end-free
014000060214     c                   Eval      dsbl4m = ar4Not
014100040416     c     kOrm          Chain     Fnorm01l
014200040416     c                   If        %Found(Fnorm01l) and OrmFao <> 900
014300051212     c* Se il p.o. di ritiro non è uguale alla linea di partenza,
014400051212     c*  aggiorno in questo momento
014500060214    2c                   if        ormpor<>ar4lnp
014600051212     c                   exsr      sr_Storico
014700051212     c* Scrivo fase 200
014800051212     c                   z-add     200           ormfao
014900051212     C                   Z-ADD     FGVDFV        ORMDFO
015000070110     C                   Z-ADD     235859        ORMOFO
015100051212     c                   exsr      rieorf
015200051212     c                   write     fnorf000
015300051212     c
015400060214     c                   z-add     ar4lnp        ormpor
015500070711      * se ho aggiornato il p.o. ritiro dell'ORM devo aggiornare anche FNORG
015600070711     c     korm          chain     fnorg01l
015700070711if  1c                   if        %found(fnorg01l)
015800070711      * cambio il p.o. ritiro
015900070711     c                   eval      orgpor = ormpor
016000070711     c                   update    fnorg000
016100070711    2c                   endif
016200051212    2c                   endif
016300051212     c
016400040416     c                   ExSr      Sr_AggOrm
016500040416     c                   EndIf
016600040416e   3c                   EndDo
016700040416
016800040416e   2c                   EndDo
016900040415e   1c                   EndDo
017000040326
017100040326     c                   Eval      *InLr = *On
017200040416
017300051212      *------------------------------------------------------------------------*
017400051212     C* Riempio campi record fase O.R.M.
017500051212      *------------------------------------------------------------------------*
017600051212     C     RIEORF        BEGSR
017700051212     C                   clear                   fnorf000
017800051212     C                   eval      ORFpoe = ormpoe
017900051212     C                   eval      ORFnsr = ormnsr
018000051212     C                   eval      ORFnor = ormnor
018100051212     C                   eval      ORFnrv = ormnrv
018200051212     C                   eval      ORFpog = ormpor
018300051212     C                   eval      ORFdae = ormdfo
018400051212     C                   eval      ORFore = ormofo
018500051212     C                   eval      ORFfar = ormfao
018600051212     C                   eval      ORFpue = knmus
018700051212     c                   If        OrmNdc > *Zeros
018800120612     c                   eval      kdstnpg = ormnpg
018900060516      * devo trovare il p.o. gestione distinta giusto
019000060516     c                   move      ormndc        kdstnfv
019100060516      * prima provo con il p.o. ritiro
019200060516     c                   eval      kdstfgs = ormpor
019300120612     c     kdst          chain     fidst01l
019400060516      * se non lo trovo cerco se il p.o. ritiro è gestito
019500120612     c                   if        not %found(fidst01l)
019600060516     c                   clear                   fnlv55ds
019700060516     c                   eval      d55tpt = '6'
019800060516     c                   move      ormpor        d55lin
019900060516     c                   eval      d55drf = ormddc
020000060516     c                   call      'FNLV55R'
020100060516     c                   parm                    fnlv55ds
020200060516     c                   if        d55err = *blanks and d55tfa <> d55lin
020300060516     c                   move      d55tfa        kdstfgs
020400120612     c     kdst          chain     fidst01l
020500060516      * se non trovo neanche con il p.o. che gestisce metto il p.o. ritiro
020600060516      * teoricamente non dovrebbe succedere
020700120612     c                   if        not %found(fidst01l)
020800060516     c                   eval      kdstfgs = ormpor
020900060516     c                   endif
021000060516     c                   endif
021100060516     c                   endif
021200060516     c                   eval      orffgs = kdstfgs
021300051212     c                   Eval      OrfNdc = OrmNdc
021400051212     c                   Eval      OrfDdc = OrmDdc
021500051212     c                   EndIf
021600051212
021700051212     c                   ENDSR
021800040416      *------------------------------------------------------------------------*
021900040416      * - AGGIORNO L'ORM E SCRIVO LA FASE 900
022000040416      *------------------------------------------------------------------------*
022100040416     c     Sr_AggOrm     BegSr
022200040416
022300040416     c                   Eval      OrmFao = 900
022400040416     c                   Eval      OrmDfo = FgvDfv
022500070110     c                   Eval      OrmOfo = 235958
022600040908     c                   Z-add     *Date         OrmDtt
022700051212     c                   exsr      RIEORF
022800070711      * imposto ormeti
022900070711     c                   clear                   dfar
023000070711     c                   clear                   tibs02ds
023100070711     c                   eval      t02mod = 'C'
023200070711     c                   eval      t02sif = knsif
023300070711     c                   eval      t02cod = 'FAR'
023400070711     c                   movel(p)  ormfao        t02ke1
023500070711     c                   call      'TIBS02R'
023600070711     c                   parm                    kpjba
023700070711     c                   parm                    tibs02ds
023800070711     c                   eval      dfar = t02uni
023900070711     c                   eval      ormeti = d§farass
024000040416
024100040416     c                   Write     Fnorf000
024200040416
024300040416     c                   Update    Fnorm000
024400040416
024500040416     c                   EndSr
024600040326
024700051212      *------------------------------------------------------------------------*
024800051212     C* Scrivo lo storico orm
024900051212      *------------------------------------------------------------------------*
025000051212     C     SR_Storico    BEGSR
025100051212     C                   clear                   fnorv000
025200051212     C                   eval      ORVpoe = ORMpoe
025300051212     C                   eval      ORVnsr = ORMnsr
025400051212     C                   eval      ORVnor = ORMnor
025500051212     C                   eval      ORVnrv = ORMnrv
025600051212     C                   eval      ORVtor = ORMtor
025700051212     C                   eval      ORVcor = ORMcor
025800051212     C                   eval      ORVrso = ORMrso
025900051212     C                   eval      ORVino = ORMino
026000051212     C                   eval      ORVcao = ORMcao
026100051212     C                   eval      ORVloo = ORMloo
026200051212     C                   eval      ORVpro = ORMpro
026300051212     C                   eval      ORVnao = ORMnao
026400051212     C                   eval      ORVcra = ORMcra
026500051212     C                   eval      ORVrsr = ORMrsr
026600051212     C                   eval      ORVinr = ORMinr
026700051212     C                   eval      ORVcar = ORMcar
026800051212     C                   eval      ORVlor = ORMlor
026900051212     C                   eval      ORVprr = ORMprr
027000051212     C                   eval      ORVnar = ORMnar
027100051212     C                   eval      ORVrer = ORMrer
027200051212     C                   eval      ORVter = ORMter
027300051212     C                   eval      ORVcrc = ORMcrc
027400051212     C                   eval      ORVrsc = ORMrsc
027500051212     C                   eval      ORVinc = ORMinc
027600051212     C                   eval      ORVloc = ORMloc
027700051212     C                   eval      ORVcac = ORMcac
027800051212     C                   eval      ORVprc = ORMprc
027900051212     C                   eval      ORVnac = ORMnac
028000051212     C                   eval      ORVffd = ORMffd
028100051212     C                   eval      ORVdar = ORMdar
028200051212     C                   eval      ORVorr = ORMorr
028300051212     C                   eval      ORVnam = ORMnam
028400051212     C                   eval      ORVncl = ORMncl
028500051212     C                   eval      ORVpkg = ORMpkg
028600051212     C                   eval      ORVvlm = ORMvlm
028700051212     C                   eval      ORVbnc = ORMbnc
028800051212     C                   eval      ORVblc = ORMblc
028900051212     C                   eval      ORVatt = ORMatt
029000051212     C                   eval      ORVmtc = ORMmtc
029100051212     C                   eval      ORVpag = ORMpag
029200051212     C                   eval      ORVksc = ORMksc
029300051212     C                   eval      ORVctr = ORMctr
029400051212     C                   eval      ORVddt = ORMddt
029500051212     C                   eval      ORVpor = ORMpor
029600051212     C                   eval      ORVzor = ORMzor
029700051212     C                   eval      ORVpoc = ORMpoc
029800051212     C                   eval      ORVno1 = ORMno1
029900051212     C                   eval      ORVno2 = ORMno2
030000051212     C                   eval      ORVcst = ORMcst
030100051212     C                   eval      ORVvcs = ORMvcs
030200051212     C                   eval      ORVfcs = ORMfcs
030300051212     C                   eval      ORVdfo = ORMdfo
030400051212     C                   eval      ORVofo = ORMofo
030500051212     C                   eval      ORVfao = ORMfao
030600051212     C                   eval      ORVsto = ORMsto
030700051212     C                   eval      ORVdst = ORMdst
030800051212     C                   eval      ORVnpg = ORMnpg
030900051212     C                   eval      ORVndc = ORMndc
031000051212     C                   eval      ORVddc = ORMddc
031100051212     C                   eval      ORVstp = ORMstp
031200051212     C                   eval      ORVrfa = ORMrfa
031300051212     C                   eval      ORVsto = ORMsto
031400070711     c                   eval      orveti = ormeti
031500051212     C                   eval      ORVspi = ORMspi
031600051212     C                   eval      ORVflo = ORMflo
031700051212     C                   movel     knmus         ORVutv
031800051212     C                   TIME                    W0140            14 0
031900051212     C                   MOVEL     W0140         orvorv
032000051212     C                   CLEAR                   WLBDAT
032100051212     C                   MOVE      W0140         G02DAT
032200051212     C                   CALL      'XSRDA8'
032300051212     C                   PARM                    WLBDAT
032400051212     C                   Z-ADD     G02INV        orvdtv
032500051212     C                   Z-ADD     G02INV        orvdtt
032600051212     C                   write     fnorv000
032700051212
032800051212     c                   ENDSR
032900040326      *------------------------------------------------------------------------*
033000040326      * - ROUTINE INIZIALE
033100040326      *------------------------------------------------------------------------*
033200040326     c     *Inzsr        BegSr
033300040326
033400040326     c     *Entry        Plist
033500040326     c                   Parm                    Kpjba
033600040416
033700040416     c                   Eval      Parm01 = kpjbu
033800040416
033900040416     c     kFgv          klist
034000040416     c                   kfld                    nfv
034100040416     c                   kfld                    fgs
034200040416
034300040416     c     kBlp          klist
034400040416     c                   kfld                    lin(xx)
034500040416     c                   kfld                    wlna03
034600040416     c                   kfld                    kAas
034700040416     c                   kfld                    kMgs
034800040416
034900040416     c     kBlp1         klist
035000040416     c                   kfld                    lin(xx)
035100040416     c                   kfld                    wlna03
035200040416
035300060214     c     kAr4          klist
035400040416     c                   kfld                    BlpAas
035500040416     c                   kfld                    BlpLnp
035600040416     c                   kfld                    BlpNrs
035700040416     c                   kfld                    BlpNsp
035800060214     c                   kFld                    Ar4Trc
035900040416
036000040416     c     kOrm          klist
036100040416     c                   kfld                    §b4Poe
036200040416     c                   kfld                    §b4Nsr
036300040416     c                   kfld                    §b4Nor
036400040416     c                   kfld                    §B4Nrv
036500060516
036600060516     c     kdst          klist
036700120612     c                   kfld                    kdstnpg
036800060516     c                   kfld                    kdstnfv
036900060516     c                   kfld                    kdstfgs
037000040416
037100060214     c                   Eval      Ar4Trc = 'M'
037200040416
037300040416      * Aggancio foglio viaggio partenza
037400040416     c     kFgv          Chain     Fnfgv01l
037500040416     c     kFgv          Chain     Fnfgw01l
037600040416     c                   If        Not %Found(Fnfgw01l) or FgwAtb <> *Blanks
037700040416     c                   Clear                   FgwFl3
037800040416     c                   Clear                   FgwFl4
037900040416     c                   EndIf
038000040416
038100040416      * Carico la £1 del p.o. gestione foglio (è sempre il terminal di partenza)
038200040416     c                   Clear                   Trul06ds
038300040416     c                   Eval      D06Cod = '£1'
038400040416     c                   Movel     fgs           D06Key
038500040416     c                   Movel     Trul06ds      kpjbu
038600040416     c                   Call      'TRUL06R'
038700040416     c                   Parm                    kpjba
038800040416     c                   Movel     kpjbu         Trul06ds
038900040326
039000040326     c     EndInz        EndSr
