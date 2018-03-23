000100050831     h Decedit('0,') Datedit(*dmy.)
000200040910      *------------------------------------------------------------------------*
000300050830      * Scrittura file FNVAP in TIVGD00F x le fasi ancora da inviare al cliente
000400040910      *------------------------------------------------------------------------*
000500040910     fFnorf04l  uf   e           k disk
000600040910     fFnorm01l  if   e           k disk
000700040910     fTntbe01l  if   e           k disk
000800040910     fTiori05l  if   e           k disk
000900050830     ftivgd00f  o    e             disk
001000050830     fazorg01l  if   e           k disk
001100040910
001200040910      *------------------------------------------------------------------------*
001300040910      *   S C H I E R E
001400040910      *------------------------------------------------------------------------*
001500040910     d cfar            S                   like(ORFfar) dim(99) inz(*loval)     *cod. fasi ORM
001600040910     d far             S                   like(DFAR)     dim(99) inz           *dati fasi ORM
001700040910     d ccmr            S                   like(ORFcar) dim(99) inz(*hival)     *cod. causali ORM
001800040910     d cmr             S                   like(DCMR)     dim(99) inz           *dati causali ORM
001900040910
002000040910      *------------------------------------------------------------------------*
002100040910      *   V A R I A B I L I
002200040910      *------------------------------------------------------------------------*
002300040910     d xx              S              3  0 INZ
002400001108
002500040910      *------------------------------------------------------------------------*
002600040910      *   D S   I N T E R N E / E S T E R N E
002700040910      *------------------------------------------------------------------------*
002800040910     d Azuteds       e ds                  extname(Azute00f)
002900040910     d dcmr          E DS                  INZ
003000040910     d dDatiute      e ds
003100040910     d dfar          E DS                  INZ
003200040910     d Kpjba         e ds
003300040910     d Tibs34ds      e ds
003400050830     d fnvapds       e ds                  extname(fnvap00f)
003500050920     D trul47ds      e ds
003600050830
003700050830     d wlbda8          ds                  inz
003800050830     d  g08dat                 1      8  0
003900050830     d  g08inv                 9     16  0
004000050830     d  g08err                17     17
004100050830     d  g08tgi                18     22  0
004200050830
004300050830     d wOGGI           s              8  0
004400050830     d Cli_Abil        s              1    inz(*off)
004500060126     d woriIDC         s                   like(oriIDC) inz(*blanks)
004600001108
004700040910      *------------------------------------------------------------------------*
004800040915
004900040915      * carica le tabelle utilizzate
005000040915     c                   ExSr      Cartab
005100011108
005200050920     C*
005300050920     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VP'
005400050920     C                   clear                   trul47ds
005500050920     C                   eval      d47opz  = 'I'
005600050920     C                   eval      d47tip  = 'VP'
005700050920     C                   eval      d47lck  = 'N'
005800050920     C                   eval      d47chkj = 'N'
005900050920     C                   eval      d47pgm  = 'FIOR31R'
006000050920     C                   call      'TRUL47R'
006100050920     C                   parm                    trul47ds
006200050920     C*
006300050920     C* Se elaborazione consentita => proseguo
006400050920     C                   if        d47sts <> 'A'
006500050920     C*
006600050830     c                   Do        *Hival
006700040910     c                   Read      Fnorf04l
006800040910     c                   If        %Eof(Fnorf04l)
006900040910     c                   Leave
007000040910     c                   EndIf
007100040910
007200040910      * controllo se deve scrivere il record
007300040910     c                   ExSr      CtrOrm
007400040920
007500040920      * Aggiorno la fase in ogni caso!!!
007600050831     c                   Z-add     wOGGI         OrfDfc
007700040920     c                   Update    Fnorf000
007800040910
007900050830     c                   EndDo
008000050920     C*
008100050920     C                   endif
008200050920     C*
008300050920     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VI'
008400050920     C                   clear                   trul47ds
008500050920     C                   eval      d47opz  = 'F'
008600050920     C                   eval      d47tip  = 'VP'
008700050920     C                   call      'TRUL47R'
008800050920     C                   parm                    trul47ds
008900050920     C*
009000040910
009100040910     c                   Eval      *InLr = *On
009200040910
009300040910      *------------------------------------------------------------------------*
009400040910      * CtrOrm - controlla se scrivere il rcd x ritorno fasi ORM ai clienti
009500040910      *------------------------------------------------------------------------*
009600040910     c     CtrOrm        BegSr
009700040910
009800040910      * reperisce i dati per la fase eseguita (SE abilitata)
009900040910     c                   Eval      xx = 1
010000040910     c                   Eval      *in10 = *off
010100040910     c     OrfFar        Lookup    cfar(xx)                               10
010200040910     c     *In10         CabEq     *Off          endctrorm
010300040910      *
010400050830      * controllo che non sia una prova ORFPOE = 789
010500050830     c                   If        orfpoe = 789
010600050830     c                   goto      endctrorm
010700050830     c                   EndIf
010800050830      *
010900040910     c                   Movel     far(xx)       dfar
011000050830      *
011100040910      * aggancio file tiori05l
011200040910     c     kOrm          Chain     Tiori05l
011300050830     c                   If        %Found(Tiori05l)
011400050831      *
011500050831      * controllo se il cliente è abilitato al servizio
011600050831     c                   exsr      sr_Control
011700050831     c                   if        Cli_Abil = *off
011800050831     c                   goto      endctrorm
011900050831     c                   endif
012000050831      *
012100050831      * compongo il buffer del file d output
012200040910     c                   ExSr      ScrVap
012300050830     c                   EndIf
012400040910
012500040910     c     endctrorm     EndSr
012600050830
012700050830
012800050830      *-------------------------------------------------------------------------*
012900050830      * sr_Control :  controllo dati del record
013000050830      *-------------------------------------------------------------------------*
013100050830     c     sr_Control    begsr
013200050929     c*
013300050929     c* inizializzo a ok il flag d controllo abilitazione
013400050929     c                   eval      Cli_Abil = *on
013500050830
013600060126      * controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
013700060127     c                   clear                   woriIDC
013800060126     c                   eval      TbeCod = 'VAP'
013900060126     c                   eval      TbeKe1 = oriIDC
014000060126     c     kTntbe1       chain     Tntbe01l
014100060126     c                   if        %found(Tntbe01l)
014200060126     c                   eval      woriIDC = TbeKe2
014300060126     c                   else
014400060126     c                   eval      Cli_Abil = *off
014500060126     c                   endif
014600050830
014700050830     c                   endsr
014800040910
014900040910      *------------------------------------------------------------------------*
015000040910      * ScrVap - scrive il record al file x ritorno fasi ORM ai clienti
015100040910      *------------------------------------------------------------------------*
015200040910     c     ScrVap        BegSr
015300040910
015400040910     c     kOrm          Chain     Fnorm01l
015500040910
015600050830     c                   Clear                   Fnvapds
015700040910
015800060126     c                   Movel     OriIdc        VapIdc
015900040910     c                   Z-add     OrfPoe        VapPoe
016000040910     c                   Z-add     OrfNsr        VapNsr
016100040910     c                   Z-add     OrfNor        VapNor
016200040910     c                   Z-add     OrfNrv        VapNrv
016300040910     c                   Movel     VaoRfa        VapRfa
016400040910     c                   Z-add     OrfPog        VapPog
016500050830     c                   If        OrfFar = 50
016600040910     c                   Z-add     OrmDao        VapDae
016700040910     c                   Z-add     OrmOao        VapOre
016800050830     c                   Else
016900040910     c                   Z-add     OrfDae        VapDae
017000040910     c                   Z-add     OrfOre        VapOre
017100050830     c                   EndIf
017200040910     c                   Z-add     OrfFar        VapFar
017300040910
017400050830     c                   If        d§FARdva <> *Blanks
017500040910     c                   Movel     d§FARdva      VapDfa
017600050830     c                   Else
017700040910     c                   Movel     d§FARdes      VapDfa
017800050830     c                   EndIf
017900050830
018000050830     c                   If        OrfCar <> *Blanks
018100040910     c                   Eval      xx = 1
018200040910     c                   Eval      *In10 = *Off
018300050830     c     OrfCar        lookup    ccmr(xx)                               10
018400050830
018500050830     c                   If        *In10
018600050830     c                   Movel     cmr(xx)       dcmr
018700050830
018800050830     c                   If        d§CMRcva <> *Blanks
018900050830     c                   Movel     d§CMRcva      VapCar
019000050830     c                   Else
019100050830     c                   Movel     OrfCar        VapCar
019200050830     c                   EndIf
019300050830
019400050830     c                   If        d§CMRdva <> *Blanks
019500050830     c                   Movel     d§CMRdva      VapDca
019600050830     c                   else
019700050830     c                   Movel     d§CMRdes1     VapDca
019800050830     c                   EndIf
019900050830
020000050830     c                   Else
020100050830
020200050830     c                   Movel     OrfCar        VapCar
020300050830     c                   EndIf
020400040910
020500050830     c                   EndIf
020600050830
020700050830      *
020800050830      * decodifico il P.O. di emissione
020900050830     c     VAPpoe        chain     AZORG01L
021000050830     c                   if        %found(azorg01l)
021100050830     c                   movel     ORGdes        VAPdpe
021200050830     c                   endif
021300050830      *
021400050830      * decodifico il P.O. di esecuzione fase
021500050830     c                   if        VAPpog <> VAPpoe
021600050830     c     VAPpog        chain     AZORG01L
021700050830     c                   endif
021800050830     c                   if        %found(azorg01l)
021900050830     c                   movel     ORGdes        VAPdpg
022000050830     c                   endif
022100040910
022200050830     C*
022300050830     C                   clear                   tivgd000
022400050830     C                   eval      vgdDTA = FNVAPDS
022500050830     C                   eval      vgdTIP = 'VP'
022600060126     C*                  eval      vgdKSU = vapIDC
022700060126     C                   eval      vgdKSU = woriIDC
022800060127     C                   eval      vgdTSC = 'WW'
022900050830     C                   eval      vgdDAT = wOGGI
023000050830     C                   eval      vgdPGM = 'FIOR31R'
023100050830     C                   write     tivgd000
023200050830     C*
023300040910
023400040910     c                   EndSr
023500040910
023600040910      *------------------------------------------------------------------------*
023700040910      * Cartab - carica le schiere delle tabelle utilizzate
023800040910      *------------------------------------------------------------------------*
023900040910     c     Cartab        BegSr
024000040910
024100040910      * memorizzazione dati tab. FAR (fasi avanzamento ORM)
024200040910      *  solo dei codici che prevedono di scrivere nel VAS x cliente
024300040910     c                   Reset                   cfar
024400040910     c                   Clear                   far
024500040910     c                   Clear                   xx
024600040910     c                   Eval      TbeCod = 'FAR'
024700040910     c                   Clear                   TbeKe1
024800050830     c                   Clear                   TbeKe2
024900050830     c     kTntbe        Setll     Tntbe01l
025000050830     c                   Do        *Hival
025100050830     c     TbeCod        Reade     Tntbe01l
025200050830     c                   If        %Eof(Tntbe01l)
025300050830     c                   Leave
025400050830     c                   EndIf
025500050830     c                   Eval      dfar = TbeUni
025600050830     c                   If        d§FARuva <> 'S'
025700050830     c                   Iter
025800050830     c                   EndIf
025900050830     c                   Add       1             xx
026000050830     c                   Movel     TbeKe1        cfar(xx)
026100040910     c                   Movel     TbeUni        far(xx)
026200050830     c                   EndDo
026300040910
026400040910      * memorizzazione dati tab. CMR (causali mancato ritiro)
026500040910     c                   Reset                   ccmr
026600040910     c                   Clear                   cmr
026700040910     c                   Clear                   xx
026800040910     c                   Eval      TbeCod = 'CMR'
026900040910     c                   Clear                   TbeKe1
027000040910     c                   Clear                   TbeKe2
027100040910     c     kTntbe        Setll     Tntbe01l
027200050830     c                   Do        *Hival
027300050830     c     TbeCod        Reade     Tntbe01l
027400050830     c                   If        %Eof(Tntbe01l)
027500050830     c                   Leave
027600050830     c                   EndIf
027700050830     c                   Add       1             xx
027800050830     c                   Movel     TbeKe1        ccmr(xx)
027900050830     c                   Movel     TbeUni        cmr(xx)
028000050830     c                   EndDo
028100050830
028200050830     c                   EndSr
028300001108
028400040910      *------------------------------------------------------------------------*
028500040910      * - ROUTINE INIZIALE
028600040910      *------------------------------------------------------------------------*
028700040910     c     *Inzsr        BegSr
028800040910
028900040910     c     *Entry        Plist
029000040910     c                   Parm                    Kpjba
029100040910
029200040910     c     *dtaara       define    §azute        azuteds
029300040910     c     *dtaara       define    §datiute      ddatiute
029400040910     c                   in(E)     *dtaara
029500040910     c                   If        %error  or RSUT = *blanks
029600040910     c                   Clear                   Tibs34ds
029700040910     c                   Call      'TIBS34R'
029800040910     c                   Parm                    Tibs34ds
029900040910     c                   In        *dtaara
030000040910     c                   EndIf
030100050830
030200050830      * Giro la data odierna
030300050830     c                   move      *date         g08dat
030400050830     c                   movel     *blanks       g08err
030500050830     c                   call      'XSRDA8'
030600050830     c                   parm                    wlbda8
030700050830     c                   if        g08err = '1'
030800050830     c                   z-add     0             wOGGI
030900050830     c                   else
031000050830     c                   z-add     g08inv        wOGGI
031100050830     c                   endif
031200040910
031300040910      * chiavi
031400040910
031500040910     c     kTntbe        klist
031600040910     c                   kfld                    TbeCod
031700040910     c                   kfld                    TbeKe1
031800040910     c                   kfld                    TbeKe2
031900040910
032000060126     c     kTntbe1       klist
032100060126     c                   kfld                    TbeCod
032200060126     c                   kfld                    TbeKe1
032300060126
032400040910     c     kOrm          klist
032500040910     c                   kfld                    OrfPoe
032600040910     c                   kfld                    OrfNsr
032700040910     c                   kfld                    OrfNor
032800040910     c                   kfld                    OrfNrv
032900001108
033000040910     c                   EndSr
