000100170323     H DECEDIT('0,') DATEDIT(*DMY.)
000200170323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300040910      *------------------------------------------------------------------------*
000400050830      * Scrittura file FNVAP in TIVGD00F x le fasi ancora da inviare al cliente
000500040910      *------------------------------------------------------------------------*
000600170323     fFnorf04l  uf   e           k disk    usropn commit
000700040910     fFnorm01l  if   e           k disk
000800040910     fTntbe01l  if   e           k disk
000900040910     fTiori05l  if   e           k disk
001000170323     ftivgd00f  o    e             disk    usropn commit
001100050830     fazorg01l  if   e           k disk
001200040910
001300040910      *------------------------------------------------------------------------*
001400040910      *   S C H I E R E
001500040910      *------------------------------------------------------------------------*
001600040910     d cfar            S                   like(ORFfar) dim(99) inz(*loval)     *cod. fasi ORM
001700040910     d far             S                   like(DFAR)     dim(99) inz           *dati fasi ORM
001800040910     d ccmr            S                   like(ORFcar) dim(99) inz(*hival)     *cod. causali ORM
001900040910     d cmr             S                   like(DCMR)     dim(99) inz           *dati causali ORM
002000040910
002100040910      *------------------------------------------------------------------------*
002200040910      *   V A R I A B I L I
002300040910      *------------------------------------------------------------------------*
002400040910     d xx              S              3  0 INZ
002500001108
002600040910      *------------------------------------------------------------------------*
002700040910      *   D S   I N T E R N E / E S T E R N E
002800040910      *------------------------------------------------------------------------*
002900040910     d Azuteds       e ds                  extname(Azute00f)
003000040910     d dcmr          E DS                  INZ
003100040910     d dDatiute      e ds
003200040910     d dfar          E DS                  INZ
003300040910     d Kpjba         e ds
003400040910     d Tibs34ds      e ds
003500050830     d fnvapds       e ds                  extname(fnvap00f)
003600170323     D psds           sds
003700170323     D  procname         *PROC
003800170323     D TIS7VASDS     e ds
003900050830
004000050830     d Cli_Abil        s              1    inz(*off)
004100060126     d woriIDC         s                   like(oriIDC) inz(*blanks)
004200001108
004300040910      *------------------------------------------------------------------------*
004400040915
004500040915      * carica le tabelle utilizzate
004600040915     c                   ExSr      Cartab
004700170323     C*
004800170323     C* Avvio il monitoring del processo
004900170323     C                   monitor
005000170323     C*
005100170323     C* Avvio il controllo sincronia
005200170323     C                   CLEAR                   TIS7VASDS
005300170323     C                   EVAL      i§7VASOPZ = 'SCM'
005400170323     C                   CALL(e)   'TIS7VASR1'
005500170323     C                   PARM                    TIS7VASDS
005600170323     C*
005700170323     C* Verifico esito chiamata
005800170323     C                   if        %error OR o§7VASOK = *off
005900170323     C                   exsr      EXEERR
006000170323     C                   endif
006100170323     C*
006200170323     C* Se tutto OK apro i file in commit
006300170323     C                   if        not %open(fnorf04l)
006400170323     C                   open      fnorf04l
006500170323     C                   endif
006600170323     C                   if        not %open(tivgd00f)
006700170323     C                   open      tivgd00f
006800170323     C                   endif
006900170323     C*
007000170323     C* Stacco progressivo univoco download per tutti i VAP
007100170323     C                   exsr      RTVPRG
007200050920     C*
007300170323     C* Elaborazione dati
007400050830     c                   Do        *Hival
007500040910     c                   Read      Fnorf04l
007600040910     c                   If        %Eof(Fnorf04l)
007700040910     c                   Leave
007800040910     c                   EndIf
007900040910
008000040910      * controllo se deve scrivere il record
008100040910     c                   ExSr      CtrOrm
008200040920
008300040920      * Aggiorno la fase in ogni caso!!!
008400050831     c                   Z-add     wOGGI         OrfDfc
008500040920     c                   Update    Fnorf000
008600040910
008700050830     c                   EndDo
008800170323     C*
008900170323     C* Finalizzo la transazione VAG
009000170323     C                   exsr      RLTPRG
009100170323     C*
009200170323     C* Chiudo il file download
009300170323     C                   if        %open(fnorf04l)
009400170323     C                   close     fnorf04l
009500170323     C                   endif
009600170323     C                   if        %open(tivgd00f)
009700170323     C                   close     tivgd00f
009800170323     C                   endif
009900170323     C*
010000170323     C* Fine controllo sincronia
010100170323     C                   EVAL      i§7VASOPZ = 'ECM'
010200170323     C                   CALL(e)   'TIS7VASR1'
010300170323     C                   PARM                    TIS7VASDS
010400170323     C*
010500170323     C* Verifico esito chiamata
010600170323     C                   if        %error OR o§7VASOK = *off
010700170323     C                   exsr      EXEERR
010800170323     C                   endif
010900170323     C*
011000170323     C* Su errore
011100170323     C                   on-error
011200170323     C                   exsr      exeerr
011300170323     C*
011400170323     C* Fine monitoring
011500170323     C                   endmon
011600050920     C*
011700040910     c                   Eval      *InLr = *On
011800170323
011900170323
012000040910
012100040910      *------------------------------------------------------------------------*
012200040910      * CtrOrm - controlla se scrivere il rcd x ritorno fasi ORM ai clienti
012300040910      *------------------------------------------------------------------------*
012400040910     c     CtrOrm        BegSr
012500040910
012600040910      * reperisce i dati per la fase eseguita (SE abilitata)
012700040910     c                   Eval      xx = 1
012800040910     c                   Eval      *in10 = *off
012900040910     c     OrfFar        Lookup    cfar(xx)                               10
013000040910     c     *In10         CabEq     *Off          endctrorm
013100040910      *
013200050830      * controllo che non sia una prova ORFPOE = 789
013300050830     c                   If        orfpoe = 789
013400050830     c                   goto      endctrorm
013500050830     c                   EndIf
013600050830      *
013700040910     c                   Movel     far(xx)       dfar
013800050830      *
013900040910      * aggancio file tiori05l
014000040910     c     kOrm          Chain     Tiori05l
014100050830     c                   If        %Found(Tiori05l)
014200050831      *
014300050831      * controllo se il cliente è abilitato al servizio
014400050831     c                   exsr      sr_Control
014500050831     c                   if        Cli_Abil = *off
014600050831     c                   goto      endctrorm
014700050831     c                   endif
014800050831      *
014900050831      * compongo il buffer del file d output
015000040910     c                   ExSr      ScrVap
015100050830     c                   EndIf
015200040910
015300040910     c     endctrorm     EndSr
015400050830
015500050830
015600050830      *-------------------------------------------------------------------------*
015700050830      * sr_Control :  controllo dati del record
015800050830      *-------------------------------------------------------------------------*
015900050830     c     sr_Control    begsr
016000050929     c*
016100050929     c* inizializzo a ok il flag d controllo abilitazione
016200050929     c                   eval      Cli_Abil = *on
016300050830
016400060126      * controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
016500060127     c                   clear                   woriIDC
016600060126     c                   eval      TbeCod = 'VAP'
016700060126     c                   eval      TbeKe1 = oriIDC
016800060126     c     kTntbe1       chain     Tntbe01l
016900060126     c                   if        %found(Tntbe01l)
017000060126     c                   eval      woriIDC = TbeKe2
017100060126     c                   else
017200060126     c                   eval      Cli_Abil = *off
017300060126     c                   endif
017400050830
017500050830     c                   endsr
017600040910
017700040910      *------------------------------------------------------------------------*
017800040910      * ScrVap - scrive il record al file x ritorno fasi ORM ai clienti
017900040910      *------------------------------------------------------------------------*
018000040910     c     ScrVap        BegSr
018100040910
018200040910     c     kOrm          Chain     Fnorm01l
018300040910
018400050830     c                   Clear                   Fnvapds
018500040910
018600060126     c                   Movel     OriIdc        VapIdc
018700040910     c                   Z-add     OrfPoe        VapPoe
018800040910     c                   Z-add     OrfNsr        VapNsr
018900040910     c                   Z-add     OrfNor        VapNor
019000040910     c                   Z-add     OrfNrv        VapNrv
019100080918      * se ORM RC non devo passare il riferimento originale del cliente ma il riferimento
019200080918      * dell'ORM che è la bolla legata
019300080918     c                   if        orfnsr > *zeros
019400080918     c                   eval      vaprfa = ormrfa
019500080918     c                   else
019600040910     c                   Movel     VaoRfa        VapRfa
019700080918     c                   endif
019800040910     c                   Z-add     OrfPog        VapPog
019900050830     c                   If        OrfFar = 50
020000040910     c                   Z-add     OrmDao        VapDae
020100040910     c                   Z-add     OrmOao        VapOre
020200050830     c                   Else
020300040910     c                   Z-add     OrfDae        VapDae
020400040910     c                   Z-add     OrfOre        VapOre
020500050830     c                   EndIf
020600040910     c                   Z-add     OrfFar        VapFar
020700040910
020800050830     c                   If        d§FARdva <> *Blanks
020900040910     c                   Movel     d§FARdva      VapDfa
021000050830     c                   Else
021100040910     c                   Movel     d§FARdes      VapDfa
021200050830     c                   EndIf
021300050830
021400050830     c                   If        OrfCar <> *Blanks
021500040910     c                   Eval      xx = 1
021600040910     c                   Eval      *In10 = *Off
021700050830     c     OrfCar        lookup    ccmr(xx)                               10
021800050830
021900050830     c                   If        *In10
022000050830     c                   Movel     cmr(xx)       dcmr
022100050830
022200050830     c                   If        d§CMRcva <> *Blanks
022300050830     c                   Movel     d§CMRcva      VapCar
022400050830     c                   Else
022500050830     c                   Movel     OrfCar        VapCar
022600050830     c                   EndIf
022700050830
022800050830     c                   If        d§CMRdva <> *Blanks
022900050830     c                   Movel     d§CMRdva      VapDca
023000050830     c                   else
023100050830     c                   Movel     d§CMRdes1     VapDca
023200050830     c                   EndIf
023300050830
023400050830     c                   Else
023500050830
023600050830     c                   Movel     OrfCar        VapCar
023700050830     c                   EndIf
023800040910
023900050830     c                   EndIf
024000050830
024100050830      *
024200050830      * decodifico il P.O. di emissione
024300050830     c     VAPpoe        chain     AZORG01L
024400050830     c                   if        %found(azorg01l)
024500050830     c                   movel     ORGdes        VAPdpe
024600050830     c                   endif
024700050830      *
024800050830      * decodifico il P.O. di esecuzione fase
024900050830     c                   if        VAPpog <> VAPpoe
025000050830     c     VAPpog        chain     AZORG01L
025100050830     c                   endif
025200050830     c                   if        %found(azorg01l)
025300050830     c                   movel     ORGdes        VAPdpg
025400050830     c                   endif
025500040910
025600050830     C*
025700050830     C                   clear                   tivgd000
025800080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
025900050830     C                   eval      vgdTIP = 'VP'
026000170323     C                   eval      vgdKSU = woriIDC
026100060127     C                   eval      vgdTSC = 'WW'
026200050830     C                   eval      vgdDAT = wOGGI
026300170323     C                   eval      vgdPRG = o§7VASPRG
026400170323     C                   eval      vgdPGM = procname
026500170323     C                   eval      vgdSTO = '?'
026600050830     C                   write     tivgd000
026700050830     C*
026800040910
026900040910     c                   EndSr
027000040910
027100040910      *------------------------------------------------------------------------*
027200040910      * Cartab - carica le schiere delle tabelle utilizzate
027300040910      *------------------------------------------------------------------------*
027400040910     c     Cartab        BegSr
027500040910
027600040910      * memorizzazione dati tab. FAR (fasi avanzamento ORM)
027700040910      *  solo dei codici che prevedono di scrivere nel VAS x cliente
027800040910     c                   Reset                   cfar
027900040910     c                   Clear                   far
028000040910     c                   Clear                   xx
028100040910     c                   Eval      TbeCod = 'FAR'
028200040910     c                   Clear                   TbeKe1
028300050830     c                   Clear                   TbeKe2
028400050830     c     kTntbe        Setll     Tntbe01l
028500050830     c                   Do        *Hival
028600050830     c     TbeCod        Reade     Tntbe01l
028700050830     c                   If        %Eof(Tntbe01l)
028800050830     c                   Leave
028900050830     c                   EndIf
029000050830     c                   Eval      dfar = TbeUni
029100050830     c                   If        d§FARuva <> 'S'
029200050830     c                   Iter
029300050830     c                   EndIf
029400050830     c                   Add       1             xx
029500050830     c                   Movel     TbeKe1        cfar(xx)
029600040910     c                   Movel     TbeUni        far(xx)
029700050830     c                   EndDo
029800040910
029900040910      * memorizzazione dati tab. CMR (causali mancato ritiro)
030000040910     c                   Reset                   ccmr
030100040910     c                   Clear                   cmr
030200040910     c                   Clear                   xx
030300040910     c                   Eval      TbeCod = 'CMR'
030400040910     c                   Clear                   TbeKe1
030500040910     c                   Clear                   TbeKe2
030600040910     c     kTntbe        Setll     Tntbe01l
030700050830     c                   Do        *Hival
030800050830     c     TbeCod        Reade     Tntbe01l
030900050830     c                   If        %Eof(Tntbe01l)
031000050830     c                   Leave
031100050830     c                   EndIf
031200050830     c                   Add       1             xx
031300050830     c                   Movel     TbeKe1        ccmr(xx)
031400050830     c                   Movel     TbeUni        cmr(xx)
031500050830     c                   EndDo
031600050830
031700050830     c                   EndSr
031800170323
031900170323
032000170323
032100170323     C*------------------------------------------------------------------------*
032200170323     C* EXEERR - Routine di esecuzione azioni su Errore
032300170323     C*------------------------------------------------------------------------*
032400170323     C     EXEERR        BEGSR
032500170323     C*
032600170323     C                   dump(A)
032700170323     C                   rolbk(e)
032800170323     C                   seton                                        lr
032900170323     C                   return
033000170323     C*
033100170323     C                   ENDSR
033200170323     C*------------------------------------------------------------------------*
033300170323
033400170323
033500170323
033600170323     C*------------------------------------------------------------------------*
033700170323     C* RTVPRG - Routine di reperimento progressivo univoco transazione
033800170323     C*------------------------------------------------------------------------*
033900170323     C     RTVPRG        BEGSR
034000170323     C*
034100170323     C* Forzo come valore Progressivo univoco un valore particolare che
034200170323     C* che forzerà il "ricalcolo"
034300170323     C                   eval      o§7VASPRG = *all'0'
034400170323     C*
034500170323     C                   ENDSR
034600170323     C*------------------------------------------------------------------------*
034700170323
034800170323
034900170323
035000170323     C*------------------------------------------------------------------------*
035100170323     C* RLTPRG - Routine di finalizzazione transazione (solo TIP)
035200170323     C*------------------------------------------------------------------------*
035300170323     C     RLTPRG        BEGSR
035400170323     C*
035500170323     C                   EVAL      i§7VASOPZ  = 'RLT'
035600170323     C                   EVAL      i§7VASTIP  = 'VP'
035700170323     C                   EVAL      i§7VASSTO  = '?'
035800170323     C                   EVAL      i§7VASSTTO = 'G'
035900170323     C                   EVAL      i§7VASPRG  = o§7VASPRG
036000170323     C                   CALL(e)   'TIS7VASR1'
036100170323     C                   PARM                    TIS7VASDS
036200170323     C*
036300170323     C* Verifico esito chiamata
036400170323     C                   if        %error OR o§7VASOK = *off
036500170323     C                   exsr      EXEERR
036600170323     C                   endif
036700170323     C*
036800170323     C                   ENDSR
036900170323     C*------------------------------------------------------------------------*
037000170323
037100170323
037200001108
037300040910      *------------------------------------------------------------------------*
037400040910      * - ROUTINE INIZIALE
037500040910      *------------------------------------------------------------------------*
037600040910     c     *Inzsr        BegSr
037700040910
037800040910     c     *Entry        Plist
037900040910     c                   Parm                    Kpjba
038000040910
038100040910     c     *dtaara       define    §azute        azuteds
038200040910     c     *dtaara       define    §datiute      ddatiute
038300040910     c                   in(E)     *dtaara
038400040910     c                   If        %error  or RSUT = *blanks
038500040910     c                   Clear                   Tibs34ds
038600040910     c                   Call      'TIBS34R'
038700040910     c                   Parm                    Tibs34ds
038800040910     c                   In        *dtaara
038900040910     c                   EndIf
039000050830
039100050830      * Giro la data odierna
039200170323     c                   z-add     *zeros        wOGGI             8 0
039300170323     c                   eval      wOGGI = %dec(%date() : *iso)
039400040910
039500040910      * chiavi
039600040910
039700040910     c     kTntbe        klist
039800040910     c                   kfld                    TbeCod
039900040910     c                   kfld                    TbeKe1
040000040910     c                   kfld                    TbeKe2
040100040910
040200060126     c     kTntbe1       klist
040300060126     c                   kfld                    TbeCod
040400060126     c                   kfld                    TbeKe1
040500060126
040600040910     c     kOrm          klist
040700040910     c                   kfld                    OrfPoe
040800040910     c                   kfld                    OrfNsr
040900040910     c                   kfld                    OrfNor
041000040910     c                   kfld                    OrfNrv
041100001108
041200040910     c                   EndSr
