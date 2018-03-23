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
007200170324     C                   eval      %subst(o§7VASPRG:1:2) = 'XX'
007300050920     C*
007400170323     C* Elaborazione dati
007500050830     c                   Do        *Hival
007600040910     c                   Read      Fnorf04l
007700040910     c                   If        %Eof(Fnorf04l)
007800040910     c                   Leave
007900040910     c                   EndIf
008000040910
008100040910      * controllo se deve scrivere il record
008200040910     c                   ExSr      CtrOrm
008300040920
008400040920      * Aggiorno la fase in ogni caso!!!
008500050831     c                   Z-add     wOGGI         OrfDfc
008600040920     c                   Update    Fnorf000
008700040910
008800050830     c                   EndDo
008900170323     C*
009000170323     C* Finalizzo la transazione VAG
009100170323     C                   exsr      RLTPRG
009200170323     C*
009300170323     C* Chiudo il file download
009400170323     C                   if        %open(fnorf04l)
009500170323     C                   close     fnorf04l
009600170323     C                   endif
009700170323     C                   if        %open(tivgd00f)
009800170323     C                   close     tivgd00f
009900170323     C                   endif
010000170323     C*
010100170323     C* Fine controllo sincronia
010200170323     C                   EVAL      i§7VASOPZ = 'ECM'
010300170323     C                   CALL(e)   'TIS7VASR1'
010400170323     C                   PARM                    TIS7VASDS
010500170323     C*
010600170323     C* Verifico esito chiamata
010700170323     C                   if        %error OR o§7VASOK = *off
010800170323     C                   exsr      EXEERR
010900170323     C                   endif
011000170323     C*
011100170323     C* Su errore
011200170323     C                   on-error
011300170323     C                   exsr      exeerr
011400170323     C*
011500170323     C* Fine monitoring
011600170323     C                   endmon
011700050920     C*
011800040910     c                   Eval      *InLr = *On
011900170323
012000170323
012100040910
012200040910      *------------------------------------------------------------------------*
012300040910      * CtrOrm - controlla se scrivere il rcd x ritorno fasi ORM ai clienti
012400040910      *------------------------------------------------------------------------*
012500040910     c     CtrOrm        BegSr
012600040910
012700040910      * reperisce i dati per la fase eseguita (SE abilitata)
012800040910     c                   Eval      xx = 1
012900040910     c                   Eval      *in10 = *off
013000040910     c     OrfFar        Lookup    cfar(xx)                               10
013100040910     c     *In10         CabEq     *Off          endctrorm
013200040910      *
013300050830      * controllo che non sia una prova ORFPOE = 789
013400050830     c                   If        orfpoe = 789
013500050830     c                   goto      endctrorm
013600050830     c                   EndIf
013700050830      *
013800040910     c                   Movel     far(xx)       dfar
013900050830      *
014000040910      * aggancio file tiori05l
014100040910     c     kOrm          Chain     Tiori05l
014200050830     c                   If        %Found(Tiori05l)
014300050831      *
014400050831      * controllo se il cliente è abilitato al servizio
014500050831     c                   exsr      sr_Control
014600050831     c                   if        Cli_Abil = *off
014700050831     c                   goto      endctrorm
014800050831     c                   endif
014900050831      *
015000050831      * compongo il buffer del file d output
015100040910     c                   ExSr      ScrVap
015200050830     c                   EndIf
015300040910
015400040910     c     endctrorm     EndSr
015500050830
015600050830
015700050830      *-------------------------------------------------------------------------*
015800050830      * sr_Control :  controllo dati del record
015900050830      *-------------------------------------------------------------------------*
016000050830     c     sr_Control    begsr
016100050929     c*
016200050929     c* inizializzo a ok il flag d controllo abilitazione
016300050929     c                   eval      Cli_Abil = *on
016400050830
016500060126      * controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
016600060127     c                   clear                   woriIDC
016700060126     c                   eval      TbeCod = 'VAP'
016800060126     c                   eval      TbeKe1 = oriIDC
016900060126     c     kTntbe1       chain     Tntbe01l
017000060126     c                   if        %found(Tntbe01l)
017100060126     c                   eval      woriIDC = TbeKe2
017200060126     c                   else
017300060126     c                   eval      Cli_Abil = *off
017400060126     c                   endif
017500050830
017600050830     c                   endsr
017700040910
017800040910      *------------------------------------------------------------------------*
017900040910      * ScrVap - scrive il record al file x ritorno fasi ORM ai clienti
018000040910      *------------------------------------------------------------------------*
018100040910     c     ScrVap        BegSr
018200040910
018300040910     c     kOrm          Chain     Fnorm01l
018400040910
018500050830     c                   Clear                   Fnvapds
018600040910
018700060126     c                   Movel     OriIdc        VapIdc
018800040910     c                   Z-add     OrfPoe        VapPoe
018900040910     c                   Z-add     OrfNsr        VapNsr
019000040910     c                   Z-add     OrfNor        VapNor
019100040910     c                   Z-add     OrfNrv        VapNrv
019200080918      * se ORM RC non devo passare il riferimento originale del cliente ma il riferimento
019300080918      * dell'ORM che è la bolla legata
019400080918     c                   if        orfnsr > *zeros
019500080918     c                   eval      vaprfa = ormrfa
019600080918     c                   else
019700040910     c                   Movel     VaoRfa        VapRfa
019800080918     c                   endif
019900040910     c                   Z-add     OrfPog        VapPog
020000050830     c                   If        OrfFar = 50
020100040910     c                   Z-add     OrmDao        VapDae
020200040910     c                   Z-add     OrmOao        VapOre
020300050830     c                   Else
020400040910     c                   Z-add     OrfDae        VapDae
020500040910     c                   Z-add     OrfOre        VapOre
020600050830     c                   EndIf
020700040910     c                   Z-add     OrfFar        VapFar
020800040910
020900050830     c                   If        d§FARdva <> *Blanks
021000040910     c                   Movel     d§FARdva      VapDfa
021100050830     c                   Else
021200040910     c                   Movel     d§FARdes      VapDfa
021300050830     c                   EndIf
021400050830
021500050830     c                   If        OrfCar <> *Blanks
021600040910     c                   Eval      xx = 1
021700040910     c                   Eval      *In10 = *Off
021800050830     c     OrfCar        lookup    ccmr(xx)                               10
021900050830
022000050830     c                   If        *In10
022100050830     c                   Movel     cmr(xx)       dcmr
022200050830
022300050830     c                   If        d§CMRcva <> *Blanks
022400050830     c                   Movel     d§CMRcva      VapCar
022500050830     c                   Else
022600050830     c                   Movel     OrfCar        VapCar
022700050830     c                   EndIf
022800050830
022900050830     c                   If        d§CMRdva <> *Blanks
023000050830     c                   Movel     d§CMRdva      VapDca
023100050830     c                   else
023200050830     c                   Movel     d§CMRdes1     VapDca
023300050830     c                   EndIf
023400050830
023500050830     c                   Else
023600050830
023700050830     c                   Movel     OrfCar        VapCar
023800050830     c                   EndIf
023900040910
024000050830     c                   EndIf
024100050830
024200050830      *
024300050830      * decodifico il P.O. di emissione
024400050830     c     VAPpoe        chain     AZORG01L
024500050830     c                   if        %found(azorg01l)
024600050830     c                   movel     ORGdes        VAPdpe
024700050830     c                   endif
024800050830      *
024900050830      * decodifico il P.O. di esecuzione fase
025000050830     c                   if        VAPpog <> VAPpoe
025100050830     c     VAPpog        chain     AZORG01L
025200050830     c                   endif
025300050830     c                   if        %found(azorg01l)
025400050830     c                   movel     ORGdes        VAPdpg
025500050830     c                   endif
025600040910
025700050830     C*
025800050830     C                   clear                   tivgd000
025900080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
026000050830     C                   eval      vgdTIP = 'VP'
026100170323     C                   eval      vgdKSU = woriIDC
026200060127     C                   eval      vgdTSC = 'WW'
026300050830     C                   eval      vgdDAT = wOGGI
026400170323     C                   eval      vgdPRG = o§7VASPRG
026500170323     C                   eval      vgdPGM = procname
026600170323     C                   eval      vgdSTO = '?'
026700050830     C                   write     tivgd000
026800050830     C*
026900040910
027000040910     c                   EndSr
027100040910
027200040910      *------------------------------------------------------------------------*
027300040910      * Cartab - carica le schiere delle tabelle utilizzate
027400040910      *------------------------------------------------------------------------*
027500040910     c     Cartab        BegSr
027600040910
027700040910      * memorizzazione dati tab. FAR (fasi avanzamento ORM)
027800040910      *  solo dei codici che prevedono di scrivere nel VAS x cliente
027900040910     c                   Reset                   cfar
028000040910     c                   Clear                   far
028100040910     c                   Clear                   xx
028200040910     c                   Eval      TbeCod = 'FAR'
028300040910     c                   Clear                   TbeKe1
028400050830     c                   Clear                   TbeKe2
028500050830     c     kTntbe        Setll     Tntbe01l
028600050830     c                   Do        *Hival
028700050830     c     TbeCod        Reade     Tntbe01l
028800050830     c                   If        %Eof(Tntbe01l)
028900050830     c                   Leave
029000050830     c                   EndIf
029100050830     c                   Eval      dfar = TbeUni
029200050830     c                   If        d§FARuva <> 'S'
029300050830     c                   Iter
029400050830     c                   EndIf
029500050830     c                   Add       1             xx
029600050830     c                   Movel     TbeKe1        cfar(xx)
029700040910     c                   Movel     TbeUni        far(xx)
029800050830     c                   EndDo
029900040910
030000040910      * memorizzazione dati tab. CMR (causali mancato ritiro)
030100040910     c                   Reset                   ccmr
030200040910     c                   Clear                   cmr
030300040910     c                   Clear                   xx
030400040910     c                   Eval      TbeCod = 'CMR'
030500040910     c                   Clear                   TbeKe1
030600040910     c                   Clear                   TbeKe2
030700040910     c     kTntbe        Setll     Tntbe01l
030800050830     c                   Do        *Hival
030900050830     c     TbeCod        Reade     Tntbe01l
031000050830     c                   If        %Eof(Tntbe01l)
031100050830     c                   Leave
031200050830     c                   EndIf
031300050830     c                   Add       1             xx
031400050830     c                   Movel     TbeKe1        ccmr(xx)
031500050830     c                   Movel     TbeUni        cmr(xx)
031600050830     c                   EndDo
031700050830
031800050830     c                   EndSr
031900170323
032000170323
032100170323
032200170323     C*------------------------------------------------------------------------*
032300170323     C* EXEERR - Routine di esecuzione azioni su Errore
032400170323     C*------------------------------------------------------------------------*
032500170323     C     EXEERR        BEGSR
032600170323     C*
032700170323     C                   dump(A)
032800170323     C                   rolbk(e)
032900170323     C                   seton                                        lr
033000170323     C                   return
033100170323     C*
033200170323     C                   ENDSR
033300170323     C*------------------------------------------------------------------------*
033400170323
033500170323
033600170323
033700170323     C*------------------------------------------------------------------------*
033800170323     C* RTVPRG - Routine di reperimento progressivo univoco transazione
033900170323     C*------------------------------------------------------------------------*
034000170323     C     RTVPRG        BEGSR
034100170324     C*
034200170324     c                   CLEAR                   TIS7VASDS
034300170324     c                   EVAL      i§7VASOPZ = 'PRG'
034400170324     c                   CALL(e)   'TIS7VASR1'
034500170324     c                   PARM                    TIS7VASDS
034600170324     c*
034700170324     c                   if        %error OR
034800170324     c                             o§7VASOK = *off OR
034900170324     c                             o§7VASPRG = *blanks
035000170324     c                   exsr      EXEERR
035100170324     c                   endif
035200170323     C*
035300170323     C                   ENDSR
035400170323     C*------------------------------------------------------------------------*
035500170323
035600170323
035700170323
035800170323     C*------------------------------------------------------------------------*
035900170323     C* RLTPRG - Routine di finalizzazione transazione (solo TIP)
036000170323     C*------------------------------------------------------------------------*
036100170323     C     RLTPRG        BEGSR
036200170323     C*
036300170323     C                   EVAL      i§7VASOPZ  = 'RLT'
036400170323     C                   EVAL      i§7VASTIP  = 'VP'
036500170323     C                   EVAL      i§7VASSTO  = '?'
036600170323     C                   EVAL      i§7VASSTTO = 'G'
036700170323     C                   EVAL      i§7VASPRG  = o§7VASPRG
036800170323     C                   CALL(e)   'TIS7VASR1'
036900170323     C                   PARM                    TIS7VASDS
037000170323     C*
037100170323     C* Verifico esito chiamata
037200170323     C                   if        %error OR o§7VASOK = *off
037300170323     C                   exsr      EXEERR
037400170323     C                   endif
037500170323     C*
037600170323     C                   ENDSR
037700170323     C*------------------------------------------------------------------------*
037800170323
037900170323
038000001108
038100040910      *------------------------------------------------------------------------*
038200040910      * - ROUTINE INIZIALE
038300040910      *------------------------------------------------------------------------*
038400040910     c     *Inzsr        BegSr
038500040910
038600040910     c     *Entry        Plist
038700040910     c                   Parm                    Kpjba
038800040910
038900040910     c     *dtaara       define    §azute        azuteds
039000040910     c     *dtaara       define    §datiute      ddatiute
039100040910     c                   in(E)     *dtaara
039200040910     c                   If        %error  or RSUT = *blanks
039300040910     c                   Clear                   Tibs34ds
039400040910     c                   Call      'TIBS34R'
039500040910     c                   Parm                    Tibs34ds
039600040910     c                   In        *dtaara
039700040910     c                   EndIf
039800050830
039900050830      * Giro la data odierna
040000170323     c                   z-add     *zeros        wOGGI             8 0
040100170323     c                   eval      wOGGI = %dec(%date() : *iso)
040200040910
040300040910      * chiavi
040400040910
040500040910     c     kTntbe        klist
040600040910     c                   kfld                    TbeCod
040700040910     c                   kfld                    TbeKe1
040800040910     c                   kfld                    TbeKe2
040900040910
041000060126     c     kTntbe1       klist
041100060126     c                   kfld                    TbeCod
041200060126     c                   kfld                    TbeKe1
041300060126
041400040910     c     kOrm          klist
041500040910     c                   kfld                    OrfPoe
041600040910     c                   kfld                    OrfNsr
041700040910     c                   kfld                    OrfNor
041800040910     c                   kfld                    OrfNrv
041900001108
042000040910     c                   EndSr
