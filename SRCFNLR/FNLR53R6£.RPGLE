000100080421     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200930210     H DECEDIT('0,') DATEDIT(*DMY.)
000300941111      * FNLR53R1 *---------------------------------------------------*
000400930210      *----------*                                                   *
000500080421      *     - Autogenerazione quadratura PDA (forza scrittura CRI)   *
000600930210      *--------------------------------------------------------------*
000700080421     FFNLR53D6  CF   E             WORKSTN
000800080421     Ffidst09l  iF   E           K DISK
000900080422     Ffnarb70l  iF   E           K DISK
001000080422     Ffnarb01l  iF   E           K DISK    rename(fnarb000:fnarb001)
001100080422     f                                     prefix(£)
001200080430     Ftabel00f  iF   E           K DISK
001300080430     Ffnlbl01l  iF   E           K DISK
001400080422     Ffiplg01l  iF   E           K DISK
001500080422     Ffnart01l  iF   E           K DISK
001600080422     Ffnbrv02l  iF   E           K DISK
001700080421     Ffipct02l  iF a E           K DISK    commit
001800080421     Ffipcd03l  iF a E           K DISK    commit
001900080421     Ffidsf02l  iF a E           K DISK    commit
002000911028      *
002100020822     D ddatiute      e ds
002200020822     D azuteds       e ds                  extname(AZUTE00F)
002300020822     D tibs34ds      E DS                  inz
002400080421     D fnlr53ds6     E DS                  inz
002500080422     D fipcdcedds    E DS                  inz
002600080422     D fipctcetds    E DS                  inz
002700080725     D fidsfcrids    E DS                  inz
002800950224     D KPJBA         E DS
002900080421     D WLBDA8          DS
003000080421     D  G02DAT                 1      8  0
003100080421     D  G02INV                 9     16  0
003200080421     D  G02ERR                17     17
003300080421     D  G02TGI                18     22  0
003400080428     D DS3E          E DS
003500080428     D COA             S              1    DIM(100)                             ANOMAL.MANC.CONS.
003600080430     D CMB             S              3    DIM(100)                             CODICI MANC.CONS.
003700080428     D PMC             S              2    DIM(100)                             PRIORITA COD.M.C.
003800941229      *
003900080421     C* se richiamato ricevo i rif. della distinta e quindi ometto l'emis-
004000080421     c* sione del video
004100080422     c                   if        kpjbu = ' '
004200080421     c                   do        *hival
004300941111     C                   EXFMT     LR53D01
004400080421     C   KL              leave
004500080421     c                   exsr      srctr
004600080421     c   90              iter
004700080422     c                   if        *inkf
004800080422     c                   exsr      aggio
004900080422     c* commit solo se non richiamato
005000080421     c                   commit
005100080422     c                   clear                   vndt
005200080506     c                   clear                   msgv
005300080506     c                   setoff                                       90
005400080422     c                   end
005500080421     c                   enddo
005600080421     c                   else
005700080421     c                   exsr      srctr
005800080421     c                   if        *in90
005900080421     c                   eval      d53err = '1'
006000080421     c                   eval      d53msg = msgv
006100080421     c                   else
006200080421     c                   exsr      aggio
006300080422     c* commit solo se richiesto
006400080422     c                   if        d53cmt ='S'
006500080422     c                   commit
006600080422     c                   end
006700080421     c                   end
006800080421     c                   end
006900080421     c                   eval      kpjbu = fnlr53ds6
007000911028     C*
007100930210     C                   SETON                                        LR
007200080421     c**********************************************************************
007300080421     c     srctr         begsr
007400080421     c**********************************************************************
007500080422     c                   clear                   msgv
007600080421     c                   setoff                                       90
007700080421     c     kdst          chain     fidst09l
007800080421     c                   if        not %found(fidst09l)
007900080421     c                   seton                                        90
008000080421     c                   eval      msgv ='Distinta inesistente'
008100080421     c                   leavesr
008200080421     c                   end
008300080422     c* distinta già chiusa
008400080422     c                   if        DSTFCF <> ' '
008500080421     c                   seton                                        90
008600080422     c                   eval      msgv ='Distinta chiusa'
008700080421     c                   leavesr
008800080421     c                   end
008900080422     c* non testo il flag di test pda perchè non dovrei ricevere gli esiti
009000080422     c                   if        dstpda <> 'E' and dstpda <> 'C'
009100080422     c                   seton                                        90
009200080422     c                   eval      msgv ='Distinta non PDA'
009300080422     c                   leavesr
009400080422     c                   end
009500080422     c* verifico se passati i dati a PDA
009600080422     c                   eval      dsftrd = 'PDC'
009700080421     c     kdsf          setll     fidsf02l                               90
009800080422     c                   if        not *in90
009900080422     c                   seton                                        90
010000080422     c                   eval      msgv ='Distinta non passata a PDA'
010100080421     c                   leavesr
010200080421     c                   end
010300080422     c* verifico se già presente la fase CRI
010400080422     c                   eval      dsftrd = 'CRI'
010500080422     c     kdsf          setll     fidsf02l                               90
010600080422     c                   if        *in90
010700080422     c                   eval      msgv ='Esiste già la fase di quadratura'
010800080422     c                   leavesr
010900080422     c                   end
011000080422     c     kplg          setll     fiplg01l                               90
011100080422     c                   if        *in90
011200080422     c                   eval      msgv ='La fase di quadratura sta per essere-
011300080422     c                              elaborata'
011400080422     c                   leavesr
011500080422     c                   end
011600080421     c                   endsr
011700080421     c**********************************************************************
011800080421     c     aggio         begsr
011900080421     c**********************************************************************
012000080422     c                   move      vndt          arbndc
012100080422     c     karb70        setll     fnarb70l
012200080421     c                   do        *hival
012300080422     c     karb70        reade     fnarb70l
012400080421     c                   if        %eof(fnarb70l)
012500080421     c                   leave
012600080421     c                   end
012700080421     c* per ogni spedizione in distinta verifico se ho l'esito e nel caso
012800080421     c* non ci sia verifico l'eventuale spunta di rientro, se non c'è
012900080421     c* nemmeno quella vuol dire che la spedizione è stata consegnata
013000080421     c     kpct          setll     fipct02l                               01
013100080430     c* ho l'esito tengo buono quello
013200080421     c                   if        *in01
013300080421     c                   iter
013400080421     c                   end
013500080421     c                   clear                   fipctcetds
013600080428     c* elaboro il dettaglio colli solo se non è un ritorno all'incasso
013700080428     c                   if        arbdcm = 0
013800080422     c                   eval      artaas = arbaas
013900080422     c                   eval      artlnp = arblnp
014000080422     c                   eval      artnrs = arbnrs
014100080422     c                   eval      artnsp = arbnsp
014200080422     C* Controllo se bolla con mamma ....
014300080422     C     karb          chain     fnlbl01l
014400080422     C                   IF        %found(fnlbl01l)
014500080422     C                             and  lbllan  =  lbllap
014600080422     C     klbl          chain     fnarb01l
014700080422     C* no reso (Riconsegna)
014800080422     C                   IF        %found(fnarb01l)
014900080422     C                             and arbcca <> '2' and arbcca <> '6'
015000080422     c                   eval      artaas = £arbaas
015100080422     c                   eval      artlnp = £arblnp
015200080422     c                   eval      artnrs = £arbnrs
015300080422     c                   eval      artnsp = £arbnsp
015400080422     c                   end
015500080422     c                   end
015600080422      * scrivo segnacolli drogati solo per PDA
015700080422      * con spedizione figlia e segnacolli mamma
015800080422      *ATTENZIONE questi rekord non hanno corrispondenza sul gestionale
015900080421     c* leggo il dettaglio segnacollo per la spedizione, verifico di non
016000080421     c* avere esiti, e se non ho nemmeno la spunta di rientro, scrivo il CED
016100080422     c     kart          setll     fnart01l
016200080421     c                   do        *hival
016300080422     c     kart          reade     fnart01l
016400080421     c                   if        %eof(fnart01l)
016500080421     c                   leave
016600080421     c                   end
016700080421     c                   if        artdcm <> 0
016800080421     c                   eval      §PCTCMC = 'P'
016900080421     c                   leave
017000080421     c                   end
017100080430     c* spunta 8
017200080430     c     kbrv          setll     fnbrv02l                               01
017300080430     c                   if        *in01
017400080430     c                   eval      §PCTCMC = 'P'
017500080430     c                   leave
017600080430     c                   end
017700080421     c* esito CED
017800080421     c     kpcd          setll     fipcd03l                               01
017900080421     c                   if        *in01
018000080422     c                   iter
018100080421     c                   end
018200080421     c                   enddo
018300080428     c                   end
018400080421     c* se esco dal loop dei colli e ho il flag di parziale allora scrivo
018500080423     c* il dettaglio segnacolli altrimenti do per consegnata la spedizione
018600080421     c* totalemente
018700080421     c                   if        §PCTCMC = 'P'
018800080421     c                   exsr      srdet
018900080421     c                   else
019000081117     c                   clear                   §PCTCMC
019100080421     c                   end
019200080421     c                   exsr      srtesta
019300080421     c                   enddo
019400080421     c* scrivo il CRI
019500080421     c                   clear                   fidsf000
019600080421     c                   eval      DSFFGS = dstfgs
019700080421     c                   eval      DSFNPG = dstnpg
019800080421     c                   eval      DSFNFV = dstnfv
019900080421     c                   eval      DSFTRD = 'CRI'
020000080421     c                   eval      DSFPDAAS4 ='A'
020100080725     c                   clear                   fidsfcrids
020200080725     c                   eval      §DSFSPCDES=knmus
020300080725     c                   eval      DSFdati = fidsfcrids
020400080421     c* i dati vuoto ???
020500080421     C                   movel     OGGI          dsfDTORIN
020600080421     C                   TIME                    ora               6 0
020700080421     c                   move      ora           dsfDTORIN
020800080421     c                   write     fidsf000
020900080421     c                   endsr
021000080421     c**********************************************************************
021100080421     c     srdet         begsr
021200080421     c**********************************************************************
021300080423     C                   MOVE      '99'          COMPRI            2
021400080423     c                   clear                   §PCTCMC
021500080423     c                   clear                   ok                1 0
021600080422     c                   clear                   fipcdcedds
021700080421     c                   eval      PCDFGS = dstfgs
021800080421     c                   eval      PCDNDC = dstnfv
021900080805     c                   eval      PCDcodaut = arbpdc
022000080421     c                   eval      PCDAAS = arbaas
022100080421     c                   eval      PCDLNP = arblnp
022200080421     c                   eval      PCDNRS = arbnrs
022300080421     c                   eval      PCDNSP = arbnsp
022400080421     c                   eval      PCDTRD ='CED'
022500080421     c                   eval      PCDPDAAS4 = 'A'
022600080422     c     kart          setll     fnart01l
022700080421     c                   do        *hival
022800080422     c     kart          reade     fnart01l
022900080421     c                   if        %eof(fnart01l)
023000080421     c                   leave
023100080421     c                   end
023200080421     c                   if        artdcm <> 0
023300080421     c                   iter
023400080421     c                   end
023500080430     c* spunta 8 -> cerco il codice mancata consegna
023600080423     c     kbrv          chain     fnbrv02l
023700080430     c                   if        %found(fnbrv02l)
023800080430     c                   if        brvcan <> ' '
023900080423     c                   exsr      mancon
024000080430     c                   end
024100080423     c                   iter
024200080421     c                   end
024300080421     c* esito CED
024400080421     c     kpcd          setll     fipcd03l                               01
024500080421     c                   if        *in01
024600080422     c* forzo ok = 1 anche se presente un CED scritto da pda
024700080422     c                   eval      ok = 1
024800080421     c                   iter
024900080421     c                   end
025000080421     c                   eval      PCDFLS = artfls
025100080421     c                   eval      PCDLNA = artlna
025200080421     c                   eval      PCDNRSER = artnrs
025300080421     c                   eval      PCDNSC = artnsc
025400080421     C* VUOTO???
025500080421     c                   eval      PCDDATI= FIPCDCEDDS
025600080421     C                   movel     OGGI          PCDDTORIN
025700080421     C                   TIME                    ora               6 0
025800080421     c                   move      ora           PCDDTORIN
025900080421     c                   write     fipcd000
026000080422     c                   eval      ok = 1
026100080421     c                   enddo
026200080422     c* se esco senza aver scritto almento un record vuol dire che è una
026300080430     c* non fatta ???
026400080422     c                   if        ok = 0
026500080422     c                   eval      §PCTCMC = 'N'
026600080422     c                   end
026700080430     c* se esco senza aver trovato una spunta 8 per poter dare il codice
026800080430     c* di consegna forzo parziale
026900080430     c                   if        ok = 1 and §pctcmc = ' '
027000080430     c                   eval      §PCTCMC = 'P'
027100080430     c                   end
027200080421     c                   endsr
027300080423     c**********************************************************************
027400080423     C     MANCON        BEGSR
027500080423     c**********************************************************************
027600080423     C*prendo il codice di mancata consegna dalla spunta 8
027700080423     C                   Z-ADD     1             X
027800080423     C     BRVCAN        LOOKUP    COA(X)                                 93
027900080423     C     *IN93         IFEQ      '1'
028000080423     C     PMC(X)        ANDLT     COMPRI
028100080423     C                   MOVEL     CMB(X)        §PCTCMC
028200080423     C                   MOVEL     PMC(X)        COMPRI
028300101006     c                   eval      ok = 1
028400080423     C                   ENDIF
028500080423     C*
028600080423     C                   ENDSR
028700080421     c**********************************************************************
028800080421     c     srtesta       begsr
028900080421     c**********************************************************************
029000080421     c                   eval      PCtFGS = dstfgs
029100080421     c                   eval      PCtNDC = dstnfv
029200080805     c                   eval      PCtcodaut = arbpdc
029300080805     c                   eval      PCtAAS = arbaas
029400080421     c                   eval      PCtLNP = arblnp
029500080421     c                   eval      PCtNRS = arbnrs
029600080421     c                   eval      PCtNSP = arbnsp
029700080421     c                   eval      PCtTRD ='CET'
029800080421     c                   eval      PCtPDAAS4 = 'A'
029900080421     c                   eval      §PCTDATA = oggi
030000080421     c                   eval      §PCTORA = 140000
030100101102     c                   eval      §PCTLDVFIR = 'S'
030200080422     c                   eval      PCTDATI= FIPCTCETDS
030300080421     C                   movel     OGGI          PCtDTORIN
030400080421     C                   TIME                    ora               6 0
030500080421     c                   move      ora           PCtDTORIN
030600080421     c                   write     fipct000
030700080421     c                   endsr
030800080421     c**********************************************************************
030900080421     c     *inzsr        begsr
031000080421     c**********************************************************************
031100080421     C     *ENTRY        PLIST
031200080421     C                   PARM                    KPJBA
031300080421     c*
031400080421     c     *dtaara       define    §azute        azuteds
031500080421     c     *dtaara       define    §datiute      ddatiute
031600080421     C                   in(E)     *dtaara
031700080421     C                   IF        %Error  or  RSUT = *blanks
031800080421     C                   call      'TIBS34R'
031900080421     C                   parm                    Tibs34Ds
032000080421     C                   in        *dtaara
032100080422     c                   ENDIF
032200080421     C* Giro data del giorno
032300080421     C                   TIME                    WHHDAT           14 0
032400080421     C                   MOVE      WHHDAT        G02DAT
032500080421     C                   MOVEL     *BLANK        G02ERR
032600080421     C                   CALL      'XSRDA8'
032700080421     C                   PARM                    WLBDA8
032800080421     C                   Z-ADD     G02INV        OGGI              8 0
032900080421     c*
033000080422     C     Kplg          KLIST
033100080422     C                   KFLD                    plgtiporcd
033200080421     C                   KFLD                    d53fgs
033300080422     C                   KFLD                    VNDT
033400080422     C     Kdst          KLIST
033500080422     C                   KFLD                    d53fgs
033600080422     C                   KFLD                    dstNPG
033700080422     C                   KFLD                    VNDT
033800080421     C     Kdsf          KLIST
033900080421     C                   KFLD                    dstNPG
034000080421     C                   KFLD                    VNDT
034100080421     C                   KFLD                    d53fgs
034200080421     C                   KFLD                    dsftrd
034300080422     C     Karb70        KLIST
034400080422     C                   KFLD                    arbifp
034500080422     C                   KFLD                    arbndc
034600080422     C     Karb          KLIST
034700080422     C                   KFLD                    arbaas
034800080422     C                   KFLD                    arblnp
034900080422     C                   KFLD                    arbnrs
035000080422     C                   KFLD                    arbnsp
035100080422     C     Kart          KLIST
035200080422     C                   KFLD                    artaas
035300080422     C                   KFLD                    artlnp
035400080422     C                   KFLD                    artnrs
035500080422     C                   KFLD                    artnsp
035600080422     C     klbl          klist
035700080422     C                   kfld                    lblaap
035800080422     C                   kfld                    lbllpp
035900080422     C                   kfld                    lblnrp
036000080422     C                   kfld                    lblnsp
036100080422     C     Kpct          KLIST
036200080422     C                   KFLD                    d53fgs
036300080422     C                   KFLD                    VNDT
036400080805     C                   KFLD                    arbpdc
036500080422     C                   KFLD                    pcttrd
036600080422     C                   KFLD                    arbaas
036700080422     C                   KFLD                    arblnp
036800080422     C                   KFLD                    arbnrs
036900080422     C                   KFLD                    arbnsp
037000080422     C     Kpcd          KLIST
037100080422     C                   KFLD                    d53fgs
037200080422     C                   KFLD                    VNDT
037300080805     C                   KFLD                    arbpdc
037400080422     C                   KFLD                    pcdtrd
037500080422     C                   KFLD                    arbaas
037600080422     C                   KFLD                    arblnp
037700080422     C                   KFLD                    arbnrs
037800080422     C                   KFLD                    arbnsp
037900080422     C                   KFLD                    artnsc
038000080422     C     Kbrv          KLIST
038100080422     C                   KFLD                    brvnpg
038200080422     C                   KFLD                    VNDT
038300080422     C                   KFLD                    d53fgs
038400080422     C                   KFLD                    artlna
038500080422     C                   KFLD                    artfls
038600080422     C                   KFLD                    artnrs
038700080422     C                   KFLD                    artnsc
038800080428     C     KTAB1         KLIST
038900080430     C                   KFLD                    TblKUT
039000080430     C                   KFLD                    TblCOD
039100080422     c* riceve obbligatoriamente il p.o., la distinta è opzionale, se non
039200080422     c* passata emetto il video per richiederla
039300080422     c                   eval      fnlr53ds6 = kpjbu
039400080422     c                   eval      brvnpg = 8
039500080422     c                   eval      plgtiporcd = 'CRI'
039600080422     c                   eval      pcttrd = 'CET'
039700080422     c                   eval      pcdtrd = 'CED'
039800080421     C                   Z-ADD     4             dstNPG
039900080422     c                   if        kpjbu = ' '
040000080422     c                   eval      d53fgs = DUTPOU
040100080422     c                   clear                   vndt
040200080422     c                   else
040300080422     c                   move      d53ndc        vndt
040400080422     c                   end
040500080422     c                   move      d53fgs        arbifp
040600080421     c                   clear                   d53err
040700080422     c                   clear                   d53msg
040800080428     C*  Caricamento Tabella 3E
040900080428     C                   Z-ADD     0             X                 3 0
041000080430     C                   MOVEL     '3E'          TblCOD
041100101006     c                   z-add     1             tblkut
041200080428     C     KTAB1         CHAIN     TABEL00F                           96
041300080428     C     *IN96         DOWEQ     '0'
041400080428     C     X             ANDLT     100
041500080428     C     TBLFLG        IFEQ      ' '
041600080428     C                   ADD       1             X
041700080428     C                   MOVEL     TBLKEY        COA(X)
041800080428     C                   MOVEL     TBLUNI        DS3E
041900080430     C                   MOVEL     §3ECMB        CMB(X)
042000080428     C                   MOVEL     §3EPMC        PMC(X)
042100080428     C                   END
042200080428     C     KTAB1         READE     TABEL00F                               96
042300080428     C                   END
042400080430     c                   endsr
