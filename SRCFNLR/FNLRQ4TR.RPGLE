000100110104     h dftactgrp(*yes)
000200000810     H DECEDIT('0,') DATEDIT(*DMY.)
000300110113      * FNLRQ4TR *---------------------------------------------------*
000400110113      *---------**                                                   *
000500110113      *     - TEST x VERIFICA CONTROLLI BOLLA        -               *
000600101221      *--------------------------------------------------------------*
000700101220      ***Se richiamato la DS ha i seguenti campi:                                     ***
000800110113      ***   LRQ4AASI       1     4 S  0 ANNO BOLLA                                    ***
000900110113      ***   LRQ4LNPI       5     3 S  0 LINEA BOLLA                                   ***
001000110113      ***   LRQ4NRSI       8     2 S  0 NR.SERIE BOLLA                                ***
001100110113      ***   LRQ4NSPI      10     7 S  0 NR.SPEDIZIONE                                 ***
001200110113      ***   LRQ4NDCI      17     6 S  0 NUM. DISTINTA                                 ***
001300110113      ***   LRQ4DDCI      23     8 S  0 DATA DISTINTA                                 ***
001400110113      ***   LRQ4ERRO      31     1 A    ERRORE RITORNO                                ***
001500110113      ***   LRQ4MSGO      32    70 A    MESSAGGIO ERRORE                              ***
001600101213     F*--------------------------------------------------------------*
001700110113     FFNLRQ4TD  CF   E             WORKSTN
001800110114     F*--------------------------------------------------------------*
001900110114     FFNARB01L  iF   E           K DISK    infds(arbds)   prefix(£)
002000110114     FFIARG01L  iF   E           K DISK    infds(argds)   prefix(£)
002100101223     F*--------------------------------------------------------------*
002200101221     D ERR             S             66    DIM(50) CTDATA PERRCD(1)             MSG ERRORE
002300101213      *---------------------------------------------------------------
002400030730     d* ds esterne
002500030730      *---------------------------------------------------------------
002600110114      *
002700110114     d fnarbds$      e ds                  extname(fnarb00F) prefix($)
002800110114     d fnarbds£      e ds                  extname(fnarb00F) prefix(£)
002900110114     d fnarbds       e ds                  extname(fnarb00F)
003000110114      *
003100110114     d  varbds         ds
003200110114     d   v03arb1               1     78
003300110114     d   v03arb2              79    156
003400110114     d   v03arb3             157    234
003500110114     d   v03arb4             235    312
003600110114     d   v03arb5             313    390
003700110114     d   v03arb6             391    468
003800110114     d   v03arb7             469    546
003900110114     d   v03arb8             547    624
004000110114     d   v03arb9             625    702
004100110114      *
004200110114     d fiargds$      e ds                  extname(fiarg00F) prefix($)
004300110114     d fiargds£      e ds                  extname(fiarg00F) prefix(£)
004400110114     d fiargds       e ds                  extname(fiarg00F)
004500110114      *
004600110114     D fnlrq4ds      E DS
004700110114      *
004800110114     d arbds           ds
004900110114     d  arb_nrr              397    400b 0
005000110114      *
005100110114     d argds           ds
005200110114     d  arg_nrr              397    400b 0
005300110114      *
005400030730     D KPJBA         E DS
005500101217     D KPJBuS          s                   like(KPJBu)
005600110113     D ddatiute      e ds
005700110113     D azuteds       e ds                  extname(AZUTE00F)
005800110113     D tibs34ds      E DS                  inz
005900101214      *
006000101214     D WLBDA8          DS
006100101214     D  G02DAT                 1      8  0
006200101214     D  G02INV                 9     16  0
006300101214     D  G02ERR                17     17
006400101214     D  G02TGI                18     22  0
006500101214      *
006600101214     d Wdata8          DS
006700101214     d  dadata                 1      8  0
006800101214     d  adata                  9     16  0
006900101214     d  GioLav                17     21  0
007000101214      *
007100101214     D DATPAR          DS
007200101214     D  GIODAT                 1      8  0
007300101214     D  GIOINV                 9     16  0
007400101214     D  GIOTGI                17     21  0
007500030730     d*---------------------------------------------------------------*
007600030730     d* campi di comodo                                               *
007700030730     d*---------------------------------------------------------------*
007800030730     d barcode         s             14
007900030730     d dtaeur          s               d   datfmt(*eur)
008000030730     d dtaiso          s               d
008100101214     d dtaiso_oggi     s               d   datfmt(*iso)
008200101214     d dtaiso_dst      s               d   datfmt(*iso)
008300101222     C*-----------------------------------------------------*
008400110113     C     *ENTRY        PLIST
008500110113     C                   PARM                    KPJBA
008600110113     C*-----------------------------------------------------*
008700110113     C*    Operazioni iniziali
008800110113     C*-----------------------------------------------------*
008900110113     C*
009000110113     C* Inverto la data del giorno
009100110113     C                   TIME                    WHHDAT           14 0
009200110113     C                   MOVEL     WHHDAT        HHMMss            6 0          *ORA/MINUTI/sec
009300110113     C                   MOVE      WHHDAT        G02DAT
009400110113     C                   MOVE      *BLANKS       G02ERR
009500110113     C                   CALL      'XSRDA8'
009600110113     C                   PARM                    WLBDA8
009700110113     C                   Z-ADD     G02INV        DATEU             8 0
009800110113      *
009900110113     c     *dtaara       define    §azute        azuteds
010000110113     c     *dtaara       define    §datiute      ddatiute
010100110113     C                   in(E)     *dtaara
010200110113     c*
010300110113     C                   IF        %Error  or  RSUT = *blanks
010400110113     C                   call      'TIBS34R'
010500110113     C                   parm                    Tibs34Ds
010600110113     C                   in        *dtaara
010700110113     c                   ENDIF
010800110113     c*
010900110113     c* controllo se p.o. partito non può + usare la funzione ma deve
011000110113     c* agire dal pgm di assegnaz./disassegnaz.
011100110113     C                   move      *date         dtaeur
011200110113     C                   move      dtaeur        dtaiso
011300110113     c                   move      dtaiso        oggin             8 0
011400110113     c                   move      oggin         oggi              8
011500110113      *---------------------------*
011600110113     C*
011700110113     C                   MOVEL     'FNLRQ4TR'    VIDPGM
011800110113     C                   MOVE      *year         V03ANS
011900110113     C*
012000110113     C*
012100110113      * se richiamato da preparazione FNLR50R riceve i dati della spedizione
012200110113     c                   clear                   fnlrQ4ds
012300110104     C*
012400110104     C*   subito accende per usufruire del RETURN
012500110104     c                   seton                                        LR
012600110114     C*
012700110114     c                   eval      v03chi = 'R'
012800110114     c                   eval      v03son = 'N'
012900110104     C*
013000030730     C                   DO        *HIVAL
013100101222      * -----------
013200101213      *                *-------------------*
013300030730     C                   EXFMT     LR98D01
013400101213      *                *-------------------*
013500110113     c                   clear                   v03MSG
013600110113     c                   setoff                                       90
013700030730     c* fine lavoro
013800941107     C     *INKC         IFEQ      '1'
013900030730     C                   LEAVE
014000941107     C                   END
014100110114     c                   exsr      srctr
014200110113     c* x errori
014300110113     C     *IN90         IFEQ      '1'
014400110113     C                   iter
014500110113     C                   END
014600101213      *--------
014700030730     c* controlli
014800110113     C     *INKF         IFEQ      '1'
014900110113     c                   exsr      callpgm
015000110113     C                   END
015100101223      *
015200030730     c                   enddo
015300101213      *
015400101220     c**********************************************************************
015500101220     C     srctr         BEGSR
015600101220     c**********************************************************************
015700110114      *
015800110114     C     Bolla_Passata KLIST
015900110114     C                   KFLD                    V03ANS
016000110114     C                   KFLD                    V03LNP
016100110114     C                   KFLD                    V03NRS
016200110114     C                   KFLD                    V03NSP
016300110114      *
016400110114     C                   SETOFF                                       9091
016500110113      *
016600101220     C     V03ANS        IFEQ      *ZEROS
016700101220     C                   SETON                                        0190
016800101220     c                   leaveSR
016900101220     C                   END
017000110113      *
017100101220     C     V03LNP        IFEQ      *ZEROS
017200101220     C                   SETON                                        0290
017300101220     c                   leaveSR
017400101220     C                   END
017500110113      *
017600101220     C     V03NSP        IFEQ      *ZEROS
017700110113     C                   SETON                                        0490
017800101220     c                   leaveSR
017900101220     C                   END
018000110113      *
018100110113      *
018200110113     C     V03DDC        IFEQ      *ZEROS
018300110113     C                   SETON                                        0690
018400110113     c                   leaveSR
018500110113     C                   END
018600101220      *
018700110114     c                   clear                   fnarbds£
018800110114     c                   clear                   fiargds£
018900110114     c                   clear                   varbds
019000110114      *
019100110114     c                   if        v03son ='S'
019200110114     c                   seton                                        91
019300110114     C     Bolla_Passata chain     fnarb01l
019400110114     c                   if        %Found(fnarb01l)
019500110114      *
019600110114     c                   eval        fnarbds = fnarbds£
019700110114     c                   exsr      a_video
019800110114      *
019900110114     C     Bolla_Passata chain     fiarg01l
020000110114     c                   if        %Found(fiarg01l)
020100110114      *
020200110114     c                   eval        fiargds = fiargds£
020300110114      *
020400110114     c                   end
020500110114     c                   else
020600110114     c                   seton                                        90
020700110114     c                   eval      v03msg ='Bolla non trovata !!'
020800110114     c                   end
020900110114     c                   end
021000110114      *
021100101220     c                   endsr
021200101220     c**********************************************************************
021300110113     C     CALLPGM       BEGSR
021400110107     C*
021500101223     C*   esternizzata Routine di controlli
021600101223     C*      sulla Consegnabilità della Bolla
021700101223     c                   clear                   Fnlrq4DS
021800101223     c                   z-add     v03ans        LRQ4aasI
021900101223     c                   z-add     v03lnp        LRQ4lnpI
022000101223     c                   z-add     v03nrs        LRQ4nrsI
022100101223     c                   z-add     v03nsp        LRQ4nspI
022200110113     c                   z-add     v03ddc        LRQ4ddcI
022300110114     c                   move      v03chi        LRQ4chiu
022400110114      *
022500110113     c                   clear                   fnarbds$
022600110113     c                   clear                   fiargds$
022700110114      *  Parametri
022800110114     c                   eval      kpjbu = Fnlrq4DS
022900110114     c                   if        v03son ='S'
023000110114     c                   eval        fnarbds$ = fnarbds£
023100110114     c                   eval        fiargds$ = fiargds£
023200110114     c                   endif
023300110114      *
023400101223     c                   call      'FNLRQ4R'
023500101223     c                   parm                    kpjba
023600110114     c                   parm                    fnarbds$
023700110114     c                   parm                    fiargds$
023800110114      *
023900110114      * ritorno parametri
024000101223     c                   eval      Fnlrq4DS = kpjbu
024100110114      *
024200110114     c                   setoff                                       91
024300110114     c                   clear                   varbds
024400110114     c                   if         fnarbds$ <> fnarbds£
024500110114     c                   eval      fnarbds  =  fnarbds$
024600110114     c                   exsr      a_video
024700110114     c                   seton                                        91
024800110114     c                   end
024900101223      * imposta l'errore
025000110114     c                   seton                                        90
025100110113     c                   eval      v03MSG    = LRQ4MSGO
025200110113     c                   if        v03MSG = *blank
025300110114     c                   setoff                                       90
025400110114     c                   move      'TUTTO OK !!' v03MSG
025500110113     c                   end
025600101223     C*
025700101223     C                   ENDSR
025800080416     C*-----------------------------------------------------*
025900110114     C     a_video       BEGSR
026000110114     C*
026100110114     c                   eval      varbds = arbatB
026200110114     c                                    + %editc(arbAAS:'X')
026300110114     c                                    + %editc(arbLNP:'X')
026400110114     c                                    + %editc(arbNRS:'X')
026500110114     c                                    + %editc(arbNSP:'X')
026600110114     c                                    + %editc(arbMGS:'X')
026700110114     c                                    +        ARBCBO
026800110114     c                                    +        ARBFNS
026900110114     c                                    +        ARBSCL
027000110114     c                                    +        ARBFBR
027100110114     c                                    +        ARBFDN
027200110114     c                                    + %editc(arbLNA:'X')
027300110114     c                                    + %editc(ARBZNC:'X')
027400110114     c                                    + %editc(ARBKSC:'X')
027500110114     c                                    +        ARBRSM
027600110114     c                                    +        ARBINM
027700110114     c                                    +        ARBCAM
027800110114     c                                    +        ARBLOM
027900110114     c                                    +        ARBPRM
028000110114     c                                    +        ARBFAP
028100110114     c                                    +        ARBNZM
028200110114     c                                    +        ARBCPI
028300110114     c                                    +        ARBRSD
028400110114     c                                    +        ARBIND
028500110114     c                                    +        ARBCAD
028600110114     c                                    +        ARBLOD
028700110114     c                                    +        ARBPRD
028800110114     c                                    +        ARBFIN
028900110114     c                                    +        ARBNZD
029000110114     c                                    +        ARBGC1
029100110114     c                                    +        ARBGC2
029200110114     c                                    + %editc(ARBCTR:'X')
029300110114     c                                    +        ARBCTS
029400110114     c                                    +        ARBFTM
029500110114     c                                    +        ARBTSP
029600110114     c                                    +        ARBNAS
029700110114     c                                    + %editc(ARBNCL:'X')
029800110114     c                                    + %editc(ARBNCP:'X')
029900110114     c                                    + %editc(ARBPKC:'X')
030000110114     c                                    + %editc(ARBPKB:'X')
030100110114     c                                    + %editc(ARBPKF:'X')
030200110114     c                                    + %editc(ARBNCR:'X')
030300110114     c                                    + %editc(ARBVLC:'X')
030400110114     c                                    +        ARBFVB
030500110114     c                                    + %editc(ARBVLB:'X')
030600110114     c                                    +        ARBFVF
030700110114     c                                    + %editc(ARBVLF:'X')
030800110114     c                                    + %editc(ARBQFT:'X')
030900110114     c                                    + %editc(ARBVMD:'X')
031000110114     c                                    +        ARBVAD
031100110114     c                                    + %editc(ARBIAS:'X')
031200110114     c                                    +        ARBVAS
031300110114     c                                    + %editc(ARBCCM:'X')
031400110114     c                                    + %editc(ARBRMN:'X')
031500110114     c                                    +        ARBRMA
031600110114     c                                    +        ARBRMO
031700110114     c                                    + %editc(ARBFLS:'X')
031800110114     c                                    + %editc(ARBNCD:'X')
031900110114     c                                    + %editc(ARBNCA:'X')
032000110114     c                                    +        ARBXCO
032100110114     c                                    +        ARBFFD
032200110114     c                                    +        ARBTCR
032300110114     c                                    + %editc(ARBDCR:'X')
032400110114     c                                    + %editc(ARBHCR:'X')
032500110114     c                                    + %editc(ARBDTI:'X')
032600110114     c                                    + %editc(ARBHTI:'X')
032700110114     c                                    + %editc(ARBDCE:'X')
032800110114     c                                    + %editc(ARBNPI:'X')
032900110114     c                                    + %editc(ARBNCI:'X')
033000110114     c                                    + %editc(ARBNRC:'X')
033100110114     c                                    + %editc(ARBDBR:'X')
033200110114     c                                    + %editc(ARBNFV:'X')
033300110114     c                                    + %editc(ARBDPC:'X')
033400110114     c                                    + %editc(ARBDUC:'X')
033500110114     c                                    + %editc(ARBFLP:'X')
033600110114     c                                    + %editc(ARBDET:'X')
033700110114     c                                    + %editc(ARBDUT:'X')
033800110114     c                                    + %editc(ARBDAM:'X')
033900110114     c                                    + %editc(ARBPDC:'X')
034000110114     c                                    + %editc(ARBNDC:'X')
034100110114     c                                    + %editc(ARBDDC:'X')
034200110114     c                                    + %editc(ARBNTC:'X')
034300110114     c                                    + %editc(ARBDCP:'X')
034400110114     c                                    + %editc(ARBDCM:'X')
034500110114     c                                    + %editc(ARBHMC:'X')
034600110114     c                                    +        ARBTC1
034700110114     c                                    +        ARBTC2
034800110114     c                                    +        ARBFAN
034900110114     c                                    +        ARBCCA
035000110114     c                                    +        ARBTMC
035100110114     c                                    +        ARBCMC
035200110114     c                                    +        ARBGGA
035300110114     c                                    +        ARBGMA
035400110114     c                                    +        ARBGVA
035500110114     c                                    +        ARBFBS
035600110114     c                                    + %editc(ARBSTP:'X')
035700110114     c                                    + %editc(ARBNGD:'X')
035800110114     c                                    + %editc(ARBNSS:'X')
035900110114     c                                    + %editc(ARBAMP:'X')
036000110114     c                                    + %editc(ARBAMA:'X')
036100110114     c                                    +        ARBACC
036200110114     c                                    +        ARBACA
036300110114     c                                    +        ARBICC
036400110114     c                                    +        ARBICA
036500110114     c                                    +        ARBMIF
036600110114     c                                    + %editc(ARBIFP:'X')
036700110114     c                                    +        ARBFIP
036800110114     c                                    + %editc(ARBIPP:'X')
036900110114     c                                    +        ARBFBC
037000110114     c                                    + %editc(ARBCUC:'X')
037100110114     c                                    +        ARBFDC
037200110114     c                                    +        ARBFNC
037300110114     c                                    + %editc(ARBNMI:'X')
037400110114     c                                    + %editc(ARBDMI:'X')
037500110114     c                                    + %editc(ARBFLE:'X')
037600110114     c                                    + %editc(ARBTFP:'X')
037700110114     c                                    + %editc(ARBFEA:'X')
037800110114     c                                    + %editc(ARBTFA:'X')
037900110114     c                                    +        ARBFT1
038000110114     c                                    + %editc(ARBDT1:'X')
038100110114     c                                    +        ARBFT2
038200110114     c                                    + %editc(ARBDT2:'X')
038300110114     c                                    +        ARBFT3
038400110114     c                                    + %editc(ARBDT3:'X')
038500110114     C                   ENDSR
038600110114     C*-----------------------------------------------------*
