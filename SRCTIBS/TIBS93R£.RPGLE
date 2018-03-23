000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200080529      * tibs90R *----------------------------------------------------*
000300080529      *         - MODIFICA terminal a filiali di secondo livello
000400940915      *--------------------------------------------------------------*
000500080529     FTIBS90D   CF   E             WORKSTN
000600080529     f                                     sfile(bs90s01 : nrr1)
000700951013     FAZORG01L  IF   E           K DISK
000800080529     FAZcae03L  IF   E           K DISK
000900020805      *
001000020805      * DEFINIZIONE SCHIERE
001100960302     D*
001200080529     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
001300040917      * DS per FNLV55R - Reperimento Terminal di Partenza e di Arrivo
001400040917     D FNLV55DS      E DS
001500040917     D KPJBA         E DS
001600040917     D CNCR80        E DS
001700050414     D og140         E DS
001800040917     D UT§DSE0F      E DS
001900080529     d
002000080529     D WLBDAT          DS                  INZ
002100080529     D  G02DAT                 1      8  0
002200080529     D  G02INV                 9     16  0
002300080529     D  G02ERR                17     17
002400080529     D  G02TGI                18     22  0
002500080529     d
002600050103     d Dataiso         s               d   datfmt(*iso)
002700050103     d Dataeur         s               d   datfmt(*eur)
002800080529     d datadmy         s               d   datfmt(*dmy)
002900050103     d
003000080529     d nrr1            s              4  0 inz
003100080529     D skipo           s              3    DIM(30)
003200041220     d
003300050103     C**********************************************************************
003400040917     C     *ENTRY        PLIST
003500040917     C                   PARM                    KPJBA
003600080529     c*
003700080529     C                   Z-ADD     1             CODUT
003800080529     C                   CALL      'X§PARUT'
003900080529     C                   PARM                    UT§DSE0F
004000080529     C                   MOVEL     REC80         CNCR80
004100080529     C                   MOVEL     RAGUT         VIDRSU
004200080529     C
004300080529     C                   EVAL      VIDQINTER='NO'
004400080529     c
004500080529     c                   clear                   viddela
004600080529     c                   exsr      ctrd01
004700080529     c
004800080529     c     inizio        tag
004900080529     c                   write     bs90t01
005000080529     c                   write     bs90p01
005100080529     c                   if        nrr1=0
005200080529     c                   seton                                        30
005300080529     c                   write     bs90d02
005400080529     c                   endif
005500040917     c
005600040917     C     FOR01         TAG
005700000828      *
005800080529     C                   EXFMT     BS90c01
005900080529     C                   SETOFF                                       289001
006000080529     c                   clear                   rivisual          1
006100040917     C* F3 - FINE
006200040917     C   KC              GOTO      FINE
006300080529     c
006400050104     C* F7 - manutenzione organigramma
006500050104     c                   if        *inkg
006600050110     C                   call      'TIBS84C'
006700050104     c                   parm                    kpjba
006800080529     c                   goto      inizio
006900050104     c                   endif
007000041220      *  CONTROLLI
007100040917     C                   EXSR      CTRD01
007200080529     c                   if        *in90
007300080529     c                   goto      for01
007400080529     c                   else
007500080529     c                   if        not *inkf or rivisual='S'
007600080529     c                   goto      inizio
007700080529     c                   endif
007800080529     c                   endif
007900080529     c
008000080529     c* F6 - conferma
008100080529     c                   if        *inkf
008200080529     c                   clear                   skipo
008300080529     c                   clear                   elab
008400080529     c                   z-add     1             nrr1
008500080529     c                   dow       nrr1<=totnrr
008600080529     c     nrr1          chain     bs90s01
008700080529     c
008800080529     c                   if        s01sce='1'
008900080529     c
009000080529     c* non voglio modificare il qinter
009100080529     c                   if        vidqinter='NO'
009200080529     c                   eval      s01oaex='NO '
009300080529     c                   clear                   s01naex
009400080529     c                   endif
009500080529     c
009600080529     c                   call      'TIBS86C'
009700080529     c                   parm                    s01fil
009800080529     c                   parm                    s01otfp
009900080529     c                   parm                    s01ntfp
010000080529     c                   parm                    skipo
010100080529     c                   parm                    s01oaex
010200080529     c                   parm                    s01naex
010300080529
010400080529     c                   add       1             elab              5 0
010500080529     c                   endif
010600080529     c
010700080529     c                   add       1             nrr1
010800080529     c                   enddo
010900080529     c
011000080529     c                   if        elab=0 and totnrr>0
011100080529     c                   movel     msg(2)        vidmsg
011200080529     c                   seton                                        2890
011300080529     c                   goto      for01
011400080529     c                   endif
011500080529     c                   endif
011600900515      *
011700000000     C     FINE          TAG
011800960129     C*
011900040917     c                   clear                   fnlv55ds
012000040917     C                   MOVEL     'C'           D55TLA
012100040917     C                   CALL      'FNLV55R'
012200040917     C                   PARM                    FNLV55DS
012300000000     C                   SETON                                        LR
012400080529      *-----------------------------------------------------***********
012500080529      * Caricamento sfl con filiali che cambiano terminal
012600080529      *-----------------------------------------------------***********
012700080529     C     Carfil        BEGSR
012800080529     C* PULIZIA SFL
012900080529     C                   SETON                                        3031
013000080529     C                   WRITE     BS90C01
013100080529     C                   SETOFF                                       3031
013200080529     c                   clear                   nrr1
013300080529     c                   clear                   totnrr            5 0
013400080529     c
013500080529     c     'P'           setll     azcae03l
013600080529     c     'P'           reade     azcae03l
013700080529    1c                   dow       not %eof(azcae03l)
013800080529     c
013900080529    2c                   if        amgdela>=caedde  and   amgdela<=caedsc
014000080529     c                             and caetfp<>caetfe
014100090115     c* Verifico se alla data -1 il terminal era diverso
014200080529     C                   CLEAR                   FNLV55DS
014300080529     C                   MOVEL     'P'           D55TPT
014400080529     C                   MOVE      caetfp        D55LIN
014500090115     C                   MOVE      woggim1       d55drf
014600080529     C                   CALL      'FNLV55R'
014700080529     C                   PARM                    FNLV55DS
014800090115     c
014900090115     c* Se non cambia a -1, provo  a -3
015000090115    3c***                if        d55tfp= caetfe
015100090115     C***                CLEAR                   FNLV55DS
015200090115     C***                MOVEL     'P'           D55TPT
015300090115     C***                MOVE      caetfp        D55LIN
015400090115     C***                MOVE      woggim3       d55drf
015500090115     C***                CALL      'FNLV55R'
015600090115     C***                PARM                    FNLV55DS
015700090115     c***                endif
015800080529     c*
015900080529    3c                   if        caetfp<>d55tfp  and d55tfp<>caetfe
016000080529     c* riempo sfl
016100080529     c                   eval      s01fil=caetfp
016200080529     C                   MOVE      s01fil        CODFIL            3 0
016300080529     c                   exsr      ctrfil
016400080529     c                   movel     orgdes        s01dfil
016500080529
016600080529     c                   eval      s01otfp=d55tfp
016700080529     C                   MOVE      s01otfp       CODFIL            3 0
016800080529     c                   exsr      ctrfil
016900080529     c                   movel     orgdes        s01dotfp
017000080529     C* MEMORIZZO AREA EX AS400 PER QINTER
017100080529     C                   MOVEL     ORGDE0        OG140
017200080529     C                   MOVEL     §OGAEX        s01OAEX
017300080529
017400080529     c                   eval      s01Ntfp=caetfe
017500080529     C                   MOVE      s01Ntfp       CODFIL            3 0
017600080529     c                   exsr      ctrfil
017700080529     c                   movel     orgdes        s01dNtfp
017800080529     C* MEMORIZZO AREA EX AS400 PER QINTER
017900080529     C                   MOVEL     ORGDE0        OG140
018000080529     C                   MOVEL     §OGAEX        s01NAEX
018100080529
018200080529     c                   add       1             nrr1
018300080529     c                   write     bs90s01
018400080529    3c                   endif
018500080529    2c                   endif
018600080529
018700080529     c     'P'           reade     azcae03l
018800080529    1c                   enddo
018900080529     c
019000080529     c                   z-add     1             c01rcd
019100080529     c                   z-add     1             c01csr
019200080529     c                   z-add     nrr1          totnrr
019300080529     c                   seton                                        33
019400080529     c                   ENDSR
019500080529      *-----------------------------------------------------***********
019600080529      * controllo data di elaborazione
019700080529      *-----------------------------------------------------***********
019800080529     C     Ctrd01        BEGSR
019900080529     c                   if        viddela=0
020000080529     c                   Move      *date         dataiso
020100080529     c                   Move      dataiso       dataeur
020200080529     c                   Move      dataeur       VIDDELA
020300080529     c                   endif
020400080529     C                   Z-ADD     viddela       G02DAT
020500080529     C                   MOVEL     *BLANK        G02ERR
020600080529     C                   CALL      'XSRDA8'
020700080529     C                   PARM                    WLBDAT
020800080529     C* ERRATA
020900080529    1C     G02ERR        IFEQ      '1'
021000080529     c                   movel     msg(1)        vidmsg
021100080529     c                   seton                                        289001
021200080529     c                   else
021300080529     c                   movel     viddela       w002a             2
021400080529     c                   if        w002a='00'
021500080529     c     *dmy          move      viddela       datadmy
021600080529     c                   move      datadmy       dataiso
021700080529     c                   else
021800080529     c     *eur          movel     viddela       dataiso
021900080529     c                   endif
022000080529     c                   movel     dataiso       amgdela           8 0
022100080529     c                   movel     dataiso       dataeur
022200080529     c                   movel     dataeur       viddela
022300090115      * Calcolo oggi - 1 gg
022400090115     c                   subdur    1:*d          Dataiso
022500090115     c                   Move      Dataiso       woggim1           8 0
022600090115      * Calcolo oggi - 2 gg
022700090115     c                   subdur    2:*d          Dataiso
022800090115     c                   Move      Dataiso       woggim3           8 0
022900080529     c                   endif
023000080529     c
023100080529     c                   if        not *in90 and amgdela<>savela
023200080529     c                   exsr      carfil
023300080529     c                   movel     amgdela       savela            8 0
023400080529     c                   eval      rivisual='S'
023500080529     c                   endif
023600080529     c
023700080529     c                   ENDSR
023800951025     C**************************************************************************
023900951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
024000951025     C**************************************************************************
024100951025     C     CTRFIL        BEGSR
024200020213
024300951025     C     CODFIL        CHAIN     AZORG01L                           34
024400951025    1C  N34ORGFVA        IFNE      ' '
024500951025     C     ORGFAG        ORNE      'A'
024600951025     C     ORGFAG        ANDNE     'F'
024700951025     C                   SETON                                        34
024800951025    1C                   ENDIF
024900951025     C*
025000951025     C                   ENDSR
025100020805      **************************************************************************
025200960302** SCHIERA MSG - MESSAGGI DI ERRORE
025300080529Data errata                                                                   05
025400080529Nessuna filiale scelta per l'elaborazione                                     05
