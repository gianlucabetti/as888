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
002500171130     d
002600171130     D WDAT8           DS
002700171130     D  DATADA                 1      8  0
002800171130     D  DATAA                  9     16  0
002900171130     D  GGL                   17     21  0
003000080529     d
003100050103     d Dataiso         s               d   datfmt(*iso)
003200050103     d Dataeur         s               d   datfmt(*eur)
003300080529     d datadmy         s               d   datfmt(*dmy)
003400050103     d
003500080529     d nrr1            s              4  0 inz
003600080529     D skipo           s              3    DIM(30)
003700041220     d
003800050103     C**********************************************************************
003900040917     C     *ENTRY        PLIST
004000040917     C                   PARM                    KPJBA
004100080529     c*
004200080529     C                   Z-ADD     1             CODUT
004300080529     C                   CALL      'X§PARUT'
004400080529     C                   PARM                    UT§DSE0F
004500080529     C                   MOVEL     REC80         CNCR80
004600080529     C                   MOVEL     RAGUT         VIDRSU
004700080529     C
004800080529     C                   EVAL      VIDQINTER='NO'
004900080529     c
005000080529     c                   clear                   viddela
005100171201     c                   seton                                        10
005200080529     c                   exsr      ctrd01
005300080529     c
005400080529     c     inizio        tag
005500080529     c                   write     bs90t01
005600080529     c                   write     bs90p01
005700080529     c                   if        nrr1=0
005800080529     c                   seton                                        30
005900080529     c                   write     bs90d02
006000080529     c                   endif
006100040917     c
006200040917     C     FOR01         TAG
006300000828      *
006400080529     C                   EXFMT     BS90c01
006500080529     C                   SETOFF                                       289001
006600080529     c                   clear                   rivisual          1
006700040917     C* F3 - FINE
006800040917     C   KC              GOTO      FINE
006900080529     c
007000050104     C* F7 - manutenzione organigramma
007100050104     c                   if        *inkg
007200050110     C                   call      'TIBS84C'
007300050104     c                   parm                    kpjba
007400080529     c                   goto      inizio
007500050104     c                   endif
007600171201     C* F1 - sblocca data
007700171201     c                   if        *inka
007800171201     c                   setoff                                       10
007900171201     c                   goto      for01
008000171201     c                   endif
008100041220      *  CONTROLLI
008200040917     C                   EXSR      CTRD01
008300080529     c                   if        *in90
008400080529     c                   goto      for01
008500080529     c                   else
008600080529     c                   if        not *inkf or rivisual='S'
008700080529     c                   goto      inizio
008800080529     c                   endif
008900080529     c                   endif
009000171201
009100080529     c
009200080529     c* F6 - conferma
009300080529     c                   if        *inkf
009400080529     c                   clear                   skipo
009500080529     c                   clear                   elab
009600080529     c                   z-add     1             nrr1
009700080529     c                   dow       nrr1<=totnrr
009800080529     c     nrr1          chain     bs90s01
009900080529     c
010000080529     c                   if        s01sce='1'
010100080529     c
010200080529     c* non voglio modificare il qinter
010300080529     c                   if        vidqinter='NO'
010400080529     c                   eval      s01oaex='NO '
010500080529     c                   clear                   s01naex
010600080529     c                   endif
010700080529     c
010800080529     c                   call      'TIBS86C'
010900080529     c                   parm                    s01fil
011000080529     c                   parm                    s01otfp
011100080529     c                   parm                    s01ntfp
011200080529     c                   parm                    skipo
011300080529     c                   parm                    s01oaex
011400080529     c                   parm                    s01naex
011500080529
011600080529     c                   add       1             elab              5 0
011700080529     c                   endif
011800080529     c
011900080529     c                   add       1             nrr1
012000080529     c                   enddo
012100080529     c
012200080529     c                   if        elab=0 and totnrr>0
012300080529     c                   movel     msg(2)        vidmsg
012400080529     c                   seton                                        2890
012500080529     c                   goto      for01
012600080529     c                   endif
012700080529     c                   endif
012800900515      *
012900000000     C     FINE          TAG
013000960129     C*
013100040917     c                   clear                   fnlv55ds
013200040917     C                   MOVEL     'C'           D55TLA
013300040917     C                   CALL      'FNLV55R'
013400040917     C                   PARM                    FNLV55DS
013500000000     C                   SETON                                        LR
013600080529      *-----------------------------------------------------***********
013700080529      * Caricamento sfl con filiali che cambiano terminal
013800080529      *-----------------------------------------------------***********
013900080529     C     Carfil        BEGSR
014000080529     C* PULIZIA SFL
014100080529     C                   SETON                                        3031
014200080529     C                   WRITE     BS90C01
014300080529     C                   SETOFF                                       3031
014400080529     c                   clear                   nrr1
014500080529     c                   clear                   totnrr            5 0
014600080529     c
014700080529     c     'P'           setll     azcae03l
014800080529     c     'P'           reade     azcae03l
014900080529    1c                   dow       not %eof(azcae03l)
015000080529     c
015100171130     c* Carico i terminal che oggi decorrono o scadono
015200171130     c                   clear                   wdec              1
015300171130    2c                   if        caetfp<>caetfe
015400171130     c*
015500171130    3c                   if        amgdela =caedde  or    amgdela =caedsc
015600171130    4c                   if        amgdela=caedde
015700171130     c                   eval      wdec ='D'
015800171130     c                   ENDIF
015900171130    4c                   if        amgdela=caeDSC
016000171130     c* lo considero solo se durata temporanea ( meno di 10 gg lavorativi)
016100171130     c                   clear                   wdat8
016200171130     c                   z-add     caedde        datada
016300171130     c                   z-add     caedsc        dataa
016400171130     c                   call      'XSRLAV8'
016500171130     c                   parm                    wdat8
016600171130    5c                   if        ggl<10
016700171130     c                   eval      wdec ='S'
016800171130    5c                   endif
016900171130     c
017000171130    4c                   endif
017100171130    3c                   endif
017200171130     c
017300171130    3c                   if        wdec<>' '
017400171130
017500171130    2c**                 if        amgdela>=caedde  and   amgdela<=caedsc
017600171130     c**                           and caetfp<>caetfe
017700171130     c
017800171130    4c                   if        wdec='D'
017900090115     c* Verifico se alla data -1 il terminal era diverso
018000171130     c* se decorre da oggi
018100080529     C                   CLEAR                   FNLV55DS
018200080529     C                   MOVEL     'P'           D55TPT
018300080529     C                   MOVE      caetfp        D55LIN
018400090115     C                   MOVE      woggim1       d55drf
018500080529     C                   CALL      'FNLV55R'
018600080529     C                   PARM                    FNLV55DS
018700171130   x4c                   else
018800171130     C                   CLEAR                   FNLV55DS
018900171130     C                   MOVEL     'P'           D55TPT
019000171130     C                   MOVE      caetfp        D55LIN
019100171130     C                   MOVE      woggip1       d55drf
019200171130     C                   CALL      'FNLV55R'
019300171130     C                   PARM                    FNLV55DS
019400171130    4c                   endif
019500090115     c
019600090115     c* Se non cambia a -1, provo  a -3
019700090115    3c***                if        d55tfp= caetfe
019800090115     C***                CLEAR                   FNLV55DS
019900090115     C***                MOVEL     'P'           D55TPT
020000090115     C***                MOVE      caetfp        D55LIN
020100090115     C***                MOVE      woggim3       d55drf
020200090115     C***                CALL      'FNLV55R'
020300090115     C***                PARM                    FNLV55DS
020400090115     c***                endif
020500080529     c*
020600171130    4c                   if        caetfp<>d55tfp  and d55tfp<>caetfe
020700080529     c* riempo sfl
020800080529     c                   eval      s01fil=caetfp
020900080529     C                   MOVE      s01fil        CODFIL            3 0
021000080529     c                   exsr      ctrfil
021100080529     c                   movel     orgdes        s01dfil
021200080529
021300080529     c                   eval      s01otfp=d55tfp
021400080529     C                   MOVE      s01otfp       CODFIL            3 0
021500080529     c                   exsr      ctrfil
021600080529     c                   movel     orgdes        s01dotfp
021700080529     C* MEMORIZZO AREA EX AS400 PER QINTER
021800080529     C                   MOVEL     ORGDE0        OG140
021900080529     C                   MOVEL     §OGAEX        s01OAEX
022000080529
022100080529     c                   eval      s01Ntfp=caetfe
022200080529     C                   MOVE      s01Ntfp       CODFIL            3 0
022300080529     c                   exsr      ctrfil
022400080529     c                   movel     orgdes        s01dNtfp
022500080529     C* MEMORIZZO AREA EX AS400 PER QINTER
022600080529     C                   MOVEL     ORGDE0        OG140
022700080529     C                   MOVEL     §OGAEX        s01NAEX
022800080529
022900080529     c                   add       1             nrr1
023000080529     c                   write     bs90s01
023100171130    4c                   endif
023200171130    3c                   endif
023300080529    2c                   endif
023400080529
023500080529     c     'P'           reade     azcae03l
023600080529    1c                   enddo
023700080529     c
023800080529     c                   z-add     1             c01rcd
023900080529     c                   z-add     1             c01csr
024000080529     c                   z-add     nrr1          totnrr
024100080529     c                   seton                                        33
024200080529     c                   ENDSR
024300080529      *-----------------------------------------------------***********
024400080529      * controllo data di elaborazione
024500080529      *-----------------------------------------------------***********
024600080529     C     Ctrd01        BEGSR
024700080529     c                   if        viddela=0
024800080529     c                   Move      *date         dataiso
024900080529     c                   Move      dataiso       dataeur
025000080529     c                   Move      dataeur       VIDDELA
025100080529     c                   endif
025200080529     C                   Z-ADD     viddela       G02DAT
025300080529     C                   MOVEL     *BLANK        G02ERR
025400080529     C                   CALL      'XSRDA8'
025500080529     C                   PARM                    WLBDAT
025600080529     C* ERRATA
025700080529    1C     G02ERR        IFEQ      '1'
025800080529     c                   movel     msg(1)        vidmsg
025900080529     c                   seton                                        289001
026000080529     c                   else
026100080529     c                   movel     viddela       w002a             2
026200080529     c                   if        w002a='00'
026300080529     c     *dmy          move      viddela       datadmy
026400080529     c                   move      datadmy       dataiso
026500080529     c                   else
026600080529     c     *eur          movel     viddela       dataiso
026700080529     c                   endif
026800080529     c                   movel     dataiso       amgdela           8 0
026900080529     c                   movel     dataiso       dataeur
027000080529     c                   movel     dataeur       viddela
027100090115      * Calcolo oggi - 1 gg
027200090115     c                   subdur    1:*d          Dataiso
027300090115     c                   Move      Dataiso       woggim1           8 0
027400090115      * Calcolo oggi - 2 gg
027500090115     c                   subdur    2:*d          Dataiso
027600090115     c                   Move      Dataiso       woggim3           8 0
027700171130      * Calcolo oggi + 1 gg
027800171130     c                   movel     amgdela       dataiso
027900171130     c                   adddur    1:*d          Dataiso
028000171130     c                   Move      Dataiso       woggip1           8 0
028100080529     c                   endif
028200080529     c
028300080529     c                   if        not *in90 and amgdela<>savela
028400080529     c                   exsr      carfil
028500080529     c                   movel     amgdela       savela            8 0
028600080529     c                   eval      rivisual='S'
028700080529     c                   endif
028800080529     c
028900080529     c                   ENDSR
029000951025     C**************************************************************************
029100951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
029200951025     C**************************************************************************
029300951025     C     CTRFIL        BEGSR
029400020213
029500951025     C     CODFIL        CHAIN     AZORG01L                           34
029600951025    1C  N34ORGFVA        IFNE      ' '
029700951025     C     ORGFAG        ORNE      'A'
029800951025     C     ORGFAG        ANDNE     'F'
029900951025     C                   SETON                                        34
030000951025    1C                   ENDIF
030100951025     C*
030200951025     C                   ENDSR
030300020805      **************************************************************************
030400960302** SCHIERA MSG - MESSAGGI DI ERRORE
030500080529Data errata                                                                   05
030600080529Nessuna filiale scelta per l'elaborazione                                     05
