000100990426     H DECEDIT('0,') DATEDIT(*DMY.)
000200990426      * ---------------------------------------------------------
000300030910     fTnvrpf1D  CF   E             WORKSTN
000400990426     fAZORG01L  IF   E           K DISK
000500030507
000600990428     d TIBS69DS      E DS
000700990426     d DS_cnaco      E DS                  extname(CNACO00F)
000800990426     d DS_cnind      E DS                  extname(CNIND00F)
000900990426     d DS_cnclp      E DS                  extname(CNCLP00F)
001000990426     d DS_fncls      E DS                  extname(FNCLS00F)
001100990426     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200990426     d  tcu                  398    697    DIM(50)
001300990426     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
001400990426     D CNCR80        E DS
001500990426     D KPJBA         E DS
001600990426
001700990426     D WLBDAT          DS                  INZ
001800990426     D  G02DAT                 1      8  0
001900990426     D  G02INV                 9     16  0
002000990426     D  G02ERR                17     17
002100990426     D  G02TGI                18     22  0
002200990426
002300990426     d TCUDS           DS
002400990426     d  f1                     1      1
002500990426     d  f3                     3      3
002600990426     d  f2                     2      2
002700990426     d  f4                     4      4
002800990426     d  f56                    5      6
002900020130      *
003000030508     d PARAM           DS
003100030508     d  PARKSC                        7  0
003200030508     d  PARDDS                        8  0
003300030508     d  PARADS                        8  0
003400030508     d  PARLNP                        3  0
003500030508     d  PARLNA                        3  0
003600030910     d  PARPUL                        2
003700030910     d  PARINV                        2
003800030508      *
003900030507     D wtibs69r        s              1
004000030508     D INVDDS          s              8  0
004100030508     D INVADS          s              8  0
004200030508     D XX              s              2  0
004300990426      * ---------------------------------------------------------
004400990427
004500990427      *            G E S T I O N E   V I D E O  1
004600030507     C                   DO        *HIVAL
004700030509      *
004800030509     c                   setoff                                       28
004900990427      *
005000030507     C                   EXFMT     VIDEO01
005100990427      *
005200990428      * F3  = FINE
005300990427     C                   IF        *INKC = *ON
005400030507     C                   LEAVE
005500990427     C                   ENDIF
005600990427
005700990427     c                   EXSR      CTRD01
005800990427
005900030507     C                   IF        *IN28 = *ON
006000030507     C                   ITER
006100990427     C                   ENDIF
006200030508      *
006300030508      * F6  = CONFERMA
006400030508     C                   IF        *INKF = *ON
006500030508     C                   MOVEL     PARAM         KPJBU
006600030910     C                   CALL      'TNVRPF2C'
006700030508     C                   PARM                    KPJBA
006800030508     C                   LEAVE
006900030508     C                   ENDIF
007000030507
007100030507     C                   ENDDO
007200990426      *
007300990426     C                   IF        Wtibs69r  = 'S'
007400990428     C                   clear                   TIBS69DS
007500990426     C                   EVAL      I69TLA  = 'C'
007600990426     C                   CALL      'TIBS69R'
007700990426     C                   PARM                    TIBS69DS
007800990426     C                   ENDIF
007900990426      *
008000990426     C                   EVAL      *INLR = *ON
008100990427      **********************************************************************
008200990427      *  CONTROLLI VIDEATA 1
008300990427      **********************************************************************
008400990427     C     CTRD01        BEGSR
008500030507
008600030507      * CONTROLLO CODICE CLIENTE
008700030507     c                   IF        VIDKSC > *ZEROS
008800030507      *   se codificato decodifico da p.d.c.
008900030507     c                   clear                   ACOrag
009000030507     C                   clear                   TIBS69DS
009100030507     c                   z-add     VIDksc        I69kac
009200030507     C                   CALL      'TIBS69R'
009300030507     C                   PARM                    tibs69DS
009400030507     C                   PARM                    DS_cnaco
009500030507     C                   PARM                    DS_cnind
009600030507     C                   PARM                    DS_cnclp
009700030507     C                   PARM                    DS_fncls
009800030507      *
009900030507     C                   IF         O69ERR =  '1'
010000030507     C                   SETON                                        2850
010100030507     C                   ENDIF
010200030507      *
010300030507     C                   eval      wtibs69r = 'S'
010400030507      *
010500030507     c  N50              movel     ACOrag        VIDRAG
010600030507     C                   ENDIF
010700990427
010800030507      * CONTROLLO LA DATA SPEDIZIONE
010900030508
011000030508     C                   CLEAR                   INVDDS
011100030508     c                   if        viddds > 0  and *in28 = *off
011200030508     C                   CLEAR                   WLBDAT
011300030508     C                   Z-ADD     vIddds        G02DAT
011400030508     C                   CALL      'XSRDA8'
011500030508     C                   PARM                    WLBDAT
011600030508    1C     G02ERR        IFEQ      '1'
011700030508     C                   SETON                                        5128
011800030508   X1C                   ELSE
011900030508     C                   MOVE      G02INV        INVdds
012000030508     C                   MOVE      G02dat        viddds
012100030508    1C                   END
012200030508     c                   endif
012300030508
012400030508      * CONTROLLO LA DATA SPEDIZIONE
012500030508
012600030508     C                   CLEAR                   INVADS
012700030508     c                   if        vidads > 0  and  *in28 = *off
012800030508     C                   CLEAR                   WLBDAT
012900030508     C                   Z-ADD     vIdads        G02DAT
013000030508     C                   CALL      'XSRDA8'
013100030508     C                   PARM                    WLBDAT
013200030508    1C     G02ERR        IFEQ      '1'
013300030508     C                   SETON                                        5228
013400030508   X1C                   ELSE
013500030508     C                   MOVE      G02INV        INVads
013600030508     C                   MOVE      G02dat        vidads
013700030508    1C                   END
013800030508     c                   endif
013900030508      * verifico se le date sono corrette
014000030508     c                   if        *in28 = *off and invads > 0 and  invdds > 0
014100030508     c                             and invads < invdds
014200030508     C                   SETON                                        5528
014300030508     c                   endif
014400030508
014500030508      * CONTROLLO LINNE PARTENZA ED ARRIVO
014600030508
014700030508     c                   if        vidLNP > 0  and *in28 = *off
014800030508     C     VIDLNP        CHAIN     AZORG01L
014900030508     C                   IF        NOT %FOUND(AZORG01L)
015000030508     C                   SETON                                        5328
015100030508     C                   ELSE
015200030508     C                   MOVEL     ORGDES        VIDDLP
015300030508     C                   ENDIF
015400030508     C                   ENDIF
015500030508
015600030508     c                   if        vidLNA > 0  and *in28 = *off
015700030508     C     VIDLNA        CHAIN     AZORG01L
015800030508     C                   IF        NOT %FOUND(AZORG01L)
015900030508     C                   SETON                                        5428
016000030508     C                   ELSE
016100030508     C                   MOVEL     ORGDES        VIDDLA
016200030508     C                   ENDIF
016300030508     C                   ENDIF
016400030508
016500030508     C                   IF        *IN28 = *OFF
016600030508     C                   EVAL      PARKSC = VIDKSC
016700030508     C                   EVAL      PARDDS = INVDDS
016800030508     C                   EVAL      PARADS = INVADS
016900030508     C                   EVAL      PARLNP = VIDLNP
017000030508     C                   EVAL      PARLNA = VIDLNA
017100030910     C                   EVAL      PARPUL = VIDPUL
017200030910     C                   EVAL      PARINV = VIDINV
017300030508     C                   ENDIF
017400030508
017500990427     c     ECTRD01       ENDSR
017600010905
017700990426      *****************************************************************
017800990426      *  ROUTINE INIZIALE
017900990426      *****************************************************************
018000990426     C     *INZSR        BEGSR
018100990426
018200990426     C     *ENTRY        PLIST
018300990426     C                   PARM                    KPJBA
018400990503
018500990426      * Dati società
018600990426     C                   Z-ADD     1             CODUT
018700990426     C                   CALL      'X§PARUT'
018800990426     C                   PARM                    UT§DSE
018900990426      *
019000990426     C                   MOVEL     RAGUT         RSUT
019100990426     C                   MOVEL     REC80         CNCR80
019200990426      *
019300990426      *--- RICERCA CAPOCONTI
019400990426     C                   DO        50            XX
019500990426     C                   MOVE      TCU(XX)       TCUDS
019600990426     C     f56           CABNE     'CG'          END1
019700990426     C     f4            COMP      '1'                                    21
019800990426     C     f4            COMP      '2'                                    22
019900990426     C     f4            COMP      '3'                                    23
020000990426     C     f4            COMP      '6'                                    27
020100990426     C** 1 CLIENTI   21
020200990426     C** 2 FORNITORI 22
020300990426     C** 3 AGENTI    23
020400990426     C     f3            comp      '0'                                242425
020500990426     C     f3            comp      'I'                                    26
020600990426     C** 0 ITALIA   25
020700990426     C** 1 ESTERO   24
020800990426     ** I CAPO CONTO IVA
020900990426     c                   if        *in21 = *on
021000990426     C   24              Z-ADD     KCU(XX)       KCE               4 0
021100990426     C   25              Z-ADD     KCU(XX)       KCI               4 0
021200990426     c                   end
021300990426     c                   if        *in22 = *on
021400990426     C   24              Z-ADD     KCU(XX)       KFE               4 0
021500990426     C   25              Z-ADD     KCU(XX)       KFI               4 0
021600990426     c                   end
021700990426     c                   if        *in23 = *on
021800990426     C   24              Z-ADD     KCU(XX)       KAE               4 0
021900990426     C   25              Z-ADD     KCU(XX)       KAI               4 0
022000990426     c                   end
022100990426     C   26              Z-ADD     KCU(XX)       KIVA              4 0
022200990426     C   27              Z-ADD     KCU(XX)       KBNA              4 0
022300990426     C     end1          tag
022400990426     C                   end
022500990426     C                   SETOFF                                       212223
022600990426     C                   SETOFF                                       242526
022700990426     C                   SETOFF                                       27
022800030910      *
022900030910     c                   eval      vidpul = 'NO'
023000990427      *
023100030910     c                   eval      vidinv = 'NO'
023200030910      *
023300990426     C                   ENDSR
023400990426      *------------------------------------------------------------
