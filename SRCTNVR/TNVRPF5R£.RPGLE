000100990426     H DECEDIT('0,') DATEDIT(*DMY.)
000200990426      * ---------------------------------------------------------
000300050207     fTnvrpf3D  CF   E             WORKSTN
000400990426     fAZORG01L  IF   E           K DISK
000401050203     ftitas30c  IF   E           K DISK
000500030507
000600990428     d TIBS69DS      E DS
000700990426     d DS_cnaco      E DS                  extname(CNACO00F)
000800990426     d DS_cnind      E DS                  extname(CNIND00F)
000900990426     d DS_cnclp      E DS                  extname(CNCLP00F)
001000990426     d DS_fncls      E DS                  extname(FNCLS00F)
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
003100050203     d  PARKSC                        7s 0
003200050203     d  PARDDS                        8s 0
003300050203     d  PARADS                        8s 0
003400050203     d  PARLNP                        3s 0
003500050203     d  PARLNA                        3s 0
003600050203     d  PARPUL                        2
003700050203     d  PARINV                        2
003701050203     d  PARaass                       4s 0
003702050203     d  PARlnps                       3s 0
003703050203     d  PARnrss                       2s 0
003704050203     d  PARnsps                       7s 0
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
006600050207     C                   CALL      'TNVRPF4C'
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
008501050207     c                   setoff                                       505152
008502050207     c                   setoff                                       535455
008503050207     c                   setoff                                       5657
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
016301050207     C* se immessa spedizione verifico se esiste e obbligo la scelta solo
016302050207     c* di questa
016304050207     c                   if        aasv <> 0 or lnpv <> 0 or nrsv <> 0 or
016400050207     c                             nspv <> 0
016402050207     c     ktitas        chain     titas30c
016403050207     c                   if        not %found(titas30c)
016404050207     c                   seton                                        5628
016405050207     c                   end
016406050207     c*
016407050207     c                   if        vidksc <> 0 or invdds <> 0 or invads <> 0
016408050207     c                             or vidlnp <> 0 or vidlna <> 0
016409050207     c                   seton                                        5728
016410050207     c                   end
016411050207     c                   end
016412050207     c*
016500030508     C                   IF        *IN28 = *OFF
016600030508     C                   EVAL      PARKSC = VIDKSC
016700030508     C                   EVAL      PARDDS = INVDDS
016800030508     C                   EVAL      PARADS = INVADS
016900030508     C                   EVAL      PARLNP = VIDLNP
017000030508     C                   EVAL      PARLNA = VIDLNA
017100030910     C                   EVAL      PARPUL = VIDPUL
017200030910     C                   EVAL      PARINV = VIDINV
017201050207     C                   EVAL      PARaass= aasv
017202050207     C                   EVAL      PARlnps= lnpv
017203050207     C                   EVAL      PARnrss= nrsv
017204050207     C                   EVAL      PARnsps= nspv
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
022800030910      *
022900030910     c                   eval      vidpul = 'NO'
023000990427      *
023100030910     c                   eval      vidinv = 'NO'
023101050207      *
023102050207     c     ktitas        klist
023103050207     c                   kfld                    aasv
023104050207     c                   kfld                    lnpv
023105050207     c                   kfld                    nrsv
023106050207     c                   kfld                    nspv
023200030910      *
023300990426     C                   ENDSR
023400990426      *------------------------------------------------------------
