000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300150310     Ftrmz63d   CF   E             WORKSTN
000400020304     dkpjba          e ds
000500030630     d Tibs36Ds      e ds
000600150310     d Trmz63ds      e ds
000700070605     d dataiso         s               d   datfmt(*iso)
000800070605     d dataeur         s               d   datfmt(*eur)
000900070605     D WLBDA8          DS
001000070605     D  G02DAT                 1      8  0
001100070605     D  G02INV                 9     16  0
001200070605     D  G02ERR                17     17
001300070605     D  G02TGI                18     22  0
001400000000     C*---------------------------------------------------------------*
001500150310     c                   eval      vtipoela = 'E'
001600150306     c*
001700031105     c                   do        *hival
001800031105     c                   exfmt     video1
001900031105     c                   clear                   $msg
002000031105     c   kc              leave
002100031105     c                   exsr      controv1
002200031105     c   96              iter
002300050623     c                   if        *inkf
002400050623     c                   exsr      elabora
002500050805     c                   leave
002600050623     c                   endif
002700050805     c                   enddo
002800150306     c*
002900020308     c                   seton                                        lr
003000050802     c*-------------------------------------------------
003100090518     c     elabora       begsr
003200050802     c*-------------------------------------------------
003300150306     c*
003400150310     c                   clear                   trmz63ds
003500150310     c                   eval      i63tipoela= vtipoela
003600150310     c                   eval      i63datdal= vdatdalg
003700150310     c                   eval      i63datal = vdatalg
003800150310     c                   movel(p)  trmz63ds      kpjbu
003900150310     C                   MOVEL     'MZ6A'        KCOAZ
004000150306     c                   call      'BCH10'
004100150306     C                   PARM                    KPJBA
004200150306     c*
004300070530     c                   endsr
004400031105     c*-------------------------------------------------
004500031105     c     controv1      begsr
004600031105     c*-------------------------------------------------
004700070606     c                   setoff                                       9697
004800150310      *
004900150310     c                   if        vdatdal > 0
005000150310     C                   MOVE      Vdatdal       G02DAT
005100150310     C                   MOVEL     *BLANK        G02ERR
005200150310     C                   CALL      'XSRDA8'
005300150310     C                   PARM                    WLBDA8
005400150310     C     G02ERR        IFEQ      '1'
005500150310     C                   SETON                                        4295
005600150310     c                   goto      endcontr
005700150310     C                   END
005800150310     c                   move      g02dat        vdatdal
005900150310     c                   move      g02inv        vdatdalg          8 0
006000150310     c                   else
006100150310     c                   clear                   vdatdalg          8 0
006200150310     c                   endif
006300150310      *
006400090518     c                   if        vdatal > 0
006500090518     C                   MOVE      Vdatal        G02DAT
006600070605     C                   MOVEL     *BLANK        G02ERR
006700070605     C                   CALL      'XSRDA8'
006800070605     C                   PARM                    WLBDA8
006900070605     C     G02ERR        IFEQ      '1'
007000150310     C                   SETON                                        4195
007100070605     c                   goto      endcontr
007200070605     C                   END
007300090518     c                   move      g02dat        vdatal
007400090518     c                   move      g02inv        vdatalg           8 0
007500150306     c                   else
007600150306     c                   clear                   vdatalg           8 0
007700070529     c                   endif
007800150310      *  range di date
007900150310     c                   if        vdatdalg > vdatalg
008000150310     c                   seton                                        4396
008100150310     c                   goto      endcontr
008200150310     c                   endif
008300150310      *
008400051122     c     endcontr      endsr
008500011026     c*-------------------------------------------------
008600011026     c     *inzsr        begsr
008700011026     c*-------------------------------------------------
008800020304     c     *entry        plist
008900020304     c                   parm                    kpjba
009000050801
009100020218     C                   time                    w0120            14 0
009200020218     C                   move      w0120         wdat              8 0
009300030630     C                   CLEAR                   Tibs36Ds
009400030630     C                   EVAL      I36ute = knmus
009500030630     C                   CALL      'TIBS36R'
009600030630     C                   PARM                    tibs36ds
009700150310      *
009800090518     c                   endsr
009900150310     c*-------------------------------------------------
