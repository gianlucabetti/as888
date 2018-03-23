000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300151103     Ftntl88d   CF   E             WORKSTN
000400020304     dkpjba          e ds
000500030630     d Tibs36Ds      e ds
000600150310     D TIBS34DS      E DS                                                       *Profili utente
000700150310     D DDATIUTE      E DS                                                       *Dati utente
000800150310     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
000900151103     d Tntl88ds      e ds
001000070605     d dataiso         s               d   datfmt(*iso)
001100070605     d dataeur         s               d   datfmt(*eur)
001200070605     D WLBDA8          DS
001300070605     D  G02DAT                 1      8  0
001400070605     D  G02INV                 9     16  0
001500070605     D  G02ERR                17     17
001600070605     D  G02TGI                18     22  0
001700000000     C*---------------------------------------------------------------*
001800130123     c                   move      'N'           vmese
001900150527     c                   move      'F'           vtipoela
002000150306     c*
002100031105     c                   do        *hival
002200031105     c                   exfmt     video1
002300031105     c                   clear                   $msg
002400031105     c   kc              leave
002500031105     c                   exsr      controv1
002600031105     c   96              iter
002700050623     c                   if        *inkf
002800050623     c                   exsr      elabora
002900050805     c                   leave
003000050623     c                   endif
003100050805     c                   enddo
003200150306     c*
003300020308     c                   seton                                        lr
003400050802     c*-------------------------------------------------
003500090518     c     elabora       begsr
003600050802     c*-------------------------------------------------
003700150306     c*
003800151103     c                   clear                   tntl88ds
003900151103     c                   eval      i88tipoel= vtipoela
004000151103     c                   eval      i88mese= vmese
004100151103     c                   eval      i88datal = vdatalg
004200151103     c                   eval      i88datdal= vdatdalg
004300151103     c                   movel(p)  tntl88ds      kpjbu
004400151103     C                   MOVEL     'TL8A'        KCOAZ
004500151103     c                   call      'BCH10'
004501151103     c*                  call      'TNTL88R1'
004600150306     C                   PARM                    KPJBA
004700150306     c*
004800070530     c                   endsr
004900031105     c*-------------------------------------------------
005000031105     c     controv1      begsr
005100031105     c*-------------------------------------------------
005200070606     c                   setoff                                       9697
005300070605      *data disaccr. per turnover
005400070605     c                   setoff                                       44
005500090518     c                   if        vdatdal > 0  and vdatal > 0
005600070605     c                   seton                                        4496
005700070605     c                   goto      endcontr
005800070605     c                   endif
005900090518     c                   if        vdatal > 0
006000090518     C                   MOVE      Vdatal        G02DAT
006100070605     C                   MOVEL     *BLANK        G02ERR
006200070605     C                   CALL      'XSRDA8'
006300070605     C                   PARM                    WLBDA8
006400070605     C     G02ERR        IFEQ      '1'
006500070605     C                   SETON                                        4295
006600070605     c                   goto      endcontr
006700070605     C                   END
006800090518     c                   move      g02dat        vdatal
006900090518     c                   move      g02inv        vdatalg           8 0
007000150306     c                   else
007100150306     c                   clear                   vdatalg           8 0
007200070529     c                   endif
007300051121
007400090518     c                   if        vdatdal > 0
007500090518     C                   MOVE      Vdatdal       G02DAT
007600090518     C                   MOVEL     *BLANK        G02ERR
007700090518     C                   CALL      'XSRDA8'
007800090518     C                   PARM                    WLBDA8
007900090518     C     G02ERR        IFEQ      '1'
008000090518     C                   SETON                                        4195
008100090518     c                   goto      endcontr
008200090518     C                   END
008300090518     c                   move      g02dat        vdatdal
008400090518     c                   move      g02inv        vdatdalg          8 0
008500150306     c                   else
008600150306     c                   clear                   vdatdalg          8 0
008700090518     c                   endif
008800051122     c     endcontr      endsr
008900011026     c*-------------------------------------------------
009000011026     c     *inzsr        begsr
009100011026     c*-------------------------------------------------
009200020304     c     *entry        plist
009300020304     c                   parm                    kpjba
009400050801
009500020218     C                   time                    w0120            14 0
009600020218     C                   move      w0120         wdat              8 0
009700030630     C                   CLEAR                   Tibs36Ds
009800030630     C                   EVAL      I36ute = knmus
009900030630     C                   CALL      'TIBS36R'
010000030630     C                   PARM                    tibs36ds
010100150310     C* INIZIALIZZA VARIABILI DI WRK
010200150310     C                   CLEAR                   TIBS34DS
010300150310     C                   CLEAR                   AZUTEDS
010400150310     C                   CLEAR                   DDATIUTE
010500150310     C*
010600150310     C     *DTAARA       DEFINE    §azute        azuteds
010700150310     C     *DTAARA       DEFINE    §datiute      ddatiute
010800150310     C                   IN(E)     *DTAARA
010900150310     C                   IF        %Error
011000150310     C                   EVAL      I34Tla = 'L'
011100150310     C                   CALL      'TIBS34R'
011200150310     C                   PARM                    Tibs34Ds
011300150310     C                   IN        *DTAARA
011400150310     C                   ENDIF
011500150310     C*
011600090518     c                   endsr
