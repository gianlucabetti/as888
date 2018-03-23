000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300090518     Ftrmz85d   CF   E             WORKSTN
000900020304     dkpjba          e ds
001000030630     d Tibs36Ds      e ds
001001150310     D TIBS34DS      E DS                                                       *Profili utente
001002150310     D DDATIUTE      E DS                                                       *Dati utente
001003150310     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001004150306     d Trmz85ds      e ds
001400070605     d dataiso         s               d   datfmt(*iso)
001500070605     d dataeur         s               d   datfmt(*eur)
001600070605     D WLBDA8          DS
001700070605     D  G02DAT                 1      8  0
001800070605     D  G02INV                 9     16  0
001900070605     D  G02ERR                17     17
002000070605     D  G02TGI                18     22  0
005000000000     C*---------------------------------------------------------------*
005200130123     c                   move      'N'           vmese
005201150313     c                   move      'N'           vdata0
005202150313     c                   move      'S'           vtipoela
005203150306     c*
005300031105     c                   do        *hival
005400031105     c                   exfmt     video1
005500031105     c                   clear                   $msg
005600031105     c   kc              leave
005700031105     c                   exsr      controv1
005800031105     c   96              iter
005900050623     c                   if        *inkf
006000050623     c                   exsr      elabora
006100050805     c                   leave
006200050623     c                   endif
006300050805     c                   enddo
007000150306     c*
008100020308     c                   seton                                        lr
008200050802     c*-------------------------------------------------
008300090518     c     elabora       begsr
008400050802     c*-------------------------------------------------
020100150306     c*
020101150306     c                   clear                   trmz85ds
020102150306     c                   eval      i85data0= vdata0
020103150306     c                   eval      i85tipoela= vtipoela
020104150306     c                   eval      i85mese= vmese
020105150306     c                   eval      i85datal = vdatalg
020106150306     c                   eval      i85datdal= vdatdalg
020107150306     c                   movel(p)  trmz85ds      kpjbu
020108150306     C                   MOVEL     'MZ8A'        KCOAZ
020110150306     c                   call      'BCH10'
020111150306     C                   PARM                    KPJBA
020112150306     c*
020200070530     c                   endsr
020300031105     c*-------------------------------------------------
020400031105     c     controv1      begsr
020500031105     c*-------------------------------------------------
020600070606     c                   setoff                                       9697
020700070605      *data disaccr. per turnover
020800070605     c                   setoff                                       44
020900090518     c                   if        vdatdal > 0  and vdatal > 0
021000070605     c                   seton                                        4496
021100070605     c                   goto      endcontr
021200070605     c                   endif
021300090518     c                   if        vdatal > 0
021400090518     C                   MOVE      Vdatal        G02DAT
021500070605     C                   MOVEL     *BLANK        G02ERR
021600070605     C                   CALL      'XSRDA8'
021700070605     C                   PARM                    WLBDA8
021800070605     C     G02ERR        IFEQ      '1'
021900070605     C                   SETON                                        4295
022000070605     c                   goto      endcontr
022100070605     C                   END
022200090518     c                   move      g02dat        vdatal
022300090518     c                   move      g02inv        vdatalg           8 0
022501150306     c                   else
022502150306     c                   clear                   vdatalg           8 0
022600070529     c                   endif
022700051121
022800090518     c                   if        vdatdal > 0
022900090518     C                   MOVE      Vdatdal       G02DAT
023000090518     C                   MOVEL     *BLANK        G02ERR
023100090518     C                   CALL      'XSRDA8'
023200090518     C                   PARM                    WLBDA8
023300090518     C     G02ERR        IFEQ      '1'
023400090518     C                   SETON                                        4195
023500090518     c                   goto      endcontr
023600090518     C                   END
023700090518     c                   move      g02dat        vdatdal
023800090518     c                   move      g02inv        vdatdalg          8 0
023801150306     c                   else
023802150306     c                   clear                   vdatdalg          8 0
023900090518     c                   endif
024000051122     c     endcontr      endsr
025000011026     c*-------------------------------------------------
025100011026     c     *inzsr        begsr
025200011026     c*-------------------------------------------------
025300020304     c     *entry        plist
025400020304     c                   parm                    kpjba
025500050801
026400020218     C                   time                    w0120            14 0
026500020218     C                   move      w0120         wdat              8 0
026600030630     C                   CLEAR                   Tibs36Ds
026700030630     C                   EVAL      I36ute = knmus
026800030630     C                   CALL      'TIBS36R'
026900030630     C                   PARM                    tibs36ds
026901150310     C* INIZIALIZZA VARIABILI DI WRK
026902150310     C                   CLEAR                   TIBS34DS
026903150310     C                   CLEAR                   AZUTEDS
026904150310     C                   CLEAR                   DDATIUTE
026905150310     C*
026906150310     C     *DTAARA       DEFINE    §azute        azuteds
026907150310     C     *DTAARA       DEFINE    §datiute      ddatiute
026908150310     C                   IN(E)     *DTAARA
026909150310     C                   IF        %Error
026910150310     C                   EVAL      I34Tla = 'L'
026911150310     C                   CALL      'TIBS34R'
026912150310     C                   PARM                    Tibs34Ds
026913150310     C                   IN        *DTAARA
026914150310     C                   ENDIF
026915150310     C*
028300090518     c                   endsr
