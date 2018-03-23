004800940927     H****************************************************************
004900050705     H*             - storicizza file TILAC00F  -                    *
005000940927     H****************************************************************
005001050705     FTIlad00f  o    E             DISK
021500050705     D tnsb34ds      E DS
021501050705     D tiladds       E DS                  extname(tilad00f)
021502050705     D WLBDA8          DS
021503050705     D  G02DAT                 1      8  0
021504050705     D  G02INV                 9     16  0
021505050705     D  G02ERR                17     17
021506050705     D  G02TGI                18     22  0
021507050705     D comdta          DS
021508050705     D  comaa                  1      4
021509050705     D  commm                  5      6
021510050705     D  comgg                  7      8
021511050720     d comprg          s              2
028100940914     C*--------------------------------------------------------------*
028200940914     C*  CICLO PRINCIPALE
028300940914     C*--------------------------------------------------------------*
028400940927     C     *ENTRY        PLIST
028500050705     C                   PARM                    tnsb34ds
028601050630     c*
028602050630     c                   do
028603050630     c* chiudo in lr
028604050705     c                   if        sb34tla = 'C'
028605050630     c                   leave
028606050630     c                   end
028607050630     c*
070618050630     C* controlli passaggio parametri
070619050705     c                   if        sb34aas = 0  or
070620050705     c                             sb34lnp = 0  or
070623050705     c                             sb34nsp = 0
070624050705     c                   eval      sb34err = '1'
070625050630     c                   leave
070626050630     c                   else
070627050705     c                   clear                   sb34err
070628050630     c                   end
070629050630     c*
070630050705     c                   clear                   tiladds
070633050705     c                   move      sb34aas       ladaas
070634050705     c                   move      sb34lnp       ladlnp
070635050705     c                   move      sb34nrs       ladnrs
070636050705     c                   move      sb34nsp       ladnsp
070637050706     c                   eval      ladcop = sb34cop
070638050705     c                   time                    comtim           14 0
070639050705     c                   movel     comtim        comora            6
070641050705     C                   MOVE      comtim        G02DAT
070642050705     C                   MOVE      *ZEROS        G02INV
070643050705     C                   MOVEL     *BLANK        G02ERR
070644050705     C                   CALL      'XSRDA8'
070645050705     C                   PARM                    WLBDA8
070646050705     C                   move      G02INV        comdta
070647050705     c                   eval      ladtim = comaa + commm+ comgg + comora
070648050720     c                   move      sb34dtp       comdta
070649050720     c                   move      sb34prg       comprg
070650071017     c                   eval      laddir = comdta + '/' +
070652050705     c                             ladlnp + '/' +
070653050705     c                             ladnrs
070654050705     c                   eval      ladela = '00'
070655050705     c* monitorizzo perchè se ho A3+F1 storicizzo una sola volta
070656050705     c                   write     tilad000                             99
070657050705     c                   enddo
070666050630     c*
070667050705     c                   if        sb34tla = 'C' or sb34tla = 'L'
070668050630     c                   seton                                        lr
070669050630     c                   else
070670050630     c                   seton                                        rt
070671050630     c                   end
