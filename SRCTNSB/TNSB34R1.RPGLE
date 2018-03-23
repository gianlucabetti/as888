000100940927     H****************************************************************
000200120709     H*             - storicizza file TIJDC00F  -                    *
000300940927     H****************************************************************
000400120709     FTIjdd00f  o    E             DISK
000500050705     D tnsb34ds      E DS
000600120709     D tijddds       E DS                  extname(tijdd00f)
000700050705     D WLBDA8          DS
000800050705     D  G02DAT                 1      8  0
000900050705     D  G02INV                 9     16  0
001000050705     D  G02ERR                17     17
001100050705     D  G02TGI                18     22  0
001200050705     D comdta          DS
001300050705     D  comaa                  1      4
001400050705     D  commm                  5      6
001500050705     D  comgg                  7      8
001600940914     C*--------------------------------------------------------------*
001700940914     C*  CICLO PRINCIPALE
001800940914     C*--------------------------------------------------------------*
001900940927     C     *ENTRY        PLIST
002000050705     C                   PARM                    tnsb34ds
002100050630     c*
002200050630     c                   do
002300050630     c* chiudo in lr
002400050705     c                   if        sb34tla = 'C'
002500050630     c                   leave
002600050630     c                   end
002700050630     c*
002800050630     C* controlli passaggio parametri
002900050705     c                   if        sb34aas = 0  or
003000050705     c                             sb34lnp = 0  or
003100050705     c                             sb34nsp = 0
003200050705     c                   eval      sb34err = '1'
003300050630     c                   leave
003400050630     c                   else
003500050705     c                   clear                   sb34err
003600050630     c                   end
003700050630     c*
003800120709     c                   clear                   tijddds
003900050705     c                   move      sb34aas       ladaas
004000050705     c                   move      sb34lnp       ladlnp
004100050705     c                   move      sb34nrs       ladnrs
004200050705     c                   move      sb34nsp       ladnsp
004300050706     c                   eval      ladcop = sb34cop
004400050705     c                   time                    comtim           14 0
004500050705     c                   movel     comtim        comora            6
004600050705     C                   MOVE      comtim        G02DAT
004700050705     C                   MOVE      *ZEROS        G02INV
004800050705     C                   MOVEL     *BLANK        G02ERR
004900050705     C                   CALL      'XSRDA8'
005000050705     C                   PARM                    WLBDA8
005100050705     C                   move      G02INV        comdta
005200050705     c                   eval      ladtim = comaa + commm+ comgg + comora
005300050720     c                   move      sb34dtp       comdta
005400120709     c                   eval      laddir = 'DI/' + comdta + '/' +
005500050705     c                             ladlnp + '/' +
005600050705     c                             ladnrs
005700050705     c                   eval      ladela = '00'
005800050705     c* monitorizzo perchè se ho A3+F1 storicizzo una sola volta
005900120709     c                   write     tijdd000                             99
006000050705     c                   enddo
006100050630     c*
006200050705     c                   if        sb34tla = 'C' or sb34tla = 'L'
006300050630     c                   seton                                        lr
006400050630     c                   else
006500050630     c                   seton                                        rt
006600050630     c                   end
