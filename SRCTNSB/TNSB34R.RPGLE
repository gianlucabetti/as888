000100940927     H****************************************************************
000200050705     H*             - storicizza file TILAC00F  -                    *
000300940927     H****************************************************************
000400050705     FTIlad00f  o    E             DISK
000500050705     D tnsb34ds      E DS
000600050705     D tiladds       E DS                  extname(tilad00f)
000700050705     D WLBDA8          DS
000800050705     D  G02DAT                 1      8  0
000900050705     D  G02INV                 9     16  0
001000050705     D  G02ERR                17     17
001100050705     D  G02TGI                18     22  0
001200050705     D comdta          DS
001300050705     D  comaa                  1      4
001400050705     D  commm                  5      6
001500050705     D  comgg                  7      8
001600120709     d**!!! comprg          s              2
001700940914     C*--------------------------------------------------------------*
001800940914     C*  CICLO PRINCIPALE
001900940914     C*--------------------------------------------------------------*
002000940927     C     *ENTRY        PLIST
002100050705     C                   PARM                    tnsb34ds
002200050630     c*
002300050630     c                   do
002400050630     c* chiudo in lr
002500050705     c                   if        sb34tla = 'C'
002600050630     c                   leave
002700050630     c                   end
002800050630     c*
002900050630     C* controlli passaggio parametri
003000050705     c                   if        sb34aas = 0  or
003100050705     c                             sb34lnp = 0  or
003200050705     c                             sb34nsp = 0
003300050705     c                   eval      sb34err = '1'
003400050630     c                   leave
003500050630     c                   else
003600050705     c                   clear                   sb34err
003700050630     c                   end
003800050630     c*
003900050705     c                   clear                   tiladds
004000050705     c                   move      sb34aas       ladaas
004100050705     c                   move      sb34lnp       ladlnp
004200050705     c                   move      sb34nrs       ladnrs
004300050705     c                   move      sb34nsp       ladnsp
004400050706     c                   eval      ladcop = sb34cop
004500050705     c                   time                    comtim           14 0
004600050705     c                   movel     comtim        comora            6
004700050705     C                   MOVE      comtim        G02DAT
004800050705     C                   MOVE      *ZEROS        G02INV
004900050705     C                   MOVEL     *BLANK        G02ERR
005000050705     C                   CALL      'XSRDA8'
005100050705     C                   PARM                    WLBDA8
005200050705     C                   move      G02INV        comdta
005300050705     c                   eval      ladtim = comaa + commm+ comgg + comora
005400050720     c                   move      sb34dtp       comdta
005500120709     c**!!!              move      sb34prg       comprg
005600071017     c                   eval      laddir = comdta + '/' +
005700050705     c                             ladlnp + '/' +
005800050705     c                             ladnrs
005900050705     c                   eval      ladela = '00'
006000050705     c* monitorizzo perchè se ho A3+F1 storicizzo una sola volta
006100050705     c                   write     tilad000                             99
006200050705     c                   enddo
006300050630     c*
006400050705     c                   if        sb34tla = 'C' or sb34tla = 'L'
006500050630     c                   seton                                        lr
006600050630     c                   else
006700050630     c                   seton                                        rt
006800050630     c                   end
