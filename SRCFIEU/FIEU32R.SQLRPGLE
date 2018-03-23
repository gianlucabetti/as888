000100010514     H DECEDIT('0,') DATEDIT(*DMY.)
000200010514      *----------------------------------------------------*
000300010515      * Pulizia ORM in sede
000400010514      *----------------------------------------------------*
000500010515     ftabel00f  if   e           k disk
000600010515
000700010515     d kcod            s                   like(tblcod)
000800010515     d kkey            s                   like(tblkey)
000900010515
001000010515
001100010515     d w0140           s             14  0
001200010515     d wdate           s              8  0
001300010514
001400010515     d wlbdat          ds                  inz
001500010515     d  g02dat                 1      8  0
001600010515     d  g02inv                 9     16  0
001700010515     d  g02err                17     17
001800010515     d  g02tgi                18     22  0
001900010515
002000010515     d wgidat          ds                  inz
002100010515     d  giodat                 1      8  0
002200010515     d  gioinv                 9     16  0
002300010515     d  giotgi                17     21  0
002400010515
002500010514     d kpjba         e ds
002600010515     d ds5as1        e ds
002700040928     d kkut            s                   like(tblkut) inz(1)
002800010514      *-------------------------------------------------------------------------*
002900010514     c     *entry        plist
003000010514     c                   parm                    kpjba
003100010514
003200010515     c     ktab          klist
003300040928     c                   kfld                    KKUT
003400010515     c                   kfld                    kcod
003500010515     c                   kfld                    kkey
003600040909
003700130628     c                   eval      *inlr = *on
003800130628
003900010515      * Lettura giorni di pulizia orm
004000010515     c                   clear                   ds5as1
004100010515     c                   movel     '5A'          kcod
004200010515     c                   movel     'SEDE1'       kkey
004300010515     c     ktab          chain     tabel00f
004400010515     c                   if        %found(tabel00f) and tblflg = *blanks
004500010515     c                   movel     tbluni        ds5as1
004600010515     c                   endif
004700130628
004800010515      * Ricavo il nr. giorni dalla data odierna
004900010515     c                   time                    w0140
005000010515     c                   move      w0140         wdate
005100010515     c                   move      wdate         g02dat
005200010515     c                   movel     *blanks       g02err
005300010515     c                   call      'XSRDA8'
005400010515     c                   parm                    wlbdat
005500010515      * Calcolo data
005600010515     c     g02tgi        sub       §5aors        giotgi
005700010515     c                   call      'XSRGI8'
005800010515     c                   parm                    wgidat
005900130628     c                   z-add     giodat        datorm            8 0
006000010515      * Giro la data
006100010515     c                   if        datorm > 0
006200010515     c                   move      datorm        g02dat
006300010515     c                   movel     *blanks       g02err
006400010515     c                   call      'XSRDA8'
006500010515     c                   parm                    wlbdat
006600010515     c                   if        g02err = '1'
006700010515     c                   z-add     0             datorm
006800010515     c                   else
006900010515     c                   z-add     g02inv        datorm
007000010515     c                   endif
007100010515     c                   endif
007200130628
007300130628     c                   if           datorm > 0
007400130628     C/EXEC SQL
007500130628     C+     DELETE
007600130628     C+     FROM fidpo00f WHERE
007700130628     C+     DPODTORM < :datorm  and DPODTR < :datorm  and DPODTRIC < :datorm
007800130628     C+     and
007900130628     C+     (DPODTORM > 0 or DPODTR > 0 or DPODTRIC > 0)
008000130628     C/END-EXEC
008100130628     c                   endif
008200040909
008300130628     c                   return
