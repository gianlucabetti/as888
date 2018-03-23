000100020220     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200010514      *----------------------------------------------------*
000300020220      * Pulizia bolle rigenerate SEDE
000400010514      *----------------------------------------------------*
000500090421     ffnrig01l  uf   e           k disk
000600010515     ftabel00f  if   e           k disk
000700010515
000800010515     d kcod            s                   like(tblcod)
000900010515     d kkey            s                   like(tblkey)
001000010515
001100090421     d wdtpul          s                   like(rigdela)
001200090421     d wrigdue         s                   like(rigdela)  inz(1)
001300020220
001400010515     d w0140           s             14  0
001500010515     d wdate           s              8  0
001600010514
001700010515     d wlbdat          ds                  inz
001800010515     d  g02dat                 1      8  0
001900010515     d  g02inv                 9     16  0
002000010515     d  g02err                17     17
002100010515     d  g02tgi                18     22  0
002200010515
002300010515     d wgidat          ds                  inz
002400010515     d  giodat                 1      8  0
002500010515     d  gioinv                 9     16  0
002600010515     d  giotgi                17     21  0
002700010515
002800010514     d kpjba         e ds
002900010515     d ds5as1        e ds
003000010514     d ut§dse0f      e ds
003100010514     d cncr80        e ds
003200010514
003300010514      *-------------------------------------------------------------------------*
003400010514
003500090421     c     wrigdue       setll     fnrig01l
003600010514     c                   do        *hival
003700090421     c                   read      fnrig01l
003800020220      * Fine file
003900090421     c                   if        %eof(fnrig01l)
004000010514     c                   leave
004100010514     c                   endif
004200020220      * Escludo record ancora da elaborare
004300090421     c                   if        rigdela = 0
004400020220     c                   iter
004500020220     c                   endif
004600010515
004700020220      * Se trascorsi i tot.giorni dalla data di elaborazione
004800090421     c                   if        rigdela <= wdtpul
004900090421     c                   delete    fnrig000
005000010514     c                   endif
005100010514
005200010514     c                   enddo
005300010515
005400010515     c                   eval      *inlr = *on
005500010514
005600010514      *-------------------------------------------------------------------------*
005700010514      * Subroutine iniziale
005800010514      *-------------------------------------------------------------------------*
005900010514     c     *inzsr        begsr
006000010514
006100010514     c     *entry        plist
006200010514     c                   parm                    kpjba
006300010514
006400010514     c                   z-add     1             codut
006500010514     c                   call      'X§PARUT'
006600010514     c                   parm                    ut§dse0f
006700010514     c                   movel     rec80         cncr80
006800010515
006900010515     c     ktab          klist
007000010515     c                   kfld                    codut
007100010515     c                   kfld                    kcod
007200010515     c                   kfld                    kkey
007300010515
007400020220      * Lettura giorni di pulizia rigenerazione bolle SEDE
007500010515     c                   clear                   ds5as1
007600010515     c                   movel     '5A'          kcod
007700010515     c                   movel     'SEDE1'       kkey
007800010515     c     ktab          chain     tabel00f
007900010515     c                   if        %found(tabel00f) and tblflg = *blanks
008000010515     c                   movel     tbluni        ds5as1
008100010515     c                   endif
008200010515
008300010515      * Ricavo il nr. giorni dalla data odierna
008400010515     c                   time                    w0140
008500010515     c                   move      w0140         wdate
008600010515     c                   move      wdate         g02dat
008700010515     c                   movel     *blanks       g02err
008800010515     c                   call      'XSRDA8'
008900010515     c                   parm                    wlbdat
009000010515
009100010515      * Calcolo data
009200020221     c     g02tgi        sub       §5arig        giotgi
009300010515     c                   call      'XSRGI8'
009400010515     c                   parm                    wgidat
009500020220     c                   z-add     giodat        wdtpul
009600010514
009700010515      * Giro la data
009800020220     c                   if        wdtpul > 0
009900020220     c                   move      wdtpul        g02dat
010000010515     c                   movel     *blanks       g02err
010100010515     c                   call      'XSRDA8'
010200010515     c                   parm                    wlbdat
010300010515     c                   if        g02err = '1'
010400020220     c                   z-add     0             wdtpul
010500010515     c                   else
010600020220     c                   z-add     g02inv        wdtpul
010700010515     c                   endif
010800010515     c                   endif
010900010515
011000010514     c                   endsr
