000100011102     H DECEDIT('0,') DATEDIT(*DMY.)
000200011102      *----------------------------------------------------*
000300011105      * Pulizia file TIORI00F (solo in sede)
000400011102      *----------------------------------------------------*
000500011105     ftiori04l  uf   e           k disk
000600011102     ftabel00f  if   e           k disk
000700011102
000800011102     d kcod            s                   like(tblcod)
000900011102     d kkey            s                   like(tblkey)
001000011102
001100011102     d datori          s                   like(orider)
001200011102
001300011102     d w0140           s             14  0
001400011102     d wdate           s              8  0
001500011105
001600011105     d $OK_GG          s              1
001700011102
001800011102     d wlbdat          ds                  inz
001900011102     d  g02dat                 1      8  0
002000011102     d  g02inv                 9     16  0
002100011102     d  g02err                17     17
002200011102     d  g02tgi                18     22  0
002300011102
002400011102     d wgidat          ds                  inz
002500011102     d  giodat                 1      8  0
002600011102     d  gioinv                 9     16  0
002700011102     d  giotgi                17     21  0
002800011102
002900011102     d kpjba         e ds
003000011102     d ds5as1        e ds
003100011102     d ut§dse0f      e ds
003200011102     d cncr80        e ds
003300011102
003400011102      *-------------------------------------------------------------------------*
003500011102
003600011105DO  1c                   dow       $OK_GG = *on
003700011105
003800011102     c                   read      tiori04l
003900011102
004000011102      * Se trascorsi i tot.giorni dalla data di elaborazione
004100011105IF  2c                   if        not %eof(tiori04l)
004200011105     c                             and orider <= datori
004300011105     c                   delete    tiori000
004400011102X   2c                   else
004500011102     c                   leave
004600011102E   2c                   endif
004700011102
004800011102E   1c                   enddo
004900011102
005000011102     c                   eval      *inlr = *on
005100011102
005200011102      *-------------------------------------------------------------------------*
005300011102      * Subroutine iniziale
005400011102      *-------------------------------------------------------------------------*
005500011102     c     *inzsr        begsr
005600011102
005700011102     c     *entry        plist
005800011102     c                   parm                    kpjba
005900011102
006000011102     c                   z-add     1             codut
006100011102     c                   call      'X§PARUT'
006200011102     c                   parm                    ut§dse0f
006300011102     c                   movel     rec80         cncr80
006400011102
006500011102     c     ktab          klist
006600011102     c                   kfld                    codut
006700011102     c                   kfld                    kcod
006800011102     c                   kfld                    kkey
006900011102
007000011102      * Lettura giorni di pulizia orm
007100011102     c                   clear                   ds5as1
007200011102     c                   movel     '5A'          kcod
007300011102     c                   movel     'SEDE1'       kkey
007400011102     c     ktab          chain     tabel00f
007500011102IF  1c                   if        %found(tabel00f) and tblflg = *blanks
007600011105     c                   movel     tbluni        ds5As1
007700011102E   1c                   endif
007800011105IF  1c                   if        §5Aori > 0
007900011105     c                   movel     *on           $OK_GG
008000011105X   1c                   else
008100011105     c                   movel     *off          $OK_GG
008200011105E   1c                   endif
008300011102
008400011102      * Ricavo il nr. giorni dalla data odierna
008500011102     c                   time                    w0140
008600011102     c                   move      w0140         wdate
008700011102     c                   move      wdate         g02dat
008800011102     c                   movel     *blanks       g02err
008900011102     c                   call      'XSRDA8'
009000011102     c                   parm                    wlbdat
009100011102
009200011102      * Calcolo data
009300011105     c     g02tgi        sub       §5Aori        giotgi
009400011102     c                   call      'XSRGI8'
009500011102     c                   parm                    wgidat
009600011102     c                   z-add     giodat        datori
009700011102
009800011102      * Giro la data
009900011102IF  1c                   if        datori > 0
010000011102     c                   move      datori        g02dat
010100011102     c                   movel     *blanks       g02err
010200011102     c                   call      'XSRDA8'
010300011102     c                   parm                    wlbdat
010400011102IF  2c                   if        g02err = '1'
010500011102     c                   z-add     0             datori
010600011102X   2c                   else
010700011102     c                   z-add     g02inv        datori
010800011102E   2c                   endif
010900011102E   1c                   endif
011000011102
011100011102     c                   endsr
