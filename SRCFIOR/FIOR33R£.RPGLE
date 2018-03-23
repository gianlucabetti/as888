000100011102     H DECEDIT('0,') DATEDIT(*DMY.)
000200011102      *----------------------------------------------------*
000300011105      * Pulizia file TIORI00F (solo in sede)
000400011102      *----------------------------------------------------*
000500011105     ftiori04l  uf   e           k disk
000600100218     ftiore01l  uf   e           k disk
000700011102     ftabel00f  if   e           k disk
000800011102
000900011102     d kcod            s                   like(tblcod)
001000011102     d kkey            s                   like(tblkey)
001100011102
001200011102     d datori          s                   like(orider)
001300011102
001400011102     d w0140           s             14  0
001500011102     d wdate           s              8  0
001600011105
001700011105     d $OK_GG          s              1
001800011102
001900011102     d wlbdat          ds                  inz
002000011102     d  g02dat                 1      8  0
002100011102     d  g02inv                 9     16  0
002200011102     d  g02err                17     17
002300011102     d  g02tgi                18     22  0
002400011102
002500011102     d wgidat          ds                  inz
002600011102     d  giodat                 1      8  0
002700011102     d  gioinv                 9     16  0
002800011102     d  giotgi                17     21  0
002900011102
003000011102     d kpjba         e ds
003100011102     d ds5as1        e ds
003200011102     d ut§dse0f      e ds
003300011102     d cncr80        e ds
003400011102
003500011102      *-------------------------------------------------------------------------*
003600011102
003700011105DO  1c                   dow       $OK_GG = *on
003800011105
003900011102     c                   read      tiori04l
004000011102
004100011102      * Se trascorsi i tot.giorni dalla data di elaborazione
004200011105IF  2c                   if        not %eof(tiori04l)
004300011105     c                             and orider <= datori
004400100218
004500100218      * leggo estensione e cancello anche quella
004600100218     c     oriprg        setll     tiore01l
004700100218     c                   do        *hival
004800100218     c     oriprg        reade     tiore01l
004900100218     c                   if        %eof(tiore01l)
005000100218     c                   leave
005100100218     c                   endif
005200100218     c                   delete    tiore000
005300100218     c                   enddo
005400100218
005500011105     c                   delete    tiori000
005600011102X   2c                   else
005700011102     c                   leave
005800011102E   2c                   endif
005900011102
006000011102E   1c                   enddo
006100011102
006200011102     c                   eval      *inlr = *on
006300011102
006400011102      *-------------------------------------------------------------------------*
006500011102      * Subroutine iniziale
006600011102      *-------------------------------------------------------------------------*
006700011102     c     *inzsr        begsr
006800011102
006900011102     c     *entry        plist
007000011102     c                   parm                    kpjba
007100011102
007200011102     c                   z-add     1             codut
007300011102     c                   call      'X§PARUT'
007400011102     c                   parm                    ut§dse0f
007500011102     c                   movel     rec80         cncr80
007600011102
007700011102     c     ktab          klist
007800011102     c                   kfld                    codut
007900011102     c                   kfld                    kcod
008000011102     c                   kfld                    kkey
008100011102
008200011102      * Lettura giorni di pulizia orm
008300011102     c                   clear                   ds5as1
008400011102     c                   movel     '5A'          kcod
008500011102     c                   movel     'SEDE1'       kkey
008600011102     c     ktab          chain     tabel00f
008700011102IF  1c                   if        %found(tabel00f) and tblflg = *blanks
008800011105     c                   movel     tbluni        ds5As1
008900011102E   1c                   endif
009000011105IF  1c                   if        §5Aori > 0
009100011105     c                   movel     *on           $OK_GG
009200011105X   1c                   else
009300011105     c                   movel     *off          $OK_GG
009400011105E   1c                   endif
009500011102
009600011102      * Ricavo il nr. giorni dalla data odierna
009700011102     c                   time                    w0140
009800011102     c                   move      w0140         wdate
009900011102     c                   move      wdate         g02dat
010000011102     c                   movel     *blanks       g02err
010100011102     c                   call      'XSRDA8'
010200011102     c                   parm                    wlbdat
010300011102
010400011102      * Calcolo data
010500011105     c     g02tgi        sub       §5Aori        giotgi
010600011102     c                   call      'XSRGI8'
010700011102     c                   parm                    wgidat
010800011102     c                   z-add     giodat        datori
010900011102
011000011102      * Giro la data
011100011102IF  1c                   if        datori > 0
011200011102     c                   move      datori        g02dat
011300011102     c                   movel     *blanks       g02err
011400011102     c                   call      'XSRDA8'
011500011102     c                   parm                    wlbdat
011600011102IF  2c                   if        g02err = '1'
011700011102     c                   z-add     0             datori
011800011102X   2c                   else
011900011102     c                   z-add     g02inv        datori
012000011102E   2c                   endif
012100011102E   1c                   endif
012200011102
012300011102     c                   endsr
