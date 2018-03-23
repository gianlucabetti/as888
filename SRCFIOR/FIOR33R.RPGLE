000100011102     H DECEDIT('0,') DATEDIT(*DMY.)
000200011102      *----------------------------------------------------*
000300011105      * Pulizia file TIORI00F (solo in sede)
000400011102      *----------------------------------------------------*
000500011105     ftiori04l  uf   e           k disk
000600151028     ftiori01l  if   e           k disk    rename(tiori000:tiori01)
000700100218     ftiore01l  uf   e           k disk
000800011102     ftabel00f  if   e           k disk
000900011102
001000011102     d kcod            s                   like(tblcod)
001100011102     d kkey            s                   like(tblkey)
001200151028     d kOREprg         s                   like(OREprg)
001300011102
001400011102     d datori          s                   like(orider)
001500011102
001600011102     d w0140           s             14  0
001700011102     d wdate           s              8  0
001800011105
001900011105     d $OK_GG          s              1
002000011102
002100011102     d wlbdat          ds                  inz
002200011102     d  g02dat                 1      8  0
002300011102     d  g02inv                 9     16  0
002400011102     d  g02err                17     17
002500011102     d  g02tgi                18     22  0
002600011102
002700011102     d wgidat          ds                  inz
002800011102     d  giodat                 1      8  0
002900011102     d  gioinv                 9     16  0
003000011102     d  giotgi                17     21  0
003100011102
003200011102     d kpjba         e ds
003300011102     d ds5as1        e ds
003400011102     d ut§dse0f      e ds
003500011102     d cncr80        e ds
003600011102
003700011102      *-------------------------------------------------------------------------*
003800011102
003900011105DO  1c                   dow       $OK_GG = *on
004000011105
004100011102     c                   read      tiori04l
004200011102
004300011102      * Se trascorsi i tot.giorni dalla data di elaborazione
004400011105IF  2c                   if        not %eof(tiori04l)
004500011105     c                             and orider <= datori
004600100218
004700100218      * leggo estensione e cancello anche quella
004800100218     c     oriprg        setll     tiore01l
004900100218     c                   do        *hival
005000100218     c     oriprg        reade     tiore01l
005100100218     c                   if        %eof(tiore01l)
005200100218     c                   leave
005300100218     c                   endif
005400100218     c                   delete    tiore000
005500100218     c                   enddo
005600100218
005700011105     c                   delete    tiori000
005800011102X   2c                   else
005900011102     c                   leave
006000011102E   2c                   endif
006100011102
006200011102E   1c                   enddo
006300151028
006400151028      /free
006500151028       //?Leggo TIORE, se non presente, come PRG, su TIORI cancello
006600151028       kOREprg = 1;
006700151028       setll kOREprg TIORE01L;
006800151028       read TIORE01L;
006900151028       DOW not %eof(TIORE01L);
007000151028         chain OREprg TIORI01L;
007100151028         IF  not %found(TIORI01L);
007200151028           delete TIORE000;
007300151028         ENDIF;
007400151028         read TIORE01L;
007500151028       ENDDO;
007600151028
007700151028      /end-free
007800011102
007900011102     c                   eval      *inlr = *on
008000011102
008100011102      *-------------------------------------------------------------------------*
008200011102      * Subroutine iniziale
008300011102      *-------------------------------------------------------------------------*
008400011102     c     *inzsr        begsr
008500011102
008600011102     c     *entry        plist
008700011102     c                   parm                    kpjba
008800011102
008900011102     c                   z-add     1             codut
009000011102     c                   call      'X§PARUT'
009100011102     c                   parm                    ut§dse0f
009200011102     c                   movel     rec80         cncr80
009300011102
009400011102     c     ktab          klist
009500011102     c                   kfld                    codut
009600011102     c                   kfld                    kcod
009700011102     c                   kfld                    kkey
009800011102
009900011102      * Lettura giorni di pulizia orm
010000011102     c                   clear                   ds5as1
010100011102     c                   movel     '5A'          kcod
010200011102     c                   movel     'SEDE1'       kkey
010300011102     c     ktab          chain     tabel00f
010400011102IF  1c                   if        %found(tabel00f) and tblflg = *blanks
010500011105     c                   movel     tbluni        ds5As1
010600011102E   1c                   endif
010700011105IF  1c                   if        §5Aori > 0
010800011105     c                   movel     *on           $OK_GG
010900011105X   1c                   else
011000011105     c                   movel     *off          $OK_GG
011100011105E   1c                   endif
011200011102
011300011102      * Ricavo il nr. giorni dalla data odierna
011400011102     c                   time                    w0140
011500011102     c                   move      w0140         wdate
011600011102     c                   move      wdate         g02dat
011700011102     c                   movel     *blanks       g02err
011800011102     c                   call      'XSRDA8'
011900011102     c                   parm                    wlbdat
012000011102
012100011102      * Calcolo data
012200011105     c     g02tgi        sub       §5Aori        giotgi
012300011102     c                   call      'XSRGI8'
012400011102     c                   parm                    wgidat
012500011102     c                   z-add     giodat        datori
012600011102
012700011102      * Giro la data
012800011102IF  1c                   if        datori > 0
012900011102     c                   move      datori        g02dat
013000011102     c                   movel     *blanks       g02err
013100011102     c                   call      'XSRDA8'
013200011102     c                   parm                    wlbdat
013300011102IF  2c                   if        g02err = '1'
013400011102     c                   z-add     0             datori
013500011102X   2c                   else
013600011102     c                   z-add     g02inv        datori
013700011102E   2c                   endif
013800011102E   1c                   endif
013900011102
014000011102     c                   endsr
