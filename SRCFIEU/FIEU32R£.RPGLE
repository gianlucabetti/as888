000100010514     H DECEDIT('0,') DATEDIT(*DMY.)
000200010514      *----------------------------------------------------*
000300010515      * Pulizia ORM in sede
000400010514      *----------------------------------------------------*
000500010514     ffnorm01l  if   e           k disk
000600010515     ftabel00f  if   e           k disk
000700040909     Ffitgd02l  if   e           k disk
000800010515
000900010515     d kcod            s                   like(tblcod)
001000010515     d kkey            s                   like(tblkey)
001100010515
001200010515     d datorm          s                   like(ormdao)
001300050520     d comrec          s                   like(tgddcd)
001400010515
001500010515     d w0140           s             14  0
001600010515     d wdate           s              8  0
001700010514
001800010515     d wlbdat          ds                  inz
001900010515     d  g02dat                 1      8  0
002000010515     d  g02inv                 9     16  0
002100010515     d  g02err                17     17
002200010515     d  g02tgi                18     22  0
002300010515
002400010515     d wgidat          ds                  inz
002500010515     d  giodat                 1      8  0
002600010515     d  gioinv                 9     16  0
002700010515     d  giotgi                17     21  0
002800010515
002900010514     d kpjba         e ds
003000010514     d fior30ds      e ds
003100010515     d ds5as1        e ds
003200050520     d ds5a2         e ds                  prefix(F_)
003300010514
003400040909     d ktgtor          s                   like(tgdtor) inz('O')
003500040909     d ktgogg          s                   like(tgdogg)
003600040928     d kkut            s                   like(tblkut) inz(1)
003700040909
003800040909     D*-------------
003900040909     D* DS OGGETTO RECLAMO
004000040909     D*-------------
004100040909     D                 DS
004200040909     D  W_poe                         3  0
004300040909     D  W_nsr                         2  0
004400040909     D  W_nor                         7  0
004500040909     D  W_nrv                         2  0
004600040909     D  Worm                   1     14  0 inz
004700010514      *-------------------------------------------------------------------------*
004800010514
004900010514     c                   do        *hival
005000010514     c                   read      fnorm01l
005100040928      *
005200010514     c                   if        %eof(fnorm01l)
005300010514     c                   leave
005400010514     c                   endif
005500040909
005600010514      * Se trascorsi i tot.giorni dalla data di emissione
005700010514     c                   if        ormdao <= datorm
005800040909      *
005900040909      * RECLAMI : verifico se ORM presente nei reclami
006000040928      *            non si deve pulire l'ORM
006100040909      *
006200040909     c                   movel     ormpoe        W_poe
006300040909     c                   movel     ormnsr        W_nsr
006400040909     c                   movel     ormnor        W_nor
006500040909     c                   movel     ormnrv        W_nrv
006600040909     c                   movel     worm          ktgogg
006700040909      *
006800050520     c                   clear                   wok               1
006900050520     c     ktgd          setll     fitgd02l
007000050520     c                   do        *hival
007100050520     c     ktgd          reade     fitgd02l
007200050520     c                   if        %eof(fitgd02l)
007300050520     c                   leave
007400050520     c                   endif
007500050520      * se annullato leggo il successivo
007600050520     c                   If        TGDatb <> ' '
007700050520     c                   iter
007800050520     c                   endif
007900050520      * verifico se reclamo ancora aperto
008000050520     c                   If        TGDdcd = 0  or
008100050520      * se reclamo chiuso verifico la data chiusura con la dta di pulizia
008200050520     c                             TGDdcd > comrec
008300050520     c                   eval      wok = 'N'
008400050520     c                   leave
008500050520     c                   endif
008600050520     c                   enddo
008700040909
008800050520     c                   if        wok = ' '
008900010514     c                   clear                   fior30ds
009000010514     c                   eval      i30mod = 'D'
009100010514     c                   eval      i30poe = ormpoe
009200010514     c                   eval      i30nsr = ormnsr
009300010514     c                   eval      i30nor = ormnor
009400010514     c                   eval      i30nrv = ormnrv
009500010514     c                   call      'FIOR30R'
009600010514     c                   parm                    kpjba
009700010514     c                   parm                    fior30ds
009800040909     c                   endif
009900040928
010000010514     c                   endif
010100010514
010200010514     c                   enddo
010300010515
010400010515     c                   eval      *inlr = *on
010500010514
010600010514      *-------------------------------------------------------------------------*
010700010514      * Subroutine iniziale
010800010514      *-------------------------------------------------------------------------*
010900010514     c     *inzsr        begsr
011000010514
011100010514     c     *entry        plist
011200010514     c                   parm                    kpjba
011300010514
011400010515     c     ktab          klist
011500040928     c                   kfld                    KKUT
011600010515     c                   kfld                    kcod
011700010515     c                   kfld                    kkey
011800040909
011900040909     c*  File Fitgd02l    RECLAMI
012000040909     c     ktgd          klist
012100040909     c                   kfld                    ktgtor
012200040909     c                   kfld                    ktgogg
012300010515
012400010515      * Lettura giorni di pulizia orm
012500010515     c                   clear                   ds5as1
012600010515     c                   movel     '5A'          kcod
012700010515     c                   movel     'SEDE1'       kkey
012800010515     c     ktab          chain     tabel00f
012900010515     c                   if        %found(tabel00f) and tblflg = *blanks
013000010515     c                   movel     tbluni        ds5as1
013100010515     c                   endif
013200050520      * Lettura giorni di pulizia orm
013300050520     c                   clear                   ds5a2
013400050520     c                   movel     '5A'          kcod
013500050520     c                   movel(p)  '2'           kkey
013600050520     c     ktab          chain     tabel00f
013700050520     c                   if        %found(tabel00f) and tblflg = *blanks
013800050520     c                   movel     tbluni        ds5a2
013900050520     c                   endif
014000010515
014100010515      * Ricavo il nr. giorni dalla data odierna
014200010515     c                   time                    w0140
014300010515     c                   move      w0140         wdate
014400010515     c                   move      wdate         g02dat
014500010515     c                   movel     *blanks       g02err
014600010515     c                   call      'XSRDA8'
014700010515     c                   parm                    wlbdat
014800010515
014900010515      * Calcolo data
015000010515     c     g02tgi        sub       §5aors        giotgi
015100010515     c                   call      'XSRGI8'
015200010515     c                   parm                    wgidat
015300010515     c                   z-add     giodat        datorm
015400050520      * Calcolo data per controllo reclami
015500050520     c     g02tgi        sub       F_§5agpr      giotgi
015600050520     c                   call      'XSRGI8'
015700050520     c                   parm                    wgidat
015800050520     c                   z-add     giodat        comrec
015900010514
016000010515      * Giro la data
016100010515     c                   if        datorm > 0
016200010515     c                   move      datorm        g02dat
016300010515     c                   movel     *blanks       g02err
016400010515     c                   call      'XSRDA8'
016500010515     c                   parm                    wlbdat
016600010515     c                   if        g02err = '1'
016700010515     c                   z-add     0             datorm
016800010515     c                   else
016900010515     c                   z-add     g02inv        datorm
017000010515     c                   endif
017100010515     c                   endif
017200050520      * Giro la data per controllo reclami
017300050520     c                   if        comrec > 0
017400050520     c                   clear                   wlbdat
017500050520     c                   move      comrec        g02dat
017600050520     c                   movel     *blanks       g02err
017700050520     c                   call      'XSRDA8'
017800050520     c                   parm                    wlbdat
017900050520     c                   if        g02err = '1'
018000050520     c                   z-add     0             comrec
018100050520     c                   else
018200050520     c                   z-add     g02inv        comrec
018300050520     c                   endif
018400050520     c                   endif
018500050520     c
018600040909
018700010514     c                   endsr
