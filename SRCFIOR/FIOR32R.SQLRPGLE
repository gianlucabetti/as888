000100010514     H DECEDIT('0,') DATEDIT(*DMY.)
000200010514      *----------------------------------------------------*
000300010515      * Pulizia ORM in sede
000400010514      *----------------------------------------------------*
000500010515     ftabel00f  if   e           k disk
000600150403     Ffitgd32l  if   e           k disk
000700010515
000800010515     d kcod            s                   like(tblcod)
000900010515     d kkey            s                   like(tblkey)
001000010515
001100010515     d datorm          s                   like(ormdao)
001200050520     d comrec          s                   like(tgddcd)
001300010515
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
002900170710     d fior3Ads      e ds
003000010515     d ds5as1        e ds
003100050520     d ds5a2         e ds                  prefix(F_)
003200160607
003300160607      // - Ds File FNORM00F
003400160607     d FNORM00F      e ds                  extname(FNORM00F)
003500010514
003600040909     d ktgtor          s                   like(tgdtor) inz('O')
003700040909     d ktgogg          s                   like(tgdogg)
003800040928     d kkut            s                   like(tblkut) inz(1)
003900040909
004000040909     D*-------------
004100040909     D* DS OGGETTO RECLAMO
004200040909     D*-------------
004300040909     D                 DS
004400040909     D  W_poe                         3  0
004500040909     D  W_nsr                         2  0
004600040909     D  W_nor                         7  0
004700040909     D  W_nrv                         2  0
004800040909     D  Worm                   1     14  0 inz
004900160607
005000160607     d EndFile         s               n   inz(*off)
005100010514      *-------------------------------------------------------------------------*
005200010514
005300160607      /free
005400160607       //?Leggo ORM per Data Immissione ORM
005500160607          exec sql
005600160607          DECLARE delorm CURSOR for
005700160607          SELECT * from FNORM00F
005800160607          WHERE ORMdao <= :datorm
005900160607          ORDER BY ORMdao, ORMpoe, ORMnsr, ORMnor, ORMnrv;
006000160607
006100160607         //?Apertura del cursore
006200160607           exec sql
006300160607           OPEN delorm;
006400160607
006500160607           IF sqlcode < 0;
006600160607             exec sql
006700160607             CLOSE delorm;
006800160607             EndFile = *on;
006900160607           ENDIF;
007000160607
007100160607           DOW  not EndFile;
007200160607             exec sql
007300160607             FETCH NEXT from delorm into :FNORM00F;
007400160607             IF  sqlcod = 100 or sqlcod < 0;
007500160607               EndFile = *on;
007600160607               leave;
007700160607             ENDIF;
007800160607      /end-free
007900040909      *
008000040909      * RECLAMI : verifico se ORM presente nei reclami
008100040928      *            non si deve pulire l'ORM
008200040909      *
008300040909     c                   movel     ormpoe        W_poe
008400040909     c                   movel     ormnsr        W_nsr
008500040909     c                   movel     ormnor        W_nor
008600040909     c                   movel     ormnrv        W_nrv
008700040909     c                   movel     worm          ktgogg
008800040909      *
008900050520     c                   clear                   wok               1
009000150403     c     ktgd          setll     fitgd32l
009100050520     c                   do        *hival
009200150403     c     ktgd          reade     fitgd32l
009300150403     c                   if        %eof(fitgd32l)
009400050520     c                   leave
009500050520     c                   endif
009600050520      * se annullato leggo il successivo
009700050520     c                   If        TGDatb <> ' '
009800050520     c                   iter
009900050520     c                   endif
010000050520      * verifico se reclamo ancora aperto
010100050520     c                   If        TGDdcd = 0  or
010200050520      * se reclamo chiuso verifico la data chiusura con la dta di pulizia
010300050520     c                             TGDdcd > comrec
010400050520     c                   eval      wok = 'N'
010500050520     c                   leave
010600050520     c                   endif
010700050520     c                   enddo
010800040909
010900050520     c                   if        wok = ' '
011000170710     c                   clear                   fior3Ads
011100010514     c                   eval      i30mod = 'D'
011200010514     c                   eval      i30poe = ormpoe
011300010514     c                   eval      i30nsr = ormnsr
011400010514     c                   eval      i30nor = ormnor
011500010514     c                   eval      i30nrv = ormnrv
011600170710     c                   call      'FIOR3AR'
011700010514     c                   parm                    kpjba
011800170710     c                   parm                    fior3Ads
011900040909     c                   endif
012000040928
012100160607      /free
012200160607           ENDDO;
012300160607
012400160607         //?Chiusura del cursore
012500160607           exec sql
012600160607           CLOSE delorm;
012700141121
012800141121       //?Alla fine di tutto cancello i vecchi dati della statistica ORM
012900141121       //?cancello WFROR00F
013000141121          exec sql
013100141121          DELETE from WFROR00F
013200141121          where RORdae < :datorm;
013300170710
013400170710       //?richiamo FIOR3AR per chiusura file
013500170710     c                   clear                   fior3Ads
013600170710     c                   eval      i30tla = 'C'
013700170710     c                   call      'FIOR3AR'
013800170710     c                   parm                    kpjba
013900170710     c                   parm                    fior3Ads
014000141121      /end-free
014100010515
014200010515     c                   eval      *inlr = *on
014300010514
014400010514      *-------------------------------------------------------------------------*
014500010514      * Subroutine iniziale
014600010514      *-------------------------------------------------------------------------*
014700010514     c     *inzsr        begsr
014800010514
014900010514     c     *entry        plist
015000010514     c                   parm                    kpjba
015100010514
015200010515     c     ktab          klist
015300040928     c                   kfld                    KKUT
015400010515     c                   kfld                    kcod
015500010515     c                   kfld                    kkey
015600040909
015700150403     c*  File Fitgd32l    RECLAMI
015800040909     c     ktgd          klist
015900040909     c                   kfld                    ktgtor
016000040909     c                   kfld                    ktgogg
016100010515
016200010515      * Lettura giorni di pulizia orm
016300010515     c                   clear                   ds5as1
016400010515     c                   movel     '5A'          kcod
016500010515     c                   movel     'SEDE1'       kkey
016600010515     c     ktab          chain     tabel00f
016700010515     c                   if        %found(tabel00f) and tblflg = *blanks
016800010515     c                   movel     tbluni        ds5as1
016900010515     c                   endif
017000050520      * Lettura giorni di pulizia orm
017100050520     c                   clear                   ds5a2
017200050520     c                   movel     '5A'          kcod
017300050520     c                   movel(p)  '2'           kkey
017400050520     c     ktab          chain     tabel00f
017500050520     c                   if        %found(tabel00f) and tblflg = *blanks
017600050520     c                   movel     tbluni        ds5a2
017700050520     c                   endif
017800010515
017900010515      * Ricavo il nr. giorni dalla data odierna
018000010515     c                   time                    w0140
018100010515     c                   move      w0140         wdate
018200010515     c                   move      wdate         g02dat
018300010515     c                   movel     *blanks       g02err
018400010515     c                   call      'XSRDA8'
018500010515     c                   parm                    wlbdat
018600010515
018700010515      * Calcolo data
018800010515     c     g02tgi        sub       §5aors        giotgi
018900010515     c                   call      'XSRGI8'
019000010515     c                   parm                    wgidat
019100010515     c                   z-add     giodat        datorm
019200050520      * Calcolo data per controllo reclami
019300050520     c     g02tgi        sub       F_§5agpr      giotgi
019400050520     c                   call      'XSRGI8'
019500050520     c                   parm                    wgidat
019600050520     c                   z-add     giodat        comrec
019700010514
019800010515      * Giro la data
019900010515     c                   if        datorm > 0
020000010515     c                   move      datorm        g02dat
020100010515     c                   movel     *blanks       g02err
020200010515     c                   call      'XSRDA8'
020300010515     c                   parm                    wlbdat
020400010515     c                   if        g02err = '1'
020500010515     c                   z-add     0             datorm
020600010515     c                   else
020700010515     c                   z-add     g02inv        datorm
020800010515     c                   endif
020900010515     c                   endif
021000050520      * Giro la data per controllo reclami
021100050520     c                   if        comrec > 0
021200050520     c                   clear                   wlbdat
021300050520     c                   move      comrec        g02dat
021400050520     c                   movel     *blanks       g02err
021500050520     c                   call      'XSRDA8'
021600050520     c                   parm                    wlbdat
021700050520     c                   if        g02err = '1'
021800050520     c                   z-add     0             comrec
021900050520     c                   else
022000050520     c                   z-add     g02inv        comrec
022100050520     c                   endif
022200050520     c                   endif
022300050520     c
022400040909
022500010514     c                   endsr
