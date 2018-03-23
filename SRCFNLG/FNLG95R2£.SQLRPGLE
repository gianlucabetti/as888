000100051109     H*PARMS commit(*all)
000200040820     H DECEDIT('0,') DATEDIT(*dmy.)
000300051014     H**********************************************************************
000400051014     c* 900       RENDERE
000500051014     c* 910       RICONSEGNARE STESSO INDIRIZZO
000600051014     c* 911       RICONSEGNARE STESSO INDIRIZZO CON APPUNTAMENTO
000700051014     c* 912       TELEFONO
000800051014     c* 913       SIG.
000900051014     c* 914       RICONSEGNARE STESSO INDIRIZZO
001000051014     c* 915       IN DATA
001100051014     c* 9152      DOPO IL
001200051014     c* 916       RICONSEGNARE - F.D.
001300080922     c* 917       RITIRERA
001400051014     c* 920       RICONSEGNARE ALTRO INDIRIZZO
001500051014     c*
001600051014     c* IMPORTANTE : è indispensabile che l'associazione progressivo e
001700051014     c* testo non venga modificato per evitare scarti in fase
001800051014     c* di ricezione disposizioni
001801051205     c*
001802051205     c* ATTENZIONE in alcune istruzioni SQL è stato specificato FOR UPDATE
001803051205     c* non per aggiornare effettivamente il campo ma per avere i records
001804051205     c* allocati anche in lettura
001900020409     H**********************************************************************
002000051205     ffnvar10s  o    E             disk    commit prefix(£)
002100050928     ffnvar00r  uf   E             disk    commit
002200050927     ftabel00f  if   E           k disk
002300040820     H**********************************************************************
002400050927     d PRG             S              3  0 DIM(5)
002500050927     d esi             S              3    DIM(50)
002600050927     d vis             S              1    DIM(50)
002700050927     d X               S              3  0
002800050927     d XX              S              3  0
002900050923     d VOLTE           S              3  0
003000050923     d CONTA           S              3  0
003100050923     d AGC             S                   LIKE(VARAGC)
003200050923     d FGC             S                   LIKE(VARFGC)
003300050923     d NGC             S                   LIKE(VARNGC)
003400050923     d SAGC            S                   LIKE(VARAGC)
003500050923     d SFGC            S                   LIKE(VARFGC)
003600050923     d SNGC            S                   LIKE(VARNGC)
003700050923     d NPR             S                   LIKE(VARNPR)
003800050927     d dtr             S                   LIKE(VARdtr)
003900050923     d NPRFINE         S                   LIKE(VARNPR)
004000050923     d PRG1            S                   LIKE(VARNPR)
004100050923     d PRG2            S                   LIKE(VARNPR)
004200050923     d PRG3            S                   LIKE(VARNPR)
004300050923     d PRG4            S                   LIKE(VARNPR)
004400050923     d PRG5            S                   LIKE(VARNPR)
004500050923     d DMC             S                   LIKE(VARDMC)
004600050923     d NRR             S              9  0
004700050927     D WHHDAT          s             14  0
004800050927     D OGGI            s              8  0
004900050926     D ORA             s              6  0
005000040824      *
005100051108     D kpjba         E DS
005200050927     d Ds2n          e ds
005201051205     d fnvar         e ds                  extname(fnvar00r)
005202051205     d fnvars        e ds                  extname(fnvar10s) prefix(£)
005300050927     D WLBDA8          DS
005400050927     D  G02DAT                 1      8  0
005500050927     D  G02INV                 9     16  0
005600050927     D  G02ERR                17     17
005700050927     D  G02TGI                18     22  0
005800040820      *****************************************************************
005900040903     c*  Ciclo principale
006000040928     c*
006100051014     c* controllo che non ci sia disposioni e note x la stessa giac.
006200050928     c                   exsr      srdisnote
006300051014     c* controllo che non ci siano + disposizioni x la stessa giac.
006400050928     c                   exsr      srpiudis
006500051014     c* controllo che non ci sia + volte lo stesso progr. x la stessa giac.
006600051014     c                   exsr      srpiuprg
006700051014     c* controllo che le disposiz.ricevute siano congruenti x la stessa giac
006800051108     c                   exsr      srdisok
006900051014     c* controllo di non avere testo note doppi x la stessa giac.
007000050928     c                   exsr      srpiutes
007100051014     c* controllo di non avere ricevuto note generiche con progressivo >=900
007200051014     c                   exsr      srnotgen
007300051108     c*
007400051108     c                   commit
007401051206     C/EXEC SQL
007402051206     C+ COMMIT
007403051206     C/END-EXEC
007500051108     c*
007600050923      *
007700050923     C                   SETON                                        LR
007800050928     C**************************************************************************
007900050928     c* controllo che non ci sia disposioni e note
008000050928     C**************************************************************************
008100050928     C     srdisnote     BEGSR
008200051014     c*
008300050928     C/EXEC SQL
008400050928     C+ DECLARE E1 CURSOR FOR
008500050928     C+ select a.varagc, a.varfgc, a.varngc
008600050928     C+ from fnvar00r a join ( SELECT b.varagc, b.varfgc, b.varngc, b.varnpr
008700051110     C+  FROM fnvar00r b WHERE (b. varnpr = 900 and b.vardmc = 'RENDERE') or
008800051110     C+  (b.varnpr = 910 and b.vardmc = 'RICONSEGNARE STESSO INDIRIZZO') OR
008900050928     C+ (b.varnpr = 911 and b.vardmc =
009000050928     C+ 'RICONSEGNARE STESSO INDIRIZZO CON APPUNTAMENTO') OR (b.varnpr =
009100050928     C+ 914 and b.vardmc = 'RICONSEGNARE STESSO INDIRIZZO') OR (b.VArnpr =
009200050928     C+ 916 and b.vardmc = 'RICONSEGNARE - F.D.') OR (b.varnpr = 920 and
009300050928     C+ b.vardmc = 'RICONSEGNARE ALTRO INDIRIZZO')) c on a.varagc =
009400050928     C+ c.varagc and a.varfgc = c.varfgc and a.varngc = c.varngc where
009500050928     C+ a.varnpr < 900 and c.varnpr in (900, 910, 911, 914, 916 , 920)
009600050928     C+ GROUP BY a.varagc, a.varfgc, a.varngc
009700050928     C/END-EXEC
009800050928
009900050928     C/EXEC SQL
010000050928     C+ OPEN E1
010100050928     C/END-EXEC
010200051108     C                   DO        *hival
010300050928     C/EXEC SQL
010400050928     C+ FETCH NEXT FROM E1 INTO :agc, :fgc, :ngc
010500050928     C/END-EXEC
010600050928     C                   SELECT
010700050928     c*
010800051108     C                   WHEN      SqlCod = 100
010900051108     c                   leave
011000050928     C                   WHEN      SqlCod < 0
011100051108     c                   if        sqlcod <> -913
011200050928     C                   seton                                        H1
011300051108     c                   end
011400051108     c                   exsr      srfine
011500050928     c                   other
011600051108     c                   exsr      srwtr4
011700050928     C                   ENDSL
011800050928      *
011900050928     C                   ENDDO
012000050928     C/EXEC SQL
012100050928     C+ CLOSE E1
012200050928     C/END-EXEC
012300050928     c*
012400050928     c                   endsr
012500050928     C**************************************************************************
012600050928     c* controllo che non ci sia + disposioni
012700050928     C**************************************************************************
012800050928     C     srpiudis      BEGSR
012900051109     c*
013000050928     C/EXEC SQL
013100050928     C+ DECLARE D1 CURSOR FOR SELECT VARAGC, VARFGC, VARNGC, count(*)
013200050928     C+ FROM fnvar00r WHERE varnpr in (900, 911, 914, 916, 920) GROUP BY
013300050928     C+ VARAGC, VARFGC, VARNGC HAVING count(*) > 1
013400050928     C/END-EXEC
013500050928
013600050928     C/EXEC SQL
013700050928     C+ OPEN D1
013800050928     C/END-EXEC
013900051108     C                   Do        *hival
014000050928     C/EXEC SQL
014100050928     C+ FETCH NEXT FROM D1 INTO :agc, :fgc, :ngc, :conta
014200050928     C/END-EXEC
014300050928     C                   SELECT
014400050928     c*
014500051108     C                   WHEN      SqlCod = 100
014600051108     c                   leave
014700050928     C                   WHEN      SqlCod < 0
014800051108     c                   if        sqlcod <> -913
014900050928     C                   seton                                        H1
015000051108     c                   end
015100051108     c                   exsr      srfine
015200050928     c                   other
015300051108     c                   exsr      srwtr4
015400050928     C                   ENDSL
015500050928      *
015600050928     C                   ENDDO
015700050928     C/EXEC SQL
015800050928     C+ CLOSE D1
015900050928     C/END-EXEC
016000050928     c*
016100050928     c                   endsr
016200050928     C**************************************************************************
016300050928     c* controllo che non ci sia + volte lo stesso progressivo nota
016400050928     C**************************************************************************
016500050928     C     srpiuprg      BEGSR
016600051109     c*
016700050928     C/EXEC SQL
016800050928     C+ DECLARE B1 CURSOR FOR SELECT VARAGC, VARFGC, VARNGC, VARNPR,
016900050928     C+ count(*) FROM fnvar00r GROUP BY VARAGC, VARFGC, VARNGC, VARNPR
017000050928     C+ HAVING count(*) > 1
017100050928     C/END-EXEC
017200050928
017300050928     C/EXEC SQL
017400050928     C+ OPEN B1
017500050928     C/END-EXEC
017600051108     C                   DO        *hival
017700050928     C/EXEC SQL
017800050928     C+ FETCH NEXT FROM B1 INTO :agc, :fgc, :ngc, :npr, :conta
017900050928     C/END-EXEC
018000050928     C                   SELECT
018100050928     c*
018200051108     C                   WHEN      SqlCod = 100
018300051108     c                   leave
018400050928     C                   WHEN      SqlCod < 0
018500051108     c                   if        sqlcod <> -913
018600050928     C                   seton                                        H1
018700051108     c                   end
018800051108     c                   exsr      srfine
018900050928     c                   other
019000051108     c                   exsr      srwtr2
019100051108     c                   commit
019200050928     C                   ENDSL
019300050928      *
019400050928     C                   ENDDO
019500050928     C/EXEC SQL
019600050928     C+ CLOSE B1
019700050928     C/END-EXEC
019800050928     c*
019900050928     c                   endsr
020000050923     C**************************************************************************
020100050923     c* controllo le disposioni siano congruenti
020200050923     C**************************************************************************
020300050928     C     srdisok       BEGSR
020400051109     c*
020500050923     C/EXEC SQL
020600050923     C+ DECLARE C1 CURSOR FOR SELECT VARAGC, VARFGC, VARNGC, VARNPR FROM
020700050923     C+ fnvar00r WHERE VARNPR > 910 ORDER BY VARAGC, VARFGC, VARNGC,
020800050923     C+ VARNPR
020900050923     C/END-EXEC
021000050923
021100050923     C/EXEC SQL
021200050923     C+ OPEN C1
021300050923     C/END-EXEC
021400051108     C                   DO        *hival
021500050923     C/EXEC SQL
021600050923     C+ FETCH NEXT FROM C1 INTO :agc, :fgc, :ngc, :npr
021700050923     C/END-EXEC
021800050923     C                   SELECT
021900050923     c*
022000051108     C                   WHEN      SqlCod = 100
022100050928     c                   exsr      ctrdisok
022200051108     c                   leave
022300050923     C                   WHEN      SqlCod < 0
022400051108     c                   if        sqlcod <> -913
022500051108     C                   seton                                        H1
022600051108     c                   end
022700051108     c                   exsr      srfine
022800050923     c                   other
022900050928     c                   exsr      ctrdisok
023000051108     c                   add       1             x
023100051108     c                   eval      prg(x) = npr
023200050923     C                   ENDSL
023300050923      *
023400050923     C                   ENDDO
023500050923     C/EXEC SQL
023600050923     C+ CLOSE C1
023700050923     C/END-EXEC
023800050923     c*
023900050923     c                   endsr
024000050928     C**************************************************************************
024100050928     c* controllo di non avere disposizioni doppie
024200050928     C**************************************************************************
024300050928     C     srpiutes      BEGSR
024400050928      **
024500051109     C*
024600050928     C/EXEC SQL
024700050928     C+ DECLARE A1 CURSOR FOR SELECT VARAGC, VARFGC, VARNGC,
024800050928     C+ VARDMC, count(*) FROM fnvar00r GROUP BY VARAGC, VARFGC, VARngc,
024900050928     C+ vardmc HAVING count(*) > 1
025000050928     C/END-EXEC
025100050928
025200050928     C/EXEC SQL
025300050928     C+ OPEN A1
025400050928     C/END-EXEC
025500051108     C                   DO        *hival
025600050928     C/EXEC SQL
025700050928     C+ FETCH NEXT FROM A1 INTO :agc, :fgc, :ngc, :dmc, :conta
025800050928     C/END-EXEC
025900050928     C                   SELECT
026000050928     c*
026100051108     C                   WHEN      SqlCod = 100
026200051108     c                   leave
026300050928     C                   WHEN      SqlCod < 0
026400051108     c                   if        sqlcod <> -913
026500051108     C                   seton                                        H1
026600051108     c                   end
026700051108     c                   exsr      srfine
026800050928     c                   other
026900051108     c                   exsr      srwtr1
027000051108     c                   commit
027100050928     C                   ENDSL
027200050928      *
027300050928     C                   ENDDO
027400050928     C/EXEC SQL
027500050928     C+ CLOSE A1
027600050928     C/END-EXEC
027700050928     c*
027800050928     c                   endsr
027900051014     C**************************************************************************
028000051014     c* controllo di non avere ricevuto note generiche con progressivo >=900
028100051014     C**************************************************************************
028200051014     C     srnotgen      BEGSR
028300051014      **
028400051014     C/EXEC SQL
028500051205     C+ DECLARE M1 CURSOR FOR SELECT * FROM fnvar00r WHERE (varnpr = 900
028600051205     C+ and vardmc<> 'RENDERE') or (varnpr = 910 and vardmc <>
028700051205     C+ 'RICONSEGNARE STESSO INDIRIZZO') OR (varnpr = 911 and vardmc <>
028800051205     C+ 'RICONSEGNARE STESSO INDIRIZZO CON APPUNTAMENTO') OR (varnpr =
028900051014     C+ 912 and substr(vardmc, 1, 8)<> 'TELEFONO') OR (varnpr = 913 and
029000051014     C+ substr(vardmc, 1, 4)<> 'SIG.') OR (varnpr = 914 and vardmc <>
029100051014     C+ 'RICONSEGNARE STESSO INDIRIZZO') OR (VArnpr=915 and
029200051014     C+ (substr(vardmc, 1, 7) <> 'IN DATA') and (substr(vardmc, 1, 7) <>
029300051014     C+ 'DOPO IL')) OR (VArnpr = 916 and vardmc <>'RICONSEGNARE - F.D.')
029400080922     C+ OR (varnpr = 917 and substr(vardmc, 1, 8) <> 'RITIRERA') or
029500051205     C+ (varnpr = 920 and vardmc <> 'RICONSEGNARE ALTRO INDIRIZZO') FOR
029501051205     C+ UPDATE OF VARDRD
029600051014     C/END-EXEC
029700051014
029800051014     C/EXEC SQL
029900051014     C+ OPEN M1
030000051014     C/END-EXEC
030100051108     C                   DO        *hival
030200051014     C/EXEC SQL
030300051205     C+ FETCH NEXT FROM M1 INTO :fnvar
030400051014     C/END-EXEC
030500051014     C                   SELECT
030600051014     c*
030700051108     C                   WHEN      SqlCod = 100
030800051108     c                   leave
030900051014     C                   WHEN      SqlCod < 0
031000051108     c                   if        sqlcod <> -913
031100051108     C                   seton                                        H1
031200051108     c                   end
031300051108     c                   exsr      srfine
031400051014     c                   other
031601051206     c                   eval      %subst(fnvars: 1: 106) = fnvar
031700051205     c                   eval      £varesi = 'NPE'
031701051205     C/EXEC SQL
031702051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF M1
031703051205     C/END-EXEC
031800051205     c                   exsr      srwtr
032100051108     c                   commit
032200051014     C                   ENDSL
032300051014      *
032400051014     C                   ENDDO
032500051014     C/EXEC SQL
032600051014     C+ CLOSE M1
032700051014     C/END-EXEC
032800051014     c*
032900051014     c                   endsr
033000050923     ***********************************************************************
033100050923     C* controllo la sequenza delle disposizioni
033200050923     C**************************************************************************
033300050928     C     ctrdisok      BEGSR
033400050923     c* a rottura di giacenza testo la sequenza dei progressivi
033500050923     c                   if        agc <> sagc  or
033600050926     c                             ngc <> sngc  or
033700050928     c                             fgc <> sfgc  or
033800050928     c                             sqlcod = 100
033900050923     c                   if        sagc <> 0
034000050923     c                   if        (prg(1) = 911 and
034100050923     c                             (prg(2) <> 912 or prg(3) <> 913)) OR
034200050923     c                             (prg(1) = 914
034300050923     c                             and prg(2) <> 915) OR
034400050923     c                             (prg(1) = 916
034500050923     c                             and prg(2) <> 917) OR
034600050923     c                             (prg(1) = 920
034700051107     c                             and (prg(2) <> 921 or prg(3) <> 922 or
034800051107     c                             prg(4) <> 923 or prg(5) <> 924)) OR
034900050927     c                             (prg(1) <> 911 and
035000050927     c                             prg(1) <> 914 and
035100050927     c                             prg(1) <> 916 and
035200050927     c                             prg(1) <> 920)
035300050923     C                   EVAL      PRG1 = PRG(1)
035400050923     C                   EVAL      PRG2 = PRG(2)
035500050923     C                   EVAL      PRG3 = PRG(3)
035600050923     C                   EVAL      PRG4 = PRG(4)
035700050923     C                   EVAL      PRG5 = PRG(5)
035800050923     c                   exsr      srwtr3
035900050928     c                   commit
036000050923     c                   end
036100050923     c                   end
036200050923     c                   eval      sagc = agc
036300050923     c                   eval      sfgc = fgc
036400050923     c                   eval      sngc = ngc
036500050923     c                   clear                   prg
036600050923     c                   clear                   x
036700050923     c                   end
036800050923     c*
036900050923     c                   endsr
037000050923     ***********************************************************************
037100050928     C* cancella i record + vecchi con stesso testo nota
037200050923     C**************************************************************************
037300050923     C     srwtr1        BEGSR
037400050923     c                   clear                   volte
037500050923     c*
037600050923     C/EXEC SQL
037700051205     C+ DECLARE F1 CURSOR FOR SELECT * FROM fnvar00r
037800050926     C+ WHERE VARAGC=:agc and VARfGC=:fgc and VARnGC=:ngc and VARdmc=:dmc
037900051205     C+ ORDER BY VARDtr FOR UPDATE OF VARDRD
038000050923     C/END-EXEC
038100050923
038200050923     C/EXEC SQL
038300050923     C+ OPEN F1
038400050923     C/END-EXEC
038500051108     C                   DO        *hival
038600050923     C/EXEC SQL
038700051205     C+ FETCH NEXT FROM F1 INTO :fnvar
038800050923     C/END-EXEC
038900050923     C                   SELECT
039000050923     c*
039100051108     C                   WHEN      SqlCod = 100
039200051108     c                   leave
039300050923     C                   WHEN      SqlCod < 0
039400051108     c                   if        sqlcod <> -913
039500051108     C                   seton                                        H1
039600051108     c                   end
039700051108     c                   exsr      srfine
039800050923     c                   other
039900051108     c                   add       1             volte
040000051108     c                   if        volte < conta
040300051206     c                   eval      %subst(fnvars: 1: 106) = fnvar
040301051205     c                   eval      £varesi = 'TND'
040400051108     c                   exsr      srwtr
040401051205     C/EXEC SQL
040402051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF F1
040403051205     C/END-EXEC
040700051108     c                   else
040800051108     c                   leave
040900051108     c                   end
041000050923     C                   ENDSL
041100050923      *
041200050923     C                   ENDDO
041300050923     C/EXEC SQL
041400050923     C+ CLOSE F1
041500050923     C/END-EXEC
041600050923     c*
041700050923     c                   endsr
041800050923     C**************************************************************************
041900050928     C* cancella i record + vecchi con stesso progressivo
042000050923     C**************************************************************************
042100050923     C     srwtr2        BEGSR
042200050923     c                   clear                   volte
042300051109     c*
042400050923     C/EXEC SQL
042500051205     C+ DECLARE G1 CURSOR FOR SELECT * FROM fnvar00r WHERE VARAGC=:agc
042600051205     C+ and VARfGC=:fgc and VARnGC=:ngc and VARnpr=:npr ORDER BY VARdtr
042700051205     C+ FOR UPDATE OF VARDRD
042800050923     C/END-EXEC
042900050923     C/EXEC SQL
043000050923     C+ OPEN G1
043100050923     C/END-EXEC
043200051108     C                   DO        *hival
043300050923     C/EXEC SQL
043400051205     C+ FETCH NEXT FROM G1 INTO :fnvar
043500050923     C/END-EXEC
043600050923     C                   SELECT
043700050923     c*
043800051108     C                   WHEN      SqlCod = 100
043900051108     c                   leave
044000050923     C                   WHEN      SqlCod < 0
044100051108     c                   if        sqlcod <> -913
044200051108     C                   seton                                        H1
044300051108     c                   end
044400051108     c                   exsr      srfine
044500050923     c                   other
044600051108     c                   add       1             volte
044700051108     c                   if        volte < conta
044701051206     c                   eval      %subst(fnvars: 1: 106) = fnvar
045000051205     c                   eval      £varesi = 'DPD'
045100051108     c                   exsr      srwtr
045101051205     C/EXEC SQL
045102051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF G1
045103051205     C/END-EXEC
045600051108     c                   end
045700050923     C                   ENDSL
045800050923      *
045900050923     C                   ENDDO
046000050923     C/EXEC SQL
046100050923     C+ CLOSE G1
046200050923     C/END-EXEC
046300050923
046400050923     c*
046500050923     c                   endsr
046600050923     C**************************************************************************
046700050928     C* cancella le disposizioni incomplete
046800050928     C* es. se ho i progressivi 911 e 912 ma non 913 allora li cancello
046900050923     C**************************************************************************
047000050923     C     srwtr3        BEGSR
047100050923
047200050923     C/EXEC SQL
047300051205     C+ DECLARE H1 CURSOR FOR SELECT * FROM fnvar00r WHERE varagc =:sagc
047400051205     C+ and VARfGC=:sfgc and VARnGC=:sngc and varnpr in (:prg1, :prg2,
047500051205     C+ :prg3, :prg4, :prg5) FOR UPDATE OF vardtr
047600050923     C/END-EXEC
047700050923
047800050923     C/EXEC SQL
047900050923     C+ OPEN H1
048000050923     C/END-EXEC
048100051108     C                   DO        *hival
048200050923     C/EXEC SQL
048300051205     C+ FETCH NEXT FROM H1 INTO :fnvar
048400050923     C/END-EXEC
048500050923     C                   SELECT
048600050923     c*
048700051108     C                   WHEN      SqlCod = 100
048800051108     c                   leave
048900050923     C                   WHEN      SqlCod < 0
049000051108     c                   if        sqlcod <> -913
049100051108     C                   seton                                        H1
049200051108     c                   end
049300051108     c                   exsr      srfine
049400050923     c                   other
049401051206     c                   eval      %subst(fnvars: 1: 106) = fnvar
049700051205     C                   EVAL      £VARESI = 'DIN'
049800051108     c                   exsr      srwtr
049801051205     C/EXEC SQL
049802051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF H1
049803051205     C/END-EXEC
050100050923     C                   ENDSL
050200050923      *
050300050923     C                   ENDDO
050400050923     C/EXEC SQL
050500050923     C+ CLOSE H1
050600050923     C/END-EXEC
050700050923
050800050923     c*
050900050923     c                   endsr
051000050923     C**************************************************************************
051100050928     C* cerco il nrr più alto (ultimo ad essere inserito), prelevo il
051200050928     C* progressivo in modo da scartare durante la cancellaione il range dei
051300050928     C* progressivi appartenenti a questo
051400050923     C**************************************************************************
051500050923     C     srwtr4        BEGSR
051600050923
051700050923     C/EXEC SQL
051800050927     C+ DECLARE I1 CURSOR FOR SELECT max(rrn(fnvar00r)) FROM fnvar00r
051900051205     C+ WHERE varagc =:agc and VARfGC=:fgc and VARnGC=:ngc OPTIMIZE FOR 1
052000051205     C+ ROWS
052100050923     C/END-EXEC
052200050923
052300050923     C/EXEC SQL
052400050923     C+ OPEN I1
052500050923     C/END-EXEC
052600051108     C                   DO        *hival
052700050923     C/EXEC SQL
052800050927     C+ FETCH NEXT FROM I1 INTO :nrr
052900050923     C/END-EXEC
053000050923     C                   SELECT
053100050923     c*
053200051108     C                   WHEN      SqlCod = 100
053300051108     c                   leave
053400050923     C                   WHEN      SqlCod < 0
053500051108     c                   if        sqlcod <> -913
053600051108     C                   seton                                        H1
053700051108     c                   end
053800051108     c                   exsr      srfine
053900050923     c                   other
054000051108     c                   exsr      srwtr41
054100050923     c                   leave
054200050923     C                   ENDSL
054300050923      *
054400050923     C                   ENDDO
054500050923     C/EXEC SQL
054600050923     C+ CLOSE I1
054700050923     C/END-EXEC
054800050923
054900050923     c*
055000050923     c                   endsr
055100050923     C**************************************************************************
055200050928     C* scarto il range dei progressivi dell'ultima disposizione immessa
055300050923     C**************************************************************************
055400050923     C     srwtr41       BEGSR
055500050927     c     nrr           chain(n)  fnvar00r
055600050928     c* imposto il range dei progressivi da scartare dalla delete
055700050923     c                   select
055800050927     c                   when      varnpr < 900
055900050927     c                   eval      npr = 0
056000050923     c                   eval      nprfine = 899
056100050927     c                   when      varnpr = 900
056200050927     c                   eval      npr = 900
056300050923     c                   eval      nprfine = 900
056400050927     c                   when      varnpr >= 911 and varnpr <= 913
056500050927     c                   eval      npr = 911
056600050923     c                   eval      nprfine = 913
056700050927     c                   when      varnpr = 914 or varnpr = 915
056800050927     c                   eval      npr = 914
056900050923     c                   eval      nprfine = 915
057000050927     c                   when      varnpr = 916 or varnpr = 917
057100050927     c                   eval      npr = 916
057200050923     c                   eval      nprfine = 917
057300050927     c                   when      varnpr >= 920 and varnpr <= 924
057400050927     c                   eval      npr = 920
057500050923     c                   eval      nprfine = 924
057600050923     c                   endsl
057700050923
057800051109     c*
057900050923     C/EXEC SQL
058000051205     C+ DECLARE L1 CURSOR FOR SELECT * FROM fnvar00r WHERE varagc =:agc
058100051205     C+ and VARfGC=:fgc and VARnGC=:ngc and varnpr not between :npr and
058200051205     C+ :nprfine FOR UPDATE OF vardtr
058300050923     C/END-EXEC
058400050923
058500050923     C/EXEC SQL
058600050923     C+ OPEN L1
058700050923     C/END-EXEC
058800051108     C                   DO        *hival
058900050923     C/EXEC SQL
059000051205     C+ FETCH NEXT FROM L1 INTO :fnvar
059100050923     C/END-EXEC
059200050923     C                   SELECT
059300050923     c*
059400051108     C                   WHEN      SqlCod = 100
059500051108     c                   leave
059600050923     C                   WHEN      SqlCod < 0
059700051108     c                   if        sqlcod <> -913
059800051108     C                   seton                                        H1
059900051108     c                   end
060000051108     c                   exsr      srfine
060100050923     c                   other
060400051206     C                   EVAL      %subst(fnvars: 1: 106) = fnvar
060401051205     C                   EVAL      £varesi = 'IPD'
060500051108     c                   exsr      srwtr
060501051205     C/EXEC SQL
060502051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF L1
060503051205     C/END-EXEC
060800050923     C                   ENDSL
060900050923      *
061000050923     C                   ENDDO
061100050923     C/EXEC SQL
061200050923     C+ CLOSE L1
061300050923     C/END-EXEC
061400050928     c                   commit
061500050923     c*
061600050923     c                   endsr
061700050927     C**************************************************************************
061800051108     C* esco con codice d'errore
061900050927     C**************************************************************************
062000051108     C     srfine        BEGSR
062100051108     c*
062200051108     c                   rolbk
062300051110     C/EXEC SQL
062400051110     C+ ROLLBACK
062500051110     C/END-EXEC
062600051108     c*
062700051108     c* record allocato
062800051108     c                   eval      kpjbu = '2'
062900051108     c* errore sql
063000051108     c                   if        *inh1
063100051108     c                   eval      kpjbu = '1'
063200051108     c                   end
063300051108     c                   seton                                        lr
063400051108     c                   return
063500051108     c*
063600051108     c                   endsr
063700051108     C**************************************************************************
063800051108     C* scrivo scarti nello storico
063900051108     C**************************************************************************
064000051108     C     srwtr         BEGSR
064100050927     c* reperisco il flag se visualizzabile dal po s/n
064200050927     c                   z-add     1             xx
064300051205     c     £varesi       lookup    esi(xx)                                01
064400050927     c                   if        *in01
064500051205     c                   movel     vis(xx)       £varvis
064600050927     c                   else
064700051205     c                   clear                   £varvis
064800050927     c                   end
064900051205     C                   Z-ADD     OGGI          £VARdrd
065000051205     c                   z-add     ora           £varorz
065100050923     C                   WRITE     FNVARS10
065200050923     c*
065300050923     c                   endsr
065400050927     C**************************************************************************
065500050927     C* sub. inizio
065600050927     C**************************************************************************
065700050927     C     *inzsr        BEGSR
065800051108     C     *ENTRY        PLIST
065900051108     C                   PARM                    KPJBA
066000050927     C*  Reperisco data del giorno
066100050927     C                   CLEAR                   WLBDA8
066200050927     C                   TIME                    WHHDAT
066300050927     C                   MOVE      WHHDAT        G02DAT
066400050927     C                   Z-ADD     0             G02INV
066500050927     C                   MOVEL     ' '           G02ERR
066600050927     C                   CALL      'XSRDA8'
066700050927     C                   PARM                    WLBDA8
066800050927     C                   Z-ADD     G02INV        OGGI
066900050927     C                   MOVEl     WHHDAT        ora
067000050927     c*
067100050927     c                   z-add     1             tblkut
067200050927     c                   eval      tblcod = '2N'
067300050927     C     KTAB          KLIST
067400050927     C                   KFLD                    tblkut
067500050927     C                   KFLD                    TBLCOD
067600050927     c* carico tabella 2N x esito elaborazione
067700050927     c                   clear                   ds2n
067800050927     c                   clear                   xx
067900050927     C     KTAB          setll     TABEL00f
068000050927     c                   do        *hival
068100050927     C     KTAB          reade     TABEL00f
068200050927     C                   if        %eof(tabel00f)
068300050927     c                   leave
068400050927     c                   end
068500050927     c                   if        tblflg <> ' '
068600050927     c                   iter
068700050927     c                   end
068800050927     C                   MOVEL     TBLUNI        DS2n
068900050927     c                   add       1             xx
069000050927     c                   movel     tblkey        esi(xx)
069100050927     c                   movel     §2Nvis        vis(xx)
069200050927     c                   enddo
069300050927     c*
069400050927     c                   endsr
