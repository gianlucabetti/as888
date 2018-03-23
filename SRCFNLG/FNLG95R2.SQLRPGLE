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
001900170308       //?ATTENZIONE IL TESTO ORA VIENE SOTTOSTRINGATO DALLA 1 ALLA 49 PERCHÈ
002000170308       //?AL 50 BYTE È STATO INSERITO IL FLAG M/D PER CAPIRE CHI INSERISCE LE
002100170308       //?DISPOSIZIONI
002200051205     c*
002300051205     c* ATTENZIONE in alcune istruzioni SQL è stato specificato FOR UPDATE
002400051205     c* non per aggiornare effettivamente il campo ma per avere i records
002500051205     c* allocati anche in lettura
002600020409     H**********************************************************************
002700051205     ffnvar10s  o    E             disk    commit prefix(£)
002800050928     ffnvar00r  uf   E             disk    commit
002900050927     ftabel00f  if   E           k disk
003000040820     H**********************************************************************
003100050927     d PRG             S              3  0 DIM(5)
003200050927     d esi             S              3    DIM(50)
003300050927     d vis             S              1    DIM(50)
003400050927     d X               S              3  0
003500050927     d XX              S              3  0
003600050923     d VOLTE           S              3  0
003700050923     d CONTA           S              3  0
003800050923     d AGC             S                   LIKE(VARAGC)
003900050923     d FGC             S                   LIKE(VARFGC)
004000050923     d NGC             S                   LIKE(VARNGC)
004100050923     d SAGC            S                   LIKE(VARAGC)
004200050923     d SFGC            S                   LIKE(VARFGC)
004300050923     d SNGC            S                   LIKE(VARNGC)
004400050923     d NPR             S                   LIKE(VARNPR)
004500050927     d dtr             S                   LIKE(VARdtr)
004600050923     d NPRFINE         S                   LIKE(VARNPR)
004700050923     d PRG1            S                   LIKE(VARNPR)
004800050923     d PRG2            S                   LIKE(VARNPR)
004900050923     d PRG3            S                   LIKE(VARNPR)
005000050923     d PRG4            S                   LIKE(VARNPR)
005100050923     d PRG5            S                   LIKE(VARNPR)
005200050923     d DMC             S                   LIKE(VARDMC)
005300050923     d NRR             S              9  0
005400050927     D WHHDAT          s             14  0
005500050927     D OGGI            s              8  0
005600050926     D ORA             s              6  0
005700040824      *
005800051108     D kpjba         E DS
005900050927     d Ds2n          e ds
006000051205     d fnvar         e ds                  extname(fnvar00r)
006100051205     d fnvars        e ds                  extname(fnvar10s) prefix(£)
006200050927     D WLBDA8          DS
006300050927     D  G02DAT                 1      8  0
006400050927     D  G02INV                 9     16  0
006500050927     D  G02ERR                17     17
006600050927     D  G02TGI                18     22  0
006700040820      *****************************************************************
006800040903     c*  Ciclo principale
006900040928     c*
007000051014     c* controllo che non ci sia disposioni e note x la stessa giac.
007100050928     c                   exsr      srdisnote
007200051014     c* controllo che non ci siano + disposizioni x la stessa giac.
007300050928     c                   exsr      srpiudis
007400051014     c* controllo che non ci sia + volte lo stesso progr. x la stessa giac.
007500051014     c                   exsr      srpiuprg
007600051014     c* controllo che le disposiz.ricevute siano congruenti x la stessa giac
007700051108     c                   exsr      srdisok
007800051014     c* controllo di non avere testo note doppi x la stessa giac.
007900050928     c                   exsr      srpiutes
008000051014     c* controllo di non avere ricevuto note generiche con progressivo >=900
008100051014     c                   exsr      srnotgen
008200051108     c*
008300051108     c                   commit
008400051206     C/EXEC SQL
008500051206     C+ COMMIT
008600051206     C/END-EXEC
008700051108     c*
008800050923      *
008900050923     C                   SETON                                        LR
009000050928     C**************************************************************************
009100050928     c* controllo che non ci sia disposioni e note
009200050928     C**************************************************************************
009300050928     C     srdisnote     BEGSR
009400051014     c*
009500050928     C/EXEC SQL
009600050928     C+ DECLARE E1 CURSOR FOR
009700050928     C+ select a.varagc, a.varfgc, a.varngc
009800050928     C+ from fnvar00r a join ( SELECT b.varagc, b.varfgc, b.varngc, b.varnpr
009900170223     C+  FROM fnvar00r b WHERE (b. varnpr = 900
010000170223     C+  and substr(vardmc, 1, 49) = 'RENDERE') or
010100170223     C+  (b.varnpr = 910 and substr(vardmc, 1, 49)
010200170223     C+   = 'RICONSEGNARE STESSO INDIRIZZO') OR
010300170223     C+ (b.varnpr = 911 and substr(vardmc, 1, 49) =
010400050928     C+ 'RICONSEGNARE STESSO INDIRIZZO CON APPUNTAMENTO') OR (b.varnpr =
010500170223     C+ 914 and substr(vardmc, 1, 49)
010600170223     C+  = 'RICONSEGNARE STESSO INDIRIZZO') OR (b.VArnpr =
010700170223     C+ 916 and substr(vardmc, 1, 49) = 'RICONSEGNARE - F.D.')
010800170223     C+ OR (b.varnpr = 920 and
010900170223     C+ substr(vardmc, 1, 49) ='RICONSEGNARE ALTRO INDIRIZZO')) c on a.varagc =
011000050928     C+ c.varagc and a.varfgc = c.varfgc and a.varngc = c.varngc where
011100050928     C+ a.varnpr < 900 and c.varnpr in (900, 910, 911, 914, 916 , 920)
011200050928     C+ GROUP BY a.varagc, a.varfgc, a.varngc
011300050928     C/END-EXEC
011400050928
011500050928     C/EXEC SQL
011600050928     C+ OPEN E1
011700050928     C/END-EXEC
011800051108     C                   DO        *hival
011900050928     C/EXEC SQL
012000050928     C+ FETCH NEXT FROM E1 INTO :agc, :fgc, :ngc
012100050928     C/END-EXEC
012200050928     C                   SELECT
012300050928     c*
012400051108     C                   WHEN      SqlCod = 100
012500051108     c                   leave
012600050928     C                   WHEN      SqlCod < 0
012700051108     c                   if        sqlcod <> -913
012800050928     C                   seton                                        H1
012900051108     c                   end
013000051108     c                   exsr      srfine
013100050928     c                   other
013200051108     c                   exsr      srwtr4
013300050928     C                   ENDSL
013400050928      *
013500050928     C                   ENDDO
013600050928     C/EXEC SQL
013700050928     C+ CLOSE E1
013800050928     C/END-EXEC
013900050928     c*
014000050928     c                   endsr
014100050928     C**************************************************************************
014200050928     c* controllo che non ci sia + disposioni
014300050928     C**************************************************************************
014400050928     C     srpiudis      BEGSR
014500051109     c*
014600050928     C/EXEC SQL
014700050928     C+ DECLARE D1 CURSOR FOR SELECT VARAGC, VARFGC, VARNGC, count(*)
014800050928     C+ FROM fnvar00r WHERE varnpr in (900, 911, 914, 916, 920) GROUP BY
014900050928     C+ VARAGC, VARFGC, VARNGC HAVING count(*) > 1
015000050928     C/END-EXEC
015100050928
015200050928     C/EXEC SQL
015300050928     C+ OPEN D1
015400050928     C/END-EXEC
015500051108     C                   Do        *hival
015600050928     C/EXEC SQL
015700050928     C+ FETCH NEXT FROM D1 INTO :agc, :fgc, :ngc, :conta
015800050928     C/END-EXEC
015900050928     C                   SELECT
016000050928     c*
016100051108     C                   WHEN      SqlCod = 100
016200051108     c                   leave
016300050928     C                   WHEN      SqlCod < 0
016400051108     c                   if        sqlcod <> -913
016500050928     C                   seton                                        H1
016600051108     c                   end
016700051108     c                   exsr      srfine
016800050928     c                   other
016900051108     c                   exsr      srwtr4
017000050928     C                   ENDSL
017100050928      *
017200050928     C                   ENDDO
017300050928     C/EXEC SQL
017400050928     C+ CLOSE D1
017500050928     C/END-EXEC
017600050928     c*
017700050928     c                   endsr
017800050928     C**************************************************************************
017900050928     c* controllo che non ci sia + volte lo stesso progressivo nota
018000050928     C**************************************************************************
018100050928     C     srpiuprg      BEGSR
018200051109     c*
018300050928     C/EXEC SQL
018400050928     C+ DECLARE B1 CURSOR FOR SELECT VARAGC, VARFGC, VARNGC, VARNPR,
018500050928     C+ count(*) FROM fnvar00r GROUP BY VARAGC, VARFGC, VARNGC, VARNPR
018600050928     C+ HAVING count(*) > 1
018700050928     C/END-EXEC
018800050928
018900050928     C/EXEC SQL
019000050928     C+ OPEN B1
019100050928     C/END-EXEC
019200051108     C                   DO        *hival
019300050928     C/EXEC SQL
019400050928     C+ FETCH NEXT FROM B1 INTO :agc, :fgc, :ngc, :npr, :conta
019500050928     C/END-EXEC
019600050928     C                   SELECT
019700050928     c*
019800051108     C                   WHEN      SqlCod = 100
019900051108     c                   leave
020000050928     C                   WHEN      SqlCod < 0
020100051108     c                   if        sqlcod <> -913
020200050928     C                   seton                                        H1
020300051108     c                   end
020400051108     c                   exsr      srfine
020500050928     c                   other
020600051108     c                   exsr      srwtr2
020700051108     c                   commit
020800050928     C                   ENDSL
020900050928      *
021000050928     C                   ENDDO
021100050928     C/EXEC SQL
021200050928     C+ CLOSE B1
021300050928     C/END-EXEC
021400050928     c*
021500050928     c                   endsr
021600050923     C**************************************************************************
021700050923     c* controllo le disposioni siano congruenti
021800050923     C**************************************************************************
021900050928     C     srdisok       BEGSR
022000051109     c*
022100050923     C/EXEC SQL
022200050923     C+ DECLARE C1 CURSOR FOR SELECT VARAGC, VARFGC, VARNGC, VARNPR FROM
022300050923     C+ fnvar00r WHERE VARNPR > 910 ORDER BY VARAGC, VARFGC, VARNGC,
022400050923     C+ VARNPR
022500050923     C/END-EXEC
022600050923
022700050923     C/EXEC SQL
022800050923     C+ OPEN C1
022900050923     C/END-EXEC
023000051108     C                   DO        *hival
023100050923     C/EXEC SQL
023200050923     C+ FETCH NEXT FROM C1 INTO :agc, :fgc, :ngc, :npr
023300050923     C/END-EXEC
023400050923     C                   SELECT
023500050923     c*
023600051108     C                   WHEN      SqlCod = 100
023700050928     c                   exsr      ctrdisok
023800051108     c                   leave
023900050923     C                   WHEN      SqlCod < 0
024000051108     c                   if        sqlcod <> -913
024100051108     C                   seton                                        H1
024200051108     c                   end
024300051108     c                   exsr      srfine
024400050923     c                   other
024500050928     c                   exsr      ctrdisok
024600051108     c                   add       1             x
024700051108     c                   eval      prg(x) = npr
024800050923     C                   ENDSL
024900050923      *
025000050923     C                   ENDDO
025100050923     C/EXEC SQL
025200050923     C+ CLOSE C1
025300050923     C/END-EXEC
025400050923     c*
025500050923     c                   endsr
025600050928     C**************************************************************************
025700050928     c* controllo di non avere disposizioni doppie
025800050928     C**************************************************************************
025900050928     C     srpiutes      BEGSR
026000050928      **
026100051109     C*
026200050928     C/EXEC SQL
026300050928     C+ DECLARE A1 CURSOR FOR SELECT VARAGC, VARFGC, VARNGC,
026400050928     C+ VARDMC, count(*) FROM fnvar00r GROUP BY VARAGC, VARFGC, VARngc,
026500050928     C+ vardmc HAVING count(*) > 1
026600050928     C/END-EXEC
026700050928
026800050928     C/EXEC SQL
026900050928     C+ OPEN A1
027000050928     C/END-EXEC
027100051108     C                   DO        *hival
027200050928     C/EXEC SQL
027300050928     C+ FETCH NEXT FROM A1 INTO :agc, :fgc, :ngc, :dmc, :conta
027400050928     C/END-EXEC
027500050928     C                   SELECT
027600050928     c*
027700051108     C                   WHEN      SqlCod = 100
027800051108     c                   leave
027900050928     C                   WHEN      SqlCod < 0
028000051108     c                   if        sqlcod <> -913
028100051108     C                   seton                                        H1
028200051108     c                   end
028300051108     c                   exsr      srfine
028400050928     c                   other
028500051108     c                   exsr      srwtr1
028600051108     c                   commit
028700050928     C                   ENDSL
028800050928      *
028900050928     C                   ENDDO
029000050928     C/EXEC SQL
029100050928     C+ CLOSE A1
029200050928     C/END-EXEC
029300050928     c*
029400050928     c                   endsr
029500051014     C**************************************************************************
029600051014     c* controllo di non avere ricevuto note generiche con progressivo >=900
029700051014     C**************************************************************************
029800051014     C     srnotgen      BEGSR
029900051014      **
030000051014     C/EXEC SQL
030100051205     C+ DECLARE M1 CURSOR FOR SELECT * FROM fnvar00r WHERE (varnpr = 900
030200170223     C+ and  substr(vardmc, 1, 49) <> 'RENDERE')
030300170223     C+ or (varnpr = 910 and substr(vardmc, 1, 49) <>
030400170223     C+ 'RICONSEGNARE STESSO INDIRIZZO')
030500170223     C+  OR (varnpr = 911 and substr(vardmc, 1, 49) <>
030600051205     C+ 'RICONSEGNARE STESSO INDIRIZZO CON APPUNTAMENTO') OR (varnpr =
030700051014     C+ 912 and substr(vardmc, 1, 8)<> 'TELEFONO') OR (varnpr = 913 and
030800170223     C+ substr(vardmc, 1, 4)<> 'SIG.')
030900170223     C+ OR (varnpr = 914 and substr(vardmc, 1, 49) <>
031000051014     C+ 'RICONSEGNARE STESSO INDIRIZZO') OR (VArnpr=915 and
031100051014     C+ (substr(vardmc, 1, 7) <> 'IN DATA') and (substr(vardmc, 1, 7) <>
031200170223     C+ 'DOPO IL'))
031300170223     C+ OR (VArnpr = 916 and substr(vardmc, 1, 49) <>'RICONSEGNARE - F.D.')
031400080922     C+ OR (varnpr = 917 and substr(vardmc, 1, 8) <> 'RITIRERA') or
031500170223     C+ (varnpr = 920 and substr(vardmc, 1, 49) <>
031600170223     C+ 'RICONSEGNARE ALTRO INDIRIZZO') FOR
031700051205     C+ UPDATE OF VARDRD
031800051014     C/END-EXEC
031900051014
032000051014     C/EXEC SQL
032100051014     C+ OPEN M1
032200051014     C/END-EXEC
032300051108     C                   DO        *hival
032400051014     C/EXEC SQL
032500051205     C+ FETCH NEXT FROM M1 INTO :fnvar
032600051014     C/END-EXEC
032700051014     C                   SELECT
032800051014     c*
032900051108     C                   WHEN      SqlCod = 100
033000051108     c                   leave
033100051014     C                   WHEN      SqlCod < 0
033200051108     c                   if        sqlcod <> -913
033300051108     C                   seton                                        H1
033400051108     c                   end
033500051108     c                   exsr      srfine
033600051014     c                   other
033700051206     c                   eval      %subst(fnvars: 1: 106) = fnvar
033800051205     c                   eval      £varesi = 'NPE'
033900051205     C/EXEC SQL
034000051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF M1
034100051205     C/END-EXEC
034200051205     c                   exsr      srwtr
034300051108     c                   commit
034400051014     C                   ENDSL
034500051014      *
034600051014     C                   ENDDO
034700051014     C/EXEC SQL
034800051014     C+ CLOSE M1
034900051014     C/END-EXEC
035000051014     c*
035100051014     c                   endsr
035200050923     ***********************************************************************
035300050923     C* controllo la sequenza delle disposizioni
035400050923     C**************************************************************************
035500050928     C     ctrdisok      BEGSR
035600050923     c* a rottura di giacenza testo la sequenza dei progressivi
035700050923     c                   if        agc <> sagc  or
035800050926     c                             ngc <> sngc  or
035900050928     c                             fgc <> sfgc  or
036000050928     c                             sqlcod = 100
036100050923     c                   if        sagc <> 0
036200050923     c                   if        (prg(1) = 911 and
036300050923     c                             (prg(2) <> 912 or prg(3) <> 913)) OR
036400050923     c                             (prg(1) = 914
036500050923     c                             and prg(2) <> 915) OR
036600050923     c                             (prg(1) = 916
036700050923     c                             and prg(2) <> 917) OR
036800050923     c                             (prg(1) = 920
036900051107     c                             and (prg(2) <> 921 or prg(3) <> 922 or
037000051107     c                             prg(4) <> 923 or prg(5) <> 924)) OR
037100050927     c                             (prg(1) <> 911 and
037200050927     c                             prg(1) <> 914 and
037300050927     c                             prg(1) <> 916 and
037400050927     c                             prg(1) <> 920)
037500050923     C                   EVAL      PRG1 = PRG(1)
037600050923     C                   EVAL      PRG2 = PRG(2)
037700050923     C                   EVAL      PRG3 = PRG(3)
037800050923     C                   EVAL      PRG4 = PRG(4)
037900050923     C                   EVAL      PRG5 = PRG(5)
038000050923     c                   exsr      srwtr3
038100050928     c                   commit
038200050923     c                   end
038300050923     c                   end
038400050923     c                   eval      sagc = agc
038500050923     c                   eval      sfgc = fgc
038600050923     c                   eval      sngc = ngc
038700050923     c                   clear                   prg
038800050923     c                   clear                   x
038900050923     c                   end
039000050923     c*
039100050923     c                   endsr
039200050923     ***********************************************************************
039300050928     C* cancella i record + vecchi con stesso testo nota
039400050923     C**************************************************************************
039500050923     C     srwtr1        BEGSR
039600050923     c                   clear                   volte
039700050923     c*
039800050923     C/EXEC SQL
039900051205     C+ DECLARE F1 CURSOR FOR SELECT * FROM fnvar00r
040000050926     C+ WHERE VARAGC=:agc and VARfGC=:fgc and VARnGC=:ngc and VARdmc=:dmc
040100051205     C+ ORDER BY VARDtr FOR UPDATE OF VARDRD
040200050923     C/END-EXEC
040300050923
040400050923     C/EXEC SQL
040500050923     C+ OPEN F1
040600050923     C/END-EXEC
040700051108     C                   DO        *hival
040800050923     C/EXEC SQL
040900051205     C+ FETCH NEXT FROM F1 INTO :fnvar
041000050923     C/END-EXEC
041100050923     C                   SELECT
041200050923     c*
041300051108     C                   WHEN      SqlCod = 100
041400051108     c                   leave
041500050923     C                   WHEN      SqlCod < 0
041600051108     c                   if        sqlcod <> -913
041700051108     C                   seton                                        H1
041800051108     c                   end
041900051108     c                   exsr      srfine
042000050923     c                   other
042100051108     c                   add       1             volte
042200051108     c                   if        volte < conta
042300051206     c                   eval      %subst(fnvars: 1: 106) = fnvar
042400051205     c                   eval      £varesi = 'TND'
042500051108     c                   exsr      srwtr
042600051205     C/EXEC SQL
042700051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF F1
042800051205     C/END-EXEC
042900051108     c                   else
043000051108     c                   leave
043100051108     c                   end
043200050923     C                   ENDSL
043300050923      *
043400050923     C                   ENDDO
043500050923     C/EXEC SQL
043600050923     C+ CLOSE F1
043700050923     C/END-EXEC
043800050923     c*
043900050923     c                   endsr
044000050923     C**************************************************************************
044100050928     C* cancella i record + vecchi con stesso progressivo
044200050923     C**************************************************************************
044300050923     C     srwtr2        BEGSR
044400050923     c                   clear                   volte
044500051109     c*
044600050923     C/EXEC SQL
044700051205     C+ DECLARE G1 CURSOR FOR SELECT * FROM fnvar00r WHERE VARAGC=:agc
044800051205     C+ and VARfGC=:fgc and VARnGC=:ngc and VARnpr=:npr ORDER BY VARdtr
044900051205     C+ FOR UPDATE OF VARDRD
045000050923     C/END-EXEC
045100050923     C/EXEC SQL
045200050923     C+ OPEN G1
045300050923     C/END-EXEC
045400051108     C                   DO        *hival
045500050923     C/EXEC SQL
045600051205     C+ FETCH NEXT FROM G1 INTO :fnvar
045700050923     C/END-EXEC
045800050923     C                   SELECT
045900050923     c*
046000051108     C                   WHEN      SqlCod = 100
046100051108     c                   leave
046200050923     C                   WHEN      SqlCod < 0
046300051108     c                   if        sqlcod <> -913
046400051108     C                   seton                                        H1
046500051108     c                   end
046600051108     c                   exsr      srfine
046700050923     c                   other
046800051108     c                   add       1             volte
046900051108     c                   if        volte < conta
047000051206     c                   eval      %subst(fnvars: 1: 106) = fnvar
047100051205     c                   eval      £varesi = 'DPD'
047200051108     c                   exsr      srwtr
047300051205     C/EXEC SQL
047400051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF G1
047500051205     C/END-EXEC
047600051108     c                   end
047700050923     C                   ENDSL
047800050923      *
047900050923     C                   ENDDO
048000050923     C/EXEC SQL
048100050923     C+ CLOSE G1
048200050923     C/END-EXEC
048300050923
048400050923     c*
048500050923     c                   endsr
048600050923     C**************************************************************************
048700050928     C* cancella le disposizioni incomplete
048800050928     C* es. se ho i progressivi 911 e 912 ma non 913 allora li cancello
048900050923     C**************************************************************************
049000050923     C     srwtr3        BEGSR
049100050923
049200050923     C/EXEC SQL
049300051205     C+ DECLARE H1 CURSOR FOR SELECT * FROM fnvar00r WHERE varagc =:sagc
049400051205     C+ and VARfGC=:sfgc and VARnGC=:sngc and varnpr in (:prg1, :prg2,
049500051205     C+ :prg3, :prg4, :prg5) FOR UPDATE OF vardtr
049600050923     C/END-EXEC
049700050923
049800050923     C/EXEC SQL
049900050923     C+ OPEN H1
050000050923     C/END-EXEC
050100051108     C                   DO        *hival
050200050923     C/EXEC SQL
050300051205     C+ FETCH NEXT FROM H1 INTO :fnvar
050400050923     C/END-EXEC
050500050923     C                   SELECT
050600050923     c*
050700051108     C                   WHEN      SqlCod = 100
050800051108     c                   leave
050900050923     C                   WHEN      SqlCod < 0
051000051108     c                   if        sqlcod <> -913
051100051108     C                   seton                                        H1
051200051108     c                   end
051300051108     c                   exsr      srfine
051400050923     c                   other
051500051206     c                   eval      %subst(fnvars: 1: 106) = fnvar
051600051205     C                   EVAL      £VARESI = 'DIN'
051700051108     c                   exsr      srwtr
051800051205     C/EXEC SQL
051900051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF H1
052000051205     C/END-EXEC
052100050923     C                   ENDSL
052200050923      *
052300050923     C                   ENDDO
052400050923     C/EXEC SQL
052500050923     C+ CLOSE H1
052600050923     C/END-EXEC
052700050923
052800050923     c*
052900050923     c                   endsr
053000050923     C**************************************************************************
053100050928     C* cerco il nrr più alto (ultimo ad essere inserito), prelevo il
053200050928     C* progressivo in modo da scartare durante la cancellaione il range dei
053300050928     C* progressivi appartenenti a questo
053400050923     C**************************************************************************
053500050923     C     srwtr4        BEGSR
053600050923
053700050923     C/EXEC SQL
053800050927     C+ DECLARE I1 CURSOR FOR SELECT max(rrn(fnvar00r)) FROM fnvar00r
053900051205     C+ WHERE varagc =:agc and VARfGC=:fgc and VARnGC=:ngc OPTIMIZE FOR 1
054000051205     C+ ROWS
054100050923     C/END-EXEC
054200050923
054300050923     C/EXEC SQL
054400050923     C+ OPEN I1
054500050923     C/END-EXEC
054600051108     C                   DO        *hival
054700050923     C/EXEC SQL
054800050927     C+ FETCH NEXT FROM I1 INTO :nrr
054900050923     C/END-EXEC
055000050923     C                   SELECT
055100050923     c*
055200051108     C                   WHEN      SqlCod = 100
055300051108     c                   leave
055400050923     C                   WHEN      SqlCod < 0
055500051108     c                   if        sqlcod <> -913
055600051108     C                   seton                                        H1
055700051108     c                   end
055800051108     c                   exsr      srfine
055900050923     c                   other
056000051108     c                   exsr      srwtr41
056100050923     c                   leave
056200050923     C                   ENDSL
056300050923      *
056400050923     C                   ENDDO
056500050923     C/EXEC SQL
056600050923     C+ CLOSE I1
056700050923     C/END-EXEC
056800050923
056900050923     c*
057000050923     c                   endsr
057100050923     C**************************************************************************
057200050928     C* scarto il range dei progressivi dell'ultima disposizione immessa
057300050923     C**************************************************************************
057400050923     C     srwtr41       BEGSR
057500050927     c     nrr           chain(n)  fnvar00r
057600050928     c* imposto il range dei progressivi da scartare dalla delete
057700050923     c                   select
057800050927     c                   when      varnpr < 900
057900050927     c                   eval      npr = 0
058000050923     c                   eval      nprfine = 899
058100050927     c                   when      varnpr = 900
058200050927     c                   eval      npr = 900
058300050923     c                   eval      nprfine = 900
058400050927     c                   when      varnpr >= 911 and varnpr <= 913
058500050927     c                   eval      npr = 911
058600050923     c                   eval      nprfine = 913
058700050927     c                   when      varnpr = 914 or varnpr = 915
058800050927     c                   eval      npr = 914
058900050923     c                   eval      nprfine = 915
059000050927     c                   when      varnpr = 916 or varnpr = 917
059100050927     c                   eval      npr = 916
059200050923     c                   eval      nprfine = 917
059300050927     c                   when      varnpr >= 920 and varnpr <= 924
059400050927     c                   eval      npr = 920
059500050923     c                   eval      nprfine = 924
059600050923     c                   endsl
059700050923
059800051109     c*
059900050923     C/EXEC SQL
060000051205     C+ DECLARE L1 CURSOR FOR SELECT * FROM fnvar00r WHERE varagc =:agc
060100051205     C+ and VARfGC=:fgc and VARnGC=:ngc and varnpr not between :npr and
060200051205     C+ :nprfine FOR UPDATE OF vardtr
060300050923     C/END-EXEC
060400050923
060500050923     C/EXEC SQL
060600050923     C+ OPEN L1
060700050923     C/END-EXEC
060800051108     C                   DO        *hival
060900050923     C/EXEC SQL
061000051205     C+ FETCH NEXT FROM L1 INTO :fnvar
061100050923     C/END-EXEC
061200050923     C                   SELECT
061300050923     c*
061400051108     C                   WHEN      SqlCod = 100
061500051108     c                   leave
061600050923     C                   WHEN      SqlCod < 0
061700051108     c                   if        sqlcod <> -913
061800051108     C                   seton                                        H1
061900051108     c                   end
062000051108     c                   exsr      srfine
062100050923     c                   other
062200051206     C                   EVAL      %subst(fnvars: 1: 106) = fnvar
062300051205     C                   EVAL      £varesi = 'IPD'
062400051108     c                   exsr      srwtr
062500051205     C/EXEC SQL
062600051205     C+ DELETE FROM FNVAR00R WHERE CURRENT OF L1
062700051205     C/END-EXEC
062800050923     C                   ENDSL
062900050923      *
063000050923     C                   ENDDO
063100050923     C/EXEC SQL
063200050923     C+ CLOSE L1
063300050923     C/END-EXEC
063400050928     c                   commit
063500050923     c*
063600050923     c                   endsr
063700050927     C**************************************************************************
063800051108     C* esco con codice d'errore
063900050927     C**************************************************************************
064000051108     C     srfine        BEGSR
064100051108     c*
064200051108     c                   rolbk
064300051110     C/EXEC SQL
064400051110     C+ ROLLBACK
064500051110     C/END-EXEC
064600051108     c*
064700051108     c* record allocato
064800051108     c                   eval      kpjbu = '2'
064900051108     c* errore sql
065000051108     c                   if        *inh1
065100051108     c                   eval      kpjbu = '1'
065200051108     c                   end
065300051108     c                   seton                                        lr
065400051108     c                   return
065500051108     c*
065600051108     c                   endsr
065700051108     C**************************************************************************
065800051108     C* scrivo scarti nello storico
065900051108     C**************************************************************************
066000051108     C     srwtr         BEGSR
066100050927     c* reperisco il flag se visualizzabile dal po s/n
066200050927     c                   z-add     1             xx
066300051205     c     £varesi       lookup    esi(xx)                                01
066400050927     c                   if        *in01
066500051205     c                   movel     vis(xx)       £varvis
066600050927     c                   else
066700051205     c                   clear                   £varvis
066800050927     c                   end
066900051205     C                   Z-ADD     OGGI          £VARdrd
067000051205     c                   z-add     ora           £varorz
067100050923     C                   WRITE     FNVARS10
067200050923     c*
067300050923     c                   endsr
067400050927     C**************************************************************************
067500050927     C* sub. inizio
067600050927     C**************************************************************************
067700050927     C     *inzsr        BEGSR
067800051108     C     *ENTRY        PLIST
067900051108     C                   PARM                    KPJBA
068000050927     C*  Reperisco data del giorno
068100050927     C                   CLEAR                   WLBDA8
068200050927     C                   TIME                    WHHDAT
068300050927     C                   MOVE      WHHDAT        G02DAT
068400050927     C                   Z-ADD     0             G02INV
068500050927     C                   MOVEL     ' '           G02ERR
068600050927     C                   CALL      'XSRDA8'
068700050927     C                   PARM                    WLBDA8
068800050927     C                   Z-ADD     G02INV        OGGI
068900050927     C                   MOVEl     WHHDAT        ora
069000050927     c*
069100050927     c                   z-add     1             tblkut
069200050927     c                   eval      tblcod = '2N'
069300050927     C     KTAB          KLIST
069400050927     C                   KFLD                    tblkut
069500050927     C                   KFLD                    TBLCOD
069600050927     c* carico tabella 2N x esito elaborazione
069700050927     c                   clear                   ds2n
069800050927     c                   clear                   xx
069900050927     C     KTAB          setll     TABEL00f
070000050927     c                   do        *hival
070100050927     C     KTAB          reade     TABEL00f
070200050927     C                   if        %eof(tabel00f)
070300050927     c                   leave
070400050927     c                   end
070500050927     c                   if        tblflg <> ' '
070600050927     c                   iter
070700050927     c                   end
070800050927     C                   MOVEL     TBLUNI        DS2n
070900050927     c                   add       1             xx
071000050927     c                   movel     tblkey        esi(xx)
071100050927     c                   movel     §2Nvis        vis(xx)
071200050927     c                   enddo
071300050927     c*
071400050927     c                   endsr
