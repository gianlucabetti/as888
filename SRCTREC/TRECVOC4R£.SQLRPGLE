000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300000920     H DECEDIT('0,') DATEDIT(*DMY.)
000400970526     F*------------------------------------------------------------------------*
000500970526     F* DATA BASE
000600970526     F*------------------------------------------------------------------------*
000700050408     Fsysprt    o    f  132        printer
000800110420     Fecevt01l  if   e           k disk
000900111011     Feccev30c  if   e           k disk
001000111011     d mmgg1           s                   like(tasmgs)
001100111011     d mmgg2           s                   like(tasmgs)
001200111011     d lnp1            s                   like(taslnp)
001300111011     d lnp2            s                   like(taslnp)
001400111011     d lna1            s                   like(taslna)
001500111011     d lna2            s                   like(taslna)
001600111011     d pdc1            s                   like(taspdc)
001700111011     d pdc2            s                   like(taspdc)
001800111011     d pdr1            s                   like(taspdr)
001900111011     d pdr2            s                   like(taspdr)
002000100520     d totale          s             15  5
002100111011     d titasds       e ds                  extname(titas00f)
002200110420     D KPJBA         E DS
002300110420     d param           ds
002400110420     d voc                     1      3s 0
002500110420     d dta                     4     11s 0
002600110420     d aa                      4      7s 0
002700110420     d mm                      8      9s 0
002800110420     d gg                     10     11s 0
002900111011     d lnp                    12     14s 0
003000111011     d lna                    15     17s 0
003100111011     d pdc                    18     24s 0
003200111011     d pdr                    25     31s 0
003300110420     c* --------------------------------------------------------------------
003400110420     C     *ENTRY        PLIST
003500110420     C                   PARM                    KPJBA
003600110420     C                   MOVEL     KPJBU         param
003700110420     c     ktas          klist
003800111011     c                   kfld                    tasaas
003900111011     c                   kfld                    taslnp
004000111011     c                   kfld                    tasnrs
004100111011     c                   kfld                    tasnsp
004200111011     c                   kfld                    tastbl
004300110420     c     voc           chain     ecevt01l
004400110420     c                   if        not %found(ecevt01l)
004500110420     c                   return
004600100519     c                   end
004700110420     c                   if        evttvo <> 'V'
004800110420     c                   return
004900110420     c                   end
005000111011     c*
005100050502     c                   clear                   totale
005200111011     c*
005300111011     c                   clear                   mmgg1
005400111011     c                   clear                   mmgg2
005500111011     c                   movel     mm            mmgg1
005600111011     c                   movel     mm            mmgg2
005700111011     c                   if        gg <> 0
005800111011     c                   move      gg            mmgg1
005900111011     c                   move      gg            mmgg2
006000111011     c                   else
006100111011     c                   move      31            mmgg2
006200111011     c                   end
006300111011     c*
006400111011     c                   clear                   lnp1
006500111011     c                   eval      lnp2 = *hival
006600111011     c                   if        lnp <> 0
006700111011     c                   eval      lnp1 = lnp
006800111011     c                   eval      lnp2 = lnp
006900111011     c                   end
007000111011     c*
007100111011     c                   clear                   lna1
007200111011     c                   eval      lna2 = *hival
007300111011     c                   if        lna <> 0
007400111011     c                   eval      lna1 = lna
007500111011     c                   eval      lna2 = lna
007600111011     c                   end
007700111011     c*
007800111011     c                   clear                   pdc1
007900111011     c                   eval      pdc2 = *hival
008000111011     c                   if        pdc <> 0
008100111011     c                   eval      pdc1 = pdc
008200111011     c                   eval      pdc2 = pdc
008300111011     c                   else
008400111011     c                   if        lna <> 0
008500111011     c                   movel     lna           pdc1
008600111011     c                   movel     lna           pdc2
008700111011     c                   end
008800111011     c                   end
008900111011     c*
009000111011     c                   clear                   pdr1
009100111011     c                   eval      pdr2 = *hival
009200111011     c                   if        pdr <> 0
009300111011     c                   eval      pdr1 = pdr
009400111011     c                   eval      pdr2 = pdr
009500111011     c                   else
009600111011     c                   if        lnp <> 0
009700111011     c                   movel     lnp           pdr1
009800111011     c                   movel     lnp           pdr2
009900111011     c                   end
010000111011     c                   end
010100111011     c*
010200111011     c                   exsr      tas00
010300111011     c*
010400111011     c                   exsr      tas10
010500111011     c*
010600111011     c                   except    riga
010700111011     c                   seton                                        lr
010800111011     c**********************************************************************
010900111011     c     tas00         begsr
011000111011     c**********************************************************************
011100050502     C/EXEC SQL
011200111011     C+ DECLARE C1 CURSOR FOR SELECT * FROM titas00f WHERE tasAAS = :aa
011300111011     C+ and tasmgs between :mmgg1 and :mmgg2
011400111011     C+ and taslnp between :lnp1 and :lnp2
011500111011     C+ and taspdr between :pdr1 and :pdr2 and taslna between :lna1
011600111011     C+ and :lna2 and taspdc between :pdc1 and :pdc2
011700050502     C/END-EXEC
011800110420     c*
011900050502     C/EXEC SQL
012000050502     C+ OPEN c1
012100050502     C/END-EXEC
012200110420     c*
012300050502
012400050502     C                   DO        *HIVAL
012500050502
012600050502     C/EXEC SQL
012700111011     C+ FETCH NEXT FROM c1 INTO :titasds
012800050502     C/END-EXEC
012900050502
013000110420     C                   IF        SQLCod = 100 or sqlcod < 0
013100050502     C                   LEAVE
013200050502     C                   ENDIF
013300110420
013400111011     c     ktas          chain     eccev30c
013500111011     c                   if           not %found(eccev30c)
013600110420     c                   iter
013700110420     c                   end
013800110420
013900050502     c                   EXSR      conta
014000050502
014100050502     C                   ENDDO
014200050502
014300050502     C/EXEC SQL
014400050502     C+ CLOSE c1
014500050502     C/END-EXEC
014600111011     c                   endsr
014700111011     c**********************************************************************
014800111011     c     tas10         begsr
014900111011     c**********************************************************************
015000111011     C/EXEC SQL
015100111011     C+ DECLARE C10 CURSOR FOR SELECT * FROM titas10f WHERE tasAAS = :aa
015200111011     C+ and tasmgs between :mmgg1 and :mmgg2
015300111011     C+ and taslnp between :lnp1 and :lnp2
015400111011     C+ and taspdr between :pdr1 and :pdr2 and taslna between :lna1
015500111011     C+ and :lna2 and taspdc between :pdc1 and :pdc2
015600111011     C/END-EXEC
015700111011     c*
015800111011     C/EXEC SQL
015900111011     C+ OPEN c10
016000111011     C/END-EXEC
016100111011     c*
016200111011
016300111011     C                   DO        *HIVAL
016400111011
016500111011     C/EXEC SQL
016600111011     C+ FETCH NEXT FROM c10 INTO :titasds
016700111011     C/END-EXEC
016800111011
016900111011     C                   IF        SQLCod = 100 or sqlcod < 0
017000111011     C                   LEAVE
017100111011     C                   ENDIF
017200111011
017300111011     c     ktas          chain     eccev30c
017400111011     c                   if           not %found(eccev30c)
017500111011     c                   iter
017600111011     c                   end
017700111011
017800111011     c                   EXSR      conta
017900111011
018000111011     C                   ENDDO
018100111011
018200111011     C/EXEC SQL
018300111011     C+ CLOSE c10
018400111011     C/END-EXEC
018500050429     c*
018600111011     c                   endsr
018700050408     C*--------------------------------------------------------------- ---------
018800050408     C     conta         BEGSR
018900100519     c                   if        cevc01 = evtvoc
019000050408     c                   add       cevi01        totale
019100050408     c                   end
019200100519     c                   if        cevc02 = evtvoc
019300050408     c                   add       cevi02        totale
019400050408     c                   end
019500100519     c                   if        cevc03 = evtvoc
019600050408     c                   add       cevi03        totale
019700050408     c                   end
019800100519     c                   if        cevc04 = evtvoc
019900050408     c                   add       cevi04        totale
020000050408     c                   end
020100100519     c                   if        cevc05 = evtvoc
020200050408     c                   add       cevi05        totale
020300050408     c                   end
020400100519     c                   if        cevc06 = evtvoc
020500050408     c                   add       cevi06        totale
020600050408     c                   end
020700100519     c                   if        cevc07 = evtvoc
020800050408     c                   add       cevi07        totale
020900050408     c                   end
021000100519     c                   if        cevc08 = evtvoc
021100050408     c                   add       cevi08        totale
021200050408     c                   end
021300100519     c                   if        cevc09 = evtvoc
021400050408     c                   add       cevi09        totale
021500050408     c                   end
021600100519     c                   if        cevc10 = evtvoc
021700050408     c                   add       cevi10        totale
021800050408     c                   end
021900100519     c                   if        cevc11 = evtvoc
022000050408     c                   add       cevi11        totale
022100050408     c                   end
022200100519     c                   if        cevc12 = evtvoc
022300050408     c                   add       cevi12        totale
022400050408     c                   end
022500100519     c                   if        cevc13 = evtvoc
022600050408     c                   add       cevi13        totale
022700050408     c                   end
022800100519     c                   if        cevc14 = evtvoc
022900050408     c                   add       cevi14        totale
023000050408     c                   end
023100100519     c                   if        cevc15 = evtvoc
023200050408     c                   add       cevi15        totale
023300050408     c                   end
023400100519     c                   if        cevc16 = evtvoc
023500050408     c                   add       cevi16        totale
023600050408     c                   end
023700100519     c                   if        cevc17 = evtvoc
023800050408     c                   add       cevi17        totale
023900050408     c                   end
024000100519     c                   if        cevc18 = evtvoc
024100050408     c                   add       cevi18        totale
024200050408     c                   end
024300100519     c                   if        cevc19 = evtvoc
024400050408     c                   add       cevi19        totale
024500050408     c                   end
024600100519     c                   if        cevc20 = evtvoc
024700050408     c                   add       cevi20        totale
024800050408     c                   end
024900100519     c                   if        cevc21 = evtvoc
025000050408     c                   add       cevi21        totale
025100050408     c                   end
025200100519     c                   if        cevc22 = evtvoc
025300050408     c                   add       cevi22        totale
025400050408     c                   end
025500100519     c                   if        cevc23 = evtvoc
025600050408     c                   add       cevi23        totale
025700050408     c                   end
025800100519     c                   if        cevc24 = evtvoc
025900050408     c                   add       cevi24        totale
026000050408     c                   end
026100100519     c                   if        cevc25 = evtvoc
026200050408     c                   add       cevi25        totale
026300050408     c                   end
026400100519     c                   if        cevc26 = evtvoc
026500050408     c                   add       cevi26        totale
026600050408     c                   end
026700100519     c                   if        cevc27 = evtvoc
026800050408     c                   add       cevi27        totale
026900050408     c                   end
027000100519     c                   if        cevc28 = evtvoc
027100050408     c                   add       cevi28        totale
027200050408     c                   end
027300100519     c                   if        cevc29 = evtvoc
027400050408     c                   add       cevi29        totale
027500050408     c                   end
027600100519     c                   if        cevc30 = evtvoc
027700050408     c                   add       cevi30        totale
027800050408     c                   end
027900100519     c                   if        cevc31 = evtvoc
028000050408     c                   add       cevi31        totale
028100050408     c                   end
028200100519     c                   if        cevc32 = evtvoc
028300050408     c                   add       cevi32        totale
028400050408     c                   end
028500100519     c                   if        cevc33 = evtvoc
028600050408     c                   add       cevi33        totale
028700050408     c                   end
028800100519     c                   if        cevc34 = evtvoc
028900050408     c                   add       cevi34        totale
029000050408     c                   end
029100100519     c                   if        cevc35 = evtvoc
029200050408     c                   add       cevi35        totale
029300050408     c                   end
029400100519     c                   if        cevc36 = evtvoc
029500050408     c                   add       cevi36        totale
029600050408     c                   end
029700100519     c                   if        cevc37 = evtvoc
029800050408     c                   add       cevi37        totale
029900050408     c                   end
030000100519     c                   if        cevc38 = evtvoc
030100050408     c                   add       cevi38        totale
030200050408     c                   end
030300100519     c                   if        cevc39 = evtvoc
030400050408     c                   add       cevi39        totale
030500050408     c                   end
030600100519     c                   if        cevc40 = evtvoc
030700050408     c                   add       cevi40        totale
030800050408     c                   end
030900100519     c                   if        cevc41 = evtvoc
031000050408     c                   add       cevi41        totale
031100050408     c                   end
031200100519     c                   if        cevc42 = evtvoc
031300050408     c                   add       cevi42        totale
031400050408     c                   end
031500100519     c                   if        cevc43 = evtvoc
031600050408     c                   add       cevi43        totale
031700050408     c                   end
031800100519     c                   if        cevc44 = evtvoc
031900050408     c                   add       cevi44        totale
032000050408     c                   end
032100100519     c                   if        cevc45 = evtvoc
032200050408     c                   add       cevi45        totale
032300050408     c                   end
032400100519     c                   if        cevc46 = evtvoc
032500050408     c                   add       cevi46        totale
032600050429     c                   end
032700100519     c                   if        cevc47 = evtvoc
032800050408     c                   add       cevi47        totale
032900050408     c                   end
033000100519     c                   if        cevc48 = evtvoc
033100050408     c                   add       cevi48        totale
033200050408     c                   end
033300100519     c                   if        cevc49 = evtvoc
033400050408     c                   add       cevi49        totale
033500050408     c                   end
033600100519     c                   if        cevc50 = evtvoc
033700050408     c                   add       cevi50        totale
033800050408     c                   end
033900050408     C                   ENDSR
034000050408     osysprt    e            riga        1
034100110420c    o                                           +0 'ECCEV00F: Totale voce '
034200100519c    o                       evtvoc              +1
034300100519c    o                       totale        4     +1
