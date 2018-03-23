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
003100111011     d pdr                    18     24s 0
003200111011     d pdc                    25     31s 0
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
010200111011     c                   if        mm = 0  or (mm = *month)
010300111011     c                   exsr      tas00
010400111011     c                   end
010500111011     c*
010600111011     c                   if        mm = 0  or (mm < *month)
010700111011     c                   exsr      tas10
010800111011     c                   end
010900111011     c*
011000111011     c                   except    riga
011100111011     c                   seton                                        lr
011200111011     c**********************************************************************
011300111011     c     tas00         begsr
011400111011     c**********************************************************************
011500050502     C/EXEC SQL
011600111011     C+ DECLARE C1 CURSOR FOR SELECT * FROM titas00f WHERE tasAAS = :aa
011700111011     C+ and tasmgs between :mmgg1 and :mmgg2
011800111011     C+ and taslnp between :lnp1 and :lnp2
011900111011     C+ and taspdr between :pdr1 and :pdr2 and taslna between :lna1
012000111011     C+ and :lna2 and taspdc between :pdc1 and :pdc2
012100050502     C/END-EXEC
012200110420     c*
012300050502     C/EXEC SQL
012400050502     C+ OPEN c1
012500050502     C/END-EXEC
012600110420     c*
012700050502
012800050502     C                   DO        *HIVAL
012900050502
013000050502     C/EXEC SQL
013100111011     C+ FETCH NEXT FROM c1 INTO :titasds
013200050502     C/END-EXEC
013300050502
013400110420     C                   IF        SQLCod = 100 or sqlcod < 0
013500050502     C                   LEAVE
013600050502     C                   ENDIF
013700110420
013800111011     c     ktas          chain     eccev30c
013900111011     c                   if           not %found(eccev30c)
014000110420     c                   iter
014100110420     c                   end
014200110420
014300050502     c                   EXSR      conta
014400050502
014500050502     C                   ENDDO
014600050502
014700050502     C/EXEC SQL
014800050502     C+ CLOSE c1
014900050502     C/END-EXEC
015000111011     c                   endsr
015100111011     c**********************************************************************
015200111011     c     tas10         begsr
015300111011     c**********************************************************************
015400111011     C/EXEC SQL
015500111011     C+ DECLARE C10 CURSOR FOR SELECT * FROM titas10f WHERE tasAAS = :aa
015600111011     C+ and tasmgs between :mmgg1 and :mmgg2
015700111011     C+ and taslnp between :lnp1 and :lnp2
015800111011     C+ and taspdr between :pdr1 and :pdr2 and taslna between :lna1
015900111011     C+ and :lna2 and taspdc between :pdc1 and :pdc2
016000111011     C/END-EXEC
016100111011     c*
016200111011     C/EXEC SQL
016300111011     C+ OPEN c10
016400111011     C/END-EXEC
016500111011     c*
016600111011
016700111011     C                   DO        *HIVAL
016800111011
016900111011     C/EXEC SQL
017000111011     C+ FETCH NEXT FROM c10 INTO :titasds
017100111011     C/END-EXEC
017200111011
017300111011     C                   IF        SQLCod = 100 or sqlcod < 0
017400111011     C                   LEAVE
017500111011     C                   ENDIF
017600111011
017700111011     c     ktas          chain     eccev30c
017800111011     c                   if           not %found(eccev30c)
017900111011     c                   iter
018000111011     c                   end
018100111011
018200111011     c                   EXSR      conta
018300111011
018400111011     C                   ENDDO
018500111011
018600111011     C/EXEC SQL
018700111011     C+ CLOSE c10
018800111011     C/END-EXEC
018900050429     c*
019000111011     c                   endsr
019100050408     C*--------------------------------------------------------------- ---------
019200050408     C     conta         BEGSR
019300100519     c                   if        cevc01 = evtvoc
019400050408     c                   add       cevi01        totale
019500050408     c                   end
019600100519     c                   if        cevc02 = evtvoc
019700050408     c                   add       cevi02        totale
019800050408     c                   end
019900100519     c                   if        cevc03 = evtvoc
020000050408     c                   add       cevi03        totale
020100050408     c                   end
020200100519     c                   if        cevc04 = evtvoc
020300050408     c                   add       cevi04        totale
020400050408     c                   end
020500100519     c                   if        cevc05 = evtvoc
020600050408     c                   add       cevi05        totale
020700050408     c                   end
020800100519     c                   if        cevc06 = evtvoc
020900050408     c                   add       cevi06        totale
021000050408     c                   end
021100100519     c                   if        cevc07 = evtvoc
021200050408     c                   add       cevi07        totale
021300050408     c                   end
021400100519     c                   if        cevc08 = evtvoc
021500050408     c                   add       cevi08        totale
021600050408     c                   end
021700100519     c                   if        cevc09 = evtvoc
021800050408     c                   add       cevi09        totale
021900050408     c                   end
022000100519     c                   if        cevc10 = evtvoc
022100050408     c                   add       cevi10        totale
022200050408     c                   end
022300100519     c                   if        cevc11 = evtvoc
022400050408     c                   add       cevi11        totale
022500050408     c                   end
022600100519     c                   if        cevc12 = evtvoc
022700050408     c                   add       cevi12        totale
022800050408     c                   end
022900100519     c                   if        cevc13 = evtvoc
023000050408     c                   add       cevi13        totale
023100050408     c                   end
023200100519     c                   if        cevc14 = evtvoc
023300050408     c                   add       cevi14        totale
023400050408     c                   end
023500100519     c                   if        cevc15 = evtvoc
023600050408     c                   add       cevi15        totale
023700050408     c                   end
023800100519     c                   if        cevc16 = evtvoc
023900050408     c                   add       cevi16        totale
024000050408     c                   end
024100100519     c                   if        cevc17 = evtvoc
024200050408     c                   add       cevi17        totale
024300050408     c                   end
024400100519     c                   if        cevc18 = evtvoc
024500050408     c                   add       cevi18        totale
024600050408     c                   end
024700100519     c                   if        cevc19 = evtvoc
024800050408     c                   add       cevi19        totale
024900050408     c                   end
025000100519     c                   if        cevc20 = evtvoc
025100050408     c                   add       cevi20        totale
025200050408     c                   end
025300100519     c                   if        cevc21 = evtvoc
025400050408     c                   add       cevi21        totale
025500050408     c                   end
025600100519     c                   if        cevc22 = evtvoc
025700050408     c                   add       cevi22        totale
025800050408     c                   end
025900100519     c                   if        cevc23 = evtvoc
026000050408     c                   add       cevi23        totale
026100050408     c                   end
026200100519     c                   if        cevc24 = evtvoc
026300050408     c                   add       cevi24        totale
026400050408     c                   end
026500100519     c                   if        cevc25 = evtvoc
026600050408     c                   add       cevi25        totale
026700050408     c                   end
026800100519     c                   if        cevc26 = evtvoc
026900050408     c                   add       cevi26        totale
027000050408     c                   end
027100100519     c                   if        cevc27 = evtvoc
027200050408     c                   add       cevi27        totale
027300050408     c                   end
027400100519     c                   if        cevc28 = evtvoc
027500050408     c                   add       cevi28        totale
027600050408     c                   end
027700100519     c                   if        cevc29 = evtvoc
027800050408     c                   add       cevi29        totale
027900050408     c                   end
028000100519     c                   if        cevc30 = evtvoc
028100050408     c                   add       cevi30        totale
028200050408     c                   end
028300100519     c                   if        cevc31 = evtvoc
028400050408     c                   add       cevi31        totale
028500050408     c                   end
028600100519     c                   if        cevc32 = evtvoc
028700050408     c                   add       cevi32        totale
028800050408     c                   end
028900100519     c                   if        cevc33 = evtvoc
029000050408     c                   add       cevi33        totale
029100050408     c                   end
029200100519     c                   if        cevc34 = evtvoc
029300050408     c                   add       cevi34        totale
029400050408     c                   end
029500100519     c                   if        cevc35 = evtvoc
029600050408     c                   add       cevi35        totale
029700050408     c                   end
029800100519     c                   if        cevc36 = evtvoc
029900050408     c                   add       cevi36        totale
030000050408     c                   end
030100100519     c                   if        cevc37 = evtvoc
030200050408     c                   add       cevi37        totale
030300050408     c                   end
030400100519     c                   if        cevc38 = evtvoc
030500050408     c                   add       cevi38        totale
030600050408     c                   end
030700100519     c                   if        cevc39 = evtvoc
030800050408     c                   add       cevi39        totale
030900050408     c                   end
031000100519     c                   if        cevc40 = evtvoc
031100050408     c                   add       cevi40        totale
031200050408     c                   end
031300100519     c                   if        cevc41 = evtvoc
031400050408     c                   add       cevi41        totale
031500050408     c                   end
031600100519     c                   if        cevc42 = evtvoc
031700050408     c                   add       cevi42        totale
031800050408     c                   end
031900100519     c                   if        cevc43 = evtvoc
032000050408     c                   add       cevi43        totale
032100050408     c                   end
032200100519     c                   if        cevc44 = evtvoc
032300050408     c                   add       cevi44        totale
032400050408     c                   end
032500100519     c                   if        cevc45 = evtvoc
032600050408     c                   add       cevi45        totale
032700050408     c                   end
032800100519     c                   if        cevc46 = evtvoc
032900050408     c                   add       cevi46        totale
033000050429     c                   end
033100100519     c                   if        cevc47 = evtvoc
033200050408     c                   add       cevi47        totale
033300050408     c                   end
033400100519     c                   if        cevc48 = evtvoc
033500050408     c                   add       cevi48        totale
033600050408     c                   end
033700100519     c                   if        cevc49 = evtvoc
033800050408     c                   add       cevi49        totale
033900050408     c                   end
034000100519     c                   if        cevc50 = evtvoc
034100050408     c                   add       cevi50        totale
034200050408     c                   end
034300050408     C                   ENDSR
034400050408     osysprt    e            riga        1
034500110420c    o                                           +0 'ECCEV00F: Totale voce '
034600100519c    o                       evtvoc              +1
034700100519c    o                       totale        4     +1
