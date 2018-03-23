000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300000920     H DECEDIT('0,') DATEDIT(*DMY.)
000400970526     F*------------------------------------------------------------------------*
000500970526     F* DATA BASE
000600970526     F*------------------------------------------------------------------------*
000700050408     Fsysprt    o    f  132        printer
000800110420     Fecevt01l  if   e           k disk
000900111011     Fecc2v30c  if   e           k disk
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
004300111011     c     voc           chain     ecevt01l
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
013400111011     c     ktas          chain     ecc2v30c
013500111011     c                   if           not %found(ecc2v30c)
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
017300111011     c     ktas          chain     ecc2v30c
017400111011     c                   if           not %found(ecc2v30c)
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
018900111011     c                   if        c2vc51 = evtvoc
019000111011     c                   add       c2vi51        totale
019100050408     c                   end
019200111011     c                   if        c2vc52 = evtvoc
019300111011     c                   add       c2vi52        totale
019400050408     c                   end
019500111011     c                   if        c2vc53 = evtvoc
019600111011     c                   add       c2vi53        totale
019700050408     c                   end
019800111011     c                   if        c2vc54 = evtvoc
019900111011     c                   add       c2vi54        totale
020000050408     c                   end
020100111011     c                   if        c2vc55 = evtvoc
020200111011     c                   add       c2vi55        totale
020300050408     c                   end
020400111011     c                   if        c2vc56 = evtvoc
020500111011     c                   add       c2vi56        totale
020600050408     c                   end
020700111011     c                   if        c2vc57 = evtvoc
020800111011     c                   add       c2vi57        totale
020900050408     c                   end
021000111011     c                   if        c2vc58 = evtvoc
021100111011     c                   add       c2vi58        totale
021200050408     c                   end
021300111011     c                   if        c2vc59 = evtvoc
021400111011     c                   add       c2vi59        totale
021500050408     c                   end
021600111011     c                   if        c2vc60 = evtvoc
021700111011     c                   add       c2vi60        totale
021800050408     c                   end
021900111011     c                   if        c2vc61 = evtvoc
022000111011     c                   add       c2vi61        totale
022100050408     c                   end
022200111011     c                   if        c2vc62 = evtvoc
022300111011     c                   add       c2vi62        totale
022400050408     c                   end
022500111011     c                   if        c2vc63 = evtvoc
022600111011     c                   add       c2vi63        totale
022700050408     c                   end
022800111011     c                   if        c2vc64 = evtvoc
022900111011     c                   add       c2vi64        totale
023000050408     c                   end
023100111011     c                   if        c2vc65 = evtvoc
023200111011     c                   add       c2vi65        totale
023300050408     c                   end
023400111011     c                   if        c2vc66 = evtvoc
023500111011     c                   add       c2vi66        totale
023600050408     c                   end
023700111011     c                   if        c2vc67 = evtvoc
023800111011     c                   add       c2vi67        totale
023900050408     c                   end
024000111011     c                   if        c2vc68 = evtvoc
024100111011     c                   add       c2vi68        totale
024200050408     c                   end
024300111011     c                   if        c2vc69 = evtvoc
024400111011     c                   add       c2vi69        totale
024500050408     c                   end
024600111011     c                   if        c2vc70 = evtvoc
024700111011     c                   add       c2vi70        totale
024800050408     c                   end
024900111011     c                   if        c2vc71 = evtvoc
025000111011     c                   add       c2vi71        totale
025100050408     c                   end
025200111011     c                   if        c2vc72 = evtvoc
025300111011     c                   add       c2vi72        totale
025400050408     c                   end
025500111011     c                   if        c2vc73 = evtvoc
025600111011     c                   add       c2vi73        totale
025700050408     c                   end
025800111011     c                   if        c2vc74 = evtvoc
025900111011     c                   add       c2vi74        totale
026000050408     c                   end
026100111011     c                   if        c2vc75 = evtvoc
026200111011     c                   add       c2vi75        totale
026300050408     c                   end
026400050408     C                   ENDSR
026500050408     osysprt    e            riga        1
026600111011c    o                                           +0 'ECC2V00F: Totale voce '
026700100519c    o                       evtvoc              +1
026800100519c    o                       totale        4     +1
