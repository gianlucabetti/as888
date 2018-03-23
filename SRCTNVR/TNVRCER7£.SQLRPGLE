000001100324     feccev00f  uf   e             disk
000002100324     feccevp0f  uf   e             disk    prefix(£)
000003100324     fecc2v00f  uf   e             disk
000004100324     fecc2vp0f  uf   e             disk    prefix(£)
000005100324     fecevt00f  uf   e             disk
000006100324     fecevd00f  uf   e             disk
001207050502     d eccevds       e ds                  extname(eccev00f)
001208100324     d eccevpds      e ds                  extname(eccevp0f) prefix(£)
001209100315     d ecc2vds       e ds                  extname(ecc2v00f)
001210100324     d ecc2vpds      e ds                  extname(ecc2vp0f) prefix(£)
001211100315     D Ptr             S               *
001212100315     D                                     INZ(%ADDR(cevc01))
001213100315     D voc             S                   LIKE(cevc01)
001214100315     D                                     DIM(50)
001215100315     D                                     BASED(Ptr)
001216100315     D Ptr2            S               *
001217100324     D                                     INZ(%ADDR(c2vc51))
001218100324     D voc2            S                   LIKE(c2vc51)
001219100315     D                                     DIM(25)
001220100315     D                                     BASED(Ptr2)
001221100324     D Ptr£            S               *
001222100324     D                                     INZ(%ADDR(£cevc01))
001223100324     D voc£            S                   LIKE(£cevc01)
001224100324     D                                     DIM(50)
001225100324     D                                     BASED(Ptr£)
001226100324     D Ptr2£           S               *
001227100324     D                                     INZ(%ADDR(£c2vc51))
001228100324     D voc2£           S                   LIKE(£c2vc51)
001229100324     D                                     DIM(25)
001230100324     D                                     BASED(Ptr2£)
001231100324     Dnrr              s              9  0
001232100324     Dx                s              3  0
001233100315     c*
001234100315     c* ECCEV00F
001235100315     c*
001236050502     C/EXEC SQL
001237100324     C+ DECLARE C1 CURSOR FOR SELECT rrn(eccev00f) FROM eccev00f
001238100419     C+ WHERE CEVC01 between 610 and 623 or CEVC02 between 610 and 623 or
001239100419     C+ CEVC03 between 610 and 623 or CEVC04 between 610 and 623 or
001240100419     C+ CEVC05 between 610 and 623 or CEVC06 between 610 and 623 or
001241100419     C+ CEVC07 between 610 and 623 or CEVC08 between 610 and 623 or
001242100419     C+ CEVC09 between 610 and 623 or CEVC10 between 610 and 623 or
001243100419     C+ CEVC11 between 610 and 623 or CEVC12 between 610 and 623 or
001244100419     C+ CEVC13 between 610 and 623 or CEVC14 between 610 and 623 or
001245100419     C+ CEVC15 between 610 and 623 or CEVC16 between 610 and 623 or
001246100419     C+ CEVC17 between 610 and 623 or CEVC18 between 610 and 623 or
001247100419     C+ CEVC19 between 610 and 623 or CEVC20 between 610 and 623 or
001248100419     C+ CEVC21 between 610 and 623 or CEVC22 between 610 and 623 or
001249100419     C+ CEVC23 between 610 and 623 or CEVC24 between 610 and 623 or
001250100419     C+ CEVC25 between 610 and 623 or CEVC26 between 610 and 623 or
001251100419     C+ CEVC27 between 610 and 623 or CEVC28 between 610 and 623 or
001252100419     C+ CEVC29 between 610 and 623 or CEVC30 between 610 and 623 or
001253100419     C+ CEVC31 between 610 and 623 or CEVC32 between 610 and 623 or
001254100419     C+ CEVC33 between 610 and 623 or CEVC34 between 610 and 623 or
001255100419     C+ CEVC35 between 610 and 623 or CEVC36 between 610 and 623 or
001256100419     C+ CEVC37 between 610 and 623 or CEVC38 between 610 and 623 or
001257100419     C+ CEVC39 between 610 and 623 or CEVC40 between 610 and 623 or
001258100419     C+ CEVC41 between 610 and 623 or CEVC42 between 610 and 623 or
001259100419     C+ CEVC43 between 610 and 623 or CEVC44 between 610 and 623 or
001260100419     C+ CEVC45 between 610 and 623 or CEVC46 between 610 and 623 or
001261100419     C+ CEVC47 between 610 and 623 or CEVC48 between 610 and 623 or
001262100419     C+ CEVC49 between 610 and 623 or CEVC50 between 610 and 623
001263050502     C/END-EXEC
001264050502     c*
001265050502     C/EXEC SQL
001266050502     C+ OPEN c1
001267050502     C/END-EXEC
001268050502
001269050502     C                   DO        *HIVAL
001270050502
001271050502     C/EXEC SQL
001272100315     C+ FETCH NEXT FROM c1 INTO :nrr
001273050502     C/END-EXEC
001274050502
001275100315     C                   IF        SQLCod = 100 or sqlcod < 0
001276050502     C                   LEAVE
001277050502     C                   ENDIF
001278100315     c*
001279100315     c     nrr           chain     eccev00f
001280100315     c                   do        50            x
001281100419     c                   if        voc(x) >= 610 and voc(x) <= 623
001282100315     c                   add       200           voc(x)
001283100315     c                   end
001284100315     c                   enddo
001285100315     c                   update    eccev000
001286050502     C                   ENDDO
001287050502
001288050502     C/EXEC SQL
001289100315     C+ CLOSE a1
001290050502     C/END-EXEC
001291100315     c*
001292100315     c* ECCEVP0F
001293100315     c*
001300100315     C/EXEC SQL
001400100324     C+ DECLARE a1 CURSOR FOR SELECT rrn(eccevp0f) FROM eccevp0f
001500100419     C+ WHERE CEVC01 between 610 and 623 or CEVC02 between 610 and 623 or
001600100419     C+ CEVC03 between 610 and 623 or CEVC04 between 610 and 623 or
001700100419     C+ CEVC05 between 610 and 623 or CEVC06 between 610 and 623 or
001800100419     C+ CEVC07 between 610 and 623 or CEVC08 between 610 and 623 or
001900100419     C+ CEVC09 between 610 and 623 or CEVC10 between 610 and 623 or
002000100419     C+ CEVC11 between 610 and 623 or CEVC12 between 610 and 623 or
002100100419     C+ CEVC13 between 610 and 623 or CEVC14 between 610 and 623 or
002200100419     C+ CEVC15 between 610 and 623 or CEVC16 between 610 and 623 or
002300100419     C+ CEVC17 between 610 and 623 or CEVC18 between 610 and 623 or
002400100419     C+ CEVC19 between 610 and 623 or CEVC20 between 610 and 623 or
002500100419     C+ CEVC21 between 610 and 623 or CEVC22 between 610 and 623 or
002600100419     C+ CEVC23 between 610 and 623 or CEVC24 between 610 and 623 or
002700100419     C+ CEVC25 between 610 and 623 or CEVC26 between 610 and 623 or
002800100419     C+ CEVC27 between 610 and 623 or CEVC28 between 610 and 623 or
002900100419     C+ CEVC29 between 610 and 623 or CEVC30 between 610 and 623 or
003000100419     C+ CEVC31 between 610 and 623 or CEVC32 between 610 and 623 or
003100100419     C+ CEVC33 between 610 and 623 or CEVC34 between 610 and 623 or
003200100419     C+ CEVC35 between 610 and 623 or CEVC36 between 610 and 623 or
003300100419     C+ CEVC37 between 610 and 623 or CEVC38 between 610 and 623 or
003400100419     C+ CEVC39 between 610 and 623 or CEVC40 between 610 and 623 or
003500100419     C+ CEVC41 between 610 and 623 or CEVC42 between 610 and 623 or
003600100419     C+ CEVC43 between 610 and 623 or CEVC44 between 610 and 623 or
003700100419     C+ CEVC45 between 610 and 623 or CEVC46 between 610 and 623 or
003800100419     C+ CEVC47 between 610 and 623 or CEVC48 between 610 and 623 or
003900100419     C+ CEVC49 between 610 and 623 or CEVC50 between 610 and 623
004000100315     C/END-EXEC
004100100315     c*
004200100315     C/EXEC SQL
004300100315     C+ OPEN a1
004400100315     C/END-EXEC
004500100315
004600100315     C                   DO        *HIVAL
004700100315
004800100315     C/EXEC SQL
004900100315     C+ FETCH NEXT FROM a1 INTO :nrr
005000100315     C/END-EXEC
005100100315
005200100315     C                   IF        SQLCod = 100 or sqlcod < 0
005300100315     C                   LEAVE
005400100315     C                   ENDIF
005500100315     c*
005600100315     c     nrr           chain     eccevp0f
005700100315     c                   do        50            x
005800100419     c                   if        voc£(x) >= 610 and voc£(x) <= 623
005900100324     c                   add       200           voc£(x)
006000100315     c                   end
006100100315     c                   enddo
006200100315     c                   update    eccevp00
006300100315     C                   ENDDO
006400100315
006500100315     C/EXEC SQL
006600100315     C+ CLOSE a1
006700100315     C/END-EXEC
006800100315     c*
006900100315     c* ECC2V00F
007000100315     c*
007100100315     C/EXEC SQL
007200100324     C+ DECLARE B1 CURSOR FOR SELECT rrn(ecc2v00f) FROM ecc2v00f
007300100419     C+ WHERE C2VC51 between 610 and 623 or C2VC52 between 610 and 623 or
007400100419     C+ C2VC53 between 610 and 623 or C2VC54 between 610 and 623 or
007500100419     C+ C2VC55 between 610 and 623 or C2VC56 between 610 and 623 or
007600100419     C+ C2VC57 between 610 and 623 or C2VC58 between 610 and 623 or
007700100419     C+ C2VC59 between 610 and 623 or C2VC60 between 610 and 623 or
007800100419     C+ C2VC61 between 610 and 623 or C2VC62 between 610 and 623 or
007900100419     C+ C2VC63 between 610 and 623 or C2VC64 between 610 and 623 or
008000100419     C+ C2VC65 between 610 and 623 or C2VC66 between 610 and 623 or
008100100419     C+ C2VC67 between 610 and 623 or C2VC68 between 610 and 623 or
008200100419     C+ C2VC69 between 610 and 623 or C2VC70 between 610 and 623 or
008300100419     C+ C2VC71 between 610 and 623 or C2VC72 between 610 and 623 or
008400100419     C+ C2VC73 between 610 and 623 or C2VC74 between 610 and 623 or
008500100419     C+ C2VC75 between 610 and 623
009800100315     C/END-EXEC
009900100315     c*
010000100315     C/EXEC SQL
010100100315     C+ OPEN b1
010200100315     C/END-EXEC
010300100315
010400100315     C                   DO        *HIVAL
010500100315
010600100315     C/EXEC SQL
010700100315     C+ FETCH NEXT FROM b1 INTO :nrr
010800100315     C/END-EXEC
010900100315
011000100315     C                   IF        SQLCod = 100 or sqlcod < 0
011100100315     C                   LEAVE
011200100315     C                   ENDIF
011300100315     c*
011400100315     c     nrr           chain     ecc2v00f
011500100315     c                   do        25            x
011600100419     c                   if        voc2(x) >= 610 and voc2(x) <= 623
011700100324     c                   add       200           voc2(x)
011800100315     c                   end
011900100315     c                   enddo
012000100315     c                   update    ecc2v000
012100100315     C                   ENDDO
012200100315
012300100315     C/EXEC SQL
012400100315     C+ CLOSE d1
012500100315     C/END-EXEC
012600100315     c*
012700100315     c* ECC2VP0F
012800100315     c*
012900100315     C/EXEC SQL
013000100324     C+ DECLARE d1 CURSOR FOR SELECT rrn(ecc2vp0f) FROM ecc2vp0f
013100100419     C+ WHERE C2VC51 between 610 and 623 or C2VC52 between 610 and 623 or
013200100419     C+ C2VC53 between 610 and 623 or C2VC54 between 610 and 623 or
013300100419     C+ C2VC55 between 610 and 623 or C2VC56 between 610 and 623 or
013400100419     C+ C2VC57 between 610 and 623 or C2VC58 between 610 and 623 or
013500100419     C+ C2VC59 between 610 and 623 or C2VC60 between 610 and 623 or
013600100419     C+ C2VC61 between 610 and 623 or C2VC62 between 610 and 623 or
013700100419     C+ C2VC63 between 610 and 623 or C2VC64 between 610 and 623 or
013800100419     C+ C2VC65 between 610 and 623 or C2VC66 between 610 and 623 or
013900100419     C+ C2VC67 between 610 and 623 or C2VC68 between 610 and 623 or
014000100419     C+ C2VC69 between 610 and 623 or C2VC70 between 610 and 623 or
014100100419     C+ C2VC71 between 610 and 623 or C2VC72 between 610 and 623 or
014200100419     C+ C2VC73 between 610 and 623 or C2VC74 between 610 and 623 or
014300100419     C+ C2VC75 between 610 and 623
014400100315     C/END-EXEC
014500100315     c*
014600100315     C/EXEC SQL
014700100315     C+ OPEN d1
014800100315     C/END-EXEC
014900100315
015000100315     C                   DO        *HIVAL
015100100315
015200100315     C/EXEC SQL
015300100315     C+ FETCH NEXT FROM d1 INTO :nrr
015400100315     C/END-EXEC
015500100315
015600100315     C                   IF        SQLCod = 100 or sqlcod < 0
015700100315     C                   LEAVE
015800100315     C                   ENDIF
015900100315     c*
016000100315     c     nrr           chain     ecc2vp0f
016100100315     c                   do        25            x
016200100419     c                   if        voc2£(x) >= 610 and voc2£(x) <= 623
016300100324     c                   add       200           voc2£(x)
016400100315     c                   end
016500100315     c                   enddo
016600100315     c                   update    ecc2vp00
016700100315     C                   ENDDO
016800100315
016900100315     C/EXEC SQL
017000100315     C+ CLOSE d1
017100100315     C/END-EXEC
017200100324     c*
017300100324     c* ECEVT00F
017400100324     c*
017500100324     C/EXEC SQL
017600100324     C+ DECLARE e1 CURSOR FOR SELECT rrn(ecevt00f) FROM ecevt00f
017700100419     C+ WHERE evtvoc between 610 and 623
019000100324     C/END-EXEC
019100100324     c*
019200100324     C/EXEC SQL
019300100324     C+ OPEN e1
019400100324     C/END-EXEC
019500100324
019600100324     C                   DO        *HIVAL
019700100324
019800100324     C/EXEC SQL
019900100324     C+ FETCH NEXT FROM e1 INTO :nrr
020000100324     C/END-EXEC
020100100324
020200100324     C                   IF        SQLCod = 100 or sqlcod < 0
020300100324     C                   LEAVE
020400100324     C                   ENDIF
020500100324     c*
020600100324     c     nrr           chain     ecevt00f
020900100324     c                   add       200           evtvoc
021200100324     c                   update    ecevt
021300100324     C                   ENDDO
021400100324
021500100324     C/EXEC SQL
021600100324     C+ CLOSE e1
021700100324     C/END-EXEC
021800100324     c*
021900100324     c* ECEVD00F
022000100324     c*
022100100324     C/EXEC SQL
022200100324     C+ DECLARE f1 CURSOR FOR SELECT rrn(ecevd00f) FROM ecevd00f
022300100419     C+ WHERE evdvoc between 610 and 623
022400100324     C/END-EXEC
022500100324     c*
022600100324     C/EXEC SQL
022700100324     C+ OPEN f1
022800100324     C/END-EXEC
022900100324
023000100324     C                   DO        *HIVAL
023100100324
023200100324     C/EXEC SQL
023300100324     C+ FETCH NEXT FROM f1 INTO :nrr
023400100324     C/END-EXEC
023500100324
023600100324     C                   IF        SQLCod = 100 or sqlcod < 0
023700100324     C                   LEAVE
023800100324     C                   ENDIF
023900100324     c*
024000100324     c     nrr           chain     ecevd00f
024100100324     c                   add       200           evdvoc
024200100324     c                   update    ecevd000
024300100324     C                   ENDDO
024400100324
024500100324     C/EXEC SQL
024600100324     C+ CLOSE f1
024700100324     C/END-EXEC
024800100324     c                   seton                                        lr
