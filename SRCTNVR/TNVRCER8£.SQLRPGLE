000005100324     fecevt00f  uf   e             disk
000006100324     fecevd00f  uf   e             disk
000007100519     Dnrr              s              9  0
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
