000001120628     C     *ENTRY        PLIST
000002120628     C                   PARM                    innra            10
000003120628     C                   PARM                    outnrcli          2 0
049401120628     c* A fronte di un multiassegno verifico se ho codici clienti diversi
049403120628     c                   clear                   outnrcli
049404120628     C/EXEC SQL
049405120628     C+ SELECT count(*) into :outnrcli
049406120628     C+ from ( select csbcdi from tncsb00f where
049407120628     C+ CSBnra = :innra group by csbcdi) as pippo
049408120628     C/END-EXEC
049500120628     c                   seton                                        lr
