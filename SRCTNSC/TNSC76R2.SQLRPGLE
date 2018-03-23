000001120628     C     *ENTRY        PLIST
000002120628     C                   PARM                    innra            10
000003160118     C                   PARM                    outcdi            7 0
000004160118     C                   PARM                    outndp            7 0
000005160118     C                   PARM                    outddp            8 0
049401120628     c* A fronte di un multiassegno verifico se ho codici clienti diversi
049402160118     c* e nel caso restituisco il più piccolo
049403160118     c                   clear                   outnrcli          2 0
049404160118     c                   clear                   outcdi
049405160118     c                   clear                   outndp
049406160118     c                   clear                   outddp
049407120628     C/EXEC SQL
049408120628     C+ SELECT count(*) into :outnrcli
049409120628     C+ from ( select csbcdi from tncsb00f where
049410120628     C+ CSBnra = :innra group by csbcdi) as pippo
049411120628     C/END-EXEC
049412160118     c                   if        outnrcli > 1
049413160118     C/EXEC SQL
049414160118     C+ SELECT csbcdi, csbndp, csbddp INTO :outcdi, :outndp, :outddp FROM
049415160118     C+ tncsb00f WHERE csbnra= :innra and csbcdi = ( select min(csbcdi)
049416160118     C+ from tncsb00f where CSBNra = :innra and csbndp <> 0)
049420160118     C/END-EXEC
049421160118     c                   end
049500120628     c                   seton                                        lr
