000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
001300990601     d
001302151009     d param           ds
001305151009     d  pvocA                         8    dim(10)
001306151009     d  pvocD                         8    dim(10)
001307151009     d  pdata                          d
001308151009     d  pfil                          3
001309151013     d  ptsr                          1
001310151009     d  pimpo                        13s 3
001311151009      *
001312151009     d  filiale        s              8
001313151013     d  filialen       s              3  0
001314151009     d  voce           s              8
001315151009     d  importo        s             13  3
001316151009     d  importoD       s             13  3
001317151009     d  importoA       s             13  3
001318151013     d  importoF       s             13  3
001319151009     d  x              s              2  0
001320151013      * reperisce stime inserite in sede dalla contabilità
001800151008      * ____________________________________________________
001801151009     C     *ENTRY        PLIST                                                  *
001802151009     C                   PARM                    param
001803151009     c                   movel     pfil          filiale
001804151013     c                   move      pfil          filialen
001805151009     c                   z-add     0             importoA
001806151009     c                   z-add     0             importoD
001807151013     c                   z-add     0             importoF
001808151009      * voci avere
001809151009     c                   do        10            x
001810151009     c                   if        pVoca(x) = *blank
001811151009     c                   leave
001812151009     c                   endif
001813151009     c                   z-add     0             importo
001814151009     c                   movel     pvoca(x)      voce
001815151009     C/EXEC SQL
001816151009     C+ SELECT  sum(moaimporto) into :importo
001817151009     C+    FROM ndmoa00f WHERE moactb = 'CE' and moasocieta = '201'
001818151202     C+    and moavoce = :voce  and moadtcoan = :pdata and moaentit1 = :filiale
001819151009     C+    and moaavere = '1'
001820151009     C/END-EXEC
001821151009      *
001822151009     c                   if           SQLCOD = 0
001823151009     c                   add       importo       importoA
001824151009     c                   end
001825151009     c                   enddo
001826151009      *
001827151009      * voci Dare
001828151009     c                   do        10            x
001829151009     c                   if        pVocd(x) = *blank
001830151009     c                   leave
001831151009     c                   endif
001832151009     c                   z-add     0             importo
001833151009     c                   movel     pvocd(x)      voce
001834151009     C/EXEC SQL
001835151009     C+ SELECT  sum(moaimporto) into :importo
001836151009     C+    FROM ndmoa00f WHERE moactb = 'CE' and moasocieta = '201'
001837151202     C+    and moavoce = :voce  and moadtcoan = :pdata and moaentit1 = :filiale
001838151009     C+    and moadare = '1'
001839151009     C/END-EXEC
001840151009      *
001841151009     c                   if           SQLCOD = 0
001842151009     c                   add       importo       importoD
001843151009     c                   end
001844151009     c                   enddo
001845151013      * reperisce stime inserite in filiale per tipo servizio data e filiale da sommare alla sede
001846151013     c                   z-add     0             importo
001847151013     C/EXEC SQL
001848151013     C+ SELECT  sum(rnaimp) into :importo
001849151013     C+    FROM fnrna00f WHERE rnaann = ' ' and rnatsr = :ptsr
001850151013     C+    and rnadtac = :pdata and rnafil = :filialen
001851151013     C/END-EXEC
001852151013      *
001853151013     c                   if           SQLCOD = 0
001854151013     c                   add       importo       importoF
001855151013     c                   end
001856151013      * importo totale con stime addebiti filiale e sede
001857151013     c                   eval      pimpo = importoF + (importoD - importoA)
003000990504     c                   seton                                        lr
