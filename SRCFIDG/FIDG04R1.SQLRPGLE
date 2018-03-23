000200180130     H DEBUG DECEDIT('0,') DATEDIT(*ymd)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     d kpjba         e ds
000900020624     D*
000901180130     d dataiso         s               d   datfmt(*iso)
000902180130     d dataiso2        s               d   datfmt(*iso)
000903180130     d tmp             s              8
000904180130     d tmpn            s              8  0
000905180130     d minima          s             17
000906180130     d codaut          s              7  0
000907180130     d Wdata8          DS
000908180130     d  dadata                 1      8  0
000909180130     d  adata                  9     16  0
000910180130     d  GioLav                17     21  0
000911180130      *
001000011026
001100000000     C*---------------------------------------------------------------*
001200020624     c     *entry        plist
001300020624     c                   parm                    kpjba
001500180130      *deduco la data di pulizia
001502180130     c                   move      *date         dataiso
001503180130     c     dataiso       subdur    2:*d          dataiso2
001504180130     c                   do        *hival
001505180130     c                   move      dataiso2      dadata
001506180130     c                   move      dataiso       adata
001507180130     c                   CALL      'XSRLAV8'
001508180130     c                   PARM                    Wdata8
001509180130      * se corrisponde ho trovato il D-Day
001510180130     c                   if        giolav = 3
001511180130     c                   leave
001512180130     c                   else
001513180130      * altrimenti continuo a sottrarre i giorni da udate
001514180130     c     dataiso2      subdur    1:*d          dataiso2
001515180130     c                   iter
001516180130     c                   endif
001517180130     c                   enddo
001518180130     c                   movel     dataiso2      tmpn
001519180130     c                   move      tmpn          tmp
001520180130     c*
001521180130     C/EXEC SQL
001522180130     C+ DECLARE A1 CURSOR FOR
001523180130     C+ SELECT pdccodaut, min(pdcdatora)
001524180130     C+ FROM fipdc00t WHERE pdcndc = 99999
001525180130     C+ GROUP BY pdccodaut
001526180130     C+ HAVING min(pdcdatora) < :tmp
001527180130     C/END-EXEC
001528180130      *
001529180130     C/EXEC SQL
001530180130     C+ OPEN A1
001531180130     C/END-EXEC
001532180130     C                   DOU       SqlCod <> 0
001533180130     C/EXEC SQL
001534180130     C+ FETCH NEXT FROM A1 INTO :codaut, :minima
001535180130     C/END-EXEC
001536180130     C                   SELECT
001537180130     c*
001538180130     C                   WHEN      SqlCod = 0
001539180130     c                   exsr      elimina
001540180130     c                   eval      sqlcod = 0
001555180130      *   EoF
001556180130     C                   WHEN      SqlCod = 100
001557180130     c                   leave
001558180130      **
001559180130      *   Errori
001560180130     C                   WHEN      SqlCod <> 0
001561180130     c                   leave
001562180130      **
001563180130     C                   ENDSL
001564180130      **
001565180130     C                   ENDDO
001566180130     C/EXEC SQL
001567180130     C+ CLOSE A1
001568180130     C/END-EXEC
001569180130     c                   seton                                        lr
001570180130     c*__________________________________________
001571180130     c     elimina       begsr
001572180130     c*_________________________________________
001600020313     C/EXEC SQL
001700180130     C+ delete from FIpdc00t where pdccodaut = :codaut and pdcndc = 99999
001800020313     C/END-EXEC
001900020515      *
001901180130     C/EXEC SQL
001902180130     C+ delete from FIpdd00t where pddcodaut = :codaut and pddndc = 99999
001903180130     C/END-EXEC
001904180130      *
001909180130     C/EXEC SQL
001910180130     C+ delete from FIpdo00t where pdocodaut = :codaut and pdondc = 99999
001911180130     C/END-EXEC
001912180130     c*
001913180130     c                   endsr
002900011129     C*--------------------------------------------------------------------
