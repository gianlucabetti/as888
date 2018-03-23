000200070808     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000300000000     H*--------------------------------------------------------------*
000400070808      * Il programma viene schedulato ogni inizio mese partendo dalla
000500070808      * udate toglie un mese per ricavare la data di confronto
000600171106      * elimina il log variazioni a tutte le giacenze in fase 60
000900070808      *
000901171106     Ftigfp01L  UF   E           K DISK
000902171106     Ftigfd01L  UF   E           K DISK
001000020624     d kpjba         e ds
001100070808     d dataiso         s               d   datfmt(*iso)
001101171106     d wagc            s              4  0
001102171106     d wfgc            s              3  0
001103171106     d wngc            s              7  0
001200020624     D*
001300070808     d
001400011026
001500000000     C*---------------------------------------------------------------*
001600020624     c     *entry        plist
001700020624     c                   parm                    kpjba
001701171106     c     kgia          klist
001702171106     c                   kfld                    wagc
001703171106     c                   kfld                    wfgc
001704171106     c                   kfld                    wngc
001705171106      *
001800070808     c                   move      *date         dataiso
001900070808     c                   move      dataiso       datau             8 0
002000070808     c     dataiso       subdur    1:*m          dataiso
002100070808     c                   move      dataiso       dataup            8 0
002101171106     C/EXEC SQL
002102171106     C+ DECLARE A1 CURSOR FOR
002103171106     C+  SELECT GcPAGC, GcPFGC, GcPNGC
002104171106     C+   FROM TIGcP00F
002105171106     C+    WHERE  GcPFAS  = 60     AND
002106171106     C+           GcPdcg <= :dataup
002108171106     c+   ORDER BY  GcPAGC, GcPFGC, GcPNGC
002109171106     C/END-EXEC
002110171106     **
002111171106     C/EXEC SQL
002112171106     C+ OPEN A1
002113171106     C/END-EXEC
002114171106     **
002115171106     c                   dow       sqlcod <> 100
002116171106     C/EXEC SQL
002117171106     C+ FETCH NEXT FROM A1 INTO
002118171106     C+        :wAGC, :wFGC, :wNGC
002119171106     C/END-EXEC
002120171106     **
002121171106     ** x errore
002122171106     c                   IF        sqlcod <  0
002123171106     c                   leave
002124171106     c                   endIF
002125171107     c                   IF        sqlcod =  0
002126171107     c                   exsr      pulisci
002128171107     c                   endIF
002132171106     c                   enddo
002133171107     C/EXEC SQL
002134171107     C+ CLOSE A1
002135171107     C/END-EXEC
002136171106     c                   seton                                        lr
002137171106      *_____________________________________________________
002138171106     c     pulisci       begsr
002139171107      *_____________________________________________________
002140171106     c     kgia          setll     tigfp01l
002141171106     c                   do        *hival
002142171106     c     kgia          reade     tigfp01l
002143171106     c                   if        %eof(tigfp01l)
002144171106     c                   leave
002145171106     c                   endif
002146171106     c                   delete    tigfp000
002147171106     c                   enddo
002605171106      *
002606171106     c     kgia          setll     tigfd01l
002607171106     c                   do        *hival
002608171106     c     kgia          reade     tigfd01l
002609171106     c                   if        %eof(tigfd01l)
002612171106     c                   leave
002613171106     c                   endif
002614171106     c                   delete    tigfd000
002615171106     c                   enddo
002616171106      *
002701171106     c                   endsr
002900011129     C*--------------------------------------------------------------------
