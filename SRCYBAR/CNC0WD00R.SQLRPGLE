000100040423     H*------------------------------------------------------------------------*
000200040423     H DECEDIT('0,') DATEDIT(*DMY.)
000300040423      *---------------------------------------------------------------------*
000301061128     fazute02l  if   e           k disk
000302061128     fazorg01l  if   e           k disk
000303061128     ftncas00f  o    e             disk
000500040423      *---------------------------------------------------------------------*
000900040423     D*-------------------
001000040423     D* DS PARAMETRI DI LANCIO PGM
001100040423     D*-------------------
001200040423     D KPJBA         E DS
001201061120     D CNC0WD00S     e DS
001300040423     D*-------------------
002100000509     C     *ENTRY        PLIST
002200040423     C                   parm                    kpjba
002300061120     C                   eval      CNC0WD00S = kpjbu
002301061128     c     kute          klist
002302061128     c                   kfld                    fle
002303061128     c                   kfld                    cascuc
002500000510     C/EXEC SQL
002600061120     C+ DECLARE A1 CURSOR FOR SELECT CASFLE, CASCUC, CASDA8, CASORA,
002601120515     C+ CASVIE, sum( CASVALORE *CASQTA ) FROM yfcas00f WHERE casfle =
002602120515     C+ :fle GROUP BY CASFLE, CASCUC, CASDA8, CASORA, CASVIE ORDER BY
002603120515     C+ CASFLE, CASCUC, CASDA8, CASORA
003300000510     C/END-EXEC
003400040423     **
003500000510     C/EXEC SQL
003600000510     C+ OPEN A1
003700000510     C/END-EXEC
004300040423     **
004400061120     c                   do        *hival
005100040423     **
005200040423     C/EXEC SQL
005300040423     C+ FETCH NEXT FROM A1 INTO
005301061120     C+        :casfle, :cascuc, :casda8, :casora, :casvie, :casicon
005500040423     C/END-EXEC
005501061120     ** x errore o fine file
005502061120     c                   IF        sqlcod <> 0
005503061120     c                   leave
005504061120     c                   endIF
005505061128     ** distretto e area
005506061128     c     fle           chain     azorg01l
005507061128     c                   if        %found(azorg01l)
005508061128     c                   eval      casdit = orgfl3
005509061128     c                   eval      cascar = orgcar
005510061128     c                   else
005511061128     c                   clear                   casdit
005512061128     c                   clear                   cascar
005513061128     c                   end
005514061128     c* utente
005515061128     c     kute          chain     azute02l
005516061128     c                   if        %found(azute02l)
005517061128     c                   eval      caspru = uteute
005518061128     c                   else
005520061128     c                   clear                   caspru
005521061128     c                   end
005522061128     c*
005523061120     c                   write     tncas000
005900061128     c*
006000040423     C                   enddo
006100000510     C/EXEC SQL
006200000510     C+ CLOSE A1
006300000510     C/END-EXEC
006400011123      *------------------------------------------------------------------
006500011123     C                   Seton                                        LR
