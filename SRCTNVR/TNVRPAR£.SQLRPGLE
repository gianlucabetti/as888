000100100729     H DEBUG DECEDIT('0,') DATEDIT(*ymd/)
000200100324     fecevd00f  uf   e             disk
000300100519     Dnrr              s              9  0
000400100324     c*
000500100324     C/EXEC SQL
000600100729     C+ DECLARE E1 CURSOR FOR SELECT rrn(ecevd00f) FROM
000700100729     C+ wrkquery/ypulecevd join ecevd00f on digits(evdcli) =
000800100729     C+ substr(PC1KSC, 2, 7)
000900100324     C/END-EXEC
001000100324     c*
001100100324     C/EXEC SQL
001200100324     C+ OPEN e1
001300100324     C/END-EXEC
001400100324
001500100324     C                   DO        *HIVAL
001600100324
001700100324     C/EXEC SQL
001800100324     C+ FETCH NEXT FROM e1 INTO :nrr
001900100324     C/END-EXEC
002000100324
002100100324     C                   IF        SQLCod = 100 or sqlcod < 0
002200100324     C                   LEAVE
002300100324     C                   ENDIF
002400100324     c*
002500100729     c     nrr           chain     ecevd00f
002600100729     c                   if        %found(ecevd00f)
002700100729     c                   eval      evdatb = 'A'
002800100729     c                   move      *date         evdduv
002900100729     c                   update    ecevd000
003000100729     c                   end
003100100324     C                   ENDDO
003200100324
003300100324     C/EXEC SQL
003400100324     C+ CLOSE e1
003500100324     C/END-EXEC
003600100324     c*
003700100324     c                   seton                                        lr
