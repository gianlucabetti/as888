000100101111     ffidsf00f  uf   e             disk
000200101111     d rrn             s              9  0
000300101111     c                   seton                                        lr
000400101111     c* pulisco se non trovo l'archivio distinte
000500050221     C/EXEC SQL
000600101111     C+ DECLARE A CURSOR FOR SELECT rrn(fidsf00f) FROM fidsf00f exception
000700101111     C+ join fidst00f on dstfgs = dsffgs and dstnfv = dsfnfv and dsfnpg =
000800101111     C+ dstnpg WHERE FIDSF00F.DSFDTORIN < 20090000000000
000900101111     C/END-EXEC
001000050221     C/EXEC SQL
001100101111     C+ OPEN a
001200050221     C/END-EXEC
001300050221
001400050221     C                   DO        *HIVAL
001500050221
001600050221     C/EXEC SQL
001700101111     C+ FETCH NEXT FROM a INTO :rrn
001800050221     C/END-EXEC
001900050221
002000050221     C                   SELECT
002100050221     C
002200050221     C                   WHEN      SQLCod = 100
002300050222     C                   LEAVE
002400050221     C
002500050221     C                   WHEN      SQLCod < 0
002600050222     C                   LEAVE
002700050221     C
002800050221     C                   OTHER
002900101111     C     rrn           chain     fidsf00f
003000101111     c                   if        %found(fidsf00f)
003100101111     c                   delete    fidsf000
003200080104     c                   end
003300050221     C                   ENDSL
003400050221
003500050221     C                   ENDDO
003600050224
003700050221
003800050221     C/EXEC SQL
003900101111     C+ CLOSE a
004000050221     C/END-EXEC
