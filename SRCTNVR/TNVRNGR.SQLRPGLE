000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300050811     h* Il pgm cancella record note giacenze scritti x errore
000400020409     h*****************************************************************
000500050811     ftignp00f  uf   e             disk
001000050811     D tignp           ds
001100020821     d rrn                            8  0
001700020702     c*
001800050811     c* leggo tutti i record di tignp con nota = 'D.T.' scritti anche se
001900050811     c* le dosposizioni non sono x decorsi termini
002000020702     c*
002100020702     C/EXEC SQL
002200050811     C+ DECLARE A1 CURSOR FOR SELECT rrn(tignp00f) FROM tignp00f join
002300050811     C+ tigcp00f on GNPAGC = GcPAGC and GNPFGC = GcPFGC and GNPNGC =
002301050811     C+ GcPNGC WHERE GNPDMC ='D.T.' and gcpdis <>'R' and gcpfrg = 0
002400020702     C/END-EXEC
002500020702
002600020702     C/EXEC SQL
002700020702     C+ OPEN A1
002800020702     C/END-EXEC
002900020702
003000020702     C                   dou       *inlr
003100020702
003200020702     C/EXEC SQL
003300050811     C+ FETCH NEXT FROM A1 INTO :tignp
003400020702     C/END-EXEC
003500020702
003600020702     C                   select
003700020821     C                   when      sqlcod <> 0
003800020702     C                   seton                                        lr
003900020821     C                   other
005600050811     c* cancello il record
005700050811     C     rrn           chain     tignp00f
005800050811     c                   if        %found(tignp00f)
006000050811     c                   delete    tignp000
006100020821     c                   end
006200020702     C                   endsl
006300020702     C*
006400020702     C                   enddo
006500020702
006600020702     C/EXEC SQL
006700020702     C+ CLOSE A1
006800020702     C/END-EXEC
