000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300050907     h* toglie flag di sede da giacenze se esiste ancora bolla in filiale
000400020409     h*****************************************************************
000500050907     ftigcp00f  uf   e             disk
001000050907     D tigcp           ds
001100020821     d rrn                            8  0
001700020702     c*
001800050907     c* leggo tutti i record di tigcp con flag sede = 'S' ma presente
001900050908     c* ancora ARB
002000020702     c*
002100020702     C/EXEC SQL
002200050907     C+ DECLARE A1 CURSOR FOR SELECT rrn(tigcp00f) FROM tigcp00f join
002300050908     C+ fnarb00f on arbaas = GcPaasa and arblnp = gcplnpa and gcpnrsa =
002301050908     C+ arbnrs and gcpnspa = arbnsp WHERE gcpsede='S'
002400020702     C/END-EXEC
002500020702
002600020702     C/EXEC SQL
002700020702     C+ OPEN A1
002800020702     C/END-EXEC
002900020702
003000020702     C                   dou       *inlr
003100020702
003200020702     C/EXEC SQL
003300050907     C+ FETCH NEXT FROM A1 INTO :tigcp
003400020702     C/END-EXEC
003500020702
003600020702     C                   select
003700020821     C                   when      sqlcod <> 0
003800020702     C                   seton                                        lr
003900020821     C                   other
005600050811     c* cancello il record
005700050907     C     rrn           chain     tigcp00f
005800050907     c                   if        %found(tigcp00f)
005801050907     c                   clear                   gcpsede
006000050907     c                   update    tigcp000
006100020821     c                   end
006200020702     C                   endsl
006300020702     C*
006400020702     C                   enddo
006500020702
006600020702     C/EXEC SQL
006700020702     C+ CLOSE A1
006800020702     C/END-EXEC
