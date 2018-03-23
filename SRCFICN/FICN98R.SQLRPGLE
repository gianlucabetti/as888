000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300040218     h* Il pgm pulisce l'archivio FIFRE00F x le stime e tutti i file saldi
000400020409     h*****************************************************************
000700000607     D kpjba         e ds
000900020702     d pardt           s              8  0
001100020702     c*
001200040218     c* cancello tutti i record che hanno data prestazione <= data richiesta
001201040218     c* a video
001202040218     c*
001301031124     C/EXEC SQL
001302040218     C+ DELETE FROM FIfre00F WHERE freddc <= :pardt and
001303040218     c+ fretip = 'G'
001304031124     C/END-EXEC
001305040218     c*
001306040218     C/EXEC SQL
001307040218     C+ DELETE FROM FIfsa00F WHERE fsaddc <= :pardt
001309040218     C/END-EXEC
001310040218     c*
001311040218     C/EXEC SQL
001312040218     C+ DELETE FROM FIfsc00F WHERE fscddc <= :pardt
001313040218     C/END-EXEC
001314040218     c*
001315040218     C/EXEC SQL
001316040218     C+ DELETE FROM FIfsr00F WHERE fsrddc <= :pardt
001317040218     C/END-EXEC
001318040218     c*
001319040218     C/EXEC SQL
001320040218     C+ DELETE FROM FIfss00F WHERE fssddc <= :pardt
001321040218     C/END-EXEC
001322040218     c*
004000030213     C                   seton                                        lr
004100000607      *-----------------------------------------------
004200000607     c     *inzsr        begsr
004300000607      *-----------------------------------------------
004400000607     c
004500000607     c     *entry        plist
004600020205     c                   parm                    kpjba
004700020702     c                   movel     kpjbu         pardt
005300020402     c*
005400020402     c                   endsr
