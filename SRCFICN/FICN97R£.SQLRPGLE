000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300031202     h* Il pgm pulisce l'archivio delle autofatture extra
000400020409     h*****************************************************************
000600020826     ftntbe01l  uf   e           k disk
000700000607     D kpjba         e ds
000900020702     d pardt           s              8  0
001100020702     c*
001200031124     c* cancello tutti i record che hanno data fattura = video
001301031124     C/EXEC SQL
001302040217     C+ DELETE FROM FIOTT00F WHERE ottdft between 1 and :pardt
001303031124     C/END-EXEC
001304070607     c* cancello storico anagrafiche che hanno data fattura = video
001305070607     C/EXEC SQL
001306070607     C+ DELETE FROM TNANF00F WHERE anfdft between 1 and :pardt and anftsr
001307070607     C+ = 'O'
001308070607     C/END-EXEC
001309031124      *
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
