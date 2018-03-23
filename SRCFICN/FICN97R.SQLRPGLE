000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300031202     h* Il pgm pulisce l'archivio delle autofatture extra
000400020409     h*****************************************************************
000500020826     ftntbe01l  uf   e           k disk
000600000607     D kpjba         e ds
000700020702     d pardt           s              8  0
000800020702     c*
000900031124     c* cancello tutti i record che hanno data fattura = video
001000031124     C/EXEC SQL
001100040217     C+ DELETE FROM FIOTT00F WHERE ottdft between 1 and :pardt
001200031124     C/END-EXEC
001300070607     c* cancello storico anagrafiche che hanno data fattura = video
001400031124      *
001500030213     C                   seton                                        lr
001600000607      *-----------------------------------------------
001700000607     c     *inzsr        begsr
001800000607      *-----------------------------------------------
001900000607     c
002000000607     c     *entry        plist
002100020205     c                   parm                    kpjba
002200020702     c                   movel     kpjbu         pardt
002300020402     c*
002400020402     c                   endsr
