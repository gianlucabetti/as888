000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300030212     h* Il pgm pulisce tutti gli archivi dei conteggi cooperative
000400020409     h*****************************************************************
000900000607     D kpjba         e ds
000901160119     d param           ds
001100160119     d  pardt                         8  0
001101160119     d  tip                           1
001102160119      *
001200040217     d pardc           s              8  0
001300040217     d dtaiso          s               d
001500020702     c*
001600160119     c* pulisco i record del tnsre00f
001700040217     c* prestazione <= data fattura richiesta a video - 1 mese
001800040217     c*
001900040217     C/EXEC SQL
002000160119     C+ DELETE FROM tnsre00f WHERE sreddc <= :pardc and sretip = :tip
002100040217     C/END-EXEC
007300030213     C                   seton                                        lr
007400000607      *-----------------------------------------------
007500000607     c     *inzsr        begsr
007600000607      *-----------------------------------------------
007700000607     c
007800000607     c     *entry        plist
007900020205     c                   parm                    kpjba
008000160119     c                   movel     kpjbu         param
008100040217     c* tolgo 1 mesi dalla data fattura richiesta a video
008200040217     c                   movel     pardt         dtaiso
008300040217     c                   subdur    1:*m          dtaiso
008400040217     c                   movel     dtaiso        pardc
010100020402     c*
010200020402     c                   endsr
