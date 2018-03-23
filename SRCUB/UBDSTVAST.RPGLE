000100121024     d stringa         s            100
000200121107     d FGS             s              3s 0
000300121107     d CCM             s              7s 0
000400121107     d CMR             s             35
000500121107     d CNT             s              5s 0
000600121107     d Flags           s              4
000700121108     d Lib             s             10
000800121108     d TpFile          s              5
000900121107     d pOut_Esito      s              2s 0
001000121107     d pOut_MotivoErr  s              5s 0
001100121108     C                   EVAL      FGS = 360
001200121108     C                   EVAL      CCM = 3600004
001300121108     C                   EVAL      CMR ='2485771329'
001400121107     C                   EVAL      CNT = 1
001500121107     c                   clear                   pOut_Esito
001600121107     c                   clear                   pOut_MotivoErr
001700121108     C                   CALL      'UBDSTVASR'
001800121107     C                   PARM                    FGS
001900121107     C                   PARM                    CCM
002000121107     C                   PARM                    CMR
002100121107     C                   PARM                    CNT
002200121108     c                   PARM      '  X '        Flags
002300121108     c                   PARM                    Lib
002400121108     c                   PARM      'EDIVA'       TpFile
002500121107     c                   parm                    pOut_Esito
002600121107     c                   parm                    pOut_MotivoErr
002700121024
002800111214     c                   seton                                        lr
