000100111221     D pInAnno         S              4s 0
000200111221     D pInLineaPar     S              3s 0
000300111221     D pInSerie        S              2s 0
000400111221     D pInNumSped      S              7s 0
000500111222     D pInTpChiamata   S              2s 0
000600111222     D pOutFNDCT_DS  E DS                  extname(FNDCT00F)
000700111222     D pOutEsito       S              1A
000800111220     C                   EVAL      pInTpChiamata = 0
000900111222     C                   CALL      'UBRTVDCT'
001000111220     C                   PARM      2010          pInAnno
001100111222     C                   PARM      132           pInLineaPar
001200111220     C                   PARM      0             pInSerie
001300111222     C                   PARM      111166        pInNumSped
001400111220     C                   PARM                    pInTpChiamata
001500111222     C                   PARM                    pOutFNDCT_DS
001600111222     C                   PARM                    pOutEsito
001700111220     c                   seton                                        lr
