000100111220     D FNEVB00F      E DS
000200111221     D pInAnno         S              4s 0
000300111221     D pInLineaPar     S              3s 0
000400111221     D pInSerie        S              2s 0
000500111221     D pInNumSped      S              7s 0
000600111221     D pInCausale      S              3a
000700111221     D pInDtEvento     S              8s 0
000800111220     D* tipo chiamata:
000900111220     D*  0=prima chiamata
001000111220     D*  1=successive
001100111220     D pInTpChiamata   S              1s 0
001200111221     D pOutFNEVB_DS    DS                  occurs(50)
001300111221     D  pOutCausale                   3a
001400111221     D  pOutDtEvento                  8s 0
001500111221     D  pOutOraEvento                 4s 0
001600111221     D  pOutDtImmEv                   8s 0
001700111221     D  pOutOraImmEv                  6s 0
001800111220     D* esito reperimento:
001900111220     D*  0=non reperito
002000111220     D*  1=reperito
002100111220     D pOutEsito       S              1A
002200111220     D pOutNrOccurs    S              3S 0
002300111220     D pOutEnd         S              1A
002400111220     C                   EVAL      pInTpChiamata = 0
002500111220     C                   dou       pOutEnd = '1'
002600111220     C                   CALL      'UBRTVEVB'
002700111220     C                   PARM      2010          pInAnno
002800111220     C                   PARM      127           pInLineaPar
002900111220     C                   PARM      0             pInSerie
003000111220     C                   PARM      189925        pInNumSped
003100111220     C                   PARM      'ZTL'         pInCausale
003200111220     C                   PARM      20101103      pInDtEvento
003300111220     C                   PARM                    pInTpChiamata
003400111220     C                   PARM                    pOutFNEVB_DS
003500111220     C                   PARM                    pOutEsito
003600111220     C                   PARM                    pOutNrOccurs
003700111220     C                   PARM                    pOutEnd
003800111220     C                   EVAL      pInTpChiamata = 1
003900111220     c                   enddo
004000111221     c* per testare cosa arriva nelle occorrenze successive
004100111221     c*    2             occur     pOutFNEVB_DS
004200111220     c                   seton                                        lr
