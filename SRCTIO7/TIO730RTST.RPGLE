000100030625     H DFTACTGRP(*NO) BNDDIR('TIO7')
000200030626
000300030626     FQSysPrt   O    F  132        PRINTER
000400030625
000500160210     D GetNPRorm       PR            10
000600160210     D  ParametroU                   10
000700030625
000800160210     D WrkNPR          S             10
000900030625
001000030626     C                   DO        17576
001100160210     C                   EVAL      WrkNPR = GetNPRorm(WrkNPR)
001200030626     C                   EXCEPT
001300030626     C                   ENDDO
001400030625
001500030625     C                   EVAL      *INLR = *ON
001600030626
001700030626     OQSysPrt   E                        1
001800160210     O                       WrkNPR
