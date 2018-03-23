000100030625     H DFTACTGRP(*NO) BNDDIR('TIO7')
000200030626
000300030626     FQSysPrt   O    F  132        PRINTER
000400030625
000500030625     D GetRifORM       PR            15
000600030625     D  ParametroU                   15
000700030625
000800030626     D WrkRfa          S             15
000900030625
001000030626     C                   DO        17576
001100030626     C                   EVAL      WrkRfa = GetRifORM(WrkRfa)
001200030626     C                   EXCEPT
001300030626     C                   ENDDO
001400030625
001500030625     C                   EVAL      *INLR = *ON
001600030626
001700030626     OQSysPrt   E                        1
001800030626     O                       WrkRfa
