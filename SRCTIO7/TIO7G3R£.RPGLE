000100060626     H DFTACTGRP(*NO)
000200060626
000300060626     D TIO1G3IDS     E DS
000400030620     D                                     PREFIX(Inp)
000500030620     D TIO1G3ODS     E DS
000600030620     D                                     PREFIX(Out)
000700030620     D Esito           S              1
000800030620
000900030620     ***********************************************************************
001000030620     **
001100030620     ** Main
001200030620     **
001300030620     ***********************************************************************
001400030620     C     *ENTRY        PLIST
001500030709     C                   PARM                    Esito
001600030620     C                   PARM                    TIO1G3IDS
001700030620     C                   PARM                    TIO1G3ODS
001800030620
001900041223     C                   CALL(E)   'TIO7G31R'
002000030714     C                   PARM      *BLANK        Esito
002100030620     C                   PARM                    TIO1G3IDS
002200030620     C                   PARM                    TIO1G3ODS
002300041223
002400041223     C                   IF        %ERROR
002500041223     C                   EVAL      Esito = *ON
002600041223     C                   ENDIF
002700030620
002800041223     C                   RETURN
