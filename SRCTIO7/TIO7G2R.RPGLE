000100060626     H DFTACTGRP(*NO)
000200060626
000300060626     D TIO1G2IDS     E DS
000400030616     D                                     PREFIX(Inp)
000500030616     D TIO1G2PDS     E DS
000600030616     D                                     PREFIX(Pag)
000700030616     D Operazione      S              1
000800030616     D RigheRic        S              5S 0
000900030618     D RigheOut        S              5S 0
001000030616     D Esito           S              1
001100030618     D SistemaInf      S             10
001200030618     D                                     INZ('GAITRA201')
001300030616
001400030616     ***********************************************************************
001500030616     **
001600030616     ** Main
001700030616     **
001800030616     ***********************************************************************
001900030618     C     *ENTRY        PLIST
002000030618     C                   PARM                    TIO1G2IDS
002100030618     C                   PARM                    Operazione
002200030618     C                   PARM                    RigheRic
002300030618     C                   PARM                    TIO1G2PDS
002400030618     C                   PARM                    RigheOut
002500030618     C                   PARM                    Esito
002600030617
002700041220     C*                  IF        Operazione = '1'
002800041220     C*                  CALL      'XCHGLIBL'
002900041220     C*                  PARM                    SistemaInf
003000041220     C*                  PARM                    Esito
003100041220     C*                  ENDIF
003200030617
003300030618     C                   CALL      'TIO7G21R'
003400030617     C                   PARM                    TIO1G2IDS
003500030617     C                   PARM                    Operazione
003600030618     C                   PARM                    RigheRic
003700030617     C                   PARM                    TIO1G2PDS
003800030618     C                   PARM                    RigheOut
003900030618     C                   PARM      *BLANK        Esito
004000030617
004100030618     C                   RETURN
