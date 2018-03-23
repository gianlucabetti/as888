000100030616     D TIO1G2IDS     E DS
000200030616     D                                     PREFIX(Inp)
000300030616     D TIO1G2PDS     E DS
000400030616     D                                     PREFIX(Pag)
000500030616     D Operazione      S              1
000600030616     D RigheRic        S              5S 0
000700030618     D RigheOut        S              5S 0
000800030616     D Esito           S              1
000900030618     D SistemaInf      S             10
001000030618     D                                     INZ('GAITRA201')
001100030616
001200030616     ***********************************************************************
001300030616     **
001400030616     ** Main
001500030616     **
001600030616     ***********************************************************************
001700030618     C     *ENTRY        PLIST
001800030618     C                   PARM                    TIO1G2IDS
001900030618     C                   PARM                    Operazione
002000030618     C                   PARM                    RigheRic
002100030618     C                   PARM                    TIO1G2PDS
002200030618     C                   PARM                    RigheOut
002300030618     C                   PARM                    Esito
002400030617
002500041220     C*                  IF        Operazione = '1'
002600041220     C*                  CALL      'XCHGLIBL'
002700041220     C*                  PARM                    SistemaInf
002800041220     C*                  PARM                    Esito
002900041220     C*                  ENDIF
003000030617
003100030618     C                   CALL      'TIO7G21R'
003200030617     C                   PARM                    TIO1G2IDS
003300030617     C                   PARM                    Operazione
003400030618     C                   PARM                    RigheRic
003500030617     C                   PARM                    TIO1G2PDS
003600030618     C                   PARM                    RigheOut
003700030618     C                   PARM      *BLANK        Esito
003800030617
003900030618     C                   RETURN
