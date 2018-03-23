000100151215      /TITLE Upload via Internet
000200100322     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300990920
000400100322
000500151216     D InFILORI        s            258A
000600151216     D InFIL           s            258A
000700151216     D OutFILORI       s            258A
000800151216     D OutFIL          s            258A
000900100322     D esito           s              1
001000991228
001100100322
001200100322
001300100322     C*
001400151215     C                   eval      esito = '2'
001500100322     C*
001600151216     C                   eval      OutFILORI = %scanrpl('''':'_':InFILORI)
001700151216     C                   eval      OutFIL    = %scanrpl('''':'_':InFIL)
001800100322     C*
001900100322     C                   eval      esito = '0'
002000990916     C*
002100990916     C                   return
002200100322
002300100322
002400990916
002500990916      /TITLE Operazioni iniziali.
002600990916     C     *inzsr        begsr
002700990916     C*
002800990916     C     *entry        plist
002900151215     C                   parm                    InFILORI
003000151215     C                   parm                    InFIL
003100151215     C                   parm                    OutFILORI
003200151215     C                   parm                    OutFIL
003300151215     C                   parm                    esito
003400990916     C*
003500990916     C                   endsr
