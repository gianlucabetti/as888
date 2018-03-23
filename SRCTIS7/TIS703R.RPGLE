000100990920      /TITLE Upload via Internet: invio a PO.
000200100322     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300990920
000400100322     Ftivlre01l if   e           k disk
000500100322
000600100322     D OutTivlreDs   e ds                  extname(tivlre00f)
000700100322     D InTRC           s                   like(VLRETRC)
000800100322     D InPRG           s                   like(VLREPRG)
000900100322     D esito           s              1
001000991228
001100100322
001200100322
001300100322     C*
001400100322     C                   clear                   OutTivlreDs
001500100322     C                   clear                   esito
001600100322     C*
001700100322     C                   eval      VLRETRC = InTRC
001800100322     C                   eval      VLREPRG = InPRG
001900100322     C     KEYvlre_01C   chain     tivlre01l
002000100322     C*
002100100322     C                   if        %found(tivlre01l)
002200100322     C                   eval      esito = '0'
002300100322     C                   else
002400100322     C                   eval      esito = '2'
002500100322     C                   endif
002600990916     C*
002700990916     C                   return
002800100322
002900100322
003000990916
003100990916      /TITLE Operazioni iniziali.
003200990916     C     *inzsr        begsr
003300990916     C*
003400990916     C     *entry        plist
003500100322     C                   parm                    InTRC
003600100322     C                   parm                    InPRG
003700100322     C                   parm                    OutTivlreDs
003800990916     C                   parm                    esito
003900100322     C*
004000100322     C     KEYvlre_01C   klist
004100100322     C                   kfld                    VLRETRC
004200100322     C                   kfld                    VLREPRG
004300990916     C*
004400990916     C                   endsr
