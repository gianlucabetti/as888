000100990514      *PARMS DFTACTGRP(*YES)
000200990514      *PARMS
000300990514      *PARMS
000400990514      *PARMS
000500990514      *PARMS
000600990525      /TITLE Cancellazione record di una libreria.
000700990525     Fxrdbr02l  uf   e           k disk    usropn
000800990525     Fxrpgm02l  uf   e           k disk    usropn
000900990525
001000990525     C     *entry        plist
001100990525     C                   parm                    libreria         10
001200990525     C                   parm                    dltdbr            1
001300990525     C                   parm                    dltpgm            1
001400990525
001500990525     C                   if        dltdbr = *on
001600990525     C                   open      xrdbr02l
001700990525     C     *in01         doueq     *on
001800990525     C     libreria      delete    qwhdrdbr                           01
001900990525     C                   enddo
002000990525     C                   close     xrdbr02l
002100990525     C                   endif
002200990525
002300990525     C                   if        dltpgm = *on
002400990525     C                   open      xrpgm02l
002500990525     C     *in01         doueq     *on
002600990525     C     libreria      delete    qwhdrppr                           01
002700990525     C                   enddo
002800990525     C                   close     xrpgm02l
002900990525     C                   endif
003000990525
003100990525     C                   seton                                        lr
