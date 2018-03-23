000100010525      /TITLE Upload via Internet: sflegga stati su membro TIVIN
000200130114     H dftactgrp(*no) actgrp(*caller)
000300990916
000400010525     Ftivin00r  uf   e             disk
000500990920
000600990916
000700010525     C* Sfleggo incondizionatamente tutti i record cmq elaborati del membro corrente del TIVIN00R
000800010525     C                   read      tivin00r                               lr
000900010525     C*
001000010525     C                   dow       not *inlr
001100010525     C*
001200010525     C                   if        vinflg <> *blanks
001300010525     C                   eval      vinflg = *blanks
001400010525     C                   update    tivin000
001500010525     C                   endif
001600010525     C*
001700010525     C                   read      tivin00r                               lr
001800010525     C                   enddo
001900010525     C*
