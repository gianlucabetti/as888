000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300020507      * FICN56R - reperisce OUTQ in base all'unità di registrazione  *
000400020423      *           ricevo l'unità di registrazione e mi compongo OUTQ *
000500900606?     *--------------------------------------------------------------*
000600910702     FAZORG01L  IF   E           K DISK
000700020423     D com3            s              3
000800020423     D uni             s              8
000900020423     D outq            s             10
001000020423     D og143         E DS
001100020423     C     *ENTRY        PLIST                                                  *
001200020423     C                   PARM                    uni                            *
001300020423     C                   PARM                    outq                           *
001400020423     c                   clear                   outq
001500020423     C*
001600020423     C                   eval      com3 = %subst(uni: 1: 3)
001700020423     c                   move      com3          orgfil
001800020423     c     orgfil        chain     azorg01l
001900020423     c                   IF        %found
002000020423     C                   MOVEL     ORGDE3        OG143
002100020423     c                   move      orgfel        com03             3
002200020423     c                   eval      OUTQ = 'AS' + com03
002300020507     c                   if        orgfel <> orgfil
002400020423     c                   move      orgfil        com03
002500020423     c                   eval      %subst(outq: 6: 3) = com03
002600020423     c                   end
002700020507     c                   if        orgdit <> 'BAR'
002800020507     c                   move      orgdit        com03
002900020507     c                   eval      %subst(outq: 6: 3) = com03
003000020507     c                   end
003100020423     c                   end
003200020423     c                   EVAL      *INLR = *ON
