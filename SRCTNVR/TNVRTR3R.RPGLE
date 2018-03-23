000100020712     FFLTR300F  IF   E           K DISK
000200020712     FFLTR350F  O  A E             DISK
000300020712
000400020712     d W003A           s              3
000500020712     d sistema         s              8
000600020712
000700020710     c     *entry        plist
000800020712     C                   PARM                    Sistema
000900020710      *
001000020712     c                   eval      w003A = %subst(sistema:3:3)
001100020710      *
001200020712     c                   IF        %subst(W003A:1:1) >= '0' and
001300020712     c                             %subst(W003A:2:1) >= '0'  and
001400020712     c                             %subst(W003A:3:1) >= '0'
001500020710      *
001600020712     c     *loval        setll     FLTR300F
001700020712     C                   read      fltr300f
001800020712     c                   DOW       not %EOF(FLTR300F)
001900020712     C                   movel     W003a         tr3pog
002000020712     c                   Write     fltr3500
002100020712     C                   read      fltr300f
002200020712     c                   ENDDO
002300020710      *
002400020712     c                   ENDIF
002500020712      *
002600020710     C                   eval      *inlr = *on
