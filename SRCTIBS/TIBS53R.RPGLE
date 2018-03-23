000100971103     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200971110     F
000300020716     Fazorg01l  iF   E           K DISK
000400971110     c**
000500020716     c     *entry        plist
000600020716     C                   PARM                    i53po             3
000700020716     C                   PARM                    o53dst            3
000800020716     c*
000900020716     c                   if        i53po='946'
001000020716     c                   eval      o53dst='046'
001100020716     c                   else
001200020716     c                   move      i53po         k0030             3 0
001300020716     c     k0030         chain     azorg01l                           31
001400020716     c                   if        *in31
001500020716     c                   movel     i53po         o53dst
001600020716     c                   else
001700020716     c                   movel     orgfel        o53dst
001800020716     c                   endif
001900020716     c                   endif
002000020716     c                   seton                                        rt
