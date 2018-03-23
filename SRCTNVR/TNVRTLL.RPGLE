000100011015     H DECEDIT('0,') DATEDIT(*DMY/)
000200011015
000300011015     ftitll00f  up   e             disk
000400011015
000500011015     c                   if        tllili <> 0 and tlldiv = '   '
000600011015     c                   eval      tlldiv = 'ITL'
000700011015     c                   except    aggtll
000800011015     c                   endif
000900011015
001000011015     otitll000  e            aggtll
001100011015     o                       tlldiv
