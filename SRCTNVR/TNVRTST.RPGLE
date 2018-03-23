000100011015     H DECEDIT('0,') DATEDIT(*DMY/)
000200011015
000300011015     ftntst00f  up   e             disk
000400011015
000500011015     c                   if        (tsties <> 0 or tstipt <> 0 or
000600011015     c                              tstico <> 0 or tstili <> 0)and
000700011015     c                             tstdiv = '   '
000800011015     c                   eval      tstdiv = 'ITL'
000900011015     c                   except    aggtst
001000011015     c                   endif
001100011015
001200011015     otntst000  e            aggtst
001300011015     o                       tstdiv
