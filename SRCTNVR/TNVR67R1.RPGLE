000100990914     fFNBLP00F  UP   E             DISK
000200990914
000300990914     c                   if          BLPvmd > *zeros  and  BLPvad = *blanks
000400990914     c                   eval      BLPvad = 'ITL'
000500990914     c                   endif
000600990914
000700990914     c                   if          BLPias > *zeros  and  BLPvas = *blanks
000800990914     c                   eval      BLPvas = 'ITL'
000900990914     c                   endif
001000990914
001100990914     c                   UPDATE    FNBLP000
