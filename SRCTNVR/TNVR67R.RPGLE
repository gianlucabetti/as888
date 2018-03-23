000100990914     fFNARB00F  UP   E             DISK
000200990917     fFIARI00F  O    E             DISK
000300990914
000400990914     c                   if          ARBvmd > *zeros  and  ARBvad = *blanks
000500990914     c                   eval      ARBvad = 'ITL'
000600990914     c                   endif
000700990914
000800990914     c                   if          ARBias > *zeros  and  ARBvas = *blanks
000900990914     c                   eval      ARBvas = 'ITL'
001000990914     c                   endif
001100990914
001200990917     c                   if          ARBifp > *zeros  or  ARBipp > *zeros
001300990917     c                   eval      ARIaas = ARBaas
001400990917     c                   eval      ARIlnp = ARBlnp
001500990917     c                   eval      ARInrs = ARBnrs
001600990917     c                   eval      ARInsp = ARBnsp
001700990917     c                   eval      ARItip = 'A'
001800990917     c                   eval      ARIdiv = 'ITL'
001900990917     c                   eval      ARIimp = ARBifp
002000990917     c                   eval      ARIipp = ARBipp
002100990917     c                   WRITE     FIARI000
002200990917     c                   clear                   ARBifp
002300990917     c                   clear                   ARBipp
002400990917     c                   endif
002500990917
002600990914     c                   UPDATE    FNARB000
