000100990910     h DECEDIT('0,') DATEDIT(*DMY.)
000200990910
000300990914     fFNARBU0F  IP   E             DISK    prefix(O)
000400990914     fFIARBU0F  O    E             DISK
000500990910
000600990914     c                   clear                   fiarbU00
000700990910      *
000800990914     c                   eval      ARBaas = OARBaas
000900990914     c                   eval      ARBlnp = OARBlnp
001000990914     c                   eval      ARBnrs = OARBnrs
001100990914     c                   eval      ARBnsp = OARBnsp
001200990914     c                   eval      ARBtrc = '1'
001300990914     c                   eval      ARBdtv = OARBdtv
001400990914     c                   eval      ARBorv = OARBorv
001500990914     c                   eval      ARBsvn = OARBsvn
001600990914     c                   eval      ARBvan = OARBvan
001700990913      *
001800990914     c                   WRITE     FIARBu00
