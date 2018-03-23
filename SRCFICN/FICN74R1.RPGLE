000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200080919     D ficn74ds      e ds
000300040309     D kpjba         E DS
000400000607      *--------------------------------------------------------------------
000500000607     c
000600080922     c                   clear                   ficn74ds
000700080922     c                   z-add     3850          i74imp
000800080922     c                   z-add     0010001       i74pdr
000900080922     c                   call      'FICN74R'
001000080919     c                   parm                    ficn74ds
001100080922     c                   eval      *inlr = *on
