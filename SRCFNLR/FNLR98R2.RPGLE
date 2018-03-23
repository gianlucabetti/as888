000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300141112     ffnlr98d2  cf   e             workstn
000400141112     D kpjba         e ds
000500141112     D fnlr98ds2     e ds
000600141112     D fnlry09ds2    e ds
000700000607      *--------------------------------------------------------------------
000800000607     c
000900000607     c     *entry        plist
001000141112     c                   parm                    fnlr98ds2
001100141112      *
001200141112     c                   clear                   fnlry09ds2
001300141112     c                   eval      ilry09tch = 'C'
001400141112     c                   eval      ilry09tdis = 'D'
001500141112     c                   eval      ilry09ffr  = 'N'
001600141112     c                   eval      ilry09aas = i982aas
001700141112     c                   eval      ilry09lnp = i982Lnp
001800141112     c                   eval      ilry09nrs = i982NRs
001900141112     c                   eval      ilry09nsp = i982NSP
002000141112     c                   eval      kpjbu =  fnlry09ds2
002100141112     c                   call      'FNLRY09R'
002200141112     c                   parm                    kpjba
002300141112     c                   eval      fnlry09ds2 =  kpjbu
002400141112     c                   if        olry09esdi = '1'
002500141112      *
002600000607
002700000607     c                   do        *hival
002800141113     c                   eval      vaas = i982aas
002900141113     c                   eval      vlnp = i982Lnp
003000141113     c                   eval      vnrs = i982NRs
003100141113     c                   eval      vnsp = i982NSP
003200141112     c                   exfmt     finestra
003300141112     c                   if        *inkb
003400141112     c                   move      '0'           o982esito
003500141112     c                   eval      o982msgerr = 'Forzatura effettuata'
003600141112     c                   leave
003700141112     c                   endif
003800141112     c                   if        *inkl
003900141112     c                   move      '1'           o982esito
004000141112     c                   eval      o982msgerr = 'Forzatura non effettuata -
004100141112     c                             disposizioni presenti'
004200141112     c                   leave
004300141112     c                   endif
004400000607     c                   enddo
004500141112     c                   else
004600141112      *no dirottamento
004700141112     c                   move      '0'           o982esito
004800141112     c                   clear                   o982msgerr
004900141112     c                   endif
005000000607     c                   seton                                        lr
