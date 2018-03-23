000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*--------------------------------------------------------------*
000300020802     D
000400121120     d tntb50ds      e ds
000500121120     d kpjba         e ds
000600980528     I***************************************************************
000700111206     c     *entry        plist
000800121120     c                   parm                    v2cccm            7
000900121120     c                   parm                    v2cfil            3
001000121120     C* se non lo è controllo che si possa comunque trasferire mediante iscrizione su tab.3TR
001100121120     c                   clear                   tntb50ds
001200121120     c                   movel     v2cccm        b50ke2
001300121120     c                   movel     v2cccm        b50ke1
001400121120     c                   movel     'C'           b50opz
001500121120     c                   movel     v2cfil        b50poa
001600121120     c                   call      'TNTB51R'
001700121120     c                   parm                    kpjba
001800121120     c                   parm                    tntb50ds
001900121120     c
002000121120     c                   if        b50err<>' '
002100121120     c     b50err        dsply
002200121120     c                   endif
002300020322     C*
002400020322     C                   SETON                                        LR
