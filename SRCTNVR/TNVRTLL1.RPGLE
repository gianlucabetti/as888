000100011015     H DECEDIT('0,') DATEDIT(*DMY/)
000200011015
000300011015     ftitll00f  up   e             disk
000400011015
000500011113     d yeurcods      e ds
000600011113
000700011113     c                   if        tlldiv = 'ITL'
000800011113
000900011113     c                   if        tllili > 1
001000011113     c                   clear                   yeurcods
001100011113     c                   movel     tlldiv        yecdvi
001200011113     c                   z-add     tllili        yecimi
001300011113     c                   movel     'EUR'         yecdvo
001400011113     c                   call      'YEURCO'
001500011113     c                   parm                    yeurcods
001600011113     c                   if        yecesi = *blanks
001700011113     c                   z-add     yecimo        tllili
001800011113     c                   endif
001900011113     c                   endif
002000011113
002100011113     c                   if        tllies > 1
002200011113     c                   clear                   yeurcods
002300011113     c                   movel     tlldiv        yecdvi
002400011113     c                   z-add     tllies        yecimi
002500011113     c                   movel     'EUR'         yecdvo
002600011113     c                   call      'YEURCO'
002700011113     c                   parm                    yeurcods
002800011113     c                   if        yecesi = *blanks
002900011113     c                   z-add     yecimo        tllies
003000011113     c                   endif
003100011113     c                   endif
003200011113
003300011113     c                   eval      tlldiv = 'EUR'
003400011231     c                   clear                   tllftr
003500011015     c                   except    aggtll
003600011015     c                   endif
003700011015
003800011015     otitll000  e            aggtll
003900011113     o                       tllili
004000011113     o                       tllies
004100011231     o                       tllftr
004200011015     o                       tlldiv
