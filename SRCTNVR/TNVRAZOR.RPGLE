000100021118     FAZorg00f  UP   e             disk
000200021118     Fazorg01L  IF   e           k disk    rename(azorg:azorg01)
000300021118
000400021118     d  og140        e ds
000500021118     d  og143        e ds
000600021118     d  Wog140       e ds                  extname(og140) prefix(W)
000700021118
000800021118     c                   IF        orgfel <> 46
000900021118
001000021118     c                   if        ORGFIL <> ORGFEL
001100021118     c     orgfel        chain     azorg01
001200021118     c                   endif
001300021118     c                   movel     orgcar        §ogara
001400021118     c                   movel     orgfel        §ogaex
001500021118     c                   movel     orgfel        §ogapo
001600021118     c                   movel     OG140         ORGDE0
001700021118     c                   clear                   ORGFTR
001800021118     c                   except    AGGAZORG
001900021118
002000021118     c                   ELSE
002100021118     c                   movel     orgde3        og143
002200021118     c                   SELECT
002300021118     c                   WHEN      ORGFIL = 26
002400021118     c                   movel     orgcar        §ogara
002500021118     c                   movel     026           §ogaex
002600021118     c                   movel     026           §ogapo
002700021118     c                   movel     OG140         ORGDE0
002800021118     c                   clear                   ORGFTR
002900021118     c                   except    AGGAZORG
003000021118     c                   WHEN      ORGFIL = 18  or ORGFIL = 19
003100021118     c                   movel     orgcar        §ogara
003200021118     c                   movel     018           §ogaex
003300021118     c                   movel     018           §ogapo
003400021118     c                   movel     OG140         ORGDE0
003500021118     c                   clear                   ORGFTR
003600021118     c                   except    AGGAZORG
003700021118     c                   WHEN      §ogas  = 946
003800021118     c                   movel     102           §ogara
003900021118     c                   movel     049           §ogaex
004000021118     c                   movel     049           §ogapo
004100021118     c                   movel     OG140         ORGDE0
004200021118     c                   clear                   ORGFTR
004300021118     c                   except    AGGAZORG
004400021118     c                   ENDSl
004500021118
004600021118     c                   ENDIF
004700021118
004800021118      *________________________________________________
004900021118     oAZORG     e            aggAZORG
005000021118     o                       ORGDE0
005100021118     o                       ORGFTR
