000100940224     H*--------------------------------------------------------------*
000200100716     Fedmap00F  o    E             DISK
000300100716     Fedsgm00f  if   E             DISK
000400900131     C*---------------------------------------------------------------*
000500030515     c*
000600030515     c                   do        *hival
000700100716     c                   read      edsgm00f
000800030515     c* fine file
000900100716     c                   if        %eoF(edsgm00f)
001000030515     c                   leave
001100030515     c                   end
001200100716     C*
001300100716     c                   clear                   edmap000
001400030205     C*
001500100716     c                   eval      MAPCODICE = SGMCODICE
001600100716     c                   eval      MAPSEQUEN = SGMSEQUEN
001700100716     c                   eval      MAPPROGR  = SGMPROGR
001800100716     c                   eval      MAPCATEGO = SGMCATEGO
001900100716     c                   eval      MAPNCAMPO = SGMNCAMPO
002000100716     c                   eval      MAPOBBLIG = SGMOBBLIG
002100100716     c                   eval      MAPTIPOD  = SGMTIPOD
002200100716     c                   eval      MAPLUNGH  = SGMLUNGH
002300100716     c                   eval      MAPDECIM  = SGMDECIM
002400100716     c                   eval      MAPDESCR  = SGMDESCR
002500100716     c                   eval      MAPDESCI  = SGMDESCI
002600100716     C*
002700100716     c                   write     edmap000
002800030205     c                   enddo
002900000616     C*
003000000000     C                   SETON                                        LR
