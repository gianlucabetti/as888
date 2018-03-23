000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500060519      *       AGGIORNA TITAI                                          *
000600031105      *                                                               *
000700031105      *****************************************************************
000800060519     FTITAS00F  IF   E             DISK
000900060519     FTITAI01L  UF a E           K DISK
001000031105
001100060519     c     KSPE          Klist
001200060519     c                   Kfld                    TASaas
001300060519     c                   Kfld                    TASlnp
001400060519     c                   Kfld                    TASnrs
001500060519     c                   Kfld                    TASnsp
001600060519     c                   Kfld                    TAStbl
001700050302      *
001800060228     c                   do        *hival
001900060519     c                   read      TITAS00F
002000060228
002100060519     c                   if        %eof(TITAS00F)
002200060228     c                   leave
002300060228     c                   endif
002400060228
002500060519     c                   if        TASIMV > 0
002600060519     c     KSPE          CHAIN     TITAI01L
002700060519     c                   IF        %found(titai01l)
002800060228
002900060519     c                   eval      taisvt = '£'
003000060519     c                   eval      taivat = tasimv
003100060519     c                   write     titai000
003200060228     c                   endif
003300060519     c                   endif
003400060228
003500060228     c                   enddo
003600060228
003700050302     C                   EVAL      *inlr = *on
