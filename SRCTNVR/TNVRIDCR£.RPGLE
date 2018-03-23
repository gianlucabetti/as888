000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500060228      *       scrive iftlav                                           *
000600031105      *                                                               *
000700031105      *****************************************************************
000800060228     FIMLAV00F  IF   E             DISK
000900060228     FTIIFT00f  o  a E             DISK
001000031105
001100050302      *
001200060228     c                   do        *hival
001300060228     c                   read      imlav00f
001400060228
001500060228     c                   if        %eof(imlav00f)
001600060228     c                   leave
001700060228     c                   endif
001800060228
001900060228     c                   if        imldft <> 310106 and imldft <> 010206
002000060228     c                   iter
002100060228     c                   endif
002200060228
002300060228     c                   If        imldft = 310106
002400060228     c                   eval      iftdft = 20060131
002500060228     c                   eval      ifttdf = 'F'
002600060228     c                   endif
002700060228
002800060228     c                   If        imldft = 010206
002900060228     c                   eval      iftdft = 20060201
003000060228     c                   eval      ifttdf = 'I'
003100060228     c                   endif
003200050302
003300060228     c                   eval      iftnft = imlnft
003400060228     c                   eval      iftcbo = imlcbo
003500060228     c                   eval      iftcfa = imlcfa
003600060228     c                   eval      ifttft = '0'
003700060228     c                   eval      iftfft = '4'
003800060228     c                   eval      iftdsp = 20060131
003900060228     c                   eval      iftpag = imlpag
004000060228     c                   eval      ifteti = imleti
004100060228
004200060228     c                   select
004300060228      * indirizzo mail  pagine < 50 MAIL
004400060228     c                   WHEN      imlind <> *blanks and imlpag < 50
004500060228     c                   eval      ifttif = 'M'
004600060228      * no indirizzo mail , pagine < 50 e no etichetta POSTEL
004700060228     c                   WHEN      imlind = *blanks and imlpag < 50 and
004800060228     c                             imleti = 'NO'
004900060228     c                   eval      ifttif = 'P'
005000060228      * negli altri casi carta
005100060228     c                   OTHER
005200060228     c                   eval      ifttif = 'C'
005300060228     c                   endsl
005400060228
005500060228     c                   write     tiift000
005600060228
005700060228     c                   enddo
005800060228
005900050302     C                   EVAL      *inlr = *on
