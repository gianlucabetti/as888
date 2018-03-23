000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300031105      *                                                               *
000400031105      *****************************************************************
000500171204     Ftiidc01l  IF   E           k DISK
000600171204     D fidna7ds      E DS
000700171204     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800031105
000900050302      *
001000171204     c                   clear                   kaas              4 0
001100171204     c                   eval      kaas=2017
001200171204     c     kaas          setll     tiidc01l
001300171204     c
001400171204     c                   do        *hival
001500171204     c     kaas          reade     tiidc01l
001600060228
001700171204     c                   if        %eof(tiidc01l)
001800060228     c                   leave
001900060228     c                   endif
002000060228
002100171204     c                   if        idcelaute='INTERNET'
002200171204     c                             and idceladata>=20171130120900
002300171204     c                             and idceladata<=20171130120959
002400171204     c                   except    ra
002500171204     c                   clear                   fidna7ds
002600171204     C                   EVAL      IDNA7ANT=IDCANT
002700171204     C                   EVAL      IDNA7NUT=IDCNUT
002800171204     C                   EVAL      IDNA7PDT=IDCPDT
002900171204     C                   EVAL      IDNA7PCT=idcfgs
003000171204     C                   EVAL      IDNA7utc='INTERNET'
003100171204     c                   call      'FIDNA7R'
003200171204     c                   parm                    fidna7ds
003300171204     c                   add       1             conta             7 0
003400171204     c
003500171204     c                   endif
003600060228     c                   enddo
003700171204     c                   except    tot
003800060228
003900050302     C                   EVAL      *inlr = *on
004000171204     OQSYSPRT   E            ra          1
004100171204     O                       idcant            +  1
004200171204     O                       idcnut            +  1
004300171204     O                       idcpdt            +  1
004400171204     O                       idcaas            +  1
004500171204     O                       idclnp            +  1
004600171204     O                       idcnrs        z   +  1
004700171204     O                       idcnsp        z   +  1
004800171204     OQSYSPRT   E            tot         1
004900171204     O                       conta         2   +  1
