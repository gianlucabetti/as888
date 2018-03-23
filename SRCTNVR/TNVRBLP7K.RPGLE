000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300130124     Fblpnobld  iF   E             DISK
000400130124     FFNblp01l  uF   E           K DISK    rename(fnblp000:fnblp001)            3
000500130124     f                                     prefix(blx:3)
000600091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700021214      *--------------------------------------------------
000800070913     c     kblp          klist
000900130124     c                   kfld                    blpaas
001000130124     c                   kfld                    blplnp
001100130124     c                   kfld                    blpnrs
001200130124     c                   kfld                    blpnsp
001300130124     c
001400130124     c                   clear                   conta             4 0
001500070913     c
001600130124     c                   read      blpnobld
001700051110
001800130124    1c                   DOW       not %EOF(blpnobld)
001900130124     c     kblp          chain     fnblp01l
002000130124     c                   if        %found(fnblp01l)
002100130124     c                   eval      blxctm='6 '
002200130124     c                   update    fnblp001
002300130124     c                   eval      conta=conta+1
002400130124     c                   endif
002500130124     c
002600130124     c                   read      blpnobld
002700091125     c                   enddo
002800021216      *
002900130124     c                   except    totale
003000130124     c
003100021214     C                   eval       *inlr = *on
003200130124      *
003300130124     OQSYSPRT   E            totale      1
003400130124     O                                              'Tot:'
003500130124     O                       conta         2  +   1
