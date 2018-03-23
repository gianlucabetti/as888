000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300051125     Fjanmdel   if   E             DISK
000400051125     Ffnanml    iF   E           K DISK
000500051125     F                                     RENAME(QJORDJE:QJORDJEl)
000600051125     Ffnanmout  o    f   85        DISK
000700051125     Ffnanmnrr  o    f   85        DISK
000800051125     d wrec            s             85
000900051125     d wrec1           s             85
001000051125     c                   do        *hival
001100051125     c                   read      janmdel
001200051125     c                   if        %eof(janmdel)
001300051125     c                   leave
001400051125     c                   endif
001500051125     c     joctrr        setgt     fnanml
001600051125     c     joctrr        readpe    fnanml
001700051125     c                   if        not %eof(fnanml)
001800051125     c                   movel     joesd         wrec
001900051125     c                   except    riga
002000051125     c                   else
002100051125     c                   movel     joctrr        wrec1
002200051125     c                   except    riga1
002300051125     c                   endif
002400051125     c                   enddo
002500051125     c                   seton                                        lr
002600051125     Ofnanmout  e            riga
002700051125     O                       wrec                85
002800051125     Ofnanmnrr  e            riga1
002900051125     O                       wrec1               85
