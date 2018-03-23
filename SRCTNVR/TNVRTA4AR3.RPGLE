000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400071106      * Pulisco record "A"
000500040225      *------------------------------------------------------------------------*
000600071107     fTitas30c  if   e           k Disk
000700071107     fTita430c  if   e           k Disk
000800071107     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900001006      *------------------------------------------------------------------------*
001000071107     c     ktas          klist
001100071107     c                   kfld                    ta4aas
001200071107     c                   kfld                    ta4lnp
001300071107     c                   kfld                    ta4nrs
001400071107     c                   kfld                    ta4nsp
001500071107     c
001600040225do  1c                   Do        *Hival
001700071107     c                   Read      Tita430c
001800040226
001900040226     c                   If        %eof
002000040226     c                   Leave
002100040226     c                   EndIf
002200071107     c
002300071107     c     ktas          chain     titas30c
002400071107     c                   if        not %found(titas30c)
002500071107     c                   except    stampa
002600071107     c                   endif
002700071107     c
002800071107      *
002900071107     c                   enddo
003000040225     c                   Eval      *Inlr = *On
003100071107     c
003200071107     OQSYSPRT   E            STAMPA         1
003300071107     O                       ta4aas        Z
003400071107     O                       ta4lnp        Z   +  1
003500071107     O                       ta4nrs        Z   +  1
003600071107     O                       ta4nsp        Z   +  1
