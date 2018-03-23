000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400071106      * Pulisco record "A"
000500040225      *------------------------------------------------------------------------*
000600071107     fTitas30c  if   e           k Disk
000700160121     fTita430c  uf a e           k Disk
000800071107     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000801160121
000802160121     D DTA4A         E DS
000803160121     ITITAS000      05
000804160121     ITITAS010      06
000805160121     ITITASP00      07
000900001006      *------------------------------------------------------------------------*
001000160121     c     kta4          klist
001100160121     c                   kfld                    tasaas
001200160121     c                   kfld                    taslnp
001300160121     c                   kfld                    tasnrs
001400160121     c                   kfld                    tasnsp
001401160121     c                   kfld                    ktrc
001402160121     c                   movel     'A'           ktrc              1
001500071107     c
001600040225do  1c                   Do        *Hival
001601160121     c                   setoff                                       050607
001700160121     c                   Read      Titas30c
001800040226
001900040226     c                   If        %eof
002000040226     c                   Leave
002100040226     c                   EndIf
002200071107     c
002300160121     c     kta4          chain     tita430c
002400160121     c                   if        not %found(tita430c)
002401160121     c                   clear                   tita4000
002402160121     c                   clear                   tita4p00
002403160121     c                   eval      ta4aas=tasaas
002404160121     c                   eval      ta4lnp=taslnp
002405160121     c                   eval      ta4nrs=tasnrs
002406160121     c                   eval      ta4nsp=tasnSP
002407160121     c                   eval      ta4trc='A'
002408160121     c                   clear                   ta4not
002600071107     c                   endif
002601160121     c                   movel     ta4not        dta4a
002602160121     c                   eval      §ta4anas=tasnas
002603160121     c                   movel     dta4a         ta4not
002604160121     c                   if        not %found(tita430c)
002605160121     c   07              write     tita4p00
002606160121     c  n07              write     tita4000
002607160121     c                   else
002608160121     c   07              update    tita4p00
002609160121     c  n07              update    tita4000
002610160121     c                   endif
002700071107     c
002800071107      *
002900071107     c                   enddo
003000040225     c                   Eval      *Inlr = *On
003100071107     c
