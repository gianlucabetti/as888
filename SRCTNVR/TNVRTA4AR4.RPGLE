000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400071106      * Pulisco record "A"
000500040225      *------------------------------------------------------------------------*
000501160127     fTitas00f  if   e             Disk
000700160121     fTita430c  uf a e           k Disk
000800071107     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000801160121
000802160121     D DTA4A         E DS
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
001700160127     c                   Read      Titas00f
001801160210     c*
001900040226     c                   If        %eof
002000040226     c                   Leave
002100040226     c                   EndIf
002101160210
002102160210     c                   if        tascbo<>'FI'
002103160210     c                   iter
002104160210     c                   endif
002105160210     c
002200071107     c
002300160121     c     kta4          chain     tita430c
002400160121     c                   if        not %found(tita430c)
002402160127     c                   clear                   tita4000
002403160121     c                   eval      ta4aas=tasaas
002404160121     c                   eval      ta4lnp=taslnp
002405160121     c                   eval      ta4nrs=tasnrs
002406160121     c                   eval      ta4nsp=tasnSP
002407160121     c                   eval      ta4trc='A'
002408160121     c                   clear                   ta4not
002600071107     c                   endif
002601160210     c
002602160121     c                   movel     ta4not        dta4a
002603160210     c                   if        §ta4anas=*blanks
002604160210     c                   add       1             conta             9 0
002605160210     c                   endif
002606160210     c
002607160121     c                   eval      §ta4anas=tasnas
002608160121     c                   movel     dta4a         ta4not
002609160121     c                   if        not %found(tita430c)
002610160127     c                   write     tita4000
002611160121     c                   else
002612160127     c                   update    tita4000
002613160121     c                   endif
002700071107     c
002900071107     c                   enddo
002901160127      *
002902160210     c                   dsply                   conta
003000040225     c                   Eval      *Inlr = *On
003100071107     c
