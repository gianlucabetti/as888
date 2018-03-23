000100060207     h Decedit('0,') Datedit(*YMD.)
000200001006
000400060208     fftdselez1 if   e             Disk    rename(ftdselez2:ftdsel)
000600060208     fFifce01l  o  a e           k Disk
000800010116
000900001006      *------------------------------------------------------------------------*
002100040225
002200040225do  1c                   Do        *Hival
002300060208     c                   Read      ftdselez1
002400040226
002500060208     c                   If        %eof(ftdselez1)
002600040226     c                   Leave
002700040226     c                   EndIf
002701060208
002702060208     c                   clear                   fifce000
002703060208
002704060208     c                   z-add     ftdpdr        fcepdr
002705060208     c                   move      ftdtsr        fcetsr
002706060208     c                   movel     ftdpdr        FCEFGS
002707060208
002708060208     c                   z-add     ftdNDC        FCENDC
002709060208     c                   z-add     ftdDDC        FCEDDC
002710060208     c                   z-add     ftdAAS        FCEAAS
002711060208     c                   z-add     ftdLNP        FCELNP
002712060208     c                   z-add     ftdNRS        FCENRS
002713060208     c                   z-add     ftdNSP        FCENSP
002714060208     c                   move      '005'         FCEcce
002715060208
002716060208     c                   write     fifce000
002717060208
005500060207     c                   enddo
011900010202
012000040225     c                   Eval      *Inlr = *On
