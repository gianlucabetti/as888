000100060207     h Decedit('0,') Datedit(*YMD.)
000200001006
000300040225
000400060207     fFttval    if   e             Disk    rename(fttval:fttval00)
000500060207     fFiftt01l  if   e           k Disk
000600010116
000700001006      *------------------------------------------------------------------------*
000800060207     c     kftt          klist
000900060207     c                   kfld                    fttpdr
001000060207     c                   kfld                    ftttsr
001100060207     c                   kfld                    fttndc
001200060207     c                   kfld                    fttddc
001300040225
001400040225do  1c                   Do        *Hival
001500060207     c                   Read      Fttval
001600040226
001700040226     c                   If        %eof
001800040226     c                   Leave
001900040226     c                   EndIf
002000060207     c                   move      sel0001       fttpdr
002100060207     c                   move      sel0002       ftttsr
002200060207     c                   move      sel0004       fttndc
002300060207     c                   move      sel0005       fttddc
002400060207      * solo ftt valorizzati e non confermati
002500060207     c     kftt          chain     fiftt01l
002600060207     c                   If        %found(fiftt01l)
002700060207     c                   If        fttdcv <> 0
002800060207     c                   Iter
002900040302     c                   EndIf
003000060207     c                   else
003100060207     c                   Iter
003200060207     c                   EndIf
003300040303
003400060207     c                   Call      'TNVR021RAB'
003500060207     c                   Parm                    fttpdr
003600060207     c                   Parm                    ftttsr
003700060207     c                   Parm                    fttddc
003800060207     c                   Parm                    fttndc
003900040225
004000040225     c                   EndDo
004100010202
004200040225     c                   Eval      *Inlr = *On
