000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400160412      * elimina campo natura merce
000500040225      *------------------------------------------------------------------------*
000600160412     fTitasP0f  uf   e           k Disk
000700040225
000800001006      *------------------------------------------------------------------------*
000900111107     c
001000040225
001100160412     c                   Read      titasp0f
001200040226
001300160412    1c                   dow       not %eof(titasp0f)
001400160412     c                   clear                   tasnas
001500160412     c                   except    tas
001600160412     c                   Read      titasp0f
001700040415    1c                   EndDo
001800010202
001900111107     c
002000040225     c                   Eval      *Inlr = *On
002100160412     Otitasp00  E            tas
002200160412     O                       TASNAS
