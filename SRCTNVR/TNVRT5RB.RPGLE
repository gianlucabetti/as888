000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400160412      * elimina campo natura merce
000500040225      *------------------------------------------------------------------------*
000600160427     fTitas10f  uf   e             Disk
002500001006      *------------------------------------------------------------------------*
003500160427     c     35648538      chain     titas10f
003501160427     c                   if        %found(titas10f)
004400160412     c                   clear                   tasnas
004700160427     c                   except    tas10
004701160427     c                   end
005900040225     c                   Eval      *Inlr = *On
006300160413     Otitas010  E            tas10
006400160413     O                       TASNAS
