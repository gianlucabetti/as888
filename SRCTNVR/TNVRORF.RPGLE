000100010323     H DECEDIT('0,') DATEDIT(*YMD.)
000200010323
000300041020     fFnorf00f  uf   e             disk
000400010323
000500041020     c                   Do        *Hival
000600041020     c                   Read      Fnorf00f
000700041020     c                   If        %Eof (Fnorf00f)
000800041020     c                   Leave
000900041020     c                   EndIf
001000041020     c                   If        OrfDdc = *Zeros
001100041020     c                   Iter
001200041020     c                   EndIf
001300041020     c                   If        OrfDdc >= 20040000 and OrfDdc <= 20040099
001400041020     c                   add       1000          OrfDdc
001500041020     c                   Update    Fnorf000
001600041020     c                   EndIf
001700041020     c                   EndDo
001800041020
001900041020     c                   Eval      *InLr = *On
