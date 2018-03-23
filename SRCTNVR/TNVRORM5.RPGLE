000100040909     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200991227
000300040914     fFnorf00f  if   e             Disk
000400040914     f                                     prefix(F)
000500040914     fFnorf01l  o    e           k Disk    rename(FNORF000:FNORF01)
000600000725
000700000725      *----------------------------------------------------------------*
000800040909
000900040909     c                   Do        *Hival
001000040914     c                   Read      Fnorf00f
001100040909      * Fine file
001200040914     c                   If        %Eof(Fnorf00f)
001300040909     c                   Leave
001400040909     c                   EndIf
001500040909
001600040914     c                   Clear                   Fnorf01
001700040914     c                   Eval      OrfPoe = FOrfPoe
001800040914     c                   Eval      OrfNsr = FOrfNsr
001900040914     c                   Eval      OrfNor = FOrfNor
002000040914     c                   Eval      OrfNrv = FOrfNrv
002100040914     c                   Eval      OrfPog = FOrfPog
002200040914     c                   Eval      OrfDae = FOrfDae
002300040914     c                   Eval      OrfOre = FOrfOre
002400040914     c                   Eval      OrfFar = FOrfFar
002500040914     c                   Eval      OrfCar = FOrfCar
002600040914     c                   Eval      OrfPue = FOrfPue
002700040914     c                   Eval      OrfDfc = *Date
002800040914     c                   Write     Fnorf01
002900040909
003000040909     c                   EndDo
003100040914
003200040914     c                   Eval      *InLr = *On
