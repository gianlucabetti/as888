000100040211     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040211
000300040211     fFnspe01l  uf   e           k Disk
000400040211     fCnaco00f  if   e           k Disk
000500040211
000600040211     d wSpeFls         s                   Like(SpeFls) Inz('L')
000700040211
000800040211      *---------------------------------------------------------------*
000900040211
001000040211     c     kAco          Klist
001100040211     c                   Kfld                    AcoKut
001200040211     c                   Kfld                    AcoKcc
001300040211     c                   Kfld                    AcoKsc
001400040211
001500040211     c                   Eval      AcoKut = 1
001600040211     c                   Eval      AcoKcc = 151
001700040211
001800040211     c     wSpeFls       Setll     Fnspe01l
001900040211     c                   Do        *Hival
002000040211     c     wSpeFls       Reade     Fnspe01l
002100040211     c                   If        %Eof(Fnspe01l)
002200040211     c                   Leave
002300040211     c                   EndIf
002400040211
002500040211      * Aggancio Cnaco
002600040211     c                   Eval      AcoKsc = SpeCli
002700040211     c     kAco          Chain     Cnaco00f
002800040211      * Se non esiste su Cnaco elimino Fnspe
002900040211     c                   If        Not %Found(Cnaco00f)
003000040211     c                   Delete    Fnspe000
003100040211     c                   EndIf
003200040211
003300040211     c                   EndDo
003400040211
003500040211     c                   Eval      *InLr = *On
