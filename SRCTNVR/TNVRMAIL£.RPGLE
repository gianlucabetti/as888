000100040220     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040220
000300040607     fCnclp00f  uf   e           k Disk
000400040607     fCnaco00f  uf   e           k Disk
000500040607
000600040607     d kkut            s                   like(clpkut) inz(1)
000700040607     d kkcc            s                   like(clpkcc) inz(151)
000800040223
000900040220      *---------------------------------------------------------------*
001000040206
001100040607     c     kClp          klist
001200040607     c                   kfld                    kkut
001300040607     c                   kfld                    kkcc
001400040607
001500040607     c     kAco          klist
001600040607     c                   kfld                    ClpKut
001700040607     c                   kfld                    ClpKcc
001800040607     c                   kfld                    ClpKsc
001900040607
002000040607     c     kClp          Setll     Cnclp00f
002100040220do  1c                   Do        *Hival
002200040607     c     kClp          Reade(e)  Cnclp00f
002300040607      * Allocato leggo il record e poi torno subito a leggere quello sucessivo
002400040607     c                   If        %Error
002500040607     c     kClp          Reade(n)  Cnclp00f
002600040607     c                   Iter
002700040607     c                   EndIf
002800040223      * fine file
002900040220     c                   If        %Eof
003000040220     c                   Leave
003100040223     c                   EndIf
003200040223      * controllo rcd
003300040607     c                   If        ClpFft <> 4
003400040607     c                   Eval      ClpFft = 4
003500040607      * sfleggo aco x la trasmissione
003600040607     c     kAco          Chain     Cnaco00f
003700040607     c                   If        %Found(Cnaco00f)
003800040607     c                   Clear                   AcoDtr
003900040607     c                   Clear                   AcoFtr
004000040607     c                   Update    Cnaco000
004100040607     c                   Update    Cnclp000
004200040607     c                   EndIf
004300040607     c                   EndIf
004400040223
004500040223    1c                   EndDo
004600040220
004700040220     c                   Eval      *InLr = *On
