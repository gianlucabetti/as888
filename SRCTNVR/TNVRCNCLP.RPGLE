000100040220     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040220
000300121106     fCnclp00f  if   e           k Disk
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
002200121106     c     kClp          Reade     Cnclp00f
002300040223      * fine file
002400040220     c                   If        %Eof
002500040220     c                   Leave
002600040223     c                   EndIf
002700040223      * controllo rcd
002800121106     c                   If        Clpcon <> '   '
002900121106     c                             and Clpnar <> '   '
003000040607      * sfleggo aco x la trasmissione
003100040607     c     kAco          Chain     Cnaco00f
003200040607     c                   If        %Found(Cnaco00f)
003300040607     c                   Clear                   AcoDtr
003400040607     c                   Clear                   AcoFtr
003500040607     c                   Update    Cnaco000
003600040607     c                   EndIf
003700040607     c                   EndIf
003800040223
003900040223    1c                   EndDo
004000040220
004100040220     c                   Eval      *InLr = *On
