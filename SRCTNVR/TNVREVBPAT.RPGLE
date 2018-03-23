000100990914     H DECEDIT('0,') DATEDIT(*DMY.)
000200061011     Ffnevb03l  UF   E           K DISK
000300061011     Ffnarb01l  IF   E           K DISK
000400990914     C*--------------------------------------------------------------------*
000500061011     C     Karb          KLIST
000600061011     C                   Kfld                    evbaas
000700061011     C                   Kfld                    evblnp
000800061011     C                   Kfld                    evbnrs
000900061011     C                   Kfld                    evbnsp
001000061011     C     Kevb          KLIST
001100061011     C                   Kfld                    kfle              3 0
001200061011     C                   Kfld                    kcev              3
001300061011     c                   eval      kfle=046
001400061011     c                   eval      kcev='PAT'
001500061011     c
001600061011     c     kEVB          setll     fnevb03l
001700061011     c     kEVB          reade     fnevb03l
001800061011     c                   dow       not %eof(fnevb03l)
001900061011     c     karb          chain     fnarb01l
002000061011     c                   if        %found(fnarb01l)
002100061011     c                   movel     arblna        evbfle
002200061011     c                   update    fnevb000
002300061011     c                   endif
002400061011     c     kEVB          reade     fnevb03l
002500061011     c                   enddo
002600061011     c
002700040324     c                   seton                                        lr
