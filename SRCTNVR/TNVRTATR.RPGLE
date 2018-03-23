000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300031105      *****************************************************************
000600031105      *                                                               *
000700031105      *****************************************************************
000800160203     Ftitas_orm IF   E             DISK    rename(titas_orm:titasorm0)
000900160203     Ftnvrta7   uF a E           k DISK
000901160203     Ftita730c  iF   E           k DISK
002000050302      *
002700140312     c
002701160203     c     kta7          klist
002702160203     c                   kfld                    tasaas
002703160203     c                   kfld                    taslnp
002704160203     c                   kfld                    tasnrs
002705160203     c                   kfld                    tasnsp
002706160203     c                   kfld                    tastbl
002707160203     c                   kfld                    ksvn              1
002708160203     c                   eval      ksvn='t'
002709160203     c
003300160203     c                   read      titas_orm
003301160203     c                   dow       not %eof(titas_orm)
003302160203     c                   if        tasfic<>' '
003303160203     c                   if        tastbl='F1' or tastbl='A2' or
003304160203     c                             tastbl='A3' or tastbl='F3' or
003305160203     c                             tastbl='AR'
003306160203
003401160203     c     tasksc        chain     tnvrta7
003402160203     c                   if        not %found(tnvrta7)
003403160203     c                   clear                   tnvrta70
003404160203     c                   eval      tnvksc=tasksc
003405160203     c                   endif
003406160203     c
003407160203     c                   clear                   ta7svn
003408160203     c                   clear                   ta7van
003409160203     c     kta7          chain     tita730c
003412160203     c
003413160203     c                   add       1             tnvtot
003414160203     c                   if        tassv1='t' or tassv2='t' or tassv3='t' or
003415160203     c                             ta7svn='t'
003416160203     c                   add       1             tnvpag
003417160203     c*
003418160203     c                   if        (tassv1='t' and tasva1=0) or
003419160203     c                             (tassv2='t' and tasva2=0) or
003420160203     c                             (tassv3='t' and tasva3=0) or
003421160203     c                             (ta7svn='t' and ta7van=0)
003422160203     c                   add       1             tnvazz
003423160203     c                   endif
003424160203     c
003425160203     c                   else
003426160203     c                   add       1             tnvnop
003427160203     c                   endif
003428160203     c
003429160203     c                   if        not %found(tnvrta7)
003430160203     c                   write     tnvrta70
003431160203     c                   else
003432160203     c                   update    tnvrta70
003433160203     c                   endif
003434160203     c                   endif
003435160203     c                   endif
003436160203     c
003437160203     c                   read      titas_orm
003438160203     c                   enddo
003439160203     c
004800050302     C                   EVAL      *inlr = *on
