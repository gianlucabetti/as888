000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200160205     Ffiar5errf iF   E             DISK
000300160205     Ffiar531c  uF   E           K DISK
000400130315     f                                     prefix(X_)
000500160205     F                                     ignore(Fiar5p00)
000600100317     c
000700111012     c*
000800130314     c*
000900160205     C     kar5          klist
001000160205     C                   kfld                    ar5aas
001100160205     C                   kfld                    ar5lnp
001200160205     C                   kfld                    ar5nrs
001300160205     C                   kfld                    ar5nsp
001400160205     C                   kfld                    ar5trd
001500160205     C
001600011003     C*
001700160205     c                   do        *hival
001800160205     C                   read      fiar5errf
001900160205     c                   if        %eof(fiar5errf)
002000160205     c                   leave
002100160205     c                   endif
002200160205     c     kar5          chain     fiar531c
002300160205     c                   if        %found(fiar531c)
002400160205     c                   eval      X_ar5uni=ar5uni
002500160205     c                   update    fiar5000
002600160205     c                   endif
002700160205     c                   enddo
002800011003     C
002900011003     C                   SETON                                        LR
