000100980127     H DECEDIT('0,') DATEDIT(*YMD.)
000200981106      *---------------------------------------------------------------------*
000300031001     FTIASC999  IP   E           K DISK    PREFIX(W)
000400001023     F*----------
000500031001     FTIASC02L  UF   E           K DISK    rename(tiasc000:tiasc)
000600010612      *---------------------------------------------------------------------*
000700980127      * Ciclo principale
000800980127      *---------------------------------------------------------------------*
000900031001     C     kasc          klist
001000031001     c                   kfld                    wascaas
001100031001     c                   kfld                    wasclnp
001200031001     c                   kfld                    wascnrs
001300031001     c                   kfld                    wascnsp
001400031001     c                   kfld                    wasctbl
001500031001      *
001600031001     C     kasc          chain     tiasc02l
001700031001     c                   If        %found(tiasc02l)
001800031001      *
001900010612     C                   EVAL      ASCKSC = WASCKSC
002000010612     C                   EVAL      ASCRSD = WASCRSD
002100010612     C                   EVAL      ASCIND = WASCIND
002200010612     C                   EVAL      ASCNAZ = WASCNAZ
002300010612     C                   EVAL      ASCCAP = WASCCAP
002400010612     C                   EVAL      ASCLOC = WASCLOC
002500010612     C*
002600031001     C                   update    TIASC
002700010612     C*
002800031001     c                   endif
