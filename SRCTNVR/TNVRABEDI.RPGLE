000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300170330     FEDRDE01L  IF   E           K DISK
000400000602      *
000500000601      * -------
000600000602      * ------------------------------------------------------
000700170330     c                   read      edrde01l
000800000601      *------
000900170330     c                   dow       not %EoF(edrde01l)
001000170330     c                   if        RDENMC = 'DD4440'
001100170330      *------
001200170330      **  bisogna togliere tutti i caratteri speciali
001300170330     c     '''':' '      xlate     rdeval        rdeval
001400170330     c     '?':' '       xlate     rdeval        rdeval
001500170330     c     '+':' '       xlate     rdeval        rdeval
001600170330     c     ':':' '       xlate     rdeval        rdeval
001700170330      **
001800170330      *------
001900170330     c                   end
002000170330     c                   read      edrde01l
002100170330     c                   enddo
002200950201     C                   SETON                                        LR
002300000601      *---------------------------------------------------------
