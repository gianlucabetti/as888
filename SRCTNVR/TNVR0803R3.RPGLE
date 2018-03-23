000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200030804
000300030805     FTITAS00F  UP   E             DISK
000400030805     FTIASC02L  UF   E           K DISK
000500030805
000600030805     C     KTAS          KLIST
000700030805     C                   KFLD                    TASAAS
000800030805     C                   KFLD                    TAsLNP
000900030805     C                   KFLD                    TAsNRS
001000030805     C                   KFLD                    TAsNSP
001100030805     C                   KFLD                    TAsTBL
001200030805
001300030805     C                   EVAL      *IN99 = *On
001400030805
001500030805    1C                   IF        TASFIV > 900   and
001600030805     C                             (TASdFT = 20030731 or tasdFT = 20030801)
001700030804
001800030805     C                   CLEAR                   TASDFT
001900030805     C                   CLEAR                   TASNFT
002000030805     C                   CLEAR                   TASFIV
002100030805     C                   CLEAR                   TASFIC
002200030805
002300030805     C   99              UPDATE    TITAS000
002400030805
002500030805     C     KTAS          CHAIN     TIASC000
002600030805     C                   IF        %FOUND(TIASC02L)
002700030805     C   99              DELETE    TIASC000
002800030805     C                   ENDIF
002900030805
003000030805    1C                   ENDIF
