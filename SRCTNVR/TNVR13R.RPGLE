000100991116     H*------------------------------------------------------------------------*
000200991116     H DECEDIT('0,') DATEDIT(*DMY.)
000300030131     FSICOP00F  IF   E             DISK    RENAME(SICOP000:SICOPBDG)
000400030131     FSICOP01L  UF A E           K DISK    PREFIX(U_)
000500030131     C
000600991116     C*
000700030131     C     KEYCOP        KLIST
000800030131     C                   KFLD                    COPFIL
000900030131     C                   KFLD                    COPDAT
001000030131     C                   KFLD                    COPVOC
001100030131     C                   KFLD                    COPAAS
001200030131     C                   KFLD                    COPMES
001300030131     C
001400030131     C                   READ      SICOP00F                               99
001500030131DO  1C     *IN99         DOWEQ     *OFF
001600030131     C     KEYCOP        CHAIN     SICOP01L                           98
001700030131IF  2C     *IN98         IFEQ      *ON
001800030131     C                   WRITE     SICOP000
001900030131X   2C                   ELSE
002000030131     C                   ADD       COPIMV        U_COPIMV
002100030131     C                   ADD       COPNSP        U_COPNSP
002200030131     C                   ADD       COPNCL        U_COPNCL
002300030131     C                   ADD       COPPKF        U_COPPKF
002400030131     C                   ADD       COPVLF        U_COPVLF
002500030131     C                   UPDATE    SICOP000
002600030131E   2C                   ENDIF
002700030131     C                   READ      SICOP00F
002800030131E   1C                   ENDDO
002900030131     C
003000030131     C                   SETON                                        LR
