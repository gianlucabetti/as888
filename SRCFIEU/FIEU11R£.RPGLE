000100161298      ***********************************************************************
000200161298      *   PULIZIA FNEUR                                                     *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600070102     FTIDP300F  UP   E             DISK
000700161298     FFNBLP01L  IF   E           K DISK
000800000302     FFNARB01L  IF   E           K DISK
000900161298      *---------------------------------------------------------------------*
001000000302     c                   IF        DP3nsp > *zeros
001100000224     C     KBOL          chain     FNBLP000
001200000302     c                   If        NOT %found
001300000302     C     KBOL          chain     FNARB000
001400000302     c                   If        NOT %found
001500070102     c                   delete    TIDP3000
001600000302     C                   Endif
001700000302     C                   Endif
001800000224     C                   ENDIF
001900161298      *----------------------------------------------------------------
002000161298     C     KBOL          KLIST
002100000302     C                   KFLD                    dp3AAS
002200000302     C                   KFLD                    dp3LNP
002300000302     C                   KFLD                    dp3NRS
002400000302     C                   KFLD                    dp3NSP
