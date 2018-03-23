000100000000     H Option( *SrcStmt )
000200160804
000300170904     D/COPY GAITRASRC/SRCPROTOPI,UBCKFAMR
000400090521
000500090520
000600170511     C*
000700170904     C                   eval      iCKFAMDRF  = *zeros
000800170904     C                   eval      iCKFAMTLE  = 'WW'
000900170904     C                   eval      iCKFAMCLI1 = 1662139
001000170904     C                   eval      iCKFAMCLI2 = 1020001
001100170509     C*
001200170904     C                   Call      'UBCKFAMR'
001300170904     C                   Parm                    iCKFAMDRF
001400170904     C                   Parm                    iCKFAMTLE
001500170904     C                   Parm                    iCKFAMCLI1
001600170904     C                   Parm                    iCKFAMCLI2
001700170904     C                   Parm                    oCKFAMesito
001800170904     C                   Parm                    oCKFAMCOP
001900160804     **
002000170904     C     oCKFAMesito   DSPLY
002100000000     **
002200090520     C                   Seton                                        LR
002300000000     **
