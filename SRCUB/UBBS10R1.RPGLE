000100000000     H Option( *SrcStmt )
000200160804
000300170515     D/COPY GAITRASRC/SRCPROTOPI,UBBS10R
000400090521
000500090520
000600170511     C*
000700170515     C                   eval      iBS10DRF  = *zeros
000800170515     C                   eval      iBS10TLE  = 'WW'
000900170515     C                   eval      iBS10CLI  = 1662139
001000170509     C*
001100170515     C                   Call      'UBBS10R'
001200170515     C                   Parm                    iBS10DRF
001300170515     C                   Parm                    iBS10TLE
001400170515     C                   Parm                    iBS10CLI
001500170515     C                   Parm                    oBS10Esito
001600170515     C                   Parm                    oBS10COP
001700170515     C                   Parm                    oBS10TIP
001800160804     **
001900170515     C     oBS10TIP      DSPLY
002000000000     **
002100090520     C                   Seton                                        LR
002200000000     **
