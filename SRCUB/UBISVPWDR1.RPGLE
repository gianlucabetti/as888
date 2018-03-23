000100000000     H Option( *SrcStmt )
000200160804
000300160916     D/COPY GAITRASRC/SRCPROTOPI,UBISVPWDR
000400090521
000500090520
000600090520     C*
000700160804     C                   Call      'UBISVPWDR'
000800160916     C                   Parm                    iISVPWDUTE
000900160916     C                   Parm                    iISVPWDSUN
001000160916     C                   Parm                    iISVPWDISV
001100161017     C                   Parm                    iISVPWDTIP
001200160916     C                   Parm                    iISVPWDPWD
001300160916     C                   Parm                    iISVPWDLANG
001400160923     C                   Parm                    oISVPWDEsito
001500161108     C                   Parm                    oISVPWDUTE
001600161108     C                   Parm                    oISVPWDSUN
001700160923     C                   Parm                    oISVPWDTLE
001800160923     C                   Parm                    oISVPWDRSP
001900160923     C                   Parm                    oISVPWDEML
002000160923     C                   Parm                    oISVPWDTEL
002100160923     C                   Parm                    oISVPWDMsg
002200160804     **
002300160916     C                   movel     oISVPWDMsg    wDiplay          52
002400160804     **
002500160804     C     wDiplay       DSPLY
002600000000     **
002700090520     C                   Seton                                        LR
002800000000     **
