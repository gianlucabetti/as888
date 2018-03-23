000100000000     H Option( *SrcStmt )
000200160804
000300160804       // - Parametri di input
000400160809     D pIn_KSU         s              7a
000500160809     D pIn_TIP         s              2a
000600160804     D pIn_LANG        s              2a
000700160804
000800160804       // - Parametri di output
000900160804     D pOut_Esito      s              1s 0                                      0=Err 1=OK 2=KO
001000160809     D pOut_PGT        s             10a
001100160809     D pOut_PPT        s             50a
001200160804     D pOut_Msg        s            128a   varying
001300090521
001400090520
001500090520     C*
001600160809     C                   Call      'UBKSUTIPR'
001700160809     C                   Parm                    pIn_KSU
001800160809     C                   Parm                    pIn_TIP
001900160804     C                   Parm                    pIn_LANG
002000160804     C                   Parm                    pOut_Esito
002100160809     C                   Parm                    pOut_PGT
002200160809     C                   Parm                    pOut_PPT
002300160804     C                   Parm                    pOut_Msg
002400160804     **
002500160804     C                   movel     pOut_Msg      wDiplay          52
002600160804     **
002700160804     C     wDiplay       DSPLY
002800000000     **
002900090520     C                   Seton                                        LR
003000000000     **
