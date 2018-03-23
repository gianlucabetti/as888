000100010308      /TITLE Restituisce schiere £3, L3 e §L3.
000200010308
000300010308     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000400010308
000500010308     D PrmDrf          S               D
000600010308     D PrmTfp          S              3S 0
000700010308     D PrmAddReplace   S              1
000800010308     D PrmEsito        S              1
000900010308     D £3              S              3  0 DIM(999)
001000010308     D L3              S              3    DIM(999)
001100010308     D §L3             S              3    DIM(999)
001200010308
001300010308     C     *ENTRY        PLIST
001400010308     C                   PARM                    PrmDrf
001500010308     C                   PARM                    PrmTfp
001600010308     C                   PARM                    PrmAddReplace
001700010308     C                   PARM                    PrmEsito
001800010308      *
001900010308     C                   CALL      'TRUL18R'
002000010308     C                   PARM                    PrmDrf
002100010308     C                   PARM                    PrmTfp
002200010308     C                   PARM                    PrmAddReplace
002300010308     C                   PARM                    PrmEsito
002400010308     C                   PARM                    £3
002500010308     C                   PARM                    §L3
002600010308     C                   PARM                    L3
002700010308      *
002800010308     C                   RETURN
