000100170726     H DECEDIT('0,') DATEDIT(*DMY.)
000200170726
000300170509     D/COPY GAITRASRC/SRCPROTOPI,UBWS775R
000400090521
000500090520
000600170511     C*
000700170511     C                   eval      iWS775TIP  = 'A'
000800170509     C                   eval      iWS775AAS  = *zeros
000900170511     C                   eval      iWS775RMA  = 'DTYgjHz2b_1'
001000170511     C                   eval      iWS775RMN  = 166002033416786
001100170511     C                   eval      iWS775CLI  = 1663558
001200170515     C                   eval      iWS775TLE  = 'WW'
001300170509     C                   eval      iWS775LANG = *blanks
001400170509     C*
001500170509     C                   Call      'UBWS775R'
001600170511     C                   Parm                    iWS775TIP
001700170509     C                   Parm                    iWS775AAS
001800170509     C                   Parm                    iWS775RMA
001900170509     C                   Parm                    iWS775RMN
002000170511     C                   Parm                    iWS775CLI
002100170515     C                   Parm                    iWS775TLE
002200170509     C                   Parm                    iWS775LANG
002300170509     C                   Parm                    oWS775Esito
002400170509     C                   Parm                    oWS775AAS
002500170509     C                   Parm                    oWS775LNP
002600170509     C                   Parm                    oWS775NRS
002700170509     C                   Parm                    oWS775NSP
002800170515     C                   Parm                    oWS775DSP
002900170509     C                   Parm                    oWS775CLI
003000170515     C                   Parm                    oWS775COP
003100170509     C                   Parm                    oWS775Msg
003200170515     C
003300160804     **
003400170509     C                   movel     oWS775Msg     wDiplay          52
003500160804     **
003600160804     C     wDiplay       DSPLY
003700000000     **
003800090520     C                   Seton                                        LR
003900000000     **
