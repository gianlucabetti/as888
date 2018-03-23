000100000000     H Option( *SrcStmt )
000200110614
000300100514     D In_TypENV       S              2A
000400100514     D In_TypEXE       S              1A
000500090811     D RTNesito        S             10I 0
000600100503     D RTNopcode       S             10A
000700090811     D RTNstatus       S             10I 0
000800110614      *
000900110614     d wTime_Str       s              6  0 inz
001000110614     d wTime_End       s              6  0 inz
001100110614     d wText           s             50    inz
001200090521
001300100514     C                   Eval      In_TypENV = 'EW'
001400100518     C                   Eval      In_TypEXE = '1'
001500110614     c                   time                    wTime_Str
001600110614     **
001700100514     C                   Call      'FISPE04R'
001800100514     C                   Parm                    In_TypENV
001900100514     C                   Parm                    In_TypEXE
002000100514     C                   Parm                    rtnesito
002100100514     C                   Parm                    rtnopcode
002200100514     C                   Parm                    rtnstatus
002300000000     **
002400110614     c                   time                    wTime_End
002500110614     c                   eval      wText = 'Start Time: ' +
002600110614     c                                     %editw( wTime_Str : '0 :  :  ' )
002700110614     C     wText         Dsply
002800110614     c                   eval      wText = 'End Time:   ' +
002900110614     c                                     %editw( wTime_End : '0 :  :  ' )
003000110614     C     wText         Dsply
003100100407     C     rtnesito      Dsply
003200100407     C     rtnopcode     Dsply
003300100407     C     rtnstatus     Dsply
003400100407     **
003500090520     C                   Seton                                        LR
003600000000     **
