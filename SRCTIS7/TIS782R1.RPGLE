000100120521     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200990907
000300010524
000400120521       // Parametri ricevuti?
000500120521     d pInLib          s             10
000600120521     d pInObjType      s              7
000700120521     d pInObj          s             10
000800120521     d pInDate         s              8s 0
000900120521     d pInDateType     s              1
001000120521     d pInMode         s              1
001100120521     d pOutError       s              2
001200120521     d pOutErrorD      s             70
001300120521
001400010525
001500010525
001600010525     C*-----------------------------------------------------------------------------
001700010525     C*
001800120531     C                   eval      pInLib      = 'VASUPLOAD'
001900120521     C                   eval      pInObjType  = '*FILE'
002000120521     C                   eval      pInObj      = '*ALL'
002100120522     C                   eval      pInDate     = %dec(%date()-%days(1))
002200120521     C                   eval      pInDateType = '1'
002300120521     C                   eval      pInMode     = '1'
002400050920     C*
002500050920     C* Elaboro il record corrente
002600120521     C                   call(e)   'TIS785R'
002700120521     C                   parm                    pInLib
002800120521     C                   parm                    pInObjType
002900120521     C                   parm                    pInObj
003000120521     C                   parm                    pInDate
003100120521     C                   parm                    pInDateType
003200120521     C                   parm                    pInMode
003300120521     C                   parm      *blanks       pOutError
003400120521     C                   parm      *blanks       pOutErrorD
003500100624     C*
003600120521     C                   seton                                        lr
003700120531     C*
