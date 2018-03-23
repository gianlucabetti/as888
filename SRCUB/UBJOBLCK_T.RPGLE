000100000000     H Option( *SrcStmt )
000200000000     **-- Parameters:  -------------------------------------------------------**
000300090520     D InJobName       S             10a
000400090520     D InJobUsrName    S             10a
000500090520     D InJobNumber     S              6a
000600090520     D InOption        S             10a
000700090520     D InFilterLib     S             10a
000800090520     D InFilterFil     S             10a
000900090521     D InFilterMbr     S             10a
001000090521     D InFilterTyp     S             10a
001100090520     D InFilterLck     S             10a
001200090521     D OutListAlloc    s                   Dim(256) like(UBJOBLCKDS)
001300090520     D OutErrorCod     S             10i 0
001400090520     D OutErrorMsg     S             80a
001500000000     **
001600090521     D ubjoblckds    E DS                  Qualified
001700090521     **
001800090521
001900090520
002000090520     C*
002100090521     C                   Eval      InOption    = '*dealloc'
002200090521     C                   Eval      InFilterFil = ''
002300090521     C                   Eval      InFilterTyp = '*QDDS*'
002400090521     C                   Eval      InFilterLck = '*excl*'
002500090520     C*
002600090521     C                   Call      'UBJOBLCK'
002700090521     C                   Parm                    InJobName
002800090521     C                   Parm                    InJobUsrName
002900090521     C                   Parm                    InJobNumber
003000090521     C                   Parm                    InOption
003100090521     C                   Parm                    InFilterLib
003200090521     C                   Parm                    InFilterFil
003300090521     C                   Parm                    InFilterMbr
003400090521     C                   Parm                    InFilterTyp
003500090521     C                   Parm                    InFilterLck
003600090521     C                   Parm                    OutListAlloc
003700090521     C                   Parm                    OutErrorCod
003800090521     C                   Parm                    OutErrorMsg
003900000000     **
004000090520     C                   Seton                                        LR
004100000000     **
