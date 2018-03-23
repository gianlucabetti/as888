000100040128     D data_iso        s               d   datfmt(*iso)
000200040128     D data2           s               d   datfmt(*iso)
000300040128      *
000400000509     C     *ENTRY        PLIST
000500040128     C                   PARM                    File             10
000600040128     C                   PARM                    Membro           10
000700040128     C                   PARM                    Giorno           13
000800040128     C                   PARM                    Anno2             2
000900011123      *
001000040128     c                   movel     giorno        data7             7
001100040128     c                   move      giorno        orario            6
001200040128     c                   move      data7         data8             8
001300040128     c                   movel     '20'          anno
001400040128     c                   move      anno2         anno              4
001500040128     c                   movel     anno          data8
001600040128     c                   movel     data8         data80            8 0
001700011123      *
001800040128     c                   move      data80        data_iso
001900040128     c     data_iso      subdur    15:*d         data2
002000040128      *
002100040128     C                   IF        FILE = 'EDIFTSTAT '
002200040128     C/EXEC SQL
002300040128     C+ DELETE   FROM ediftstas  where
002400040128     C+ Date(timestamp(substr( EDIFTSTAS , 1981, 14))) <  :DATA2
002500040128     C/END-EXEC
002600040128     C                   END
002700040128      *
002800040128     C                   IF        FILE = 'EDIFCSUMT '
002900040128     C/EXEC SQL
003000040128     C+ DELETE   FROM edifcsums  where
003100040128     C+ Date(timestamp(substr( EDIFCSUMS , 1981, 14))) <  :DATA2
003200040128     C/END-EXEC
003300040128     C                   END
003400040128      *
003500011123     C                   Seton                                        LR
