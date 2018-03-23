000100040310     H DECEDIT('0,') DATEDIT(*YMD.)
000200040128     D data_iso        s               d   datfmt(*iso)
000300040128     D data2           s               d   datfmt(*iso)
000400011123      *
000500040310     c                   move      *date         data80            8 0
000600011123      *
000700040128     c                   move      data80        data_iso
000800040128     c     data_iso      subdur    15:*d         data2
000900040310     c                   move      data2         data_8            8 0
001000040128      *
001100040128     C/EXEC SQL
001200040310     C+ DELETE   FROM fifcew0s  where  FCSDDC <  :DATA_8
001300040128     C/END-EXEC
001400040128      *
001500011123     C                   Seton                                        LR
