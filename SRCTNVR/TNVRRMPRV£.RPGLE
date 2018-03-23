000100940224     H*--------------------------------------------------------------*
000200070110     Ffibrv00f  iF   E             DISK
000300070110     Ffnbrv00f  o  a E             DISK
000400070110     Ifibrv000
000500070110     I              brvhcs                      brvhcs4
000600070110     I              brvhms                      brvhms4
000700060928
000800900131     C*---------------------------------------------------------------*
000900070110     c                   do        *hival
001000070110     c                   read      fibrv00f
001100070110     c                   if        %eof(fibrv00f)
001200070110     c                   leave
001300070110     c                   endif
001400070110     c* pulizia campi nuovi
001500070110     c                   clear                   brvhcs
001600070110     c                   clear                   brvhms
001700070110     c                   clear                   brvtsu
001800070110     c                   clear                   brvpru
001900070110     c                   clear                   brvmis
002000070110     c                   clear                   brvtap
002100070110     c*
002200070110     c                   movel     brvatb        brvnvr
002300070110     c                   movel     brvhcs4       brvhcs
002400070110     c                   movel     brvhms4       brvhms
002500070110     c                   write     fnbrv000
002600070110     c                   enddo
002700030205      *
002800000000     C                   SETON                                        LR
