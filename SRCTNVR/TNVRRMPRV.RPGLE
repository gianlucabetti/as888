000100940224     H*--------------------------------------------------------------*
000200070110     Ffibrv00f  iF   E             DISK
000300070110     Ffnbrv00f  o  a E             DISK
000400070124     d nrr1            s             10  0
000500070110     Ifibrv000
000600070110     I              brvhcs                      brvhcs4
000700070110     I              brvhms                      brvhms4
000800060928
000900900131     C*---------------------------------------------------------------*
001000070124     c     *entry        plist
001100070124     c                   parm                    nrr              10
001200070124     c                   move      nrr           nrr1
001300070124     c*
001400070124     c                   sub       1             nrr1
001500070124     c     nrr1          chain     fibrv00f
001600070110     c                   do        *hival
001700070110     c                   read      fibrv00f
001800070110     c                   if        %eof(fibrv00f)
001900070110     c                   leave
002000070110     c                   endif
002100070110     c* pulizia campi nuovi
002200070110     c                   clear                   brvhcs
002300070110     c                   clear                   brvhms
002400070110     c                   clear                   brvtsu
002500070110     c                   clear                   brvpru
002600070110     c                   clear                   brvmis
002700070110     c                   clear                   brvtap
002800070110     c*
002900070110     c                   movel     brvatb        brvnvr
003000070110     c                   movel     brvhcs4       brvhcs
003100070110     c                   movel     brvhms4       brvhms
003200070110     c                   write     fnbrv000
003300070110     c                   enddo
003400030205      *
003500000000     C                   SETON                                        LR
