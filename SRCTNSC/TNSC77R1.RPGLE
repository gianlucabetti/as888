000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200030521     ftnsc77d   cf   e             workstn
000300000607     D kpjba         e ds
000400000607      *--------------------------------------------------------------------
000500000607     c
000600000607     c     *entry        plist
000700000607     c                   parm                    kpjba
000800010510     c                   move      'S'           v1cpul
000900010510     c                   move      udate         v1cdat
001000000607
001100000607     c                   do        *hival
001200000607     c                   exfmt     video1
001300010511     c                   setoff                                       99
001400000607     c   kc              leave
001500010511     c     *dmy          test(d)                 v1cdat                 99
001600010511     c   99              iter
001700010510     c                   move      v1cdat        kdtjb
001800010510     c                   if        *inkf
001900010510      * pulizia file per P.C
002000010510     c                   if        v1cpul = 'S'
002100010510     c                   call      'TNSC74C1'
002200010510     c                   endif
002300010510      * lancio procedura standard
002400030527     c                   MOVEL     'SC77'        KCOAZ
002500030527     c                   CALL      'BCH10'
002600010510     c                   PARM                    KPJBA
002700000607     c                   leave
002800010510     c                   endif
002900000607     c                   enddo
003000000607     c                   seton                                        lr
