000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300010510     ftnsc74d   cf   e             workstn
000400000607     D kpjba         e ds
000500000607      *--------------------------------------------------------------------
000600000607     c
000700000607     c     *entry        plist
000800000607     c                   parm                    kpjba
000900010510     c                   move      'S'           v1cpul
001000010510     c                   move      udate         v1cdat
001100000607
001200000607     c                   do        *hival
001300000607     c                   exfmt     video1
001400010511     c                   setoff                                       99
001500000607     c   kc              leave
001600010511     c     *dmy          test(d)                 v1cdat                 99
001700010511     c   99              iter
001800010510     c                   move      v1cdat        kdtjb
001900010510     c                   if        *inkf
002000010510      * pulizia file per P.C
002100010510     c                   if        v1cpul = 'S'
002200010510     c                   call      'TNSC74C1'
002300010510     c                   endif
002400010510      * lancio procedura standard
002500030326     c*                  MOVEL     'SC77'        KCOAZ
002600030326     c                   CALL      'TNSC77C'
002700010510     c                   PARM                    KPJBA
002800000607     c                   leave
002900010510     c                   endif
003000000607     c                   enddo
003100000607     c                   seton                                        lr
