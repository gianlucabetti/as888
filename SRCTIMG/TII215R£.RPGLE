000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140418     fmagad     cf   e             workstn
000300000607     D kpjba         e ds
000400000607      *--------------------------------------------------------------------
000500140418     c     *entry        plist
000600140418     c                   parm                    kpjba
000700140418      *
000800140418     c                   move      *year         vanno
000900100720     c                   do        *hival
001000100720     c                   exfmt     video1
001100100720     c
001200100720     c   kc              leave
001300100720     c                   if        *inkf
001400140418     c                   movel     vanno         kpjbu
001500140418     c                   call      'MAGACL'
001600140418     c                   parm                    kpjba
001700140418     c                   endif
001800100720     c                   enddo
001900000607     c                   seton                                        lr
002000000607     O*------------------------------------------------------*
