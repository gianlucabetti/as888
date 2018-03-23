000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140506     ftii215d   cf   e             workstn
000300000607     D kpjba         e ds
000400140422     D OCLBL1          DS
000500140422     D*--- DATA AREA N.BOLLA /DATA BOLLA :
000600140422     D  NUMBOL                 1      6  0
000700140422     D  DATABL                 7     12  0
000800140422     d annog           s              4  0 inz(2000)
000900000607      *--------------------------------------------------------------------
001000140418     c     *entry        plist
001100140418     c                   parm                    kpjba
001200140418      *
001300140422     C     *DTAARA       DEFINE                  OCLBL1
001400140422     c                   in        oclbl1
001500140422     c                   movel     databl        anno2             2 0
001600140422     c                   add       anno2         annog
001700140422      *
001800140429     c     annog         add       1             vanno
001900140429     c                   z-add     vanno         wanno             4 0
002000100720     c                   do        *hival
002100100720     c                   exfmt     video1
002200140422     c                   setoff                                       2890
002300100720     c   kc              leave
002400140422     c                   if        vanno <= annog
002500140422     c                   eval      $msg = 'Anno immesso inferiore o uguale -
002600140422     c                             ad anno attualmente in gestione'
002700140422     c                   seton                                        2890
002800140422     c                   endif
002900140429     c  n90              if        vanno > wanno
003000140429     c                   eval      $msg = 'Anno immesso Maggiore rispetto -
003100140429     c                             a quello calcolato dalla attuale gestione'
003200140429     c                   seton                                        2890
003300140429     c                   endif
003400140422     c   90              iter
003500100720     c                   if        *inkf
003600140418     c                   movel     vanno         kpjbu
003700140422     c                   call      'TII215C'
003800140418     c                   parm                    kpjba
003900140418     c                   endif
004000100720     c                   enddo
004100000607     c                   seton                                        lr
004200000607     O*------------------------------------------------------*
