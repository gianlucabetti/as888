000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300051111     FFNarb01L  iF   E           K DISK
000400051111     FFNar401L  iF   E           K DISK
000500051111     F                                     INFDS(ar41)
000600051111     FFNar400f  uF   E             DISK    rename(fnar4000:fnar4fis)
000700051111     Ffnar4wrk  o  a E             DISK    rename(fnar4000:fnar400W)
000800051111     D ar41            DS
000900051111     D  ar4NRR               397    400B 0
001000021214      *--------------------------------------------------
001100051111     c     *loval        setll     fnar401l
001200051111     c                   read      fnar401l
001300051110
001400051111    1c                   DOW       not %EOF(FNar401l)
001500051111     c                   if        ar4nrr<7473484
001600051111     c     kblt          chain     fnarb01l                           90
001700051110     c                   if        *in90
001800051111     c                   write     fnar400w
001900051110     c
002000051111     c     ar4nrr        delete    fnar4fis
002100051110     c                   endif
002200051110     c                   endif
002300051110     c
002400051111     c                   read      fnar401l
002500051110     c                   enddo
002600021216      *
002700021214     C                   eval       *inlr = *on
002800021214      *---------------------------------------------------------
002900021214     c     *inzsr        BEGSR
003000021214      *
003100051110     C     Kblt          KLIST
003200051111     C                   KFLD                    ar4aas
003300051111     C                   KFLD                    ar4lnp
003400051111     C                   KFLD                    ar4nrs
003500051111     C                   KFLD                    ar4nsp
003600021216     C*
003700021214     C                   ENDSR
