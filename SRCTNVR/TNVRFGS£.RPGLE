000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300051110     FFNblp01L  iF   E           K DISK
000400051110     FFNblt01L  iF   E           K DISK
000500051110     F                                     INFDS(blt1)
000600051110     FFNblt00f  uF   E             DISK    rename(fnblt000:fnbltfis)
000700051110     Ffnbltwrk  o  a E             DISK    rename(fnblt000:fnblt00W)
000800051110     D blt1            DS
000900051110     D  bltNRR               397    400B 0
001000021214      *--------------------------------------------------
001100051110     c     *loval        setll     fnblt01l
001200051110     c                   read      fnblt01l
001300051110
001400051110    1c                   DOW       not %EOF(FNblt01l)
001500051110     c     kblt          chain     fnblp01l                           90
001600051110     c                   if        *in90
001700051110     c                   write     fnblt00w
001800051110     c
001900051110     c     bltnrr        delete    fnbltfis
002000051110     c                   endif
002100051110     c
002200051110     c                   read      fnblt01l
002300051110     c                   enddo
002400021216      *
002500021214     C                   eval       *inlr = *on
002600021214      *---------------------------------------------------------
002700021214     c     *inzsr        BEGSR
002800021214      *
002900051110     C     Kblt          KLIST
003000051110     C                   KFLD                    bltaas
003100051110     C                   KFLD                    bltlnp
003200051110     C                   KFLD                    bltnrs
003300051110     C                   KFLD                    bltnsp
003400021216     C*
003500021214     C                   ENDSR
