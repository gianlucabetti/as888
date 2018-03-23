000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300130430     FFNfgv01L  iF   E           K DISK
000400130430     FFNfgs01L  uF   E           K DISK
000500130430     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600021214      *--------------------------------------------------
000700130430     c     *loval        setll     fnfgs01l
000800130430     c                   read      fnfgs01l
000900051110
001000130430    1c                   DOW       not %EOF(FNfgs01l)
001100130430     c     kfgv          chain     fnfgv01l                           90
001200130430     c                   if        *in90
001300130430     c                   except    manca
001400130430     c                   delete    fnfgs000
001500130430     c                   add       1             conta             7 0
001600051110     c                   endif
001700051110     c
001800130430     c                   read      fnfgs01l
001900051110     c                   enddo
002000021216      *
002100130430     c                   except    fine
002200021214     C                   eval       *inlr = *on
002300021214      *---------------------------------------------------------
002400021214     c     *inzsr        BEGSR
002500021214      *
002600130430     C     Kfgv          KLIST
002700130430     C                   KFLD                    fgsnfv
002800130430     C                   KFLD                    fgslnp
002900021216     C*
003000021214     C                   ENDSR
003100130430     OQSYSPRT   E            manca       1
003200130430     O                       fgslnp
003300130430     O                       fgsnfv        z  +   1
003400130430     O                       fgsdgs           +   1 '    /  /  '
003500130430     OQSYSPRT   E            fine        1
003600130430     O                       conta         2
