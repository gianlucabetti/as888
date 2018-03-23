000100011116     H DECEDIT('0,') DATEDIT(*dmy.)  DATFMT(*ISO)
000200050124      *_____________________________________________________________________
000300050124      *  Compilare con
000400050124      * OVRDBF     FILE(WWOBJ) TOFILE(QSYS/QADSPOBJ)
000500050124      *_____________________________________________________________________
000600050124     fWWOBJ     IP   e             DISK
000700050121     fFIINV00F  o  a e             DISK
000800011116
000900050124     c     *entry        PLIST
001000050124     C                   PARM                    ILNM             10
001100050124     C                   PARM                    ILTX             50
001200050124     C                   PARM                    ILUC             10
001300050124
001400050124     c                   movel     Ilnm          INVLNM
001500050124     c                   movel     Iltx          INVLTX
001600050124     c                   movel     Iluc          INVLUC
001700050124     c                   movel     ODObnm        INVONM
001800050124     c                   movel     ODObtp        INVOTI
001900050124     c                   movel     ODObat        INVOAT
002000050124     c                   movel     ODObtx        INVOTX
002100050124     c                   movel     ODCrtu        INVOUC
002200050124
002300050124     c                   write     FIINV000
