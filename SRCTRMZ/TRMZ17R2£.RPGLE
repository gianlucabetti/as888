000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011206     F*          STATISTICA CONSUMI                        *
000300011206     F* SCRIVO RECORD DELLO STORICO PER AUTISTA            *
000400000000     F*----------------------------------------------------*
000500011206     Fmzamh01l  if   E           K DISK
000600011206     FMZAME01L  If   e           K disk
000700011206     FWRKAME0F  O    e             disk    RENAME(MZAME000:WRKAME)
000800011205     d param           ds
000900011205     d anno                           4  0
001000011205     d mese                           2  0
001100011205     d vscef                          1
001200011205     d vscec                          1
001300011205     d vscea                          1
001400011205     d vscet                          1
001500011205     d vfil                           3  0
001600141124     d vcat                          20
001700011205     d vaus                           2
001800011205     d vtga                           2
001900011205     d vtgn                           9
002000141124     d vvend                          1
002100000000     D KPJBA         E DS
002200000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002300000000     C*---------------------------------------------------------------*
002400000000     C     *ENTRY        PLIST
002500000000     C                   PARM                    KPJBA
002600000000     C                   Z-ADD     1             CODUT
002700000000     C                   CALL      'XPARUT'
002800000000     C                   PARM                    UTEDSE
002900000000     C                   MOVEL     RAGUT         RSUT             20
003000011205     c                   movel     kpjbu         param
003100011206     C                   Z-ADD     1             AMENRF
003200011205     c*
003300011206     c     AMENRF        setll     MZAME01L
003400011205     c                   do        *hival
003500011206     c                   read      MZAME01L                               01
003600011206     c   01              leave
003700011206     c     amenrf        setll     mzamh01l
003800011206     c                   do        *hival
003900011206     c     amenrf        reade     mzamh01l                               02
004000011206     c   02              leave
004100011206     c                   if        amhtip = 'AU'
004200141216     c                             and amhcau <> amecau
004300011206     c                   move      amhcau        amecau
004400011206     c                   move      amhdal        amedal
004500011206     c                   write     WRKAME
004600011206     c                   end
004700011205     c                   enddo
004800011206     c                   enddo
004900011206     c                   seton                                        lr
