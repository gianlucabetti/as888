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
002100160810     d vkmdal                         6  0
002200160810     d vkmal                          6  0
002300160810     d vcaut                          2
002400160902     d vfun                           5
002500000000     D KPJBA         E DS
002600000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002700000000     C*---------------------------------------------------------------*
002800000000     C     *ENTRY        PLIST
002900000000     C                   PARM                    KPJBA
003000000000     C                   Z-ADD     1             CODUT
003100000000     C                   CALL      'XPARUT'
003200000000     C                   PARM                    UTEDSE
003300000000     C                   MOVEL     RAGUT         RSUT             20
003400011205     c                   movel     kpjbu         param
003500011206     C                   Z-ADD     1             AMENRF
003600011205     c*
003700011206     c     AMENRF        setll     MZAME01L
003800011205     c                   do        *hival
003900011206     c                   read      MZAME01L                               01
004000011206     c   01              leave
004100011206     c     amenrf        setll     mzamh01l
004200011206     c                   do        *hival
004300011206     c     amenrf        reade     mzamh01l                               02
004400011206     c   02              leave
004500011206     c                   if        amhtip = 'AU'
004600141216     c                             and amhcau <> amecau
004700011206     c                   move      amhcau        amecau
004800011206     c                   move      amhdal        amedal
004900011206     c                   write     WRKAME
005000011206     c                   end
005100011205     c                   enddo
005200011206     c                   enddo
005300011206     c                   seton                                        lr
