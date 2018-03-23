000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011206     F*          STATISTICA CONSUMI                        *
000300141128     F* reperisce riferimenti auto vendute                 *
000400000000     F*----------------------------------------------------*
000500141128     Fmzamh01l  if   E           K DISK
000600141203     Fmzamh02l  if   E           K DISK    rename(mzamh000:mzamh2)
000700141201     FWRKAME1l  uf a e           k disk    RENAME(MZAME000:WRKAME)
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
002700141204     d dataiso         s               d   datfmt(*iso)
002800141204     d dataeur         s               d   datfmt(*eur)
002900000000     C*---------------------------------------------------------------*
003000000000     C     *ENTRY        PLIST
003100000000     C                   PARM                    KPJBA
003200000000     C                   Z-ADD     1             CODUT
003300000000     C                   CALL      'XPARUT'
003400000000     C                   PARM                    UTEDSE
003500000000     C                   MOVEL     RAGUT         RSUT             20
003600011205     c                   movel     kpjbu         param
003700011206     C                   Z-ADD     1             AMENRF
003800141203     c     kmh2          klist
003900141203     c                   kfld                    ameTGA
004000141203     c                   kfld                    ameTGN
004100011205     c*
004200141128     c     *loval        setll     MZAMh01L
004300011205     c                   do        *hival
004400141128     c                   read      MZAMh01L                               01
004500011206     c   01              leave
004600141203     c                   if        amhtip <> 'AN'
004700141128     c                   iter
004800141128     c                   endif
004900141128     c                   movel     amhdta        annov             4 0
005000141218     c*                  if        annov <> anno and anno > 0
005100141218     c*                  iter
005200141218     c*                  endif
005300141218     c                   if        annov <  anno
005400141218     c                   iter
005500141218     c                   endif
005600141204     c                   move      amhdta        dataiso
005700141204     c                   move      dataiso       dataeur
005800141204     c                   move      dataeur       dataven           8 0
005900141204     c                   eval      ametip = 'Venduta - ' + %editc(dataven:'Y')
006000141201     c     amhnrf        chain     wrkame1l
006100141201     c                   if        %found(wrkame1l)
006200141201     c                   iter
006300141201     c                   endif
006400141128     c                   move      AMHNRF        ameNRF
006500141128     c                   move      AMHTGA        ameTGA
006600141128     c                   move      AMHTGN        ameTGN
006700141128     c                   move      AMHTLA        ameTLA
006800141128     c                   move      AMHCAU        ameCAU
006900141128     c                   move      AMHDAL        ameDAL
007000141128     c                   move      AMHCSP        ameCSP
007100141128     c                   move      AMHFIL        ameFIL
007200141128     c                   move      AMHSOC        ameSOC
007300141128     c                   move      AMHCAT        ameCAT
007400141128     c                   move      AMHCDC        ameCDC
007500141128     c                   move      AMHTEL        ameTEL
007600011206     c                   write     WRKAME
007700141203      * scrivo e cerco l'ultimo cambio autista per poterlo attribuire
007800141203     c     kmh2          setll     mzamh02l
007900141203     c                   do        *hival
008000141203     c     kmh2          reade     mzamh02l                               02
008100141203     c   02              leave
008200141203     c                   if        amhtip <> 'AU'
008300141203     c                   iter
008400141203     c                   endif
008500141204     c     amhnrf        chain     wrkame1l
008600141204     c                   if        %found(wrkame1l)
008700141203     c                   move      AMHTLA        ameTLA
008800141203     c                   move      AMHCAU        ameCAU
008900141203     c                   move      AMHDAL        ameDAL
009000141203     c                   move      AMHCSP        ameCSP
009100141203     c                   move      AMHFIL        ameFIL
009200141203     c                   move      AMHSOC        ameSOC
009300141205     c                   if        amhcat <> *blank
009400141203     c                   move      AMHCAT        ameCAT
009500141205     c                   else
009600141216     c                   move      'A1'          ameCAT
009700141205     c                   endif
009800141203     c                   move      AMHCDC        ameCDC
009900141203     c                   move      AMHTEL        ameTEL
010000141204     c                   update    WRKAME
010100141204     c                   endif
010200141203     c                   enddo
010300011206     c                   enddo
010400011206     c                   seton                                        lr
