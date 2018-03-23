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
002100000000     D KPJBA         E DS
002200000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002300141204     d dataiso         s               d   datfmt(*iso)
002400141204     d dataeur         s               d   datfmt(*eur)
002500000000     C*---------------------------------------------------------------*
002600000000     C     *ENTRY        PLIST
002700000000     C                   PARM                    KPJBA
002800000000     C                   Z-ADD     1             CODUT
002900000000     C                   CALL      'XPARUT'
003000000000     C                   PARM                    UTEDSE
003100000000     C                   MOVEL     RAGUT         RSUT             20
003200011205     c                   movel     kpjbu         param
003300011206     C                   Z-ADD     1             AMENRF
003400141203     c     kmh2          klist
003500141203     c                   kfld                    ameTGA
003600141203     c                   kfld                    ameTGN
003700011205     c*
003800141128     c     *loval        setll     MZAMh01L
003900011205     c                   do        *hival
004000141128     c                   read      MZAMh01L                               01
004100011206     c   01              leave
004200141203     c                   if        amhtip <> 'AN'
004300141128     c                   iter
004400141128     c                   endif
004500141128     c                   movel     amhdta        annov             4 0
004600141218     c*                  if        annov <> anno and anno > 0
004700141218     c*                  iter
004800141218     c*                  endif
004900141218     c                   if        annov <  anno
005000141218     c                   iter
005100141218     c                   endif
005200141204     c                   move      amhdta        dataiso
005300141204     c                   move      dataiso       dataeur
005400141204     c                   move      dataeur       dataven           8 0
005500141204     c                   eval      ametip = 'Venduta - ' + %editc(dataven:'Y')
005600141201     c     amhnrf        chain     wrkame1l
005700141201     c                   if        %found(wrkame1l)
005800141201     c                   iter
005900141201     c                   endif
006000141128     c                   move      AMHNRF        ameNRF
006100141128     c                   move      AMHTGA        ameTGA
006200141128     c                   move      AMHTGN        ameTGN
006300141128     c                   move      AMHTLA        ameTLA
006400141128     c                   move      AMHCAU        ameCAU
006500141128     c                   move      AMHDAL        ameDAL
006600141128     c                   move      AMHCSP        ameCSP
006700141128     c                   move      AMHFIL        ameFIL
006800141128     c                   move      AMHSOC        ameSOC
006900141128     c                   move      AMHCAT        ameCAT
007000141128     c                   move      AMHCDC        ameCDC
007100141128     c                   move      AMHTEL        ameTEL
007200011206     c                   write     WRKAME
007300141203      * scrivo e cerco l'ultimo cambio autista per poterlo attribuire
007400141203     c     kmh2          setll     mzamh02l
007500141203     c                   do        *hival
007600141203     c     kmh2          reade     mzamh02l                               02
007700141203     c   02              leave
007800141203     c                   if        amhtip <> 'AU'
007900141203     c                   iter
008000141203     c                   endif
008100141204     c     amhnrf        chain     wrkame1l
008200141204     c                   if        %found(wrkame1l)
008300141203     c                   move      AMHTLA        ameTLA
008400141203     c                   move      AMHCAU        ameCAU
008500141203     c                   move      AMHDAL        ameDAL
008600141203     c                   move      AMHCSP        ameCSP
008700141203     c                   move      AMHFIL        ameFIL
008800141203     c                   move      AMHSOC        ameSOC
008900141205     c                   if        amhcat <> *blank
009000141203     c                   move      AMHCAT        ameCAT
009100141205     c                   else
009200141216     c                   move      'A1'          ameCAT
009300141205     c                   endif
009400141203     c                   move      AMHCDC        ameCDC
009500141203     c                   move      AMHTEL        ameTEL
009600141204     c                   update    WRKAME
009700141204     c                   endif
009800141203     c                   enddo
009900011206     c                   enddo
010000011206     c                   seton                                        lr
