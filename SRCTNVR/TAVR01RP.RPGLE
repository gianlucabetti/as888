000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300030708     H* pulizia archivi tariffe/offerte particolari  da ftc = §                *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600030708     FTITPT01L  Uf   e           K DISK
000700030708     FTITPD01L  Uf   e           K DISK
000800030708     FTIOPT01L  Uf   e           K DISK    rename(titpt000:tiopt000)prefix(O)
000900030708     FTIOPD01L  Uf   e           K DISK    rename(titpd000:tiopd000)prefix(O)
001000030708     fsysprt    o    f  132        printer
001100990614      *
001200030708      * TITPT
001300990823      *
001400030708     C     *loval        setll     titpt01l
001500030708     c                   do        *hival
001600030708     c                   read      titpt01l
001700030708     c                   if        %eof(titpt01l)
001800030708     c                   leave
001900030708     c                   endif
002000030708     c                   if        tptftc = '§ '  and tptatb = ' '
002100030708     c                   eval      tptatb = 'A'
002200030708     c*                  update    titpt000
002300030708     c                   except    dettat
002400030708     c                   endif
002500030708     c                   enddo
002600990614      *
002700030708      *
002800030708      * TITPD
002900030708      *
003000030708     C     *loval        setll     titpd01l
003100030708     c                   do        *hival
003200030708     c                   read      titpd01l
003300030708     c                   if        %eof(titpd01l)
003400030708     c                   leave
003500030708     c                   endif
003600030708     c                   if        tpdftc = '§ '  and tpdatb = ' '
003700030708     c                   eval      tpdatb = 'A'
003800030708     c*                  update    titpd000
003900030708     c                   except    dettad
004000030708     c                   endif
004100030708     c                   enddo
004200990614      *
004300030708      *
004400030708      * TIOPT
004500030708      *
004600030708     C     *loval        setll     tiopt01l
004700030708     c                   do        *hival
004800030708     c                   read      tiopt01l
004900030708     c                   if        %eof(tiopt01l)
005000030708     c                   leave
005100030708     c                   endif
005200030708     c                   if        Otptftc = '§ '  and Otptatb = ' '
005300030708     c                   eval      Otptatb = 'A'
005400030708     c*                  update    tiopt000
005500030708     c                   except    dettaot
005600030708     c                   endif
005700030708     c                   enddo
005800030708      *
005900030708      *
006000030708      * TITPD
006100030708      *
006200030708     C     *loval        setll     tiopd01l
006300030708     c                   do        *hival
006400030708     c                   read      tiopd01l
006500030708     c                   if        %eof
006600030708     c                   leave
006700030708     c                   endif
006800030708     c                   if        Otpdftc = '§ '  and Otpdatb = ' '
006900030708     c                   eval      Otpdatb = 'A'
007000030708     c*                  update    tiopd000
007100030708     c                   except    dettaod
007200030708     c                   endif
007300030708     c                   enddo
007400030708      *
007500030708     c                   seton                                        lr
007600030708     osysprt    e            dettat
007700030708     o                                         +  1 'tpt'
007800030708     o                       tptksc            +  1
007900030708     o                       tptctr            +  1
008000030708     o                       tptprg            +  1
008100030708     o                       tptftc            +  1
008200030708     o          e            dettad
008300030708     o                                         +  1 'tpd'
008400030708     o                       tpdksc            +  1
008500030708     o                       tpdctr            +  1
008600030708     o                       tpdprg            +  1
008700030708     o                       tpdftc            +  1
008800030708     o          e            dettaot
008900030708     o                                         +  1 'opt'
009000030708     o                       otptksc           +  1
009100030708     o                       otptctr           +  1
009200030708     o                       otptprg           +  1
009300030708     o                       otptftc           +  1
009400030708     o          e            dettaod
009500030708     o                                         +  1 'opd'
009600030708     o                       otpdksc           +  1
009700030708     o                       otpdctr           +  1
009800030708     o                       otpdprg           +  1
009900030708     o                       otpdftc           +  1
