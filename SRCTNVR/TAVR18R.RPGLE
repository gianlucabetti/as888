000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300030708     H* pulizia archivi tariffe/offerte particolari  da ftc = §                *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600030708     FTITPT01L  Uf   e           K DISK
000700030708     FTITPD01L  Uf   e           K DISK
000800030708     FTIOPT01L  Uf   e           K DISK    rename(titpt000:tiopt000)prefix(O)
000900030708     FTIOPD01L  Uf   e           K DISK    rename(titpd000:tiopd000)prefix(O)
001000030708     f
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
002200030708     c                   update    titpt000
002300030708     c                   endif
002400030708     c                   enddo
002500990614      *
002600030708      *
002700030708      * TITPD
002800030708      *
002900030708     C     *loval        setll     titpd01l
003000030708     c                   do        *hival
003100030708     c                   read      titpd01l
003200030708     c                   if        %eof(titpd01l)
003300030708     c                   leave
003400030708     c                   endif
003500030708     c                   if        tpdftc = '§ '  and tpdatb = ' '
003600030708     c                   eval      tpdatb = 'A'
003700030708     c                   update    titpd000
003800030708     c                   endif
003900030708     c                   enddo
004000990614      *
004100030708      *
004200030708      * TIOPT
004300030708      *
004400030708     C     *loval        setll     tiopt01l
004500030708     c                   do        *hival
004600030708     c                   read      tiopt01l
004700030708     c                   if        %eof(tiopt01l)
004800030708     c                   leave
004900030708     c                   endif
005000030708     c                   if        Otptftc = '§ '  and Otptatb = ' '
005100030708     c                   eval      Otptatb = 'A'
005200030708     c                   update    tiopt000
005300030708     c                   endif
005400030708     c                   enddo
005500030708      *
005600030708      *
005700030708      * TITPD
005800030708      *
005900030708     C     *loval        setll     tiopd01l
006000030708     c                   do        *hival
006100030708     c                   read      tiopd01l
006200030708     c                   if        %eof(tiopd01l)
006300030708     c                   leave
006400030708     c                   endif
006500030708     c                   if        Otpdftc = '§ '  and Otpdatb = ' '
006600030708     c                   eval      Otpdatb = 'A'
006700030708     c                   update    tiopd000
006800030708     c                   endif
006900030708     c                   enddo
007000030708      *
007100030708     c                   seton                                        lr
