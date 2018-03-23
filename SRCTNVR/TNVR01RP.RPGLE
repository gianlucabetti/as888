000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300111215     H* pulizia archivi offerte particolari  d = AC BASE con importi a zero    *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600030708     FTIOPT01L  Uf   e           K DISK    rename(titpt000:tiopt000)prefix(O)
000700030708     FTIOPD01L  Uf   e           K DISK    rename(titpd000:tiopd000)prefix(O)
000800030708     fsysprt    o    f  132        printer
000900111215
001000111215     c     KTPT          klist
001100111215     c                   kfld                    Otpdksc
001200111215     c                   kfld                    Otpdctr
001300111215     c                   kfld                    Otpdprg
001400111215     c                   kfld                    Otpdftc
001500030708      *
001600030708      *
001700111215      * TIOPD
001800030708      *
001900030708     C     *loval        setll     tiopd01l
002000030708     c                   do        *hival
002100030708     c                   read      tiopd01l
002200030708     c                   if        %eof
002300030708     c                   leave
002400030708     c                   endif
002500111215     c                   if        Otpdftc = 'd '  and Otpditr = 0
002600111215     c*                  delete    tiopd000
002700030708     c                   except    dettaod
002800111215      *
002900111215      *
003000111215      * TIOPT
003100111215      *
003200111215     c     Ktpt          Chain     tiopt01l
003300111215     c                   if        %eof(tiopt01l)
003400111215     c                   iter
003500111215     c                   endif
003600111215     c*                  delete    tiopt000
003700111215     c                   except    dettaot
003800030708     c                   endif
003900030708     c                   enddo
004000030708      *
004100030708     c                   seton                                        lr
004200111215     osysprt    e            dettaot
004300030708     o                                         +  1 'opt'
004400030708     o                       otptksc           +  1
004500030708     o                       otptctr           +  1
004600030708     o                       otptprg           +  1
004700030708     o                       otptftc           +  1
004800030708     o          e            dettaod
004900030708     o                                         +  1 'opd'
005000030708     o                       otpdksc           +  1
005100030708     o                       otpdctr           +  1
005200030708     o                       otpdprg           +  1
005300030708     o                       otpdftc           +  1
