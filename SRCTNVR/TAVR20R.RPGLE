000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300030707     H* PREP. ARCHIVIO TESTATE   TARIFFE PARTICOLARE                           *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600030707     FTioptdel  ip   e             DISK    prefix(O) rename(titpt000:tipot)
000700030707     FTIopt01l  uf a e           k DISK
000800030707     FTIopd01l  uf a e           k DISK
000900990614      *
001000990823      *
001100030707     c     keytpd        klist
001200030707     c                   kfld                    otptksc
001300030707     c                   kfld                    otptctr
001400030707     c                   kfld                    otptprg
001500030707     c                   kfld                    otptftc
001600990823      *
001700030707     c     keytpd        chain     tiopd01l
001800030707      * se esiste
001900030707     c                   if        %found(tiopd01l) and tpdatb = ' '
002000030707      * verifico se esiste in tiopt
002100030707     c     keytpd        chain     tiopt01l
002200030707     c                   if        not %found(tiopt01l)
002300990614      *
002400030707     c                   eval      TPTksc = OTPTksc
002500030707     c                   eval      TPTctr = OTPTctr
002600030707     c                   eval      TPTprg = OTPTprg
002700030707     c                   eval      TPTduv = OTPTduv
002800030707     c                   eval      TPTftc = OTPTftc
002900030707     c                   eval      TPTtpg = OTPTtpg
003000030707     c                   eval      TPTarl = OTPTarl
003100030707     c                   eval      TPTarf = OTPTarf
003200030707     c                   eval      TPTaro = OTPTaro
003300030707     c                   eval      TPTrpv = OTPTrpv
003400030707     c                   eval      TPTvlm = OTPTvlm
003500030707     c                   eval      TPTvvm = OTPTvvm
003600030707     c                   eval      TPTfvm = OTPTfvm
003700030707     c                   eval      TPTtma = OTPTtma
003800030707     c                   eval      TPTapl = OTPTapl
003900030707     c                   eval      TPTftr = OTPTftr
004000030707     c                   eval      TPTdtr = OTPTdtr
004100030707     c                   eval      TPTatb = ' '
004200990614      *
004300030707     c                   write     tiTPT000
004400990823      *
004500990823     c                   endif
004600990614      *
004700030707     c                   endif
004800030707      *
