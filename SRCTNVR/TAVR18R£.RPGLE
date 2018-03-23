000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300990614     H* PREP. ARCHIVIO DETTAGLIO TARIFFE PARTICOLARE                           *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600990614     FTNTPD00f  ip   e             DISK
000700990823     FTITPD01l  uf a e           k DISK
000800990614     f                                     prefix(o)
000900990614      *
001000990823      *
001100990823     c     keytpd        klist
001200990823     c                   kfld                    otpdksc
001300990823     c                   kfld                    otpdctr
001400990823     c                   kfld                    otpdprg
001500990823     c                   kfld                    otpdftc
001600990823     c                   kfld                    otpdorp
001700990823     c                   kfld                    otpdsgl
001800990823      *
001900990614     c                   clear                   tiTPD000
002000990614      *
002100990614     c                   eval      oTPDksc = TPDksc
002200990614     c                   eval      oTPDctr = TPDctr
002300990614     c                   eval      oTPDprg = TPDprg
002400990614     c                   movel     tpdftc        otpdftc
002500990729     c                   move      TPDorp        oTPDorp
002600990614     c                   eval      oTPDsgl = TPDsgl
002700990823     c* controllo se esiste il record
002800990823     c     keytpd        chain     titpd01l                           30
002900990823     c                   if        *in30
003000990823     c                   eval      oTPDatb = TPDatb
003100990823     c                   eval      oTPDcts = TPDcts
003200990614     c                   eval      oTPDitr = TPDitr
003300990614     c                   z-add     TPDmin        oTPDmin
003400990614     c                   z-add     TPDmax        oTPDmax
003500990614     c                   eval      oTPDain = TPDain
003600990614      * se flag trasmissione uguale a 'T' lo pulisco e trasmetto
003700990614     c                   if        TPDftr = 'T'
003800990614     c                   else
003900990614     c                   eval      oTPDftr = TPDftr
004000990614     c                   endif
004100990614      *
004200990614     c                   eval      oTPDdtr = TPDdtr
004300990614      *
004400990614     c                   write     tiTPD000
004500990823      *
004600990823     c                   endif
004700990614      *
