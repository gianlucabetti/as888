000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300990614     H* PREP. ARCHIVIO tariffe giacenze                                        *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600990614     FTNtgc00f  ip   e             DISK
000700990823     FTItgc01l  uf a e           k DISK
000800990614     f                                     prefix(o)
000900990614      *
001000990823     c     keytgc        klist
001100990823     c                   kfld                    otgcksc
001200990823     c                   kfld                    otgcctr
001300990823     c                   kfld                    otgcprg
001400990823      *
001500990614     c                   clear                   titgc000
001600990614      *
001700990614     c                   eval      otgcksc = tgcksc
001800990614     c                   eval      otgcctr = tgcctr
001900990614     c                   eval      otgcprg = tgcprg
002000990823     c* controllo se esiste il record
002100990823     c     keytgc        chain     titgc01l                           30
002200990823     c                   if        *in30
002300990823     c                   eval      otgcatb = tgcatb
002400990614     c                   z-add     tgcsgv        otgcsgv
002500990614     c                   z-add     tgcsgr        otgcsgr
002600990614     c                   z-add     tgcsgp        otgcsgp
002700990614     c                   z-add     tgcsgd        otgcsgd
002800990614     c                   eval      otgcsg1 = tgcsg1
002900990614     c                   eval      otgcsg2 = tgcsg2
003000990614     c                   eval      otgcsg3 = tgcsg3
003100990614     c                   z-add     tgcst1        otgcst1
003200990614     c                   z-add     tgcst2        otgcst2
003300990614     c                   z-add     tgcst3        otgcst3
003400990614     c                   z-add     tgcstm        otgcstm
003500990614     c                   eval      otgcrpv = tgcrpv
003600990614     c                   eval      otgcfaf = tgcfaf
003700990614     c                   eval      otgcsgf = tgcsgf
003800990614     c                   eval      otgcggr = tgcggr
003900990614     c                   eval      otgctcm = tgctcm
004000990614     c                   eval      otgctfg = tgctfg
004100990614      * se flag trasmissione uguale a 'T' lo pulisco e trasmetto
004200990614     c                   if        tgcftr = 'T'
004300990614     c                   else
004400990614     c                   eval      otgcftr = tgcftr
004500990614     c                   endif
004600990614      *
004700990614     c                   eval      otgcdtr = tgcdtr
004800990614      *
004900990614     c                   write     titgc000
005000990614      *
005100990823     c                   endif
