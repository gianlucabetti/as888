000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060207     F*---------------------------------------------------------------------------------
000300060207     fWxftdarb04if   e             disk    rename(WXFTDARB04:WXFTD)
000400060207     f                                     prefix(wx_)
000500060207     F*---------------------------------------------------------------------------------
000600060207     ffiftd05l  uf   e           k disk
000700030429     F*---------------------------------------------------------------------------------
000800060207      ***
000900060207     C                   read      wxftdarb04
001000060207      ***
001100060207     c                   dow       not %EoF(wxftdarb04)
001200060207      ***
001300060207      * ?     x bolla
001400060207     C     Kftd          chain     Fiftd05l
001500060207     c                   if        %Found(Fiftd05l)
001600060207      *
001700060207     c                   eval      ftdatb = ' '
001800060207     c                   if        ftdsic = ' '
001900060207     c                   eval      ftdncl = wx_arbncl
002000060207     c                   eval      ftdpkl = wx_wpes
002100060207     c                   eval      ftdvlu = wx_wvol
002200060207     c                   end
002300060207     c                   eval      ftdtmc = wx_wtmc
002400060207     c                   eval      ftdcmc = wx_wcmc
002500060207     c                   select
002600060207     c                   when      wx_wcmc = 'RDC' or
002700060207     c                             wx_wcmc = 'NIC' or
002800060207     c                             wx_wcmc = 'R'
002900060207     c                   clear                   ftdtmc
003000060207     c                   clear                   ftdcmc
003100060207     c                   when      wx_wcmc = 'A16'
003200060207     c                   eval      ftdcmc = '016'
003300060207     c                   when      wx_wcmc = 'A23'
003400060207     c                   eval      ftdcmc = '023'
003500060207     c                   when      wx_wcmc = 'AV2'
003600060207     c                   eval      ftdcmc = 'AVV'
003700060207     c                   endsl
003800060207     c                   if        wx_arbdcm <> 0
003900060207     c                   eval      ftdstp = wx_arbstp
004000060207     c                   end
004100060207     c                   if        wx_wcmc = ' '
004200060207     c                   eval      ftddcm = wx_arbdcm
004300060207     c                   eval      ftdhmc = wx_arbhmc
004400060207     c                   end
004500060207     c                   eval      ftdcap = wx_arbcad
004600060207     c                   eval      ftdrsc = wx_arbrsd
004700060207     c                   eval      ftdfin = wx_arbfin
004800060207     c                   eval      ftditt = 0
004900060207     c                   eval      ftdita = 0
005000060207      *
005100060207     c                   update    fiftd000
005200060207      *
005300060207     C     Kftd          reade     Fiftd05l
005400060207     c                   EndIF
005500060207      *
005600060207     C                   read      wxftdarb04
005700060207     c                   EndDO
005800060207      *
005900000607     c                   seton                                        lr
006000060207     C**-------------------------------------------------------------**
006100060207     c     *inzsr        begsr
006200060127      *----------*
006300060207     c     kftd          klist
006400060207     c                   kfld                    wx_ftdpdr
006500060207     c                   kfld                    wx_ftdtsr
006600060207     c                   kfld                    wx_ftdndc
006700060207     c                   kfld                    wx_ftdddc
006800060207     c                   kfld                    wx_ftdaas
006900060207     c                   kfld                    wx_ftdlnp
007000060207     c                   kfld                    wx_ftdnrs
007100060207     c                   kfld                    wx_ftdnsp
007200060127      *----------*
007300020402     c                   endsr
007400060207      *---------------------------------------------------------*
