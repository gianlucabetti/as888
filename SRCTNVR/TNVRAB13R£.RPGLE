000100940224     H*--------------------------------------------------------------*
000200041122     FFIFCE01L  UF   E           k DISK
000300041122     Ffce01104  iF   E             DISK    prefix(N_)
000400041122     f                                     rename(fce01104:fce011)
000500900131     C*---------------------------------------------------------------*
000600041122     c     kfce          klist
000700041122     c                   kfld                    N_fcefgs
000800041122     c                   kfld                    N_fcepdr
000900041122     c                   kfld                    N_fcetsr
001000041122     c                   kfld                    N_fcendc
001100041122     c                   kfld                    N_fceddc
001200041122     c                   kfld                    N_fceaas
001300041122     c                   kfld                    N_fcelnp
001400041122     c                   kfld                    N_fcenrs
001500041122     c                   kfld                    N_fcensp
001600041122     c                   kfld                    N_fcecce
001700030205      *
001800030515     c                   do        *hival
001900041122     c                   read      fce01104
002000030515     c* fine file
002100041122     c                   if        %eoF(fce01104)
002200030515     c                   leave
002300030515     c                   end
002400030205     C*
002500041122     c     kfce          chain     fifce01l
002600041122     c                   if        %Found(fifce01l)
002700041122     c                   z-add     N_fceice      fceice
002800041122     c                   update    fifce000
002900041122     c                   end
003000041122     C*
003100030205     c                   enddo
003200000616     C*
003300000000     C                   SETON                                        LR
