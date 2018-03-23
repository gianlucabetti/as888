000100940224     H*--------------------------------------------------------------*
000200060619     FFIdp300F  iF   E             DISK
000300060619     FTIdp300F  o  a E             DISK    prefix(new)
000400900131     C*---------------------------------------------------------------*
000500060619     c     karb01        Klist
000600060619     c                   Kfld                    dp3AAS
000700060619     c                   Kfld                    dp3LNP
000800060619     c                   Kfld                    dp3NRS
000900060619     c                   Kfld                    dp3NSP
001000030205      *
001100030515     c                   do        *hival
001200060619     c                   read      fiDP300F
001300030515     c* fine file
001400060619     c                   if        %eoF(fiDP300F)
001500030515     c                   leave
001600030515     c                   end
001700060619     C*
001800060619     c                   clear                   tiDP3000
001900060619     c                   move      dp3ATB        newdp3ATB
002000060619     c                   z-add     dp3AAS        newdp3AAS
002100060619     c                   z-add     dp3LNP        newdp3LNP
002200060619     c                   z-add     dp3NRS        newdp3NRS
002300060619     c                   z-add     dp3NSP        newdp3NSP
002400060619     c                   movel     dp3PRN        newdp3PRN
002500060619     c                   movel     dp3EVD        newdp3EVD
002600060619     c                   movel     dp3CEV        newdp3CEV
002700060619     c                   z-add     dp3FLE        newdp3FLE
002800060619     c                   z-add     dp3DEV        newdp3DEV
002900060619     c                   z-add     dp3HEV        newdp3HEV
003000060619     c                   movel     dp3FTR        newdp3FTR
003100060619     c                   z-add     dp3DTR        newdp3DTR
003200060619     C*
003300060619     c                   write     tiDP3000
003400041122     C*
003500030205     c                   enddo
003600000616     C*
003700000000     C                   SETON                                        LR
