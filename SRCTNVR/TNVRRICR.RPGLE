000800970526     F*------------------------------------------------------------------------*
001000050322     Frictotf   ip   E             DISK    rename(rictotf:rictot000)
001001050321     Ftitas30c  uf   E           k DISK    ignore(titas000)
001002050321     F                                     ignore(titasp00)
006800970526     C*------------------------------------------------------------------------*
006900991214     C* MAIN LINES
007000970526     C*------------------------------------------------------------------------*
007001050321     c     kbol          klist
007002050321     c                   kfld                    vftaas
007003050321     c                   kfld                    vftlnp
007004050321     c                   kfld                    vftnrs
007005050321     c                   kfld                    vftnsp
007006050322     c                   kfld                    vfttbl
007007050321     c*
007300050321     C     kbol          chain     titas30c
007301050321     c                   if        %found(titas30c)
007302050321     c                   eval      tasll2 = 'A'
007303050321     c                   update    titas010
007304050321     c                   end
