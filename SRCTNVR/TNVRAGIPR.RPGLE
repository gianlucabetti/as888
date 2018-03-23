000400030121     H DEBUG DECEDIT('0,') DATEDIT(*YMD/)
001103030122     fansif01l  if   e           k disk
001104030124     fynfea01l  uf a e           k disk
001105030124     c     kfea          klist
001106030124     c                   kfld                    featpfor
001107030124     c                   kfld                    feacontra
014000030122     c     *loval        setll     ansif01l
014100010531     c                   do        *hival
014200030122     c                   read      ansif01l                               99
014300010531     c   99              leave
014301030124     c                   if        sifsocieta = '201' or
014302030124     c                             sifsocieta = '888' or
014303030124     c                             sifsocieta = '999'
014304030122     c                   iter
014305030122     c                   end
014306030124     c                   eval      feacontra = sifsocieta
014307030124     c                   eval      featpfor = 'RIAG'
014308030124     c     kfea          chain     ynfea01l
014309030124     c                   if        not %found(ynfea01l)
014310030124     c                   clear                   ynfea000
014311030124     c                   eval      feacontra = sifsocieta
014312030124     c                   eval      feasocieta = '201'
014313030124     c                   eval      feakccfor = '000151'
014314030124     c                   eval      feakscfor = '05000000'
014315030124     c                   move      sifsocieta    feakscfor
014316030124     c                   move      '9999-12-31'  feadtfico
014317030124     c                   eval      featprif1 = 'CDC'
014318030124     c                   eval      feaentit1 = '500'
014319030124     c                   eval      feaunita = '500'
014320030124     c                   eval      featpfor = 'RIAG'
014321030124     c                   eval      featprif2 = 'MAN'
014322030124     c                   write     ynfea000
014323030124     c                   end
026400020205     c                   ENDDO
103100010601     c                   seton                                        lr
