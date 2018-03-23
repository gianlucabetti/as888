000100010213     H DECEDIT('0,') DATEDIT(*YMD.)
000200010213
001000010213
001100010905     FFNBLTE0T  UF   E             DISK
001200010905     FFNART01L  UF A E           K DISK
001300010905     FFNARB01L  IF   E           K DISK
001400010905     FFIBRV09L  IF   E           K DISK
009100010214
009101010905     D wnpgp           s                   like(brvnpg)inz(1)
009102010905     D wnpga           s                   like(brvnpg)inz(2)
009200010905      *    Accesso a FNART
009300010214
009400010905     C     kfnart        KLIST
009500010905     C                   kfld                    bltaas
009501010905     C                   kfld                    bltlnp
009502010905     C                   kfld                    bltnrs
009503010905     C                   kfld                    bltnsp
009504010905     C                   kfld                    bltnsc
009800010214
009801010905      *    Accesso a FNARB
009802010905
009803010905     C     kfnarb        KLIST
009804010905     C                   kfld                    bltaas
009805010905     C                   kfld                    bltlnp
009806010905     C                   kfld                    bltnrs
009807010905     C                   kfld                    bltnsp
009808010905
009809010905      *    Accesso a FIBRV  partenza
009810010905
009811010905     C     kfibrvp       KLIST
009812010905     C                   kfld                    wnpgp
009813010905     C                   kfld                    bltlnp
009814010905     C                   kfld                    arblna
009815010905     C                   kfld                    bltnrs
009816010905     C                   kfld                    bltnsc
009818010905
009819010905      *    Accesso a FIBRV  arrivo
009820010905
009821010905     C     kfibrva       KLIST
009822010905     C                   kfld                    wnpga
009823010905     C                   kfld                    bltlnp
009824010905     C                   kfld                    arblna
009825010905     C                   kfld                    bltnrs
009826010905     C                   kfld                    bltnsc
012200010213      *------------------------------------------------------------------------*
012300010213      *                                                                        *
012400010213      *    ROUTINE PRINCIPALE                                                  *
012500010213      *                                                                        *
012600010213      *------------------------------------------------------------------------*
012700010213
012800010905      *    Lettura File FNBLTE
012900010905      *    ===================
013000010213
013200010213     C                   do        *hival
013300010905     C                   read      FNBLTE0T
013400010905     C                   if        %eof(fnblte0t)
013500010213     C                   leave
013600010213     C                   endif
013601010905
013700010905      *    verifco l'esistenza del record in FNART01L
013701010905
013702010905     c     kfnart        chain     FNART01L
013703010905     c                   if        %found(fnart01l)
013704010905     c                   unlock    fnart01l
013705010905     c                   iter
013706010905     c                   endif
013707010905
013708010905      *    recupero i dati da FNARB01L
013709010905
013710010905     c     kfnarb        chain     fnarb01l
013711010905     c                   if        not %found(fnarb01l)
013712010905     c                   iter
013713010905     c                   endif
013714010905
013715010905     c                   clear                   fnart000
013716010905     c                   eval      artaas = bltaas
013717010905     c                   eval      artlnp = bltlnp
013718010905     c                   eval      artnrs = bltnrs
013719010905     c                   eval      artnsp = bltnsp
013720010905     c                   eval      artlna = arblna
013721010905     c                   eval      artfls = arbfls
013722010905     c                   eval      artnsc = bltnsc
013723010905     c                   eval      artdfv = 20010904
013726010905     c                   eval      artftr = 'R'
013727010905     c                   eval      artdtr = 20010905
013728010905
013729010905      *    recupero i dati da FIBRV09L per i dati partenza
013730010905
013731010905     c     kfibrvp       chain     fibrv09l
013732010905     c                   if        %found(fibrv09l)
013733010905     c                   z-add     brvnps        artnpp
013734010905     c                   z-add     brvnfv        artnfv
013735010905     c                   endif
013736010905
013737010905      *    recupero i dati da FIBRV09L per i dati arrivo
013738010905
013739010905     c     kfibrva       chain     fibrv09l
013740010905     c                   if        %found(fibrv09l)
013741010905     c                   z-add     brvnps        artnap
013742010905     c                   eval      artdam = 20010905
013743010905     c                   endif
013744010905
013745010905      * scrivo il record  di fnart01l
013746010905     c                   write     fnart000
013747010905      * deleto il record  di fnblte0t
013748010905     c                   delete    fnblte00
038700010213
038800010213     C                   enddo
038900010213
039000010213      *    Fine Programma
039100010606      *    ==============
039200010213
039600010214
039700010213     C                   eval      *inlr = *on
039800010219
