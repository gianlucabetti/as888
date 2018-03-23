000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000201151008     FTntbe01l  IF   E           K DISK
000203151008     d dtce          e ds
000400990504     d dataiso         s               d   datfmt(*iso)
001300990601     d
001301151009     d param           ds
001305151009     d  pDave                         8    dim(10)
001306151009     d  pDdar                         8    dim(10)
001307151009     d  pAave                         8    dim(10)
001308151009     d  pAdar                         8    dim(10)
001309151009     d  pcave                         8    dim(10)
001310151009     d  pcdar                         8    dim(10)
001311151009      *
001312151009     d  Dave           s              8    dim(10)
001313151009     d  Ddar           s              8    dim(10)
001314151009     d  Aave           s              8    dim(10)
001315151008     d  Adar           s              8    dim(10)
001316151009     d  Cave           s              8    dim(10)
001317151009     d  Cdar           s              8    dim(10)
001318151008     d  vuoto          s              8
001800151008      * ____________________________________________________
001801151008     c     'TCE'         setll     tntbe01l
001802151008     c                   do        *hival
001803151008     c     'TCE'         reade     tntbe01l
001804151008     c                   if        %eof(tntbe01l)
001805151008     c                   leave
001806151008     c                   endif
001807151008     c                   movel     tbeuni        dtce
001808151008     c                   if        §tcece = 'S' and §tceute = 'S'
001809151008     c                   exsr      reprce
001810151008     c                   movel     'TCE'         kcod
001811151008     c     ktbe          setgt     tntbe01l
001812151008     c
001813151008     c                   select
001814151008      *tipo servizio autisti
001815151008     c                   when      §tctsr = 'A'
001816151008     c                   if        voca <> *blank
001817151008     c     voca          lookup    aave                                   90
001818151008     c  n90              z-add     1             i                 2 0
001819151008     c  n90vuoto         lookup    aave(i)                                90
001820151008     c   90              move      voca          aave(i)
001821151008     c                   endif
001822151008     c                   if        vocd <> *blank
001823151008     c     vocd          lookup    adar                                   90
001824151008     c  n90              z-add     1             i
001825151008     c  n90vuoto         lookup    adar(i)                                90
001826151008     c   90              move      vocd          adar(i)
001827151008     c                   endif
001828151008      *tipo servizio coop
001829151008     c                   when      §tctsr = 'C'
001830151008     c                   if        voca <> *blank
001831151008     c     voca          lookup    cave                                   90
001832151008     c  n90              z-add     1             i
001833151008     c  n90vuoto         lookup    cave(i)                                90
001834151008     c   90              move      voca          cave(i)
001835151008     c                   endif
001836151008     c                   if        vocd <> *blank
001837151008     c     vocd          lookup    cdar                                   90
001838151008     c  n90              z-add     1             i
001839151008     c  n90vuoto         lookup    cdar(i)                                90
001840151008     c   90              move      vocd          cdar(i)
001841151008     c                   endif
001842151008      *tipo servizio aff/def
001843151008     c                   when      §tctsr = 'D'
001844151008     c                   if        voca <> *blank
001845151008     c     voca          lookup    dave                                   90
001846151008     c  n90              z-add     1             i
001847151008     c  n90vuoto         lookup    dave(i)                                90
001848151008     c   90              move      voca          dave(i)
001849151008     c                   endif
001850151008     c                   if        vocd <> *blank
001851151008     c     vocd          lookup    ddar                                   90
001852151008     c  n90              z-add     1             i
001853151008     c  n90vuoto         lookup    ddar(i)                                90
001854151008     c   90              move      vocd          ddar(i)
001855151008     c                   endif
001856151008     c                   endsl
001857151008     c                   endif
001858151008     c                   enddo
001859151009     c
001860151009     c                   movea     Dave          pDave
001861151009     c                   movea     Ddar          pDdar
001862151009     c                   movea     Aave          pAave
001863151009     c                   movea     Adar          pAdar
001864151009     c                   movea     cave          pcave
001865151009     c                   movea     cdar          pcdar
003000990504     c                   seton                                        lr
003100151008     C*_____________________________________________________________
003200151008     c     reprce        begsr
003300151008     C*_____________________________________________________________
003400151008      * reperisce capoconto causale
003401151008     c                   clear                   vocd
003402151008     c                   clear                   voca
003403151008     c                   movel     'RCE'         kcod
003601151008     c                   movel(p)  tbeke1        kke1
004200151008     c                   movel(p)  'VOCEDARE'    kke2
004300151008     c     ktbe          chain     tntbe01l
004400151008     c                   if        %found(tntbe01l)
004500151008     C                   MOVEL     TBeUNI        VOCD              8
004600151008     c                   endif
004700151008     c                   movel(p)  'VOCEAVERE'   kke2
004800151008     c     ktbe          chain     tntbe01l
004900151008     c                   if        %found(tntbe01l)
005000151008     C                   MOVEL     TBeUNI        VOCA              8
005100151008     c                   endif
005101151008     c                   endsr
005102151008     C*----------------------------------------------------*
005103151008     c     *inzsr        begsr
005104151008     C*----------------------------------------------------*
005105151008     C     *ENTRY        PLIST                                                  *
005106151009     C                   PARM                    param                          *
005200151008     C     KTbe          KLIST                                                  *
005300151008     C                   KFLD                    kcod              3            *
005400151008     C                   KFLD                    kke1             15            *
005500151008     C                   KFLD                    kke2             15
005600151008     C                   move      'RCE'         kcod
005700151008     c                   endsr
