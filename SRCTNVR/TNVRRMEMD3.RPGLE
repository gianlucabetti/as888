000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000101111102     femldst_errif   e             disk    rename(emldst_err:emldst_rec)
000200111102     Ffiar501l  uF   E           K DISK
000400111012     FTITAS30C  UF   E           K DISK    usropn extfile(wtas30)
000401111102     f                                     ignore(titas010)
000402111012     f                                     ignore(titasp00)
000403111012     FTITA730C  IF   E           K DISK    usropn extfile(wta730)
000405111012     f                                     ignore(tita7p00)
000407111012     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
000408111012
000700090715     d
000800111012     d dAr5emd       e ds                  Inz
000801111012     d dtasflo       e ds
000901111012     d Wtas30          s             21    inz('GAITRAGRU /TITAS30C')
000902111012     d Wta730          s             21    inz('GAITRAGRU /TITA730C')
000903111102     d trd             s                   like(ar5trd) inz('EMD')
000911111012     d wimpvrm         s                   like(ta7van)
000912111102     d olddto          s                   like(§ar5dto)
000913111102     d waas            s                   like(tasaas)
000914111102     D waggio          s              1
000915111012
001200100317     d
001500100317
001600011003     C*------------------------------------------------------------------------*
001700011003     C* MAIN LINE
001800011003     C*------------------------------------------------------------------------*
002600100317     c
002700111102     c     ktas          klist
002800111102     c                   kfld                    waas
002900111102     c                   kfld                    lnp
003000111102     c                   kfld                    nrs
003001111102     c                   kfld                    nsp
003002111102     c     kar5          klist
003003111102     c                   kfld                    waas
003004111102     c                   kfld                    lnp
003005111102     c                   kfld                    nrs
003006111102     c                   kfld                    nsp
003007111102     c                   kfld                    trd
003008111012     c     kta7          klist
003009111012     c                   kfld                    tasaas
003010111012     c                   kfld                    taslnp
003011111012     c                   kfld                    tasnrs
003012111012     c                   kfld                    tasnsp
003013111012     c                   kfld                    tastbl
003100100317     c
004401111012     c*
004402111102     c                   movel     'S'           waggio
004414111012     c*
004415111012     c                   Open(e)   titas30c
004416111012     c                   if        not %open(titas30c)
004417111012     c                   eval      %subst(Wtas30:7:4)='GRPS'
004418111012     c                   Open      titas30c
004419111012     c                   endif
004420111012     c                   Open(e)   tita730c
004421111012     c                   if        not %open(tita730c)
004422111012     c                   eval      %subst(Wta730:7:4)='GRPS'
004423111012     c                   Open      tita730c
004424111012     c                   endif
004425111102     c*
004427111102     c                   read      emldst_err
004428111102    1c                   dow       not %eof(emldst_err)
004429111102     c                   z-add     aas           waas
004430111102     c     ktas          chain     titas30c
004431111102    3c                   if        %found(titas30c) and tasnft=0
004432111102     c                   movel     tasflo        dtasflo
004433111102     c                   clear                   §floemd
004434111102     c                   movel     dtasflo       tasflo
004435111102     c* verifico se tassata la varia 'm' e in questo caso la stampo
004436111102     c                   exsr      sr_variam
004437111102     c                   if        waggio='S'
004438111102     C                   UPDATE    TITAS000
004439111102     c                   else
004440111102     c                   unlock    titas30c
004441111102     c                   endif
004442111102     c     kar5          chain     fiar501l
004443111102     c                   if        %found(fiar501l)
004444111102     c                   movel     ar5uni        dar5emd
004445111102     c                   eval      olddto=§ar5dto
004446111102     c                   clear                   §ar5dto
004447111102     c                   movel     dar5emd       ar5uni
004448111102     c                   if        waggio='S'
004449111102     c                   update    fiar5000
004450111102     c                   else
004451111102     c                   unlock    fiar501l
004452111102     c                   endif
004453111102     c                   endif
004454111102     c                   except    bolla
004455111102     c                   endif
004456111102     c                   read      emldst_err
004457111102     c                   enddo
004500011003     C*
004959111012     c*
004960111012     c                   close     titas30c
004961111012     c                   close     tita730c
013800011003     C
013900011003     C                   SETON                                        LR
013901111012     c******************************************************************************
013902111012     c*Recupero importo della varia 'm'
013903111012     c******************************************************************************
013904111012     C     sr_variam     begsr
013905111012     c                   clear                   wimpvrm
013906111012     c                   select
013907111012     c                   when      tassv1='m'
013908111012     c                   z-add     tasva1        wimpvrm
013909111012     c                   when      tassv2='m'
013910111012     c                   z-add     tasva2        wimpvrm
013911111012     c                   when      tassv3='m'
013912111012     c                   z-add     tasva3        wimpvrm
013913111012     c                   when      tassv3<>*blanks
013914111012     c     kta7          setll     tita730c
013915111012     c     kta7          reade     tita730c
013916111012     c                   dow       not  %eof(tita730c)
013917111012     c                   if        ta7svn='m'
013918111012     c                   z-add     ta7van        wimpvrm
013919111012     c                   leave
013920111012     c                   endif
013921111012     c     kta7          reade     tita730c
013922111012     c                   enddo
013923111012     c                   endsl
013924111012     c                   endsr
018600111012     OPRTF198   E            bolla       1
018700100317     O                       tasaas           +   1
018800100317     O                       taslnp           +   1
018900100317     O                       tasnrs           +   1
019000100317     O                       tasnsp        2  +   1
019100111012     o                       tasccm           +   1
019101111012     o                       wimpvrm       2  +   2
019102111102     o                       olddto           +   1
