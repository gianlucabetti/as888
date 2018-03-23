000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200130424     Ffnfgv01l  iF   E           K DISK    extfile(wlibfil) usropn
000201130424     Ftnfvs01l  iF   E           K DISK    extfile(wlibsede) usropn
000400130424     Ffnfv401l  IF A E           K DISK    extfile(wlib) usropn
000912111012
000913130424     d dataiso_cor     s               d   datfmt(*iso)
000915130424     D DATEU           S              8  0
000916130424     D KEPA            s                   like(FV4EPA) inz('P')
000917130424     D KLAI            s                   like(FV4LAI) inz
000918130424     D Ktrc            s                   like(FV4trc) inz('P')
000919130424     D giro            S              1
000920130424     D Wlibsede        S             21
000921130424     D Wlibfil         S             21
000922130424     D Wlib            S             21
000923130424
000924130423     c* N.B: su fnfv4 di filiale ci saranno già quasi tutti
000925130423     c* quello di sede invece è sempre da scrivere
001600011003     C*------------------------------------------------------------------------*
001700011003     C* MAIN LINE
001800011003     C*------------------------------------------------------------------------*
001802130424     C     KFV4          KLIST
001803130424     C                   KFLD                    fgvlnp
001804130424     C                   KFLD                    fgvNFV
001805130424     C                   KFLD                    KEPA
001806130424     C                   KFLD                    Klai
001807130424     C                   KFLD                    KTRC
001808130424      /free
001809130424       // Reperisco la data corrente
001810130424       dataiso_cor=(%date());
001812130424       DATEU=%dec(dataiso_cor);
001813130424      /end-free
002600100317     c
002601130424     c* primo giro su fogli viaggio di filiale
002602130424     c                   exsr      elaborafil
002603130424     c* secondo giro su fogli viaggio di sede
003100130424     c                   exsr      elaborasede
004408111012     c*
004409130424     c                   seton                                        lr
004411130424
004412130424     C*------------------------------------------------------------------------*
004413130424     c* scrittura piombi su archivi di filiale
004414130424     C*------------------------------------------------------------------------*
004415130424     c     elaborafil    begsr
004416130424     c                   eval      giro='F'
004417130424     C* FNFGV
004418130424     c                   eval      wlibfil='FILTRAPRD/'
004419130424     c                   eval      wlibfil=%trim(wlibfil)+ 'FNFGV01L'
004420130424     c                   open(e)   fnfgv01l
004421130424     c                   if        not %open(fnfgv01l)
004422130424     c                   eval      wlibfil='FILTRA201/'
004423130424     c                   eval      wlibfil=%trim(wlibfil)+ 'FNFGV01L'
004424130424     c                   open      fnfgv01l
004425130424     C                   ENDIF
004426130424     C* FNFV4
004427130424     c                   eval      wlib='FILTRAPRD/'
004428130424     c                   eval      wlib=%trim(wlib)+ 'FNFV401L'
004429130424     c                   open(e)   fnfV401l
004430130424     c                   if        not %open(fnfV401l)
004431130424     c                   eval      wlib='FILTRA201/'
004432130424     c                   eval      wlib=%trim(wlib)+ 'FNFV401L'
004433130424     c                   open      fnfV401l
004434130424     C                   ENDIF
004435130424
004436130424     c     *loval        setll     fnfgv01l
004437130424     c                   read      fnfgv01l
004438130424     c                   dow       not %eof(fnfgv01l)
004439130424     c                   if        fgvpmb<>*blanks
004440130424     c     kfv4          setll     fnfv401l
004441130424     c                   if        not %equal(fnfv401l)
004443130424     c                   eval      fv4not=fgvpmb
004444130424     c                   exsr      wtrfv4
004445130424     c* vedo se scivere anche secondo record
004446130424    2c                   if        %subst(fgvpmb:36)<>*blanks
004447130424     c                   eval      fv4not=%subst(fgvpmb:36)
004448130424     c                   exsr      wtrfv4
004449130424    2c                   endif
004450130424     c                   endif
004451130424     c                   endif
004452130424     c                   read      fnfgv01l
004453130424     c                   enddo
004454130424     c                   close     fnfgv01l
004455130424     c                   close     fnfv401l
004456130424     c                   endsr
004457130424     C*------------------------------------------------------------------------*
004458130424     c* scrittura piombi su archivi di sede
004459130424     C*------------------------------------------------------------------------*
004460130424     c     elaborasede   begsr
004461130424     c                   eval      giro='S'
004462130424     C* TNFVS
004463130424     c                   eval      wlibsede='GAITRAGRPS/'
004464130424     c                   eval      wlibsede=%trim(wlibsede)+ 'TNFVS01L'
004465130424     c                   open(e)   TNFVS01L
004466130424     c                   if        not %open(TNFVS01L)
004467130424     c                   eval      wlibsede='GAITRAGRU/'
004468130424     c                   eval      wlibsede=%trim(wlibsede)+ 'TNFVS01L'
004469130424     c                   open      TNFVS01L
004470130424     C                   ENDIF
004471130424     C* FNFV4
004472130424     c                   eval      wlib='GAITRAGRPS/'
004473130424     c                   eval      wlib=%trim(wlib)+ 'FNFV401L'
004474130424     c                   open(e)   fnfV401l
004475130424     c                   if        not %open(fnfV401l)
004476130424     c                   eval      wlib='GAITRAGRU/'
004477130424     c                   eval      wlib=%trim(wlib)+ 'FNFV401L'
004478130424     c                   open      fnfV401l
004479130424     C                   ENDIF
004480130424
004481130424     c     *loval        setll     tnfvs01l
004482130424     c                   read      tnfvs01l
004483130424     c                   dow       not %eof(tnfvs01l)
004484130424     c                   if        fvspmb<>*blanks
004487130424     c                   eval      fv4not=fvspmb
004488130424     c                   exsr      wtrfv4
004489130424     c* vedo se scivere anche secondo record
004490130424    2c                   if        %subst(fvspmb:36)<>*blanks
004491130424     c                   eval      fv4not=%subst(fvspmb:36)
004492130424     c                   exsr      wtrfv4
004493130424    2c                   endif
004494130424     c                   endif
004495130424     c                   read      tnfvs01l
004496130424     c                   enddo
004497130424     c                   close     tnfvs01l
004498130424     c                   close     fnfv401l
004499130424     c                   endsr
004500130424     C*------------------------------------------------------------------------*
004501130424     c* scrittura piombi su fnfv4
004502130424     C*------------------------------------------------------------------------*
004503130424     c     wtrfv4        begsr
004504130424     C* SE FV4NOT
004505130424     C                   CLEAR                   FV4ATB
004506130424     C                   CLEAR                   FV4lai
004509130424     C                   MOVEL     KEPA          FV4EPA
004510130424     C                   MOVEL     KTRC          FV4TRC
004511130424     c* FNFV4 di SEDE
004512130424     c                   if        giro='S'
004513130424     C                   Z-ADD     fvslnp        FV4LNP
004514130424     C                   Z-ADD     fvsnfv        FV4NFV
004515130424     c                   clear                   fv4ft1
004516130424     c                   clear                   fv4dt1
004517130424     C                   MOVEL     'R'           FV4FT2
004518130424     c                   else
004519130424     c* FNFV4 di FILIALE
004520130424     C                   Z-ADD     fgvlnp        FV4LNP
004521130424     C                   Z-ADD     fgvnfv        FV4NFV
004522130424     C                   MOVEL     'N'           FV4FT1
004523130424     C                   MOVEL     dateu         FV4DT1
004524130424     C                   MOVEL     'N'           FV4FT2
004525130424     c                   endif
004526130424
004527130424     C                   MOVEL     dateu         FV4DT2
004528130424     C*
004529130424     C                   WRITE     FNFV4000
004530130424     c                   endsr
