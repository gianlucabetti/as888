000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300991105      *------------------------------------------------------------------------*
000900940712      *------------------------------------------------------------------------*
001000991105     H DECEDIT('0,') DATEDIT(*DMY/)
001200940712      *
001300150402     FFNLRU6td  CF   E             WORKSTN
006600040218      *------------------------------------------------------------------------*
012700040216     D* PASSAGGIO A INTERROGAZIONE VARIAZIONI           - FNLR75R -
012800940720     D* PASSAGGIO A INTERROGAZIONE DETTAGLIO COLLI      - FNLR37R -
012900940712     D PARAM1          DS
013000150402     D  pa1AAS                14     17  0
013100150409     D  pa1Lp2                18     20  0
013200150402     D  pa1NRS                21     22  0
013300150402     D  pa1NSP                23     29  0
013400940712     D  PA1F03                30     30
013500940713     D  PA1FLG                31     31
013600940713     D  PA1PGM                32     41
013700940714     D  PA1DSA                42     61
013800940809     D  PA1CML                62     62
013900940720     D  PA1DSP                63     70  0
014000940720     D  PA1RMN                71     85  0
014100940720     D  PA1LNA                86     88  0
014200940802     D  PA1LN1                89    108
014300940802     D  PA1RSD               109    143
014400941213     D* SE RICH DA PGM GIAC. = 1
014500941213     D  PA1GIA               144    144
014600941227     D  PA1FEL               145    147  0
014700950628     D  PA1TBO               148    148
014800110929     d  pa1rma               149    163
014900110929     d  pa1xco               164    164
026600131111     D
028201150319     D                 DS
028202150319     D  V1cem1                 1     40
028203150319     D  V1cem2                41     70
028204150319     D  V1ceml                 1     70
028300131111     d
030200040218      *
030500991027      *
030600040216     D KPJBA         E DS
030701150310     d FNLRU6DS      e ds
030702150402     D w0070           s              7  0
030703150410     D wCALU6          s              1
031200040216      *
050903150331
050904150331      // ? PROTOTIPI ?
050907150327
051000900518     C****************************************************************
051100900518     C*  RIEPILOGO INDICATORI
051200900518     C***************************************************************
051300150402     C* 01    -
058900900518     C*****************************************************************
059000000000     C     *ENTRY        PLIST
059100000000     C                   PARM                    KPJBA
059300131210     c
059301150402     c                   clear                   param1
059302150402     c                   clear                   fnlru6ds
059303150402
059304150402     c                   eval      v1caff='N'
059305150402     c                   eval      pa1aas=2015
059306150402
059307150402     c                   do        *hival
059308150402     c                   exfmt     lru6d01
059309150402     c                   if        *inkc
059310150402     c                   seton                                        lr
059311150402     c                   return
059312150402     c                   endif
059313150410     C                   EVAL      WCALU6=' '
059314150402     c                   exsr      sr_parametri
059315150402     c                   movel     param1        kpjbu
059316150402     c                   select
059317150402     c* f1=singola
059318150402     c                   when      *inka
059319150402     c                   if        v1cdara='S' or v1cu6ds='S'
059320150402     c                   call      'FNLRU6R1'
059321150402     c                   parm                    kpjba
059322150402     c                   parm                    fnlru6ds
059323150402     c                   else
059324150402     c                   call      'FNLRU6R1'
059325150402     c                   parm                    kpjba
059326150402     c                   endif
059327150410     c                   eval      wcalu6='1'
059328150402     c* f6=sfl
059329150408     c                   when      *inkf
059330150402     c                   call      'FNLRU6R'
059331150402     c                   parm                    kpjba
059332150402     c                   parm                    fnlru6ds
059333150410     c                   eval      wcalu6='1'
059334150402     c
059335150408     c                   when      *inkg
059336150402     c                   call      'FNLRU6R'
059337150402     c                   parm                    kpjba
059338150410     c                   eval      wcalu6='1'
059339150402     c                   endsl
059340150410     c* non riscontrati errori: visualizzo il contenuto dei campi di output
059341150410     c                   setoff                                       51
059342150410     c                   if        WCALU6='1' AND olru6err=' '
059343150410     c                   seton                                        51
059344150410     c                   eval      param1=kpjbu
059345150410     c                   endif
059346150402     c                   enddo
059700980609     C**
059800150402     C**************************************************************************
059900150402     C     sr_parametri  BEGSR
059901150408     c                   eval      ilru6sel   = v1cu6ds
059902150408     c                   eval      ilru6ra    = v1cdara
059903150402     c                   eval      ILRU6BRTCO = v1cbarcode
059904150402     c                   eval      ILRU6SSP   = v1cssp
059905150402     c                   eval      ILRU6LNA   = v1clna
059906150402     c                   eval      ILRU6NZD   = v1cnzd
059907150402     c                   eval      ILRU6PRD   = v1cprd
059908150402     c                   eval      ILRU6RSD   = v1crsd
059909150402     c                   eval      ILRU6XRSD  = v1cxrsd
059910150402     c                   eval      ILRU6IND   = v1cind
059911150402     c                   eval      ILRU6XIND  = v1cxind
059912150402     c                   eval      ILRU6LOD   = v1clod
059913150402     c                   eval      ILRU6XLOD  = v1cxlod
059914150402     c                   eval      ILRU6RSM   = v1crsm
059915150402     c                   eval      ILRU6XRSM  = v1cxrsm
059916150402     c                   eval      ILRU6LP1   = v1clp1
059917150402     c                   eval      ILRU6DTD   = v1cdtd
059918150402     c                   eval      ILRU6DTA   = v1cdta
059919150402     c                   eval      ILRU6NOT   = v1cnot
059920150402     c                   eval      ILRU6RPE   = v1crpe
059921150402     c                   eval      ILRU6POE   = v1cpoe
059922150402     c                   eval      ILRU6NSR   = v1cnsr
059923150402     c                   eval      ILRU6NOR   = v1cnor
059924150402     c                   eval      ILRU6NRV   = v1cnrv
059925150402     c                   eval      ILRU6RMN   = v1crmn
059926150402     c                   eval      ILRU6RMA   = v1crma
059927150402     c                   eval      ILRU6PSD   = v1cpsd
059928150402     c                   eval      ILRU6PSA   = v1cpsa
059929150402     c                   eval      ILRU6SFD   = v1csfd
059930150402     c                   eval      ILRU6SCD   = v1cscd
059931150402     c                   eval      ILRU6SCA   = v1csca
059932150402     c                   eval      ILRU6FLS   = v1cfls
059933150402     c                   eval      ILRU6TSP   = v1ctsp
059934150402     c                   eval      ILRU6DAN   = v1cdan
059935150402     c* Affidabilità S/N
059936150402     c                   eval      ILRU6AFF = v1caff
059938150402     c                   eval      ILRU6FBR = v1cfbr
059939150402     c                   eval      ILRU6CBO = v1ccbo
059940150402     c                   eval      ILRU6GIA = v1cgia
059941150508     c****               eval      ILRU6FT2 = v1cft1
059943150402     c                   movel     v1cklp        w0070
059944150402     c                   move      v1cksc        w0070
059945150402     c                   eval      ILRU6CCM = w0070
059946150402     c                   eval      ILRU6RAG = v1crag
059947150402     c                   eval      ILRU6EML = v1ceml
059948150402     c                   eval          ILRU6TEL=v1ctel
059949150408     c                   clear                   olru6tor
059950150408     c                   clear                   olru6ogg
059951150408     c                   clear                   olru6err
059952150408     c                   clear                   olru6msg
060000150402     c                   endsr
