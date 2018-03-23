000300090717     H DECEDIT('0,') DATEDIT(*YMD.)
000400940224      *
001300940128     F*----------------------------------------------------*
001301120423     FTII326D   CF   E             WORKSTN
001400120423     Fwfmat00f  o    E             disk
001600090717     F*----------------------------------------------------*
001800940211     D KPJBA         E DS
001801120423     D WLBDA8          DS
001802120423     D  G02DAT                 1      8  0
001803120423     D  G02INV                 9     16  0
001804120423     D  G02ERR                17     17
001805120423     D  G02TGI                18     22  0
001823120423      *------------------------------------------------------------------
001824120423     C* Reperimento parametri
001825120423     C     *ENTRY        PLIST
001826120423     C                   PARM                    KPJBA
001829120423     c                   do        *hival
001830120423     c                   exfmt     v1
001831120423     c                   exsr      controlli
001832120423     c                   if        not*in96 and *inkf
001833120423     c                   leave
001834120423     c                   endif
001835120423     c                   enddo
001836120423     C/EXEC SQL
001837120423     C+ DECLARE A1 CURSOR FOR SELECT moccli, mocmat, sum(mocqta)
001838120423     c+ from tmmoc01l WHERE mocmat = :articolo and mocatt = ' '
001839120423     c+ and mocdor between :data1 and :data2
001840120423     c+ GROUP BY moccli, mocmat
001841120423     c+ ORDER BY moccli
001842120423     C+
001843120423     C/END-EXEC
001844120423     c*
001845120423     C/EXEC SQL
001846120423     C+ OPEN A1
001847120423     C/END-EXEC
001848120423     C                   dou       *inlr
001849120423     C/EXEC SQL
001850120423     C+ FETCH NEXT FROM A1 INTO :mocmat, :moccli, :mocqta
001851120423     C/END-EXEC
001852120423     C                   select
001853120423     C                   when      sqlcod = 100
001854120423     C                   leave
001855120423     C                   when      sqlcod = 0
001856120423     c                   write     wfmat000
001857120423     C                   other
001858120423     C                   exsr      sqlerror
001859120423     C                   endsl
001860120423     C*
001861120423     C                   enddo
001862120423
001863120423     C/EXEC SQL
001864120423     C+ CLOSE A1
001865120423     C/END-EXEC
001866120423     c*
001867120423     C                   SETON                                            LR
001868120423     c*----------------------------------------------------------------
001869120423     c     controlli     begsr
001870120423     c*---------------------------------------------------------------
001871120423     c                   setoff                                       96
001872120423     c     vmat          chain     tmamp01l
001873120423     c                   if        not %found(tmamp01l)
001874120423     c                   seton                                        96
001875120423     c                   iter
001876120423     c                   endif
001877120423     c                   if        vdat1 = 0 or vdat2 = 0
001878120423     c                   seton                                        96
001879120423     c                   iter
001880120423     c                   endif
001881120423
001882120423     C* gira data1
001883120423     c                   move      vdata1        g02dat
001884120423     C                   MOVEL     *BLANK        G02ERR
001885120423     C                   CALL      'XSRDA8'
001886120423     C                   PARM                    WLBDA8
001887120423     c                   if        g02err <> *blank
001888120423     c                   seton                                        96
001889120423     c                   iter
001890120423     c                   endif
001891120423     C                   Z-ADD     G02INV        data1             8 0
001892120423     c
001893120423     C* gira data2
001894120423     c                   move      vdata2        g02dat
001895120423     C                   MOVEL     *BLANK        G02ERR
001896120423     C                   CALL      'XSRDA8'
001897120423     C                   PARM                    WLBDA8
001898120423     c                   if        g02err <> *blank
001899120423     c                   seton                                        96
001900120423     c                   iter
001901120423     c                   endif
001902120423     C                   Z-ADD     G02INV        data2             8 0
001903120423     c                   if        data1 > data2
001904120423     c                   seton                                        96
001905120423     c                   iter
001906120423     c                   endif
001907120423     c                   endsr
013433090722     C**********************************************************************
013434090722     C     sqlerror      begsr
013435090722     C**********************************************************************
013436090722     C*
013437090722     C                   call      'X66CHGJOB'
013438090722     C                   seton                                        lr
013439090722     C                   ENDSR
019300090311     c*--------------------------------------------------------------
