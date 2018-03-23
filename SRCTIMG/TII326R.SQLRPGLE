000300090717     H DECEDIT('0,') DATEDIT(*YMD.)
000400940224      *
001300940128     F*----------------------------------------------------*
001301120423     FTII326D   CF   E             WORKSTN
001302120423     Ftmamp01l  if   e           k disk
001400120508     Fwfmat00f  o    E             disk
001600090717     F*----------------------------------------------------*
001800940211     D KPJBA         E DS
001801120423     D WLBDA8          DS
001802120423     D  G02DAT                 1      8  0
001803120423     D  G02INV                 9     16  0
001804120423     D  G02ERR                17     17
001805120423     D  G02TGI                18     22  0
001806120423     d coman           s             99
001807120423     d luncmd          s             15  5
001809120423     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
001810120423     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
001811120423     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
001812120423     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
001823120423      *------------------------------------------------------------------
001824120423     C* Reperimento parametri
001825120423     C     *ENTRY        PLIST
001826120423     C                   PARM                    KPJBA
001827120423      *
001828120423     c                   close     wfmat00f
001829120424     c                   movea     cmd4          coman
001830120423     c                   exsr      comandi
001831120423     c                   open      wfmat00f
001832120423
001833120423     c                   do        *hival
001834120423     c                   exfmt     v1
001835120423     c   kc              goto      fine
001838120423     c                   exsr      controlli
001839120424     c                   if        not*in96 and not*in95 and
001840120424     c                             not*in94 and *inkf
001841120423     c                   leave
001842120423     c                   endif
001843120423     c                   enddo
001844120423     C/EXEC SQL
001845120423     C+ DECLARE A1 CURSOR FOR SELECT moccli, mocmat, sum(mocqta)
001846120423     c+ from tmmoc01l WHERE mocmat = :articolo and mocatt = ' '
001847120424     c+ and mocdbl between :data1 and :data2
001848120423     c+ GROUP BY moccli, mocmat
001849120423     c+ ORDER BY moccli
001850120423     C+
001851120423     C/END-EXEC
001852120423     c*
001853120423     C/EXEC SQL
001854120423     C+ OPEN A1
001855120423     C/END-EXEC
001856120423     C                   dou       *inlr
001857120423     C/EXEC SQL
001858120508     C+ FETCH NEXT FROM A1 INTO :moccli, :mocmat, :mocqtat
001859120423     C/END-EXEC
001860120423     C                   select
001861120423     C                   when      sqlcod = 100
001862120423     C                   leave
001863120423     C                   when      sqlcod = 0
001864120423     c                   write     wfmat000
001865120423     C                   other
001866120423     C                   exsr      sqlerror
001867120423     C                   endsl
001868120423     C*
001869120423     C                   enddo
001870120423
001871120423     C/EXEC SQL
001872120423     C+ CLOSE A1
001873120423     C/END-EXEC
001874120423     c*
001875120423      *verifica se esiste file in libreria di wrk per utente
001876120423     c                   close     wfmat00f
001877120424     c                   movea     cmd1          coman
001878120423     c                   exsr      comandi
001879120423      *se trova il file esegue copy file altrimenti lo crea
001880120423     c                   if        *in34
001881120424     c                   movea     cmd2          coman
001882120423     c                   exsr      comandi
001883120423     c                   else
001884120424     c                   movea     cmd3          coman
001885120423     c                   exsr      comandi
001886120423     c                   endif
001887120423     c     fine          tag
001888120423     C                   SETON                                            LR
001889120423     c*----------------------------------------------------------------
001890120423     c     controlli     begsr
001891120423     c*---------------------------------------------------------------
001892120424     c                   setoff                                       969594
001893120423     c     varticolo     chain     tmamp01l
001894120423     c                   if        not %found(tmamp01l)
001895120423     c                   seton                                        96
001896120423     c                   endif
001897120423     c                   move      varticolo     articolo         12
001898120423     c                   if        vdata1 = 0 or vdata2 = 0
001899120424     c                   seton                                        95
001900120423     c                   endif
001901120424      *
001902120423     C* gira data1
001903120423     c                   move      vdata1        g02dat
001904120423     C                   MOVEL     *BLANK        G02ERR
001905120423     C                   CALL      'XSRDA8'
001906120423     C                   PARM                    WLBDA8
001907120423     c                   if        g02err <> '0'
001908120424     c                   seton                                        95
001909120423     c                   endif
001910120423     C                   Z-ADD     G02INV        data1             8 0
001911120424     C                   Z-ADD     G02dat        vdata1
001912120423     c
001913120423     C* gira data2
001914120423     c                   move      vdata2        g02dat
001915120423     C                   MOVEL     *BLANK        G02ERR
001916120423     C                   CALL      'XSRDA8'
001917120423     C                   PARM                    WLBDA8
001918120423     c                   if        g02err <> '0'
001919120424     c                   seton                                        94
001920120423     c                   endif
001921120423     C                   Z-ADD     G02INV        data2             8 0
001922120424     C                   Z-ADD     G02dat        vdata2
001923120423     c                   if        data1 > data2
001924120424     c                   seton                                        94
001925120423     c                   endif
001926120423     c                   endsr
001927120423     c*-------------------------------------------------
001928120423     c     comandi       begsr
001929120423     c*-------------------------------------------------
001930120423      *
001931120423     C                   z-add     99            luncmd
001932120423     C                   CALL      'QCMDEXC'                            34
001933120423     C                   parm                    coman
001934120423     C                   parm                    luncmd
001935120423      *
001936120423     c                   endsr
013433090722     C**********************************************************************
013434090722     C     sqlerror      begsr
013435090722     C**********************************************************************
013436090722     C*
013437090722     C                   call      'X66CHGJOB'
013438090722     C                   seton                                        lr
013439090722     C                   ENDSR
019300090311     c*--------------------------------------------------------------
019400120423      *---------------------------------------------------------
020100120424**  CMD1
020200120423CHKOBJ  OBJ(WRKQUERY/wfmat00f) OBJTYPE(*FILE)
020300120424**  CMD2
020400120423CRTDUPOBJ OBJ(wfmat00f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(wfmat00f) DATA(*YES)
020500120424**  CMD2
020600120423CPYF FROMFILE(wfmat00f) TOFILE(WRKQUERY/wfmat00f) MBROPT(*REPLACE)
020700120424**  CMD4
020800120423CLRPFM    FILE(wfmat00F)
