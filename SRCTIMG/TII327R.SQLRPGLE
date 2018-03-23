000300171121     H DECEDIT('0,') DATEDIT(*YMD.)
000400171121      *
001300171121     F*----------------------------------------------------*
001301171121     FTII327D   CF   E             WORKSTN
001302171121     Ftmamp01l  if   e           k disk
001303171121     Fcnaco00f  if   e           k disk
001400171121     Fwfmatc0f  o    E             disk
001600171121     F*----------------------------------------------------*
001800171121     D KPJBA         E DS
001801171121     D WLBDA8          DS
001802171121     D  G02DAT                 1      8  0
001803171121     D  G02INV                 9     16  0
001804171121     D  G02ERR                17     17
001805171121     D  G02TGI                18     22  0
001806171122     d wmoccom         s             20
001807171121     d coman           s             99
001808171121     d luncmd          s             15  5
001809171121     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
001810171121     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
001811171121     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
001812171121     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
001823171121      *------------------------------------------------------------------
001824171121     C* Reperimento parametri
001825171121     C     *ENTRY        PLIST
001826171121     C                   PARM                    KPJBA
001827171121      *
001828171121     c     kaco          klist
001829171121     c                   kfld                    acokut
001830171121     c                   kfld                    acokcc
001831171121     c                   kfld                    acoksc
001832171121     c                   z-add     1             acokut
001833171121     c                   z-add     151           acokcc
001834171122     c                   move      knsif         vknsif
001835171122     c                   move      knmus         vknmus
001836171121      *
001837171121     c                   close     wfmatc0f
001838171121     c                   movea     cmd4          coman
001839171121     c                   exsr      comandi
001840171121     c                   open      wfmatc0f
001841171121
001842171121     c                   do        *hival
001843171121     c                   exfmt     v1
001844171121     c   kc              goto      fine
001845171121     c                   exsr      controlli
001846171121     c                   if        not*in96 and not*in95 and
001847171121     c                             not*in94 and *inkf
001848171121     c                   leave
001849171121     c                   endif
001850171121     c                   enddo
001851171121     C/EXEC SQL
001852171121     C+ DECLARE A1 CURSOR FOR SELECT moccli, mocmat, mocdmt,
001853171121     C+ mocdbl, mocnbl, mocqta, moccom
001854171121     c+ from tmmoc01l WHERE  mocatt = ' ' and substr(moccom, 1, 2) = 'C:'
001855171121     c+ and mocdbl between :data1 and :data2
001856171121     C+
001857171121     C/END-EXEC
001858171121     c*
001859171121     C/EXEC SQL
001860171121     C+ OPEN A1
001861171121     C/END-EXEC
001862171121     C                   dou       *inlr
001863171121     C/EXEC SQL
001864171121     C+ FETCH NEXT FROM A1 INTO :moccli, :mocmat, :mocdmt,
001865171122     C+ :mocdbl, :mocnbl, :mocqta, :wmoccom
001866171121     C/END-EXEC
001867171121     C                   select
001868171121     C                   when      sqlcod = 100
001869171121     C                   leave
001870171121     C                   when      sqlcod = 0
001871171121      *verifica se record da scrivere
001872171121     c                   exsr      testwtr
001880171121      *
001882171121     C                   other
001883171121     C                   exsr      sqlerror
001884171121     C                   endsl
001885171121     C*
001886171121     C                   enddo
001887171121
001888171121     C/EXEC SQL
001889171121     C+ CLOSE A1
001890171121     C/END-EXEC
001891171121     c*
001892171121      *verifica se esiste file in libreria di wrk per utente
001893171121     c                   close     wfmatc0f
001894171121     c                   movea     cmd1          coman
001895171121     c                   exsr      comandi
001896171121      *se trova il file esegue copy file altrimenti lo crea
001897171121     c                   if        *in34
001898171121     c                   movea     cmd2          coman
001899171121     c                   exsr      comandi
001900171121     c                   else
001901171121     c                   movea     cmd3          coman
001902171121     c                   exsr      comandi
001903171121     c                   endif
001904171121     c     fine          tag
001905171121     C                   SETON                                            LR
001906171121     c*----------------------------------------------------------------
001907171121     c     testwtr       begsr
001908171121     c*----------------------------------------------------------------
001909171122     c                   eval      cliente = %subst(wmoccom:3:7)
001910171121     c                   if        cliente > '0000000'
001911171121     c                   move      cliente       acoksc
001912171121     c     kaco          chain     cnaco00f
001913171121     c                   if        %found(cnaco00f)
001914171121     c                   eval      ragcli = acorag
001915171121     c                   else
001916171121     c                   eval      ragcli = '?'
001917171121     c                   end
001918171121     c                   else
001919171121     c                   eval      ragcli = '????????'
001920171121     c                   end
001921171123      *richiesto singolo articolo se non coprrisponde non scrive
001922171123     c                   if        articolo <> *blank and
001923171123     c                             mocmat <> articolo
001924171123     c                   leavesr
001925171123     c                   endif
001926171121     c                   write     wfmatc00
001927171121     c                   endsr
001928171121     c*----------------------------------------------------------------
001929171121     c     controlli     begsr
001930171121     c*---------------------------------------------------------------
001931171121     c                   setoff                                       969594
001932171122     c                   if        varticolo <> *blank
001933171124     c                   if        varticolo = '?          '
001934171124     C                   MOVE      *BLANKS       varticolo
001935171124     C                   clear                   desmat           30
001936171124     C                   CALL      'TII104R'                                    *
001937171124     C                   PARM                    varticolo
001938171124     C                   PARM                    desmat
001939171124     c                   endif
001940171121     c     varticolo     chain     tmamp01l
001941171121     c                   if        not %found(tmamp01l)
001942171121     c                   seton                                        96
001943171121     c                   endif
001944171122     c                   endif
001945171121     c                   move      varticolo     articolo         12
001946171122      *
001947171121     c                   if        vdata1 = 0 or vdata2 = 0
001948171123     C                   Z-ADD     0             data1             8 0
001949171123     C                   Z-ADD     20391231      data2             8 0
001950171122     c                   else
001951171121      *
001952171121     C* gira data1
001953171121     c                   move      vdata1        g02dat
001954171121     C                   MOVEL     *BLANK        G02ERR
001955171121     C                   CALL      'XSRDA8'
001956171121     C                   PARM                    WLBDA8
001957171121     c                   if        g02err <> '0'
001958171121     c                   seton                                        95
001959171121     c                   endif
001960171121     C                   Z-ADD     G02INV        data1             8 0
001961171121     C                   Z-ADD     G02dat        vdata1
001962171121     c
001963171121     C* gira data2
001964171121     c                   move      vdata2        g02dat
001965171121     C                   MOVEL     *BLANK        G02ERR
001966171121     C                   CALL      'XSRDA8'
001967171121     C                   PARM                    WLBDA8
001968171121     c                   if        g02err <> '0'
001969171121     c                   seton                                        94
001970171121     c                   endif
001971171121     C                   Z-ADD     G02INV        data2             8 0
001972171121     C                   Z-ADD     G02dat        vdata2
001973171121     c                   if        data1 > data2
001974171121     c                   seton                                        94
001975171121     c                   endif
001976171122      *
001977171122     c                   endif
001978171121     c                   endsr
001979171121     c*-------------------------------------------------
001980171121     c     comandi       begsr
001981171121     c*-------------------------------------------------
001982171121      *
001983171121     C                   z-add     99            luncmd
001984171121     C                   CALL      'QCMDEXC'                            34
001985171121     C                   parm                    coman
001986171121     C                   parm                    luncmd
001987171121      *
001988171121     c                   endsr
013433171121     C**********************************************************************
013434171121     C     sqlerror      begsr
013435171121     C**********************************************************************
013436171121     C*
013437171121     C                   call      'X66CHGJOB'
013438171121     C                   seton                                        lr
013439171121     C                   ENDSR
019300171121     c*--------------------------------------------------------------
019400171121      *---------------------------------------------------------
020100171121**  CMD1
020200171121CHKOBJ  OBJ(WRKQUERY/wfmatc0f) OBJTYPE(*FILE)
020300171121**  CMD2
020400171121CRTDUPOBJ OBJ(wfmatc0f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(wfmatc0f) DATA(*YES)
020500171121**  CMD2
020600171121CPYF FROMFILE(wfmatc0f) TOFILE(WRKQUERY/wfmatc0f) MBROPT(*REPLACE)
020700171121**  CMD4
020800171121CLRPFM    FILE(wfmatc0F)
