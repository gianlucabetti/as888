000100170518     H DECEDIT('0,') DATEDIT(*YMD.)
000200170518      *
000300170518      *  Aggiorna Tabella aig con nuovi obiettivi
000500170518      *--------------------------------------------------------------*
000600170518     Ftntbe01L  uF A E           K DISK
000601170912     Ftntbex1L  uF A E           K DISK    rename(tntbe000:tntbex) prefix(x)
000700170522     Fwfaig00f  iF   E             DISK    usropn
000800170518
001000170518      *
001001170518     D kpjba         E DS
001200170519     D daig          E DS
001201170518     D WLBDA8          DS
001202170518     D  G02DAT                 1      8  0
001203170518     D  G02INV                 9     16  0
001204170518     D  G02ERR                17     17
001205170518     D  G02TGI                18     22  0
001206170619     d dataiso         s               d
001600170518
001601170518     D COD             S                   like(tbeCOD)                         QCAEXEC
001602170518     D KE1             S                   like(tbeKE1)                         QCAEXEC
001603170518     D KE2             S                   like(tbeKE2)                         QCAEXEC
001604170605     D Cmd             S             70                                         QCAEXEC
001605170605     D Cmd1            S             70    dim(1) ctdata perrcd(1)              QCAEXEC
001606170605     D Cmd2            S             70    dim(1) ctdata perrcd(1)              QCAEXEC
002100170518     C*---------------------------------------------------------------*
002200170518     C     *ENTRY        PLIST
002300170518     C                   PARM                    kpjba
002301170518     C                   TIME                    W0120            14 0
002302170518     C                   MOVE      W0120         WDAT              8 0
002303170518     C*
002304170518     C                   Z-ADD     WDAT          G02DAT
002305170518     C                   MOVEL     *BLANK        G02ERR
002306170518     C                   CALL      'XSRDA8'
002307170518     C                   PARM                    WLBDA8
002308170518     C* UDATE A 8 IN AAAA/MM/GG
002309170518     C                   Z-ADD     G02INV        WUDA              8 0
002310170619     C                   move      wuda          dataiso
002311170619     c                   subdur    1:*d          dataiso
002312170619     c                   move      dataiso       wuda
002313170619      *
006200170518     C     Ktbe          KLIST
006300170518     C                   KFLD                    COD
006400170518     C                   KFLD                    KE1
006500170518     C                   KFLD                    KE2
006501170518     c                   move      'AIG'         cod
006502170518     c                   movel     20391231      ke2
006503170522      *
006506170606     C                   movea     cmd1          cmd
006507170606     c                   exsr      comando
006508170529     c                   open      wfaig00f
006700170518     C*---------------------------------------------------------------*
006701170518     c     *start        setll     wfaig00f
006702170518     c                   do        *hival
006703170518     c                   read      wfaig00f
006704170518     c                   if        %eof(wfaig00f)
006705170518     c                   leave
006706170518     c                   endif
006707170522     c                   eval      ke1 = %editc(wffil:'X') + 'O'
006708170518     c                             + wfter + wfmas + wftat
007400170518     C     Ktbe          chain     tntbe01l
007401170518      *se trova il rekord lo aggiorna con la data facendolo scadere
007700170518    2C                   if        %found(tntbe01l)
007701170619     c                   movel     tbeke2        wudaex            8 0
007702170619     c                   if        wudaex > wuda
007703170518     c                   movel     wuda          tbeke2
007704170912     c*m                 clear                   tbeftr
007705170912     c*m                 clear                   tbedtr
007706170518     c                   update    tntbe000
007708170518     c                   endif
007709170912      *aggiorna filiale in diretta
007710170912     C     Ktbe          chain     tntbex1l
007712170912    2C                   if        %found(tntbex1l)
007715170912     c                   movel     wuda          xtbeke2
007718170912     c                   update    tntbex
007720170912     c                   endif
007721170912      *
007722170912     c                   endif
007723170518      * poi scrive comunque il nuovo
007724170518     c                   clear                   tntbe000
007725170518     c                   movel     'BI'          tbeapl
007726170518     c                   move      'AIG'         tbecod
007727170518     c                   movel     ke1           tbeke1
007728170518     c                   movel     '20391231'    tbeke2
007729170913     c                   move      'S'           tbeftt
007730170522     c                   eval      D你IGIMP   = 0
007731170721     c                   eval(h)   D你IGQTAS  = wfqtas
007732170522     c                   eval      D你IGQTAV  = 0
007733170519     c                   movel     daig          tbeuni
007734170518     c                   write     tntbe000
007735170912      * filiale
007736170912     c                   clear                   tntbex
007737170912     c                   movel     'BI'          xtbeapl
007738170912     c                   move      'AIG'         xtbecod
007739170912     c                   movel     ke1           xtbeke1
007740170912     c                   movel     '20391231'    xtbeke2
007741170913     c                   move      'S'           xtbeftt
007742170912     c                   eval      D你IGIMP   = 0
007743170912     c                   eval(h)   D你IGQTAS  = wfqtas
007744170912     c                   eval      D你IGQTAV  = 0
007745170912     c                   movel     daig          xtbeuni
007746170912     c                   write     tntbex
007747170518     c                   enddo
016100170518     C*
016101170605     C*                  movea     cmd2          cmd
016102170605     c*                  exsr      comando
016500170518     C                   seton                                        lr
016501170522      *---------------------------------------------------------------------
016502170522     C     comando       BEGSR
016503170522      *---------------------------------------------------------------------
016517170522      * cpyf
016519170522     C                   movel(p)  cmd           comman
016520170522     C                   call      'QCMDEXC'
016521170522     C                   parm                    comman           80
016522170605     C                   parm      70            lung             15 5
016523170522     C*
016525170522     C                   ENDSR
016600170522**         CMD1
016700170605CPYF FROMFILE(WFaig) TOFILE(wfaig00f) mbropt(*replace) FMTOPT(*NOCHK)
016800170529**         CMD2
016900170529DLTF FILE(WFAIG)
