000100170518     H DECEDIT('0,') DATEDIT(*YMD.)
000200170518      *
000300170518      *  Aggiorna Tabella aig con nuovi obiettivi
000500170518      *--------------------------------------------------------------*
000600170518     Ftntbe01L  uF A E           K DISK
000700170518     Fwfaig00f  iF   E             DISK    usropn
000800170518
001000170518      *
001001170518     D kpjba         E DS
001200170518     D tibs55ds      E DS
001201170518     D WLBDA8          DS
001202170518     D  G02DAT                 1      8  0
001203170518     D  G02INV                 9     16  0
001204170518     D  G02ERR                17     17
001205170518     D  G02TGI                18     22  0
001600170518
001601170518     D COD             S                   like(tbeCOD)                         QCAEXEC
001602170518     D KE1             S                   like(tbeKE1)                         QCAEXEC
001603170518     D KE2             S                   like(tbeKE2)                         QCAEXEC
001604170518     D Cmd             S             60                                         QCAEXEC
001606170518     D Cmd1            S             60    dim(1) ctdata perrcd(1)              QCAEXEC
001607170518     D Cmd2            S             60    dim(1) ctdata perrcd(1)              QCAEXEC
001608170518     D Cmd3            S             60    dim(1) ctdata perrcd(1)              QCAEXEC
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
006200170518     C     Ktbe          KLIST
006300170518     C                   KFLD                    COD
006400170518     C                   KFLD                    KE1
006500170518     C                   KFLD                    KE2
006501170518     c                   move      'AIG'         cod
006502170518     c                   movel     20391231      ke2
006700170518     C*---------------------------------------------------------------*
006701170518     c     *start        setll     wfaig00f
006702170518     c                   do        *hival
006703170518     c                   read      wfaig00f
006704170518     c                   if        %eof(wfaig00f)
006705170518     c                   leave
006706170518     c                   endif
006707170518     c                   eval      ke1 = %editc(wffil:'X') + wftas
006708170518     c                             + wfter + wfmas + wftat
007400170518     C     Ktbe          chain     tntbe01l
007401170518      *se trova il rekord lo aggiorna con la data facendolo scadere
007700170518    2C                   if        %found(tntbe01l)
007701170518     c                   movel     wuda          tbeke2
007702170518     c                   clear                   tbeftr
007703170518     c                   clear                   tbedtr
007704170518     c                   update    tntbe000
007708170518     c                   endif
007709170518      * poi scrive comunque il nuovo
007710170518     c                   clear                   tntbe000
007711170518     c                   movel     'BI'          tbeapl
007712170518     c                   move      'AIG'         tbecod
007713170518     c                   movel     ke1           tbeke1
007714170518     c                   movel     '20391231'    tbeke2
007715170518     c                   movel     wfdati        tbeuni
007716170518     c                   write     tntbe000
007717170518     c                   enddo
016100170518     C*
016500170518     C                   seton                                        lr
016700170518     C**************************************************************************
016800170518     C* PER FARLO FUNZIONARE SIA IN PROVA CHE IN BUONA
016900170518     C     *INZSR        begsr
017000170518     C* CERCO CONIL TIBS55 LA LIBRERIA DI GRUPPO DI SEDE
017100170518     C                   CLEAR                   tibs55ds
017200170518     c                   movel     'L'           i50tla
017300170518     c                   Movel     046           I50apo
017400170518     C                   CALL      'TIBS55R'
017500170518     C                   PARM                    tibs55ds
017600170518     C                   exsr      comando
018900170518     C                   ENDSR
018901170518      *---------------------------------------------------------------------
018902170518     C     comando       BEGSR
018903170518      *---------------------------------------------------------------------
018905170518     C                   movea     cmd1          cmd
018909170518     C                   movel(p)  cmd           comman
018910170518     c                   eval      %subst(comman:30 :10) = O50ALR
018911170518     C*                  call      'QCMDEXC'
018912170518     C*                  parm                    comman           80
018913170518     C*                  parm      60            lung             15 5
018914170518     C*
018916170518     C                   movea     cmd2          cmd
018920170518     C                   movel(p)  cmd           comman
018921170518     c                   eval      %subst(comman:27 :10) = O50ALR
018922170518     C*                  call      'QCMDEXC'
018923170518     C*                  parm                    comman
018924170518     C*                  parm      60            lung
018925170518      * cpyf
018926170518     C                   movea     cmd3          cmd
018927170518     C                   movel(p)  cmd           comman
018928170518     C                   call      'QCMDEXC'
018929170518     C                   parm                    comman           80
018930170518     C                   parm      60            lung             15 5
018931170518     C*
018932170518     c                   open      wfaig00f
018933170518     C                   ENDSR
020000170518     O**************************************************************************
020001170518**         CMD1
020002170518OVRDBF FILE(wfaig00f) TOFILE(gaitraazp /wfaig00f)
020003170518**         CMD2
020200170518OVRDBF FILE(wfaig) TOFILE(gaitraazp /wfaig)
020601170518**         CMD3
020700170518CPYF FROMFILE(WFaig) TOFILE(wfaig00f) mbropt(*replace)
