000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200090928     H* TIST67R *----------------------------------------------------*
000300110221     H* Controllo Cap spedizioni -  Filtro                           *
000400000000     H*--------------------------------------------------------------*
000500110221     FFNLV76d   CF   E             WORKSTN
000600090928     ftabel00f  if   e           k disk
000601110223     FCNACO00F  IF   E           K DISK
000700941223     D*
000800900517     D KPJBA         E DS
000900051114     D*-------------------
001000051114     D* DS PARAMETRI DI LANCIO PGM
001100051114     D*-------------------
001200110221     d fnlv76ds      e DS
001300090928
001400090928     d Tibs34ds      e ds
001900090928     d Azuteds       e ds                  Extname(Azute00f)
002000090928     d dDatiute      e ds
002001110223     d tibs10ds      e ds
002002110223
002003110223     d ds$3          e ds
002500090928
002600090928     d wlbdat          ds
002700090928     d  g02dat                 1      8  0
002800090928     d  g02inv                 9     16  0
002900090928     d  g02err                17     17
003000090928     d  g02tgi                18     22  0
004300051114     d
004301110223     d codut           s                   like(acoKut) inz(1)
004400090928     D comdal          S                   LIKE(viddal) inz
004500090928     D comal           S                   LIKE(viddal) inz
004800090928     d dataisoda       s               d   datfmt(*iso)
004900090928     d dataisoa        s               d   datfmt(*iso)
005300090928     D kcod            S                   LIKE(tblcod)
005400090928     D kkey            S                   LIKE(tblkey)
006100090928      *   S C H I E R E
006200110221     d msg             s             79    dim(07) ctdata perrcd(1)
006201110225      * - Parametri x QCMDEXC
006202110225     d Qcmd            s            100    inz
006203110225     d Qlen            s             15  5 inz(100)
006500000000     C*---------------------------------------------------------------*
006600920925     C*
006700090928     c                   exsr      sr_inzd01
006800000615     C*
006900900509     C     FOR01         TAG
007000110223     C                   EXFMT     lv76D01
007100090928     c                   Eval      *In28 = *Off
007200090928     c                   Eval      *In90 = *Off
007300090928     c                   Clear                   vidmsg
007400941223     C*
007500941223     C** CMD3 - FINE LAVORO
007600110221     C                   if        *inkc
007700090928     c                   goto      fine
007800090928     c                   endif
007801110225      *
007802110225     c                   if        *inki
007803110225     c                   exsr      F09D01
007804110225     c                   goto      for01
007805110225     c                   endif
007900900524     C* CONTROLLO
008000900524     C                   EXSR      CONTR
008100941223     C  NKF
008200900524     COR 90              GOTO      FOR01
008300101110     C** F6 - ELABORAZIONE
008400110221     c                   clear                   fnlv76ds
008800110221     C                   movel     comDal        lv76aas
008900110221     C                   move      comDal        lv76mgs
008901110221     C                   movel     comAl         lv76aasa
008902110221     C                   move      comAl         lv76mgsa
009200110223     c                   z-add     vidksc        lv76ksc
009300110221     c                   movel     vidkscu       lv76kscu
009301110223     c                   movel     viduni        lv76uni
010200000606     C                   CLEAR                   KPJBU
010300110221     C                   MOVEL     fnlv76ds      KPJBU
010400110221     C                   MOVEL     'LV77'        KCOAZ
010500900523     C                   CALL      'BCH10'
010600900523     C                   PARM                    KPJBA
010700920925     C*
010800920925     C     FINE          TAG
010900000000     C                   SETON                                        LR
011000090928     c*------------------------------------------------------------------------
011100090928     c     *inzsr        begsr
011200090928     C     *ENTRY        PLIST
011300090928     C                   PARM                    KPJBA
011400090928     c     *dtaara       define    §azute        azuteds
011500090928     c     *dtaara       define    §datiute      ddatiute
011600090928     c                   in(E)     *dtaara
011700090928     c                   If        %error  or RSUT = *blanks
011800090928     c                   CLEAR                   tibs34ds
011900090928     c                   CALL      'TIBS34R'
012000090928     c                   PARM                    tibs34ds
012100090928     c                   in        *dtaara
012200090928     c                   EndIf
012300091007     c*
021100090928     c*
021200090928     c* reperimento data ultima pulizia titasp
021300090928     c                   clear                   ds$3
021400090928     C                   MOVEL     '$3'          KCOD
021500090928     C                   MOVEL     '1       '    KKEY
021600090928     c     ktab          chain     tabel00f
021700090928     c                   if        %found(tabel00f)
021800090928     c                   movel     tbluni        ds$3
021900090928     c                   endif
022000090928     C*  KLIST
022100090928     C     KTAB          KLIST
022200110223     C                   KFLD                    codut
022300090928     C                   KFLD                    KCOD
022400090928     C                   KFLD                    KKEY
022401110223     C     KACO          KLIST
022402110223     C                   KFLD                    codut
022403110223     C                   KFLD                    dutkci
022404110223     C                   KFLD                    Vidksc
022500090928     c                   endsr
022600090928     C*
022700090928     C*--- Inizializzazione campi del video --------------------------*
022800090928     C     sr_inzd01     BEGSR
022900090928     c                   clear                   viddal
023000090928     c                   clear                   vidal
023300110221     c                   clear                   vidksc
023400110221     c                   clear                   vidkscd
023500110221     c                   eval      vidkscu='S'
024200090928     c                   endsr
024201110225      *---------------------------------------------------------------*
024202110225      *?Gestione tasto funzionale F09 da videata D01                 ?*
024203110225      *---------------------------------------------------------------*
024204110225     c     F09D01        BEGSR
024205110225      *
024206110225     c                   eval      Qcmd    =  'DSPDBF FILE'
024207110225     c                                     +  '(*LIBL/WFEIB00F) '
024208110225     c                                     +  'MBR(*FIRST) '
024209110225     c***                                  +  'RCDSLT(*YES) '
024210110225     c                                     +  'OUTPUT(*) '
024211110225     c                   call      'QCMDEXC'                            90
024212110225     c                   parm                    Qcmd
024213110225     c                   parm                    Qlen
024214110225      *
024223110225      *
024224110225     c                   ENDSR
024300900524     C*
024400900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
024500900524     C     CONTR         BEGSR
024600110223     C                   SETOFF                                       404144
024601110223     C                   SETOFF                                       51
024602110223     c                   clear                   viduni
024900110223     C* DATA Dal
025000090928     c                   clear                   wlbdat
025100090928     C                   MOVE      VIDDal        G02DAT
025200920925     C                   MOVEL     *BLANK        G02ERR
025300941223     C                   CALL      'XSRDA8'
025400920925     C                   PARM                    WLBDAT
025500920925     C     G02ERR        IFEQ      '1'
025600090928     C                   SETON                                        402890
025700090928     c                   eval      vidmsg=msg(02)
025800920925     C                   GOTO      ENDCTR
025900920925     C                   END
026000090928     C                   Z-ADD     G02INV        comdal
026100110223     C*
026200090928     C                   Z-ADD     G02DAT        VIDDal
026300110223     C* Data Al
026500090928     c                   clear                   wlbdat
026600090928     C                   MOVE      VIDal         G02DAT
026700051114     C                   MOVEL     *BLANK        G02ERR
026800051114     C                   CALL      'XSRDA8'
026900051114     C                   PARM                    WLBDAT
027000051114     C     G02ERR        IFEQ      '1'
027100090928     C                   SETON                                        412890
027200090928     c                   eval      vidmsg=msg(02)
027300051114     C                   GOTO      ENDCTR
027400051114     C                   END
027500090928     C                   Z-ADD     G02INV        comal
027600110223     C*
027700090928     C                   Z-ADD     G02DAT        VIDal
028100941223     C*
028200900524     C* DATA DAL NO MAGGIORE DATA AL
028300090928     C     comdal        IFGT      comal
028400090928     C                   SETON                                        409028
028500090928     c                   eval      vidmsg= msg(03)
028600900524     C                   GOTO      ENDCTR
028700900524     C                   END
030900090928     c* errore se superati i giorni di pulizia di titsp per il periodo richiesto
031000090928     c                   if        comdal<=§$3dp0
031100090928     C                   SETON                                        409028
031200090928     c                   eval      vidmsg= msg(06)
031300090928     c                   goto      endctr
031400090928     c                   endif
042800101110     c
042900110223     c* Codice cliente
043000090929     c* interrogazione (?)
043100110223    1c                   if        vidksc= 0
043101110223    2c                   if        vidkscd<>*blanks
043102110223     C* PARSTA = 9  ESCLUDO ANNULLATI
043103110223     C                   Z-ADD     9             PARSTA
043104110223     C                   MOVEL     RSUT          PARDUT           30
043105110223     C                   MOVEL     *BLANKS       DESCR            48
043106110223     C                   MOVEL     vidkscd       DESCR
043107110223     C                   Z-ADD     DUTKCI        CCC               4 0
043108110223     C                   CLEAR                   PARFLR
043109110223     C                   CLEAR                   PARDIT
043110110223     C                   Z-ADD     1             PAXNUM
043111110223     C                   CALL      'XALFA3BR'
043112110223     C                   PARM                    PARDUT
043113110223     C                   PARM                    CODUT
043114110223     C                   PARM                    DESCR
043115110223     C                   PARM                    CCC
043116110223     C                   PARM                    PARSTA            1 0
043117110223     C                   PARM                    PARFLR           90
043118110223     C                   PARM                    PARDIT            3
043119110223     C                   PARM                    PAXNUM            2 0
043120110223     C                   PARM                    PAXKCM           80
043121110223     C                   PARM                    PAXKSM          140
043122110223     C                   PARM                    PAXKDM           60
043123110223    3C     PARSTA        IFNE      -1
043124110223     C                   MOVEL     PAXKSM        Vidksc
043125110223     C                   MOVEL     DESCR         Vidkscd
043126110223     C                   ELSE
043127110223     C                   CLEAR                   Vidksc
043128110223    3C                   ENDIF
043129110223     C*
044200090929     C                   EVAL      *IN90=*ON
044300090929     C                   GOTO      ENDctr
044400110223    2c                   endif
044415110223   x1c                   else
044416110223     C* CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
044418110223     c                   mllzo     1             vidksc
044419110223     C     KACO          CHAIN     CNACO000
044420110223     c                   if        %found(cnaco00f) and acoflg=*blanks
044425110223     C                   MOVEL     ACORAG        Vidkscd
044426110223     C                   ELSE
044427110223     C                   CLEAR                   Vidkscd
044428110223     c                   eval      vidmsg= msg(07)
044429110223     C                   SETON                                        904428
044430110223     C                   GOTO      ENDCTR
044431110223     C                   END
044432110223    1c                   endif
044433110223     c* Se tipo lancio cliente unificante lo cerco ee lo emetto a video
044434110223    1c                   if        vidkscu='U'
044435110223     c* verifico se codice è figlio
044436110223     c                   clear                   tibs10ds
044437110223     c                   eval      d10tle='ST'
044438110223     c                   eval      d10paf='P'
044439110223     c                   eval      d10cod=vidksc
044440110223     c                   call      'TIBS10R'
044441110223     c                   parm                    tibs10ds
044442110223    2c                   if        d10cop=0
044443110223     c* se non è figlio verifico se è padre
044444110223     c                   clear                   tibs10ds
044445110223     c                   eval      d10tle='ST'
044446110223     c                   eval      d10paf='F'
044447110223     c                   eval      d10cod=vidksc
044448110223     c                   call      'TIBS10R'
044449110223     c                   parm                    tibs10ds
044450110223    2c                   endif
044451110223    2c                   if        d10cop>0
044452110223     c                   eval      viduni=d10cop
044453110225     c****               seton                                        51
044457110223    2c                   endif
044458110223    1c                   endif
051500941223     C*
051600900524     C     ENDCTR        ENDSR
051700920925     C*
051800090928**
051900110223
052000091001Data errata o mancante                                                        02
052100090928Data al minore di data dal                                                    03
052200110221
052300110221
052400090928Periodo non elaborabile per mancanza di dati                                  06
052500090928Codice Commerciale errato o mancante                                          07
