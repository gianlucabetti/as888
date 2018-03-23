000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200090928     H* TIST67R *----------------------------------------------------*
000300110221     H* Controllo Cap spedizioni -  Filtro                           *
000400000000     H*--------------------------------------------------------------*
000500110221     FFNLV76d   CF   E             WORKSTN
000600090928     ftabel00f  if   e           k disk
000700110223     FCNACO00F  IF   E           K DISK
000800941223     D*
000900900517     D KPJBA         E DS
001000051114     D*-------------------
001100051114     D* DS PARAMETRI DI LANCIO PGM
001200051114     D*-------------------
001300110221     d fnlv76ds      e DS
001400090928
001500090928     d Tibs34ds      e ds
001600090928     d Azuteds       e ds                  Extname(Azute00f)
001700090928     d dDatiute      e ds
001800110223     d tibs10ds      e ds
001900110223
002000110223     d ds$3          e ds
002100090928
002200090928     d wlbdat          ds
002300090928     d  g02dat                 1      8  0
002400090928     d  g02inv                 9     16  0
002500090928     d  g02err                17     17
002600090928     d  g02tgi                18     22  0
002700051114     d
002800110223     d codut           s                   like(acoKut) inz(1)
002900090928     D comdal          S                   LIKE(viddal) inz
003000090928     D comal           S                   LIKE(viddal) inz
003100090928     d dataisoda       s               d   datfmt(*iso)
003200090928     d dataisoa        s               d   datfmt(*iso)
003300090928     D kcod            S                   LIKE(tblcod)
003400090928     D kkey            S                   LIKE(tblkey)
003500090928      *   S C H I E R E
003600110221     d msg             s             79    dim(07) ctdata perrcd(1)
003700110225      * - Parametri x QCMDEXC
003800110225     d Qcmd            s            100    inz
003900110225     d Qlen            s             15  5 inz(100)
004000000000     C*---------------------------------------------------------------*
004100920925     C*
004200090928     c                   exsr      sr_inzd01
004300000615     C*
004400900509     C     FOR01         TAG
004500110223     C                   EXFMT     lv76D01
004600090928     c                   Eval      *In28 = *Off
004700090928     c                   Eval      *In90 = *Off
004800090928     c                   Clear                   vidmsg
004900941223     C*
005000941223     C** CMD3 - FINE LAVORO
005100110221     C                   if        *inkc
005200090928     c                   goto      fine
005300090928     c                   endif
005400110225      *
005500110225     c                   if        *inki
005600110225     c                   exsr      F09D01
005700110225     c                   goto      for01
005800110225     c                   endif
005900900524     C* CONTROLLO
006000900524     C                   EXSR      CONTR
006100941223     C  NKF
006200900524     COR 90              GOTO      FOR01
006300101110     C** F6 - ELABORAZIONE
006400110221     c                   clear                   fnlv76ds
006500110221     C                   movel     comDal        lv76aas
006600110221     C                   move      comDal        lv76mgs
006700110221     C                   movel     comAl         lv76aasa
006800110221     C                   move      comAl         lv76mgsa
006900110223     c                   z-add     vidksc        lv76ksc
007000110221     c                   movel     vidkscu       lv76kscu
007100110223     c                   movel     viduni        lv76uni
007200000606     C                   CLEAR                   KPJBU
007300110221     C                   MOVEL     fnlv76ds      KPJBU
007400111219     c*
007500111219     c                   if        vidnew=' '
007600110221     C                   MOVEL     'LV77'        KCOAZ
007700900523     C                   CALL      'BCH10'
007800900523     C                   PARM                    KPJBA
007900111219     c                   else
008000111219     C                   MOVEL     'LV78'        KCOAZ
008100111219     C                   CALL      'FNLV78R'
008200111219     C                   PARM                    KPJBA
008300111219     c                   endif
008400920925     C*
008500920925     C     FINE          TAG
008600000000     C                   SETON                                        LR
008700090928     c*------------------------------------------------------------------------
008800090928     c     *inzsr        begsr
008900090928     C     *ENTRY        PLIST
009000090928     C                   PARM                    KPJBA
009100090928     c     *dtaara       define    §azute        azuteds
009200090928     c     *dtaara       define    §datiute      ddatiute
009300090928     c                   in(E)     *dtaara
009400090928     c                   If        %error  or RSUT = *blanks
009500090928     c                   CLEAR                   tibs34ds
009600090928     c                   CALL      'TIBS34R'
009700090928     c                   PARM                    tibs34ds
009800090928     c                   in        *dtaara
009900090928     c                   EndIf
010000091007     c*
010100090928     c*
010200090928     c* reperimento data ultima pulizia titasp
010300090928     c                   clear                   ds$3
010400090928     C                   MOVEL     '$3'          KCOD
010500090928     C                   MOVEL     '1       '    KKEY
010600090928     c     ktab          chain     tabel00f
010700090928     c                   if        %found(tabel00f)
010800090928     c                   movel     tbluni        ds$3
010900090928     c                   endif
011000090928     C*  KLIST
011100090928     C     KTAB          KLIST
011200110223     C                   KFLD                    codut
011300090928     C                   KFLD                    KCOD
011400090928     C                   KFLD                    KKEY
011500110223     C     KACO          KLIST
011600110223     C                   KFLD                    codut
011700110223     C                   KFLD                    dutkci
011800110223     C                   KFLD                    Vidksc
011900090928     c                   endsr
012000090928     C*
012100090928     C*--- Inizializzazione campi del video --------------------------*
012200090928     C     sr_inzd01     BEGSR
012300111219     c                   clear                   vidnew
012400111219     c                   clear                   viddal
012500090928     c                   clear                   vidal
012600110221     c                   clear                   vidksc
012700110221     c                   clear                   vidkscd
012800110221     c                   eval      vidkscu='S'
012900090928     c                   endsr
013000110225      *---------------------------------------------------------------*
013100110225      *?Gestione tasto funzionale F09 da videata D01                 ?*
013200110225      *---------------------------------------------------------------*
013300110225     c     F09D01        BEGSR
013400110225      *
013500110225     c                   eval      Qcmd    =  'DSPDBF FILE'
013600110225     c                                     +  '(*LIBL/WFEIB00F) '
013700110225     c                                     +  'MBR(*FIRST) '
013800110225     c***                                  +  'RCDSLT(*YES) '
013900110225     c                                     +  'OUTPUT(*) '
014000110225     c                   call      'QCMDEXC'                            90
014100110225     c                   parm                    Qcmd
014200110225     c                   parm                    Qlen
014300110225      *
014400110225      *
014500110225     c                   ENDSR
014600900524     C*
014700900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
014800900524     C     CONTR         BEGSR
014900110223     C                   SETOFF                                       404144
015000110223     C                   SETOFF                                       51
015100110223     c                   clear                   viduni
015200110223     C* DATA Dal
015300090928     c                   clear                   wlbdat
015400090928     C                   MOVE      VIDDal        G02DAT
015500920925     C                   MOVEL     *BLANK        G02ERR
015600941223     C                   CALL      'XSRDA8'
015700920925     C                   PARM                    WLBDAT
015800920925     C     G02ERR        IFEQ      '1'
015900090928     C                   SETON                                        402890
016000090928     c                   eval      vidmsg=msg(02)
016100920925     C                   GOTO      ENDCTR
016200920925     C                   END
016300090928     C                   Z-ADD     G02INV        comdal
016400110223     C*
016500090928     C                   Z-ADD     G02DAT        VIDDal
016600110223     C* Data Al
016700090928     c                   clear                   wlbdat
016800090928     C                   MOVE      VIDal         G02DAT
016900051114     C                   MOVEL     *BLANK        G02ERR
017000051114     C                   CALL      'XSRDA8'
017100051114     C                   PARM                    WLBDAT
017200051114     C     G02ERR        IFEQ      '1'
017300090928     C                   SETON                                        412890
017400090928     c                   eval      vidmsg=msg(02)
017500051114     C                   GOTO      ENDCTR
017600051114     C                   END
017700090928     C                   Z-ADD     G02INV        comal
017800110223     C*
017900090928     C                   Z-ADD     G02DAT        VIDal
018000941223     C*
018100900524     C* DATA DAL NO MAGGIORE DATA AL
018200090928     C     comdal        IFGT      comal
018300090928     C                   SETON                                        409028
018400090928     c                   eval      vidmsg= msg(03)
018500900524     C                   GOTO      ENDCTR
018600900524     C                   END
018700090928     c* errore se superati i giorni di pulizia di titsp per il periodo richiesto
018800090928     c                   if        comdal<=§$3dp0
018900090928     C                   SETON                                        409028
019000090928     c                   eval      vidmsg= msg(06)
019100090928     c                   goto      endctr
019200090928     c                   endif
019300101110     c
019400110223     c* Codice cliente
019500090929     c* interrogazione (?)
019600110223    1c                   if        vidksc= 0
019700110223    2c                   if        vidkscd<>*blanks
019800110223     C* PARSTA = 9  ESCLUDO ANNULLATI
019900110223     C                   Z-ADD     9             PARSTA
020000110223     C                   MOVEL     RSUT          PARDUT           30
020100110223     C                   MOVEL     *BLANKS       DESCR            48
020200110223     C                   MOVEL     vidkscd       DESCR
020300110223     C                   Z-ADD     DUTKCI        CCC               4 0
020400110223     C                   CLEAR                   PARFLR
020500110223     C                   CLEAR                   PARDIT
020600110223     C                   Z-ADD     1             PAXNUM
020700110223     C                   CALL      'XALFA3BR'
020800110223     C                   PARM                    PARDUT
020900110223     C                   PARM                    CODUT
021000110223     C                   PARM                    DESCR
021100110223     C                   PARM                    CCC
021200110223     C                   PARM                    PARSTA            1 0
021300110223     C                   PARM                    PARFLR           90
021400110223     C                   PARM                    PARDIT            3
021500110223     C                   PARM                    PAXNUM            2 0
021600110223     C                   PARM                    PAXKCM           80
021700110223     C                   PARM                    PAXKSM          140
021800110223     C                   PARM                    PAXKDM           60
021900110223    3C     PARSTA        IFNE      -1
022000110223     C                   MOVEL     PAXKSM        Vidksc
022100110223     C                   MOVEL     DESCR         Vidkscd
022200110223     C                   ELSE
022300110223     C                   CLEAR                   Vidksc
022400110223    3C                   ENDIF
022500110223     C*
022600090929     C                   EVAL      *IN90=*ON
022700090929     C                   GOTO      ENDctr
022800110223    2c                   endif
022900110223   x1c                   else
023000110223     C* CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
023100110223     c                   mllzo     1             vidksc
023200110223     C     KACO          CHAIN     CNACO000
023300110223     c                   if        %found(cnaco00f) and acoflg=*blanks
023400110223     C                   MOVEL     ACORAG        Vidkscd
023500110223     C                   ELSE
023600110223     C                   CLEAR                   Vidkscd
023700110223     c                   eval      vidmsg= msg(07)
023800110223     C                   SETON                                        904428
023900110223     C                   GOTO      ENDCTR
024000110223     C                   END
024100110223    1c                   endif
024200110223     c* Se tipo lancio cliente unificante lo cerco ee lo emetto a video
024300110223    1c                   if        vidkscu='U'
024400110223     c* verifico se codice è figlio
024500110223     c                   clear                   tibs10ds
024600110223     c                   eval      d10tle='ST'
024700110223     c                   eval      d10paf='P'
024800110223     c                   eval      d10cod=vidksc
024900110223     c                   call      'TIBS10R'
025000110223     c                   parm                    tibs10ds
025100110223    2c                   if        d10cop=0
025200110223     c* se non è figlio verifico se è padre
025300110223     c                   clear                   tibs10ds
025400110223     c                   eval      d10tle='ST'
025500110223     c                   eval      d10paf='F'
025600110223     c                   eval      d10cod=vidksc
025700110223     c                   call      'TIBS10R'
025800110223     c                   parm                    tibs10ds
025900110223    2c                   endif
026000110223    2c                   if        d10cop>0
026100110223     c                   eval      viduni=d10cop
026200110225     c****               seton                                        51
026300110223    2c                   endif
026400110223    1c                   endif
026500941223     C*
026600900524     C     ENDCTR        ENDSR
026700920925     C*
026800090928**
026900110223
027000091001Data errata o mancante                                                        02
027100090928Data al minore di data dal                                                    03
027200110221
027300110221
027400090928Periodo non elaborabile per mancanza di dati                                  06
027500090928Codice Commerciale errato o mancante                                          07
