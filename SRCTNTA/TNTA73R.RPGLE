000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200060517      * TNTA73R *----------------------------------------------------*
000300060517      *        VISUALIZZAZIONE VARIAZIONI piano dei conti
000400940726      *--------------------------------------------------------------*
000500940726      *  Indicatori utilizzati:
000600060519      *  15   ON   visualizza opzione 5 in testaTA X SINTETICO
000700060519      *  16   ON   REVERS IMM. per identificare linea di analitico richiest
000800060519      *  20   ON   SFLDSP
000900940726      *  21   ON   SFLDSPCTL
001000940726      *  21   OFF  SFLCLR
001100940726      *  23   ON   SFLEND
001200060523      *  28   ON   emetto campo messaggio
001300060523      * 30/31 Lettura comodo
001400060519      *  50   non visualizza nell'analitico data e ora variazione se + righe
001500060530      *  51   sottilineatura divisione variazioni analitiche
001600060523      *  52   Proteggo campo selezione se non c'e' dettaglio
001700940726      *--------------------------------------------------------------*
001800060517     fcnacvt1l  if   e           k disk
001900060517     fcnacvd1l  if   e           k disk
002000030612     fCnAco00f  if   e           k disk
002100130905     fAZCMM01L  if   e           k disk
002200060522     ftntbe02l  if   e           k disk
002300060522     ftabel00f  if   e           k disk
002400060517     fTnta73d   cf   e             workstn sfile(TA73s01:NRR1)
002500060518     f                                     sfile(TA73S03:NRR3)
002600171228     dDESPRI           s             50    DIM(15) CTDATA PERRCD(1)
002700171228     dDESDOP           s             50    DIM(9) CTDATA PERRCD(1)
002800060523     dmsg              s             78    DIM(1) CTDATA PERRCD(1)
002900060523     d
003000060522     dDavar            s              1    DIM(9)
003100060522     dDavarEs          s              1    DIM(9)
003200060522     dyda              s              1    DIM(25)
003300060522     dydadeb           s              7    DIM(25)
003400030122
003500060522     d dacv_A        e ds
003600061030     d dacv_A3       e ds
003601171228     d dacv_A4       e ds
003700060522     d dacv_B        e ds
003800060522     d dacv_C        e ds
003900060522     d dacv_F        e ds
004000060522     d dacv_F2       e ds
004100100804     d dacv_I        e ds
004200060522     d dacv_P        e ds
004300060522     d dacv_S        e ds
004400060518     d Tnta73ds      e ds
004500030612     d kpjba         e ds
004600030612     d ddatiute      e ds
004700030612     d azuteds       e ds                  extname(AZUTE00F)
004800030612     d tibs34ds      e ds                  inz
004900060523     d tibs02ds      e ds                  inz
005000060522     d dyda          e ds
005100060522     d dtft          e ds
005200060522     d dsff          e ds
005300060522     d ds13          e ds
005400030612      *
005500060523     d wrecord         s              1
005600060523     d wemfor          s              1
005700060523     d sfl             s              1
005800060524     d emetti          s              1
005900060522     D Kke1            S                   LIKE(tbeke1)
006000060522     D KSIF            S                   LIKE(tbesif)
006100060522     D KLIN            S                   LIKE(tbelin)
006200060522     D KCODt           S                   LIKE(tbecod)
006300060522     D KCOD            S                   LIKE(tblcod)
006400060522     D Kkey            S                   LIKE(tblkey)
006500060523     D savsel          S                   LIKE(v1csel)
006600060530     D primasel        S                   LIKE(v1csel)
006700060522     d cortoprima      s              8
006800060522     d cortodopo       s              8
006900060523     d wdata           s              6  0
007000060522     d Dataiso         s               d   datfmt(*iso)
007100060522     d Datadmy         s               d   datfmt(*dmy)
007200130905      *
007300021212      *--------------------------------------------------------------*     *****
007400021212     c     *ENTRY        PLIST
007500060523     c                   PARM                    kpjba
007600060523     c                   movel     kpjbu         Tnta73Ds
007700021212      *
007800021212      *   P U L I Z I A   S U B F I L E
007900021212      *
008000021212      *
008100021212      * Riempio i campi del control
008200021212     c                   Exsr      RieCtl
008300060519     c* Caricamento sfl
008400060523     c                   eval      savsel='9'
008500060523     c                   eval      sfl='S'
008600060523     c                   dow       *inkl='0' and v1csel<>savsel
008700060523     c
008800060518     C                   EXSR      RieSfl
008900021212      *
009000021212      *  E M E T T O    S U B F I L E
009100021212      *
009200021218     C     NRR1          IFNE      0
009300060518     c                   seton                                        20
009400030722     C     *INKL         DOWEQ     *off
009500060523     c     v1csel        andeq     savsel
009600060523     c* Sintetico
009700060523     c                   if        sfl='S'
009800060523     c                   EXSR      EMISSSint
009900060523     c                   endif
010000060523     c* Analitico
010100060523     c                   if        sfl='A'
010200060523     c                   EXSR      EMISSAnal
010300060523     c                   endif
010400060518     c
010500060518     C                   ENDDO
010600021212     C                   ENDIF
010700060518     c
010800030122      * Subfile vuoto emetto una videata diversa
010900030122     c                   If        Nrr1 = *Zeros
011000060704     c                   Dow       Not *Inkl  and v1csel = savsel
011100060704     c     fornodati     tag
011200060518     C                   WRITE     TA73T01
011300060518     C                   WRITE     TA73Z01
011400060518     C                   WRITE     TA73D01
011500060704     c     fornodatiE    tag
011600060518     C                   EXFMT     TA73C01
011700060704     c                   setoff                                       28
011800060704     c                   clear                   wemfor
011900060704     c
012000060704     c                   if        not *inkl
012100060704     c* Controllo selezione
012200060704     c                   exsr      CTRSEL
012300060704     c* Riemetto tutto
012400060704     c                   if        wemfor='S'
012500060704     c                   goto      fornodati
012600060704     c                   endif
012700060704     c* Emetto per msg di errore
012800060704     c   28              goto      fornodatiE
012900060704     c                   EndIf
013000060704     c
013100030122     c                   EndDo
013200030122     c                   EndIf
013300060523     c                   EndDo
013400021212      *
013500030122     C                   SETON                                        LR
013600021212      **************************************************************************
013700021212      *    Riempio i campi del control
013800021212      **************************************************************************
013900021212     C     RieCtl        BegSr
014000030611      *
014100060518     C                   clear                   V1CKcc
014200060518     C                   clear                   V1CKsc
014300060518     C                   clear                   V1crag
014400021212      *
014500060518      * Codice
014600060518     C                   z-add     I73Kcc        V1CKcc
014700060518     C                   z-add     I73Ksc        V1CKsc
014800030612      *
014900030613      * Ragione sociale cliente
015000060518     C                   IF        I73Rag = *blanks
015100030612     C     KAco          chain     CnAco000                           34
015200030612     C                   IF        *in34 = *off and Acoflg =' '
015300060518     C                   MOVEL     AcoRag        V1crag
015400030612     C                   ENDIF
015500030613     C                   ELSE
015600030613      * campo passato in DS
015700060518     C                   MOVEL     I73Rag        V1Crag
015800030613     C                   ENDIF
015900030613     C
016000030612      *
016100021212     C                   EndSr
016200060523      **************************************************************************
016300060523      *    Emissione SFL Sintetico
016400060523      **************************************************************************
016500060523     c     EmissSint     BEGSR
016600060523     c                   setoff                                       ki
016700060523     c                   eval      sfl='S'
016800060523     c* F9 - cambio da analitico a sintetico
016900060523     C     *INKI         DOWEQ     *off
017000060523     C     *INKL         andeq     *off
017100060523     c     v1csel        andeq     savsel
017200060523     C                   Z-ADD     1             NRR1
017300060523     C                   Z-ADD     1             REC1
017400060524     c     for01t        tag
017500060523     C                   WRITE     TA73T01
017600060523     C                   WRITE     TA73Z01
017700060523     c     for01         tag
017800060523     C                   EXFMT     TA73C01
017900060523     c                   setoff                                       28
018000060523     c                   clear                   wemfor
018100060523     c
018200060530     c                   if        not *inkl
018300060523     c* Controllo selezione
018400060523     c                   exsr      CTRSEL
018500060524     c* Riemetto tutto
018600060523     c                   if        wemfor='S'
018700060524     c                   goto      for01t
018800060523     c                   endif
018900060524     c* Emetto per msg di errore
019000060524     c   28              goto      for01
019100060523     c
019200060523     c* Verifico se richiesta opzione 5
019300060523     c                   exsr      Leggisfl
019400060523     c                   endif
019500060530     c* F9 - cambio sfl in analitico
019600060523     c                   if        *inki
019700060523     c                   eval      sfl='A'
019800060523     c                   endif
019900060523     c*
020000060523     C                   ENDDO
020100060523     c                   ENDSR
020200060523      **************************************************************************
020300060523      *    Emissione SFL analitico
020400060523      **************************************************************************
020500060523     c     EmissAnal     BEGSR
020600060523     c                   setoff                                       ki
020700060523     c                   eval      sfl='A'
020800060523     C     *INKI         DOWEQ     *off
020900060523     C     *INKL         andeq     *off
021000060523     c     v1csel        andeq     savsel
021100060529     C  n16              Z-ADD     1             NRR3              4 0
021200060529     C  n16              Z-ADD     1             REC3
021300060524     c     for03t        tag
021400060523     C                   WRITE     TA73T01
021500060523     C                   WRITE     TA73Z03
021600060523     c     for03         tag
021700060523     C                   EXFMT     TA73C03
021800060523     c                   setoff                                       28
021900060523     c                   clear                   wemfor
022000060530     c                   if        not *inkl
022100060523     c* Controllo selezione
022200060523     c                   exsr      CTRSEL
022300060523     c                   if        wemfor='S'
022400060524     c                   goto      for03t
022500060523     c                   endif
022600060524     c   28              goto      for03
022700060523     c                   endif
022800060523     c* F9 - cambio sfl in  sintetico
022900060523     c                   if        *inki
023000060523     c                   eval      sfl='S'
023100060523     c                   endif
023200060523     C                   ENDDO
023300060523     c
023400060523     c                   ENDSR
023500021212      **************************************************************************
023600021212      *    Riempio i campi del subfile
023700021212      **************************************************************************
023800021212     c     RieSfl        BegSr
023900060523     c                   eval      *in20 = *off
024000060523     c                   eval      *in21 = *off
024100060523     c                   eval      *in23 = *off
024200060523      *
024300060523     c                   WRITE     TA73C01
024400060523     c                   WRITE     TA73C03
024500060523      *
024600060523     c                   eval      *in21 = *on
024700060523     c                   eval      *in23 = *on
024800060523      *
024900060523     c                   Z-ADD     0             NRR1              4 0
025000060523     c                   Z-ADD     0             NRR3              4 0
025100060522     c*
025200060519     c     kacvt         setll     cnacvt1l
025300060519     c     kacvt         reade     cnacvt1l
025400060519     c                   dow       not %eof(cnacvt1l)
025500060519     c*
025600060519     c                   if        v1csel=' '
025700060519     c                   eval      wrecord='S'
025800060519     c                   else
025900060519     c                   clear                   wrecord
026000060519     c                   endif
026100060518     c* Sintetico
026200060518     c
026300060518     c* Data  variazione PdC
026400060522     c     *iso          movel     acvdav        dataiso
026500060522     c                   movel     dataiso       datadmy
026600060522     c                   movel     datadmy       v1sdav
026700060530     c                   movel     datadmy       v3sdav
026800060518     c
028200060601     c* decodifico il pgm
028300060601     c                   Clear                   tibs02ds
028400060601     c                   Eval      T02Mod = 'C'
028500060601     c                   Eval      T02Sif = Knsif
028600060601     c                   Eval      T02Cod = 'YPG'
028700060601     c                   movel(P)  acvpgm        t02ke1
028800060601     c                   Call      'TIBS02R'
028900060601     c                   Parm                    Kpjba
029000060601     c                   Parm                    tibs02ds
029100060601If  2c                   If        T02Err <> *Blanks
029200060601     c                   eval      v1spgm=*all'?'
029300060601     c                   else
029400060601     c                   movel     t02uni        v1spgm
029500060601     c                   endif
029600060601     c
029700060601     c* Se inserimento scrivo da dove
029800060601     c                   clear                   v1sins
029900060601     c                   if        acvcta<>'M'
030000060601     c                   select
030100100804     C                   when      ACVcta='A'
030200100804     c                   eval      v1sins = despri(13)
030300060601     C                   when      ACVCTA='O'
030400060601     c                   movel     despri(9)     v1sins
030500060601     C                   when      ACVCTA='C'
030600060601     c                   movel     despri(10)    v1sins
030700060601     C                   OTHER
030800060601     c                   movel     despri(8)     v1sins
030900060601     c                   endsl
031000060601     c                   endif
031100030611      *
031200060518     c                   z-add     acvOrv        V1sOrv
031300060530     c                   z-add     acvOrv        V3sOrv
031400060518     c                   movel     acvPru        V1sPru
031500060530     c                   movel     acvPru        V3sPru
031600060518     c                   movel     acvNoj        V1sNoj
031700060530     c                   movel(P)  acvNoj        V3sNoj
031800060518     c* dati modificati
031900060518     c                   clear                   v1sana
032000060518     c                   clear                   v1sblc
032100060518     c                   clear                   v1scom
032200060518     c                   clear                   v1sfat
032300060518     c                   clear                   v1spgf
032400060518     c                   clear                   v1spgc
032500060518     c                   clear                   v1sgia
032600060518     c                   clear                   v1sdan
032700060518     c                   clear                   v1sope
032800060524     c                   clear                   DaVarEs
032900060522     c
033000060522     c                   movea     acvflo        DaVar
033100060522     c                   do        9             xx                2 0
033200060522     c                   if        daVar(XX)<>' '
033300060518     c*
033400060522     c                   if        DaVar(xx)='A'
033500060519     c
033600060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
033700060519     c                   eval      wrecord='S'
033800060519     c                   endif
033900060518     c                   eval      v1sana='SI'
034000060518     c                   endif
034100060519     c
034200060522     c                   if        DaVar(xx)='B'
034300060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
034400060519     c                   eval      wrecord='S'
034500060519     c                   endif
034600060518     c                   eval      v1sblc='SI'
034700060518     c                   endif
034800060519     c
034900060522     c                   if        DaVar(xx)='C'
035000060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
035100060519     c                   eval      wrecord='S'
035200060519     c                   endif
035300060518     c                   eval      v1scom='SI'
035400060518     c                   endif
035500060519     c
035600060522     c                   if        DaVar(xx)='F'
035700060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
035800060519     c                   eval      wrecord='S'
035900060519     c                   endif
036000060518     c                   eval      v1sfat='SI'
036100060518     c                   endif
036200060519     c
036300060522     c                   if        DaVar(xx)='P'
036400060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
036500060519     c                   eval      wrecord='S'
036600060519     c                   endif
036700060518     c                   eval      v1spgf='SI'
036800060518     c                   endif
036900060522     c
037000100804     c                   if        DaVar(xx)='S' or DaVar(xx) = 'I'
037100100805     c                             or DaVar(xx) = 'H'
037200060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
037300060519     c                   eval      wrecord='S'
037400060519     c                   endif
037500060518     c                   eval      v1spgc='SI'
037600060518     c                   endif
037700060519     c
037800060522     c                   if        DaVar(xx)='G'
037900060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
038000060519     c                   eval      wrecord='S'
038100060519     c                   endif
038200060518     c                   eval      v1sgia='SI'
038300060518     c                   endif
038400060519     c
038500060522     c                   if        DaVar(xx)='D'
038600060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
038700060519     c                   eval      wrecord='S'
038800060519     c                   endif
038900060518     c                   eval      v1sdan='SI'
039000060518     c                   endif
039100060519     c
039200060522     c                   if        DaVar(xx)='O'
039300060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
039400060519     c                   eval      wrecord='S'
039500060519     c                   endif
039600060518     c                   eval      v1sope='SI'
039700060518     c                   endif
039800060522     c
039900060522     c                   endif
040000060522     c                   enddo
040100060519     c
040200060519     c* Leggo e scrivo analitico
040300060519     c                   if        wrecord='S'
040400060519     c                   Exsr      SCRiana
040500060524     c
040600060524     c* Scrivo un record in analitico per dati variati non registrati
040700060524     c* solo per variazione
040800060524     c                   if        acvcta='M'
040900060524     c                   Exsr      Datinoregis
041000060524     c                   endif
041100060530     c
041200060530     c* visto che almeno una riga l'ho scritta in analitico, aggancio
041300060530     c*  l'ultima per sottolineare i campi
041400060530     c     nrr3          chain     ta73s03                            31
041500060530     c                   if        not *in31
041600060530     c                   movel     *off          *inkl
041700060530     c                   movel     in50          *in50
041800060530     c                   movel     in51          *in51
041900060530     c                   if        not *in50
042000060530     c                   clear                   v3spru
042100060530     c                   clear                   v3snoj
042200060530     c                   clear                   v3sorv
042300060530     c                   clear                   v3sdav
042400060530     c                   endif
042500060530     c                   seton                                        5051
042600060530     c                   movel     *in50         in50
042700060530     c                   movel     *in51         in51
042800060530     c                   update    ta73s03
042900060530     c                   endif
043000060530     c
043100060530     c
043200060523     c
043300060523     c* Se non ho caricato nessun record di analitico, proteggo la selezione
043400060523     c                   if        v1sreca=0
043500060523     c                   seton                                        52
043600060523     c                   endif
043700060523     c                   movel     *in52         in52
043800060518     c
043900060518     c                   add       1             nrr1
044000060518     c                   write     ta73s01
044100060523     c
044200060523     c                   setoff                                       52
044300060519     c                   endif
044400060519     c*
044500060519     c     kacvt         reade     cnacvt1l
044600060519     c                   enddo
044700060523     c
044800060523     c                   eval      savsel=v1csel
044900060518     c*
045000021212     c                   EndSr
045100060519      **************************************************************************
045200060519      *    Scrivo sfl analitico
045300060519      **************************************************************************
045400060519     C     Scriana       BEGSR
045500060522     c                   seton                                        50
045600060530     c                   setoff                                       51
045700060522     c                   setoff                                       16
045800060522     c                   clear                   v1sreca
045900060523     c
046000060523     c* Se non manutenzione non devo caricare la DS
046100100805    2c                   if        acvcta<>'M' and
046200100805     c                             %scan('I':acvflo) = 0 and
046300100805     c                             %scan('H':acvflo) = 0
046400060523     c                   exsr      car_ins
046500060523     c
046600060523   x2c                   else
046700060523     c
046800060522     c                   if        v1csel=' '
046900060519     c     kacvd         setll     cnacvd1l
047000060519     c     kacvd         reade     cnacvd1l
047100060522     c                   else
047200060522     c     kacvds        setll     cnacvd1l
047300060522     c     kacvds        reade     cnacvd1l
047400060522     c                   endif
047500060519     c
047600060522    1c                   dow       not %eof(cnacvd1l)
047700060522     c* Per record dati commerciali, per ora mi interessano soltanto
047800060522     c*  i primi 2 campi
047900060522     c                   if        acvyda='C'
048000060522     c                   movel     acvprima      cortoprima
048100060522     c                   movel     acvdopo       cortodopo
048200060522     c                   endif
048300060522     c
048400060522     c* Se nessun dato è stato modificato, significa che altri
048500060522     c*  dati della parte anagrafica sono stati modificat ma
048600060522     c*  non memorizzati, per cui inseriri in un unico record generico
048700060522    3c                   if        (acvyda<>'C' and acvprima=acvdopo) or
048800060523    cc                             (acvyda='C' and cortoprima=cortodopo)
048900060522     c                   z-add     1             xx
049000060522     c     acvyda        lookup    daVar(xx)                              30
049100060522    4c                   if        *in30
049200060522     c                   eval      DaVarEs(XX)='N'
049300060522    4c                   endif
049400060522   x3c                   else
049500060522     c
049600060522     c* Decodifico tipo dati variati
049700060522     c                   z-add     1             xx
049800060522     c     acvyda        lookup    yda(xx)                                30
049900060522     c   30              movel     ydadeb(xx)    v1sdt1
050000060522     c  n30              movel(p)  acvyda        v1sdt1
050100060522     c
050300060522     c
050400060522     c* Imposto la DS e per il primo record memorizzo in nrr nel sfl
050500060522     c*  sintetico
050600060522    4c                   select
050700060522     c                   when      acvyda='A'
050800061030     c                   if        acvprg=0
050900060522     c                   exsr      car_A
051000061030     c                   else
051001171228     c                   IF        acvprg = 3
051100061030     c                   exsr      car_A3
051101171228     c                   ENDIF
051102171228     c                   IF        acvprg = 4
051103171228     c                   exsr      car_A4
051104171228     c                   ENDIF
051200061030     c                   endif
051300060522     c                   when      acvyda='B'
051400060522     c                   exsr      car_B
051500060522     c                   when      acvyda='C'
051600060522     c                   exsr      car_C
051700060522     c                   when      acvyda='F'
051800060522     c                   if        acvprg=0
051900060522     c                   exsr      car_F
052000060522     c                   else
052100060522     c                   exsr      car_F2
052200060522     c                   endif
052300100805     c                   when      acvyda='I' or acvyda = 'H'
052400100804     c                   exsr      car_I
052500060522     c                   when      acvyda='P'
052600060522     c                   exsr      car_P
052700060522     c                   when      acvyda='S'
052800060522     c                   exsr      car_S
052900060522    4c                   endsl
053000060522     c
053100060522    3c                   endif
053200060522     c
053300060522    2c                   if        v1csel=' '
053400060519     c     kacvd         reade     cnacvd1l
053500060522     c                   else
053600060522     c     kacvds        reade     cnacvd1l
053700060522    2c                   endif
053800060522    1c                   enddo
053900060530     c* finita la scrittura
054000060523     c
054100060523    2c                   endif
054200060519     c
054300060519     c                   EndSr
054400060523      **************************************************************************
054500060523      *    SCRIVO RECORD IN SFL TA73S03
054600060523      **************************************************************************
054700060523     C     SCRIVIS03     BEGSR
054800060523     c                   add       1             nrr3
054900060523     c                   movel     *in50         in50
055000060530     c                   movel     *in51         in51
055100060523     c                   write     ta73s03
055200060523     c*
055300060523     c* Imposto se il primo nrr3
055400060523     c                   if        v1sreca=0
055500060523     c                   z-add     nrr3          v1sreca
055600060523     c                   endif
055700060524     c                   setoff                                       50
055800060524     c
055900060524     c* Memorizzo il record dati variati scritto in analitico
056000060524     c                   z-add     1             xx
056100060524     c     acvyda        lookup    daVar(xx)                              30
056200060524    4c                   if        *in30
056300060524     c                   eval      DaVarEs(XX)='S'
056400060524    4c                   endif
056500060523     C                   ENDSR
056600060524      **************************************************************************
056700060524      *    Scrivo un record in sfl analitico per dati variati non regis
056800060524      **************************************************************************
056900060524     C     datinoregis   BEGSR
057000060524     c                   clear                   v1sdt1
057200060524     c                   clear                   v1sva2
057300060524     c                   clear                   emetti
057400060524     c                   movel     despri(11)    v1sva1
057500060524     c                   z-add     1             xx
057600060524     c                   do        9             xx
057700060524     c                   if        DaVar(XX)<>' ' and DaVarEs(xx)<>'S'
057800060524     c
057900060524     c* Memorizzo se in questo record c'e' il tipo dati selezionato
058000060524     c                   if        (v1csel<>' ' and davar(xx)=v1csel) or
058100060524     c                              v1csel=' '
058200060524     c
058300060524     c                   z-add     1             yy                2 0
058400060524     c     davar(xx)     lookup    yda(yy)                                30
058500060524     c                   if        *in30
058600060524     c                   movel     ydadeb(yy)    des7a             7
058700060524     c                   else
058800060524     c                   movel     *all'?'       des7a
058900060524     c                   endif
059000060524     c* riempo stringa
059100060524     c                   eval      v1sva2=%trim(v1sva2) + ' '+ des7a + ' /'
059200060524     c                   endif
059300060524     c                   endif
059400060524     c                   enddo
059500060524     c
059600060524     c                   if        v1sva2<>*blanks
059700060530     c                   eval      v1sva2=%trim(v1sva2)
059800060524     c                   exsr      ScriviS03
059900060524     c                   endif
060000060524     c
060100060524     C                   ENDSR
060200060522      **************************************************************************
060300060522      *    Carico record inserimento
060400060522      **************************************************************************
060500060522     C     Car_ins       BEGSR
060600060522     C                   MOVEL     *blanks       V1SDT1
060800060522     c                   select
060900060522     C                   when      ACVCTA='O'
061000060524     c                   movel     despri(9)     v1sva2
061100060522     C                   when      ACVCTA='C'
061200060524     c                   movel     despri(10)    v1sva2
061300060522     C                   OTHER
061400060524     c                   movel     despri(8)     v1sva2
061500060522     c                   endsl
061600060524     c                   movel     *BLANKS       v1sva1
061700060530     c                   seton                                        51
061800060523     c
061900060523     c                   exsr      ScriviS03
062000060530     c                   setoff                                       51
062100060522     c
062200060522     c                   endsr
062300060522      **************************************************************************
062400060522      *    Scrivo sfl per tipo record "A"
062500060522      **************************************************************************
062600060522     C     Car_A         BEGSR
062700060522     c*
062800060522     c* Riga prima
062900060522     c                   movel     acvprima      dacv_A
063000060522     c                   movel     despri(1)     v1sva1
063100060522     c                   eval      %subst(v1sva1:15:16)=indiva
063200060928     c* flag annullamento
063300060928     c                   if        acoflg<>' '
063400060928     c                   eval      %subst(v1sva1:48:2)='SI'
063500060928     c                   else
063600060928     c                   eval      %subst(v1sva1:48:2)='  '
063700060928     c                   endif
063800060522     c* Riga dopo
063900060522     c                   movel     acvdopo       dacv_A
064000060522     c                   movel     desdop(1)     v1sva2
064100060522     c                   eval      %subst(v1sva2:15:16)=indiva
064200060928     c* flag annullamento
064300060928     c                   if        acoflg<>' '
064400060928     c                   eval      %subst(v1sva2:48:2)='SI'
064500060928     c                   else
064600060928     c                   eval      %subst(v1sva2:48:2)='  '
064700060928     c                   endif
064800060523     c
064900060523     c                   EXSR      ScriviS03
065000060522     c
065100060522     c                   ENDSR
065200061030      **************************************************************************
065300061030      *    Scrivo sfl per tipo record "A" progressivo 3
065400061030      **************************************************************************
065500061030     C     Car_A3        BEGSR
065600061030     c*
065700061030     c* Riga prima
065800061030     c                   movel     acvprima      dacv_A3
065900061030     c                   movel     despri(12)    v1sva1
066000061030     c                   eval      %subst(v1sva1:17:16)=indcdf
066100061030     c* Riga dopo
066200061030     c                   movel     acvdopo       dacv_A3
066300061030     c                   movel     desdop(8)     v1sva2
066400061030     c                   eval      %subst(v1sva2:17:16)=indcdf
066500061030     c
066600061030     c                   EXSR      ScriviS03
066700061030     c
066800061030     c                   ENDSR
066801171228      **************************************************************************
066802171228      *    Scrivo sfl per tipo record "A" progressivo 4
066803171228      **************************************************************************
066804171228     C     Car_A4        BEGSR
066805171228     c*
066806171228     c* Riga prima
066807171228     c                   movel     acvprima      dacv_A4
066808171228     c                   movel     despri(15)    v1sva1
066809171228     c                   eval      %subst(v1sva1:20:11)=(%editc(ACOlib:'X'))
066810171228     c* Riga dopo
066811171228     c                   movel     acvdopo       dacv_A4
066812171228     c                   movel     desdop(9)     v1sva2
066813171228     c                   eval      %subst(v1sva2:20:11)=(%editc(ACOlib:'X'))
066815171228     c
066816171228     c                   EXSR      ScriviS03
066817171228     c
066818171228     c                   ENDSR
066900060522      **************************************************************************
067000060522      *    Scrivo sfl per tipo record "B"
067100060522      **************************************************************************
067200060522     C     Car_B         BEGSR
067300060522     c*
067400060522     c* Riga prima
067500060522     c                   movel     acvprima      dacv_B
067600060522     c                   movel     despri(2)     v1sva1
067700060522     c                   eval      %subst(v1sva1:15:2)=§clpcon
067800060522     c                   eval      %subst(v1sva1:24:1)=acoabl
067900060522     c                   eval      %subst(v1sva1:34:3)=clpnar
068000060522     c                   if        atb02='N'
068100060928     c                   eval      %subst(v1sva1:50:1)='N'
068200060522     c                   else
068300060928     c                   eval      %subst(v1sva1:50:1)='S'
068400060522     c                   endif
068500060522     c* Riga dopo
068600060522     c                   movel     acvdopo       dacv_B
068700060522     c                   movel     desdop(2)     v1sva2
068800060522     c                   eval      %subst(v1sva2:15:2)=§clpcon
068900060522     c                   eval      %subst(v1sva2:24:1)=acoabl
069000060522     c                   eval      %subst(v1sva2:34:3)=clpnar
069100060522     c                   if        atb02='N'
069200060928     c                   eval      %subst(v1sva2:50:1)='N'
069300060522     c                   else
069400060928     c                   eval      %subst(v1sva2:50:1)='S'
069500060522     c                   endif
069600060523     c
069700060523     c                   EXSR      ScriviS03
069800060522     c
069900060522     c                   ENDSR
070000060522      **************************************************************************
070100060522      *    Scrivo sfl per tipo record "C"
070200060522      **************************************************************************
070300060522     C     Car_C         BEGSR
070400060522     c* Riga prima
070500060522     c                   movel     acvprima      dacv_C
070600060522     c                   movel     despri(3)     v1sva1
070700060522     c                   eval      %subst(v1sva1:21:1)=clpclv
070800060522     c                   eval      %subst(v1sva1:36:7)=(%editc(clpage:'X'))
070900060522     c* Decodifico agente
071000130905     c     CLPage        chain     AZCMM000
071100130905     c                   if        %found(AZCMM01L)
071200130905     c                   movel     CMMdes        des7a             7
071300060522     c                   else
071400060522     c                   movel     *all'?'       des7a
071500060522     c                   endif
071600060522     c                   eval      %subst(v1sva1:44:7)=des7a
071700060522     c* Riga dopo
071800060522     c                   movel     acvdopo       dacv_C
071900060522     c                   movel     desdop(3)     v1sva2
072000060522     c                   eval      %subst(v1sva2:21:1)=clpclv
072100060522     c                   eval      %subst(v1sva2:36:7)=(%editc(clpage:'X'))
072200060522     c* Decodifico agente
072300130905     c     CLPage        chain     AZCMM000
072400130905     c                   if        %found(AZCMM01L)
072500130905     c                   movel     CMMdes        des7a
072600060522     c                   else
072700060522     c                   movel     *all'?'       des7a
072800060522     c                   endif
072900060522     c                   eval      %subst(v1sva2:44:7)=des7a
073000060522     c
073100060523     c                   EXSR      ScriviS03
073200060522     c
073300060522     c                   ENDSR
073400060522      **************************************************************************
073500060522      *    Scrivo sfl per tipo record "F"
073600060522      **************************************************************************
073700060522     C     Car_F         BEGSR
073800060522     c*
073900060522     c* Riga prima
074000060522     c                   movel     acvprima      dacv_F
074100060522     c                   movel     despri(4)     des50a           50
074200060522     c                   exsr      DecodFattur
074300060522     c                   movel     des50a        v1sva1
074400060522     c* Riga dopo
074500060522     c                   movel     acvdopo       dacv_F
074600060522     c                   movel     desdop(4)     des50a
074700060522     c                   exsr      DecodFattur
074800060522     c                   movel     des50a        v1sva2
074900060522     c
075000060523     c                   EXSR      ScriviS03
075100060522     c
075200060522     c                   ENDSR
075300060522      **************************************************************************
075400060522      *    Scrivo sfl per tipo record "F" progressivo  2°
075500060522      **************************************************************************
075600060522     C     Car_F2        BEGSR
075700060522     c*
075800060522     c* Riga prima
075900060522     c                   movel     acvprima      dacv_F2
076000060522     c                   movel     despri(5)     v1sva1
076100060522     c* Numero protocollo interno
076200060522     c                   eval      %subst(v1sva1:8:7)=(%editc(indnpn:'Z'))
076300060522     c* Numero protocollo cliente
076400060522     c                   eval      %subst(v1sva1:24:7)=(%editc(indnpr:'Z'))
076500060522     c* Data protocollo cliente
076600060830     c                   if        inddpr>0
076700060522     c     *ymd          movel     inddpr        datadmy
076800060523     c                   movel     datadmy       des8a             8
076900060830     c                   else
077000060830     c                   clear                   des8a
077100060830     c                   endif
077200060830     c                   eval      %subst(v1sva1:43:8)= des8a
077300060830     c*****              eval      %subst(v1sva1:43:8)= (%editc(wdata:'Y'))
077400060522     c* Riga dopo
077500060522     c                   movel     acvdopo       dacv_F2
077600060522     c                   movel     desdop(5)     v1sva2
077700060522     c* Numero protocollo interno
077800060522     c                   eval      %subst(v1sva2:8:7)=(%editc(indnpn:'Z'))
077900060522     c* Numero protocollo cliente
078000060522     c                   eval      %subst(v1sva2:24:7)=(%editc(indnpr:'Z'))
078100060522     c* Data protocollo cliente
078200060830     c                   if        inddpr>0
078300060522     c     *ymd          movel     inddpr        datadmy
078400060523     c                   movel     datadmy       wdata
078500060523     c                   eval      %subst(v1sva2:43:8)= (%editc(wdata:'Y'))
078600060830     c                   else
078700060830     c                   clear                   des8a
078800060830     c                   eval      %subst(v1sva1:43:8)= des8a
078900060830     c                   endif
079000060522     c*
079100060523     c                   EXSR      ScriviS03
079200060522     c
079300060522     c                   ENDSR
079400100804
079500100804      /free
079600100804       //--------------------------------------------------------------
079700100804       //?Scrivo SBFL per tipo record "I".
079800100804       //--------------------------------------------------------------
079900100804       BEGSR Car_I;
080000100804
080100100804       //?Riga di come era prima
080200100804         clear dACV_I;
080300100805         CBAcod  = %subst(ACVprima:1:1);
080400100805         CBAiban = %subst(ACVprima:2:29);
080500100804         v1sva1 = despri(14);
080600100805         %subst(v1sva1:4:1) = CBAcod;
080700100805         %subst(v1sva1:15:4) = %subst(CBAiban:1:4);
080800100805         %subst(v1sva1:20:4) = %subst(CBAiban:5:4);
080900100805         %subst(v1sva1:25:4) = %subst(CBAiban:9:4);
081000100805         %subst(v1sva1:30:4) = %subst(CBAiban:13:4);
081100100805         %subst(v1sva1:35:4) = %subst(CBAiban:17:4);
081200100805         %subst(v1sva1:40:4) = %subst(CBAiban:21:4);
081300100805         %subst(v1sva1:45:4) = %subst(CBAiban:25:4);
081400100804
081500100804       //?Riga di come è dopo
081600100804         clear dACV_I;
081700100805         CBAcod  = %subst(ACVdopo:1:1);
081800100805         CBAiban = %subst(ACVdopo:2:29);
081900100804         v1sva2 = despri(14);
082000100805         %subst(v1sva2:4:1) = CBAcod;
082100100805         %subst(v1sva2:15:4) = %subst(CBAiban:1:4);
082200100805         %subst(v1sva2:20:4) = %subst(CBAiban:5:4);
082300100805         %subst(v1sva2:25:4) = %subst(CBAiban:9:4);
082400100805         %subst(v1sva2:30:4) = %subst(CBAiban:13:4);
082500100805         %subst(v1sva2:35:4) = %subst(CBAiban:17:4);
082600100805         %subst(v1sva2:40:4) = %subst(CBAiban:21:4);
082700100805         %subst(v1sva2:45:4) = %subst(CBAiban:25:4);
082800100804
082900100804         exsr  scrivis03;
083000100804
083100100804       ENDSR;
083200100804      /end-free
083300100804
083400060522      **************************************************************************
083500060522      *    Scrivo sfl per tipo record "P"
083600060522      **************************************************************************
083700060522     C     Car_P         BEGSR
083800060522     c*
083900060522     c* Riga prima
084000060522     c                   movel     acvprima      dacv_p
084100060523     c                   movel     despri(6)     v1sva1
084200060522     c                   eval      %subst(v1sva1:5:3)=§indcdp
084300060523     c* Decodifica
084400060523     c                   movel     §indcdp       des4a             4
084500060523     c                   move      '1'           des4a
084600060523     c                   movel     'FA'          kcod
084700060524     c                   movel(P)  des4a         kkey
084800060523     c     ktab          chain     tabel00f
084900060523     c                   if        %found(tabel00f)
085000080211     c                   movel     tbluni        desfa            28
085100060523     c                   else
085200080211     c                   movel     *all'?'       desfa
085300060523     c                   endif
085400080211     c                   eval      %subst(v1sva1:9:28)=desfa
085500080211     c**!!!              eval      %subst(v1sva1:23:16)=indccb
085600060522     c                   eval      %subst(v1sva1:40:5)=(%editc(indabi:'Z'))
085700060522     c                   eval      %subst(v1sva1:46:5)=(%editc(indcab:'Z'))
085800060522     c* Riga dopo
085900060522     c                   movel     acvdopo       dacv_P
086000060523     c                   movel     desdop(6)     v1sva2
086100060522     c                   eval      %subst(v1sva2:5:3)=§indcdp
086200060523     c* Decodifica
086300060523     c                   movel     §indcdp       des4a             4
086400060523     c                   move      '1'           des4a
086500060523     c                   movel     'FA'          kcod
086600060524     c                   movel(P)  des4a         kKEY
086700060523     c     ktab          chain     tabel00f
086800060523     c                   if        %found(tabel00f)
086900080211     c                   movel     tbluni        desfa
087000060523     c                   else
087100080211     c                   movel     *all'?'       desfa
087200060523     c                   endif
087300080211     c                   eval      %subst(v1sva2:9:28)=desfa
087400080211     c**!!!              eval      %subst(v1sva2:23:16)=indccb
087500060522     c                   eval      %subst(v1sva2:40:5)=(%editc(indabi:'Z'))
087600060522     c                   eval      %subst(v1sva2:46:5)=(%editc(indcab:'Z'))
087700060523     C
087800060523     C                   EXSR      SCRIVIS03
087900060522     c
088000060522     c                   ENDSR
088100060523      **************************************************************************
088200060523      *    Scrivo sfl per tipo record "S"
088300060523      **************************************************************************
088400060523     C     Car_S         BEGSR
088500060523     c*
088600060523     c* Riga prima
088700060523     c                   movel     acvprima      dacv_s
088800060523     c                   movel     despri(7)     v1sva1
088900130326     c                   if        §indbca <> '0'
089000080116     c                   eval      %subst(v1sva1:5:1)='S'
089100060601     c                   else
089200080116     c                   eval      %subst(v1sva1:5:1)='N'
089300060601     c                   endif
089400080116     c                   eval      %subst(v1sva1:10:1)=clpfpc
089500080116     c                   eval      %subst(v1sva1:17:4)=
089600080116     c                             %subst(clpbab:1:4)
089700080116     c                   eval      %subst(v1sva1:22:4)=
089800080116     c                             %subst(clpbab:5:4)
089900080116     c                   eval      %subst(v1sva1:27:4)=
090000080116     c                             %subst(clpbab:9:4)
090100080116     c                   eval      %subst(v1sva1:32:4)=
090200080116     c                             %subst(clpbab:13:4)
090300080116     c                   eval      %subst(v1sva1:37:4)=
090400080116     c                             %subst(clpbab:17:4)
090500080116     c                   eval      %subst(v1sva1:42:4)=
090600080116     c                             %subst(clpbab:21:4)
090700080116     c                   eval      %subst(v1sva1:47:4)=
090800080116     c                             %subst(clpbab:25:4)
090900060523     c* Riga dopo
091000060523     c                   movel     acvdopo       dacv_s
091100060523     c                   movel     desdop(7)     v1sva2
091200130326     c                   if        §indbca <> '0'
091300080116     c                   eval      %subst(v1sva2:5:1)='S'
091400060601     c                   else
091500080116     c                   eval      %subst(v1sva2:5:1)='N'
091600060601     c                   endif
091700080116     c                   eval      %subst(v1sva2:10:1)=clpfpc
091800080116     c                   eval      %subst(v1sva2:17:4)=
091900080116     c                             %subst(clpbab:1:4)
092000080116     c                   eval      %subst(v1sva2:22:4)=
092100080116     c                             %subst(clpbab:5:4)
092200080116     c                   eval      %subst(v1sva2:27:4)=
092300080116     c                             %subst(clpbab:9:4)
092400080116     c                   eval      %subst(v1sva2:32:4)=
092500080116     c                             %subst(clpbab:13:4)
092600080116     c                   eval      %subst(v1sva2:37:4)=
092700080116     c                             %subst(clpbab:17:4)
092800080116     c                   eval      %subst(v1sva2:42:4)=
092900080116     c                             %subst(clpbab:21:4)
093000080116     c                   eval      %subst(v1sva2:47:4)=
093100080116     c                             %subst(clpbab:25:4)
093200060523     c
093300060523     c                   EXSR      ScriviS03
093400060523     c
093500060523     c                   ENDSR
093600060522      **************************************************************************
093700060522      *    Decodifica dati Fatturazione
093800060522      **************************************************************************
093900060522     C     DecodFattur   BEGSR
094000060522     c* sottoconto int fattura
094100060522     c                   eval      %subst(des50a:8:7)=(%editc(clpscf:'X'))
094200060522     c* Fattura unificata
094300060522     c                   if        §clsuni='S'
094400060522     c                   eval      %subst(des50a:21:1)='S'
094500060522     c                   else
094600060522     c                   eval      %subst(des50a:21:1)='N'
094700060522     c                   endif
094800060522     c* Tipo fattura
094900060522     c                   movel     'TFT'         KcodT
095000060522     c                   clear                   Ksif
095100060522     c                   clear                   Klin
095200060522     c                   movel(P)  clptft        kke1
095300060523     c     Ktbe          chain     TNTBE02l
095400060522     c                   if        %found(tntbe02l)
095500060522     c                   movel     tbeuni        dtft
095600060522     c                   movel     §tftdes1      des4a
095700060522     c                   else
095800060522     c                   movel     *all'?'       des4a             4
095900060522     c                   endif
096000060522     c                   eval      %subst(des50a:24:4)=des4a
096100060522     c* Frequenza fattura
096200060522     c                   movel     'FF'          Kcod
096300060522     c                   movel(P)  clpfft        kkey
096400060522     c     Ktab          chain     tabel00f
096500060522     c                   if        %found(tabel00f)
096600060522     c                   movel     tbluni        dsff
096700060522     c                   movel     §ffdes1       des4a
096800060522     c                   else
096900060522     c                   movel     *all'?'       des4a             4
097000060522     c                   endif
097100060522     c                   eval      %subst(des50a:28:4)=des4a
097200060522     c* tipo Data fattura
097300060522     c                   movel     '13'          Kcod
097400060522     c                   movel(P)  §clpfun       kkey
097500060522     c     Ktab          chain     tabel00f
097600060522     c                   if        %found(tabel00f)
097700060522     c                   movel     tbluni        ds13
097800060524     c                   movel     §13des2       des6a
097900060522     c                   else
098000060524     c                   movel     *all'?'       des6a             6
098100060522     c                   endif
098200060524     c                   eval      %subst(des50a:32:6)=des6a
098300060522     c* Spese invio fattura
098400060524     c                   eval      %subst(des50a:45:6)=(%editc(§indsin:'4'))
098500060522     c*
098600060522     c                   ENDSR
098700060523      **************************************************************************
098800060523      *    Controllo selezione  tipo dati variati
098900060523      **************************************************************************
099000060523     C     CtrSel        BEGSR
099100060523     c* Leggo foramto testata
099200060523     c                   read      ta73t01
099300060523     c
099400060523     c                   clear                   v1dsel
099500060523     c                   if        v1csel='?'
099600060523     c                   Clear                   tibs02ds
099700060523     c                   Eval      T02Mod = 'R'
099800060523     c                   Eval      T02Sif = Knsif
099900060523     c                   Eval      T02Cod = 'YDA'
100000060523     c                   Call      'TIBS02R'
100100060523     c                   Parm                    Kpjba
100200060523     c                   Parm                    tibs02ds
100300060523     c                   Movel     T02Ke1        v1csel
100400060524     c***                eval      wemfor='S'
100500060524     c                   endif
100600060523     c*
100700060523     c                   if        v1csel<>' '
100800060523     c                   Clear                   tibs02ds
100900060523     c                   Eval      T02Mod = 'C'
101000060523     c                   Eval      T02Sif = Knsif
101100060523     c                   Eval      T02Cod = 'YDA'
101200060523     c                   movel(P)  v1csel        t02ke1
101300060523     c                   Call      'TIBS02R'
101400060523     c                   Parm                    Kpjba
101500060523     c                   Parm                    tibs02ds
101600060523If  2c                   If        T02Err <> *Blanks
101700060523     c                   eval      v1cmsg=msg(1)
101800060523     c                   seton                                        28
101900060523     c                   else
102000060524     c                   movel     t02uni        dyda
102100060524     c                   movel     §ydades1      v1dsel
102200060523     c                   endif
102300060523     c
102400060524     c***                endif
102500060523     c                   endif
102600060523     c
102700060523     c                   ENDSR
102800060523      **************************************************************************
102900060530      *    Leggo sfl sintetico per vedere se selezionata opzione "5"
103000060523      **************************************************************************
103100060523     C     Leggisfl      BEGSR
103200060530     c* mi salvo la selezione uguale alla nuova, poi la carico
103300060530     c* se modificata dopo aver visualizzato il dettaglio
103400060530     c                   movel     savsel        primasel
103500060530     c                   movel     v1csel        savsel
103600060523     c                   readc     ta73s01                                30
103700060523     c                   dow       not *in30
103800060523     c
103900060523     c                   if        v1ssce='5'
104000060523     c                   z-add     v1sreca       rec3
104100060523     c                   z-add     v1sreca       nrr3
104200060523     c     nrr3          chain     ta73s03                            31
104300060523     c                   seton                                        16
104400060523     c                   movel     in50          *in50
104500060530     c                   movel     in51          *in51
104600060523     c                   update    ta73s03
104700060523     c
104800060523     c                   eval      *inkl=*off
104900060523     c                   exsr      EmissAnal
105000060523     c
105100060523     c     nrr3          chain     ta73s03                            31
105200060523     c                   setoff                                       16
105300060523     c                   movel     in50          *in50
105400060530     c                   movel     in51          *in51
105500060523     c                   update    ta73s03
105600060523     c
105700060523     c                   eval      v1ssce=' '
105800060523     c                   endif
105900060523     c                   movel     in52          *in52
106000060523     c                   update    ta73s01
106100060523     c
106200060523     c                   readc     ta73s01                                30
106300060523     c                   enddo
106400060523     c*
106500060523     c                   eval      *inkl=*off
106600060530     c* reimposto al selezione per eventuale caricamento
106700060530     c                   movel     primasel      savsel
106800060530     c* Reimposto sfl sintetico alla fine
106900060530     c                   eval      sfl='S'
107000060530     c
107100060523     c                   ENDSR
107200021212      **************************************************************************
107300021212      *    R O U T I N E     I N I Z I A L E
107400021212      **************************************************************************
107500060522     C     *inzsr        BEGSR
107600021212      *
107700030612     c     *dtaara       define    §azute        azuteds
107800030612     c     *dtaara       define    §datiute      ddatiute
107900030612
108000030612     C                   in(E)     *dtaara
108100030612     C                   IF        %Error  or  RSUT = *blanks
108200030612     c                   clear                   tibs34ds
108300030612     C                   call      'TIBS34R'
108400030612     C                   parm                    Tibs34Ds
108500030612     C                   in        *dtaara
108600030612     c                   ENDIF
108700030612      *
108800030612     C                   z-add     1             codut             1 0
108900060522     c
109000060522     c*  Carico descrizioni brevi tabella YDA
109100060522     c                   clear                   xx
109200060522     c                   movel     'YDA'         KcodT
109300060522     c                   clear                   Ksif
109400060522     c                   clear                   Klin
109500060523     c     Ktbe2         setll     TNTBE02l
109600060523     c     Ktbe2         reade     TNTBE02l
109700060522do  2c                   dow       NOT %eof(TNTBE02L)
109800060522if  3c                   if        TBEatb   =  *blanks
109900060522     c                   movel     TBEuni        dyda
110000060522     c                   add       1             xx
110100060522     c                   movel     TBEke1        yda(xx)
110200060522     c                   movel     §ydades1      ydadeb(xx)
110300060522e   3c                   endif
110400060523     c     Ktbe2         reade     TNTBE02l
110500060522e   2c                   enddo
110600060522      *
110700030612     c
110800030612      *
110900060522     C     ktbe2         KLIST
111000060522     C                   KFLD                    kcodt
111100060522     C                   KFLD                    klin
111200060522     C                   KFLD                    ksif
111300060522     C     ktbe          KLIST
111400060522     C                   KFLD                    kcodt
111500060522     C                   KFLD                    klin
111600060522     C                   KFLD                    ksif
111700060522     C                   KFLD                    kke1
111800060522     C     ktab          KLIST
111900060522     C                   KFLD                    codut
112000060522     C                   KFLD                    kcod
112100060522     C                   KFLD                    kkey
112200060517     C     Kacvt         KLIST
112300060517     C                   KFLD                    I73Kcc
112400060517     C                   KFLD                    I73ksc
112500060519      *
112600060519     C     Kacvd         KLIST
112700060519     C                   KFLD                    I73Kcc
112800060519     C                   KFLD                    I73ksc
112900060519     C                   KFLD                    acvdav
113000060519     C                   KFLD                    acvorv
113100060522     C                   KFLD                    acvnoj
113200060522     C     Kacvds        KLIST
113300060522     C                   KFLD                    I73Kcc
113400060522     C                   KFLD                    I73ksc
113500060522     C                   KFLD                    acvdav
113600060522     C                   KFLD                    acvorv
113700060522     C                   KFLD                    acvnoj
113800060522     C                   KFLD                    v1csel
113900030612      *
114000030612     C     KAco          KLIST
114100030612     C                   KFLD                    CODUT
114200060518     C                   KFLD                    i73kcc
114300060518     C                   KFLD                    I73ksc
114400060522     c*
114500940726     C                   ENDSR
114600060518**
114700060928Partita IVA  "1234567890123456"  Cli.Annullato"xx"
114800060928Stato Credito"ST" Tipo"B"Causale"010"Annullabile N
114900060522Importanza Cliente "D" Commerciale 0430007-xxxxxxx
115000060524IntFat"0430060" Uni"N" xxxxxxxxxxxxxx Spese 12,000
115100060522ProtIn"0123456" ProtCl"9876543"DataProtCl 01/01/06
115200080211Cod.012-xxxxxxxxx                      01245/67890
115300080116Bl. 8 Tp A IBAN 1234 1234 1234 1234 1234 1234 1234
115400060522I   N   S   E   R   I   M   E   N   T   O
115500060522I N S E R I M E N T O     D A     O F F E R T A
115600060522I N S E R I M E N T O     D A     C O P I A
115700060529Dati variati ma non registrati :
115800061030Codice Fiscale "1234567890123456"
115900100804A   N   N   U   L   L   A   M   E   N   T   O
116000100805Tp X     IBAN 1234 1234 1234 1234 1234 1234 1234
116001171228Codice Potenziale "12345678901"
116100060518**
116200060928             "1234567890123456"               "xx"
116300060522             "ST"     "B"       "010"          "N"
116400060522                   "D"             0430007-xxxxxxx
116500060524      "0430060"    "N" xxxxxxxxxxxxxx       12,000
116600060522      "0123456"       "9876543"           01/01/06
116700080211    012-xxxxxxxxx                      01245/67890
116800080116    8    A      1234 1234 1234 1234 1234 1234 1234
116900061030               "1234567890123456"
116901171228                  "12345678901"
117000060523**
117100060530Selezione  Dati Variati ERRATA
