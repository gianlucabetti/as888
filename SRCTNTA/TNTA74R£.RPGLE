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
002100060522     ftntbe02l  if   e           k disk
002200060522     ftabel00f  if   e           k disk
002300060517     fTnta73d   cf   e             workstn sfile(TA73s01:NRR1)
002400060518     f                                     sfile(TA73S03:NRR3)
002500061030     dDESPRI           s             50    DIM(12) CTDATA PERRCD(1)
002600061030     dDESDOP           s             50    DIM(8) CTDATA PERRCD(1)
002700060523     dmsg              s             78    DIM(1) CTDATA PERRCD(1)
002800060523     d
002900060522     dDavar            s              1    DIM(9)
003000060522     dDavarEs          s              1    DIM(9)
003100060522     dyda              s              1    DIM(25)
003200060522     dydadeb           s              7    DIM(25)
003300030122
003400060522     d dacv_A        e ds
003500061030     d dacv_A3       e ds
003600060522     d dacv_B        e ds
003700060522     d dacv_C        e ds
003800060522     d dacv_F        e ds
003900060522     d dacv_F2       e ds
004000060522     d dacv_P        e ds
004100060522     d dacv_S        e ds
004200060518     d Tnta73ds      e ds
004300030612     d kpjba         e ds
004400030612     d ddatiute      e ds
004500030612     d azuteds       e ds                  extname(AZUTE00F)
004600030612     d tibs34ds      e ds                  inz
004700060523     d tibs02ds      e ds                  inz
004800060522     d dyda          e ds
004900060522     d dtft          e ds
005000060522     d dsff          e ds
005100060522     d ds13          e ds
005200030612      *
005300060523     d wrecord         s              1
005400060523     d wemfor          s              1
005500060523     d sfl             s              1
005600060524     d emetti          s              1
005700060522     D Kke1            S                   LIKE(tbeke1)
005800060522     D KSIF            S                   LIKE(tbesif)
005900060522     D KLIN            S                   LIKE(tbelin)
006000060522     D KCODt           S                   LIKE(tbecod)
006100060522     D KCOD            S                   LIKE(tblcod)
006200060522     D Kkey            S                   LIKE(tblkey)
006300060523     D savsel          S                   LIKE(v1csel)
006400060530     D primasel        S                   LIKE(v1csel)
006500060522     d cortoprima      s              8
006600060522     d cortodopo       s              8
006700060523     d wdata           s              6  0
006800060522     d Dataiso         s               d   datfmt(*iso)
006900060522     d Datadmy         s               d   datfmt(*dmy)
007000060522     d Dataeur         s               d   datfmt(*eur)
007100060522     d
007200021212      *--------------------------------------------------------------*     *****
007300021212     c     *ENTRY        PLIST
007400060523     c                   PARM                    kpjba
007500060523     c                   movel     kpjbu         Tnta73Ds
007600021212      *
007700021212      *   P U L I Z I A   S U B F I L E
007800021212      *
007900021212      *
008000021212      * Riempio i campi del control
008100021212     c                   Exsr      RieCtl
008200060519     c* Caricamento sfl
008300060523     c                   eval      savsel='9'
008400060523     c                   eval      sfl='S'
008500060523     c                   dow       *inkl='0' and v1csel<>savsel
008600060523     c
008700060518     C                   EXSR      RieSfl
008800021212      *
008900021212      *  E M E T T O    S U B F I L E
009000021212      *
009100021218     C     NRR1          IFNE      0
009200060518     c                   seton                                        20
009300030722     C     *INKL         DOWEQ     *off
009400060523     c     v1csel        andeq     savsel
009500060523     c* Sintetico
009600060523     c                   if        sfl='S'
009700060523     c                   EXSR      EMISSSint
009800060523     c                   endif
009900060523     c* Analitico
010000060523     c                   if        sfl='A'
010100060523     c                   EXSR      EMISSAnal
010200060523     c                   endif
010300060518     c
010400060518     C                   ENDDO
010500021212     C                   ENDIF
010600060518     c
010700030122      * Subfile vuoto emetto una videata diversa
010800030122     c                   If        Nrr1 = *Zeros
010900060704     c                   Dow       Not *Inkl  and v1csel = savsel
011000060704     c     fornodati     tag
011100060518     C                   WRITE     TA73T01
011200060518     C                   WRITE     TA73Z01
011300060518     C                   WRITE     TA73D01
011400060704     c     fornodatiE    tag
011500060518     C                   EXFMT     TA73C01
011600060704     c                   setoff                                       28
011700060704     c                   clear                   wemfor
011800060704     c
011900060704     c                   if        not *inkl
012000060704     c* Controllo selezione
012100060704     c                   exsr      CTRSEL
012200060704     c* Riemetto tutto
012300060704     c                   if        wemfor='S'
012400060704     c                   goto      fornodati
012500060704     c                   endif
012600060704     c* Emetto per msg di errore
012700060704     c   28              goto      fornodatiE
012800060704     c                   EndIf
012900060704     c
013000030122     c                   EndDo
013100030122     c                   EndIf
013200060523     c                   EndDo
013300021212      *
013400030122     C                   SETON                                        LR
013500021212      **************************************************************************
013600021212      *    Riempio i campi del control
013700021212      **************************************************************************
013800021212     C     RieCtl        BegSr
013900030611      *
014000060518     C                   clear                   V1CKcc
014100060518     C                   clear                   V1CKsc
014200060518     C                   clear                   V1crag
014300021212      *
014400060518      * Codice
014500060518     C                   z-add     I73Kcc        V1CKcc
014600060518     C                   z-add     I73Ksc        V1CKsc
014700030612      *
014800030613      * Ragione sociale cliente
014900060518     C                   IF        I73Rag = *blanks
015000030612     C     KAco          chain     CnAco000                           34
015100030612     C                   IF        *in34 = *off and Acoflg =' '
015200060518     C                   MOVEL     AcoRag        V1crag
015300030612     C                   ENDIF
015400030613     C                   ELSE
015500030613      * campo passato in DS
015600060518     C                   MOVEL     I73Rag        V1Crag
015700030613     C                   ENDIF
015800030613     C
015900030612      *
016000021212     C                   EndSr
016100060523      **************************************************************************
016200060523      *    Emissione SFL Sintetico
016300060523      **************************************************************************
016400060523     c     EmissSint     BEGSR
016500060523     c                   setoff                                       ki
016600060523     c                   eval      sfl='S'
016700060523     c* F9 - cambio da analitico a sintetico
016800060523     C     *INKI         DOWEQ     *off
016900060523     C     *INKL         andeq     *off
017000060523     c     v1csel        andeq     savsel
017100060523     C                   Z-ADD     1             NRR1
017200060523     C                   Z-ADD     1             REC1
017300060524     c     for01t        tag
017400060523     C                   WRITE     TA73T01
017500060523     C                   WRITE     TA73Z01
017600060523     c     for01         tag
017700060523     C                   EXFMT     TA73C01
017800060523     c                   setoff                                       28
017900060523     c                   clear                   wemfor
018000060523     c
018100060530     c                   if        not *inkl
018200060523     c* Controllo selezione
018300060523     c                   exsr      CTRSEL
018400060524     c* Riemetto tutto
018500060523     c                   if        wemfor='S'
018600060524     c                   goto      for01t
018700060523     c                   endif
018800060524     c* Emetto per msg di errore
018900060524     c   28              goto      for01
019000060523     c
019100060523     c* Verifico se richiesta opzione 5
019200060523     c                   exsr      Leggisfl
019300060523     c                   endif
019400060530     c* F9 - cambio sfl in analitico
019500060523     c                   if        *inki
019600060523     c                   eval      sfl='A'
019700060523     c                   endif
019800060523     c*
019900060523     C                   ENDDO
020000060523     c                   ENDSR
020100060523      **************************************************************************
020200060523      *    Emissione SFL analitico
020300060523      **************************************************************************
020400060523     c     EmissAnal     BEGSR
020500060523     c                   setoff                                       ki
020600060523     c                   eval      sfl='A'
020700060523     C     *INKI         DOWEQ     *off
020800060523     C     *INKL         andeq     *off
020900060523     c     v1csel        andeq     savsel
021000060529     C  n16              Z-ADD     1             NRR3              4 0
021100060529     C  n16              Z-ADD     1             REC3
021200060524     c     for03t        tag
021300060523     C                   WRITE     TA73T01
021400060523     C                   WRITE     TA73Z03
021500060523     c     for03         tag
021600060523     C                   EXFMT     TA73C03
021700060523     c                   setoff                                       28
021800060523     c                   clear                   wemfor
021900060530     c                   if        not *inkl
022000060523     c* Controllo selezione
022100060523     c                   exsr      CTRSEL
022200060523     c                   if        wemfor='S'
022300060524     c                   goto      for03t
022400060523     c                   endif
022500060524     c   28              goto      for03
022600060523     c                   endif
022700060523     c* F9 - cambio sfl in  sintetico
022800060523     c                   if        *inki
022900060523     c                   eval      sfl='S'
023000060523     c                   endif
023100060523     C                   ENDDO
023200060523     c
023300060523     c                   ENDSR
023400021212      **************************************************************************
023500021212      *    Riempio i campi del subfile
023600021212      **************************************************************************
023700021212     c     RieSfl        BegSr
023800060523     c                   eval      *in20 = *off
023900060523     c                   eval      *in21 = *off
024000060523     c                   eval      *in23 = *off
024100060523      *
024200060523     c                   WRITE     TA73C01
024300060523     c                   WRITE     TA73C03
024400060523      *
024500060523     c                   eval      *in21 = *on
024600060523     c                   eval      *in23 = *on
024700060523      *
024800060523     c                   Z-ADD     0             NRR1              4 0
024900060523     c                   Z-ADD     0             NRR3              4 0
025000060522     c*
025100060519     c     kacvt         setll     cnacvt1l
025200060519     c     kacvt         reade     cnacvt1l
025300060519     c                   dow       not %eof(cnacvt1l)
025400060519     c*
025500060519     c                   if        v1csel=' '
025600060519     c                   eval      wrecord='S'
025700060519     c                   else
025800060519     c                   clear                   wrecord
025900060519     c                   endif
026000060518     c* Sintetico
026100060518     c
026200060518     c* Data  variazione PdC
026300060522     c     *iso          movel     acvdav        dataiso
026400060522     c                   movel     dataiso       datadmy
026500060522     c                   movel     datadmy       v1sdav
026600060530     c                   movel     datadmy       v3sdav
026700060518     c
026800030612      * Tipo azione
026900060601     c**                 SELECT
027000060601     c**                 WHEN      acvCta = 'A'
027100060601     c**                 movel     'Ann'         V1sCta
027200060601     c**                 WHEN      acvCta = 'I'
027300060601     c**                 movel     'Ins'         V1sCta
027400060601     c**                 WHEN      acvCta = 'M'
027500060601     c**                 movel     'Man'         V1sCta
027600060601     c**                 WHEN      acvCta = 'O'
027700060601     c**                 movel     'Off'         V1sCta
027800060601     c**                 WHEN      acvCta = 'D'
027900060601     c**                 movel     'Dup'         V1sCta
028000060601     c**                 ENDSL
028100060601     c* decodifico il pgm
028200060601     c                   Clear                   tibs02ds
028300060601     c                   Eval      T02Mod = 'C'
028400060601     c                   Eval      T02Sif = Knsif
028500060601     c                   Eval      T02Cod = 'YPG'
028600060601     c                   movel(P)  acvpgm        t02ke1
028700060601     c                   Call      'TIBS02R'
028800060601     c                   Parm                    Kpjba
028900060601     c                   Parm                    tibs02ds
029000060601If  2c                   If        T02Err <> *Blanks
029100060601     c                   eval      v1spgm=*all'?'
029200060601     c                   else
029300060601     c                   movel     t02uni        v1spgm
029400060601     c                   endif
029500060601     c
029600060601     c* Se inserimento scrivo da dove
029700060601     c                   clear                   v1sins
029800060601     c                   if        acvcta<>'M'
029900060601     c                   select
030000060601     C                   when      ACVCTA='O'
030100060601     c                   movel     despri(9)     v1sins
030200060601     C                   when      ACVCTA='C'
030300060601     c                   movel     despri(10)    v1sins
030400060601     C                   OTHER
030500060601     c                   movel     despri(8)     v1sins
030600060601     c                   endsl
030700060601     c                   endif
030800030611      *
030900060518     c                   z-add     acvOrv        V1sOrv
031000060530     c                   z-add     acvOrv        V3sOrv
031100060518     c                   movel     acvPru        V1sPru
031200060530     c                   movel     acvPru        V3sPru
031300060518     c                   movel     acvNoj        V1sNoj
031400060530     c                   movel(P)  acvNoj        V3sNoj
031500060518     c* dati modificati
031600060518     c                   clear                   v1sana
031700060518     c                   clear                   v1sblc
031800060518     c                   clear                   v1scom
031900060518     c                   clear                   v1sfat
032000060518     c                   clear                   v1spgf
032100060518     c                   clear                   v1spgc
032200060518     c                   clear                   v1sgia
032300060518     c                   clear                   v1sdan
032400060518     c                   clear                   v1sope
032500060524     c                   clear                   DaVarEs
032600060522     c
032700060522     c                   movea     acvflo        DaVar
032800060522     c                   do        9             xx                2 0
032900060522     c                   if        daVar(XX)<>' '
033000060518     c*
033100060522     c                   if        DaVar(xx)='A'
033200060519     c
033300060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
033400060519     c                   eval      wrecord='S'
033500060519     c                   endif
033600060518     c                   eval      v1sana='SI'
033700060518     c                   endif
033800060519     c
033900060522     c                   if        DaVar(xx)='B'
034000060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
034100060519     c                   eval      wrecord='S'
034200060519     c                   endif
034300060518     c                   eval      v1sblc='SI'
034400060518     c                   endif
034500060519     c
034600060522     c                   if        DaVar(xx)='C'
034700060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
034800060519     c                   eval      wrecord='S'
034900060519     c                   endif
035000060518     c                   eval      v1scom='SI'
035100060518     c                   endif
035200060519     c
035300060522     c                   if        DaVar(xx)='F'
035400060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
035500060519     c                   eval      wrecord='S'
035600060519     c                   endif
035700060518     c                   eval      v1sfat='SI'
035800060518     c                   endif
035900060519     c
036000060522     c                   if        DaVar(xx)='P'
036100060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
036200060519     c                   eval      wrecord='S'
036300060519     c                   endif
036400060518     c                   eval      v1spgf='SI'
036500060518     c                   endif
036600060522     c
036700060522     c                   if        DaVar(xx)='S'
036800060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
036900060519     c                   eval      wrecord='S'
037000060519     c                   endif
037100060518     c                   eval      v1spgc='SI'
037200060518     c                   endif
037300060519     c
037400060522     c                   if        DaVar(xx)='G'
037500060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
037600060519     c                   eval      wrecord='S'
037700060519     c                   endif
037800060518     c                   eval      v1sgia='SI'
037900060518     c                   endif
038000060519     c
038100060522     c                   if        DaVar(xx)='D'
038200060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
038300060519     c                   eval      wrecord='S'
038400060519     c                   endif
038500060518     c                   eval      v1sdan='SI'
038600060518     c                   endif
038700060519     c
038800060522     c                   if        DaVar(xx)='O'
038900060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
039000060519     c                   eval      wrecord='S'
039100060519     c                   endif
039200060518     c                   eval      v1sope='SI'
039300060518     c                   endif
039400060522     c
039500060522     c                   endif
039600060522     c                   enddo
039700060519     c
039800060519     c* Leggo e scrivo analitico
039900060519     c                   if        wrecord='S'
040000060519     c                   Exsr      SCRiana
040100060524     c
040200060524     c* Scrivo un record in analitico per dati variati non registrati
040300060524     c* solo per variazione
040400060524     c                   if        acvcta='M'
040500060524     c                   Exsr      Datinoregis
040600060524     c                   endif
040700060530     c
040800060530     c* visto che almeno una riga l'ho scritta in analitico, aggancio
040900060530     c*  l'ultima per sottolineare i campi
041000060530     c     nrr3          chain     ta73s03                            31
041100060530     c                   if        not *in31
041200060530     c                   movel     *off          *inkl
041300060530     c                   movel     in50          *in50
041400060530     c                   movel     in51          *in51
041500060530     c                   if        not *in50
041600060530     c                   clear                   v3spru
041700060530     c                   clear                   v3snoj
041800060530     c                   clear                   v3sorv
041900060530     c                   clear                   v3sdav
042000060530     c                   endif
042100060530     c                   seton                                        5051
042200060530     c                   movel     *in50         in50
042300060530     c                   movel     *in51         in51
042400060530     c                   update    ta73s03
042500060530     c                   endif
042600060530     c
042700060530     c
042800060523     c
042900060523     c* Se non ho caricato nessun record di analitico, proteggo la selezione
043000060523     c                   if        v1sreca=0
043100060523     c                   seton                                        52
043200060523     c                   endif
043300060523     c                   movel     *in52         in52
043400060518     c
043500060518     c                   add       1             nrr1
043600060518     c                   write     ta73s01
043700060523     c
043800060523     c                   setoff                                       52
043900060519     c                   endif
044000060519     c*
044100060519     c     kacvt         reade     cnacvt1l
044200060519     c                   enddo
044300060523     c
044400060523     c                   eval      savsel=v1csel
044500060518     c*
044600021212     c                   EndSr
044700060519      **************************************************************************
044800060519      *    Scrivo sfl analitico
044900060519      **************************************************************************
045000060519     C     Scriana       BEGSR
045100060522     c                   seton                                        50
045200060530     c                   setoff                                       51
045300060522     c                   setoff                                       16
045400060522     c                   clear                   v1sreca
045500060523     c
045600060523     c* Se non manutenzione non devo caricare la DS
045700060523    2c                   if        acvcta<>'M'
045800060523     c                   exsr      car_ins
045900060523     c
046000060523   x2c                   else
046100060523     c
046200060522     c                   if        v1csel=' '
046300060519     c     kacvd         setll     cnacvd1l
046400060519     c     kacvd         reade     cnacvd1l
046500060522     c                   else
046600060522     c     kacvds        setll     cnacvd1l
046700060522     c     kacvds        reade     cnacvd1l
046800060522     c                   endif
046900060519     c
047000060522    1c                   dow       not %eof(cnacvd1l)
047100060522     c* Per record dati commerciali, per ora mi interessano soltanto
047200060522     c*  i primi 2 campi
047300060522     c                   if        acvyda='C'
047400060522     c                   movel     acvprima      cortoprima
047500060522     c                   movel     acvdopo       cortodopo
047600060522     c                   endif
047700060522     c
047800060522     c* Se nessun dato è stato modificato, significa che altri
047900060522     c*  dati della parte anagrafica sono stati modificat ma
048000060522     c*  non memorizzati, per cui inseriri in un unico record generico
048100060522    3c                   if        (acvyda<>'C' and acvprima=acvdopo) or
048200060523    cc                             (acvyda='C' and cortoprima=cortodopo)
048300060522     c                   z-add     1             xx
048400060522     c     acvyda        lookup    daVar(xx)                              30
048500060522    4c                   if        *in30
048600060522     c                   eval      DaVarEs(XX)='N'
048700060522    4c                   endif
048800060522   x3c                   else
048900060522     c
049000060522     c* Decodifico tipo dati variati
049100060522     c                   z-add     1             xx
049200060522     c     acvyda        lookup    yda(xx)                                30
049300060522     c   30              movel     ydadeb(xx)    v1sdt1
049400060522     c  n30              movel(p)  acvyda        v1sdt1
049500060522     c
049600060524     c***                eval      v1spd2='DOPO'
049700060522     c
049800060522     c* Imposto la DS e per il primo record memorizzo in nrr nel sfl
049900060522     c*  sintetico
050000060522    4c                   select
050100060522     c                   when      acvyda='A'
050200061030     c                   if        acvprg=0
050300060522     c                   exsr      car_A
050400061030     c                   else
050500061030     c                   exsr      car_A3
050600061030     c                   endif
050700060522     c                   when      acvyda='B'
050800060522     c                   exsr      car_B
050900060522     c                   when      acvyda='C'
051000060522     c                   exsr      car_C
051100060522     c                   when      acvyda='F'
051200060522     c                   if        acvprg=0
051300060522     c                   exsr      car_F
051400060522     c                   else
051500060522     c                   exsr      car_F2
051600060522     c                   endif
051700060522     c                   when      acvyda='P'
051800060522     c                   exsr      car_P
051900060522     c                   when      acvyda='S'
052000060522     c                   exsr      car_S
052100060522    4c                   endsl
052200060522     c
052300060522    3c                   endif
052400060522     c
052500060522    2c                   if        v1csel=' '
052600060519     c     kacvd         reade     cnacvd1l
052700060522     c                   else
052800060522     c     kacvds        reade     cnacvd1l
052900060522    2c                   endif
053000060522    1c                   enddo
053100060530     c* finita la scrittura
053200060523     c
053300060523    2c                   endif
053400060519     c
053500060519     c                   EndSr
053600060523      **************************************************************************
053700060523      *    SCRIVO RECORD IN SFL TA73S03
053800060523      **************************************************************************
053900060523     C     SCRIVIS03     BEGSR
054000060523     c                   add       1             nrr3
054100060523     c                   movel     *in50         in50
054200060530     c                   movel     *in51         in51
054300060523     c                   write     ta73s03
054400060523     c*
054500060523     c* Imposto se il primo nrr3
054600060523     c                   if        v1sreca=0
054700060523     c                   z-add     nrr3          v1sreca
054800060523     c                   endif
054900060524     c                   setoff                                       50
055000060524     c
055100060524     c* Memorizzo il record dati variati scritto in analitico
055200060524     c                   z-add     1             xx
055300060524     c     acvyda        lookup    daVar(xx)                              30
055400060524    4c                   if        *in30
055500060524     c                   eval      DaVarEs(XX)='S'
055600060524    4c                   endif
055700060523     C                   ENDSR
055800060524      **************************************************************************
055900060524      *    Scrivo un record in sfl analitico per dati variati non regis
056000060524      **************************************************************************
056100060524     C     datinoregis   BEGSR
056200060524     c                   clear                   v1sdt1
056300060524     c**                 clear                   v1spd2
056400060524     c                   clear                   v1sva2
056500060524     c                   clear                   emetti
056600060524     c                   movel     despri(11)    v1sva1
056700060524     c                   z-add     1             xx
056800060524     c                   do        9             xx
056900060524     c                   if        DaVar(XX)<>' ' and DaVarEs(xx)<>'S'
057000060524     c
057100060524     c* Memorizzo se in questo record c'e' il tipo dati selezionato
057200060524     c                   if        (v1csel<>' ' and davar(xx)=v1csel) or
057300060524     c                              v1csel=' '
057400060524     c
057500060524     c                   z-add     1             yy                2 0
057600060524     c     davar(xx)     lookup    yda(yy)                                30
057700060524     c                   if        *in30
057800060524     c                   movel     ydadeb(yy)    des7a             7
057900060524     c                   else
058000060524     c                   movel     *all'?'       des7a
058100060524     c                   endif
058200060524     c* riempo stringa
058300060524     c                   eval      v1sva2=%trim(v1sva2) + ' '+ des7a + ' /'
058400060524     c                   endif
058500060524     c                   endif
058600060524     c                   enddo
058700060524     c
058800060524     c                   if        v1sva2<>*blanks
058900060530     c                   eval      v1sva2=%trim(v1sva2)
059000060524     c                   exsr      ScriviS03
059100060524     c                   endif
059200060524     c
059300060524     C                   ENDSR
059400060522      **************************************************************************
059500060522      *    Carico record inserimento
059600060522      **************************************************************************
059700060522     C     Car_ins       BEGSR
059800060522     C                   MOVEL     *blanks       V1SDT1
059900060524     c***                movel     *blanks       v1spd2
060000060522     c                   select
060100060522     C                   when      ACVCTA='O'
060200060524     c                   movel     despri(9)     v1sva2
060300060522     C                   when      ACVCTA='C'
060400060524     c                   movel     despri(10)    v1sva2
060500060522     C                   OTHER
060600060524     c                   movel     despri(8)     v1sva2
060700060522     c                   endsl
060800060524     c                   movel     *BLANKS       v1sva1
060900060530     c                   seton                                        51
061000060523     c
061100060523     c                   exsr      ScriviS03
061200060530     c                   setoff                                       51
061300060522     c
061400060522     c                   endsr
061500060522      **************************************************************************
061600060522      *    Scrivo sfl per tipo record "A"
061700060522      **************************************************************************
061800060522     C     Car_A         BEGSR
061900060522     c*
062000060522     c* Riga prima
062100060522     c                   movel     acvprima      dacv_A
062200060522     c                   movel     despri(1)     v1sva1
062300060522     c                   eval      %subst(v1sva1:15:16)=indiva
062400060928     c* flag annullamento
062500060928     c                   if        acoflg<>' '
062600060928     c                   eval      %subst(v1sva1:48:2)='SI'
062700060928     c                   else
062800060928     c                   eval      %subst(v1sva1:48:2)='  '
062900060928     c                   endif
063000060522     c* Riga dopo
063100060522     c                   movel     acvdopo       dacv_A
063200060522     c                   movel     desdop(1)     v1sva2
063300060522     c                   eval      %subst(v1sva2:15:16)=indiva
063400060928     c* flag annullamento
063500060928     c                   if        acoflg<>' '
063600060928     c                   eval      %subst(v1sva2:48:2)='SI'
063700060928     c                   else
063800060928     c                   eval      %subst(v1sva2:48:2)='  '
063900060928     c                   endif
064000060523     c
064100060523     c                   EXSR      ScriviS03
064200060522     c
064300060522     c                   ENDSR
064400061030      **************************************************************************
064500061030      *    Scrivo sfl per tipo record "A" progressivo 3
064600061030      **************************************************************************
064700061030     C     Car_A3        BEGSR
064800061030     c*
064900061030     c* Riga prima
065000061030     c                   movel     acvprima      dacv_A3
065100061030     c                   movel     despri(12)    v1sva1
065200061030     c                   eval      %subst(v1sva1:17:16)=indcdf
065300061030     c* Riga dopo
065400061030     c                   movel     acvdopo       dacv_A3
065500061030     c                   movel     desdop(8)     v1sva2
065600061030     c                   eval      %subst(v1sva2:17:16)=indcdf
065700061030     c
065800061030     c                   EXSR      ScriviS03
065900061030     c
066000061030     c                   ENDSR
066100060522      **************************************************************************
066200060522      *    Scrivo sfl per tipo record "B"
066300060522      **************************************************************************
066400060522     C     Car_B         BEGSR
066500060522     c*
066600060522     c* Riga prima
066700060522     c                   movel     acvprima      dacv_B
066800060522     c                   movel     despri(2)     v1sva1
066900060522     c                   eval      %subst(v1sva1:15:2)=§clpcon
067000060522     c                   eval      %subst(v1sva1:24:1)=acoabl
067100060522     c                   eval      %subst(v1sva1:34:3)=clpnar
067200060522     c                   if        atb02='N'
067300060928     c                   eval      %subst(v1sva1:50:1)='N'
067400060522     c                   else
067500060928     c                   eval      %subst(v1sva1:50:1)='S'
067600060522     c                   endif
067700060522     c* Riga dopo
067800060522     c                   movel     acvdopo       dacv_B
067900060522     c                   movel     desdop(2)     v1sva2
068000060522     c                   eval      %subst(v1sva2:15:2)=§clpcon
068100060522     c                   eval      %subst(v1sva2:24:1)=acoabl
068200060522     c                   eval      %subst(v1sva2:34:3)=clpnar
068300060522     c                   if        atb02='N'
068400060928     c                   eval      %subst(v1sva2:50:1)='N'
068500060522     c                   else
068600060928     c                   eval      %subst(v1sva2:50:1)='S'
068700060522     c                   endif
068800060523     c
068900060523     c                   EXSR      ScriviS03
069000060522     c
069100060522     c                   ENDSR
069200060522      **************************************************************************
069300060522      *    Scrivo sfl per tipo record "C"
069400060522      **************************************************************************
069500060522     C     Car_C         BEGSR
069600060522     c* Riga prima
069700060522     c                   movel     acvprima      dacv_C
069800060522     c                   movel     despri(3)     v1sva1
069900060522     c                   eval      %subst(v1sva1:21:1)=clpclv
070000060522     c                   eval      %subst(v1sva1:36:7)=(%editc(clpage:'X'))
070100060522     c* Decodifico agente
070200060522     c                   movel     '01'          kcod
070300060522     c                   movel(p)  clpage        kkey
070400060522     c     ktab          chain     tabel00f
070500060522     c                   if        %found(tabel00f)
070600060523     c                   movel     tbluni        des7a             7
070700060522     c                   else
070800060522     c                   movel     *all'?'       des7a
070900060522     c                   endif
071000060522     c                   eval      %subst(v1sva1:44:7)=des7a
071100060522     c* Riga dopo
071200060522     c                   movel     acvdopo       dacv_C
071300060522     c                   movel     desdop(3)     v1sva2
071400060522     c                   eval      %subst(v1sva2:21:1)=clpclv
071500060522     c                   eval      %subst(v1sva2:36:7)=(%editc(clpage:'X'))
071600060522     c* Decodifico agente
071700060522     c                   movel     '01'          kcod
071800060522     c                   movel(p)  clpage        kkey
071900060522     c     ktab          chain     tabel00f
072000060522     c                   if        %found(tabel00f)
072100060523     c                   movel     tbluni        des7a
072200060522     c                   else
072300060522     c                   movel     *all'?'       des7a
072400060522     c                   endif
072500060522     c                   eval      %subst(v1sva2:44:7)=des7a
072600060522     c
072700060523     c                   EXSR      ScriviS03
072800060522     c
072900060522     c                   ENDSR
073000060522      **************************************************************************
073100060522      *    Scrivo sfl per tipo record "F"
073200060522      **************************************************************************
073300060522     C     Car_F         BEGSR
073400060522     c*
073500060522     c* Riga prima
073600060522     c                   movel     acvprima      dacv_F
073700060522     c                   movel     despri(4)     des50a           50
073800060522     c                   exsr      DecodFattur
073900060522     c                   movel     des50a        v1sva1
074000060522     c* Riga dopo
074100060522     c                   movel     acvdopo       dacv_F
074200060522     c                   movel     desdop(4)     des50a
074300060522     c                   exsr      DecodFattur
074400060522     c                   movel     des50a        v1sva2
074500060522     c
074600060523     c                   EXSR      ScriviS03
074700060522     c
074800060522     c                   ENDSR
074900060522      **************************************************************************
075000060522      *    Scrivo sfl per tipo record "F" progressivo  2°
075100060522      **************************************************************************
075200060522     C     Car_F2        BEGSR
075300060522     c*
075400060522     c* Riga prima
075500060522     c                   movel     acvprima      dacv_F2
075600060522     c                   movel     despri(5)     v1sva1
075700060522     c* Numero protocollo interno
075800060522     c                   eval      %subst(v1sva1:8:7)=(%editc(indnpn:'Z'))
075900060522     c* Numero protocollo cliente
076000060522     c                   eval      %subst(v1sva1:24:7)=(%editc(indnpr:'Z'))
076100060522     c* Data protocollo cliente
076200060830     c                   if        inddpr>0
076300060522     c     *ymd          movel     inddpr        datadmy
076400060523     c                   movel     datadmy       des8a             8
076500060830     c                   else
076600060830     c                   clear                   des8a
076700060830     c                   endif
076800060830     c                   eval      %subst(v1sva1:43:8)= des8a
076900060830     c*****              eval      %subst(v1sva1:43:8)= (%editc(wdata:'Y'))
077000060522     c* Riga dopo
077100060522     c                   movel     acvdopo       dacv_F2
077200060522     c                   movel     desdop(5)     v1sva2
077300060522     c* Numero protocollo interno
077400060522     c                   eval      %subst(v1sva2:8:7)=(%editc(indnpn:'Z'))
077500060522     c* Numero protocollo cliente
077600060522     c                   eval      %subst(v1sva2:24:7)=(%editc(indnpr:'Z'))
077700060522     c* Data protocollo cliente
077800060830     c                   if        inddpr>0
077900060522     c     *ymd          movel     inddpr        datadmy
078000060523     c                   movel     datadmy       wdata
078100060523     c                   eval      %subst(v1sva2:43:8)= (%editc(wdata:'Y'))
078200060830     c                   else
078300060830     c                   clear                   des8a
078400060830     c                   eval      %subst(v1sva1:43:8)= des8a
078500060830     c                   endif
078600060522     c*
078700060523     c                   EXSR      ScriviS03
078800060522     c
078900060522     c                   ENDSR
079000060522      **************************************************************************
079100060522      *    Scrivo sfl per tipo record "P"
079200060522      **************************************************************************
079300060522     C     Car_P         BEGSR
079400060522     c*
079500060522     c* Riga prima
079600060522     c                   movel     acvprima      dacv_p
079700060523     c                   movel     despri(6)     v1sva1
079800060522     c                   eval      %subst(v1sva1:5:3)=§indcdp
079900060523     c* Decodifica
080000060523     c                   movel     §indcdp       des4a             4
080100060523     c                   move      '1'           des4a
080200060523     c                   movel     'FA'          kcod
080300060524     c                   movel(P)  des4a         kkey
080400060523     c     ktab          chain     tabel00f
080500060523     c                   if        %found(tabel00f)
080600060523     c                   movel     tbluni        des9a             7
080700060523     c                   else
080800060523     c                   movel     *all'?'       des9a
080900060523     c                   endif
081000060523     c                   eval      %subst(v1sva1:9:9)=des9a
081100060523     c                   eval      %subst(v1sva1:23:16)=indccb
081200060522     c                   eval      %subst(v1sva1:40:5)=(%editc(indabi:'Z'))
081300060522     c                   eval      %subst(v1sva1:46:5)=(%editc(indcab:'Z'))
081400060522     c* Riga dopo
081500060522     c                   movel     acvdopo       dacv_P
081600060523     c                   movel     desdop(6)     v1sva2
081700060522     c                   eval      %subst(v1sva2:5:3)=§indcdp
081800060523     c* Decodifica
081900060523     c                   movel     §indcdp       des4a             4
082000060523     c                   move      '1'           des4a
082100060523     c                   movel     'FA'          kcod
082200060524     c                   movel(P)  des4a         kKEY
082300060523     c     ktab          chain     tabel00f
082400060523     c                   if        %found(tabel00f)
082500060523     c                   movel     tbluni        des9a             7
082600060523     c                   else
082700060523     c                   movel     *all'?'       des9a
082800060523     c                   endif
082900060523     c                   eval      %subst(v1sva2:9:9)=des9a
083000060522     c                   eval      %subst(v1sva2:23:16)=indccb
083100060522     c                   eval      %subst(v1sva2:40:5)=(%editc(indabi:'Z'))
083200060522     c                   eval      %subst(v1sva2:46:5)=(%editc(indcab:'Z'))
083300060523     C
083400060523     C                   EXSR      SCRIVIS03
083500060522     c
083600060522     c                   ENDSR
083700060523      **************************************************************************
083800060523      *    Scrivo sfl per tipo record "S"
083900060523      **************************************************************************
084000060523     C     Car_S         BEGSR
084100060523     c*
084200060523     c* Riga prima
084300060523     c                   movel     acvprima      dacv_s
084400060523     c                   movel     despri(7)     v1sva1
084500060601     c                   if        §indbca='B'
084600060601     c                   eval      %subst(v1sva1:6:1)='S'
084700060601     c                   else
084800060601     c                   eval      %subst(v1sva1:6:1)='N'
084900060601     c                   endif
085000060523     c                   eval      %subst(v1sva1:14:1)=clpfpc
085100060523     c                   eval      %subst(v1sva1:21:12)=§clpccba
085200060523     c                   eval      %subst(v1sva1:38:1)=§clpcina
085300060523     c                   eval      %subst(v1sva1:40:5)=(%editc(clpabi:'Z'))
085400060523     c                   eval      %subst(v1sva1:46:5)=(%editc(clpcab:'Z'))
085500060523     c* Riga dopo
085600060523     c                   movel     acvdopo       dacv_s
085700060523     c                   movel     desdop(7)     v1sva2
085800060601     c                   if        §indbca='B'
085900060601     c                   eval      %subst(v1sva2:6:1)='S'
086000060601     c                   else
086100060601     c                   eval      %subst(v1sva2:6:1)='N'
086200060601     c                   endif
086300060523     c                   eval      %subst(v1sva2:14:1)=clpfpc
086400060523     c                   eval      %subst(v1sva2:21:12)=§clpccba
086500060523     c                   eval      %subst(v1sva2:38:1)=§clpcina
086600060523     c                   eval      %subst(v1sva2:40:5)=(%editc(clpabi:'Z'))
086700060523     c                   eval      %subst(v1sva2:46:5)=(%editc(clpcab:'Z'))
086800060523     c
086900060523     c                   EXSR      ScriviS03
087000060523     c
087100060523     c                   ENDSR
087200060522      **************************************************************************
087300060522      *    Decodifica dati Fatturazione
087400060522      **************************************************************************
087500060522     C     DecodFattur   BEGSR
087600060522     c* sottoconto int fattura
087700060522     c                   eval      %subst(des50a:8:7)=(%editc(clpscf:'X'))
087800060522     c* Fattura unificata
087900060522     c                   if        §clsuni='S'
088000060522     c                   eval      %subst(des50a:21:1)='S'
088100060522     c                   else
088200060522     c                   eval      %subst(des50a:21:1)='N'
088300060522     c                   endif
088400060522     c* Tipo fattura
088500060522     c                   movel     'TFT'         KcodT
088600060522     c                   clear                   Ksif
088700060522     c                   clear                   Klin
088800060522     c                   movel(P)  clptft        kke1
088900060523     c     Ktbe          chain     TNTBE02l
089000060522     c                   if        %found(tntbe02l)
089100060522     c                   movel     tbeuni        dtft
089200060522     c                   movel     §tftdes1      des4a
089300060522     c                   else
089400060522     c                   movel     *all'?'       des4a             4
089500060522     c                   endif
089600060522     c                   eval      %subst(des50a:24:4)=des4a
089700060522     c* Frequenza fattura
089800060522     c                   movel     'FF'          Kcod
089900060522     c                   movel(P)  clpfft        kkey
090000060522     c     Ktab          chain     tabel00f
090100060522     c                   if        %found(tabel00f)
090200060522     c                   movel     tbluni        dsff
090300060522     c                   movel     §ffdes1       des4a
090400060522     c                   else
090500060522     c                   movel     *all'?'       des4a             4
090600060522     c                   endif
090700060522     c                   eval      %subst(des50a:28:4)=des4a
090800060522     c* tipo Data fattura
090900060522     c                   movel     '13'          Kcod
091000060522     c                   movel(P)  §clpfun       kkey
091100060522     c     Ktab          chain     tabel00f
091200060522     c                   if        %found(tabel00f)
091300060522     c                   movel     tbluni        ds13
091400060524     c                   movel     §13des2       des6a
091500060522     c                   else
091600060524     c                   movel     *all'?'       des6a             6
091700060522     c                   endif
091800060524     c                   eval      %subst(des50a:32:6)=des6a
091900060522     c* Spese invio fattura
092000060524     c                   eval      %subst(des50a:45:6)=(%editc(§indsin:'4'))
092100060522     c*
092200060522     c                   ENDSR
092300060523      **************************************************************************
092400060523      *    Controllo selezione  tipo dati variati
092500060523      **************************************************************************
092600060523     C     CtrSel        BEGSR
092700060523     c* Leggo foramto testata
092800060523     c                   read      ta73t01
092900060523     c
093000060523     c                   clear                   v1dsel
093100060523     c                   if        v1csel='?'
093200060523     c                   Clear                   tibs02ds
093300060523     c                   Eval      T02Mod = 'R'
093400060523     c                   Eval      T02Sif = Knsif
093500060523     c                   Eval      T02Cod = 'YDA'
093600060523     c                   Call      'TIBS02R'
093700060523     c                   Parm                    Kpjba
093800060523     c                   Parm                    tibs02ds
093900060523     c                   Movel     T02Ke1        v1csel
094000060524     c***                eval      wemfor='S'
094100060524     c                   endif
094200060523     c*
094300060523     c                   if        v1csel<>' '
094400060523     c                   Clear                   tibs02ds
094500060523     c                   Eval      T02Mod = 'C'
094600060523     c                   Eval      T02Sif = Knsif
094700060523     c                   Eval      T02Cod = 'YDA'
094800060523     c                   movel(P)  v1csel        t02ke1
094900060523     c                   Call      'TIBS02R'
095000060523     c                   Parm                    Kpjba
095100060523     c                   Parm                    tibs02ds
095200060523If  2c                   If        T02Err <> *Blanks
095300060523     c                   eval      v1cmsg=msg(1)
095400060523     c                   seton                                        28
095500060523     c                   else
095600060524     c                   movel     t02uni        dyda
095700060524     c                   movel     §ydades1      v1dsel
095800060523     c                   endif
095900060523     c
096000060524     c***                endif
096100060523     c                   endif
096200060523     c
096300060523     c                   ENDSR
096400060523      **************************************************************************
096500060530      *    Leggo sfl sintetico per vedere se selezionata opzione "5"
096600060523      **************************************************************************
096700060523     C     Leggisfl      BEGSR
096800060530     c* mi salvo la selezione uguale alla nuova, poi la carico
096900060530     c* se modificata dopo aver visualizzato il dettaglio
097000060530     c                   movel     savsel        primasel
097100060530     c                   movel     v1csel        savsel
097200060523     c                   readc     ta73s01                                30
097300060523     c                   dow       not *in30
097400060523     c
097500060523     c                   if        v1ssce='5'
097600060523     c                   z-add     v1sreca       rec3
097700060523     c                   z-add     v1sreca       nrr3
097800060523     c     nrr3          chain     ta73s03                            31
097900060523     c                   seton                                        16
098000060523     c                   movel     in50          *in50
098100060530     c                   movel     in51          *in51
098200060523     c                   update    ta73s03
098300060523     c
098400060523     c                   eval      *inkl=*off
098500060523     c                   exsr      EmissAnal
098600060523     c
098700060523     c     nrr3          chain     ta73s03                            31
098800060523     c                   setoff                                       16
098900060523     c                   movel     in50          *in50
099000060530     c                   movel     in51          *in51
099100060523     c                   update    ta73s03
099200060523     c
099300060523     c                   eval      v1ssce=' '
099400060523     c                   endif
099500060523     c                   movel     in52          *in52
099600060523     c                   update    ta73s01
099700060523     c
099800060523     c                   readc     ta73s01                                30
099900060523     c                   enddo
100000060523     c*
100100060523     c                   eval      *inkl=*off
100200060530     c* reimposto al selezione per eventuale caricamento
100300060530     c                   movel     primasel      savsel
100400060530     c* Reimposto sfl sintetico alla fine
100500060530     c                   eval      sfl='S'
100600060530     c
100700060523     c                   ENDSR
100800021212      **************************************************************************
100900021212      *    R O U T I N E     I N I Z I A L E
101000021212      **************************************************************************
101100060522     C     *inzsr        BEGSR
101200021212      *
101300030612     c     *dtaara       define    §azute        azuteds
101400030612     c     *dtaara       define    §datiute      ddatiute
101500030612
101600030612     C                   in(E)     *dtaara
101700030612     C                   IF        %Error  or  RSUT = *blanks
101800030612     c                   clear                   tibs34ds
101900030612     C                   call      'TIBS34R'
102000030612     C                   parm                    Tibs34Ds
102100030612     C                   in        *dtaara
102200030612     c                   ENDIF
102300030612      *
102400030612     C                   z-add     1             codut             1 0
102500060522     c
102600060522     c*  Carico descrizioni brevi tabella YDA
102700060522     c                   clear                   xx
102800060522     c                   movel     'YDA'         KcodT
102900060522     c                   clear                   Ksif
103000060522     c                   clear                   Klin
103100060523     c     Ktbe2         setll     TNTBE02l
103200060523     c     Ktbe2         reade     TNTBE02l
103300060522do  2c                   dow       NOT %eof(TNTBE02L)
103400060522if  3c                   if        TBEatb   =  *blanks
103500060522     c                   movel     TBEuni        dyda
103600060522     c                   add       1             xx
103700060522     c                   movel     TBEke1        yda(xx)
103800060522     c                   movel     §ydades1      ydadeb(xx)
103900060522e   3c                   endif
104000060523     c     Ktbe2         reade     TNTBE02l
104100060522e   2c                   enddo
104200060522      *
104300030612     c
104400030612      *
104500060522     C     ktbe2         KLIST
104600060522     C                   KFLD                    kcodt
104700060522     C                   KFLD                    klin
104800060522     C                   KFLD                    ksif
104900060522     C     ktbe          KLIST
105000060522     C                   KFLD                    kcodt
105100060522     C                   KFLD                    klin
105200060522     C                   KFLD                    ksif
105300060522     C                   KFLD                    kke1
105400060522     C     ktab          KLIST
105500060522     C                   KFLD                    codut
105600060522     C                   KFLD                    kcod
105700060522     C                   KFLD                    kkey
105800060517     C     Kacvt         KLIST
105900060517     C                   KFLD                    I73Kcc
106000060517     C                   KFLD                    I73ksc
106100060519      *
106200060519     C     Kacvd         KLIST
106300060519     C                   KFLD                    I73Kcc
106400060519     C                   KFLD                    I73ksc
106500060519     C                   KFLD                    acvdav
106600060519     C                   KFLD                    acvorv
106700060522     C                   KFLD                    acvnoj
106800060522     C     Kacvds        KLIST
106900060522     C                   KFLD                    I73Kcc
107000060522     C                   KFLD                    I73ksc
107100060522     C                   KFLD                    acvdav
107200060522     C                   KFLD                    acvorv
107300060522     C                   KFLD                    acvnoj
107400060522     C                   KFLD                    v1csel
107500030612      *
107600030612     C     KAco          KLIST
107700030612     C                   KFLD                    CODUT
107800060518     C                   KFLD                    i73kcc
107900060518     C                   KFLD                    I73ksc
108000060522     c*
108100940726     C                   ENDSR
108200060518**
108300060928Partita IVA  "1234567890123456"  Cli.Annullato"xx"
108400060928Stato Credito"ST" Tipo"B"Causale"010"Annullabile N
108500060522Importanza Cliente "D" Commerciale 0430007-xxxxxxx
108600060524IntFat"0430060" Uni"N" xxxxxxxxxxxxxx Spese 12,000
108700060522ProtIn"0123456" ProtCl"9876543"DataProtCl 01/01/06
108800060522Cod.012-xxxxxxxxx C/C 1234567890123456 01245/67890
108900060601Blc."8" Tipo"A" C/C 123456789012 Cin"E"05555/63634
109000060522I   N   S   E   R   I   M   E   N   T   O
109100060522I N S E R I M E N T O     D A     O F F E R T A
109200060522I N S E R I M E N T O     D A     C O P I A
109300060529Dati variati ma non registrati :
109400061030Codice Fiscale "1234567890123456"
109500060518**
109600060928             "1234567890123456"               "xx"
109700060522             "ST"     "B"       "010"          "N"
109800060522                   "D"             0430007-xxxxxxx
109900060524      "0430060"    "N" xxxxxxxxxxxxxx       12,000
110000060522      "0123456"       "9876543"           01/01/06
110100060522    012-xxxxxxxxx     1234567890123456 01245/67890
110200060601    " "     "A"                     " "     /
110300061030               "1234567890123456"
110400060523**
110500060530Selezione  Dati Variati ERRATA
