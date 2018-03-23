000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200170508      * fnlv37R *----------------------------------------------------*
000300170608      *        VISUALIZZAZIONE VARIAZIONI anagrafiche gestionali
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
001800170509     ffiapd01l  if   e           k disk    prefix(x)
001900170509     ffiapvt1l  if   e           k disk
002000170509     ffiapvd1l  if   e           k disk
002100060522     ftntbe02l  if   e           k disk
002200170508     ffnlv37d   cf   e             workstn sfile(lv37s01:NRR1)
002300170508     f                                     sfile(lv37S03:NRR3)
002400170525     dDESPRI           s             50    DIM(16) CTDATA PERRCD(1)
002500170509     d*ESDOP           s             50    DIM(8) CTDATA PERRCD(1)
002600060523     dmsg              s             78    DIM(1) CTDATA PERRCD(1)
002700060523     d
002800170509     dDavar            s              1    DIM(5)
002900170509     dDavarEs          s              1    DIM(5)
003000170509     dapd              s              1    DIM(25)
003100170509     dapddeb           s              7    DIM(25)
003200030122
003300170509     d DAPD_A        e DS
003400170509     d DAPD_A1       e DS
003500170509     d DAPD_A2       e DS
003600170509     d DAPD_B        e DS
003700170509     d DAPD_B1       e DS
003800170509     d DAPD_C        e DS
003900170509     d DAPD_C1       e DS
004000170509     d DAPD_D        e DS
004001170524     d DAPD_D1       e DS                  prefix(§)
004100170509     d DAPD_T        e DS
004200170509      *
004300170515     d fnlv37ds      e ds
004600030612     d kpjba         e ds
004700030612     d ddatiute      e ds
004800030612     d azuteds       e ds                  extname(AZUTE00F)
004900030612     d tibs34ds      e ds                  inz
005000060523     d tibs02ds      e ds                  inz
005100170509     d dapd          e ds
005101171204     d dap4          e ds
005200030612      *
005300060523     d wrecord         s              1
005400060523     d wemfor          s              1
005500060523     d sfl             s              1
005600060524     d emetti          s              1
005700060522     D Kke1            S                   LIKE(tbeke1)
005800060522     D KSIF            S                   LIKE(tbesif)
005900060522     D KLIN            S                   LIKE(tbelin)
006000060522     D KCODt           S                   LIKE(tbecod)
006100060523     D savsel          S                   LIKE(v1csel)
006200060530     D primasel        S                   LIKE(v1csel)
006300060522     d cortoprima      s              8
006400060522     d cortodopo       s              8
006500060523     d wdata           s              6  0
006600060522     d Dataiso         s               d   datfmt(*iso)
006700060522     d Datadmy         s               d   datfmt(*dmy)
006800130905      *
006900170509      * ?___________________________________________________________________
007000021212     c     *ENTRY        PLIST
007100060523     c                   PARM                    kpjba
007200170508     c                   movel     kpjbu         fnlv37Ds
007201170524     c     ilv37tip      comp      'C'                                    40
007202170524     c     ilv37tip      comp      'D'                                    41
007300021212      *
007400021212      *   P U L I Z I A   S U B F I L E
007500021212      *
007600021212      *
007700021212      * Riempio i campi del control
007800021212     c                   Exsr      RieCtl
007900060519     c* Caricamento sfl
008000060523     c                   eval      savsel='9'
008100060523     c                   eval      sfl='S'
008200060523     c                   dow       *inkl='0' and v1csel<>savsel
008300060523     c
008400060518     C                   EXSR      RieSfl
008500021212      *
008600021212      *  E M E T T O    S U B F I L E
008700021212      *
008800021218     C     NRR1          IFNE      0
008900060518     c                   seton                                        20
009000030722     C     *INKL         DOWEQ     *off
009100060523     c     v1csel        andeq     savsel
009200060523     c* Sintetico
009300060523     c                   if        sfl='S'
009400060523     c                   EXSR      EMISSSint
009500060523     c                   endif
009600060523     c* Analitico
009700060523     c                   if        sfl='A'
009800060523     c                   EXSR      EMISSAnal
009900060523     c                   endif
010000060518     c
010100060518     C                   ENDDO
010200021212     C                   ENDIF
010300060518     c
010400030122      * Subfile vuoto emetto una videata diversa
010500030122     c                   If        Nrr1 = *Zeros
010600060704     c                   Dow       Not *Inkl  and v1csel = savsel
010700060704     c     fornodati     tag
010800170508     C                   WRITE     lv37T01
010900170508     C                   WRITE     lv37Z01
011000170508     C                   WRITE     lv37D01
011100060704     c     fornodatiE    tag
011200170508     C                   EXFMT     lv37C01
011300060704     c                   setoff                                       28
011400060704     c                   clear                   wemfor
011500060704     c
011600060704     c                   if        not *inkl
011700060704     c* Controllo selezione
011800060704     c                   exsr      CTRSEL
011900060704     c* Riemetto tutto
012000060704     c                   if        wemfor='S'
012100060704     c                   goto      fornodati
012200060704     c                   endif
012300060704     c* Emetto per msg di errore
012400060704     c   28              goto      fornodatiE
012500060704     c                   EndIf
012600060704     c
012700030122     c                   EndDo
012800030122     c                   EndIf
012900060523     c                   EndDo
013000021212      *
013100030122     C                   SETON                                        LR
013200170509      * ?___________________________________________________________________
013300021212      *    Riempio i campi del control
013400170509      * ?___________________________________________________________________
013500021212     C     RieCtl        BegSr
013600030611      *
013700170509     C                   clear                   V1Ctip
013800170509     C                   clear                   V1Cpdr
013900060518     C                   clear                   V1crag
014000021212      *
014100060518      * Codice
014200170509     C                   move      Ilv37tip      V1Ctip
014300170509     C                   move      Ilv37pdr      V1Cpdr
014400030612      *
014500170509      * Ragione sociale di fatturazione
014600170509     C     Kapd          chain     fiapd01l
014700170509     C                   IF        %found(fiapd01l)
014800170509     C                   MOVEL     xapdrsc       V1crag
014900030613     C                   ELSE
015000030613      * campo passato in DS
015100170509     C                   MOVEL     *all'?'       V1Crag
015200030613     C                   ENDIF
015300030613     C
015400030612      *
015500021212     C                   EndSr
015600170509      * ?___________________________________________________________________
015700060523      *    Emissione SFL Sintetico
015800170509      * ?___________________________________________________________________
015900060523     c     EmissSint     BEGSR
016000060523     c                   setoff                                       ki
016100060523     c                   eval      sfl='S'
016200060523     c* F9 - cambio da analitico a sintetico
016300060523     C     *INKI         DOWEQ     *off
016400060523     C     *INKL         andeq     *off
016500060523     c     v1csel        andeq     savsel
016600060523     C                   Z-ADD     1             NRR1
016700060523     C                   Z-ADD     1             REC1
016800060524     c     for01t        tag
016900170508     C                   WRITE     lv37T01
017000170508     C                   WRITE     lv37Z01
017100060523     c     for01         tag
017200170508     C                   EXFMT     lv37C01
017300060523     c                   setoff                                       28
017400060523     c                   clear                   wemfor
017500060523     c
017600060530     c                   if        not *inkl
017700060523     c* Controllo selezione
017800060523     c                   exsr      CTRSEL
017900060524     c* Riemetto tutto
018000060523     c                   if        wemfor='S'
018100060524     c                   goto      for01t
018200060523     c                   endif
018300060524     c* Emetto per msg di errore
018400060524     c   28              goto      for01
018500060523     c
018600060523     c* Verifico se richiesta opzione 5
018700060523     c                   exsr      Leggisfl
018800060523     c                   endif
018900060530     c* F9 - cambio sfl in analitico
019000060523     c                   if        *inki
019100060523     c                   eval      sfl='A'
019200060523     c                   endif
019300060523     c*
019400060523     C                   ENDDO
019500060523     c                   ENDSR
019600170509      * ?___________________________________________________________________
019700060523      *    Emissione SFL analitico
019800170509      * ?___________________________________________________________________
019900060523     c     EmissAnal     BEGSR
020000060523     c                   setoff                                       ki
020100060523     c                   eval      sfl='A'
020200060523     C     *INKI         DOWEQ     *off
020300060523     C     *INKL         andeq     *off
020400060523     c     v1csel        andeq     savsel
020500060529     C  n16              Z-ADD     1             NRR3              4 0
020600060529     C  n16              Z-ADD     1             REC3
020700060524     c     for03t        tag
020800170508     C                   WRITE     lv37T01
020900170508     C                   WRITE     lv37Z03
021000060523     c     for03         tag
021100170508     C                   EXFMT     lv37C03
021200060523     c                   setoff                                       28
021300060523     c                   clear                   wemfor
021400060530     c                   if        not *inkl
021500060523     c* Controllo selezione
021600060523     c                   exsr      CTRSEL
021700060523     c                   if        wemfor='S'
021800060524     c                   goto      for03t
021900060523     c                   endif
022000060524     c   28              goto      for03
022100060523     c                   endif
022200060523     c* F9 - cambio sfl in  sintetico
022300060523     c                   if        *inki
022400060523     c                   eval      sfl='S'
022500060523     c                   endif
022600060523     C                   ENDDO
022700060523     c
022800060523     c                   ENDSR
022900170509      * ?___________________________________________________________________
023000021212      *    Riempio i campi del subfile
023100170509      * ?___________________________________________________________________
023200021212     c     RieSfl        BegSr
023300060523     c                   eval      *in20 = *off
023400060523     c                   eval      *in21 = *off
023500060523     c                   eval      *in23 = *off
023600060523      *
023700170508     c                   WRITE     lv37C01
023800170508     c                   WRITE     lv37C03
023900060523      *
024000060523     c                   eval      *in21 = *on
024100060523     c                   eval      *in23 = *on
024200060523      *
024300060523     c                   Z-ADD     0             NRR1              4 0
024400060523     c                   Z-ADD     0             NRR3              4 0
024500060522     c*
024600170509     c     kapvt         setll     fiapvt1l
024700170509     c                   do        *hival
024800170509     c     kapvt         reade     fiapvt1l
024900170509     c                   if        %eof(fiapvt1l)
025000170509     c                   leave
025100170509     c                   endif
025200060519     c*
025300060519     c                   if        v1csel=' '
025400060519     c                   eval      wrecord='S'
025500060519     c                   else
025600060519     c                   clear                   wrecord
025700060519     c                   endif
025800060518     c* Sintetico
025900060518     c
026000060518     c* Data  variazione PdC
026100170509     c     *iso          movel     apvtdav       dataiso
026200060522     c                   movel     dataiso       datadmy
026300060522     c                   movel     datadmy       v1sdav
026400060530     c                   movel     datadmy       v3sdav
026500060518     c
026600060601     c* decodifico il pgm
026700170516     c* per ora no       movel     apvtpgm       v1spgm
026701170516     c                   clear                   v1spgm
026800060601     c
026900060601     c* Se inserimento scrivo da dove
027000060601     c                   clear                   v1sins
027100170525     c*m                 if        apvtcta <> 'M'
027200060601     c                   select
027201170525     C                   when      apvtcta = 'M' and ilv37tip = 'A'
027202170525     c                   eval      v1sins = despri(14)
027203170525     C                   when      apvtcta = 'M' and ilv37tip = 'C'
027204170525     c                   eval      v1sins = despri(15)
027205170525     C                   when      apvtcta = 'M' and ilv37tip = 'D'
027206170525     c                   eval      v1sins = despri(16)
027300170509     C                   when      apvtcta = 'I'
027400170509     c                   eval      v1sins = despri(1)
027500170509     C                   when      apvtcta = 'A'
027600170509     c                   eval      v1sins = despri(2)
027700170509     C                   when      apvtcta = 'R'
027800170509     c                   eval      v1sins = despri(3)
027900060601     c                   endsl
028000170525     c*m                 endif
028100030611      *
028200170509     c                   z-add     apvtOrv       V1sOrv
028300170509     c                   z-add     apvtOrv       V3sOrv
028400170509     c                   movel     apvtPru       V1sPru
028500170509     c                   movel     apvtPru       V3sPru
028600170509     c                   movel     apvtNoj       V1sNoj
028700170509     c                   movel(P)  apvtNoj       V3sNoj
028800060518     c* dati modificati
028900060518     c                   clear                   v1sana
029000170509     c                   clear                   v1sdis
029100170509     c                   clear                   v1star
029200170509     c                   clear                   v1smez
029300170509     c                   clear                   v1stel
029700060524     c                   clear                   DaVarEs
029800060522     c
029900170509     c                   movea     apvtflo       DaVar
030000170509     c                   do        5             xx                2 0
030100060522     c                   if        daVar(XX)<>' '
030200060518     c*
030300060522     c                   if        DaVar(xx)='A'
030400060519     c
030500060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
030600060519     c                   eval      wrecord='S'
030700060519     c                   endif
030800060518     c                   eval      v1sana='SI'
030900060518     c                   endif
031000060519     c
031100060522     c                   if        DaVar(xx)='B'
031200060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
031300060519     c                   eval      wrecord='S'
031400060519     c                   endif
031500170509     c                   eval      v1sdis='SI'
031600060518     c                   endif
031700060519     c
031800060522     c                   if        DaVar(xx)='C'
031900060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
032000060519     c                   eval      wrecord='S'
032100060519     c                   endif
032200170509     c                   eval      v1star='SI'
032300060518     c                   endif
032400060519     c
032500170509     c                   if        DaVar(xx)='D'
032600060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
032700060519     c                   eval      wrecord='S'
032800060519     c                   endif
032900170509     c                   eval      v1smez='SI'
033000060518     c                   endif
033100060519     c
033200170509     c                   if        DaVar(xx)='T'
033300060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
033400060519     c                   eval      wrecord='S'
033500060519     c                   endif
033600170509     c                   eval      v1stel='SI'
033700060518     c                   endif
033800060522     c
033900060522     c                   endif
034000060522     c                   enddo
034100060519     c
034200060519     c* Leggo e scrivo analitico
034300060519     c                   if        wrecord='S'
034400060519     c                   Exsr      SCRiana
034500060524     c
034600060524     c* Scrivo un record in analitico per dati variati non registrati
034700060524     c* solo per variazione
034800170509     c                   if        apvtcta='M'
034900060524     c                   Exsr      Datinoregis
035000060524     c                   endif
035100060530     c
035200060530     c* visto che almeno una riga l'ho scritta in analitico, aggancio
035300060530     c*  l'ultima per sottolineare i campi
035400170508     c     nrr3          chain     lv37s03                            31
035500060530     c                   if        not *in31
035600060530     c                   movel     *off          *inkl
035700060530     c                   movel     in50          *in50
035800060530     c                   movel     in51          *in51
035900060530     c                   if        not *in50
036000060530     c                   clear                   v3spru
036100060530     c                   clear                   v3snoj
036200060530     c                   clear                   v3sorv
036300060530     c                   clear                   v3sdav
036400060530     c                   endif
036500060530     c                   seton                                        5051
036600060530     c                   movel     *in50         in50
036700060530     c                   movel     *in51         in51
036800170508     c                   update    lv37s03
036900060530     c                   endif
037000060530     c
037100060530     c
037200060523     c
037300060523     c* Se non ho caricato nessun record di analitico, proteggo la selezione
037400060523     c                   if        v1sreca=0
037500060523     c                   seton                                        52
037600060523     c                   endif
037700060523     c                   movel     *in52         in52
037800060518     c
037900060518     c                   add       1             nrr1
038000170508     c                   write     lv37s01
038100060523     c
038200060523     c                   setoff                                       52
038300060519     c                   endif
038400060519     c*
038500060519     c                   enddo
038600060523     c
038700060523     c                   eval      savsel=v1csel
038800060518     c*
038900021212     c                   EndSr
039000170509      * ?___________________________________________________________________
039100060519      *    Scrivo sfl analitico
039200170509      * ?___________________________________________________________________
039300060519     C     Scriana       BEGSR
039400060522     c                   seton                                        50
039500060530     c                   setoff                                       51
039600060522     c                   setoff                                       16
039700060522     c                   clear                   v1sreca
039800060523     c
039900060523     c* Se non manutenzione non devo caricare la DS
040000170509    2c                   if        apvtcta<>'M'
040100060523     c                   exsr      car_ins
040200060523     c
040300060523   x2c                   else
040400060523     c
040500060522     c                   if        v1csel=' '
040600170509     c     kapvd         setll     fiapvd1l
040700170509     c     kapvd         reade     fiapvd1l
040800060522     c                   else
040900170509     c     kapvds        setll     fiapvd1l
041000170509     c     kapvds        reade     fiapvd1l
041100060522     c                   endif
041200060519     c
041300170509    1c                   dow       not %eof(fiapvd1l)
041400060522     c
041500060522     c* Imposto la DS e per il primo record memorizzo in nrr nel sfl
041600060522     c*  sintetico
041700060522    4c                   select
041800170509     c                   when      apvdapd ='A'
041801170516     c                   z-add     1             yy                2 0
041802170516     c     apvdapd       lookup    apd(yy)                                32
041803170516     c   32              eval      v1sdt1 = apddeb(yy)
041900170509     c                   if        apvdprg=0
042000060522     c                   exsr      car_A
042100061030     c                   else
042200170509     c                   if        apvdprg=1
042300170509     c                   exsr      car_A1
042400170509     c                   else
042500170509     c                   exsr      car_A2
042600061030     c                   endif
042700170509     c                   endif
042800170509     c                   when      apvdapd ='B'
042801170516     c                   z-add     1             yy                2 0
042802170516     c     apvdapd       lookup    apd(yy)                                32
042803170516     c   32              eval      v1sdt1 = apddeb(yy)
042900170509     c                   if        apvdprg=0
043000170509     c                   exsr      car_B
043100170509     c                   else
043200170509     c                   exsr      car_B1
043300170509     c                   endif
043400170509     c                   when      apvdapd ='C'
043401170516     c                   z-add     1             yy                2 0
043402170516     c     apvdapd       lookup    apd(yy)                                32
043403170516     c   32              eval      v1sdt1 = apddeb(yy)
043500170509     c                   if        apvdprg=0
043600170509     c                   exsr      car_C
043700170509     c                   else
043800170509     c                   exsr      car_C1
043900170509     c                   endif
044000170509     c                   when      apvdapd ='D'
044001170516     c                   z-add     1             yy                2 0
044002170516     c     apvdapd       lookup    apd(yy)                                32
044003170516     c   32              eval      v1sdt1 = apddeb(yy)
044004170524     c                   if        apvdprg=0
044100170509     c                   exsr      car_D
044101170524     c                   else
044102170524     c                   exsr      car_D1
044103170524     c                   endif
044200170509     c                   when      apvdapd ='T'
044201170516     c                   z-add     1             yy                2 0
044202170516     c     apvdapd       lookup    apd(yy)                                32
044203170516     c   32              eval      v1sdt1 = apddeb(yy)
044300170509     c                   exsr      car_T
044400060522    4c                   endsl
044500060522     c
044600060522    2c                   if        v1csel=' '
044700170509     c     kapvd         reade     fiapvd1l
044800060522     c                   else
044900170509     c     kapvds        reade     fiapvd1l
045000060522    2c                   endif
045100060522    1c                   enddo
045200060530     c* finita la scrittura
045300060523     c
045400060523    2c                   endif
045500060519     c
045600060519     c                   EndSr
045700170509      * ?___________________________________________________________________
045800170508      *    SCRIVO RECORD IN SFL lv37S03
045900170509      * ?___________________________________________________________________
046000060523     C     SCRIVIS03     BEGSR
046100060523     c                   add       1             nrr3
046200060523     c                   movel     *in50         in50
046300060530     c                   movel     *in51         in51
046400170508     c                   write     lv37s03
046500060523     c*
046600060523     c* Imposto se il primo nrr3
046700060523     c                   if        v1sreca=0
046800060523     c                   z-add     nrr3          v1sreca
046900060523     c                   endif
047000060524     c                   setoff                                       50
047100060524     c
047200060524     c* Memorizzo il record dati variati scritto in analitico
047300060524     c                   z-add     1             xx
047400170509     c     apvdapd       lookup    daVar(xx)                              30
047500060524    4c                   if        *in30
047600060524     c                   eval      DaVarEs(XX)='S'
047700060524    4c                   endif
047800060523     C                   ENDSR
047900170509      * ?___________________________________________________________________
048000060524      *    Scrivo un record in sfl analitico per dati variati non regis
048100170509      * ?___________________________________________________________________
048200060524     C     datinoregis   BEGSR
048300060524     c                   clear                   v1sdt1
048400060524     c**                 clear                   v1spd2
048500060524     c                   clear                   v1sva2
048600060524     c                   clear                   emetti
048700060524     c                   movel     despri(11)    v1sva1
048800060524     c                   z-add     1             xx
048900170509     c                   do        5             xx
049000060524     c                   if        DaVar(XX)<>' ' and DaVarEs(xx)<>'S'
049100060524     c
049200060524     c* Memorizzo se in questo record c'e' il tipo dati selezionato
049300060524     c                   if        (v1csel<>' ' and davar(xx)=v1csel) or
049400060524     c                              v1csel=' '
049500060524     c
049600060524     c                   z-add     1             yy                2 0
049700170509     c     davar(xx)     lookup    apd(yy)                                30
049800060524     c                   if        *in30
049900170509     c                   movel     apddeb(yy)    des7a             7
050000060524     c                   else
050100060524     c                   movel     *all'?'       des7a
050200060524     c                   endif
050300060524     c* riempo stringa
050400060524     c                   eval      v1sva2=%trim(v1sva2) + ' '+ des7a + ' /'
050500060524     c                   endif
050600060524     c                   endif
050700060524     c                   enddo
050800060524     c
050900060524     c                   if        v1sva2<>*blanks
051000060530     c                   eval      v1sva2=%trim(v1sva2)
051100060524     c                   exsr      ScriviS03
051200060524     c                   endif
051300060524     c
051400060524     C                   ENDSR
051500170509      * ?___________________________________________________________________
051600060522      *    Carico record inserimento
051700170509      * ?___________________________________________________________________
051800060522     C     Car_ins       BEGSR
051900060522     C                   MOVEL     *blanks       V1SDT1
052000060524     c***                movel     *blanks       v1spd2
052100060522     c                   select
052200170509     C                   when      ApvtCTA='I'
052300170509     c                   movel     despri(1)     v1sva2
052400170509     C                   when      ApvtCTA='A'
052500170509     c                   movel     despri(2)     v1sva2
052600170509     C                   when      ApvtCTA='R'
052700170509     c                   movel     despri(3)     v1sva2
052800060522     c                   endsl
052900060524     c                   movel     *BLANKS       v1sva1
053000060530     c                   seton                                        51
053100060523     c
053200060523     c                   exsr      ScriviS03
053300060530     c                   setoff                                       51
053400060522     c
053500060522     c                   endsr
053600170509      * ?___________________________________________________________________
053700060522      *    Scrivo sfl per tipo record "A"
053800170509      * ?___________________________________________________________________
053900060522     C     Car_A         BEGSR
054000060522     c*
054100060522     c* Riga prima
054200170509     c                   movel     apvdprima     dapd_A
054300170509     c                   movel     despri(4)     v1sva1
054400170509     c                   eval      %subst(v1sva1:11:7) = %editc(apdksc:'X')
054500170524     c                   eval      %subst(v1sva1:23:3) = apdcsf
054600170509     c                   eval      %subst(v1sva1:36:1) = apdpdd
054700170509     c                   eval      %subst(v1sva1:50:1) = apdfcm
054800060522     c* Riga dopo
054900170509     c                   movel     apvddopo      dapd_A
055000170509     c                   move      *blank        v1sva2
055100170509     c                   eval      %subst(v1sva2:11:7) = %editc(apdksc:'X')
055200170524     c                   eval      %subst(v1sva2:23:3) = apdcsf
055300170509     c                   eval      %subst(v1sva2:36:1) = apdpdd
055400170509     c                   eval      %subst(v1sva2:50:1) = apdfcm
055500060523     c
055600060523     c                   EXSR      ScriviS03
055700060522     c
055800060522     c                   ENDSR
055900170509      * ?___________________________________________________________________
056000170509      *    Scrivo sfl per tipo record "A1"
056100170509      * ?___________________________________________________________________
056200170509     C     Car_A1        BEGSR
056300170509     c*
056400170509     c* Riga prima
056500170509     c                   movel     apvdprima     dapd_A1
056600170509     c                   movel     despri(5)     v1sva1
056700170509     c                   eval      %subst(v1sva1:15:35) = apdrsf
056800170509     c* Riga dopo
056900170509     c                   movel     apvddopo      dapd_A1
057000170509     c                   move      *blank        v1sva2
057100170509     c                   eval      %subst(v1sva2:15:35) = apdrsf
057200170509     c
057300170509     c                   EXSR      ScriviS03
057400170509     c
057500170509     c                   ENDSR
057600170509      * ?___________________________________________________________________
057700170509      *    Scrivo sfl per tipo record "A2"
057800170509      * ?___________________________________________________________________
057900170509     C     Car_A2        BEGSR
058000170509     c*
058100170509     c* Riga prima
058200170509     c                   movel     apvdprima     dapd_A2
058300170509     c                   movel     despri(6)     v1sva1
058400170509     c                   eval      %subst(v1sva1:15:35) = apdrsc
058500170509     c* Riga dopo
058600170509     c                   movel     apvddopo      dapd_A2
058700170509     c                   move      *blank        v1sva2
058800170509     c                   eval      %subst(v1sva2:15:35) = apdrsc
058900170509     c
059000170509     c                   EXSR      ScriviS03
059100170509     c
059200170509     c                   ENDSR
059300170509      * ?___________________________________________________________________
059400170509      *    Scrivo sfl per tipo record "B"
059500170509      * ?___________________________________________________________________
059600170509     C     Car_B         BEGSR
059700170509     c*
059800170509     c* Riga prima
059900170509     c                   movel     apvdprima     dapd_B
060000170509     c                   movel     despri(7)     v1sva1
060100170509     c                   eval      %subst(v1sva1:9:1) = §APDTPM
060200170509     c                   eval      %subst(v1sva1:22:1) = §APDTSTPDA
060300170509     c                   eval      %subst(v1sva1:33:1) = §APDPDA
060400170509     c                   eval      %subst(v1sva1:41:1) = §APDTURNO
060500170509     c                   eval      %subst(v1sva1:50:1) = APDSNU
060600170509     c* Riga dopo
060700170509     c                   movel     apvddopo      dapd_b
060800170509     c                   move      *blank        v1sva2
060900170510     c                   eval      %subst(v1sva2:9:1) = §APDTPM
061000170510     c                   eval      %subst(v1sva2:22:1) = §APDTSTPDA
061100170510     c                   eval      %subst(v1sva2:33:1) = §APDPDA
061200170510     c                   eval      %subst(v1sva2:41:1) = §APDTURNO
061300170510     c                   eval      %subst(v1sva2:50:1) = APDSNU
061400170509     c
061500170509     c                   EXSR      ScriviS03
061600170509     c
061700170509     c                   ENDSR
061800170509      * ?___________________________________________________________________
061900170509      *    Scrivo sfl per tipo record "B1"
062000170509      * ?___________________________________________________________________
062100170509     C     Car_B1        BEGSR
062200170509     c*
062300170509     c* Riga prima
062400170509     c                   movel     apvdprima     dapd_B1
062500170509     c                   movel     despri(8)     v1sva1
062600170509     c                   eval      %subst(v1sva1:13:2) = §APDGCD
062700170524     c                   eval      %subst(v1sva1:22:1) = §APDATS
062800170524     c                   eval      %subst(v1sva1:31:1) = §APDAAT
062900170524     c                   eval      %subst(v1sva1:39:1) = §APDSPEC
063000170509     c                   eval      %subst(v1sva1:49:2) = APDPTP
063100170509     c* Riga dopo
063200170509     c                   movel     apvddopo      dapd_b1
063300170509     c                   move      *blank        v1sva2
063400170510     c                   eval      %subst(v1sva2:13:2) = §APDGCD
063500170524     c                   eval      %subst(v1sva2:22:1) = §APDATS
063600170524     c                   eval      %subst(v1sva2:31:1) = §APDAAT
063700170524     c                   eval      %subst(v1sva2:39:1) = §APDSPEC
063800170510     c                   eval      %subst(v1sva2:49:2) = APDPTP
063900170509     c
064000170509     c                   EXSR      ScriviS03
064100170509     c
064200170509     c                   ENDSR
064300170509      * ?___________________________________________________________________
064400170509      *    Scrivo sfl per tipo record "C"
064500170509      * ?___________________________________________________________________
064600170509     C     Car_C         BEGSR
064700170509     c*
064800170509     c* Riga prima
064900170509     c                   movel     apvdprima     dapd_C
065000170509     c                   movel     despri(9)     v1sva1
065100170509     c                   eval      %subst(v1sva1:13:1) = APDFGR
065200170509     c                   eval      %subst(v1sva1:25:1) = APDETM
065300170509     c                   eval      %subst(v1sva1:31:1) = APDETP
065400170509     c                   eval      %subst(v1sva1:44:1) = APDPKM
065500170509     c                   eval      %subst(v1sva1:50:1) = APDPKP
065600170509     c* Riga dopo
065700170509     c                   movel     apvddopo      dapd_c
065800170509     c                   move      *blank        v1sva2
065900170510     c                   eval      %subst(v1sva2:13:1) = APDFGR
066000170510     c                   eval      %subst(v1sva2:25:1) = APDETM
066100170510     c                   eval      %subst(v1sva2:31:1) = APDETP
066200170510     c                   eval      %subst(v1sva2:44:1) = APDPKM
066300170510     c                   eval      %subst(v1sva2:50:1) = APDPKP
066400170509     c
066500170509     c                   EXSR      ScriviS03
066600170509     c
066700170509     c                   ENDSR
066800170509      * ?___________________________________________________________________
066900170509      *    Scrivo sfl per tipo record "C1"
067000170509      * ?___________________________________________________________________
067100170509     C     Car_C1        BEGSR
067200170509     c*
067300170509     c* Riga prima
067400170509     c                   movel     apvdprima     dapd_C1
067500170509     c                   movel     despri(10)    v1sva1
067600170524     c                   eval      %subst(v1sva1:13:1) = APDCAM
067700170524     c                   eval      %subst(v1sva1:20:1) = APDCAP
067800170524     c                   eval      %subst(v1sva1:36:1) = APDSCM
067900170524     c                   eval      %subst(v1sva1:44:1) = APDSCP
068000170509     c* Riga dopo
068100170509     c                   movel     apvddopo      dapd_c1
068200170509     c                   move      *blank        v1sva2
068300170524     c                   eval      %subst(v1sva2:13:1) = APDCAM
068400170524     c                   eval      %subst(v1sva2:20:1) = APDCAP
068500170524     c                   eval      %subst(v1sva2:36:1) = APDSCM
068600170524     c                   eval      %subst(v1sva2:44:1) = APDSCP
068700170509     c
068800170509     c                   EXSR      ScriviS03
068900170509     c
069000170509     c                   ENDSR
069100170509      * ?___________________________________________________________________
069200170509      *    Scrivo sfl per tipo record "D"
069300170509      * ?___________________________________________________________________
069400170509     C     Car_D         BEGSR
069500170509     c*
069600170509     c* Riga prima
069700170509     c                   movel     apvdprima     dapd_D
069800170509     c                   movel     despri(11)    v1sva1
069900170510     c                   eval      %subst(v1sva1:7:9) = %editc(§APDMCP:'2')
070000170510     c                   eval      %subst(v1sva1:22:9) = %editc(APDPKL:'2')
070100170510     c                   eval      %subst(v1sva1:37:7) = %editc(APDVLU:'2')
070200170509     c                   eval      %subst(v1sva1:48:3) = §APDKM
070300170509     c* Riga dopo
070400170509     c                   movel     apvddopo      dapd_d
070500170509     c                   move      *blank        v1sva2
070600170510     c                   eval      %subst(v1sva2:7:9) = %editc(§APDMCP:'2')
070700170510     c                   eval      %subst(v1sva2:22:9) = %editc(APDPKL:'2')
070800170510     c                   eval      %subst(v1sva2:37:7) = %editc(APDVLU:'2')
070900170510     c                   eval      %subst(v1sva2:48:3) = §APDKM
071000170509     c
071100170509     c                   EXSR      ScriviS03
071200170509     c
071300170509     c                   ENDSR
071301170524      * ?___________________________________________________________________
071302170524      *    Scrivo sfl per tipo record "D"
071303170524      * ?___________________________________________________________________
071304170524     C     Car_D1        BEGSR
071305170524     c*
071306170524     c* Riga prima
071307170524     c                   movel     apvdprima     dapd_D1
071308170524     c                   movel     despri(13)    v1sva1
071309170524     c                   eval      %subst(v1sva1:7:9) = %editc(§§APDMCP:'2')
071313170524     c* Riga dopo
071314170524     c                   movel     apvddopo      dapd_d1
071315170524     c                   move      *blank        v1sva2
071316170524     c                   eval      %subst(v1sva2:7:9) = %editc(§§APDMCP:'2')
071320170524     c
071321170524     c                   EXSR      ScriviS03
071322170524     c
071323170524     c                   ENDSR
071400170509      * ?___________________________________________________________________
071500170509      *    Scrivo sfl per tipo record "T"
071600170509      * ?___________________________________________________________________
071700171205     C     Car_Told      BEGSR
071800170509     c*
071900170509     c* Riga prima
072000170509     c                   movel     apvdprima     dapd_T
072100170509     c                   movel     despri(12)    v1sva1
072200170509     c                   eval      %subst(v1sva1:18:16) = §APD4TEL
072300170509     c* Riga dopo
072400170509     c                   movel     apvddopo      dapd_T
072500170509     c                   move      *blank        v1sva2
072600170509     c                   eval      %subst(v1sva2:18:16) = §APD4TEL
072700170509     c
072800170509     c                   EXSR      ScriviS03
072900170509     c
073000170509     c                   ENDSR
073001171204      * ?___________________________________________________________________
073002171204      *    Scrivo sfl per tipo record "T"
073003171204      * ?___________________________________________________________________
073004171207     C     Car_T         BEGSR
073005171207     c*
073006171207     c                   movel     'AP4'         KcodT
073007171207     c                   movel(p)  apvd4trc      kke1
073008171207     c     ktbe          setll     tntbe02l
073009171207     c                   do        *hival
073010171207     c     ktbe          reade     tntbe02l
073011171207     c                   if        %eof(tntbe02l)
073012171207     c                   leave
073013171207     c                   endif
073014171211      *se record annullato salta
073015171211     c                   if        tbeatb <> *blank
073016171211     c                   iter
073017171211     c                   endif
073018171207     c                   movel     tbeuni        dap4
073019171207      * se è diverso lo scrivo altrimenti passo al successivo campo se esiste
073020171207      *
073021171207     c                   if        %subst(apvdprima:§ap4pos:§ap4lun) <>
073022171207     c                             %subst(apvddopo:§ap4pos:§ap4lun)
073023171207     c* Riga prima
073024171207     c                   move      *blank        v1sva1
073025171207     c                   eval      %subst(v1sva1:1:15) = dap4
073026171207     c                   eval      %subst(v1sva1:17:§ap4lun) =
073027171207     c                             %subst(apvdprima:§ap4pos:§ap4lun)
073028171207     c* Riga dopo
073029171207     c                   move      *blank        v1sva2
073030171207     c                   eval      %subst(v1sva2:17:§ap4lun) =
073031171207     c                             %subst(apvddopo:§ap4pos:§ap4lun)
073032171207     c
073033171207     c                   EXSR      ScriviS03
073034171207     c                   endif
073035171207     c                   enddo
073036171207     c
073037171207     c                   ENDSR
073100170509      * ?___________________________________________________________________
073200060523      *    Controllo selezione  tipo dati variati
073300170509      * ?___________________________________________________________________
073400060523     C     CtrSel        BEGSR
073500060523     c* Leggo foramto testata
073600170508     c                   read      lv37t01
073700060523     c
073800060523     c                   clear                   v1dsel
073900060523     c                   if        v1csel='?'
074000060523     c                   Clear                   tibs02ds
074100060523     c                   Eval      T02Mod = 'R'
074200060523     c                   Eval      T02Sif = Knsif
074300170509     c                   Eval      T02Cod = 'APD'
074400060523     c                   Call      'TIBS02R'
074500060523     c                   Parm                    Kpjba
074600060523     c                   Parm                    tibs02ds
074700060523     c                   Movel     T02Ke1        v1csel
074800060524     c***                eval      wemfor='S'
074900060524     c                   endif
075000060523     c*
075100060523     c                   if        v1csel<>' '
075200060523     c                   Clear                   tibs02ds
075300060523     c                   Eval      T02Mod = 'C'
075400060523     c                   Eval      T02Sif = Knsif
075500170509     c                   Eval      T02Cod = 'APD'
075600060523     c                   movel(P)  v1csel        t02ke1
075700060523     c                   Call      'TIBS02R'
075800060523     c                   Parm                    Kpjba
075900060523     c                   Parm                    tibs02ds
076000060523If  2c                   If        T02Err <> *Blanks
076100060523     c                   eval      v1cmsg=msg(1)
076200060523     c                   seton                                        28
076300060523     c                   else
076400170509     c                   movel     t02uni        dapd
076500170509     c                   movel     §apddes1      v1dsel
076600060523     c                   endif
076700060523     c
076800060524     c***                endif
076900060523     c                   endif
077000060523     c
077100060523     c                   ENDSR
077200170509      * ?___________________________________________________________________
077300060530      *    Leggo sfl sintetico per vedere se selezionata opzione "5"
077400170509      * ?___________________________________________________________________
077500060523     C     Leggisfl      BEGSR
077600060530     c* mi salvo la selezione uguale alla nuova, poi la carico
077700060530     c* se modificata dopo aver visualizzato il dettaglio
077800060530     c                   movel     savsel        primasel
077900060530     c                   movel     v1csel        savsel
078000170508     c                   readc     lv37s01                                30
078100060523     c                   dow       not *in30
078200060523     c
078300060523     c                   if        v1ssce='5'
078400060523     c                   z-add     v1sreca       rec3
078500060523     c                   z-add     v1sreca       nrr3
078600170508     c     nrr3          chain     lv37s03                            31
078700060523     c                   seton                                        16
078800060523     c                   movel     in50          *in50
078900060530     c                   movel     in51          *in51
079000170508     c                   update    lv37s03
079100060523     c
079200060523     c                   eval      *inkl=*off
079300060523     c                   exsr      EmissAnal
079400060523     c
079500170508     c     nrr3          chain     lv37s03                            31
079600060523     c                   setoff                                       16
079700060523     c                   movel     in50          *in50
079800060530     c                   movel     in51          *in51
079900170508     c                   update    lv37s03
080000060523     c
080100060523     c                   eval      v1ssce=' '
080200060523     c                   endif
080300060523     c                   movel     in52          *in52
080400170508     c                   update    lv37s01
080500060523     c
080600170508     c                   readc     lv37s01                                30
080700060523     c                   enddo
080800060523     c*
080900060523     c                   eval      *inkl=*off
081000060530     c* reimposto al selezione per eventuale caricamento
081100060530     c                   movel     primasel      savsel
081200060530     c* Reimposto sfl sintetico alla fine
081300060530     c                   eval      sfl='S'
081400060530     c
081500060523     c                   ENDSR
081600170509      * ?___________________________________________________________________
081700021212      *    R O U T I N E     I N I Z I A L E
081800170509      * ?___________________________________________________________________
081900060522     C     *inzsr        BEGSR
082000021212      *
082100030612     c     *dtaara       define    §azute        azuteds
082200030612     c     *dtaara       define    §datiute      ddatiute
082300030612
082400030612     C                   in(E)     *dtaara
082500030612     C                   IF        %Error  or  RSUT = *blanks
082600030612     c                   clear                   tibs34ds
082700030612     C                   call      'TIBS34R'
082800030612     C                   parm                    Tibs34Ds
082900030612     C                   in        *dtaara
083000030612     c                   ENDIF
083100030612      *
083200030612     C                   z-add     1             codut             1 0
083300060522     c
083400170509     c*  Carico descrizioni brevi tabella apd
083500060522     c                   clear                   xx
083600170509     c                   movel     'APD'         KcodT
083700060522     c                   clear                   Ksif
083800060522     c                   clear                   Klin
083900060523     c     Ktbe2         setll     TNTBE02l
084000060523     c     Ktbe2         reade     TNTBE02l
084100060522do  2c                   dow       NOT %eof(TNTBE02L)
084200060522if  3c                   if        TBEatb   =  *blanks
084300170509     c                   movel     TBEuni        dapd
084400060522     c                   add       1             xx
084500170509     c                   movel     TBEke1        apd(xx)
084600170509     c                   movel     §apddes1      apddeb(xx)
084700060522e   3c                   endif
084800060523     c     Ktbe2         reade     TNTBE02l
084900060522e   2c                   enddo
085000060522      *
085100030612     c
085200030612      *
085300060522     C     ktbe2         KLIST
085400060522     C                   KFLD                    kcodt
085500060522     C                   KFLD                    klin
085600060522     C                   KFLD                    ksif
085700060522     C     ktbe          KLIST
085800060522     C                   KFLD                    kcodt
085900060522     C                   KFLD                    klin
086000060522     C                   KFLD                    ksif
086100060522     C                   KFLD                    kke1
086200170509     C     Kapvt         KLIST
086300170509     C                   KFLD                    Ilv37tip
086400170509     C                   KFLD                    Ilv37pdr
086500060519      *
086600170509     C     Kapvd         KLIST
086700170509     C                   KFLD                    Ilv37tip
086800170509     C                   KFLD                    Ilv37pdr
086900170509     C                   KFLD                    apvtdav
087000170509     C                   KFLD                    apvtorv
087100170509     C                   KFLD                    apvtnoj
087200170509     C     Kapvds        KLIST
087300170509     C                   KFLD                    Ilv37tip
087400170509     C                   KFLD                    Ilv37pdr
087500170509     C                   KFLD                    apvtdav
087600170509     C                   KFLD                    apvtorv
087700170509     C                   KFLD                    apvtnoj
087800060522     C                   KFLD                    v1csel
087900030612      *
088000170509     C     Kapd          KLIST
088100170509     C                   KFLD                    ilv37tip
088200170509     C                   KFLD                    Ilv37pdr
088300060522     c*
088400940726     C                   ENDSR
088500060518**
088600170525     I   N   S   E   R   I   M   E   N   T   O                                1
088700170525     A   N   N   U   L   L   A   M   E   N   T   O                            2
088800170525     R   I   P   R   I   S   T   I   N   O                                    3
088900170525Fornitore 1234567 Soc 123 Fittizio 1 Mer.Por/Rit 1                            4
089000170525Rag.Soc.Fatt.                                                                 5
089100170525Rag.Soc.Int.                                                                  6
089200170525Par/Mes 1 Test x Pda 1 Dati Pda 1 Turno 1 Occas. 1                            7
089300170525GG.ecc Ch.d 12 Terr. 1 Attiv. 1 Spec. 1 Banc.sv 12                            8
089400170525Tar.Int/Mez 1 Etic.Mat. 1 Pom 1 Piking Mat 1 Pom 1                            9
089500170525Carico Mat. 1 Pom. 1  Scarico Mat. 1  Pom. 1                                 10
089600170525Massa 1234567   Peso 1234567   Vol. 12345   Km 123                           11
089700170525Telefono Autista                                                             12
089701170525Massa 1234567                                                                13
089702170525        |     |     |     |     |     |                                      14
089703170525        |     |                                                              15
089704170525        |     |     |                                                        16
089800060523**
089900060530Selezione  Dati Variati ERRATA
