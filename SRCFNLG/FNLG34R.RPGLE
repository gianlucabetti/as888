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
001900170921     ftigfp01l  if   e           k disk
002000170921     ftigfd01l  if   e           k disk
002100060522     ftntbe02l  if   e           k disk
002101170921     ftabel00f  if   e           k disk
002200170921     ffnlg34d   cf   e             workstn sfile(lg34s01:NRR1)
002300170921     f                                     sfile(lg34S03:NRR3)
002400171012     dDESPRI           s             50    DIM(10) CTDATA PERRCD(1)
002600060523     dmsg              s             78    DIM(1) CTDATA PERRCD(1)
002700060523     d
002800170509     dDavar            s              1    DIM(5)
002900170509     dDavarEs          s              1    DIM(5)
003000170921     dgcp              s              1    DIM(25)
003100170922     dgfddeb           s              7    DIM(25)
003200030122
003300170921     d Dgcp_A        e DS
003400170921     d Dgcp_A1       e DS
003600170921     d Dgcp_B        e DS
003800170921     d Dgcp_C        e DS
004000170921     d Dgcp_D        e DS
004001171012     d Dgcp_E        e DS
004200170509      *
004300170921     d fnlg34ds      e ds
004600030612     d kpjba         e ds
004700030612     d ddatiute      e ds
004800030612     d azuteds       e ds                  extname(AZUTE00F)
004900030612     d tibs34ds      e ds                  inz
005000060523     d tibs02ds      e ds                  inz
005100170921     d dgcp          e ds
005200030612      *
005201170928     d data14          ds            14
005202170928     d  data14n                       8  0
005203170928     d  ora14n                        6  0
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
007200170921     c                   movel     kpjbu         fnlg34Ds
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
010800170921     C                   WRITE     lg34T01
010900170921     C                   WRITE     lg34Z01
011000170921     C                   WRITE     lg34D01
011100060704     c     fornodatiE    tag
011200170921     C                   EXFMT     lg34C01
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
013700170921     C                   clear                   V1Cagc
013800170921     C                   clear                   V1Cfgc
013900170921     C                   clear                   V1cngc
014000021212      *
014100060518      * Codice
014200170921     C                   move      Ilg34agc      V1Cagc
014300170921     C                   move      Ilg34fgc      V1Cfgc
014301170921     C                   move      Ilg34ngc      V1cngc
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
016900170921     C                   WRITE     lg34T01
017000170921     C                   WRITE     lg34Z01
017100060523     c     for01         tag
017200170921     C                   EXFMT     lg34C01
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
020800170921     C                   WRITE     lg34T01
020900170921     C                   WRITE     lg34Z03
021000060523     c     for03         tag
021100170921     C                   EXFMT     lg34C03
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
023700170921     c                   WRITE     lg34C01
023800170921     c                   WRITE     lg34C03
023900060523      *
024000060523     c                   eval      *in21 = *on
024100060523     c                   eval      *in23 = *on
024200060523      *
024300060523     c                   Z-ADD     0             NRR1              4 0
024400060523     c                   Z-ADD     0             NRR3              4 0
024500060522     c*
024600170921     c     kgiac         setll     tigfp01l
024700170509     c                   do        *hival
024800170921     c     kgiac         reade     tigfp01l
024900170921     c                   if        %eof(tigfp01l)
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
026000170921     c* Data  variazione
026001170928     c                   eval      data14 = %subst(%char(%dec(
026002170928     c                             gfptime ) ): 1 : 14 )
026100170928     c                   movel     data14n       dataiso
026200060522     c                   movel     dataiso       datadmy
026300060522     c                   movel     datadmy       v1sdav
026400060530     c                   movel     datadmy       v3sdav
026500060518     c
026600060601     c* decodifico il pgm
026700171003     c                   movel     gfppgm        v1spgm
026701171003     c*                  clear                   v1spgm
026800060601     c
026900060601     c* Se inserimento scrivo da dove
027000060601     c                   clear                   v1sins
027200060601     c                   select
027205170921     C                   when      gfpcta = 'M'
027206170921     c                   eval      v1sins = despri(9)
027300170921     C                   when      gfpcta = 'I'
027400170509     c                   eval      v1sins = despri(1)
027500170921     C                   when      gfpcta = 'A'
027600170509     c                   eval      v1sins = despri(2)
027700170921     C                   when      gfpcta = 'R'
027800170509     c                   eval      v1sins = despri(3)
027900060601     c                   endsl
028100030611      *
028101170928     c                   move      data14        v3sorv
028102170928     c                   move      data14        v1sorv
028400170921     c                   movel     gfpPru        V1sPru
028500170921     c                   movel     gfpPru        V3sPru
028600170921     c                   movel     gfpNoj        V1sNoj
028700170921     c                   movel(P)  gfpNoj        V3sNoj
028800060518     c* dati modificati
028900170922     c                   clear                   v1sfas
029000170922     c                   clear                   v1sdis
029100170922     c                   clear                   v1sspe
029200170922     c                   clear                   v1scas
029201171012     c                   clear                   v1scom
029700060524     c                   clear                   DaVarEs
029800060522     c
029900170921     c                   movea     gfpflo        DaVar
030000170509     c                   do        5             xx                2 0
030100060522     c                   if        daVar(XX)<>' '
030200060518     c*
030300060522     c                   if        DaVar(xx)='A'
030400060519     c
030500060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
030600060519     c                   eval      wrecord='S'
030700060519     c                   endif
030800170922     c                   eval      v1sspe='SI'
030900060518     c                   endif
031000060519     c
031100060522     c                   if        DaVar(xx)='B'
031200060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
031300060519     c                   eval      wrecord='S'
031400060519     c                   endif
031500170922     c                   eval      v1scas='SI'
031600060518     c                   endif
031700060519     c
031800060522     c                   if        DaVar(xx)='C'
031900060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
032000060519     c                   eval      wrecord='S'
032100060519     c                   endif
032200170922     c                   eval      v1sdis='SI'
032300060518     c                   endif
032400060519     c
032500170509     c                   if        DaVar(xx)='D'
032600060522     c                   if        v1csel<>' ' and v1csel=daVar(xx)
032700060519     c                   eval      wrecord='S'
032800060519     c                   endif
032900170922     c                   eval      v1sfas='SI'
033000060518     c                   endif
033100060519     c
033101171012     c                   if        DaVar(xx)='E'
033102171012     c                   if        v1csel<>' ' and v1csel=daVar(xx)
033103171012     c                   eval      wrecord='E'
033104171012     c                   endif
033105171012     c                   eval      v1scom='SI'
033106171012     c                   endif
033107171012     c
033900060522     c                   endif
034000060522     c                   enddo
034100060519     c
034200060519     c* Leggo e scrivo analitico
034300060519     c                   if        wrecord='S'
034400060519     c                   Exsr      SCRiana
034500060524     c
034600060524     c* Scrivo un record in analitico per dati variati non registrati
034700060524     c* solo per variazione
034800170921     c                   if        gfpcta = 'M'
034900060524     c                   Exsr      Datinoregis
035000060524     c                   endif
035100060530     c
035200060530     c* visto che almeno una riga l'ho scritta in analitico, aggancio
035300060530     c*  l'ultima per sottolineare i campi
035400170921     c     nrr3          chain     lg34s03                            31
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
036800170921     c                   update    lg34s03
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
038000170921     c                   write     lg34s01
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
040000170921    2c                   if        gfpcta <> 'M'
040100060523     c                   exsr      car_ins
040200060523     c
040300060523   x2c                   else
040400060523     c
040500060522     c                   if        v1csel=' '
040600170921     c     kgfdd         setll     tigfd01l
040700170921     c     kgfdd         reade     tigfd01l
040800060522     c                   else
040900170921     c     kgfdds        setll     tigfd01l
041000170921     c     kgfdds        reade     tigfd01l
041100060522     c                   endif
041200060519     c
041300170921    1c                   dow       not %eof(tigfd01l)
041400060522     c
041500060522     c* Imposto la DS e per il primo record memorizzo in nrr nel sfl
041600060522     c*  sintetico
041700060522    4c                   select
041800170921     c                   when      gfdgcp = 'A'
041801170516     c                   z-add     1             yy                2 0
041802170921     c     gfdgcp        lookup    gcp(yy)                                32
041803170921     c   32              eval      v1sdt1 = gfddeb(yy)
041900170921     c                   if        gfdprg=0
042000060522     c                   exsr      car_A
042100061030     c                   else
042200170921     c                   if        gfdprg=1
042300170509     c                   exsr      car_A1
042600061030     c                   endif
042700170509     c                   endif
042800170921     c                   when      gfdgcp ='B'
042801170516     c                   z-add     1             yy                2 0
042802170922     c     gfdgcp        lookup    gcp(yy)                                32
042803170921     c   32              eval      v1sdt1 = gfddeb(yy)
042900170921     c                   if        gfdprg=0
043000170509     c                   exsr      car_B
043300170509     c                   endif
043400170921     c                   when      gfdgcp ='C'
043401170516     c                   z-add     1             yy                2 0
043402170921     c     gfdgcp        lookup    gcp(yy)                                32
043403170921     c   32              eval      v1sdt1 = gfddeb(yy)
043500170921     c                   if        gfdprg=0
043600170509     c                   exsr      car_C
043900170509     c                   endif
044000170921     c                   when      gfdgcp ='D'
044001170516     c                   z-add     1             yy                2 0
044002170921     c     gfdgcp        lookup    gcp(yy)                                32
044003170921     c   32              eval      v1sdt1 = gfddeb(yy)
044004170921     c                   if        gfdprg=0
044100170509     c                   exsr      car_D
044103170524     c                   endif
044104171012     c                   when      gfdgcp ='E'
044105171012     c                   z-add     1             yy                2 0
044106171012     c     gfdgcp        lookup    gcp(yy)                                32
044107171012     c   32              eval      v1sdt1 = gfddeb(yy)
044108171012     c                   if        gfdprg=0
044109171012     c                   exsr      car_E
044110171012     c                   endif
044400060522    4c                   endsl
044500060522     c
044600060522    2c                   if        v1csel=' '
044700170921     c     kgfdd         reade     tigfd01l
044800060522     c                   else
044900170921     c     kgfdds        reade     tigfd01l
045000060522    2c                   endif
045100060522    1c                   enddo
045200060530     c* finita la scrittura
045300060523     c
045400060523    2c                   endif
045500060519     c
045600060519     c                   EndSr
045700170509      * ?___________________________________________________________________
045800170921      *    SCRIVO RECORD IN SFL lg34S03
045900170509      * ?___________________________________________________________________
046000060523     C     SCRIVIS03     BEGSR
046100060523     c                   add       1             nrr3
046200060523     c                   movel     *in50         in50
046300060530     c                   movel     *in51         in51
046400170921     c                   write     lg34s03
046500060523     c*
046600060523     c* Imposto se il primo nrr3
046700060523     c                   if        v1sreca=0
046800060523     c                   z-add     nrr3          v1sreca
046900060523     c                   endif
047000060524     c                   setoff                                       50
047100060524     c
047200060524     c* Memorizzo il record dati variati scritto in analitico
047300060524     c                   z-add     1             xx
047400170921     c     gfdgcp        lookup    daVar(xx)                              30
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
048700170921     c                   movel     despri(9)     v1sva1
048800060524     c                   z-add     1             xx
048900170509     c                   do        5             xx
049000060524     c                   if        DaVar(XX)<>' ' and DaVarEs(xx)<>'S'
049100060524     c
049200060524     c* Memorizzo se in questo record c'e' il tipo dati selezionato
049300060524     c                   if        (v1csel<>' ' and davar(xx)=v1csel) or
049400060524     c                              v1csel=' '
049500060524     c
049600060524     c                   z-add     1             yy                2 0
049700170921     c     davar(xx)     lookup    gcp(yy)                                30
049800060524     c                   if        *in30
049900170922     c                   movel     gfddeb(yy)    des7a             7
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
052200170921     C                   when      gfpCTA='I'
052300170509     c                   movel     despri(1)     v1sva2
052400170921     C                   when      gfpCTA='A'
052500170509     c                   movel     despri(2)     v1sva2
052600170921     C                   when      gfpCTA='R'
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
054200170921     c                   movel     gfdprima      dgcp_A
054300170509     c                   movel     despri(4)     v1sva1
054500171012     c                   eval      %subst(v1sva1:1:1) = GCPASG
054600171012     c                   eval      %subst(v1sva1:10:14) = %editc(gcpsgs:'2')
054700171012     c                   eval      %subst(v1sva1:31:14) = %editc(gcpsgr:'2')
054800060522     c* Riga dopo
054900170921     c                   movel     gfddopo       dgcp_A
055000170509     c                   move      *blank        v1sva2
055001171012     c                   eval      %subst(v1sva2:1:1) = GCPASG
055002171012     c                   eval      %subst(v1sva2:10:14) = %editc(gcpsgs:'2')
055003171012     c                   eval      %subst(v1sva2:31:14) = %editc(gcpsgr:'2')
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
056500170921     c                   movel     gfdprima      dgcp_A1
056600170509     c                   movel     despri(5)     v1sva1
056601171012     c                   eval      %subst(v1sva1:9:14) = %editc(gcpsgd:'2')
056602170921     c                   eval      %subst(v1sva1:35:14) = %editc(gcpsga:'2')
056800170509     c* Riga dopo
056900170921     c                   movel     gfddopo       dgcp_A1
057000170509     c                   move      *blank        v1sva2
057001170921     c                   eval      %subst(v1sva2:9:14) = %editc(gcpsgd:'2')
057002170921     c                   eval      %subst(v1sva2:35:14) = %editc(gcpsga:'2')
057200170509     c
057300170509     c                   EXSR      ScriviS03
057400170509     c
057500170509     c                   ENDSR
059300170509      * ?___________________________________________________________________
059400170509      *    Scrivo sfl per tipo record "B"
059500170509      * ?___________________________________________________________________
059600170509     C     Car_B         BEGSR
059700170509     c*
059800170509     c* Riga prima
059900170921     c                   movel     gfdprima      dgcp_B
060000170921     c                   movel     despri(6)     v1sva1
060001170921     c                   z-add     gcpcas        gcpcas2          10 3
060002171006     c                   eval      %subst(v1sva1:10:13) = %editc(gcpcas2:'2')
060200170921     c                   eval      %subst(v1sva1:32:1) = GCPVCS
060300171012     c                   eval      %subst(v1sva1:41:1) = GCPaPC
060400171012     c                   eval      %subst(v1sva1:50:1) = GCPpPC
060600170509     c* Riga dopo
060700170922     c                   movel     gfddopo       dgcp_b
060800170509     c                   move      *blank        v1sva2
060801170921     c                   z-add     gcpcas        gcpcas2
060802171006     c                   eval      %subst(v1sva2:10:13) = %editc(gcpcas2:'2')
060803170921     c                   eval      %subst(v1sva2:32:1) = GCPVCS
060804171012     c                   eval      %subst(v1sva2:41:1) = GCPaPC
060805171012     c                   eval      %subst(v1sva2:50:1) = GCPpPC
061400170509     c
061500170509     c                   EXSR      ScriviS03
061600170509     c
061700170509     c                   ENDSR
064300170509      * ?___________________________________________________________________
064400170509      *    Scrivo sfl per tipo record "C"
064500170509      * ?___________________________________________________________________
064600170509     C     Car_C         BEGSR
064700170509     c*
064701171012     C                   movel     '2D'          kcod
064800170509     c* Riga prima
064900170921     c                   movel     gfdprima      dgcp_C
065000171003     c                   movel     despri(7)     v1sva1
065100171012     c                   eval      %subst(v1sva1:1:1) = GCPDIS
065101171012     c                   movel(p)  gcpdis        tblkey
065102171012     c     ktab          chain     tabel00f
065103171012     c                   clear                   decdisp          12
065104171012     c                   if        %found(tabel00f)
065105171012     c                   movel     tbluni        decdisp
065106171012     c                   endif
065107171012     c                   eval      %subst(v1sva1:3:12) = decdisp
065200171012     c                   eval      %subst(v1sva1:33:1) = GCPNPC
065500170922     c                   eval      %subst(v1sva1:50:1) = %editc(gcptft:'X')
065600170509     c* Riga dopo
065700170921     c                   movel     gfddopo       dgcp_c
065800170509     c                   move      *blank        v1sva2
065900171012     c                   eval      %subst(v1sva2:1:1) = GCPDIS
065901171012     c                   movel(p)  gcpdis        tblkey
065902171012     c     ktab          chain     tabel00f
065903171012     c                   clear                   decdisp          12
065904171012     c                   if        %found(tabel00f)
065905171012     c                   movel     tbluni        decdisp
065906171012     c                   endif
065907171012     c                   eval      %subst(v1sva2:3:12) = decdisp
066000171012     c                   eval      %subst(v1sva2:33:1) = GCPNPC
066300170922     c                   eval      %subst(v1sva2:50:1) = %editc(gcptft:'X')
066400170509     c
066500170509     c                   EXSR      ScriviS03
066600170509     c
066700170509     c                   ENDSR
069100170509      * ?___________________________________________________________________
069200170509      *    Scrivo sfl per tipo record "D"
069300170509      * ?___________________________________________________________________
069400170509     C     Car_D         BEGSR
069500170509     c*
069501171012     C                   movel     '2Y'          kcod
069600170509     c* Riga prima
069700170921     c                   movel     gfdprima      dgcp_D
069800170921     c                   movel     despri(8)     v1sva1
069801171012     c                   movel(p)  gcpfas        tblkey
069802170921     c     ktab          chain     tabel00f
069803170921     c                   clear                   decfase
069804170921     c                   if        %found(tabel00f)
069805170921     c                   movel     tbluni        decfase          40
069806170921     c                   endif
069900171012     c                   eval      %subst(v1sva1:1:3) = %editc(gcpfas:'X')
070000171012     c                   eval      %subst(v1sva1:5:40) = decfase
070300170509     c* Riga dopo
070400170921     c                   movel     gfddopo       dgcp_d
070500170509     c                   move      *blank        v1sva2
070501171012     c                   movel(p)  gcpfas        tblkey
070502170921     c     ktab          chain     tabel00f
070503170921     c                   clear                   decfase
070504170921     c                   if        %found(tabel00f)
070505170921     c                   movel     tbluni        decfase
070506170921     c                   endif
070600171012     c                   eval      %subst(v1sva2:1:3) = %editc(gcpfas:'X')
070700171012     c                   eval      %subst(v1sva2:5:40) = decfase
071000170509     c
071100170509     c                   EXSR      ScriviS03
071200170509     c
071300170509     c                   ENDSR
071301171012      * ?___________________________________________________________________
071302171012      *    Scrivo sfl per tipo record "E"
071303171012      * ?___________________________________________________________________
071304171012     C     Car_E         BEGSR
071305171012     c*
071306171012     C                   movel     '2F'          kcod
071307171012     c* Riga prima
071308171012     c                   movel     gfdprima      dgcp_E
071309171012     c                   movel     despri(10)    v1sva1
071320171012      *decodifica tipo comunicazione apertura
071321171012     c                   eval      %subst(v1sva1:10:1) = GCPtcm
071322171012     c                   movel(p)  gcptcm        tblkey
071323171012     c     ktab          chain     tabel00f
071324171012     c                   clear                   dectcm           15
071325171012     c                   if        %found(tabel00f)
071326171012     c                   movel     tbluni        dectcm
071327171012     c                   endif
071328171012     c                   eval      %subst(v1sva1:12:15) = dectcm
071329171012      *decodifica tipo comunicazione chiusura
071330171012     c                   eval      %subst(v1sva1:38:1) = GCPtfg
071331171012     c                   movel(p)  gcptfg        tblkey
071332171012     c     ktab          chain     tabel00f
071333171012     c                   clear                   dectfg           11
071334171012     c                   if        %found(tabel00f)
071335171012     c                   movel     tbluni        dectfg
071336171012     c                   endif
071337171012     c                   eval      %subst(v1sva1:40:11) = dectfg
071338171012     c* Riga dopo
071339171012     c                   movel     gfddopo       dgcp_e
071340171012     c                   move      *blank        v1sva2
071341171012      *decodifica tipo comunicazione apertura
071342171012     c                   eval      %subst(v1sva2:10:1) = GCPtcm
071343171012     c                   movel(p)  gcptcm        tblkey
071344171012     c     ktab          chain     tabel00f
071345171012     c                   clear                   dectcm
071346171012     c                   if        %found(tabel00f)
071347171012     c                   movel     tbluni        dectcm
071348171012     c                   endif
071349171012     c                   eval      %subst(v1sva2:12:15) = dectcm
071350171012      *decodifica tipo comunicazione chiusura
071351171012     c                   eval      %subst(v1sva2:38:1) = GCPtfg
071352171012     c                   movel(p)  gcptfg        tblkey
071353171012     c     ktab          chain     tabel00f
071354171012     c                   clear                   dectfg
071355171012     c                   if        %found(tabel00f)
071356171012     c                   movel     tbluni        dectfg
071357171012     c                   endif
071358171012     c                   eval      %subst(v1sva2:40:11) = dectfg
071360171012     c
071361171012     c                   EXSR      ScriviS03
071362171012     c
071363171012     c                   ENDSR
073100170509      * ?___________________________________________________________________
073200060523      *    Controllo selezione  tipo dati variati
073300170509      * ?___________________________________________________________________
073400060523     C     CtrSel        BEGSR
073500060523     c* Leggo foramto testata
073600170921     c                   read      lg34t01
073700060523     c
073800060523     c                   clear                   v1dsel
073900060523     c                   if        v1csel='?'
074000060523     c                   Clear                   tibs02ds
074100060523     c                   Eval      T02Mod = 'R'
074200060523     c                   Eval      T02Sif = Knsif
074300170921     c                   Eval      T02Cod = 'GCP'
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
075500170921     c                   Eval      T02Cod = 'GCP'
075600060523     c                   movel(P)  v1csel        t02ke1
075700060523     c                   Call      'TIBS02R'
075800060523     c                   Parm                    Kpjba
075900060523     c                   Parm                    tibs02ds
076000060523If  2c                   If        T02Err <> *Blanks
076100060523     c                   eval      v1cmsg=msg(1)
076200060523     c                   seton                                        28
076300060523     c                   else
076400170921     c                   movel     t02uni        dgcp
076500170921     c                   movel     §gcpdes1      v1dsel
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
078000170921     c                   readc     lg34s01                                30
078100060523     c                   dow       not *in30
078200060523     c
078300060523     c                   if        v1ssce='5'
078400060523     c                   z-add     v1sreca       rec3
078500060523     c                   z-add     v1sreca       nrr3
078600170921     c     nrr3          chain     lg34s03                            31
078700060523     c                   seton                                        16
078800060523     c                   movel     in50          *in50
078900060530     c                   movel     in51          *in51
079000170921     c                   update    lg34s03
079100060523     c
079200060523     c                   eval      *inkl=*off
079300060523     c                   exsr      EmissAnal
079400060523     c
079500170921     c     nrr3          chain     lg34s03                            31
079600060523     c                   setoff                                       16
079700060523     c                   movel     in50          *in50
079800060530     c                   movel     in51          *in51
079900170921     c                   update    lg34s03
080000060523     c
080100060523     c                   eval      v1ssce=' '
080200060523     c                   endif
080300060523     c                   movel     in52          *in52
080400170921     c                   update    lg34s01
080500060523     c
080600170921     c                   readc     lg34s01                                30
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
083400170921     c*  Carico descrizioni brevi tabella gcp
083500060522     c                   clear                   xx
083600170921     c                   movel     'GCP'         KcodT
083700060522     c                   clear                   Ksif
083800060522     c                   clear                   Klin
083900060523     c     Ktbe2         setll     TNTBE02l
084000060523     c     Ktbe2         reade     TNTBE02l
084100060522do  2c                   dow       NOT %eof(TNTBE02L)
084200060522if  3c                   if        TBEatb   =  *blanks
084300170921     c                   movel     TBEuni        dgcp
084400060522     c                   add       1             xx
084500170921     c                   movel     TBEke1        gcp(xx)
084600170922     c                   movel     §gcpdes1      gfddeb(xx)
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
085601170921     C     ktab          KLIST
085602170921     C                   KFLD                    kkut
085603171012     C                   KFLD                    kcod              2
085604170921     C                   KFLD                    tblkey
085605170921     C                   z-add     1             kkut              1 0
085700060522     C     ktbe          KLIST
085800060522     C                   KFLD                    kcodt
085900060522     C                   KFLD                    klin
086000060522     C                   KFLD                    ksif
086100060522     C                   KFLD                    kke1
086200170921     C     Kgiac         KLIST
086300170921     C                   KFLD                    Ilg34agc
086400170921     C                   KFLD                    Ilg34fgc
086401170921     C                   KFLD                    Ilg34ngc
086500060519      *
086600170922     C     Kgfdd         KLIST
086601170921     C                   KFLD                    Ilg34agc
086602170921     C                   KFLD                    Ilg34fgc
086603170921     C                   KFLD                    Ilg34ngc
087100170921     C                   KFLD                    gfpnoj
087101170927     C                   KFLD                    gfptime
087200170921     C     Kgfdds        KLIST
087201170921     C                   KFLD                    Ilg34agc
087202170921     C                   KFLD                    Ilg34fgc
087203170921     C                   KFLD                    Ilg34ngc
087208170921     C                   KFLD                    gfpnoj
087209170927     C                   KFLD                    gfptime
087800060522     C                   KFLD                    v1csel
087900030612      *
088300060522     c*
088400940726     C                   ENDSR
088500060518**
088600171002     R   I   P   R   I   S   T   I   N   O                                    1
088700170925     A   P   E   R   T   U   R   A                                            2
088800170925     R   I   A   P   E   R   T   U   R   A                                    3
0889001710121  Sosta 1234567890123 Ricons. 1234567890123                                  4
089000170921Dossier 1234567890123  Aggiuntive 1234567890123                               5
089100170921Imp. C/A 1234567890123 Variaz. 1  Prov. 1  A chi 1                            6
0892001710121 xxxxxxxxxxxx Nuovo Porto cons.1        Fattura 1                            7
089300171012123                                                                           8
089704171012        |     |     |                                                         9
089705171012Apertura 1 xxxxxxxxxxxx     Chiusura 1 xxxxxxxxxxx                           10
089800060523**
089900060530Selezione  Dati Variati ERRATA
