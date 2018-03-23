000001130513     H DECEDIT('0,') DATEDIT(*DMY.) Option(*nodebugio)
000002130513
000100000804      *------------------------------------------------------------------------*
000200090617      * GESTIONE TABELLA CLIENTI incasso contrassegni con contratto standard   *
000300980909      *------------------------------------------------------------------------*
001800980909
002100130514     FTNTB99D   CF   E             WORKSTN sfile(TB99dsf:nrr)
002101130531     f                                     sfile(tb99ds2:nrr2)
002200981102     FTNTBE01L  UF A E           K DISK
002201130513     Fansog04l  if   E           K DISK
002202130514     Fansog01l  if   E           K DISK    rename(ansog000:ansog1)
002500981102      *--------------------------------------------
002600130510     D Kcod            S                   LIKE(TBEcod)   inz('PIA')
002700990217     D Kke1            S                   LIKE(TBEke1)
002800990217     D Kke2            S                   LIKE(TBEke2)
002900990217     D Klin            S                   LIKE(TBElin)
003000990217     D Ksif            S                   LIKE(TBEsif)
003001130528     D ivatot          S             16    DIM(999)
003002130531     D grutot          S             15    DIM(999)
003003130528     D iva             S             16    DIM(16) descend
003004130513     D viva            S             16    DIM(16)
003005130528     D vivah           S             16    DIM(16)
003006130514     D diva            S             44    DIM(16)
003600990217     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG VIDEO
003700981102      *
003800980507     D KPJBA         E DS
003802130514     D ana040ds      E DS
003900981102     D UT§DSE0F      E DS
004800980507      *--------------------------------------------
004801130513     itb99r02
004802130513     i              V1CIVA1                     viva(1)
004803130513     i              V1CIVA2                     viva(2)
004804130513     i              V1CIVA3                     viva(3)
004805130513     i              V1CIVA4                     viva(4)
004806130513     i              V1CIVA5                     viva(5)
004807130513     i              V1CIVA6                     viva(6)
004808130513     i              V1CIVA7                     viva(7)
004809130513     i              V1CIVA8                     viva(8)
004810130513     i              V1CIVA9                     viva(9)
004811130513     i              V1CIVA10                    viva(10)
004812130513     i              V1CIVA11                    viva(11)
004813130513     i              V1CIVA12                    viva(12)
004814130513     i              V1CIVA13                    viva(13)
004815130513     i              V1CIVA14                    viva(14)
004816130513     i              V1CIVA15                    viva(15)
004817130513     i              V1CIVA16                    viva(16)
004818130514     i*
004819130514     i              V1DIVA1                     diva(1)
004820130514     i              V1DIVA2                     diva(2)
004821130514     i              V1DIVA3                     diva(3)
004822130514     i              V1dIVA4                     diva(4)
004823130514     i              V1dIVA5                     diva(5)
004824130514     i              V1dIVA6                     diva(6)
004825130514     i              V1dIVA7                     diva(7)
004826130514     i              V1dIVA8                     diva(8)
004827130514     i              V1dIVA9                     diva(9)
004828130514     i              V1dIVA10                    diva(10)
004829130514     i              V1dIVA11                    diva(11)
004830130514     i              V1dIVA12                    diva(12)
004831130514     i              V1dIVA13                    diva(13)
004832130514     i              V1dIVA14                    diva(14)
004833130514     i              V1dIVA15                    diva(15)
004834130514     i              V1dIVA16                    diva(16)
004835130513      *_________________________________________________________________
004900990219      * Carico tutti i record presenti
004903130513     c     inizio        tag
004904130513     c                   do        *hival
004905130514     C                   EXFMT     TB99r01
004906130513     C                   setoff                                       9028
004907130513      * F3=FINE
004908130513     c   KC              goto      fine
004911130513     c                   exsr      gesr01
004913130513     c   90              iter
004915130513     c                   enddo
004916130513      *
004917130513     C     FINE          TAG
004918130513      *
004919130513     C                   EVAL      *INLR = *ON
004920130513      *___________________________________________________________________
005400130513     c     gesr01        begsr
005401130513      *___________________________________________________________________
005402130513      * campo alfabetico per ricerca partita iva
005900980909     C                   clear                   V1CMSG
005901130513     c                   if        *inkg
005903130514     c                   exsr      call040r
005904130514     c                   endif
005905130514     c                   if        *inkj
005906130514     c                   exsr      gesr02
005907130530     c                   clear                   v1civa
005908130528     c                   leavesr
005909130514     c                   endif
005930130513      * campo partita iva pieno
005931130514     c                   if        v1civa <> *blank
005932130513     c                   exsr      cercagruppo
005933130531     c                   else
005934130531      *caricamento tutta la tabella
005935130531     c                   exsr      carica2
005936130531     c                   do        *hival
005937130531     c                   write     TB99z02
005938130531     C                   EXFMT     TB99dc2
005939130531     c   kl              leavesr
005940130531     c                   setoff                                       89
005941130531     c                   do        *hival
005942130531     c                   readc     tb99ds2                                89
005943130531     c                   if        *in89
005944130531     c                   leave
005945130531     c                   endif
005946130531     c                   if        vscsce = '1'
005947130531     c                   movel     vscgruh       v1cgru
005948130531     c                   movel     vscgruh       gruppo
005949130531     c                   exsr      carica
005950130531     c                   exsr      dettaglio
005951130531     c                   move      *blank        vscsce
005952130603     c                   setoff                                       60
005953130531     c                   update    tb99ds2
005954130531     c                   endif
005955130531     c                   enddo
005956130531     C                   clear                   V1CMSG
005957130531     C                   setoff                                       9028
005958130531     c                   enddo
005959130531      *
005960130513     c                   endif
005961130513      *
005962130528     c                   if        gruppo <> *zero
005963130514     c                   movel     gruppo        v1cgru
005964130513     c                   exsr      carica
005965130531     c                   exsr      dettaglio
005966130513     c                   else
005967130528     c                   clear                   v1cgru
005968130513     c                   seton                                        9028
005969130514     c                   eval      v1cmsg = 'Partita Iva non trovata in -
005970130514     c                             nessun gruppo esistente'
005971130528     c                   leavesr
005972130513     c                   endif
005973130531     c                   endsr
005974130531      *____________________________________________________________________
005975130531     c     dettaglio     begsr
005976130531      *_____________________________________________________________
005977130514     c                   do        *hival
005978130514     c                   write     TB99z01
005979130514     C                   EXFMT     TB99dct
005980130528     c   kl              leavesr
005981130514     c                   if        *inkf
005982130514     c                   exsr      gesr02b
005983130530     c                   clear                   v1civa
005984130530     c                   leavesr
005985130514     c                   endif
005986130514     C                   clear                   V1CMSG
005987130514     C                   setoff                                       9028
005988130514     c                   enddo
006800980506      *
007401130513     c                   endsr
007402130513      *___________________________________________________________________
007403130513     c     cercagruppo   begsr
007404130513      *___________________________________________________________________
007405130528     c                   clear                   gruppo           15 0
007406130513     c     kcod          setll     tntbe01l
007407130513     c                   do        *hival
007408130514     c     kcod          reade     tntbe01l
007409130514     c                   if        %eof(tntbe01l)
007410130513     c                   leave
007411130513     c                   endif
007412130513     c                   if        tbeatb <> *blank
007413130513     c                   iter
007414130513     c                   endif
007415130513     c                   movea     tbeuni        iva
007416130513     c                   sorta     iva
007417130513     c     v1civa        lookup    iva                                    20
007418130513     c                   if        *in20
007419130513     c                   move      tbeke1        gruppo
007420130513     c                   leave
007421130513     c                   else
007422130513     c                   iter
007423130513     c                   endif
007424130513     c                   enddo
007425130513     c                   endsr
007426130513      *___________________________________________________________________
007427130513     c     gesr02        begsr
007428130513      *___________________________________________________________________
007429130513     C                   clear                   V1CMSG
007430130528     C                   clear                   Viva
007431130528     C                   clear                   Vivah
007432130528     C                   clear                   diva
007433130528      *imposto nel primo campo la prima partita iva cercata se non trovata
007434130528      * in altri gruppi
007435130528     c                   if        v1civa <> *blank
007436130528     c                   move      v1civa        viva(1)
007437130528     c                   endif
007438130514     c     kcod          setgt     tntbe01l
007439130514     c     kcod          readpe    tntbe01l
007440130528     c                   movel     tbeke1        nuovo            15 0
007441130529     c                   eval      v1cgru =  nuovo + 1
007442130514     c                   do        *hival
007443130514     c                   exfmt     tb99r02
007444130528     c                   setoff                                       2890
007445130514     C                   clear                   V1CMSG
007446130514     c   kg              exsr      call040b
007447130514     c   kl              leavesr
007448130528     c                   exsr      ctrr02
007449130528     c   90              iter
007450130514     c                   if        *inkf
007451130514     c                   exsr      conferma
007452130528     c                   exsr      caricatutto
007453130528     c                   leave
007454130514     c                   endif
007455130514     c                   enddo
007456130513     c                   endsr
007457130514      *___________________________________________________________________
007458130514     c     gesr02b       begsr
007459130514      *___________________________________________________________________
007460130514     C                   move      gruppo        tbeke1
007461130514     c     ktbe          chain     tntbe01l
007462130514     c                   movea     tbeuni        viva
007463130528     c                   movea     tbeuni        vivah
007464130514     c                   exsr      ctrr02
007465130514     c                   do        *hival
007466130514     c                   exfmt     tb99r02
007467130514     C                   clear                   V1CMSG
007468130528     c                   setoff                                       9028
007469130514     c   kg              exsr      call040b
007470130514     c   kl              leavesr
007471130528     c                   exsr      ctrr02
007472130528     c   90              iter
007473130514     c                   if        *inkf
007474130528     c                   exsr      conferma
007475130528     c                   exsr      caricatutto
007476130528     c                   leave
007477130514     c                   endif
007478130514     c                   enddo
007479130514     c                   endsr
007480130514      *___________________________________________________________________
007481130514     c     ctrr02        begsr
007482130514      *___________________________________________________________________
007483130514     c                   do        16            xx
007484130514     c                   if        viva(xx) <> *blank
007485130528      *controlli effettuati solo in inserimento in gruppo già esistente
007486130528     c                   z-add     1             aa                2 0
007487130528     c     viva(xx)      lookup    vivah(aa)                              21
007488130528     c                   if        not *in21
007489130528     c     viva(xx)      lookup    ivatot                                 22
007490130528     c                   if        *in22
007491130528     c                   seton                                        2890
007492130528     c                   eval      v1cmsg = 'Partita iva già in tabella'
007493130528     c                   leavesr
007494130528     c                   endif
007495130528     c                   else
007496130528     c                   if        xx <> aa
007497130528     c                   seton                                        2890
007498130528     c                   eval      v1cmsg = 'Partita iva già presente in -
007499130528     c                             questo gruppo'
007500130528     c                   endif
007501130528     c                   endif
007502130528      *
007503130514     c                   movel     viva(xx)      sogpartiva
007504130514     c     sogpartiva    chain     ansog04l
007505130514     c                   if        %found(ansog04l)
007506130514     c                   movel     sogdes        diva(xx)
007507130514     c                   else
007508130514     c                   move      *all'?'       diva(xx)
007509130531     c                   seton                                        2890
007510130514     c                   eval      v1cmsg = 'Partita iva errata'
007511130514     c                   endif
007512130514     c                   else
007513130514     c                   clear                   diva(xx)
007514130514     c                   endif
007515130514     c                   enddo
007516130514     c
007517130514     c                   endsr
008000990217      *****************************************************************
008100990217      *   carico tutti gli elementi della tabella
008200990217      *****************************************************************
008300990217     c     CARICA        BEGSR
008400990217     c                   clear                   NRR               4 0
008401130513     c                   clear                   xx                2 0
008500990217     c
008600990217     c                   eval      *in35='1'
008700130514     c                   write     TB99dct
008800990217     c                   eval      *in35='0'
008900990217     c
008901130513     c                   movel     gruppo        kke1
009000130514     c     ktbe          chain     tntbe01l
009200130513     c                   if        %found(tntbe01l) and tbeatb=' '
009301130513     c                   movea     tbeuni        iva
010800130513     c                   sorta     iva
010801130513     c                   do        16            xx
010802130513     c                   if        iva(xx) <> *blank
010803130514     c                   movel     iva(xx)       vsciva
010804130513     c                   move      *blank        vsdrag
010805130514     c                   movel     iva(xx)       sogpartiva
010806130514     c     sogpartiva    chain     ansog04l
010807130513     c                   if        %found(ansog04l)
010808130513     c                   movel     sogdes        vsdrag
011100990217     c                   endif
011101130513     c                   add       1             nrr
011102130514     c                   write     TB99dsf
011103130513     c                   else
011104130513     c                   leave
011105130513     c                   endif
011400990217     c                   enddo
011401130514     c                   endif
011700990219      * Se ci sono dei record visualizzo il sfl
011800990217     c                   if        nrr>0
011900990217     c                   eval      nrr=1
012000990217     c                   eval      *in36='1'
012100990217     c                   endif
012200990217     c
012300990217     c                   ENDSR
012301130531      *****************************************************************
012302130531      *   carico tutti gli elementi della tabella
012303130531      *****************************************************************
012304130531     c     CARICA2       BEGSR
012305130531     c                   clear                   NRR2              4 0
012306130531     c                   clear                   xxx               3 0
012307130531     c                   clear                   yyy               3 0
012308130531     c
012309130531     c                   eval      *in37='1'
012310130531     c                   write     TB99dc2
012311130531     c                   eval      *in37='0'
012312130531     c
012317130531     c                   do        *hival        xxx
012318130531     c                   if        ivatot(xxx) <> *blank
012319130531     c                   movel     ivatot(xxx)   vsciva2
012320130531     c                   move      *blank        vsdrag2
012321130531     c                   movel     ivatot(xxx)   sogpartiva
012322130531     c                   if        xxx = 1
012323130531     c                   movel     grutot(xxx)   vscgruh
012324130531     c                   else
012326130531     c                   eval      yyy = xxx - 1
012327130531     c                   if        grutot(xxx) = grutot(yyy)
012328130531     c                   seton                                        60
012329130531     c                   else
012330130531     c                   setoff                                       60
012331130531     c                   movel     grutot(xxx)   vscgruh
012332130531     c                   endif
012333130531     c                   endif
012334130531     c     sogpartiva    chain     ansog04l
012335130531     c                   if        %found(ansog04l)
012336130531     c                   movel     sogdes        vsdrag2
012337130531     c                   endif
012338130531     c                   add       1             nrr2
012339130531     c                   write     TB99ds2
012340130531     c                   else
012341130531     c                   leave
012342130531     c                   endif
012343130531     c                   enddo
012345130531      * Se ci sono dei record visualizzo il sfl
012346130531     c                   if        nrr2>0
012347130531     c                   eval      nrr2=1
012348130531     c                   eval      *in38='1'
012349130531     c                   endif
012350130531     c
012351130531     c                   ENDSR
017800980507      *****************************************************************
017900980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
018000980507      *****************************************************************
018100990217     C     CONFERMA      BEGSR
018300130529     c                   movel(p)  V1Cgru        Kke1
018400990217     c     ktbe          chain     tntbe01l                           31
018500990217     c   31              clear                   tntbe000
018600990219      *
018700130529     c                   movel(p)  v1cgru        tbeke1
018900980507      *
019000981102     C   KQ              movel     'A'           TBEatb
019100990217     C  NKQ              movel     *blanks       TBEatb
019200980909      *
019300981102     c                   clear                   TBEftr
019400981102     c                   clear                   TBEdtr
019500981102      *
019600990217     c                   IF        *IN31 = *ON
019700981102     c                   movel     Ksif          TBEsif
019800981102     c                   movel     Klin          TBElin
019900981102     c                   movel     Kcod          TBEcod
020000130528     c                   movel     'AU'          TBEapl
020100990217     c                   movel     Kke2          TBEke2
020101130528     c                   movea     viva          TBEuni
020300981102     C                   WRITE     TNTBE000
020400981102      *
020500981102     c                   ELSE
020501130528     c                   movea     viva          TBEuni
020600981102     C                   UPDATE    TNTBE000
020700981102     c                   ENDIF
020900980507      *
021000980507     C                   ENDSR
021024130514     c*_____________________________________________
021025130514     c     call040r      begsr
021026130514     c*_____________________________________________
021027130514     C                   clear                   Ana040DS
021028130514     C                   MOVEL     '01'          OPZ040
021029130514     C                   MOVEL     Ana040DS      KPJBU
021030130514     C                   CALL      'ANA040R'
021031130514     C                   PARM                    KPJBA
021032130514     C                   MOVEL     KPJBU         Ana040DS
021033130514     C*
021034130514     C                   clear                   v1civa
021035130514     C     COD040        IFNE      *BLANK
021036130514     c     cod040        chain     ansog01l
021037130514     c                   if        %found(ansog01l)
021038130514     C                   MOVEL     sogpartiva    v1civa
021039130514     C                   endif
021040130514     C                   ENDIF
021043130514     C                   endsr
021044130514     c*_____________________________________________
021045130514     c     call040b      begsr
021046130514     c*_____________________________________________
021047130514     C                   clear                   Ana040DS
021048130514     C                   MOVEL     '01'          OPZ040
021049130514     C                   MOVEL     Ana040DS      KPJBU
021050130514     C                   CALL      'ANA040R'
021051130514     C                   PARM                    KPJBA
021052130514     C                   MOVEL     KPJBU         Ana040DS
021053130514     C*
021054130514     C                   clear                   vuoto            16
021055130514     c                   z-add     1             xx
021056130603     c     vuoto         lookup    viva(xx)                               61
021057130514     C     COD040        IFNE      *BLANK
021058130603     c     *in61         andeq     *on
021059130514     c     cod040        chain     ansog01l
021060130514     c                   if        %found(ansog01l)
021061130514     C                   MOVEL     sogpartiva    viva(xx)
021062130514     C                   MOVEL     sogdes        diva(xx)
021063130514     C                   endif
021064130514     C                   ENDIF
021065130514     C                   endsr
021066130528      *___________________________________________________________________
021067130528     c     caricatutto   begsr
021068130528      *___________________________________________________________________
021069130528     c                   clear                   comodo           16
021070130528     c     kcod          setll     tntbe01l
021071130528     c                   do        *hival
021072130528     c     kcod          reade     tntbe01l
021073130528     c                   if        %eof(tntbe01l)
021074130528     c                   leave
021075130528     c                   endif
021076130528     c                   if        tbeatb <> *blank
021077130528     c                   iter
021078130528     c                   endif
021079130528     c                   movea     tbeuni        iva
021080130528     c                   sorta     iva
021081130528     c                   do        16            zz                2 0
021082130528     c                   if        iva(zz) <> *blank
021083130528     c     iva(zz)       lookup    ivatot                                 20
021084130528     c  n20              z-add     1             zzz               3 0
021085130528     c  n20comodo        lookup    ivatot(zzz)                            20
021087130528     c                   if        *in20
021088130528     c                   move      iva(zz)       ivatot(zzz)
021089130531     c                   move      tbeke1        grutot(zzz)
021090130528     c                   endif
021091130528     c                   else
021092130528     c                   leave
021093130528     c                   endif
021094130528     c                   enddo
021095130528     c                   enddo
021096130528     c                   endsr
021100980506      *****************************************************************
021200980506      *   ROUTINE INIZIALE
021300980506      *****************************************************************
021400980506     C     *INZSR        BEGSR
021500980506      *
021600980506     C     *ENTRY        PLIST
021700980506     C                   PARM                    KPJBA
021800981102      *
021900981102     C                   Z-ADD     1             CODUT
022000981102     C                   CALL      'X§PARUT'
022100981102     C                   PARM                    UT§DSE0F
022200981102     C                   MOVEL     RAGUT         RSUT
025900981102      *
026000981102     C     KTBE          klist
026100981102     C                   KFLD                    Kcod
026200990217     C                   KFLD                    Kke1
027100981102      *
027101130528     c                   exsr      caricatutto
027200980506     C                   ENDSR
027300980909      *****************************************************************
027400980909** MSG  (Lungh. 78)                                                          *
027500990217Codice cliente inesistente                                                     1
