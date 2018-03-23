000100990506     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990506     H*PARMS ACTGRP(QILE)
000300980910     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500951002?     *--------------------------------------------------------------*
000700951002      *     *----------------------------------------------*         *
000800031119      *      prestazioni occasionali autofattura fornitori           *
000900031119      *     *----------------------------------------------*         *
001100951002?     *--------------------------------------------------------------*
001200951002      * KC - FINE LAVORO                                             *
001300951002?     *--------------------------------------------------------------*
001400951002      * 28 - Visualizzazione messaggio di errore                     *
002700951002?     *--------------------------------------------------------------*
003100031119     Ffiapd01l  IF   E           K DISK
003101031119     Fansog01l  IF   E           K DISK
003102031119     Fansif01l  IF   E           K DISK
003200020417     Fanfrn01l  IF   E           K DISK
003300020417     Fanrco01l  IF   E           K DISK
003400020417     Ftntbe01l  IF   E           K DISK
003500020510     Ftntlz01l  IF   E           K DISK
003600031119     Fficn92D   CF   E             WORKSTN
007800980910?     *--------------------------------------------------------------*
007900980910?     *  DS                                                          *
008000980910?     *--------------------------------------------------------------*
008002031119     D* DS "TRUL16R" - RICERCA ALFABETICA autotrasportatore
008004031119     D PARAM           DS                  INZ
008005031119     D  PARTIP                 1      2
008006031119     D  PARRSC                 3     37
008007031119     D  PARCSF                38     40
008008031119     D  PARPDR                41     47  0
008009031119     D  PARFLG                48     48
008100980910     D DSFMT           DS
008200980910     D  $TASTO               369    369
008300980910     D  NRG                  370    370
008400980910     D  NCL                  371    371
008600980910     D* posizione cursore
008700980910     D CURSOR          DS
008800980910     D  RIRI                   1      2B 0 INZ
008900980910     D  R$$                    2      2
009000980910     D  COCO                   3      4B 0 INZ
009100980910     D  C$$                    4      4
009102031119     D xsocds          DS          1000
009103031119     D soc001        E DS                  EXTNAME(xsoc001ds)
009600951002     D KPJBA         E DS
009601031119     D ana007ds      e ds                  inz
009602031119     D tibs02ds      E DS
010500031119     D ficn92ds      E DS
010501031119     D fnlv24ds      E DS
010502031120     D ddatiute      e ds
010503031120     D azuteds       e ds                  extname(AZUTE00F)
010504031120     D tibs34ds      E DS                  inz
010505031119     D dcaf          E DS
012901031119     D* Reperimento nome PGM
012902031119     D                SDS
012903031119     D  nompgm           *PROC
013000990506     D*-------------
013100031119     D com10           s             10    inz('0000000000')
013101031119     Ddataiso          s               d   datfmt(*iso)
013102031119     Ddataeur          s               d   datfmt(*eur)
013103031119     Dkksc             s                   like(v1cfor)
013104031119     Dvpdr             s                   like(v1cpdr)
013105031120     Ddutpous          s              3
013106031120     Dcom03            s              3
013400951002?     *--------------------------------------------------------------*
013500951002?     *  CICLO PRINCIPALE                                            *
013600951002?     *--------------------------------------------------------------*
013700951002      *  Impostazione parametri prima videata
013800951002     C                   EXSR      INZ01
013900951002      *  Loop gestione videata
014000031120     C                   DO        *hival
014100951002     C                   EXSR      GESD01
014200031120     C                   ENDdo
014600951002?     *--------------------------------------------------------------*
014700951002?     *  INZ01: Inizializzazione prima videata                       *
014800951002?     *--------------------------------------------------------------*
014900951002     C     INZ01         BEGSR
015000951002      *
015100031119     C                   SETOFF                                       28
015200031119     C                   movea     com10         *in(40)
015300031119     C                   z-add     0             V1CPDR
015400031119     C                   z-add     0             V2CPDR
015401031119     C                   movel     *blanks       V1dPDR
015402031119     C                   movel     *blanks       V2dPDR
015403031119     C                   movel     *blanks       V1Csoc
015404031119     C                   movel     *blanks       V1dsoc
015405031119     C                   movel     *blanks       V1Cfor
015406031119     C                   movel     *blanks       V2Cfor
015407031119     C                   movel     *blanks       V1dfor
015408031119     C                   movel     *blanks       V2dfor
015409031120     C                   movel     *blanks       V1ctip
015900020509     C                   Z-ADD     0             V1Cdti
016400031119     C                   Z-ADD     0             V1Cdtf
016900031119     C                   MOVEL     *blanks       v1ctab
016901031119     C                   MOVEL     *blanks       v1dtab
017000951002     C*
017100951002     C                   ENDSR
017200951002?     *--------------------------------------------------------------*
017300951002?     *  GESD01: Gestione prima videata                              *
017400951002?     *--------------------------------------------------------------*
017500951002     C     GESD01        BEGSR
017600951002      *
017601031120     c                   if        *in28
017602031120     c                   setoff                                       28
017700031120     C                   write     y350d01
017701031120     c                   seton                                        28
017702031120     c                   end
017703031120     C*
017704031120     C                   EXFMT     y350d01
017800031119     C                   SETOFF                                       28
017900031119     C                   movea     com10         *in(40)
018100951002      *  Fine Lavoro
018200951002     C     *INKC         IFEQ      '1'
018300031120     C                   exsr      fine
018500951002     C                   END
018600020403     C*  effettua ricerche per fornitore
018700980910     c                   if        *inkd = *on
018800980910     c                   exsr      search
018900020503     C                   GOTO      FINVD1
019000980910     c                   end
019100951002      *  Controlli
019200951002     C                   EXSR      CTR01
019300951002     C   28              GOTO      FINVD1
019400031119      *  No errori
019500031119     C                   EXSR      CALL93
019600951002      *
019700951002     C     FINVD1        ENDSR
019800951002?     *--------------------------------------------------------------*
019900951002?     *  CTR01: Controlli prima videata                              *
020000951002?     *--------------------------------------------------------------*
020100951002     C     CTR01         BEGSR
020101031119     c                   do
020102031119      *  Controllo codice autotrasportatore
020103031119     c                   clear                   v1dpdr
020104031119     c                   if        v1cpdr <> 0
020105031120     c* controllo il p.o.
020106031120     c                   if        dutpous <> '046'
020107031120     c                   movel     v1cpdr        com03
020108031120     c                   if        com03 <> dutpous
020109031120     c                   seton                                        40
020110031120     c                   leave
020111031120     c                   end
020112031120     c                   end
020113031119     c                   move      v1cpdr        vpdr
020114031119     c     kapd          chain     fiapd01l                           28
020115031119     c                   if        *in28
020116031119     c                   seton                                        40
020117031119     c                   leave
020118031119     c                   else
020119031119     c                   movel     apdrsc        v1dpdr
020120031119     c                   end
020121031119     c                   end
020122031119     c*
020123031120     c                   if        v2cpdr = 0
020124031120     c                   move      v1cpdr        v2cpdr
020125031120     c                   end
020126031120     c*
020127031119     c                   clear                   v2dpdr
020128031119     c                   if        v2cpdr <> 0
020129031120     c* controllo il p.o.
020130031120     c                   if        dutpous <> '046'
020131031120     c                   movel     v2cpdr        com03
020132031120     c                   if        com03 <> dutpous
020133031120     c                   seton                                        40
020134031120     c                   leave
020135031120     c                   end
020136031120     c                   end
020137031119     c                   move      v2cpdr        vpdr
020138031119     c     kapd          chain     fiapd01l                           28
020139031119     c                   if        *in28
020140031119     c                   seton                                        41
020141031119     c                   leave
020142031119     c                   else
020143031119     c                   movel     apdrsc        v2dpdr
020144031119     c                   end
020145031119     c                   end
026301031119      *  Controllo codice società
026303031120     c                   if        (v1cfor <> ' ' or v2cfor <> ' ') and
026304031120     c                             v1csoc = ' '
026305031119     c                   seton                                        2842
026306031119     c                   leave
026307031120     c                   end
026308031120     c                   if        v1csoc <> ' '
026309031119     c     v1csoc        chain     ansif01l                           28
026310031119     c                   if        *in28
026311031119     c                   seton                                        42
026312031119     c                   leave
026313031119     c                   else
026314031119     c                   movel     sifdesbrev    v1dsoc
026315031119     c                   end
026317031120     c                   end
026318031119      *  Controllo codice fornitore   dal
026400031119     C                   clear                   v1dfor
026500031119     C     V1Cfor        IFne      *blanks
026600031119     C                   move      V1Cfor        kksc
026700980910     c                   movel     *blank        sogdes
026800980910     C     Kfrn          CHAIN     anfrn01l                           30
026900980910     C  N30Krco          CHAIN     anrco01l                           30
027000980910     C  N30rcosogg       CHAIN     ansog01l                           30
027200031119     C                   MOVEL(p)  sogdes        v1dfor
027300951002     C     *IN30         IFEQ      '1'
027400031119     C                   SETON                                        4328
027600031119     C                   leave
027700951002     C                   END
029100020423     c                   end
029200020404      *  Controllo codice fornitore   al
029300031119     C                   clear                   v2dfor
029400031119     C     V2Cfor        IFEQ      *blanks
029500031119     c                   movel     v1cfor        v2cfor
029600020411     C                   END
029700031119     C     V2Cfor        IFne      *blanks
029800031119     C                   move      V2Cfor        kksc
029900020403     c                   movel     *blank        sogdes
030000020403     C     Kfrn          CHAIN     anfrn01l                           30
030100020403     C  N30Krco          CHAIN     anrco01l                           30
030200020403     C  N30rcosogg       CHAIN     ansog01l                           30
030400031119     C                   MOVEL(p)  sogdes        v2dfor
030500020403     C     *IN30         IFEQ      '1'
030600031119     C                   SETON                                        4528
030800031119     C                   leave
030900020403     C                   END
032300020423     c                   end
032400020411     C*
032500031119     c                   if        v1cfor > v2cfor
032700031119     C                   SETON                                        434528
032800031119     C                   leave
032900020411     c                   end
033000020403     c* data da
033100020404     c                   if        v1cdti <> 0
033200031119     c     *eur          test(d)                 v1cdti                 47
033300031119     c                   if        *in47
033400031119     c     *dmy          test(d)                 v1cdti                 47
033500031119     c  n47*dmy          move      v1cdti        dataeur
033501031120     c                   else
033502031120     c                   move      v1cdti        dataeur
033503031120     c                   end
033600031119     c  n47              move      dataeur       v1cdti
033800020404     c                   end
033900020403     c* data al
034000020404     c                   if        v1cdtf <> 0
034100031119     c     *eur          test(d)                 v1cdtf                 48
034200031119     c                   if        *in48
034300031119     c     *dmy          test(d)                 v1cdtf                 48
034400031119     c  n48*dmy          move      v1cdtf        dataeur
034401031120     c                   else
034402031120     c                   move      v1cdtf        dataeur
034403031120     c                   end
034500031119     c  n48              move      dataeur       v1cdtf
035100020404     c                   end
035101031119     c*
035200031119     c                   if        *in47 or *in48
035300980910     c                   seton                                          28
035400031119     c                   leave
035600031119     c                   end
035601031119     c*
035700020404     c                   if        v1cdti <> 0
035800980910     c                   movel     v1cdti        dataeur
035900980910     c                   movel     dataeur       dataiso
036000980910     c                   movel     dataiso       wdatda            8 0
036100020404     c                   else
036200020404     c                   z-add     0             wdatda
036300020404     c                   end
036301031119     c                   if        v1cdtf <> 0
036400980910     c                   movel     v1cdtf        dataeur
036500980910     c                   movel     dataeur       dataiso
036600980910     c                   movel     dataiso       wdataa            8 0
036601031120     c                   else
036602031120     c                   z-add     99991231      wdataa
036603031119     c                   end
036700951002      *  Controllo limiti due date
036800951002     C     WDATDA        IFGT      WDATAA
036900031119     C                   SETON                                        474828
037000031119     C                   leave                                                  Data iniz.>fina.
037200951002     C                   END
037301031119      *  Controllo tabella CAF
037302031119     c                   clear                   v1dtab
037303031119     c                   if        v1ctab <> ' '
037304031119     c                   movel     v1ctab        tbeke1
037305031119     c     ktbe          chain     tntbe01l                           28
037306031120     c                   if        *in28 or tbeatb = 'A'
037307031120     c                   seton                                        4928
037308031119     c                   else
037309031119     c                   movel     tbeuni        dcaf
037310031119     c                   movel     §cafdes       v1dtab
037311031119     c                   end
037312031119     c                   end
048900031119     C                   ENDdo
049000951003      *
049100031119     C                   ENDSR
057900020404     C************************************************************
058000020404     C* RICERCHE
058100020404     C************************************************************
058200020404     C     SEARCH        BEGSR
058400980910     C* determino Riga/Colonna del cursore
058500980910     C                   MOVE      nrg           R$$
058600980910     C                   MOVE      ncl           C$$
058700980910     C                   Z-ADD     RIRI          CURR
058800980910     C                   Z-ADD     COCO          CURC
059200980910     C*
0593009809101    C                   SELECT
059301031119     C* FMTD1  - ricerca autotrasp.
059302031119    >C                   WHEn      h1nmfl = 'V1CPDR' or
059303031119    >C                             h1nmfl = 'V2CPDR'
059304031119     C                   clear                   fnlv24ds
059305031120     c* imposto il p.o.
059306031120     c                   if        dutpous <> '046'
059307031120     C                   MOVEL     dutpou        d24fil
059308031120     c                   end
059309031120     C                   MOVEL     v1ctip        d24tip
059310031119     C                   MOVEL     'R'           d24FLG
059311031119     C                   MOVEL(p)  fnlv24ds      KPJBU
059312031119     C                   CALL      'FNLV24R'
059313031119     C                   PARM                    KPJBA
059314031119     C                   MOVEL     KPJBU         fnlv24ds
059315031119     c                   if        h1nmfl = 'V1CPDR'
059316031119     c                   move      d24pdr        v1cpdr
059317031119     c                   movel     d24rsc        v1dpdr
059318031119     c                   else
059319031119     c                   move      d24pdr        v2cpdr
059320031119     c                   movel     d24rsc        v2dpdr
059321031119     c                   end
059400031119     C* FMTD1  - ricerca società
059500031119    >C     H1NMFL        WHEneq    'V1CSOC'
059501031119     C                   reset                   ana007ds
059502031119     C                   eval      opz007 = '01'
059503031119     C                   eval      kpjbu = ana007ds
059504031119     C                   call      'ANA007R'
059505031119     C                   parm                    kpjba
059506031119     C                   eval      ana007ds = kpjbu
059507031119     C                   if        err007 <> '1' and soc007 <> *blanks
059509031119     C                   eval      v1csoc = soc007
059513031119     C                   endif
060700030422     C* FMTD1  - ricerca fornitore 1 AUTOTRASPORTATORE
060800031119    >C                   WHEn      H1NMFL = 'V1CFOR' AND
060900031119     c                             v1ctip = 'A'
061000980910     C*  deposita la KPJBU
061200031119     C                   movel     v1csoc        parcsf
061300031119     C                   MOVEl     V1Dfor        PARRSC
061400980910     C                   Z-ADD     *ZEROS        PARPDR
061500020403     C                   eval      partip = 'P '
061600980910     C                   MOVEL     *BLANKS       PARFLG
061700020419     C                   MOVEL(p)  PARAM         KPJBU
061800980910     C                   CALL      'TRUL16R'
061900980910     C                   PARM                    KPJBA
062000980910     C                   MOVEL     KPJBU         PARAM
062100980910IF  2C     PARFLG        IFEQ      '3'
062200031119     C                   MOVEL     *BLANKS       V1Dfor
062300031119     C                   SETON                                        4328
062500980910X   2C                   ELSE
062600031119     C                   MOVEl     *zeros        V1Cfor
062700031119     C                   MOVE      PARPDR        V1Cfor
062800031119     C                   MOVEL     PARRSC        V1Dfor
062900980910E   2C                   ENDIF
063000030422     C* FMTD1  - ricerca fornitore 2 AUTOTRASPORTATRE
063100031119    >C                   WHEn      H1NMFL = 'V2CFOR' AND
063200031119     c                             v1ctip = 'A'
063300020403     C*  deposita la KPJBU
063500020404     C                   MOVEl     V2DPDR        PARRSC
063600020403     C                   Z-ADD     *ZEROS        PARPDR
063700020403     C                   eval      partip = 'P '
063800031119     C                   movel     v1csoc        parcsf
063900020403     C                   MOVEL     *BLANKS       PARFLG
064000020419     C                   MOVEL(P)  PARAM         KPJBU
064100020403     C                   CALL      'TRUL16R'
064200020403     C                   PARM                    KPJBA
064300020403     C                   MOVEL     KPJBU         PARAM
064400020403IF  2C     PARFLG        IFEQ      '3'
064500031119     C                   MOVEL     *BLANKS       V2Dfor
064600031119     C                   SETON                                        4428
064800020403X   2C                   ELSE
064900031119     C                   MOVEl     *zeros        V2Cfor
065000031119     C                   MOVE      PARPDR        V2Cfor
065100031119     C                   MOVEL     PARRSC        V2Dfor
065200020403E   2C                   ENDIF
065300031119     C* FMTD1  - ricerca tabella prestazioni
065400031119    >C     H1NMFL        WHEneq    'V1CTAB'
065401031119     C                   clear                   Tibs02DS
065402031119     C                   eval      t02tla = 'L'
065404031119     C                   movel     'R'           t02mod
065405031119     C                   movel     'CAF'         t02cod
065406031119     C                   CALL      'TIBS02R'
065407031119     C                   parm                    KPJBA
065408031119     C                   parm                    TIBS02DS
065409031119    2C                   IF        T02err = *blanks
065410031119     c                   movel     t02ke1        v1ctab
065411031119     c                   movel     t02uni        dcaf
065412031119     c                   movel     §cafdes       v1dtab
065413031119     c                   end
066800020510     C                   ENDsl
067200980910     C* imposto pos. del cursore
067300980910     C                   Z-ADD     CURR          H1RIGA
067400980910     C                   Z-ADD     CURC          H1COLO
067500980910     C                   ENDsr
067600951006?     *--------------------------------------------------------------*
067700031119?     *  CALL93: Controlli prima videata                             *
067800951006?     *--------------------------------------------------------------*
067900031119     C     CALL93        BEGSR
068000951006      *
068100031119     c                   clear                   ficn92ds
068200031119     C                   movel     v1ctip        f92tip
068300031119     C                   movel     v1ctab        f92tab
068500031119     C                   movel     v1csoc        f92soc
069000031119     C                   movel     v1cpdr        f92pd1
069200031119     c                   if        v2cpdr = 0
069300031119     c                   move      *all'9'       f92pd2
069400020423     c                   else
069500031119     C                   move      V2CPDR        f92pd2
069600020423     c                   end
069601031119     c                   if        v1cfor = *blanks
069602031119     c                   movel     *all'0'       f92fo1
069603031119     c                   else
069604031119     C                   movel     v1cfor        f92fo1
069605031119     c                   end
069606031119     c                   if        v2cfor = *blanks
069607031119     c                   movel     *all'9'       f92fo2
069608031119     c                   else
069609031119     C                   movel     V2Cfor        f92fo2
069610031119     c                   end
069900031119     C                   Z-ADD     WDATDA        f92DTI
070000031119     C                   Z-ADD     WDATAA        f92DTF
070500031119     C                   MOVEL(p)  ficn92ds      KPJBU
070600030128     c* AUTOTRASPORTATORI
070700031119     C                   CALL      'FICN93R'
070800951006     C                   PARM                    KPJBA
070900031119     C                   MOVEL     KPJBU         ficn92ds
071400951006      * Se premuto F3 vado a fine lavoro
071500031119     C     f92CMD        IFEQ      '03'
071600031120     C                   exsr      fine
071700951006     C                   ELSE
071800031119     C     f92CMD        IFNE      '12'
071900951006     C                   EXSR      INZ01
072000951006     C                   END
072100960402     C                   END
072200951006      *
072300951006     C                   ENDSR
072400980910     C*----------------------------------------------------*
072500031120     C* fine
072600980910     C*----------------------------------------------------*
072700031120     C     fine          begsr
072800980910     C*
072801031120     c                   seton                                        lr
072802031120     c                   return
072803031120     c*
072804031120     C                   ENDSR
072805031120     C*----------------------------------------------------*
072806031120     C* Reperimento dati società
072807031120     C*----------------------------------------------------*
072808031120     C     REPSOC        BEGSR
072809031120     C*
072900980910     C                   CALLB     'XSOC'
073000980910     C                   PARM                    TIPXSC            6
073100980910     C                   PARM                    SOCXSC            3
073200980910     C                   PARM                    CDSXSC            9 0
073300980910     C                   PARM                    MODXSC            3
073400980910     C                   PARM      *blanks       RTNXSC            1
073500980910     C                   PARM                    XSOCDS
073600980910     C                   PARM                    KPJBA
073700980910     C*
073800980910     C                   ENDSR
073900951006?     *--------------------------------------------------------------*
074000951006?     * *INZSR: Operazioni iniziali                                  *
074100951006?     *--------------------------------------------------------------*
074200951009     C     *INZSR        BEGSR
074300951006      *
074400951006     C     *ENTRY        PLIST
074500951006     C                   PARM                    KPJBA
074501031120     c     *dtaara       define    §azute        azuteds
074502031120     c     *dtaara       define    §datiute      ddatiute
074503031120     C                   in(E)     *dtaara
074504031120     C                   IF        %Error  or  RSUT = *blanks
074505031120     C                   call      'TIBS34R'
074506031120     C                   parm                    Tibs34Ds
074507031120     C                   in        *dtaara
074508031120     c                   ENDIF
074509031120     c                   movel     dutpou        dutpous
075700980910     C*---------- RICERCA DITTA :
075800980910     C                   MOVEL     'SOC001'      TIPXSC
075900031120     C                   MOVEL     '201'         SOCXSC
076000980910     C                   EXSR      REPSOC
076100980910     C     RTNXSC        IFNE      '1'
076200980910     C                   MOVEL     XSOCDS        SOC001
076300980910     C                   MOVEL     xscrgs        RSUT             20
076400020417     c                   else
076500031120     c                   exsr      fine
076600980910     c                   end
076700020415     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
076800020415     C                   movel     'F     '      $kcc              6
076900020415     C                   movel     *blanks       $ksc              8
077000020415     C                   callb     'XCAPCLIFOR'
077100020415     C                   parm                    xscsoc
077200020415     C                   parm                    $kcc
077300020415     C                   parm                    $ksc
077400020415     C                   movel     $kcc          forita            6
083100020423     C     Kfrn          KLIST
083200031119     C                   KFLD                    v1csoc
083300020423     C                   KFLD                    kksc
083400980910     C     Krco          KLIST
083500031119     C                   KFLD                    v1csoc
083600020417     C                   KFLD                    forita
083700980910     C                   KFLD                    KKSC
084500031119     C     Ktbe          KLIST
084600031119     C                   KFLD                    tbecod
084700031119     C                   KFLD                    tbeke1
084701031119     c                   eval      tbecod='CAF'
084702031119     C     Kapd          KLIST
084703031119     C                   KFLD                    v1ctip
084704031119     C                   KFLD                    vpdr
085200951006      *  Definisco variabili
085300980910     C                   Z-ADD     0             CURR              2 0
085400980910     C                   Z-ADD     0             CURC              2 0
085700980910      *
085800980910     c                   endsr
