000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200041112     H*         - Rientri autotrasportatore anomalie
000300000000     H*--------------------------------------------------------------*
000400050418     Ffnlrr9d   cf   e             workstn
000500041025     Ffiapd01l  if   e           k disk
000600930707     F*
000700050505     D L1              S              3  0 DIM(30)                              FIL GESTITE
000800041025     D kpjba         e ds
000900950125     D*
001000910423     D WLBDAT          DS
001100950125     D  G02DAT                 1      8  0
001200950125     D  G02INV                 9     16  0
001300950125     D  G02ERR                17     17
001400950125     D  G02TGI                18     22  0
001500041025     D fnlv24ds      e ds
001600050418     d Azuteds       e ds                  Extname(Azute00f)
001700050418     d dDatiute      e ds
001800050418     d Tibs34ds      e ds                  Inz
001900050505      * ds per trul06r
002000050505     d trul06ds      e ds
002100050505     d  lin                    1     90  0 dim(30)
002200050418     D* PASSAGGIO DATI A PGM DI STAMPA
002300050418     D param           DS
002400050419     d  w01dd1                        8  0 inz
002500050419     d  aut1                          7  0 inz
002600050419     d  aut2                          7  0 inz
002700050419     d  aut3                          7  0 inz
002800050419     d  aut4                          7  0 inz
002900050418     D wpdr            S                   LIKE(apdpdr)
003000000000     I/SPACE 3
003100041025     C*--------------------------------------------------------------
003200900521     C*  RIEPILOGO INDICATORI
003300041025     C*--------------------------------------------------------------
003400931116     C* 30    - DI COMODO
003500010906     C* 40/56 - ERRORI
003600900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
003700900521     C*****************************************************************
003800041025     C     *entry        plist
003900041025     C                   parm                    kpjba
004000050418     c     *dtaara       define    §azute        azuteds
004100050418     c     *dtaara       define    §datiute      ddatiute
004200050418     c                   in(E)     *dtaara
004300050418     c                   If        %error  or RSUT = *blanks
004400050418     c                   CLEAR                   tibs34ds
004500050418     c                   CALL      'TIBS34R'
004600050418     c                   PARM                    tibs34ds
004700050418     c                   in        *dtaara
004800050418     c                   EndIf
004900050505     c*
005000050505     C* CARICO FILIALI GESTITE £1
005100050505     C*
005200050505     c                   clear                   trul06ds
005300050505     c                   eval      d06cod = '£1'
005400050505     c                   movel     simfel        d06key
005500050505     c                   movel     trul06ds      kpjbu
005600050505     c                   call      'TRUL06R'
005700050505     c                   parm                    kpjba
005800050505     c                   movel     kpjbu         trul06ds
005900050505     c                   movel     lin           l1
006000041025      *
006100041025     C     kapdv         klist
006200041025     C                   kfld                    apdtip
006300050418     C                   kfld                    wpdr
006400041025     C                   movel     'A'           apdtip
006500041025      *
006600030723     c* se  profilo edp sproteggo i campi filiale
006700050505     c****               if        %subst(knmus: 1: 3) <> 'EDP'
006800050505     c****               seton                                        46
006900050505     c****               end
007000050505     c                   setoff                                       05
007100050505     c     dutlpo        ifeq      '2'
007200050505     c     dutlpo        oreq      *blanks
007300050505     C                   SETON                                        05
007400050505     C                   END
007500931116     C*---------------------------------------------------------------*
007600910419     C*
007700950411     C*
007800041025     C     inizio        tag
007900050418     C                   exsr      pul01
008000930426     C*
008100041025     C     for01         tag
008200050418     C                   exfmt     lrr9d01
008300931116     C*
008400920323     C** CMD3 - FINE LAVORO
008500041025     C   kc              goto      fine
008600050505     C** CMD18- CAMBIO FILIALE
008700050505     C   KS              SETON                                        46
008800050505     C   KS              GOTO      FOR01
008900050505     c*
009000050505     C   46VI1pf1        IFNE      SIMFEL
009100050505     C     vi1pf1        LOOKUP    L1                                     30
009200050505     C  N30              SETON                                        50
009300050505     C  N30              GOTO      FOR01
009400050505     C                   END
009500931015     C* CONTROLLI FORMATO1
009600041025     C                   exsr      contr1
009700041025     C  nkf
009800041025     COR 90              goto      for01
009900041025     C*
010000041025     C     vi1pd1        ifeq      *blanks
010100041025     C                   movel     *zeros        vi1pd1
010200041025     C                   endif
010300041025     C     vi1pd2        ifeq      *blanks
010400041025     C                   movel     *zeros        vi1pd2
010500041025     C                   endif
010600050418     C     vi1pd3        ifeq      *blanks
010700050418     C                   movel     *zeros        vi1pd3
010800050418     C                   endif
010900050418     C     vi1pd4        ifeq      *blanks
011000050418     C                   movel     *zeros        vi1pd4
011100050418     C                   endif
011200050418     c                   movel     vi1pf1        aut1
011300050418     c                   move      vi1pd1        aut1
011400050418     c                   movel     vi1pf1        aut2
011500050418     c                   move      vi1pd2        aut2
011600050418     c                   movel     vi1pf1        aut3
011700050418     c                   move      vi1pd3        aut3
011800050418     c                   movel     vi1pf1        aut4
011900050418     c                   move      vi1pd4        aut4
012000041025     C                   movel     param         kpjbu
012100931202     C*
012200041026     c                   if        *inkf
012300050420     c                   eval      kcoaz = 'LXR9'
012400050420     C                   call      'BCH10'
012500011115     C                   parm                    Kpjba
012600011115     c                   end
012700900611     C*
012800041025     C                   goto      inizio
012900041025     C     fine          tag
013000041025     C                   seton                                        lr
013100931111     C*
013200041025     C* --------------------------------------------------------------*
013300041025     C     pul01         begsr
013400041025     C* --------------------------------------------------------------*
013500041025     C* PULIZIA CAMPI FORMATO 1
013600950125     C***
013700050418     C                   eval      Vi1PF1 = dutpou
013800050505     c                   setoff                                       46
013900050418     C                   clear                   vi1pd1
014000050418     C                   clear                   vi1pd2
014100050418     C                   clear                   vi1pd3
014200050418     C                   clear                   vi1pd4
014300050418     C                   clear                   vi1dd1
014400050418     C                   clear                   vi1dp1
014500050418     C                   clear                   vi1dp2
014600050418     C                   clear                   vi1dp3
014700050418     C                   clear                   vi1dp4
014800950125     C*
014900931111     C                   ENDSR
015000041025     C*----------------------------------------------------------------
015100041025     C     contr1        begsr
015200041025     C*----------------------------------------------------------------
015300041025     C*--- CONTROLLI FORMATO1
015400041104     C                   setoff                                       90
015500041104     C*
015600050418     C* D A T A   ritiro
015700041104     C                   move      vi1dd1        g02dat
015800041104     C                   movel     *blank        g02err
015900041104     C                   call      'XSRDA8'
016000041104     C                   parm                    wlbdat
016100041104    1C     g02err        ifeq      '1'
016200041104     C                   seton                                        43  90
016300041104     C                   goto      endct1
016400041025    1C                   end
016500041104     C                   z-add     g02inv        W01dd1
016600041104     C                   z-add     G02dat        VI1dd1
016700931126     C*
016800041104     C* P A D R O N C I N O
016900041104     C* RICERCA
017000041104     C     '?'           SCAN      VI1PD1                                 90
017100041104     C*
017200041104    1C     *IN90         IFEQ      *ON
017300041104     C                   Z-ADD     VI1PF1        D24FIL
017400041104     C                   EXSR      RICPAD
017500041104     C*
017600041104     C     D24PDR        IFNE      0
017700041104     C                   MOVEL     *ZEROS        VI1PD1
017800041104     C                   MOVE      D24PDR        VI1PD1
017900041104     C                   MOVEL     D24RSC        VI1DP1
018000041104     C                   ENDIF
018100041104     C                   GOTO      ENDCT1
018200041104    1C                   ENDIF
018300050418     C* PADRONCINO  2
018400041104     C* RICERCA
018500041104     C     '?'           SCAN      VI1PD2                                 90
018600041104     C*
018700041104    1C     *IN90         IFEQ      *ON
018800050418     C                   Z-ADD     VI1PF1        D24FIL
018900931126     C                   EXSR      RICPAD
019000931126     C*
019100021203     C     D24PDR        IFNE      0
019200931126     C                   MOVEL     *ZEROS        VI1PD2
019300021203     C                   MOVE      D24PDR        VI1PD2
019400050418     C                   MOVEL     D24RSC        VI1DP2
019500950125     C                   ENDIF
019600931126     C                   GOTO      ENDCT1
019700931126    1C                   ENDIF
019800050418     C* PADRONCINO  3
019900050418     C* RICERCA
020000050418     C     '?'           SCAN      VI1PD3                                 90
020100050418     C*
020200050418    1C     *IN90         IFEQ      *ON
020300050418     C                   Z-ADD     VI1PF1        D24FIL
020400050418     C                   EXSR      RICPAD
020500050418     C*
020600050418     C     D24PDR        IFNE      0
020700050418     C                   MOVEL     *ZEROS        VI1PD3
020800050418     C                   MOVE      D24PDR        VI1PD3
020900050418     C                   MOVEL     D24RSC        VI1DP3
021000050418     C                   ENDIF
021100050418     C                   GOTO      ENDCT1
021200050418    1C                   ENDIF
021300050418     C* PADRONCINO  4
021400050418     C* RICERCA
021500050418     C     '?'           SCAN      VI1PD4                                 90
021600050418     C*
021700050418    1C     *IN90         IFEQ      *ON
021800050418     C                   Z-ADD     VI1PF1        D24FIL
021900050418     C                   EXSR      RICPAD
022000050418     C*
022100050418     C     D24PDR        IFNE      0
022200050418     C                   MOVEL     *ZEROS        VI1PD4
022300050418     C                   MOVE      D24PDR        VI1PD4
022400050418     C                   MOVEL     D24RSC        VI1DP4
022500050418     C                   ENDIF
022600050418     C                   GOTO      ENDCT1
022700050418    1C                   ENDIF
022800931126     C*
022900041104    1C     VI1PD1        IFNE      *BLANKS
023000041104     C     VI1PD1        ANDNE     *ZEROS
023100041104     C                   TESTN                   VI1PD1               30
023200931126     C  N30              SETON                                        4190
023300050418     C   90              goto      endct1
023400050418     c                   movel     vi1pf1        wpdr
023500050418     c                   move      vi1pd1        wpdr
023600050418     C     KAPDV         CHAIN     FIAPD000                           30
023700050418    3C     *IN30         IFEQ      *ON
023800050418     C                   SETON                                        4190
023900050418     C                   GOTO      ENDCT1
024000050418    3C                   ENDIF
024100050418     C                   MOVEL     APDRSC        VI1DP1
024200050418    1C                   ENDIF
024300050418     c*
024400050418    1C     VI1PD2        IFNE      *BLANKS
024500050418     C     VI1PD2        ANDNE     *ZEROS
024600050418     C                   TESTN                   VI1PD2               30
024700050418     C  N30              SETON                                        4290
024800050418     c                   movel     vi1pf1        wpdr
024900050418     c                   move      vi1pd2        wpdr
025000050418     C     KAPDV         CHAIN     FIAPD000                           30
025100050418    3C     *IN30         IFEQ      *ON
025200050418     C                   SETON                                        4290
025300050418     C                   GOTO      ENDCT1
025400050418    3C                   ENDIF
025500050418     C                   MOVEL     APDRSC        VI1DP2
025600050418     C                   ENDIF
025700050418     C*
025800050418    1C     VI1PD3        IFNE      *BLANKS
025900050418     C     VI1PD3        ANDNE     *ZEROS
026000050418     C                   TESTN                   VI1PD3               30
026100050418     C  N30              SETON                                        4490
026200050418     c                   movel     vi1pf1        wpdr
026300050418     c                   move      vi1pd3        wpdr
026400050418     C     KAPDV         CHAIN     FIAPD000                           30
026500050418    3C     *IN30         IFEQ      *ON
026600050418     C                   SETON                                        4490
026700050418     C                   GOTO      ENDCT1
026800050418    3C                   ENDIF
026900050418     C                   MOVEL     APDRSC        VI1Dp3
027000050418     C                   ENDIF
027100050418     C*
027200050418    1C     VI1PD4        IFNE      *BLANKS
027300050418     C     VI1PD4        ANDNE     *ZEROS
027400050418     C                   TESTN                   VI1PD4               30
027500050418     C  N30              SETON                                        4590
027600050418     c                   movel     vi1pf1        wpdr
027700050418     c                   move      vi1pd4        wpdr
027800050418     C     KAPDV         CHAIN     FIAPD000                           30
027900050418    3C     *IN30         IFEQ      *ON
028000050418     C                   SETON                                        4590
028100050418     C                   GOTO      ENDCT1
028200050418    3C                   ENDIF
028300050419     C                   MOVEL     APDRSC        VI1Dp4
028400050418     C                   ENDIF
028500931123     C*
028600041104     C*
028700050418     C* immettere almeno un padroncino
028800050418     c                   if        (vi1pd1 = *zeros or vi1pd1 = *blanks) and
028900050418     c                             (vi1pd2 = *zeros or vi1pd2 = *blanks) and
029000050418     c                             (vi1pd3 = *zeros or vi1pd3 = *blanks) and
029100050418     c                             (vi1pd4 = *zeros or vi1pd4 = *blanks)
029200041104     C                   SETON                                        4090
029300041104     C                   GOTO      ENDCT1
029400041104     C                   ENDIF
029500910320     C*
029600900614     C     ENDCT1        ENDSR
029700931126     C*
029800041025     C* --------------------------------------------------------------*
029900041025     C     ricpad        begsr
030000041025     C* --------------------------------------------------------------*
030100041025     C* RICERCA CODICE PADRONCINO
030200041025     C                   movel     *blanks       d24rsc
030300041025     C                   z-add     0             d24pdr
030400041025     C                   movel     'R'           d24flg
030500041025     C                   movel     'A'           d24tip
030600041025     C                   movel(P)  fnlv24ds      kpjbu
030700041025     C                   call      'FNLV24R'
030800041025     C                   parm                    kpjba
030900041025     C                   movel     kpjbu         fnlv24ds
031000041025     C                   endsr
031100050418     c*
