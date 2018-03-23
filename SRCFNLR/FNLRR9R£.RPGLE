000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200041112     H*         - Rientri autotrasportatore anomalie
000300000000     H*--------------------------------------------------------------*
000400050418     Ffnlrr9d   cf   e             workstn
000500041025     Ffiapd01l  if   e           k disk
000600930707     F*
000700041025     D kpjba         e ds
000800950125     D*
000900910423     D WLBDAT          DS
001000950125     D  G02DAT                 1      8  0
001100950125     D  G02INV                 9     16  0
001200950125     D  G02ERR                17     17
001300950125     D  G02TGI                18     22  0
001400041025     D fnlv24ds      e ds
001500050418     d Azuteds       e ds                  Extname(Azute00f)
001600050418     d dDatiute      e ds
001700050418     d Tibs34ds      e ds                  Inz
001800050418     D* PASSAGGIO DATI A PGM DI STAMPA
001900050418     D param           DS
002000050419     d  w01dd1                        8  0 inz
002100050419     d  aut1                          7  0 inz
002200050419     d  aut2                          7  0 inz
002300050419     d  aut3                          7  0 inz
002400050419     d  aut4                          7  0 inz
002500050418     D wpdr            S                   LIKE(apdpdr)
002600000000     I/SPACE 3
002700041025     C*--------------------------------------------------------------
002800900521     C*  RIEPILOGO INDICATORI
002900041025     C*--------------------------------------------------------------
003000931116     C* 30    - DI COMODO
003100010906     C* 40/56 - ERRORI
003200900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
003300900521     C*****************************************************************
003400041025     C     *entry        plist
003500041025     C                   parm                    kpjba
003600050418     c     *dtaara       define    §azute        azuteds
003700050418     c     *dtaara       define    §datiute      ddatiute
003800050418     c                   in(E)     *dtaara
003900050418     c                   If        %error  or RSUT = *blanks
004000050418     c                   CLEAR                   tibs34ds
004100050418     c                   CALL      'TIBS34R'
004200050418     c                   PARM                    tibs34ds
004300050418     c                   in        *dtaara
004400050418     c                   EndIf
004500041025      *
004600041025     C     kapdv         klist
004700041025     C                   kfld                    apdtip
004800050418     C                   kfld                    wpdr
004900041025     C                   movel     'A'           apdtip
005000041025      *
005100030723     c* se  profilo edp sproteggo i campi filiale
005200030723     c                   if        %subst(knmus: 1: 3) <> 'EDP'
005300030723     c                   seton                                        46
005400030723     c                   end
005500931116     C*---------------------------------------------------------------*
005600910419     C*
005700950411     C*
005800041025     C     inizio        tag
005900050418     C                   exsr      pul01
006000930426     C*
006100041025     C     for01         tag
006200050418     C                   exfmt     lrr9d01
006300931116     C*
006400920323     C** CMD3 - FINE LAVORO
006500041025     C   kc              goto      fine
006600931015     C* CONTROLLI FORMATO1
006700041025     C                   exsr      contr1
006800041025     C  nkf
006900041025     COR 90              goto      for01
007000041025     C*
007100041025     C     vi1pd1        ifeq      *blanks
007200041025     C                   movel     *zeros        vi1pd1
007300041025     C                   endif
007400041025     C     vi1pd2        ifeq      *blanks
007500041025     C                   movel     *zeros        vi1pd2
007600041025     C                   endif
007700050418     C     vi1pd3        ifeq      *blanks
007800050418     C                   movel     *zeros        vi1pd3
007900050418     C                   endif
008000050418     C     vi1pd4        ifeq      *blanks
008100050418     C                   movel     *zeros        vi1pd4
008200050418     C                   endif
008300050418     c                   movel     vi1pf1        aut1
008400050418     c                   move      vi1pd1        aut1
008500050418     c                   movel     vi1pf1        aut2
008600050418     c                   move      vi1pd2        aut2
008700050418     c                   movel     vi1pf1        aut3
008800050418     c                   move      vi1pd3        aut3
008900050418     c                   movel     vi1pf1        aut4
009000050418     c                   move      vi1pd4        aut4
009100041025     C                   movel     param         kpjbu
009200931202     C*
009300041026     c                   if        *inkf
009400050420     c                   eval      kcoaz = 'LXR9'
009500050420     C                   call      'BCH10'
009600011115     C                   parm                    Kpjba
009700011115     c                   end
009800900611     C*
009900041025     C                   goto      inizio
010000041025     C     fine          tag
010100041025     C                   seton                                        lr
010200931111     C*
010300041025     C* --------------------------------------------------------------*
010400041025     C     pul01         begsr
010500041025     C* --------------------------------------------------------------*
010600041025     C* PULIZIA CAMPI FORMATO 1
010700950125     C***
010800050418     C                   eval      Vi1PF1 = dutpou
010900050418     C                   clear                   vi1pd1
011000050418     C                   clear                   vi1pd2
011100050418     C                   clear                   vi1pd3
011200050418     C                   clear                   vi1pd4
011300050418     C                   clear                   vi1dd1
011400050418     C                   clear                   vi1dp1
011500050418     C                   clear                   vi1dp2
011600050418     C                   clear                   vi1dp3
011700050418     C                   clear                   vi1dp4
011800950125     C*
011900931111     C                   ENDSR
012000041025     C*----------------------------------------------------------------
012100041025     C     contr1        begsr
012200041025     C*----------------------------------------------------------------
012300041025     C*--- CONTROLLI FORMATO1
012400041104     C                   setoff                                       90
012500041104     C*
012600050418     C* D A T A   ritiro
012700041104     C                   move      vi1dd1        g02dat
012800041104     C                   movel     *blank        g02err
012900041104     C                   call      'XSRDA8'
013000041104     C                   parm                    wlbdat
013100041104    1C     g02err        ifeq      '1'
013200041104     C                   seton                                        43  90
013300041104     C                   goto      endct1
013400041025    1C                   end
013500041104     C                   z-add     g02inv        W01dd1
013600041104     C                   z-add     G02dat        VI1dd1
013700931126     C*
013800041104     C* P A D R O N C I N O
013900041104     C* RICERCA
014000041104     C     '?'           SCAN      VI1PD1                                 90
014100041104     C*
014200041104    1C     *IN90         IFEQ      *ON
014300041104     C                   Z-ADD     VI1PF1        D24FIL
014400041104     C                   EXSR      RICPAD
014500041104     C*
014600041104     C     D24PDR        IFNE      0
014700041104     C                   MOVEL     *ZEROS        VI1PD1
014800041104     C                   MOVE      D24PDR        VI1PD1
014900041104     C                   MOVEL     D24RSC        VI1DP1
015000041104     C                   ENDIF
015100041104     C                   GOTO      ENDCT1
015200041104    1C                   ENDIF
015300050418     C* PADRONCINO  2
015400041104     C* RICERCA
015500041104     C     '?'           SCAN      VI1PD2                                 90
015600041104     C*
015700041104    1C     *IN90         IFEQ      *ON
015800050418     C                   Z-ADD     VI1PF1        D24FIL
015900931126     C                   EXSR      RICPAD
016000931126     C*
016100021203     C     D24PDR        IFNE      0
016200931126     C                   MOVEL     *ZEROS        VI1PD2
016300021203     C                   MOVE      D24PDR        VI1PD2
016400050418     C                   MOVEL     D24RSC        VI1DP2
016500950125     C                   ENDIF
016600931126     C                   GOTO      ENDCT1
016700931126    1C                   ENDIF
016800050418     C* PADRONCINO  3
016900050418     C* RICERCA
017000050418     C     '?'           SCAN      VI1PD3                                 90
017100050418     C*
017200050418    1C     *IN90         IFEQ      *ON
017300050418     C                   Z-ADD     VI1PF1        D24FIL
017400050418     C                   EXSR      RICPAD
017500050418     C*
017600050418     C     D24PDR        IFNE      0
017700050418     C                   MOVEL     *ZEROS        VI1PD3
017800050418     C                   MOVE      D24PDR        VI1PD3
017900050418     C                   MOVEL     D24RSC        VI1DP3
018000050418     C                   ENDIF
018100050418     C                   GOTO      ENDCT1
018200050418    1C                   ENDIF
018300050418     C* PADRONCINO  4
018400050418     C* RICERCA
018500050418     C     '?'           SCAN      VI1PD4                                 90
018600050418     C*
018700050418    1C     *IN90         IFEQ      *ON
018800050418     C                   Z-ADD     VI1PF1        D24FIL
018900050418     C                   EXSR      RICPAD
019000050418     C*
019100050418     C     D24PDR        IFNE      0
019200050418     C                   MOVEL     *ZEROS        VI1PD4
019300050418     C                   MOVE      D24PDR        VI1PD4
019400050418     C                   MOVEL     D24RSC        VI1DP4
019500050418     C                   ENDIF
019600050418     C                   GOTO      ENDCT1
019700050418    1C                   ENDIF
019800931126     C*
019900041104    1C     VI1PD1        IFNE      *BLANKS
020000041104     C     VI1PD1        ANDNE     *ZEROS
020100041104     C                   TESTN                   VI1PD1               30
020200931126     C  N30              SETON                                        4190
020300050418     C   90              goto      endct1
020400050418     c                   movel     vi1pf1        wpdr
020500050418     c                   move      vi1pd1        wpdr
020600050418     C     KAPDV         CHAIN     FIAPD000                           30
020700050418    3C     *IN30         IFEQ      *ON
020800050418     C                   SETON                                        4190
020900050418     C                   GOTO      ENDCT1
021000050418    3C                   ENDIF
021100050418     C                   MOVEL     APDRSC        VI1DP1
021200050418    1C                   ENDIF
021300050418     c*
021400050418    1C     VI1PD2        IFNE      *BLANKS
021500050418     C     VI1PD2        ANDNE     *ZEROS
021600050418     C                   TESTN                   VI1PD2               30
021700050418     C  N30              SETON                                        4290
021800050418     c                   movel     vi1pf1        wpdr
021900050418     c                   move      vi1pd2        wpdr
022000050418     C     KAPDV         CHAIN     FIAPD000                           30
022100050418    3C     *IN30         IFEQ      *ON
022200050418     C                   SETON                                        4290
022300050418     C                   GOTO      ENDCT1
022400050418    3C                   ENDIF
022500050418     C                   MOVEL     APDRSC        VI1DP2
022600050418     C                   ENDIF
022700050418     C*
022800050418    1C     VI1PD3        IFNE      *BLANKS
022900050418     C     VI1PD3        ANDNE     *ZEROS
023000050418     C                   TESTN                   VI1PD3               30
023100050418     C  N30              SETON                                        4490
023200050418     c                   movel     vi1pf1        wpdr
023300050418     c                   move      vi1pd3        wpdr
023400050418     C     KAPDV         CHAIN     FIAPD000                           30
023500050418    3C     *IN30         IFEQ      *ON
023600050418     C                   SETON                                        4490
023700050418     C                   GOTO      ENDCT1
023800050418    3C                   ENDIF
023900050418     C                   MOVEL     APDRSC        VI1Dp3
024000050418     C                   ENDIF
024100050418     C*
024200050418    1C     VI1PD4        IFNE      *BLANKS
024300050418     C     VI1PD4        ANDNE     *ZEROS
024400050418     C                   TESTN                   VI1PD4               30
024500050418     C  N30              SETON                                        4590
024600050418     c                   movel     vi1pf1        wpdr
024700050418     c                   move      vi1pd4        wpdr
024800050418     C     KAPDV         CHAIN     FIAPD000                           30
024900050418    3C     *IN30         IFEQ      *ON
025000050418     C                   SETON                                        4590
025100050418     C                   GOTO      ENDCT1
025200050418    3C                   ENDIF
025300050419     C                   MOVEL     APDRSC        VI1Dp4
025400050418     C                   ENDIF
025500931123     C*
025600041104     C*
025700050418     C* immettere almeno un padroncino
025800050418     c                   if        (vi1pd1 = *zeros or vi1pd1 = *blanks) and
025900050418     c                             (vi1pd2 = *zeros or vi1pd2 = *blanks) and
026000050418     c                             (vi1pd3 = *zeros or vi1pd3 = *blanks) and
026100050418     c                             (vi1pd4 = *zeros or vi1pd4 = *blanks)
026200041104     C                   SETON                                        4090
026300041104     C                   GOTO      ENDCT1
026400041104     C                   ENDIF
026500910320     C*
026600900614     C     ENDCT1        ENDSR
026700931126     C*
026800041025     C* --------------------------------------------------------------*
026900041025     C     ricpad        begsr
027000041025     C* --------------------------------------------------------------*
027100041025     C* RICERCA CODICE PADRONCINO
027200041025     C                   movel     *blanks       d24rsc
027300041025     C                   z-add     0             d24pdr
027400041025     C                   movel     'R'           d24flg
027500041025     C                   movel     'A'           d24tip
027600041025     C                   movel(P)  fnlv24ds      kpjbu
027700041025     C                   call      'FNLV24R'
027800041025     C                   parm                    kpjba
027900041025     C                   movel     kpjbu         fnlv24ds
028000041025     C                   endsr
028100050418     c*
