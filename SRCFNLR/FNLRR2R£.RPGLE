000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200041025     H*         - quadratura autotrasportatore
000300000000     H*--------------------------------------------------------------*
000400041025     Ffnlrr2d   cf   e             workstn
000500041025     Ffiapd01l  if   e           k disk
000600041025     Ftntbe01l  if   e           k disk
000700930707     F*
000800041025     D kpjba         e ds
000900950125     D*
001000041025     D* PASSAGGIO DATI A PGM DI STAMPA RIEPILOGO
001100041025     D param           DS
001200030723     d  data1                         8  0
001300030723     d  data2                         8  0
001400030723     d  aut1                          7  0
001500030723     d  aut2                          7  0
001600031114     d  prosp                         1
001700910423     D WLBDAT          DS
001800950125     D  G02DAT                 1      8  0
001900950125     D  G02INV                 9     16  0
002000950125     D  G02ERR                17     17
002100950125     D  G02TGI                18     22  0
002200931015     D                 DS
002300041026     D  vi1pf1                 1      3  0
002400041025     D  vi1pd1                 4      7
002500041025     D  vi1p1                  1      7  0
002600931126     D                 DS
002700041025     D  vi1pf2                 1      3  0
002800041025     D  vi1pd2                 4      7
002900041025     D  vi1p2                  1      7  0
003000041025     D Dcra          e ds
003100041025     D fnlv24ds      e ds
003200041025     D cncr80        e ds
003300041025     D ut§dse        e ds                  extname(ut§dse0f)
003400030723     d w01dd1          s                   like(data1)
003500030723     d w01dd2          s                   like(data2)
003600000000     I/SPACE 3
003700041025     C*--------------------------------------------------------------
003800900521     C*  RIEPILOGO INDICATORI
003900041025     C*--------------------------------------------------------------
004000931116     C* 30    - DI COMODO
004100010906     C* 40/56 - ERRORI
004200950125     C* 89    - SONO UN "REMXXX"
004300900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
004400900521     C*****************************************************************
004500041025     C     *entry        plist
004600041025     C                   parm                    kpjba
004700041025     C                   z-add     1             codut
004800900615     C                   CALL      'X§PARUT'
004900041025     C                   parm                    UT§DSE
005000041025     C                   movel     ragut         rsut             20
005100041025     C                   movel     rec80         cncr80
005200041025      *
005300041025     C     kapdv         klist
005400041025     C                   kfld                    apdtip
005500041025     C                   kfld                    vi1p1
005600041025     C                   movel     'A'           apdtip
005700041026      *
005800041025     c     ktbe          klist
005900041025     c                   kfld                    tbecod
006000041025     c                   kfld                    tbeke1
006100041025     c                   kfld                    tbeke2
006200041025     c                   move      'CRA'         tbecod
006300041025     c                   movel     'DFT'         tbeke1
006400041025      *
006500041025     c     ktbe          chain     tntbe01l                           93
006600041025     c                   if        not *in93 and tbeatb = *blank
006700041025     c                   movel     tbeuni        dcra
006800041025     c                   endif
006900030723     c* se  profilo edp sproteggo i campi filiale
007000030723     c                   if        %subst(knmus: 1: 3) <> 'EDP'
007100030723     c                   seton                                        46
007200030723     c                   end
007300931116     C*---------------------------------------------------------------*
007400910419     C*
007500950411     C* OPERAZIONI INIZIALI
007600041025     C                   exsr      pul01
007700950411     C*
007800041025     C     inizio        tag
007900041025     C                   movel     '0000'        vi1pd1
008000041025     C                   movel     '9999'        vi1pd2
008100041025     C                   clear                   vi1dd1
008200041025     C                   clear                   vi1dd2
008300041025     C                   movel     'N'           vi1det
008400930426     C*
008500041025     C     for01         tag
008600041025     C                   exfmt     lrr2d01
008700931116     C*
008800920323     C** CMD3 - FINE LAVORO
008900041025     C   kc              goto      fine
009000931015     C* CONTROLLI FORMATO1
009100041025     C                   exsr      contr1
009200041025     C  nkf
009300041025     COR 90              goto      for01
009400041025     C*
009500041025     C     vi1pd1        ifeq      *blanks
009600041025     C                   movel     *zeros        vi1pd1
009700041025     C                   endif
009800041025     C     vi1pd2        ifeq      *blanks
009900041025     C                   movel     *zeros        vi1pd2
010000041025     C                   endif
010100041026     C                   movel     vi1p1         aut1
010200041025     C                   movel     vi1p2         aut2
010300030723     c                   move      w01dd1        data1
010400030723     c                   move      w01dd2        data2
010500041025     c                   move      vi1det        prosp
010600041025     C                   movel     param         kpjbu
010700931202     C*
010800041026     c                   if        *inkf
010900041025     C                   call      'FNLRR3R'
011000011115     C                   parm                    Kpjba
011100041026     c*                  else
011200041026     c*                  movel     'LRR3'        kcoaz
011300041026     C*                  CALL      'BCH10'
011400041026     C*                  PARM                    kpjba
011500011115     c                   end
011600900611     C*
011700041025     C                   goto      inizio
011800041025     C     fine          tag
011900041025     C                   seton                                        lr
012000931111     C*
012100041025     C* --------------------------------------------------------------*
012200041025     C     pul01         begsr
012300041025     C* --------------------------------------------------------------*
012400041025     C* PULIZIA CAMPI FORMATO 1
012500950125     C***
012600041025     C* Vedo se sono SIMFEL O REMOTO
012700950125     C***
012800041026     C                   eval      Vi1PF1 = simpou
012900041026     C                   eval      Vi1PF2 = simpou
013000041025     C                   seton                                        89
013100041025     C                   time                    w0140            14 0
013200000203     C* UDATE IN GGMMAAAA
013300041025     C                   move      W0140         WDTGIO            8 0
013400041025     C                   move      WDTGIO        annoCO            4 0
013500041025     C     ANNOCO        ADD       1             annoPI            4 0
013600041025     C     ANNOCO        SUB       1             annoME            4 0
013700950125     C*
013800931111     C                   ENDSR
013900041025     C*----------------------------------------------------------------
014000041025     C     contr1        begsr
014100041025     C*----------------------------------------------------------------
014200041025     C*--- CONTROLLI FORMATO1
014300041104     C                   setoff                                       90
014400041104     C*
014500041104     C* D A T A   VIAGGI: DAL
014600041104     C                   move      vi1dd1        g02dat
014700041104     C                   movel     *blank        g02err
014800041104     C                   call      'XSRDA8'
014900041104     C                   parm                    wlbdat
015000041104    1C     g02err        ifeq      '1'
015100041104     C                   seton                                        43  90
015200041104     C                   goto      endct1
015300041025    1C                   end
015400041104     C                   z-add     g02inv        W01dd1
015500041104     C                   z-add     G02dat        VI1dd1
015600041104     C                   z-add     vi1dd1        an1               4 0
015700041104     C* CONTROLLO L'ANNO
015800041104     C                   z-add     g02dat        W0040             4 0
015900041025     C     W0040         ifgt      annopi
016000041104     C     W0040         orlt      annome
016100041104     C                   seton                                        43  90
016200041104     C                   GOTO      ENDCT1
016300041104    1C                   END
016400041104     C* DATA dal < DATA ultimapulizia archivi
016500041104    2C     W01DD1        IFLT      d§cradp
016600041104     C                   SETON                                        55  90
016700041104     C                   GOTO      ENDCT1
016800041104    2C                   ENDIF
016900931126     C*
017000041104     C* comunque imposta il 2 campo data
017100041104     C                   Z-ADD     w01dd1        w01dd2
017200041104    1C     vi1dd2        ifgt      0
017300041104     C                   move      vi1dd2        G02DAT
017400041104     C                   movel     *blank        G02ERR
017500041104     C                   CALL      'XSRDA8'
017600041104     C                   parm                    WLBDAT
017700041104    2C     G02ERR        ifeq      '1'
017800041104     C                   seton                                        44  90
017900041104     C                   goto      endct1
018000041104    2C                   END
018100931126     C                   Z-ADD     G02INV        W01DD2
018200041104     C                   Z-ADD     G02DAT        VI1DD2
018300041104     C                   z-add     vi1dd2        an2               4 0
018400041104     C* CONTROLLO L'ANNO
018500041104     C                   Z-ADD     G02DAT        W0040             4 0
018600041104     c     an1           ifne      an2
018700041104     C                   SETON                                        56  90
018800041104     C                   GOTO      ENDCT1
018900041104     c                   end
019000000203     C     W0040         IFGT      ANNOPI
019100041104     C     W0040         ORLT      ANNOME
019200041104     C                   SETON                                        44  90
019300041104     C                   GOTO      ENDCT1
019400041104    1C                   END
019500041104     C* DATA AL NO < DATA DAL
019600041104    2C     W01DD2        IFLT      W01DD1
019700041104     C                   SETON                                        45  90
019800041104     C                   GOTO      ENDCT1
019900041104    2C                   ENDIF
020000041104     C**
020100041104    1C                   ENDIF
020200041104     C*
020300041104     C* P A D R O N C I N O
020400041104     C* RICERCA
020500041104     C     '?'           SCAN      VI1PD1                                 90
020600041104     C*
020700041104    1C     *IN90         IFEQ      *ON
020800041104     C                   Z-ADD     VI1PF1        D24FIL
020900041104     C                   EXSR      RICPAD
021000041104     C*
021100041104     C     D24PDR        IFNE      0
021200041104     C                   MOVEL     D24PDR        VI1PF1
021300041104     C                   MOVEL     *ZEROS        VI1PD1
021400041104     C                   MOVE      D24PDR        VI1PD1
021500041104     C                   MOVEL     D24RSC        VI1DP1
021600041104     C                   ENDIF
021700041104     C                   GOTO      ENDCT1
021800041104    1C                   ENDIF
021900041104     C* PADRONCINO AL
022000041104     C* RICERCA
022100041104     C     '?'           SCAN      VI1PD2                                 90
022200041104     C*
022300041104    1C     *IN90         IFEQ      *ON
022400041104     C                   Z-ADD     VI1PF2        D24FIL
022500931126     C                   EXSR      RICPAD
022600931126     C*
022700021203     C     D24PDR        IFNE      0
022800021203     C                   MOVEL     D24PDR        VI1PF2
022900931126     C                   MOVEL     *ZEROS        VI1PD2
023000021203     C                   MOVE      D24PDR        VI1PD2
023100950125     C                   ENDIF
023200931126     C                   GOTO      ENDCT1
023300931126    1C                   ENDIF
023400931126     C*
023500931126     C*
023600931126    1C     VI1PD2        IFNE      *BLANKS
023700931126     C     VI1PD2        ANDNE     *ZEROS
023800931126     C                   TESTN                   VI1PD2               30
023900931126     C  N30              SETON                                        4790
024000931126     C                   ENDIF
024100931116     C*
024200041104    1C     VI1PD1        IFNE      *BLANKS
024300041104     C     VI1PD1        ANDNE     *ZEROS
024400041104     C                   TESTN                   VI1PD1               30
024500931126     C  N30              SETON                                        4190
024600041104     C*
024700041104     C* SE NE HO IMMESSO SOLTANTO UNO CONTROLLO L'ESISTENZ
024800041104    2C     VI1PD2        IFEQ      *BLANKS
024900041104     C     VI1PD2        OREQ      *ZEROS
025000041104     C*
025100041104     C     KAPDV         CHAIN     FIAPD000                           30
025200041104     C*
025300041104    3C     *IN30         IFEQ      *ON
025400041104     C* Tolto il controllo sul padroncino annullato
025500041104     C                   SETON                                        4190
025600041104     C                   GOTO      ENDCT1
025700931126    3C                   ENDIF
025800931129     C*
025900950125     C                   MOVEL     APDRSC        VI1DP1
026000931123     C*
026100931123   X2C                   ELSE
026200931126     C*
026300931123     C* PADRONCINO AL  <  MINORE PADRONCINO DAL
026400041104    3C     VI1P2         IFLT      VI1P1
026500041104     C                   SETON                                        4290
026600041104     C                   GOTO      ENDCT1
026700041104    3C                   ENDIF
026800041104    2C                   ENDIF
026900041104    1C                   ENDIF
027000041104     C*
027100010906     C* PADRONCINO AL E PADRONCINO DAL A ZERO
027200041104     C     VI1PD1        IFEQ      *ZEROS
027300041104     C     VI1PD2        ANDEQ     *ZEROS
027400041104     C                   SETON                                        4090
027500041104     C                   GOTO      ENDCT1
027600041104     C                   ENDIF
027700910320     C*
027800900614     C     ENDCT1        ENDSR
027900931126     C*
028000041025     C* --------------------------------------------------------------*
028100041025     C     ricpad        begsr
028200041025     C* --------------------------------------------------------------*
028300041025     C* RICERCA CODICE PADRONCINO
028400041025     C                   movel     *blanks       d24rsc
028500041025     C                   z-add     0             d24pdr
028600041025     C                   movel     'R'           d24flg
028700041025     C                   movel     'A'           d24tip
028800041025     C                   movel(P)  fnlv24ds      kpjbu
028900041025     C                   call      'FNLV24R'
029000041025     C                   parm                    kpjba
029100041025     C                   movel     kpjbu         fnlv24ds
029200041025     C                   endsr
