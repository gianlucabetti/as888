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
003400050505     d trul06ds      e ds
003500050505     d  lin                    1     90  0 dim(30)
003600030723     d w01dd1          s                   like(data1)
003700030723     d w01dd2          s                   like(data2)
003800000000     I/SPACE 3
003900041025     C*--------------------------------------------------------------
004000900521     C*  RIEPILOGO INDICATORI
004100041025     C*--------------------------------------------------------------
004200931116     C* 30    - DI COMODO
004300010906     C* 40/56 - ERRORI
004400950125     C* 89    - SONO UN "REMXXX"
004500900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
004600900521     C*****************************************************************
004700041025     C     *entry        plist
004800041025     C                   parm                    kpjba
004900041025     C                   z-add     1             codut
005000900615     C                   CALL      'X§PARUT'
005100041025     C                   parm                    UT§DSE
005200041025     C                   movel     ragut         rsut             20
005300041025     C                   movel     rec80         cncr80
005400041025      *
005500041025     C     kapdv         klist
005600041025     C                   kfld                    apdtip
005700041025     C                   kfld                    vi1p1
005800041025     C                   movel     'A'           apdtip
005900041026      *
006000041025     c     ktbe          klist
006100041025     c                   kfld                    tbecod
006200041025     c                   kfld                    tbeke1
006300041025     c                   kfld                    tbeke2
006400041025     c                   move      'CRA'         tbecod
006500041025     c                   movel     'DFT'         tbeke1
006600041025      *
006700041025     c     ktbe          chain     tntbe01l                           93
006800041025     c                   if        not *in93 and tbeatb = *blank
006900041025     c                   movel     tbeuni        dcra
007000041025     c                   endif
007100050505      * Aggancio la £1 e recupero le linee a loro associate
007200050505     c                   clear                   trul06ds
007300050505     c                   eval      d06cod = '£1'
007400050505     c                   movel     simpou        d06key
007500050505     c                   movel     trul06ds      kpjbu
007600050505     c                   call      'TRUL06R'
007700050505     c                   parm                    kpjba
007800050505     c                   movel     kpjbu         trul06ds
007900030723     c* se  profilo edp sproteggo i campi filiale
008000030723     c                   if        %subst(knmus: 1: 3) <> 'EDP'
008100030723     c                   seton                                        46
008200030723     c                   end
008300931116     C*---------------------------------------------------------------*
008400910419     C*
008500950411     C* OPERAZIONI INIZIALI
008600041025     C                   exsr      pul01
008700950411     C*
008800041025     C     inizio        tag
008900041025     C                   movel     '0000'        vi1pd1
009000041025     C                   movel     '9999'        vi1pd2
009100041025     C                   clear                   vi1dd1
009200041025     C                   clear                   vi1dd2
009300041025     C                   movel     'N'           vi1det
009400930426     C*
009500041025     C     for01         tag
009600041025     C                   exfmt     lrr2d01
009700931116     C*
009800920323     C** CMD3 - FINE LAVORO
009900041025     C   kc              goto      fine
010000931015     C* CONTROLLI FORMATO1
010100041025     C                   exsr      contr1
010200041025     C  nkf
010300041025     COR 90              goto      for01
010400041025     C*
010500041025     C     vi1pd1        ifeq      *blanks
010600041025     C                   movel     *zeros        vi1pd1
010700041025     C                   endif
010800041025     C     vi1pd2        ifeq      *blanks
010900041025     C                   movel     *zeros        vi1pd2
011000041025     C                   endif
011100041026     C                   movel     vi1p1         aut1
011200041025     C                   movel     vi1p2         aut2
011300030723     c                   move      w01dd1        data1
011400030723     c                   move      w01dd2        data2
011500041025     c                   move      vi1det        prosp
011600041025     C                   movel     param         kpjbu
011700931202     C*
011800041026     c                   if        *inkf
011900041025     C                   call      'FNLRR3R'
012000011115     C                   parm                    Kpjba
012100041026     c*                  else
012200041026     c*                  movel     'LRR3'        kcoaz
012300041026     C*                  CALL      'BCH10'
012400041026     C*                  PARM                    kpjba
012500011115     c                   end
012600900611     C*
012700041025     C                   goto      inizio
012800041025     C     fine          tag
012900041025     C                   seton                                        lr
013000931111     C*
013100041025     C* --------------------------------------------------------------*
013200041025     C     pul01         begsr
013300041025     C* --------------------------------------------------------------*
013400041025     C* PULIZIA CAMPI FORMATO 1
013500950125     C***
013600041025     C* Vedo se sono SIMFEL O REMOTO
013700950125     C***
013800041026     C                   eval      Vi1PF1 = simpou
013900041026     C                   eval      Vi1PF2 = simpou
014000041025     C                   seton                                        89
014100041025     C                   time                    w0140            14 0
014200000203     C* UDATE IN GGMMAAAA
014300041025     C                   move      W0140         WDTGIO            8 0
014400041025     C                   move      WDTGIO        annoCO            4 0
014500041025     C     ANNOCO        ADD       1             annoPI            4 0
014600041025     C     ANNOCO        SUB       1             annoME            4 0
014700950125     C*
014800931111     C                   ENDSR
014900041025     C*----------------------------------------------------------------
015000041025     C     contr1        begsr
015100041025     C*----------------------------------------------------------------
015200041025     C*--- CONTROLLI FORMATO1
015300041104     C                   setoff                                       90
015400041104     C*
015500041104     C* D A T A   VIAGGI: DAL
015600041104     C                   move      vi1dd1        g02dat
015700041104     C                   movel     *blank        g02err
015800041104     C                   call      'XSRDA8'
015900041104     C                   parm                    wlbdat
016000041104    1C     g02err        ifeq      '1'
016100041104     C                   seton                                        43  90
016200041104     C                   goto      endct1
016300041025    1C                   end
016400041104     C                   z-add     g02inv        W01dd1
016500041104     C                   z-add     G02dat        VI1dd1
016600041104     C                   z-add     vi1dd1        an1               4 0
016700041104     C* CONTROLLO L'ANNO
016800041104     C                   z-add     g02dat        W0040             4 0
016900041025     C     W0040         ifgt      annopi
017000041104     C     W0040         orlt      annome
017100041104     C                   seton                                        43  90
017200041104     C                   GOTO      ENDCT1
017300041104    1C                   END
017400041104     C* DATA dal < DATA ultimapulizia archivi
017500041104    2C     W01DD1        IFLT      d§cradp
017600041104     C                   SETON                                        55  90
017700041104     C                   GOTO      ENDCT1
017800041104    2C                   ENDIF
017900931126     C*
018000041104     C* comunque imposta il 2 campo data
018100041104     C                   Z-ADD     w01dd1        w01dd2
018200041104    1C     vi1dd2        ifgt      0
018300041104     C                   move      vi1dd2        G02DAT
018400041104     C                   movel     *blank        G02ERR
018500041104     C                   CALL      'XSRDA8'
018600041104     C                   parm                    WLBDAT
018700041104    2C     G02ERR        ifeq      '1'
018800041104     C                   seton                                        44  90
018900041104     C                   goto      endct1
019000041104    2C                   END
019100931126     C                   Z-ADD     G02INV        W01DD2
019200041104     C                   Z-ADD     G02DAT        VI1DD2
019300041104     C                   z-add     vi1dd2        an2               4 0
019400041104     C* CONTROLLO L'ANNO
019500041104     C                   Z-ADD     G02DAT        W0040             4 0
019600041104     c     an1           ifne      an2
019700041104     C                   SETON                                        56  90
019800041104     C                   GOTO      ENDCT1
019900041104     c                   end
020000000203     C     W0040         IFGT      ANNOPI
020100041104     C     W0040         ORLT      ANNOME
020200041104     C                   SETON                                        44  90
020300041104     C                   GOTO      ENDCT1
020400041104    1C                   END
020500041104     C* DATA AL NO < DATA DAL
020600041104    2C     W01DD2        IFLT      W01DD1
020700041104     C                   SETON                                        45  90
020800041104     C                   GOTO      ENDCT1
020900041104    2C                   ENDIF
021000041104     C**
021100041104    1C                   ENDIF
021200041104     C*
021300050505      * verifiche se cambiata la filiale
021400050505     c                   setoff                                       5051
021500050505     c                   if        vi1pf1 <> simpou and *in46
021600050505     c     vi1pf1        lookup    lin                                    50
021700050505     c  n50              seton                                        51
021800050505     c   51              goto      endct1
021900050505     c                   endif
022000050505      *
022100050505     c                   movel     vi1pf1        vi1pf2
022200050505     c
022300041104     C* P A D R O N C I N O
022400041104     C* RICERCA
022500041104     C     '?'           SCAN      VI1PD1                                 90
022600041104     C*
022700041104    1C     *IN90         IFEQ      *ON
022800041104     C                   Z-ADD     VI1PF1        D24FIL
022900041104     C                   EXSR      RICPAD
023000041104     C*
023100041104     C     D24PDR        IFNE      0
023200041104     C                   MOVEL     D24PDR        VI1PF1
023300041104     C                   MOVEL     *ZEROS        VI1PD1
023400041104     C                   MOVE      D24PDR        VI1PD1
023500041104     C                   MOVEL     D24RSC        VI1DP1
023600041104     C                   ENDIF
023700041104     C                   GOTO      ENDCT1
023800041104    1C                   ENDIF
023900041104     C* PADRONCINO AL
024000041104     C* RICERCA
024100041104     C     '?'           SCAN      VI1PD2                                 90
024200041104     C*
024300041104    1C     *IN90         IFEQ      *ON
024400041104     C                   Z-ADD     VI1PF2        D24FIL
024500931126     C                   EXSR      RICPAD
024600931126     C*
024700021203     C     D24PDR        IFNE      0
024800021203     C                   MOVEL     D24PDR        VI1PF2
024900931126     C                   MOVEL     *ZEROS        VI1PD2
025000021203     C                   MOVE      D24PDR        VI1PD2
025100950125     C                   ENDIF
025200931126     C                   GOTO      ENDCT1
025300931126    1C                   ENDIF
025400931126     C*
025500931126     C*
025600931126    1C     VI1PD2        IFNE      *BLANKS
025700931126     C     VI1PD2        ANDNE     *ZEROS
025800931126     C                   TESTN                   VI1PD2               30
025900931126     C  N30              SETON                                        4790
026000931126     C                   ENDIF
026100931116     C*
026200041104    1C     VI1PD1        IFNE      *BLANKS
026300041104     C     VI1PD1        ANDNE     *ZEROS
026400041104     C                   TESTN                   VI1PD1               30
026500931126     C  N30              SETON                                        4190
026600041104     C*
026700041104     C* SE NE HO IMMESSO SOLTANTO UNO CONTROLLO L'ESISTENZ
026800041104    2C     VI1PD2        IFEQ      *BLANKS
026900041104     C     VI1PD2        OREQ      *ZEROS
027000041104     C*
027100041104     C     KAPDV         CHAIN     FIAPD000                           30
027200041104     C*
027300041104    3C     *IN30         IFEQ      *ON
027400041104     C* Tolto il controllo sul padroncino annullato
027500041104     C                   SETON                                        4190
027600041104     C                   GOTO      ENDCT1
027700931126    3C                   ENDIF
027800931129     C*
027900950125     C                   MOVEL     APDRSC        VI1DP1
028000931123     C*
028100931123   X2C                   ELSE
028200931126     C*
028300931123     C* PADRONCINO AL  <  MINORE PADRONCINO DAL
028400041104    3C     VI1P2         IFLT      VI1P1
028500041104     C                   SETON                                        4290
028600041104     C                   GOTO      ENDCT1
028700041104    3C                   ENDIF
028800041104    2C                   ENDIF
028900041104    1C                   ENDIF
029000041104     C*
029100010906     C* PADRONCINO AL E PADRONCINO DAL A ZERO
029200041104     C     VI1PD1        IFEQ      *ZEROS
029300041104     C     VI1PD2        ANDEQ     *ZEROS
029400041104     C                   SETON                                        4090
029500041104     C                   GOTO      ENDCT1
029600041104     C                   ENDIF
029700910320     C*
029800900614     C     ENDCT1        ENDSR
029900931126     C*
030000041025     C* --------------------------------------------------------------*
030100041025     C     ricpad        begsr
030200041025     C* --------------------------------------------------------------*
030300041025     C* RICERCA CODICE PADRONCINO
030400041025     C                   movel     *blanks       d24rsc
030500041025     C                   z-add     0             d24pdr
030600041025     C                   movel     'R'           d24flg
030700041025     C                   movel     'A'           d24tip
030800041025     C                   movel(P)  fnlv24ds      kpjbu
030900041025     C                   call      'FNLV24R'
031000041025     C                   parm                    kpjba
031100041025     C                   movel     kpjbu         fnlv24ds
031200041025     C                   endsr
