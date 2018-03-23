000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000711      * FNLV74R ******************************************************
000300000711      *         CONTROLLO CAP ANAGRAFICHE                - FILTRO -  *
000400000711      *         -------------------------                ----------  *
000500000704      ****************************************************************
000600000711     FFNLV74D   CF   E             WORKSTN
000700000711     FAZORG01L  IF   E           K DISK
000800000705     D L1              S              3  0 DIM(30)
000900000711     D TCUDS           DS
001000000711     D  F1                     1      1
001100000711     D  F3                     3      3
001200000711     D  F2                     2      2
001300000711     D  F4                     4      4
001400000711     D  F56                    5      6
001500000711     D PARAM           DS
001600000711     D  VIDKCU                 1      4  0
001700000711     D  VIDVIO                 5      5
001800000711     D  VIDCLP                 6      6
001900000711     D  VIDFIL                 7      9  0
002000000711     D  VIDSAL                10     10
002100000711     D  VIDTPE                11     11
002200000712     D  VIDDES                12     31
002300010403     D  vidspe                32     32
002400010403     D  vidacr                33     33
002500061116     D  viddri                34     41  0
002600090408     D  vidcep                42     42
002700100224     D  amgdri                43     50  0
002800100224     d
002900000711     D KPJBA         E DS
003000000704     D CNCR80        E DS
003100000704     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003200000711     D  TCU                  398    697
003300000711     D                                     DIM(50)
003400000711     D  KCU                  698    847P 0
003500000711     D                                     DIM(50)
003600000711     D                                     PACKEVEN
003700000711     D  DTU                  848    895P 0
003800000711     D                                     DIM(12)
003900000711     D                                     PACKEVEN
004000000704     D* DS PER TRUL06R - CARICAMENTO £1
004100000704     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004200000704     D  LIN                    1     90  0
004300000705     D                                     DIM(30)
004400100224     D WLBDAT          DS                  INZ
004500100224     D  G02DAT                 1      8  0
004600100224     D  G02INV                 9     16  0
004700100224     D  G02ERR                17     17
004800100224     D  G02TGI                18     22  0
004900000705      ****************************************************************
005000000705      * RIEPILOGO INDICATORI:                                        *
005100000705      ****************************************************************
005200000704      * 03 - FINE                                                    *
005300000704      * 06 - CONFERMA ELABORAZIONE                                   *
005400000711      * 21 - ERRORE: CAPOCONTO ERRATO                                *
005500000711      * 22 - ERRORE: PUNTO OPERATIVO ERRATO                          *
005600000711      * 23 - ERRORE: TIPO ELABORAZIONE ERRATO                        *
005700000711      * 24 - ERRORE: NESSUNA SCELTA EFFEWTTUATA                      *
005800000711      * 25 - ERRORE: EFFETTUATA PIU' DI UNA SCELTA                   *
005900100224      * 40 - ERRORE: data di rierimento errata                       *
006000000711      * 31 - COMODO                                                  *
006100000704      * 90 - ERRORE GENERICO A VIDEO                                 *
006200000705      ****************************************************************
006300000704      *
006400000705      ****************************************************************
006500000704      *                                                              *
006600000704      *         O P E R A Z I O N I   I N I Z I A L I                *
006700000704      *                                                              *
006800000704      ****************************************************************
006900000704      *
007000000704     C     *ENTRY        PLIST
007100000704     C                   PARM                    KPJBA
007200000705      *
007300000704      * RECUPERO RAGIONE SOCIALE UTENTE E SIMFEL
007400000704      *
007500000704     C                   Z-ADD     1             CODUT
007600000704     C                   CALL      'X§PARUT'
007700000704     C                   PARM                    UT§DSE
007800000704     C                   MOVEL     RAGUT         VIDRSU
007900000704     C                   MOVEL     REC80         CNCR80
008000000704      *
008100000711      * CARICO L1 E CONTI
008200000704      *
008300000704     C                   EXSR      CARTAB
008400000711      *                  ----------------
008500000704      *
008600000711      * INIZIALIZZO CAMPI VIDEO
008700000704      *
008800000711     C                   Z-ADD     KCI           VIDKCU
008900000711     C                   MOVE      ' '           VIDVIO
009000000711     C                   MOVE      ' '           VIDCLP
009100010403     C                   MOVE      ' '           vidspe
009200010403     C                   MOVE      ' '           vidacr
009300000711     C                   MOVE      ' '           VIDSAL
009400090408     C                   MOVE      ' '           VIDcep
009500000720     C                   MOVE      'S'           VIDTPE
009600000711     C                   Z-ADD     0             VIDFIL
009700000711     C                   MOVEL(P)  '0=Tutti'     VIDDES
009800061116     C                   Z-ADD     0             VIDDRI
009900000704      *
010000000705      ****************************************************************
010100000704      *                                                              *
010200000704      *         R O U T I N E   P R I N C I P A L E                  *
010300000704      *                                                              *
010400000704      ****************************************************************
010500000704      *
010600000704     C     EMETTI        TAG
010700061116      *
010800061116      * SE UTENTE NN EDP* => GESTISCO LIMITAZIONI
010900061116      *
011000061116     C                   IF        %subst(KNMUS:1:3) <> 'EDP'
011100061116     C                   SETON                                        28
011200061116     C                   ENDIF
011300061116      *
011400000711     C                   EXFMT     FNLV741
011500000704      *
011600000704      * F3=FINE
011700000704      *
011800000704     C   03              GOTO      FINE
011900000704      *
012000000704      * CONTROLLO PARAMETRI IMMESSI A VIDEO
012100000704      *
012200000704     C                   EXSR      CONTR
012300000711      *                  ---------------
012400000704      *
012500000711      * RIEMETTO SE: 1) ERRORE 2) NO F6
012600000704      *
012700000704     C     *IN06         IFEQ      *OFF
012800000704     C     *IN90         OREQ      *ON
012900000704     C                   GOTO      EMETTI
013000000704     C                   ENDIF
013100000704      *
013200000704      * LANCIO L'ELABORAZIONE
013300000705      * =====================
013400000705      *
013500000713     C                   MOVE      'LV75'        KCOAZ
013600000711     C                   MOVEL     PARAM         KPJBU
013700070116 MOM C***                CALL      'FNLV75C'
013800070116     C                   CALL      'BCH10'
013900000711     C                   PARM                    KPJBA
014000000704      *
014100000704      * FINE PROGRAMMA
014200000704      *
014300000704     C     FINE          TAG
014400000711     C                   SETON                                        LR
014500000704      *
014600000705      ****************************************************************
014700000704      *                                                              *
014800000704      *         S U B R O U T I N E S                                *
014900000704      *                                                              *
015000000704      ****************************************************************
015100000704      *
015200000704      *--------------------------------------------------------------*
015300000704      * CARTAB - SUBROUTINE CARICAMENTO TABELLE                      *
015400000704      *--------------------------------------------------------------*
015500000704      *
015600000704     C     CARTAB        BEGSR
015700000704      *
015800000704      * CARICO TABELLA FILIALI GESTITE £1
015900000704      *
016000000711     C     SIMFEL        IFGT      0
016100000711      *
016200000704     C                   CLEAR                   DSUL06
016300000704     C                   MOVE      '£1'          D06COD
016400000704     C                   MOVEL     SIMFEL        D06KEY
016500000704     C                   MOVEL     DSUL06        KPJBU
016600000704     C*
016700000704     C                   CALL      'TRUL06R'
016800000704     C                   PARM                    KPJBA
016900000704     C                   MOVEL     KPJBU         DSUL06
017000000704     C                   MOVEA     LIN           L1
017100000711      *
017200000711     C                   ENDIF
017300000704      *
017400000711      * RICERCA CONTI
017500000711      *
017600000711     C                   DO        50            X                 2 0
017700000711     C                   MOVE      TCU(X)        TCUDS
017800000711     C     F56           CABNE     'CG'          END1
017900000711     C     F4            COMP      '1'                                    21
018000000711     C     F4            COMP      '2'                                    22
018100000711     C     F4            COMP      '3'                                    23
018200000711     C     F4            COMP      '6'                                    27
018300000711      ** 1 CLIENTI   21
018400000711      ** 2 FORNITORI 22
018500000711      ** 3 AGENTI    23
018600000711     C     F3            COMP      '0'                                242425
018700000711     C     F3            COMP      'I'                                    26
018800000711      ** 0 ITALIA   25
018900000711      ** 1 ESTERO   24
019000000711      * I CAPO CONTO IVA
019100000711     C   21
019200000711     CAN 24              Z-ADD     KCU(X)        KCE               4 0
019300000711     C   21
019400000711     CAN 25              Z-ADD     KCU(X)        KCI               4 0
019500000711     C   22
019600000711     CAN 24              Z-ADD     KCU(X)        KFE               4 0
019700000711     C   22
019800000711     CAN 25              Z-ADD     KCU(X)        KFI               4 0
019900000711     C   23
020000000711     CAN 24              Z-ADD     KCU(X)        KAE               4 0
020100000711     C   23
020200000711     CAN 25              Z-ADD     KCU(X)        KAI               4 0
020300000711     C   26              Z-ADD     KCU(X)        KIVA              4 0
020400000711     C   27              Z-ADD     KCU(X)        KBNA              4 0
020500000711     C     END1          TAG
020600000711     C                   END
020700000711     C                   SETOFF                                       212223
020800000711     C                   SETOFF                                       2425
020900000711     C                   SETOFF                                       2627
021000000711      *
021100000704     C                   ENDSR
021200000704      *
021300000704      *--------------------------------------------------------------*
021400000704      * CONTR  - SUBROUTINE CONTROLLO DATI VIDEO                     *
021500000704      *--------------------------------------------------------------*
021600000704      *
021700000704     C     CONTR         BEGSR
021800000704      *
021900000711     C                   SETOFF                                       90
022000000705      *
022100000711      * DEVE ESEERE EFFETTUATA 1 SCELTA
022200000711      *
022300000711      * .. NESSUNA
022400000711     C     VIDKCU        IFEQ      0
022500000711     C     VIDVIO        ANDEQ     ' '
022600000711     C     VIDCLP        ANDEQ     ' '
022700010403     C     vidspe        ANDEQ     ' '
022800010403     C     vidacr        ANDEQ     ' '
022900090408     C     vidcep        ANDEQ     ' '
023000000711     C                   SETON                                        24  90
023100000711     C                   GOTO      ENDCTR
023200000711     C                   ENDIF
023300000711      * .. PIÙ DI UNA (A)
023400000711     C     VIDKCU        IFNE      0
023500000711     C     VIDVIO        IFEQ      'S'
023600000711     C     VIDCLP        OREQ      'S'
023700010403     C     vidspe        OREQ      'S'
023800010403     C     vidacr        OREQ      'S'
023900090408     C     vidcep        OREQ      'S'
024000000711     C                   SETON                                        25  90
024100000711     C                   GOTO      ENDCTR
024200000711     C                   ENDIF
024300000711     C                   ENDIF
024400000711      * .. PIÙ DI UNA (B)
024500000711     C     VIDVIO        IFEQ      'S'
024600090408     C     VIDCLP        ifeq      'S'
024700010403     C     vidspe        OREQ      'S'
024800010403     C     vidacr        OREQ      'S'
024900090408     C     vidcep        OREQ      'S'
025000000711     C                   SETON                                        25  90
025100000711     C                   GOTO      ENDCTR
025200000711     C                   ENDIF
025300000711     C                   ENDIF
025400000711      * .. PIÙ DI UNA (C)
025500000711     C     VIDCLP        IFEQ      'S'
025600090408     C     vidspe        ifeq      'S'
025700010403     C     vidacr        OREQ      'S'
025800090408     C     vidcep        OREQ      'S'
025900000711     C                   SETON                                        25  90
026000000711     C                   GOTO      ENDCTR
026100000711     C                   ENDIF
026200000711     C                   ENDIF
026300010403      * .. PIÙ DI UNA (D)
026400010403     C     vidspe        IFEQ      'S'
026500090408     C     vidacr        ifeq      'S'
026600090408     C     vidcep        OREQ      'S'
026700010403     C                   SETON                                        25  90
026800010403     C                   GOTO      ENDCTR
026900010403     C                   ENDIF
027000010403     C                   ENDIF
027100010403      * .. PIÙ DI UNA (E)
027200010403     C     vidacr        IFEQ      'S'
027300090408     C     vidcep        ifeq      'S'
027400010403     C                   SETON                                        25  90
027500010403     C                   GOTO      ENDCTR
027600010403     C                   ENDIF
027700010403     C                   ENDIF
027800000711      *
027900000711      * CAPOCONTO
028000000711      *
028100000711     C     VIDKCU        IFNE      0
028200000711     C     VIDKCU        LOOKUP    KCU                                    31
028300000711     C  N31              SETON                                        21  90
028400000711     C  N31              GOTO      ENDCTR
028500000711     C                   ENDIF
028600000711      *
028700000711      * PUNTO OPERATIVO (SE SIMFEL > 0 DEVE ESSERE IN L1)
028800000711      *
028900000711     C     VIDFIL        IFNE      0
029000000711     C     VIDFIL        CHAIN     AZORG01L                           31
029100000711     C  N31ORGFVA        IFNE      ' '
029200000711     C                   SETON                                        31
029300000711     C                   ENDIF
029400000711     C  N31ORGFAG        IFNE      'F'
029500000711     C     ORGFAG        ANDNE     'A'
029600000711     C                   SETON                                        31
029700000711     C                   ENDIF
029800000711     C     *IN31         IFEQ      *ON
029900000711     C                   CLEAR                   VIDDES
030000000711     C                   SETON                                        22  90
030100000711     C                   GOTO      ENDCTR
030200000711     C                   ELSE
030300000711     C     SIMFEL        IFGT      0
030400000711     C     VIDFIL        LOOKUP    L1                                     31
030500000711     C   31              MOVEL     ORGDES        VIDDES
030600000711     C  N31              CLEAR                   VIDDES
030700000711     C  N31              SETON                                        22  90
030800000711     C  N31              GOTO      ENDCTR
030900000711     C                   ELSE
031000000711     C                   MOVEL     ORGDES        VIDDES
031100000711     C                   ENDIF
031200000711     C                   ENDIF
031300000711     C                   ELSE
031400000711     C                   MOVEL(P)  '0=Tutti'     VIDDES
031500000711     C                   ENDIF
031600000711      *
031700000711      * TIPO ELABORAZIONE
031800000711      *
031900000711     C     VIDTPE        IFEQ      ' '
032000000711     C                   SETON                                        23  90
032100000711     C                   GOTO      ENDCTR
032200000711     C                   ENDIF
032300061109      *
032400061109      * TIPO ELABORAZIONE IN AGGIORNAMENTO CONSENTITO SOLO SE UTENTE EDP*
032500061109      *
032600061109     C                   IF        %subst(KNMUS:1:3) <> 'EDP'
032700061109     C                             AND VIDTPE = 'A'
032800090408     C                   SETON                                        2990
032900061109     C                   GOTO      ENDCTR
033000061109     C                   ENDIF
033100100224     c*
033200100224     c* se immessa controllo data di riferimento
033300100224     c                   clear                   amgdri
033400100224     c                   if        viddri>0
033500100224     C                   Z-ADD     VIDDri        G02DAT
033600100224     C                   MOVEL     *BLANK        G02ERR
033700100224     C                   CALL      'XSRDA8'
033800100224     C                   PARM                    WLBDAT
033900100224     C* ERRATA
034000100224    1C     G02ERR        IFEQ      '1'
034100100224     C                   SETON                                        40  90
034200100224     C                   GOTO      ENDCTR
034300100224     c                   else
034400100224     c                   movel     g02dat        viddri
034500100224     c                   movel     g02inv        amgdri
034600100224     c                   endif
034700100224     c                   endif
034800000711      *
034900000711     C     ENDCTR        ENDSR
