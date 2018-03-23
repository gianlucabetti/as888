000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000711      * FNLV74R ******************************************************
000300000711      *         CONTROLLO CAP ANAGRAFICHE                - FILTRO -  *
000400000711      *         -------------------------                ----------  *
000500000704      ****************************************************************
000600000711     FFNLV74D   CF   E             WORKSTN
000700000711     FAZORG01L  IF   E           K DISK
000800000705     D L1              S              3  0 DIM(30)
000900110105     D CHK             S             50    dim(1) ctdata perrcd(1)
001000000711     D TCUDS           DS
001100000711     D  F1                     1      1
001200000711     D  F3                     3      3
001300000711     D  F2                     2      2
001400000711     D  F4                     4      4
001500000711     D  F56                    5      6
001600000711     D PARAM           DS
001700000711     D  VIDKCU                 1      4  0
001800000711     D  VIDVIO                 5      5
001900000711     D  VIDCLP                 6      6
002000000711     D  VIDFIL                 7      9  0
002100000711     D  VIDSAL                10     10
002200000711     D  VIDTPE                11     11
002300000712     D  VIDDES                12     31
002400010403     D  vidspe                32     32
002500010403     D  vidacr                33     33
002600061116     D  viddri                34     41  0
002700090408     D  vidcep                42     42
002800100224     D  amgdri                43     50  0
002900110105     d* file di work per controllo CAP/LOC obsoleti
003000110105     D  vidFILE               51     60
003100110105     D  vidLIB                61     70
003200100224     d
003300000711     D KPJBA         E DS
003400000704     D CNCR80        E DS
003500000704     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003600000711     D  TCU                  398    697
003700000711     D                                     DIM(50)
003800000711     D  KCU                  698    847P 0
003900000711     D                                     DIM(50)
004000000711     D                                     PACKEVEN
004100000711     D  DTU                  848    895P 0
004200000711     D                                     DIM(12)
004300000711     D                                     PACKEVEN
004400000704     D* DS PER TRUL06R - CARICAMENTO £1
004500000704     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004600000704     D  LIN                    1     90  0
004700000705     D                                     DIM(30)
004800100224     D WLBDAT          DS                  INZ
004900100224     D  G02DAT                 1      8  0
005000100224     D  G02INV                 9     16  0
005100100224     D  G02ERR                17     17
005200100224     D  G02TGI                18     22  0
005300000705      ****************************************************************
005400000705      * RIEPILOGO INDICATORI:                                        *
005500000705      ****************************************************************
005600000704      * 03 - FINE                                                    *
005700000704      * 06 - CONFERMA ELABORAZIONE                                   *
005800000711      * 21 - ERRORE: CAPOCONTO ERRATO                                *
005900000711      * 22 - ERRORE: PUNTO OPERATIVO ERRATO                          *
006000000711      * 23 - ERRORE: TIPO ELABORAZIONE ERRATO                        *
006100000711      * 24 - ERRORE: NESSUNA SCELTA EFFEWTTUATA                      *
006200000711      * 25 - ERRORE: EFFETTUATA PIU' DI UNA SCELTA                   *
006300100224      * 40 - ERRORE: data di rierimento errata                       *
006400000711      * 31 - COMODO                                                  *
006500000704      * 90 - ERRORE GENERICO A VIDEO                                 *
006600000705      ****************************************************************
006700000704      *
006800000705      ****************************************************************
006900000704      *                                                              *
007000000704      *         O P E R A Z I O N I   I N I Z I A L I                *
007100000704      *                                                              *
007200000704      ****************************************************************
007300000704      *
007400000704     C     *ENTRY        PLIST
007500000704     C                   PARM                    KPJBA
007600000705      *
007700000704      * RECUPERO RAGIONE SOCIALE UTENTE E SIMFEL
007800000704      *
007900000704     C                   Z-ADD     1             CODUT
008000000704     C                   CALL      'X§PARUT'
008100000704     C                   PARM                    UT§DSE
008200000704     C                   MOVEL     RAGUT         VIDRSU
008300000704     C                   MOVEL     REC80         CNCR80
008400000704      *
008500000711      * CARICO L1 E CONTI
008600000704      *
008700000704     C                   EXSR      CARTAB
008800000711      *                  ----------------
008900000704      *
009000000711      * INIZIALIZZO CAMPI VIDEO
009100000704      *
009200000711     C                   Z-ADD     KCI           VIDKCU
009300000711     C                   MOVE      ' '           VIDVIO
009400000711     C                   MOVE      ' '           VIDCLP
009500010403     C                   MOVE      ' '           vidspe
009600010403     C                   MOVE      ' '           vidacr
009700000711     C                   MOVE      ' '           VIDSAL
009800090408     C                   MOVE      ' '           VIDcep
009900000720     C                   MOVE      'S'           VIDTPE
010000000711     C                   Z-ADD     0             VIDFIL
010100000711     C                   MOVEL(P)  '0=Tutti'     VIDDES
010200061116     C                   Z-ADD     0             VIDDRI
010300000704      *
010400000705      ****************************************************************
010500000704      *                                                              *
010600000704      *         R O U T I N E   P R I N C I P A L E                  *
010700000704      *                                                              *
010800000704      ****************************************************************
010900000704      *
011000000704     C     EMETTI        TAG
011100061116      *
011200061116      * SE UTENTE NN EDP* => GESTISCO LIMITAZIONI
011300061116      *
011400061116     C                   IF        %subst(KNMUS:1:3) <> 'EDP'
011500061116     C                   SETON                                        28
011600061116     C                   ENDIF
011700061116      *
011800000711     C                   EXFMT     FNLV741
011900000704      *
012000000704      * F3=FINE
012100000704      *
012200000704     C   03              GOTO      FINE
012300000704      *
012400000704      * CONTROLLO PARAMETRI IMMESSI A VIDEO
012500000704      *
012600000704     C                   EXSR      CONTR
012700000711      *                  ---------------
012800000704      *
012900000711      * RIEMETTO SE: 1) ERRORE 2) NO F6
013000000704      *
013100000704     C     *IN06         IFEQ      *OFF
013200000704     C     *IN90         OREQ      *ON
013300000704     C                   GOTO      EMETTI
013400000704     C                   ENDIF
013500000704      *
013600000704      * LANCIO L'ELABORAZIONE
013700000705      * =====================
013800000705      *
013900000713     C                   MOVE      'LV75'        KCOAZ
014000000711     C                   MOVEL     PARAM         KPJBU
014100070116 MOM C***                CALL      'FNLV75C'
014200070116     C                   CALL      'BCH10'
014300000711     C                   PARM                    KPJBA
014400000704      *
014500000704      * FINE PROGRAMMA
014600000704      *
014700000704     C     FINE          TAG
014800000711     C                   SETON                                        LR
014900000704      *
015000000705      ****************************************************************
015100000704      *                                                              *
015200000704      *         S U B R O U T I N E S                                *
015300000704      *                                                              *
015400000704      ****************************************************************
015500000704      *
015600000704      *--------------------------------------------------------------*
015700000704      * CARTAB - SUBROUTINE CARICAMENTO TABELLE                      *
015800000704      *--------------------------------------------------------------*
015900000704      *
016000000704     C     CARTAB        BEGSR
016100000704      *
016200000704      * CARICO TABELLA FILIALI GESTITE £1
016300000704      *
016400000711     C     SIMFEL        IFGT      0
016500000711      *
016600000704     C                   CLEAR                   DSUL06
016700000704     C                   MOVE      '£1'          D06COD
016800000704     C                   MOVEL     SIMFEL        D06KEY
016900000704     C                   MOVEL     DSUL06        KPJBU
017000000704     C*
017100000704     C                   CALL      'TRUL06R'
017200000704     C                   PARM                    KPJBA
017300000704     C                   MOVEL     KPJBU         DSUL06
017400000704     C                   MOVEA     LIN           L1
017500000711      *
017600000711     C                   ENDIF
017700000704      *
017800000711      * RICERCA CONTI
017900000711      *
018000000711     C                   DO        50            X                 2 0
018100000711     C                   MOVE      TCU(X)        TCUDS
018200000711     C     F56           CABNE     'CG'          END1
018300000711     C     F4            COMP      '1'                                    21
018400000711     C     F4            COMP      '2'                                    22
018500000711     C     F4            COMP      '3'                                    23
018600000711     C     F4            COMP      '6'                                    27
018700000711      ** 1 CLIENTI   21
018800000711      ** 2 FORNITORI 22
018900000711      ** 3 AGENTI    23
019000000711     C     F3            COMP      '0'                                242425
019100000711     C     F3            COMP      'I'                                    26
019200000711      ** 0 ITALIA   25
019300000711      ** 1 ESTERO   24
019400000711      * I CAPO CONTO IVA
019500000711     C   21
019600000711     CAN 24              Z-ADD     KCU(X)        KCE               4 0
019700000711     C   21
019800000711     CAN 25              Z-ADD     KCU(X)        KCI               4 0
019900000711     C   22
020000000711     CAN 24              Z-ADD     KCU(X)        KFE               4 0
020100000711     C   22
020200000711     CAN 25              Z-ADD     KCU(X)        KFI               4 0
020300000711     C   23
020400000711     CAN 24              Z-ADD     KCU(X)        KAE               4 0
020500000711     C   23
020600000711     CAN 25              Z-ADD     KCU(X)        KAI               4 0
020700000711     C   26              Z-ADD     KCU(X)        KIVA              4 0
020800000711     C   27              Z-ADD     KCU(X)        KBNA              4 0
020900000711     C     END1          TAG
021000000711     C                   END
021100000711     C                   SETOFF                                       212223
021200000711     C                   SETOFF                                       2425
021300000711     C                   SETOFF                                       2627
021400000711      *
021500000704     C                   ENDSR
021600000704      *
021700000704      *--------------------------------------------------------------*
021800000704      * CONTR  - SUBROUTINE CONTROLLO DATI VIDEO                     *
021900000704      *--------------------------------------------------------------*
022000000704      *
022100000704     C     CONTR         BEGSR
022200000704      *
022300000711     C                   SETOFF                                       90
022400000705      *
022500000711      * DEVE ESEERE EFFETTUATA 1 SCELTA
022600000711      *
022700000711      * .. NESSUNA
022800000711     C     VIDKCU        IFEQ      0
022900000711     C     VIDVIO        ANDEQ     ' '
023000000711     C     VIDCLP        ANDEQ     ' '
023100010403     C     vidspe        ANDEQ     ' '
023200010403     C     vidacr        ANDEQ     ' '
023300090408     C     vidcep        ANDEQ     ' '
023400110105     C     vidfile       ANDEQ     *blanks
023500000711     C                   SETON                                        24  90
023600000711     C                   GOTO      ENDCTR
023700000711     C                   ENDIF
023800000711      * .. PIÙ DI UNA (A)
023900000711     C     VIDKCU        IFNE      0
024000000711     C     VIDVIO        IFEQ      'S'
024100000711     C     VIDCLP        OREQ      'S'
024200010403     C     vidspe        OREQ      'S'
024300010403     C     vidacr        OREQ      'S'
024400090408     C     vidcep        OREQ      'S'
024500110105     C     vidfile       ORne      *blanks
024600000711     C                   SETON                                        25  90
024700000711     C                   GOTO      ENDCTR
024800000711     C                   ENDIF
024900000711     C                   ENDIF
025000000711      * .. PIÙ DI UNA (B)
025100000711     C     VIDVIO        IFEQ      'S'
025200090408     C     VIDCLP        ifeq      'S'
025300010403     C     vidspe        OREQ      'S'
025400010403     C     vidacr        OREQ      'S'
025500090408     C     vidcep        OREQ      'S'
025600110105     C     vidfile       ORne      *blanks
025700000711     C                   SETON                                        25  90
025800000711     C                   GOTO      ENDCTR
025900000711     C                   ENDIF
026000000711     C                   ENDIF
026100000711      * .. PIÙ DI UNA (C)
026200000711     C     VIDCLP        IFEQ      'S'
026300090408     C     vidspe        ifeq      'S'
026400010403     C     vidacr        OREQ      'S'
026500090408     C     vidcep        OREQ      'S'
026600110105     C     vidfile       ORne      *blanks
026700000711     C                   SETON                                        25  90
026800000711     C                   GOTO      ENDCTR
026900000711     C                   ENDIF
027000000711     C                   ENDIF
027100010403      * .. PIÙ DI UNA (D)
027200010403     C     vidspe        IFEQ      'S'
027300090408     C     vidacr        ifeq      'S'
027400090408     C     vidcep        OREQ      'S'
027500110105     C     vidfile       ORne      *blanks
027600010403     C                   SETON                                        25  90
027700010403     C                   GOTO      ENDCTR
027800010403     C                   ENDIF
027900010403     C                   ENDIF
028000010403      * .. PIÙ DI UNA (E)
028100010403     C     vidacr        IFEQ      'S'
028200090408     C     vidcep        ifeq      'S'
028300110105     C     vidfile       ORne      *blanks
028400010403     C                   SETON                                        25  90
028500010403     C                   GOTO      ENDCTR
028600010403     C                   ENDIF
028700010403     C                   ENDIF
028800110105     c
028900110105     C     vidcep        ifeq      'S'
029000110105     C     vidfile       andne     *blanks
029100110105     C                   SETON                                        25  90
029200110105     C                   GOTO      ENDCTR
029300110105     C                   ENDIF
029400000711      *
029500000711      * CAPOCONTO
029600000711      *
029700000711     C     VIDKCU        IFNE      0
029800000711     C     VIDKCU        LOOKUP    KCU                                    31
029900000711     C  N31              SETON                                        21  90
030000000711     C  N31              GOTO      ENDCTR
030100000711     C                   ENDIF
030200000711      *
030300000711      * PUNTO OPERATIVO (SE SIMFEL > 0 DEVE ESSERE IN L1)
030400000711      *
030500000711     C     VIDFIL        IFNE      0
030600000711     C     VIDFIL        CHAIN     AZORG01L                           31
030700000711     C  N31ORGFVA        IFNE      ' '
030800000711     C                   SETON                                        31
030900000711     C                   ENDIF
031000000711     C  N31ORGFAG        IFNE      'F'
031100000711     C     ORGFAG        ANDNE     'A'
031200000711     C                   SETON                                        31
031300000711     C                   ENDIF
031400000711     C     *IN31         IFEQ      *ON
031500000711     C                   CLEAR                   VIDDES
031600000711     C                   SETON                                        22  90
031700000711     C                   GOTO      ENDCTR
031800000711     C                   ELSE
031900000711     C     SIMFEL        IFGT      0
032000000711     C     VIDFIL        LOOKUP    L1                                     31
032100000711     C   31              MOVEL     ORGDES        VIDDES
032200000711     C  N31              CLEAR                   VIDDES
032300000711     C  N31              SETON                                        22  90
032400000711     C  N31              GOTO      ENDCTR
032500000711     C                   ELSE
032600000711     C                   MOVEL     ORGDES        VIDDES
032700000711     C                   ENDIF
032800000711     C                   ENDIF
032900000711     C                   ELSE
033000000711     C                   MOVEL(P)  '0=Tutti'     VIDDES
033100000711     C                   ENDIF
033200000711      *
033300000711      * TIPO ELABORAZIONE
033400000711      *
033500000711     C     VIDTPE        IFEQ      ' '
033600000711     C                   SETON                                        23  90
033700000711     C                   GOTO      ENDCTR
033800000711     C                   ENDIF
033900061109      *
034000061109      * TIPO ELABORAZIONE IN AGGIORNAMENTO CONSENTITO SOLO SE UTENTE EDP*
034100061109      *
034200061109     C                   IF        %subst(KNMUS:1:3) <> 'EDP'
034300061109     C                             AND VIDTPE = 'A'
034400090408     C                   SETON                                        2990
034500061109     C                   GOTO      ENDCTR
034600061109     C                   ENDIF
034700100224     c*
034800100224     c* se immessa controllo data di riferimento
034900100224     c                   clear                   amgdri
035000100224     c                   if        viddri>0
035100100224     C                   Z-ADD     VIDDri        G02DAT
035200100224     C                   MOVEL     *BLANK        G02ERR
035300100224     C                   CALL      'XSRDA8'
035400100224     C                   PARM                    WLBDAT
035500100224     C* ERRATA
035600100224    1C     G02ERR        IFEQ      '1'
035700100224     C                   SETON                                        40  90
035800100224     C                   GOTO      ENDCTR
035900100224     c                   else
036000100224     c                   movel     g02dat        viddri
036100100224     c                   movel     g02inv        amgdri
036200100224     c                   endif
036300100224     c                   endif
036400110105     c*
036500110105     c* Se immessa libreria immettere file
036600110105     c                   if        vidlib<>*blanks and vidfile=*blanks
036700110105     C                   SETON                                        41  90
036800110105     C                   GOTO      ENDCTR
036900110105     c                   endif
037000110105     c                   if        vidlib=*blanks and vidfile<>*blanks
037100110105     C                   SETON                                        42  90
037200110105     C                   GOTO      ENDCTR
037300110105     c                   endif
037400110105     c
037500110105     c                   if        vidfile<>*blanks
037600110105     c                   clear                   comman
037700110105     c                   z-add     80            lenght
037800110105     c                   movel     'CHKOBJ OBJ(' comman
037900110105     c                   eval      %subst(comman:12:10)=vidlib
038000110105     c                   eval      %subst(chk(1):2:10)=vidfile
038100110105     c                   eval      comman=%trim(comman)+chk(1)
038200110105     c                   Call      'QCMDEXC'                            99
038300110105     c                   Parm                    comman           80
038400110105     c                   Parm                    lenght           15 5
038500110105     c
038600110105     c                   if        *in99
038700110105     C                   SETON                                        43  90
038800110105     C                   GOTO      ENDCTR
038900110105     c                   endif
039000110105     c                   endif
039100000711      *
039200000711     C     ENDCTR        ENDSR
039300110105**
039400110105/YYYYYYYYYY) OBJTYPE(*FILE)
