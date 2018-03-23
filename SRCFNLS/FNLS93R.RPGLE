000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200950113      * FNLS93R *----------------------------------------------------*
000300950113      * INVIO FILE DI TRASMISSIONE  BOLLE PER FIL ARRIVO E TRANSITO
000400020718      * MANCA LA SISTEMAZIONE DEL NOME DEI MEMBRI
000500900515      *--------------------------------------------------------------*
000600950113     FFNFGV01L  IF   E           K DISK
000700000428     FFNFGW01L  IF   E           K DISK
000800971105     FAZORG01L  IF   E           K DISK
000900000621     FAZCAE01L  IF   E           K DISK
001000900517      *
001100021014     D CM10            S             42    DIM(1) CTDATA PERRCD(1)
001200021014     D CM12            S             42    DIM(1) CTDATA PERRCD(1)
001300021014     D CM13            S             42    DIM(1) CTDATA PERRCD(1)
001400021014     D CM14            S             42    DIM(1) CTDATA PERRCD(1)
001500021014     D CM37            S             42    DIM(1) CTDATA PERRCD(1)
001600930930     D*
001700000428     D LP2             S              3  0 DIM(150)
001800020812     D*
001900950113     D* PASSAGGIO DATI DA FNLSA2R - FOGLIO VIAGGIO E FIL ELABORATORE -
002000971106     D PARAM5          DS
002100971106     D  PARDTP                 1      8  0
002200971106     D  PARNFV                 9     13  0
002300971106     D  PARDEF                14     14
002400971106     D* FLAG CHE MO INDICA SE HO TROVATO DEI FILE ALLOCATI
002500971106     D  WLCK                  15     15
002600000621     D  SIMFEL               158    160  0
002700930929      *
002800930929     D* LINEE DI ARRIVO
002900930929     D                 DS
003000930929     D  FGVLNA                 1      3  0
003100930929     D  FGVFFV                 4    243
003200930929     D  FGVFF2               244    450
003300000428     D  FGWFF3               451    690
003400000428     D  FGWFF4               691    900
003500000428     D  VUOTO                901    903    INZ
003600000428     D  FFV                    1    903
003700000428     D                                     DIM(301)
003800930929      *
003900930929     D* FILIALI DI SCARICO
004000930929     D                 DS
004100930929     D  LNA2                   1      3  0
004200930929     D  FGVFLP                 4    243
004300930929     D  FGVFL2               244    450
004400000428     D  FGWFL3               451    690
004500000428     D  FGWFL4               691    900
004600000428     D  VUOTO2               901    903    INZ
004700000428     D  FLP                    1    903
004800000428     D                                     DIM(301)
004900930120     D                 DS
005000930120     D  EMME                   1      1
005100021014     d  Wsimfel                2      4  0
005200021014     D  WTFP                   5      7  0
005300021014     d  varxx                  2      7
005400930203     D  VAR2                   1     10
005500021014     D VAR             DS             9
005600021014     D  VARX                   2      7
005700020812     D*
005800920305     D KPJBA         E DS
005900900601     D DSCMZ         E DS
006000000804     D OG143         E DS
006100020812     D FNLV55DS      E DS
006200020812     D TIBS55DS      E DS
006300020812      *
006400020812      * Variabili
006500020812      *
006600020919     D LUNG            S             15  5 INZ
006700020919     D COMMAN          S             80    INZ
006800020919     D WFVPT           S              1    INZ
006900020919     D W236            S            236    INZ
007000020919     D ALF4            S              4    INZ
007100020919     D W0030           S              3  0 INZ
007200020919     D XB              S              3  0 INZ
007300020919     D XX              S              3  0 INZ
007400020919     D WFIL            S              3  0 INZ
007500020919     D RECBPE          S             10  0 INZ
007600020919     D RECBE8          S             10  0 INZ
007700020919     D RECBT8          S             10  0 INZ
007800020919     D RECBT           S             10  0 INZ
007900020919     D RECFV           S             10  0 INZ
008000020919     D RECFWA          S             10  0 INZ
008100020919     D RECSP           S             10  0 INZ
008200900601      *--------------------------------------------------------------*
008300020812     C     K02FGV01      klist
008400020812     C                   kfld                    PARnfv
008500020812     C                   kfld                    SIMfel
008600020812      *
008700000000     C     *ENTRY        PLIST
008800000000     C                   PARM                    KPJBA
008900971106     C                   MOVEL     KPJBU         PARAM5
009000021014
009100021014     c                   Movel     Simfel        Wsimfel
009200930716     C*
009300930929     C* CHAINO FOGLIO VIAGGIO
009400000621   1AC     PARNFV        IFNE      99999
009500020812     C     K02FGV01      chain     FNFGV000                           31
009600930929    1C     *IN31         IFEQ      *OFF
009700020812     C     K02FGV01      chain     FNFGW000                           31
009800000428     C     *IN31         IFEQ      *ON
009900000428     C     FGWATB        ORNE      ' '
010000000428     C                   CLEAR                   FGWFF3
010100000428     C                   CLEAR                   FGWFF4
010200000428     C                   CLEAR                   FGWFL3
010300000428     C                   CLEAR                   FGWFL4
010400000804     C                   CLEAR                   FGWFGS
010500000428     C                   ENDIF
010600020812     C                   Z-ADD     FGWFGS        W0030
010700000804     C     W0030         IFEQ      0
010800000804     C                   Z-ADD     FGVLNP        W0030
010900000804     C                   ENDIF
011000000804     C     W0030         CHAIN     AZORG01L                           31
011100000804     C  N31              MOVEL     ORGDE3        OG143
011200000804     C   31              CLEAR                   OG143
011300020812     C                   IF        §OGntw = 'PPT'
011400020812     C                   MOVEL     'S'           WFVPT
011500000804     C                   ELSE
011600000804     C                   MOVEL     'N'           WFVPT
011700000804     C                   ENDIF
011800971105     C**
011900971105     C* IMPOSTO ALCUNI VALORI FISSI X CREARE FILE TRASMISSIONE
012000971105     C                   EXSR      IMPO
012100930929     C                   Z-ADD     FGVLNA        LNA2
012200930929     C*
012300020812     C                   Z-ADD     1             XB
012400020812     C                   Z-ADD     0             XX
012500930929     C*
012600020812    2C     FLP(XB)       DOWGT     *ZEROS
012700020812     C                   MOVEL     FLP(XB)       WFIL
012800930929     C     WFIL          LOOKUP    LP2                                    31
012900930929     C*
013000930929    3C     *IN31         IFEQ      *OFF
013100020812     C                   ADD       1             XX
013200020812     C                   Z-ADD     WFIL          LP2(XX)
013300930929     C*
013400020812     C                   CLEAR                   FNLV55DS
013500971105     C                   MOVEL     'P'           D55TPT
013600971105     C                   MOVEL     WFIL          D55LIN
013700971105     C                   MOVEL     FGVDFV        D55DRF
013800971105     C                   CALL      'FNLV55R'
013900020812     C                   PARM                    FNLV55DS
014000971105    4C     D55ERR        IFNE      ' '
014100971105     C                   MOVEL     WFIL          D55TFP
014200971105    4C                   ENDIF
014300971105     C**
014400971105     C* ESEGUO CALL PER REPERIRE LIBRERIE E S.I. DI TRASMISSIONE
014500020812     C                   CLEAR                   TIBS55DS
014600971106     C                   MOVEL     KNSIF         I50PSI
014700971105     C                   MOVEL     FGVLNP        I50PPO
014800971105     C                   MOVEL     D55TFP        I50APO
014900020718     C                   CALL      'TIBS55R'
015000020812     C                   PARM                    TIBS55DS
015100971105     C**
015200971105     C                   MOVEL     O50PLA        CMZLBD
015300971105     C                   MOVEL     O50ALA        CMZLBA
015400971105     C                   MOVEL     O50ASI        CMZSYF
015500930929     C                   EXSR      LCKBL
015600930930     C   40              MOVEL     'A'           WLCK
015700930929    3C                   ENDIF
015800930929     C*
015900930929     C* RISTRASMETTO PER LA LINEA DI ARRIVO SE >< FIL SCARICO E DA SIMF
016000020812    3C     FFV(XB)       IFNE      FLP(XB)
016100930929     C*
016200020812     C                   MOVEL     FFV(XB)       WFIL
016300020812     C                   CLEAR                   FNLV55DS
016400971105     C                   MOVEL     'P'           D55TPT
016500971105     C                   MOVEL     WFIL          D55LIN
016600971105     C                   MOVEL     FGVDFV        D55DRF
016700971105     C                   CALL      'FNLV55R'
016800020812     C                   PARM                    FNLV55DS
016900971105    4C     D55ERR        IFNE      ' '
017000971105     C                   MOVEL     WFIL          D55TFP
017100971105    4C                   ENDIF
017200930929     C*
017300971105     C     D55TFP        LOOKUP    LP2                                    31
017400930929     C*
017500971105    4C     *IN31         IFEQ      *OFF
017600971105     C**
017700971105     C* ESEGUO CALL PER REPERIRE LIBRERIE E S.I. DI TRASMISSIONE
017800020812     C                   CLEAR                   TIBS55DS
017900971105     C                   MOVEL     KNSIF         I50PSI
018000971105     C                   MOVEL     FGVLNP        I50PPO
018100971105     C                   MOVEL     D55TFP        I50APO
018200020718     C                   CALL      'TIBS55R'
018300020812     C                   PARM                    TIBS55DS
018400971105     C**
018500971105     C                   MOVEL     O50PLA        CMZLBD
018600971105     C                   MOVEL     O50ALA        CMZLBA
018700971105     C                   MOVEL     O50ASI        CMZSYF
018800971105     C**
018900020812     C                   ADD       1             XX
019000020812     C                   Z-ADD     D55TFP        LP2(XX)
019100930929     C*
019200930929     C                   EXSR      LCKBL
019300930930     C   40              MOVEL     'A'           WLCK
019400930929    4C                   ENDIF
019500930929    3C                   ENDIF
019600930929     C*
019700020812     C                   ADD       1             XB
019800930929    2C                   ENDDO
019900930929    1C                   ENDIF
020000000621  X1AC                   ELSE
020100000621     C* SE FV 99999 DEVO TRASMETTERE I MEMBRI PIENI
020200061108     C                   MOVEL     'N'           WFVPT
020300000621     C                   EXSR      IMPO
020400000621     C* PER SAPERE CHE MEMBRI TRASMETTERE LEGGO I TERMINAL DI
020500000621     C* PARTENZA E PER OGNUNO RICHIAMO ROUTINE LCKBL CHE TRASMETTE
020600000621     C* SOLO SE MEMBRO PIENO
020700000621     C     'P'           SETLL     AZCAE01L
020800000621     C     'P'           READE     AZCAE01L                               31
020900000621     C*
021000000621    1C     *IN31         DOWEQ     *OFF
021100000621     C*
021200000621    2C     CAETFP        IFEQ      CAETFE
021300000621     C     CAEATB        ANDEQ     *BLANKS
021400000621     C     PARDTP        ANDGE     CAEDDE
021500000621     C     PARDTP        ANDLE     CAEDSC
021600000621     C*
021700000621     C                   Z-ADD     CAETFE        D55TFP
021800000621     C* ESEGUO CALL PER REPERIRE LIBRERIE E S.I. DI TRASMISSIONE
021900020812     C                   CLEAR                   TIBS55DS
022000000621     C                   MOVEL     KNSIF         I50PSI
022100000621     C                   MOVEL     SIMFEL        I50PPO
022200000621     C                   MOVEL     D55TFP        I50APO
022300020718     C                   CALL      'TIBS55R'
022400020812     C                   PARM                    TIBS55DS
022500000621     C**
022600000621     C                   MOVEL     O50PLA        CMZLBD
022700000621     C                   MOVEL     O50ALA        CMZLBA
022800000621     C                   MOVEL     O50ASI        CMZSYF
022900000621     C***
023000000621     C                   EXSR      LCKBL
023100000621     C   40              MOVEL     'A'           WLCK
023200000621     C                   ENDIF
023300000621     C     'P'           READE     AZCAE01L                               31
023400000621     C                   ENDDO
023500000621   1AC                   ENDIF
023600930929     C*
023700971106     C                   MOVEL     PARAM5        KPJBU
023800971105     C* CHIUDO I PGM
023900020812     C                   CLEAR                   TIBS55DS
024000971105     C                   MOVEL     'C'           I50TLA
024100020718     C                   CALL      'TIBS55R'
024200020812     C                   PARM                    TIBS55DS
024300971105     C***
024400020812     C                   CLEAR                   FNLV55DS
024500971105     C                   MOVEL     'C'           D55TLA
024600971105     C                   CALL      'FNLV55R'
024700020812     C                   PARM                    FNLV55DS
024800971105     C**
024900020812     C                   MOVEL     *ON           *INLR
025000910912     C**-------------------------------------
025100930929     C** ALLOCAZIONE MEMBRI BOLLE PER FILIALE
025200910912     C**-------------------------------------
025300910912     C     LCKBL         BEGSR
025400060220     C                   SETOFF                                       4047
025500910912     C                   SETOFF                                       495051
025600060220     C                   SETOFF                                       545658
025700910926     C*
025800971105     C                   Z-ADD     D55TFP        WTFP
025900021014     c                   Move      Varxx         Varx
026000930929     C                   MOVEL     VAR2          CMZMBD
026100021014     c                   Movel     Var2          CmzMba
026200021015     c                   Movel     'M'           CMZMba
026300910926     C* VEDO SE CONTIENE RECORD
026400950113     C                   MOVEL     'FNBLPE0T'    CMZFLD           10
026500910926     C                   EXSR      NUMREC
026600910926     C                   Z-ADD     NRR           RECBPE           10 0
026700910926     C*
026800080423     C* SE NO RECORD SALTO L'INVIO DI --> FNBLPE
026900931001    1C     RECBPE        IFGT      0
027000931001     C     RECBPE        ANDEQ     *ALL'9'
027100931001     C                   SETON                                          5440
027200931001    1C                   END
027300910912     C*
027400931001    1C  N54              DO
027500910912     C**
027600000114     C                   MOVEL     'FISPP00T'    CMZFLD
027700910912     C                   EXSR      NUMREC
027800910912     C                   Z-ADD     NRR           RECSP            10 0
027900931001     C*
028000931001     C* NON RIESCO A REPERIRE RECORD NON ELABORO
028100080423    2C     RECSP         IFGT      0
028200931001     C     RECSP         ANDEQ     *ALL'9'
028300931001     C                   SETON                                          5040
028400080423    2C                   END
028500910912     C**
028600950113     C* VEDO SE CONTIENE RECORD FNFVA
028700080423    2C  N50              DO
028800960123     C                   MOVEL     'FNFVA01T'    CMZFLD
028900910912     C                   EXSR      NUMREC
029000910912     C                   Z-ADD     NRR           RECFV            10 0
029100931001     C*
029200931001     C* GIA' ALLOCATO RITENTI PIU' TARDI
029300080423    3C     RECFV         IFGT      0
029400931001     C     RECFV         ANDEQ     *ALL'9'
029500931001     C                   SETON                                          5140
029600080423    3C                   END
029700910926     C*
029800960124     C* VEDO SE CONTIENE RECORD FNFV4
029900080423    3C  N51              DO
030000000428     C                   MOVEL     'FNFWA00T'    CMZFLD
030100000428     C                   EXSR      NUMREC
030200000428     C                   Z-ADD     NRR           RECFWA           10 0
030300000428     C*
030400000428     C* GIA' ALLOCATO RITENTI PIU' TARDI
030500080423    4C     RECFWA        IFGT      0
030600000428     C     RECFWA        ANDEQ     *ALL'9'
030700000428     C                   SETON                                          5840
030800080423    4C                   END
030900960124     C*
031000080423    4C  N58              DO
031100920428     C**
031200920428     C** TRASMISSIONE FILE   T R A N S I T O
031300920428     C**
031400920428     C                   EXSR      LCK08
031500920428     C** I N V I O
031600920428     C  N40              EXSR      X25
031700910912     C**
031800960124     C*
031900000428     C* DISALLOCO FNFWA
032000080423    5C     RECFWA        IFGT      0
032100000428     C                   MOVE      VAR           CM13
032200960124     C                   MOVEL     *BLANKS       COMMAN           80
032300000428     C                   MOVEA     CM13(1)       COMMAN
032400960124     C                   CALL      'QCMDEXC'                            47
032500960124     C                   PARM                    COMMAN
032600960124     C                   PARM                    LUNG
032700080423    5C                   END
032800080423    4C                   END
032900960124     C*
033000960123     C* DISALLOCO FNFVA
033100080423    4C     RECFV         IFGT      0
033200960123     C                   MOVE      VAR           CM12
033300910912     C                   MOVEL     *BLANKS       COMMAN           80
033400910912     C                   MOVEA     CM12(1)       COMMAN
033500910912     C                   CALL      'QCMDEXC'                            47
033600910912     C                   PARM                    COMMAN
033700910912     C                   PARM                    LUNG
033800080423    4C                   END
033900080423    3C                   END
034000910912     C**
034100060220     C* DISALLOCO FISPP
034200080423    3C     RECSP         IFGT      0
034300000114     C                   MOVE      VAR           CM10
034400910912     C                   MOVEL     *BLANKS       COMMAN           80
034500910912     C                   MOVEA     CM10(1)       COMMAN
034600910912     C                   CALL      'QCMDEXC'                            47
034700910912     C                   PARM                    COMMAN
034800910912     C                   PARM                    LUNG
034900080423    3C                   END
035000080423    2C                   END
035100910912     C**
035200080423    2C     RECBPE        IFGT      0
035300950113     C                   MOVE      VAR           CM37
035400910926     C                   MOVEL     *BLANKS       COMMAN           80
035500910926     C                   MOVEA     CM37(1)       COMMAN
035600910926     C                   CALL      'QCMDEXC'                            47
035700910926     C                   PARM                    COMMAN
035800910926     C                   PARM                    LUNG
035900080423    2C                   END
036000080423    1C                   END
036100910912     C*
036200910912     C                   ENDSR
036300900601     C**-------------------------------------
036400900601     C** PREPARA SOTTOMISSIONE TRASMISSIONE
036500900601     C**-------------------------------------
036600900601     C     X25           BEGSR
036700900601     C*
036800020718     C                   MOVE      D55TFP        CMZDST
036900930310     C                   MOVE      *BLANK        CMZPRR
037000910912     C*
037100911217     C     RECBPE        IFGT      0
037200930203     C                   MOVE      *BLANK        CMZAZN
037300950113     C                   EXSR      X25BPE
037400971222     C     CMZERR        COMP      '1'                                    40
037500060220     C                   ENDif
037600960124     C*
037700920428     C                   EXSR      X25SP
037800000428     C                   EXSR      X25FWA
037900000428     C                   EXSR      X25FV
038000900601     C                   ENDSR
038100910926     C**-------------------------------------
038200910912     C** PREPARA SOTTOMISSIONE TRASMISSIONE FILE TRANSITO
038300910912     C**-------------------------------------
038400910912     C     X2508         BEGSR
038500910912     C*
038600020718     C                   MOVE      D55TFP        CMZDST
038700930203     C                   MOVE      *BLANK        CMZPRR
038800910912     C*
038900950113     C                   EXSR      X25BE8
039000910912     C                   ENDSR
039100910912     C**-------------------------------------
039200910926     C** ALLOCAZIONE MEMBRI DI TRANSITO PER INVIO
039300910912     C**-------------------------------------
039400910912     C     LCK08         BEGSR
039500080423     C                   SETOFF                                       4047
039600950113     C* VEDO SE CONTIENE RECORD FNBLP
039700950113     C                   MOVEL     'FNBLPT0T'    CMZFLD
039800910912     C                   EXSR      NUMREC
039900910926     C                   Z-ADD     NRR           RECBE8           10 0
040000931001     C*
040100931001     C* NON RIESCO A REPERIRE RECORD NON ELABORO
040200931001    1C     RECBE8        IFGT      0
040300931001    2C     RECBE8        IFEQ      *ALL'9'
040400931001     C                   SETON                                          4740
040500931001    2C                   END
040600910912     C*
040700931001    2C  N47              DO
040800080423     c                   exsr      X2508
040900910912     C**
041000950113     C* DISALLOCO FNBLPT0
041100950113     C                   MOVE      VAR           CM14
041200910912     C                   MOVEL     *BLANKS       COMMAN           80
041300910912     C                   MOVEA     CM14(1)       COMMAN
041400910912     C                   CALL      'QCMDEXC'                            47
041500910912     C                   PARM                    COMMAN
041600910912     C                   PARM                    LUNG
041700931001    2C                   END
041800931001    1C                   END
041900910912     C*
042000910912     C                   ENDSR
042100900601     C**----------------------------------
042200000428     C** SOTTOMETTE TRASMISSIONE F N B L P E 0 T
042300900601     C**----------------------------------
042400910926     C     X25BPE        BEGSR
042500910926     C     RECBPE        IFGT      0
042600950123     C                   MOVEL     'FNBLPE0T'    CMZFLD
042700950123     C                   MOVEL     'FNBLPE0R'    CMZFLA
042800950113     C                   MOVE      'LR14'        CMZAZN
042900910926     C                   MOVE      RECBPE        CMZNRR
043000910927     C*
043100910927     C                   EXSR      CALCMZ
043200910927     C                   END
043300910927     C                   ENDSR
043400910912     C**----------------------------------
043500000428     C** SOTTOMETTE TRASMISSIONE F I S P P 0 0 T
043600910912     C**----------------------------------
043700910912     C     X25SP         BEGSR
043800910912     C     RECSP         IFGT      0
043900000114     C                   MOVEL     'FISPP00T'    CMZFLD
044000000114     C                   MOVEL     'FISPP00R'    CMZFLA
044100950113     C                   MOVE      'LR22'        CMZAZN
044200910912     C                   MOVE      RECSP         CMZNRR
044300910927     C*
044400910927     C                   EXSR      CALCMZ
044500910912     C                   END
044600910912     C                   ENDSR
044700910912     C**----------------------------------
044800000428     C** SOTTOMETTE TRASMISSIONE F N F V A 0 1 T
044900910912     C**----------------------------------
045000910912     C     X25FV         BEGSR
045100910912     C     RECFV         IFGT      0
045200960123     C                   MOVEL     'FNFVA01T'    CMZFLD
045300960123     C                   MOVEL     'FNFVA01R'    CMZFLA
045400960123     C                   MOVE      'LR19'        CMZAZN
045500920325     C                   MOVE      RECFV         CMZNRR
045600910927     C*
045700910927     C                   EXSR      CALCMZ
045800910912     C                   END
045900910912     C                   ENDSR
046000000428     C**----------------------------------
046100000428     C** SOTTOMETTE TRASMISSIONE F N F W A 0 0 T
046200000428     C**----------------------------------
046300000428     C     X25FWA        BEGSR
046400000428     C     RECFWA        IFGT      0
046500000428     C                   MOVEL     'FNFWA00T'    CMZFLD
046600000428     C                   MOVEL     'FNFWA00R'    CMZFLA
046700000428     C                   MOVE      '    '        CMZAZN
046800000428     C                   MOVE      RECFWA        CMZNRR
046900000428     C*
047000000428     C                   EXSR      CALCMZ
047100000428     C                   END
047200000428     C                   ENDSR
047300910912     C**----------------------------------
047400950113     C** SOTTOMETTE TRASMISSIONE F N B L P T 0 T
047500910912     C**----------------------------------
047600910926     C     X25BE8        BEGSR
047700950113     C                   MOVEL     'FNBLPT0T'    CMZFLD
047800950113     C                   MOVEL     'FNBLPT0R'    CMZFLA
047900950113     C                   MOVE      'LR13'        CMZAZN
048000910926     C                   MOVE      RECBE8        CMZNRR
048100910927     C*
048200910927     C                   EXSR      CALCMZ
048300910912     C                   ENDSR
048400930203     C**----------------------------------
048500930203     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE
048600930203     C**----------------------------------
048700910912     C     NUMREC        BEGSR
048800930120     C                   Z-ADD     0             NRR
048900930120     C                   CALL      'TRUL51C'
049000971107     C                   PARM                    KNSIF
049100930203     C                   PARM                    CMZFLD           10
049200930120     C                   PARM                    VAR2             10
049300930120     C                   PARM                    NRR              10 0
049400910912     C                   ENDSR
049500910926     C**----------------------------------
049600910926     C** IMPOSTO ALCUNI DATI FISSI PER CREARE I FILE DI TRASMISSIONE
049700910926     C**----------------------------------
049800910926     C     IMPO          BEGSR
049900000804     C     WFVPT         IFEQ      'S'
050000000804     C                   MOVEL     'P'           EMME
050100000804     C                   MOVEL     'P'           VAR
050200000804     C                   ELSE
050300930203     C                   MOVEL     'M'           EMME
050400000804     C                   MOVEL     'M'           VAR
050500000804     C                   ENDIF
050600021014     C                   MOVE      '))'          VAR               9
050700021014     C                   Z-ADD     42            LUNG             15 5
050800920309     C*
050900910926     C                   MOVE      '0000'        CMZORI
051000910926     C                   MOVE      '0000000'     CMZDTI
051100910926     C                   MOVE      'A'           CMZADD
051200930204     C                   MOVE      'S'           CMZCLR
051300910926     C                   MOVE      'S'           CMZFTR
051400910926     C                   MOVE      *BLANK        CMZERR
051500930929     C                   MOVE      'N'           CMZURG
051600021014     C**!!!              MOVEL     'M'           ALF4              4
051700021014     C**!!!              MOVE      SIMFEL        ALF4
051800021014     C**!!!              MOVEL     ALF4          CMZMBA
051900910926     C                   ENDSR
052000910927     C**----------------------------------
052100910927     C** CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSION
052200910927     C**----------------------------------
052300910927     C     CALCMZ        BEGSR
052400971105     C* IMPOSTO SE I 2 P.O. SONO LOCALI
052500971106     C                   CLEAR                   W236
052600971106     C                   MOVEL     PARAM5        W236            236
052700971106     C**
052800971105     C                   MOVEL     O50LCL        CMZLCL            1
052900020718     C                   CALL      'TIBS53C'
053000971105     C                   PARM                    CMZLCL
053100971105     C** CAMPI PER LA TRASMISSIONE
053200971105     C                   PARM                    CMZDST
053300910927     C                   PARM                    CMZFLD
053400910927     C                   PARM                    CMZMBD
053500910927     C                   PARM                    CMZLBD
053600910927     C                   PARM                    CMZFLA
053700910927     C                   PARM                    CMZMBA
053800910927     C                   PARM                    CMZLBA
053900910927     C                   PARM                    CMZORI
054000910927     C                   PARM                    CMZDTI
054100910927     C                   PARM                    CMZURG
054200910927     C                   PARM                    CMZADD
054300910927     C                   PARM                    CMZCLR
054400910927     C                   PARM                    CMZFTR
054500910927     C                   PARM                    CMZERR
054600910927     C                   PARM                    CMZAZN
054700910927     C                   PARM                    CMZSYF
054800910927     C                   PARM                    CMZNRR
054900910927     C                   PARM                    CMZPRR
055000971106     C                   PARM                    W236
055100910927     C                   ENDSR
055200900601     C*
055300930714     C*---------------------------------------------------------------*
055400910912**         CM10
055500021014DLCOBJ OBJ((FISPP00T *FILE *EXCL Mxxx000))
055600910912**         CM12
055700021014DLCOBJ OBJ((FNFVA01T *FILE *EXCL Mxxx000))
055800000428**         CM13
055900021014DLCOBJ OBJ((FNFWA00T *FILE *EXCL Mxxx000))
056000910912**         CM14
056100021014DLCOBJ OBJ((FNBLPT0T *FILE *EXCL Mxxx000))
056200910926**         CM37
056300021014DLCOBJ OBJ((FNBLPE0T *FILE *EXCL Mxxx000))
