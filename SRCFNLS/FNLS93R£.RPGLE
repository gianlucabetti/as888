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
001100021014     D CMD6            S             42    DIM(1) CTDATA PERRCD(1)
001200021014     D CM10            S             42    DIM(1) CTDATA PERRCD(1)
001300021014     D CM12            S             42    DIM(1) CTDATA PERRCD(1)
001400021014     D CM13            S             42    DIM(1) CTDATA PERRCD(1)
001500021014     D CM14            S             42    DIM(1) CTDATA PERRCD(1)
001600021014     D CM16            S             42    DIM(1) CTDATA PERRCD(1)
001700021014     D CM37            S             42    DIM(1) CTDATA PERRCD(1)
001800930930     D*
001900000428     D LP2             S              3  0 DIM(150)
002000020812     D*
002100950113     D* PASSAGGIO DATI DA FNLSA2R - FOGLIO VIAGGIO E FIL ELABORATORE -
002200971106     D PARAM5          DS
002300971106     D  PARDTP                 1      8  0
002400971106     D  PARNFV                 9     13  0
002500971106     D  PARDEF                14     14
002600971106     D* FLAG CHE MO INDICA SE HO TROVATO DEI FILE ALLOCATI
002700971106     D  WLCK                  15     15
002800000621     D  SIMFEL               158    160  0
002900930929      *
003000930929     D* LINEE DI ARRIVO
003100930929     D                 DS
003200930929     D  FGVLNA                 1      3  0
003300930929     D  FGVFFV                 4    243
003400930929     D  FGVFF2               244    450
003500000428     D  FGWFF3               451    690
003600000428     D  FGWFF4               691    900
003700000428     D  VUOTO                901    903    INZ
003800000428     D  FFV                    1    903
003900000428     D                                     DIM(301)
004000930929      *
004100930929     D* FILIALI DI SCARICO
004200930929     D                 DS
004300930929     D  LNA2                   1      3  0
004400930929     D  FGVFLP                 4    243
004500930929     D  FGVFL2               244    450
004600000428     D  FGWFL3               451    690
004700000428     D  FGWFL4               691    900
004800000428     D  VUOTO2               901    903    INZ
004900000428     D  FLP                    1    903
005000000428     D                                     DIM(301)
005100930120     D                 DS
005200930120     D  EMME                   1      1
005300021014     d  Wsimfel                2      4  0
005400021014     D  WTFP                   5      7  0
005500021014     d  varxx                  2      7
005600930203     D  VAR2                   1     10
005700021014     D VAR             DS             9
005800021014     D  VARX                   2      7
005900020812     D*
006000920305     D KPJBA         E DS
006100900601     D DSCMZ         E DS
006200000804     D OG143         E DS
006300020812     D FNLV55DS      E DS
006400020812     D TIBS55DS      E DS
006500020812      *
006600020812      * Variabili
006700020812      *
006800020919     D LUNG            S             15  5 INZ
006900020919     D COMMAN          S             80    INZ
007000020919     D WFVPT           S              1    INZ
007100020919     D W236            S            236    INZ
007200020919     D ALF4            S              4    INZ
007300020919     D W0030           S              3  0 INZ
007400020919     D XB              S              3  0 INZ
007500020919     D XX              S              3  0 INZ
007600020919     D WFIL            S              3  0 INZ
007700020919     D RECBPE          S             10  0 INZ
007800020919     D RECBE8          S             10  0 INZ
007900020919     D RECBT8          S             10  0 INZ
008000020919     D RECBT           S             10  0 INZ
008100020919     D RECFV           S             10  0 INZ
008200020919     D RECFWA          S             10  0 INZ
008300020919     D RECSP           S             10  0 INZ
008400900601      *--------------------------------------------------------------*
008500020812     C     K02FGV01      klist
008600020812     C                   kfld                    PARnfv
008700020812     C                   kfld                    SIMfel
008800020812      *
008900000000     C     *ENTRY        PLIST
009000000000     C                   PARM                    KPJBA
009100971106     C                   MOVEL     KPJBU         PARAM5
009200021014
009300021014     c                   Movel     Simfel        Wsimfel
009400930716     C*
009500930929     C* CHAINO FOGLIO VIAGGIO
009600000621   1AC     PARNFV        IFNE      99999
009700020812     C     K02FGV01      chain     FNFGV000                           31
009800930929    1C     *IN31         IFEQ      *OFF
009900020812     C     K02FGV01      chain     FNFGW000                           31
010000000428     C     *IN31         IFEQ      *ON
010100000428     C     FGWATB        ORNE      ' '
010200000428     C                   CLEAR                   FGWFF3
010300000428     C                   CLEAR                   FGWFF4
010400000428     C                   CLEAR                   FGWFL3
010500000428     C                   CLEAR                   FGWFL4
010600000804     C                   CLEAR                   FGWFGS
010700000428     C                   ENDIF
010800020812     C                   Z-ADD     FGWFGS        W0030
010900000804     C     W0030         IFEQ      0
011000000804     C                   Z-ADD     FGVLNP        W0030
011100000804     C                   ENDIF
011200000804     C     W0030         CHAIN     AZORG01L                           31
011300000804     C  N31              MOVEL     ORGDE3        OG143
011400000804     C   31              CLEAR                   OG143
011500020812     C                   IF        §OGntw = 'PPT'
011600020812     C                   MOVEL     'S'           WFVPT
011700000804     C                   ELSE
011800000804     C                   MOVEL     'N'           WFVPT
011900000804     C                   ENDIF
012000971105     C**
012100971105     C* IMPOSTO ALCUNI VALORI FISSI X CREARE FILE TRASMISSIONE
012200971105     C                   EXSR      IMPO
012300930929     C                   Z-ADD     FGVLNA        LNA2
012400930929     C*
012500020812     C                   Z-ADD     1             XB
012600020812     C                   Z-ADD     0             XX
012700930929     C*
012800020812    2C     FLP(XB)       DOWGT     *ZEROS
012900020812     C                   MOVEL     FLP(XB)       WFIL
013000930929     C     WFIL          LOOKUP    LP2                                    31
013100930929     C*
013200930929    3C     *IN31         IFEQ      *OFF
013300020812     C                   ADD       1             XX
013400020812     C                   Z-ADD     WFIL          LP2(XX)
013500930929     C*
013600020812     C                   CLEAR                   FNLV55DS
013700971105     C                   MOVEL     'P'           D55TPT
013800971105     C                   MOVEL     WFIL          D55LIN
013900971105     C                   MOVEL     FGVDFV        D55DRF
014000971105     C                   CALL      'FNLV55R'
014100020812     C                   PARM                    FNLV55DS
014200971105    4C     D55ERR        IFNE      ' '
014300971105     C                   MOVEL     WFIL          D55TFP
014400971105    4C                   ENDIF
014500971105     C**
014600971105     C* ESEGUO CALL PER REPERIRE LIBRERIE E S.I. DI TRASMISSIONE
014700020812     C                   CLEAR                   TIBS55DS
014800971106     C                   MOVEL     KNSIF         I50PSI
014900971105     C                   MOVEL     FGVLNP        I50PPO
015000971105     C                   MOVEL     D55TFP        I50APO
015100020718     C                   CALL      'TIBS55R'
015200020812     C                   PARM                    TIBS55DS
015300971105     C**
015400971105     C                   MOVEL     O50PLA        CMZLBD
015500971105     C                   MOVEL     O50ALA        CMZLBA
015600971105     C                   MOVEL     O50ASI        CMZSYF
015700930929     C                   EXSR      LCKBL
015800930930     C   40              MOVEL     'A'           WLCK
015900930929    3C                   ENDIF
016000930929     C*
016100930929     C* RISTRASMETTO PER LA LINEA DI ARRIVO SE >< FIL SCARICO E DA SIMF
016200020812    3C     FFV(XB)       IFNE      FLP(XB)
016300930929     C*
016400020812     C                   MOVEL     FFV(XB)       WFIL
016500020812     C                   CLEAR                   FNLV55DS
016600971105     C                   MOVEL     'P'           D55TPT
016700971105     C                   MOVEL     WFIL          D55LIN
016800971105     C                   MOVEL     FGVDFV        D55DRF
016900971105     C                   CALL      'FNLV55R'
017000020812     C                   PARM                    FNLV55DS
017100971105    4C     D55ERR        IFNE      ' '
017200971105     C                   MOVEL     WFIL          D55TFP
017300971105    4C                   ENDIF
017400930929     C*
017500971105     C     D55TFP        LOOKUP    LP2                                    31
017600930929     C*
017700971105    4C     *IN31         IFEQ      *OFF
017800971105     C**
017900971105     C* ESEGUO CALL PER REPERIRE LIBRERIE E S.I. DI TRASMISSIONE
018000020812     C                   CLEAR                   TIBS55DS
018100971105     C                   MOVEL     KNSIF         I50PSI
018200971105     C                   MOVEL     FGVLNP        I50PPO
018300971105     C                   MOVEL     D55TFP        I50APO
018400020718     C                   CALL      'TIBS55R'
018500020812     C                   PARM                    TIBS55DS
018600971105     C**
018700971105     C                   MOVEL     O50PLA        CMZLBD
018800971105     C                   MOVEL     O50ALA        CMZLBA
018900971105     C                   MOVEL     O50ASI        CMZSYF
019000971105     C**
019100020812     C                   ADD       1             XX
019200020812     C                   Z-ADD     D55TFP        LP2(XX)
019300930929     C*
019400930929     C                   EXSR      LCKBL
019500930930     C   40              MOVEL     'A'           WLCK
019600930929    4C                   ENDIF
019700930929    3C                   ENDIF
019800930929     C*
019900020812     C                   ADD       1             XB
020000930929    2C                   ENDDO
020100930929    1C                   ENDIF
020200000621  X1AC                   ELSE
020300000621     C* SE FV 99999 DEVO TRASMETTERE I MEMBRI PIENI
020400061108     C                   MOVEL     'N'           WFVPT
020500000621     C                   EXSR      IMPO
020600000621     C* PER SAPERE CHE MEMBRI TRASMETTERE LEGGO I TERMINAL DI
020700000621     C* PARTENZA E PER OGNUNO RICHIAMO ROUTINE LCKBL CHE TRASMETTE
020800000621     C* SOLO SE MEMBRO PIENO
020900000621     C     'P'           SETLL     AZCAE01L
021000000621     C     'P'           READE     AZCAE01L                               31
021100000621     C*
021200000621    1C     *IN31         DOWEQ     *OFF
021300000621     C*
021400000621    2C     CAETFP        IFEQ      CAETFE
021500000621     C     CAEATB        ANDEQ     *BLANKS
021600000621     C     PARDTP        ANDGE     CAEDDE
021700000621     C     PARDTP        ANDLE     CAEDSC
021800000621     C*
021900000621     C                   Z-ADD     CAETFE        D55TFP
022000000621     C* ESEGUO CALL PER REPERIRE LIBRERIE E S.I. DI TRASMISSIONE
022100020812     C                   CLEAR                   TIBS55DS
022200000621     C                   MOVEL     KNSIF         I50PSI
022300000621     C                   MOVEL     SIMFEL        I50PPO
022400000621     C                   MOVEL     D55TFP        I50APO
022500020718     C                   CALL      'TIBS55R'
022600020812     C                   PARM                    TIBS55DS
022700000621     C**
022800000621     C                   MOVEL     O50PLA        CMZLBD
022900000621     C                   MOVEL     O50ALA        CMZLBA
023000000621     C                   MOVEL     O50ASI        CMZSYF
023100000621     C***
023200000621     C                   EXSR      LCKBL
023300000621     C   40              MOVEL     'A'           WLCK
023400000621     C                   ENDIF
023500000621     C     'P'           READE     AZCAE01L                               31
023600000621     C                   ENDDO
023700000621   1AC                   ENDIF
023800930929     C*
023900971106     C                   MOVEL     PARAM5        KPJBU
024000971105     C* CHIUDO I PGM
024100020812     C                   CLEAR                   TIBS55DS
024200971105     C                   MOVEL     'C'           I50TLA
024300020718     C                   CALL      'TIBS55R'
024400020812     C                   PARM                    TIBS55DS
024500971105     C***
024600020812     C                   CLEAR                   FNLV55DS
024700971105     C                   MOVEL     'C'           D55TLA
024800971105     C                   CALL      'FNLV55R'
024900020812     C                   PARM                    FNLV55DS
025000971105     C**
025100020812     C                   MOVEL     *ON           *INLR
025200910912     C**-------------------------------------
025300930929     C** ALLOCAZIONE MEMBRI BOLLE PER FILIALE
025400910912     C**-------------------------------------
025500910912     C     LCKBL         BEGSR
025600060220     C                   SETOFF                                       4047
025700910912     C                   SETOFF                                       495051
025800060220     C                   SETOFF                                       545658
025900910926     C*
026000971105     C                   Z-ADD     D55TFP        WTFP
026100021014     c                   Move      Varxx         Varx
026200930929     C                   MOVEL     VAR2          CMZMBD
026300021014     c                   Movel     Var2          CmzMba
026400021015     c                   Movel     'M'           CMZMba
026500910926     C* VEDO SE CONTIENE RECORD
026600950113     C                   MOVEL     'FNBLPE0T'    CMZFLD           10
026700910926     C                   EXSR      NUMREC
026800910926     C                   Z-ADD     NRR           RECBPE           10 0
026900910926     C*
027000991026     C* SE NO RECORD SALTO L'INVIO DI --> FNBLPE FIAR6 FIAR7 FNBLT
027100931001    1C     RECBPE        IFGT      0
027200931001     C     RECBPE        ANDEQ     *ALL'9'
027300931001     C                   SETON                                          5440
027400931001    1C                   END
027500910912     C*
027600931001    1C  N54              DO
027700931001     C**
027800910912     C*
027900950113     C* VEDO SE CONTIENE RECORD FNBLT
028000060220    2C     RECBPE        IFGT      0
028100950113     C                   MOVEL     'FNBLTE0T'    CMZFLD
028200910912     C                   EXSR      NUMREC
028300910912     C                   Z-ADD     NRR           RECBT            10 0
028400931001     C*
028500931001     C* NON RIESCO A REPERIRE RECORD NON ELABORO
028600060220    3C     RECBT         IFGT      0
028700931001     C     RECBT         ANDEQ     *ALL'9'
028800931001     C                   SETON                                          4940
028900060220    3C                   END
029000060220    2C                   END
029100980310     C*
029200980310     C* VEDO SE CONTIENE RECORD FNBLS
029300060220    2C  N49              DO
029400910912     C**
029500000114     C                   MOVEL     'FISPP00T'    CMZFLD
029600910912     C                   EXSR      NUMREC
029700910912     C                   Z-ADD     NRR           RECSP            10 0
029800931001     C*
029900931001     C* NON RIESCO A REPERIRE RECORD NON ELABORO
030000060220    3C     RECSP         IFGT      0
030100931001     C     RECSP         ANDEQ     *ALL'9'
030200931001     C                   SETON                                          5040
030300060220    3C                   END
030400910912     C**
030500950113     C* VEDO SE CONTIENE RECORD FNFVA
030600060220    3C  N50              DO
030700960123     C                   MOVEL     'FNFVA01T'    CMZFLD
030800910912     C                   EXSR      NUMREC
030900910912     C                   Z-ADD     NRR           RECFV            10 0
031000931001     C*
031100931001     C* GIA' ALLOCATO RITENTI PIU' TARDI
031200060220    4C     RECFV         IFGT      0
031300931001     C     RECFV         ANDEQ     *ALL'9'
031400931001     C                   SETON                                          5140
031500060220    4C                   END
031600910926     C*
031700960124     C* VEDO SE CONTIENE RECORD FNFV4
031800060220    4C  N51              DO
031900000428     C                   MOVEL     'FNFWA00T'    CMZFLD
032000000428     C                   EXSR      NUMREC
032100000428     C                   Z-ADD     NRR           RECFWA           10 0
032200000428     C*
032300000428     C* GIA' ALLOCATO RITENTI PIU' TARDI
032400060220    6C     RECFWA        IFGT      0
032500000428     C     RECFWA        ANDEQ     *ALL'9'
032600000428     C                   SETON                                          5840
032700060220    6C                   END
032800960124     C*
032900060220    6C  N58              DO
033000920428     C**
033100920428     C** TRASMISSIONE FILE   T R A N S I T O
033200920428     C**
033300920428     C                   EXSR      LCK08
033400920428     C** I N V I O
033500920428     C  N40              EXSR      X25
033600910912     C**
033700960124     C*
033800000428     C* DISALLOCO FNFWA
033900060220    7C     RECFWA        IFGT      0
034000000428     C                   MOVE      VAR           CM13
034100960124     C                   MOVEL     *BLANKS       COMMAN           80
034200000428     C                   MOVEA     CM13(1)       COMMAN
034300960124     C                   CALL      'QCMDEXC'                            47
034400960124     C                   PARM                    COMMAN
034500960124     C                   PARM                    LUNG
034600060220    7C                   END
034700060220    6C                   END
034800960124     C*
034900960123     C* DISALLOCO FNFVA
035000060220    5C     RECFV         IFGT      0
035100960123     C                   MOVE      VAR           CM12
035200910912     C                   MOVEL     *BLANKS       COMMAN           80
035300910912     C                   MOVEA     CM12(1)       COMMAN
035400910912     C                   CALL      'QCMDEXC'                            47
035500910912     C                   PARM                    COMMAN
035600910912     C                   PARM                    LUNG
035700060220    5C                   END
035800060220    4C                   END
035900910912     C**
036000060220     C* DISALLOCO FISPP
036100060220    4C     RECSP         IFGT      0
036200000114     C                   MOVE      VAR           CM10
036300910912     C                   MOVEL     *BLANKS       COMMAN           80
036400910912     C                   MOVEA     CM10(1)       COMMAN
036500910912     C                   CALL      'QCMDEXC'                            47
036600910912     C                   PARM                    COMMAN
036700910912     C                   PARM                    LUNG
036800060220    4C                   END
036900060220    3C                   END
037000910912     C**
037100910912     C* DISALLOCO
037200060220    3C     RECBT         IFGT      0
037300950113     C                   MOVE      VAR           CMD6
037400910912     C                   MOVEL     *BLANKS       COMMAN           80
037500910912     C                   MOVEA     CMD6(1)       COMMAN
037600910912     C                   CALL      'QCMDEXC'                            47
037700910912     C                   PARM                    COMMAN
037800910912     C                   PARM                    LUNG
037900060220    3C                   END
038000060220    2C                   END
038100910926     C*
038200060220    2C     RECBPE        IFGT      0
038300950113     C                   MOVE      VAR           CM37
038400910926     C                   MOVEL     *BLANKS       COMMAN           80
038500910926     C                   MOVEA     CM37(1)       COMMAN
038600910926     C                   CALL      'QCMDEXC'                            47
038700910926     C                   PARM                    COMMAN
038800910926     C                   PARM                    LUNG
038900060220    2C                   END
039000060220    1C                   END
039100910912     C*
039200910912     C                   ENDSR
039300900601     C**-------------------------------------
039400900601     C** PREPARA SOTTOMISSIONE TRASMISSIONE
039500900601     C**-------------------------------------
039600900601     C     X25           BEGSR
039700900601     C*
039800020718     C                   MOVE      D55TFP        CMZDST
039900930310     C                   MOVE      *BLANK        CMZPRR
040000910912     C*
040100911217     C     RECBPE        IFGT      0
040200930203     C                   MOVE      *BLANK        CMZAZN
040300971222     C                   EXSR      X25BT
040400971222     C     CMZERR        COMP      '1'                                    40
040500971222     C     *IN40         IFEQ      *OFF
040600950113     C                   EXSR      X25BPE
040700971222     C     CMZERR        COMP      '1'                                    40
040800060220     C                   ENDif
040900030110     c                   EndIf
041000960124     C*
041100920428     C                   EXSR      X25SP
041200000428     C                   EXSR      X25FWA
041300000428     C                   EXSR      X25FV
041400900601     C                   ENDSR
041500910926     C**-------------------------------------
041600910912     C** PREPARA SOTTOMISSIONE TRASMISSIONE FILE TRANSITO
041700910912     C**-------------------------------------
041800910912     C     X2508         BEGSR
041900910912     C*
042000020718     C                   MOVE      D55TFP        CMZDST
042100930203     C                   MOVE      *BLANK        CMZPRR
042200910912     C*
042300950113     C                   EXSR      X25T08
042400950113     C                   EXSR      X25BE8
042500910912     C                   ENDSR
042600910912     C**-------------------------------------
042700910926     C** ALLOCAZIONE MEMBRI DI TRANSITO PER INVIO
042800910912     C**-------------------------------------
042900910912     C     LCK08         BEGSR
043000910912     C                   SETOFF                                       404748
043100910926     C                   SETOFF                                       49
043200950113     C* VEDO SE CONTIENE RECORD FNBLP
043300950113     C                   MOVEL     'FNBLPT0T'    CMZFLD
043400910912     C                   EXSR      NUMREC
043500910926     C                   Z-ADD     NRR           RECBE8           10 0
043600931001     C*
043700931001     C* NON RIESCO A REPERIRE RECORD NON ELABORO
043800931001    1C     RECBE8        IFGT      0
043900931001    2C     RECBE8        IFEQ      *ALL'9'
044000931001     C                   SETON                                          4740
044100931001    2C                   END
044200910912     C*
044300931001    2C  N47              DO
044400910926     C*
044500950113     C* VEDO SE CONTIENE RECORD FNBLT08
044600950113     C                   MOVEL     'FNBLTT0T'    CMZFLD
044700910912     C                   EXSR      NUMREC
044800930120     C                   Z-ADD     NRR           RECBT8           10 0
044900931001     C*
045000931001     C* NON RIESCO A REPERIRE RECORD NON ELABORO
045100931001    3C     RECBT8        IFGT      0
045200931001     C     RECBT8        ANDEQ     *ALL'9'
045300931001     C                   SETON                                          4840
045400931001    3C                   END
045500910912     C*
045600950113     C* VEDO SE CONTIENE RECORD FNBLT
045700931001    3C  N48              DO
045800910912     C**
045900910912     C                   EXSR      X2508
046000910912     C**
046100950113     C* DISALLOCO FNBLTT0
046200931001    4C     RECBT8        IFGT      0
046300950113     C                   MOVE      VAR           CM16
046400910912     C                   MOVEL     *BLANKS       COMMAN           80
046500910912     C                   MOVEA     CM16(1)       COMMAN
046600910912     C                   CALL      'QCMDEXC'                            47
046700910912     C                   PARM                    COMMAN
046800910912     C                   PARM                    LUNG
046900931001    4C                   END
047000931001    3C                   END
047100910912     C**
047200950113     C* DISALLOCO FNBLPT0
047300950113     C                   MOVE      VAR           CM14
047400910912     C                   MOVEL     *BLANKS       COMMAN           80
047500910912     C                   MOVEA     CM14(1)       COMMAN
047600910912     C                   CALL      'QCMDEXC'                            47
047700910912     C                   PARM                    COMMAN
047800910912     C                   PARM                    LUNG
047900931001    2C                   END
048000931001    1C                   END
048100910912     C*
048200910912     C                   ENDSR
048300900601     C**----------------------------------
048400000428     C** SOTTOMETTE TRASMISSIONE F N B L P E 0 T
048500900601     C**----------------------------------
048600910926     C     X25BPE        BEGSR
048700910926     C     RECBPE        IFGT      0
048800950123     C                   MOVEL     'FNBLPE0T'    CMZFLD
048900950123     C                   MOVEL     'FNBLPE0R'    CMZFLA
049000950113     C                   MOVE      'LR14'        CMZAZN
049100910926     C                   MOVE      RECBPE        CMZNRR
049200910927     C*
049300910927     C                   EXSR      CALCMZ
049400910927     C                   END
049500910927     C                   ENDSR
049600980310     C**----------------------------------
049700950113     C** SOTTOMETTE TRASMISSIONE F N B L T E 0 T
049800910912     C**----------------------------------
049900910912     C     X25BT         BEGSR
050000910912     C     RECBT         IFGT      *ZERO
050100950113     C                   MOVEL     'FNBLTE0T'    CMZFLD
050200950113     C                   MOVEL     'FNBLTE0R'    CMZFLA
050300910912     C                   MOVE      RECBT         CMZNRR
050400910927     C*
050500910927     C                   EXSR      CALCMZ
050600910912     C                   END
050700910912     C                   ENDSR
050800910912     C**----------------------------------
050900000428     C** SOTTOMETTE TRASMISSIONE F I S P P 0 0 T
051000910912     C**----------------------------------
051100910912     C     X25SP         BEGSR
051200910912     C     RECSP         IFGT      0
051300000114     C                   MOVEL     'FISPP00T'    CMZFLD
051400000114     C                   MOVEL     'FISPP00R'    CMZFLA
051500950113     C                   MOVE      'LR22'        CMZAZN
051600910912     C                   MOVE      RECSP         CMZNRR
051700910927     C*
051800910927     C                   EXSR      CALCMZ
051900910912     C                   END
052000910912     C                   ENDSR
052100910912     C**----------------------------------
052200000428     C** SOTTOMETTE TRASMISSIONE F N F V A 0 1 T
052300910912     C**----------------------------------
052400910912     C     X25FV         BEGSR
052500910912     C     RECFV         IFGT      0
052600960123     C                   MOVEL     'FNFVA01T'    CMZFLD
052700960123     C                   MOVEL     'FNFVA01R'    CMZFLA
052800960123     C                   MOVE      'LR19'        CMZAZN
052900920325     C                   MOVE      RECFV         CMZNRR
053000910927     C*
053100910927     C                   EXSR      CALCMZ
053200910912     C                   END
053300910912     C                   ENDSR
053400000428     C**----------------------------------
053500000428     C** SOTTOMETTE TRASMISSIONE F N F W A 0 0 T
053600000428     C**----------------------------------
053700000428     C     X25FWA        BEGSR
053800000428     C     RECFWA        IFGT      0
053900000428     C                   MOVEL     'FNFWA00T'    CMZFLD
054000000428     C                   MOVEL     'FNFWA00R'    CMZFLA
054100000428     C                   MOVE      '    '        CMZAZN
054200000428     C                   MOVE      RECFWA        CMZNRR
054300000428     C*
054400000428     C                   EXSR      CALCMZ
054500000428     C                   END
054600000428     C                   ENDSR
054700910912     C**----------------------------------
054800950113     C** SOTTOMETTE TRASMISSIONE F N B L P T 0 T
054900910912     C**----------------------------------
055000910926     C     X25BE8        BEGSR
055100950113     C                   MOVEL     'FNBLPT0T'    CMZFLD
055200950113     C                   MOVEL     'FNBLPT0R'    CMZFLA
055300950113     C                   MOVE      'LR13'        CMZAZN
055400910926     C                   MOVE      RECBE8        CMZNRR
055500910927     C*
055600910927     C                   EXSR      CALCMZ
055700910912     C                   ENDSR
055800910912     C**----------------------------------
055900950113     C** SOTTOMETTE TRASMISSIONE F N B L T T 0 T
056000910912     C**----------------------------------
056100910912     C     X25T08        BEGSR
056200930120     C     RECBT8        IFGT      *ZERO
056300950113     C                   MOVEL     'FNBLTT0T'    CMZFLD
056400950113     C                   MOVEL     'FNBLTT0R'    CMZFLA
056500930203     C                   MOVE      *BLANKS       CMZAZN
056600930120     C                   MOVE      RECBT8        CMZNRR
056700910927     C*
056800910927     C                   EXSR      CALCMZ
056900910912     C                   END
057000910912     C                   ENDSR
057100930203     C**----------------------------------
057200930203     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE
057300930203     C**----------------------------------
057400910912     C     NUMREC        BEGSR
057500930120     C                   Z-ADD     0             NRR
057600930120     C                   CALL      'TRUL51C'
057700971107     C                   PARM                    KNSIF
057800930203     C                   PARM                    CMZFLD           10
057900930120     C                   PARM                    VAR2             10
058000930120     C                   PARM                    NRR              10 0
058100910912     C                   ENDSR
058200910926     C**----------------------------------
058300910926     C** IMPOSTO ALCUNI DATI FISSI PER CREARE I FILE DI TRASMISSIONE
058400910926     C**----------------------------------
058500910926     C     IMPO          BEGSR
058600000804     C     WFVPT         IFEQ      'S'
058700000804     C                   MOVEL     'P'           EMME
058800000804     C                   MOVEL     'P'           VAR
058900000804     C                   ELSE
059000930203     C                   MOVEL     'M'           EMME
059100000804     C                   MOVEL     'M'           VAR
059200000804     C                   ENDIF
059300021014     C                   MOVE      '))'          VAR               9
059400021014     C                   Z-ADD     42            LUNG             15 5
059500920309     C*
059600910926     C                   MOVE      '0000'        CMZORI
059700910926     C                   MOVE      '0000000'     CMZDTI
059800910926     C                   MOVE      'A'           CMZADD
059900930204     C                   MOVE      'S'           CMZCLR
060000910926     C                   MOVE      'S'           CMZFTR
060100910926     C                   MOVE      *BLANK        CMZERR
060200930929     C                   MOVE      'N'           CMZURG
060300021014     C**!!!              MOVEL     'M'           ALF4              4
060400021014     C**!!!              MOVE      SIMFEL        ALF4
060500021014     C**!!!              MOVEL     ALF4          CMZMBA
060600910926     C                   ENDSR
060700910927     C**----------------------------------
060800910927     C** CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSION
060900910927     C**----------------------------------
061000910927     C     CALCMZ        BEGSR
061100971105     C* IMPOSTO SE I 2 P.O. SONO LOCALI
061200971106     C                   CLEAR                   W236
061300971106     C                   MOVEL     PARAM5        W236            236
061400971106     C**
061500971105     C                   MOVEL     O50LCL        CMZLCL            1
061600020718     C                   CALL      'TIBS53C'
061700971105     C                   PARM                    CMZLCL
061800971105     C** CAMPI PER LA TRASMISSIONE
061900971105     C                   PARM                    CMZDST
062000910927     C                   PARM                    CMZFLD
062100910927     C                   PARM                    CMZMBD
062200910927     C                   PARM                    CMZLBD
062300910927     C                   PARM                    CMZFLA
062400910927     C                   PARM                    CMZMBA
062500910927     C                   PARM                    CMZLBA
062600910927     C                   PARM                    CMZORI
062700910927     C                   PARM                    CMZDTI
062800910927     C                   PARM                    CMZURG
062900910927     C                   PARM                    CMZADD
063000910927     C                   PARM                    CMZCLR
063100910927     C                   PARM                    CMZFTR
063200910927     C                   PARM                    CMZERR
063300910927     C                   PARM                    CMZAZN
063400910927     C                   PARM                    CMZSYF
063500910927     C                   PARM                    CMZNRR
063600910927     C                   PARM                    CMZPRR
063700971106     C                   PARM                    W236
063800910927     C                   ENDSR
063900900601     C*
064000930714     C*---------------------------------------------------------------*
064100910912**         CMD6
064200021014DLCOBJ OBJ((FNBLTE0T *FILE *EXCL Mxxx000))
064300910912**         CM10
064400021014DLCOBJ OBJ((FISPP00T *FILE *EXCL Mxxx000))
064500910912**         CM12
064600021014DLCOBJ OBJ((FNFVA01T *FILE *EXCL Mxxx000))
064700000428**         CM13
064800021014DLCOBJ OBJ((FNFWA00T *FILE *EXCL Mxxx000))
064900910912**         CM14
065000021014DLCOBJ OBJ((FNBLPT0T *FILE *EXCL Mxxx000))
065100910912**         CM16
065200021014DLCOBJ OBJ((FNBLTT0T *FILE *EXCL Mxxx000))
065300910926**         CM37
065400021014DLCOBJ OBJ((FNBLPE0T *FILE *EXCL Mxxx000))
