000100940118     H DECEDIT('0,') DATEDIT(*DMY.)
000200991230     H* TNSB32R *----------------------------------------------------*
000300940118     H*           CONTROLLO MANCA TARIFFA      - FILTRO -            *
000400940118     H*--------------------------------------------------------------*
000500941122     FTNSB32D   CF   E             WORKSTN
000600940118     FAZORG01L  IF   E           K DISK
000700960514     FCNACO00F  IF   E           K DISK
000800940118     D PARAM           DS
000900941122     D  VIDDSD                 1      8  0
001000941122     D  VIDDSA                 9     16  0
001100941122     D  COMDSD                17     24  0
001200941122     D  COMDSA                25     32  0
001300940118     D* FILIALE DA CONTROLLARE
001400941122     D  VIDFIL                33     35  0
001500941122     D  PARDES                36     55
001600941122     D  VIDERB                56     56
001700960514     D  VIDCLI                57     63  0
001800960514     D  VIDCLD                64     98
001900960514     D  VIDIMP                99     99
002000960521     D  VIDLET               100    100
002100991230     D  VIDTER               101    103  0
002200000111     D  PARTER               104    123
002300000111     D  VIDSTA               124    124
002400960521     D*
002500960514     D***** SCOMPOSIZIONE DI TCU PER FLG E TIPO CONTO
002600960514     D CARCNT          DS
002700960514     D  FLGCNT                 1      4
002800960514     D  FL1CNT                 1      1
002900960514     D  FL2CNT                 2      2
003000960514     D  FL3CNT                 3      3
003100960514     D  FL4CNT                 4      4
003200960514     D  TIPCNT                 5      6
003300960514     D  FL12§                  1      2
003400940118     D WLBDAT          DS
003500941122     D  G02DAT                 1      8  0
003600941122     D  G02INV                 9     16  0
003700941122     D  G02ERR                17     17
003800941122     D  G02TGI                18     22  0
003900940118     D KPJBA         E DS
004000000111     D DSLV55        E DS                  EXTNAME(FNLV55DS)
004100000111     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004200960514     D  DCU                   98    397
004300960514     D                                     DIM(50)                              *TAB DESCRI TR.CN
004400960514     D  TCU                  398    697
004500960514     D                                     DIM(50)                              *TAB FLG 8  TP.CN
004600960514     D  KCU                  698    847P 0
004700960514     D                                     DIM(50)                              *TAB CAPICONTO
004800960514     D                                     PACKEVEN
004900940118     C*****************************************************************
005000940118     C* RIEPILOGO INDICATORI
005100940118     C*****************************************************************
005200940118     C* 30    - DI COMODO
005300000111     C* 40/50 - INDICATORI DI ERRORE
005400940118     C* 90    - SEGNALA LA PRESENZA DI ERRORI
005500940118     C*****************************************************************
005600940118     C     *ENTRY        PLIST
005700940118     C                   PARM                    KPJBA
005800941122     C*
005900940118     C                   Z-ADD     1             CODUT
006000940118     C                   CALL      'X§PARUT'
006100940118     C                   PARM                    UT§DSE
006200941122     C                   MOVEL     RAGUT         RSUT
006300960514     C                   EXSR      CARTAB
006400940118     C*---------------------------------------------------------------*
006500940118     C*
006600940118     C     INIZIO        TAG
006700940118     C                   CLEAR                   VIDFIL
006800940118     C                   CLEAR                   DESFIL
006900991230     C                   CLEAR                   VIDTER
007000991230     C                   CLEAR                   DESTER
007100940119     C                   CLEAR                   PARDES
007200940118     C                   CLEAR                   VIDDSD
007300940118     C                   CLEAR                   VIDDSA
007400940118     C                   CLEAR                   COMDSD
007500940118     C                   CLEAR                   COMDSA
007600940217     C                   CLEAR                   VIDERB
007700960514     C                   CLEAR                   VIDCLI
007800960514     C                   CLEAR                   VIDCLD
007900960514     C                   CLEAR                   VIDIMP
008000050711     C                   MOVEL     'N'           VIDLET
008100940118     C*
008200940118     C     FOR01         TAG
008300941122     C                   EXFMT     SB32D01
008400940118     C** CMD3 - FINE LAVORO
008500940118     C   KC              GOTO      FINE
008600940114     C*
008700940114     C* CONTROLLI FORMATO
008800940118     C                   EXSR      CONTR
008900941122     C  NKF
009000940118     COR 90              GOTO      FOR01
009100940114     C*
009200940114     C** CMD1 - ELABORAZIONE
009300050706     C                   MOVEL     PARAM         KPJBU
009400050706     C                   MOVEL     'SB33'        KCOAZ
009500050706      * BCH09
009600050706     c                   move      '3'           Kritb
009700050706     C                   CALL      'BCH09'
009800050706     C                   PARM                    KPJBA
009900050706     c                   If        Kritb = ' '
010000940118     C                   CALL      'BCH10'
010100940118     C                   PARM                    KPJBA
010200050706     c                   endif
010300940118     C*
010400940118     C     FINE          TAG
010500940118     C                   SETON                                        LR
010600960514     C**************************************************************************
010700960514     C***** CARICA TABELLA CAPICONTO DI RAGGRUPPAMENTO E C.CN BANCHE*
010800960514     C***** & C.CN IVA, C.CN CLIENTI, C.CN AGENTI *******************
010900960514     C**************************************************************************
011000960514     C     CARTAB        BEGSR
011100960514     C                   Z-ADD     1             X                 2 0
011200960514     C                   DO        50            Y                 2 0
011300960514     C                   MOVE      TCU(Y)        CARCNT
011400960514     C     FL12§         IFEQ      '09'                                          C.CORRISP
011500960514     C                   Z-ADD     KCU(Y)        KCOR              4 0           X SCR.CNIND
011600960514     C                   END                                                    *
011700960514     C     FL12§         IFEQ      '10'                                          C.CORRISP 164
011800960514     C                   Z-ADD     KCU(Y)        KCOX              4 0           X SCR.CNIND
011900960514     C                   END                                                    *
012000960514     C     FL12§         IFEQ      '11'                                          C.CORRISP 167
012100960514     C                   Z-ADD     KCU(Y)        KCOY              4 0           X SCR.CNIND
012200960514     C                   END                                                    *
012300960514     C     FL4CNT        IFEQ      '6'
012400960514     C                   Z-ADD     KCU(Y)        CCNBNC            4 0
012500960514     C                   END
012600960514     C     FL4CNT        IFEQ      '3'
012700960514     C                   Z-ADD     KCU(Y)        AGEITA            4 0
012800960514     C                   END
012900960514     C     FL4CNT        IFEQ      '1'
013000960514     C     FL3CNT        IFEQ      '0'
013100960514     C                   Z-ADD     KCU(Y)        CLIITA            4 0
013200960514     C                   ELSE
013300960514     C                   Z-ADD     KCU(Y)        CLIEST            4 0
013400960514     C                   END
013500960514     C                   END
013600960514     C     FL4CNT        IFEQ      '2'
013700960514     C     FL3CNT        IFEQ      '0'
013800960514     C                   Z-ADD     KCU(Y)        FORITA            4 0
013900960514     C                   ELSE
014000960514     C                   Z-ADD     KCU(Y)        FOREST            4 0
014100960514     C                   END
014200960514     C                   END
014300960514     C     FL3CNT        IFEQ      'I'
014400960514     C                   Z-ADD     KCU(Y)        CCNIVA            4 0
014500960514     C                   END
014600960514     C     TIPCNT        IFEQ      'CI'
014700960514     C                   Z-ADD     KCU(Y)        CCNCDC            4 0          KCC C.D.C.
014800960514     C                   END
014900960514     C                   END
015000000111     C**
015100000111     C* UDATE IN GGMMAAAA
015200000111     C                   TIME                    W0140            14 0
015300000111     C                   MOVE      W0140         WDTGIO            8 0
015400000111     C*
015500000111     C* UDATE IN AAAAMMGG
015600000111     C                   Z-ADD     WDTGIO        G02DAT
015700000111     C                   MOVEL     *BLANK        G02ERR
015800000111     C                   CALL      'XSRDA8'
015900000111     C                   PARM                    WLBDAT
016000000111     C                   MOVEL     G02INV        DATEU             8 0
016100000111     C*
016200960514     C                   ENDSR
016300941122     C**************************************************************************
016400941122     C*    CONTROLLI FORMATO
016500941122     C**************************************************************************
016600940118     C     CONTR         BEGSR
016700941122     C*
016800940118     C                   SETOFF                                       90
016900991230     C                   CLEAR                   DESTER
017000991230     C                   CLEAR                   PARTER
017100991230     C****  TERMINAL  ***
017200991230    1C     VIDTER        IFNE      0
017300991230     C*
017400991230    2C     VIDTER        IFEQ      046
017500991230     C* NON SI PUO' IMMETTERE COME LINEA LA SEDE
017600991230     C                   SETON                                        4790
017700991230     C                   GOTO      ENDCTR
017800991230   X2C                   ELSE
017900991230     C*
018000991230     C* CONTROLLO VALIDITA' DELLA FILIALE IMMESSA
018100991230     C     VIDTER        CHAIN     AZORG                              30
018200000111     C*
018300000111    3C     *IN30         IFEQ      *ON
018400991230     C     ORGFVA        ORNE      ' '
018500991230     C     ORGFAG        ORNE      'F'
018600991230     C     ORGFAG        ANDNE     'A'
018700991230     C                   SETON                                        4790
018800991230     C                   GOTO      ENDCTR
018900000111    3C                   ENDIF
019000000111     C*
019100000111     C* DEVE ESSERE UN TERMINAL DEL RAMO AZIENDALE
019200020529     C**         ORGDIT    IFNE O50PRA
019300020529     C**                   SETON                     4990
019400020529     C**                   GOTO ENDCTR
019500020529     C**                   ENDIF
019600991230     C**
019700991230     C* CONTROLLO SE E' UN TERMINAL
019800991230     C                   CLEAR                   DSLV55
019900991230     C                   MOVEL     'P'           D55TPT
020000000111     C                   MOVEL     VIDTER        D55LIN
020100991230     C                   MOVEL     DATEU         D55DRF
020200991230     C                   CALL      'FNLV55R'
020300991230     C                   PARM                    DSLV55
020400991230     C* TERMINAL ERRATO
020500991230    3C     D55ERR        IFNE      ' '
020600991230     C     D55LIN        ORNE      D55TFP
020700991230     C                   SETON                                        4790
020800991230     C                   GOTO      ENDCTR
020900991230    3C                   ENDIF
021000991230     C*
021100991230     C   90              CLEAR                   ORGDES
021200991230     C                   MOVEL     ORGDES        DESTER
021300991230     C                   MOVEL     ORGDES        PARTER           20
021400991230    2C                   ENDIF
021500991230    1C                   ENDIF
021600991230     C**
021700991230     C**
021800940118     C****  FILIALE  ****
021900991230    1C     VIDFIL        IFNE      0
022000940118     C*
022100991230    2C     VIDFIL        IFEQ      046
022200940118     C* NON SI PUO' IMMETTERE COME LINEA LA SEDE
022300940118     C                   SETON                                        4090
022400940118     C                   GOTO      ENDCTR
022500991230   X2C                   ELSE
022600940118     C*
022700940118     C* CONTROLLO VALIDITA' DELLA FILIALE IMMESSA
022800940118     C     VIDFIL        CHAIN     AZORG                              30
022900000111     C*
023000000111    3C     *IN30         IFEQ      *ON
023100991230     C     ORGFVA        ORNE      ' '
023200991230     C     ORGFAG        ORNE      'F'
023300991230     C     ORGFAG        ANDNE     'A'
023400991230     C                   SETON                                        4190
023500991230     C                   GOTO      ENDCTR
023600000111    3C                   ENDIF
023700940118     C*
023800991230     C   30              CLEAR                   ORGDES
023900991230     C                   MOVEL     ORGDES        DESFIL
024000991230     C                   MOVEL     ORGDES        PARDES           20
024100000111     C*
024200000111     C* DEVE ESSERE UNA LINEA DEL RAMO AZIENDALE
024300020529     C**         ORGDIT    IFNE O50PRA
024400020529     C**                   SETON                     5090
024500020529     C**                   GOTO ENDCTR
024600020529     C**                   ENDIF
024700000111     C*
024800000111     C* SE IMMESSO IL TERMINAL, IL P.O. DEVE AVERE QUEL TERMINAL
024900000111    3C     VIDTER        IFGT      0
025000000111     C                   CLEAR                   DSLV55
025100000111     C                   MOVEL     'P'           D55TPT
025200000111     C                   MOVEL     VIDFIL        D55LIN
025300000111     C                   MOVEL     DATEU         D55DRF
025400000111     C                   CALL      'FNLV55R'
025500000111     C                   PARM                    DSLV55
025600000111     C* TERMINAL ERRATO
025700000111    4C     D55ERR        IFNE      ' '
025800000111     C     VIDTER        ORNE      D55TFP
025900000111     C                   SETON                                        4890
026000000111     C                   GOTO      ENDCTR
026100000111    4C                   ENDIF
026200000111    3C                   ENDIF
026300000111    2C                   ENDIF
026400940118     C*
026500991230   X1C                   ELSE
026600940118     C* VIDFIL = 0
026700940118     C                   CLEAR                   DESFIL
026800940119     C                   CLEAR                   PARDES
026900991230    1C                   ENDIF
027000991230     C**
027100960514     C*
027200960514     C     VIDCLI        IFGT      *ZERO
027300940118     C*
027400960514     C*-----CHIAVE FILE PDC CNACO00F
027500960514     C     KEYACO        KLIST
027600960514     C                   KFLD                    CODUT
027700960514     C                   KFLD                    CLIITA
027800960514     C                   KFLD                    VIDCLI
027900960514     C     KEYACO        CHAIN     CNACO00F                           46
028000960514     C  N46              MOVEL     ACORAG        VIDCLD
028100960514     C   46              SETON                                        90
028200960514     C     VIDFIL        COMP      *ZERO                                  45
028300960514     C   45              SETON                                        90
028400960514     C   90              GOTO      ENDCTR
028500960514     C                   ENDIF
028600960514     C*
028700940118     C****  DATA SPEDIZIONE  DAL  ****
028800941122     C                   Z-ADD     VIDDSD        G02DAT
028900941122     C                   Z-ADD     *ZERO         G02INV
029000940118     C                   MOVEL     *BLANKS       G02ERR
029100941122     C                   CALL      'XSRDA8'
029200940118     C                   PARM                    WLBDAT
029300940118     C     G02ERR        IFEQ      '1'
029400940118     C                   SETON                                        4290
029500940118     C                   GOTO      ENDCTR
029600940118     C                   ENDIF
029700941122     C                   Z-ADD     G02DAT        VIDDSD
029800941122     C                   Z-ADD     G02INV        COMDSD            8 0
029900940118     C*
030000940118     C****  DATA SPEDIZIONE   AL  ****
030100940118     C     VIDDSA        IFNE      0
030200941122     C                   Z-ADD     VIDDSA        G02DAT
030300941122     C                   Z-ADD     *ZERO         G02INV
030400940118     C                   MOVEL     *BLANKS       G02ERR
030500941122     C                   CALL      'XSRDA8'
030600940118     C                   PARM                    WLBDAT
030700940118     C     G02ERR        IFEQ      '1'
030800940118     C                   SETON                                        4390
030900940118     C                   GOTO      ENDCTR
031000940118     C                   ENDIF
031100941122     C                   Z-ADD     G02DAT        VIDDSA
031200941122     C                   Z-ADD     G02INV        COMDSA            8 0
031300940118     C*
031400940118     C* DATA DAL NO MAGGIORE DATA AL
031500940118     C     COMDSD        IFGT      COMDSA
031600940118     C                   SETON                                        4490
031700940118     C                   GOTO      ENDCTR
031800940118     C                   ENDIF
031900940119     C*
032000940119     C                   ELSE
032100940119     C* VIDDSA = 0
032200940119     C                   CLEAR                   COMDSA
032300940118     C                   ENDIF
032400940118     C     ENDCTR        ENDSR
