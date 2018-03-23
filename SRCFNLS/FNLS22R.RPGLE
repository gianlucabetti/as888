000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200951108     H* FNLS22R *----------------------------------------------------*
000300900522     H*         - RISTAMPA BORDERO'
000400000000     H*--------------------------------------------------------------*
000500940919     FFNLS22D   CF   E             WORKSTN
000600020807     FFNBLP79L  IF   E           K DISK
000700900509     FAZORG01L  IF   E           K DISK
000800940919     FFNFGV01L  IF   E           K DISK
000900000427     FFNFGW01L  IF   E           K DISK
001000950112     FFNLV22P   O    F  132        PRINTER OFLIND(*INOE)
001100940919     D*
001200900530     D TES             S             28    DIM(1) CTDATA PERRCD(1)
001300940919     D*
001400900522     D PARAM           DS
001500900522     D  VIDLNA                 1      3  0
001600900522     D  VIDFLP                 4      6  0
001700941006     D  VIDTSP                 7      7    INZ
001800020807     D* P.O. in Gestione
001900020807     D  VIDpog                 8     10  0 inz
002000100701     d* ccm non usato in ristampa
002100100701     D  VIDccm                11     17    inz
002200941006     D* PGM DI STAMPA ETICHETTA BORDERO'
002300941006     D  PGMSE                228    237
002400941006     D*
002500941006     D  VIDLNP               238    240  0 INZ
002600940919     D  VIDNFV               241    245  0
002700941006     D* AZZERO LA DATA BORDERO' PERCHE' NON SERVIRA' (E' NELLA PARAM)
002800941006     D  COMDBR               247    254  0 INZ
002900900625     D  FLG                  255    255
003000941007     D***
003100941006     D PARAM1          DS
003200941006     D* PGM STAMPA ETICHETTE
003300941006     D  PA1PSE               237    246
003400941006     D***
003500130520     d
003600130520     D* CHIAMATA AL PGM DI INTERROGAZ.FV PARTENZA             -FNLST3R-
003700130520     D fnlst3ds        DS
003800130520     D  PA5FLG                 1      1
003900130520     D  PA5FL2                 2      2
004000130520     D  PA5NFV                 3      5P 0
004100130520     D  PA5DIN                 6     10P 0
004200130520     D  PA5DFI                11     15P 0
004300130520     D  PA5TFV                16     16
004400130520     D  PA5FGS               242    244  0
004500951108     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
004600951108     D*   SELEZIONATI ---> SE NON UTILIZZATO NON LO DEFINISCO A PGM
004700951108     D***                                 P  17 2410FVA
004800930324     D WLBDAT          DS
004900940919     D  G02DAT                 1      8  0
005000940919     D  G02INV                 9     16  0
005100940919     D  G02ERR                17     17
005200940919     D  G02TGI                18     22  0
005300930324     D                 DS
005400930324     D  FGVFFV                 1    240
005500930324     D  FGVFF2               241    447
005600000427     D  FGWFF3               448    687
005700000427     D  FGWFF4               688    897
005800000427     D  VUOTO                898    900
005900000427     D  FFV                    1    900
006000000427     D                                     DIM(300)
006100930324     D                 DS
006200930324     D  FGVFLP                 1    240
006300930324     D  FGVFL2               241    447
006400000427     D  FGWFL3               448    687
006500000427     D  FGWFL4               688    897
006600000427     D  VUOTO2               898    900
006700000427     D  FLP                    1    900
006800000427     D                                     DIM(300)
006900900511     D KPJBA         E DS
007000000614     D CNCR80        E DS
007100000614     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007200000000     C*---------------------------------------------------------------*
007300000000     C     *ENTRY        PLIST
007400000000     C                   PARM                    KPJBA
007500920124     C                   MOVEL     KPJBU         PARAM1
007600000614      * RECUPERO SIMFEL E R.S.UTENTE
007700000614     C                   Z-ADD     1             CODUT
007800000614     C                   CALL      'X§PARUT'
007900000614     C                   PARM                    UT§DSE
008000000614     C                   MOVEL     RAGUT         VIDRSU
008100000614     C                   MOVEL     REC80         CNCR80
008200941006     C* PGM STAMPA ETICHIETTA
008300941006     C                   MOVEL     PA1PSE        PGMSE
008400900511     C*---------------------------------------------------------------*
008500020807     ***C     KBLP1         KLIST
008600020807     C     K03BLP79      klist
008700020807     C                   kfld                    SIMfel
008800940919     C                   KFLD                    VIDNFV
008900940919     C                   KFLD                    VIDLNA
009000020807     C     K02FGV01      klist
009100020807     C                   kfld                    VIDnfv
009200020807     C                   kfld                    SIMfel
009300900522     C*
009400900530     C                   MOVE      TES(1)        VIDTES
009500941006     C*
009600910610     C                   Z-ADD     0             VIDLNA
009700940919     C                   MOVEL     *BLANKS       DESLNA
009800940919     C                   Z-ADD     0             VIDNFV
009900940919     C                   Z-ADD     0             VIDDFV
010000910704     C                   MOVEL     'S'           SBRSN
010100910704     C                   MOVEL     'N'           SETSN
010200940919     C*
010300940919     C                   MOVEL     'S'           WRIEM             1
010400941006     C*
010500900509     C     FOR01         TAG
010600940919     C     WRIEM         IFEQ      'S'
010700940919     C                   WRITE     LS22T01
010800940919     C                   MOVEL     *BLANKS       WRIEM
010900940919     C                   ENDIF
011000941006     C*
011100940919     C                   EXFMT     LS22D01
011200940919     C*
011300941006     C** CMD3 - FINE LAVORO
011400900509     C   KC              GOTO      FINE
011500900509     C* CONTROLLI FORMATO1
011600900509     C                   EXSR      CONTR
011700940921     C  NKF
011800900522     COR 90              GOTO      FOR01
011900910918     C* CMD1 - ELABORAZIONE
012000910918     C     SBRSN         IFEQ      'S'
012100910704     C     SETSN         IFEQ      'N'
012200910704     C                   MOVEL     '1'           FLG
012300910704     C                   END
012400910704     C                   END
012500910704     C*
012600910918     C     SBRSN         IFEQ      'N'
012700910704     C     SETSN         IFEQ      'S'
012800910704     C                   MOVEL     '2'           FLG
012900910704     C                   END
013000910704     C                   END
013100910704     C*
013200910918     C     SBRSN         IFEQ      'S'
013300910704     C     SETSN         IFEQ      'S'
013400910704     C                   MOVEL     '3'           FLG
013500910704     C                   END
013600910704     C                   END
013700910918     C* VEDO SE LANCIARE 2 O UNA COPIA
013800910918     C     ORGFAG        IFEQ      'A'
013900910918     C                   MOVEL     ' 2'          KNMCP
014000910918     C                   ELSE
014100910918     C                   MOVEL     ' 1'          KNMCP
014200910918     C                   END
014300930324     C*
014400020807     ***C     VIDNFV        CHAIN     FNFGV01L                           31
014500020807     ***C     VIDNFV        CHAIN     FNFGW01L                           32
014600020807     C     K02FGV01      CHAIN     FNFGV01L                           31
014700020807     C     K02FGV01      CHAIN     FNFGW01L                           32
014800000427     C     *IN32         IFEQ      *ON
014900000427     C     FGWATB        ORNE      ' '
015000000427     C                   CLEAR                   FGWFF3
015100000427     C                   CLEAR                   FGWFF4
015200000427     C                   CLEAR                   FGWFL3
015300000427     C                   CLEAR                   FGWFL4
015400000427     C                   ENDIF
015500930325     C*
015600930325     C* NON C'E' FOGLIO VIAGGIO:
015700930325     C*  SE BLPFLP >< 0 E' QUELLO LO SCARICO
015800930325     C*  ALTRIMENTI E' IL TERMINAL DELLA FILIALE DI ARRIVO
015900930325     C   31BLPFLP        IFEQ      0
016000930325     C                   Z-ADD     BLPTFA        VIDFLP
016100930325     C                   ELSE
016200930325     C                   MOVEL     BLPFLP        VIDFLP
016300930325     C                   END
016400930325     C*
016500930325     C* C'E' FOGLIO VIAGGIO:
016600930325     C*  SE BLPLNA = LNA FOGLIO LO SCARICO E' SE STESSO
016700930325     C*  ALTRIMENTO QUELLO INDICATO NELLE FILIALI DI SCARICO
016800930325     C  N31BLPLNA        IFEQ      FGVLNA
016900930325     C                   MOVEL     FGVLNA        VIDFLP
017000930325     C                   ELSE
017100930325     C*
017200930325     C                   MOVEL     BLPLNA        ALF3              3
017300930325     C                   Z-ADD     1             X                 3 0
017400930325     C     ALF3          LOOKUP    FFV(X)                                 31
017500930325     C   31              MOVEL     FLP(X)        VIDFLP
017600930325     C  N31              MOVEL     BLPLNA        VIDFLP
017700930325     C                   END
017800020807     C* P.O. Gestione
017900020807     C                   z-add     SIMfel        VIDpog
018000930325     C*
018100940315     C* LANCIO STAMPA ETICHETTE
018200940315     C     FLG           IFNE      '1'
018300940315     C                   MOVEL     PARAM         KPJBU
018400940919     C                   CALL      'FNLSD5R'
018500940315     C                   PARM                    KPJBA
018600940315     C                   ENDIF
018700940315     C*
018800940315     C* LANCIO BORDERO'
018900940315     C     FLG           IFEQ      '1'
019000940315     C     FLG           OREQ      '3'
019100910918     C                   MOVEL     PARAM         KPJBU
019200940919     C                   MOVEL     'LS21'        KCOAZ
019300900522     C                   CALL      'BCH10'
019400900522     C                   PARM                    KPJBA
019500940315     C                   ENDIF
019600940919     C*
019700000000     C     FINE          TAG
019800000000     C                   SETON                                        LR
019900940919     C**************************************************************************
020000940919     C*    CONTROLLI FORMATO1
020100940919     C**************************************************************************
020200900509     C     CONTR         BEGSR
020300940919     C*
020400900509     C                   SETOFF                                       90
020500940919     C*
020600940919     C* INTERROGAZIONE FOGLI VIAGGIO
020700940919     C     *INKG         IFEQ      *ON
020800130520     C                   CLEAR                   fnlst3ds
020900130520     C                   MOVEL     'P'           PA5FLG
021000940919     C* DATA FINALE: DATA DEL GIORNO
021100940919     C                   TIME                    WTIME            14 0
021200951108     C                   Z-ADD     WTIME         UDATE8            8 0
021300951108     C                   Z-ADD     UDATE8        PA5DFI
021400000614     C                   Z-ADD     SIMFEL        PA5FGS
021500130520     C                   MOVEL     fnlst3ds      KPJBU
021600130520     C                   CALL      'FNLST3R'
021700940919     C                   PARM                    KPJBA
021800130520     C                   MOVEL     KPJBU         fnlst3ds
021900940919     C                   MOVEL     PA5NFV        VIDNFV
022000940919     C                   SETON                                        90
022100940919     C                   MOVEL     'S'           WRIEM
022200940919     C                   GOTO      ENDCTR
022300940919     C                   ENDIF
022400940919     C*
022500900522     C* LINEA ARRIVO
022600940919     C                   MOVEL     *BLANKS       DESLNA
022700900522     C     VIDLNA        IFEQ      0
022800900509     C                   SETON                                        4090
022900900509     C                   GOTO      ENDCTR
023000900509     C                   ELSE
023100940919     C     VIDLNA        CHAIN     AZORG01L                           30
023200900509     C   30              SETON                                        4090
023300900509     C   30              GOTO      ENDCTR
023400940919     C                   MOVEL     ORGDES        DESLNA
023500900511     C                   END
023600940919     C*
023700940919     C* NUMERO FOGLIO VIAGGIO
023800940919     C     VIDNFV        IFEQ      0
023900900703     C                   SETON                                        4390
024000900703     C                   GOTO      ENDCTR
024100900703     C                   END
024200940919     C*
024300940919     C* CONTROLLO CHE CI SIA ALMENO UNA BOLLA CON QUEL NUMERO
024400020807     ***C     KBLP1         CHAIN     FNBLP29L                           41
024500020807     C     K03BLP79      chain     FNBLP79L                           41
024600900522     C   41              SETON                                        90
024700900522     C   41              GOTO      ENDCTR
024800940919     C*
024900940919     C* EMETTO A VIDEO LA DATA FOGLIO VIAGGIO
025000020807     ***C     VIDNFV        CHAIN     FNFGV01L                           31
025100020807     C     K02FGV01      CHAIN     FNFGV01L                           31
025200940919     C                   MOVEL     FGVDFV        G02INV
025300940919     C                   MOVEL     *ZERO         G02DAT
025400940919     C                   MOVEL     '3'           G02ERR
025500940919     C                   CALL      'XSRDA8'
025600940919     C                   PARM                    WLBDAT
025700940919     C                   MOVEL     G02DAT        VIDDFV
025800900703     C*
025900910704     C* SE ENTRAMBI SCELTE NO ERRORE
026000910704     C     SBRSN         IFEQ      'N'
026100930324     C     SETSN         ANDEQ     'N'
026200910704     C                   SETON                                        9044
026300910704     C                   GOTO      ENDCTR
026400910704     C                   END
026500940919     C*
026600900509     C     ENDCTR        ENDSR
026700950112     OFNLV22P   T    LRNLR
026800940919     C**************************************************************************
026900900530**
027000900530 ***  RISTAMPA BORDERO'  ***
