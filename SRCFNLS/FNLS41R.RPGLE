000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020605      * FNLS41R *----------------------------------------------------*
000300020605      *        - ELABORAZIONI GIORNALIERE         FILTRO             *
000400020605      *--------------------------------------------------------------*
000500901018     FTABEL00F  IF   E           K DISK
000600941215     FFNLS41D   CF   E             WORKSTN
000700020605
000800901018     D L1              S              3  0 DIM(30)
000900920916     D PGM             S              8    DIM(199)
001000970131     D TBO             S              1    DIM(199)
001100920916     D PRN             S              3  0 DIM(199)
001200920916     D PR2             S              3  0 DIM(199)
001300020605
001400900618     D WLBDAT          DS
001500941215     D  G02DAT                 1      8  0
001600941215     D  G02INV                 9     16  0
001700941215     D  G02ERR                17     17
001800941215     D  G02TGI                18     22  0
001900020605
002000020605     D  COD            s                   like(TBLCOD)
002100020605     D  KEY            s                   like(TBLKEY)
002200020605
002300941215     D* DS PER TRUL06R - CARICAMENTO £1
002400020605     D trul06ds      E DS
002500020605     D  T06                    1     90  0 DIM(30)
002600950216     D DSLS01        E DS                  INZ
002700000000     D KPJBA         E DS
002800901018     D CNCR80        E DS
002900000112     D DS3L          E DS
003000020605     D ut§dse0f      E DS
003100020605     D fnlsc4ds      E DS
003200000000     C*---------------------------------------------------------------*
003300000000     C     *ENTRY        PLIST
003400000000     C                   PARM                    KPJBA
003500000000     C                   Z-ADD     1             CODUT
003600901018     C                   CALL      'X§PARUT'
003700020605     C                   PARM                    ut§dse0f
003800000000     C                   MOVEL     RAGUT         RSUT             20
003900901018     C                   MOVEL     REC80         CNCR80
004000000000     C*---------------------------------------------------------------*
004100941215     C* ACCESSO TABEL00F
004200920916     C     KTAB2         KLIST
004300920916     C                   KFLD                    CODUT
004400941215     C                   KFLD                    COD
004500901018     C     KTAB          KLIST
004600901018     C                   KFLD                    CODUT
004700941215     C                   KFLD                    COD
004800941215     C                   KFLD                    KEY
004900941215     C*---------------------------------------------------------------*
005000941215     C* CARICAMENTO TABELLE
005100901018     C                   EXSR      CARTAB
005200910327     C*
005300941215     C* PRENDO LA DATA DEL GIORNO DA TIME
005400941215     C                   TIME                    W0140            14 0
005500941215     C                   MOVE      W0140         UDATE8            8 0
005600941215     C*
005700950216     C                   Z-ADD     0             DS1LNP
005800950216     C                   Z-ADD     0             DS1ADA
005900941215     C                   Z-ADD     UDATE8        VIDDST
006000930416     C*
006100941215     C                   MOVE      UDATE8        G02DAT
006200930416     C                   MOVEL     *BLANK        G02ERR
006300941215     C                   CALL      'XSRDA8'
006400930416     C                   PARM                    WLBDAT
006500011018     C                   Z-ADD     G02INV        WIDDST            8 0
006600011018      *
006700000000     C     START         TAG
006800941215     C                   EXFMT     LS41D01
006900900618     C** FINE LAVORO
007000900618     C   KC              GOTO      FINE
007100900619     C*
007200941215     C* C O N T R O L L I
007300900619     C                   EXSR      CONTR
007400941215     C  NKF
007500900618     COR 90              GOTO      START
007600920916     C**
007700941215     C** CMD6 - ELABORAZIONI
007800920916     C**
007900950216     C* IMPOSTO LE COSTANTI NELLA DS DI PASSAGGIO PARAMETRI
008000970131     C* DATI COMUNI A TUTTE LE STAMPE
008100950216     C                   MOVEL     *BLANKS       DS1TBO
008200950216     C                   Z-ADD     *ZERO         DS1FGS
008300950216     C                   Z-ADD     SIMFEL        DS1FEL
008400950216     C                   Z-ADD     VIDDST        DS1GDA
008500950216     C                   Z-ADD     *ZERO         DS1GAL
008600950216     C                   Z-ADD     *ZERO         DS1AAL
008700970206     C                   CLEAR                   DS1PGM
008800970131     C* PROSPETTO NOLI: TIPO TABULATO
008900950216     C                   MOVEL     '2'           DS1FTM
009000950216     C                   MOVEL     'C'           DS1TFV
009100970205     C                   MOVEL     'N'           DS1FL2
009200970205     C                   MOVEL     'S'           DS1FL3
009300970205     C                   MOVEL     'N'           DS1FL4
009400970205     C                   Z-ADD     *ZERO         DS1NFV
009500970205     C*
009600970205     C* STASTISTICA ARRIVI
009700950216     C                   MOVEL     'T'           DS1FL1
009800970205     C* STATISTICHE
009900950405     C                   MOVEL     'S'           DS1RIS
010000970205     C* STAMPA FATTURE PREPAGATI
010100950216     C                   Z-ADD     *ZERO         DS1NFT
010200950216     C*
010300920916     C     VIDDST        IFNE      0
010400920916     C*
010500920916     C* CARICO TUTTI I PGM DA LANCIARE
010600941228     C                   Z-ADD     1             B                 5 0
010700920916     C                   MOVEL     '3L'          COD
010800920916     C     KTAB2         SETLL     TABEL
010900920916     C     KTAB2         READE     TABEL                                  30
011000920916     C*
011100920916     C     *IN30         DOWEQ     '0'
011200941215     C* ESCLUDO GLI ANNULLATI
011300920916     C     TBLFLG        IFEQ      ' '
011400920916     C                   MOVEL     TBLUNI        DS3L
011500940504     C* ESCLUDO QUELLI NON UTILIZZATI NELLE STAMPE GIORNALIERE
011600940504     C     §3LUSG        IFEQ      'S'
011700920916     C                   MOVEL     TBLKEY        PGM(B)
011800920916     C                   MOVEL     §3LPRN        PRN(B)
011900970131     C                   MOVEL     §3LTBO        TBO(B)
012000920916     C                   ADD       1             B
012100940504     C                   ENDIF
012200940504     C                   ENDIF
012300920916     C*
012400920916     C     KTAB2         READE     TABEL                                  30
012500920916     C                   END
012600920916     C**
012700920916     C** ORDINO LE STAMPE IN BASE AL PROGRESSIVO MESSO
012800920916     C                   MOVEA     PRN           PR2
012900920916     C                   SORTA     PR2
013000920916     C*
013100920916     C* CERCO LA PRIMA POSIZIONE PIENA
013200920916     C                   Z-ADD     1             B
013300940504     C     B             DOWLE     199
013400940504     C     PR2(B)        ANDEQ     0
013500920916     C                   ADD       1             B
013600920916     C                   END
013700920916     C*
013800970205     C* ELABORO SOLTANTO SE TROVATO ALMENO UN STAMPA GIORNALIERE
013900940504     C     PR2(199)      IFGT      0
014000940504     C*
014100920916     C     B             DOWLE     199
014200941228     C                   Z-ADD     1             C                 5 0
014300920916     C     PR2(B)        LOOKUP    PRN(C)                                 30
014400920916     C                   MOVEL     PGM(C)        KEY
014500920916     C     KTAB          CHAIN     TABEL                              30
014600920916     C                   MOVEL     TBLUNI        DS3L
014700920916     C*
014800950216     C                   MOVE      PGM(C)        DS1FPG
014900970205     C                   MOVE      TBO(C)        DS1TBO
015000970206     C                   MOVEL     §3LPGM        DS1PGM
015100920917     C                   MOVEL     §3LAZN        KCOAZ
015200000607      * CONTROLLO SE SIMFEL E' FILIALE POSTE
015300020605     C                   CLEAR                   fnlsc4ds
015400000608     C                   Z-ADD     SIMFEL        IC4LIN
015500000608     C                   MOVE      'I'           IC4FLG
015600020605     C                   MOVEL     fnlsc4ds      KPJBU
015700000608     C                   CALL      'FNLSC4R1'
015800000607     C                   PARM                    KPJBA
015900020605     C                   MOVEL     KPJBU         fnlsc4ds
016000000607      * -----------------
016100920925     C* LANCIO PER SIMFEL
016200000607      * -----------------
016300920925     C     §3LS1L        IFEQ      'S'
016400000607      * .. PER LANCIARE CONTROLLO I FLAG "TIPO FILIALE/ESECUZIONE" SU 3L
016500000608     C     OC4PT1        IFEQ      'S'
016600000607     C     §3LPT         ANDEQ     'N'
016700000607     C                   GOTO      NOLANC
016800000607     C                   ENDIF
016900000607      *
017000000620     C     OC4EXP        IFEQ      'S'
017100000607     C     §3LEXP        ANDEQ     'N'
017200000607     C                   GOTO      NOLANC
017300000607     C                   ENDIF
017400000607      *
017500000620     C     OC4DPD        IFEQ      'S'
017600000607     C     §3LDPD        ANDEQ     'N'
017700000607     C                   GOTO      NOLANC
017800000607     C                   ENDIF
017900020212      *
018000020212     C     OC4FED        IFEQ      'S'
018100020212     C     §3LFED        ANDEQ     'N'
018200020212     C                   GOTO      NOLANC
018300020212     C                   ENDIF
018400000607      *
018500950216     C                   MOVEL     SIMFEL        DS1LNP
018600950216     C                   CLEAR                   KPJBU
018700950216     C                   MOVEL     DSLS01        KPJBU
018800000406     C                   CALL      'BCH10'
018900000406     C                   PARM                    KPJBA
019000000607     C     NOLANC        TAG
019100920916     C                   END
019200000607      * ---------------------------------
019300920925     C* LANCIO PER 2 LIVELLO MECCANIZZATI
019400000607      * ---------------------------------
019500920925     C     §3LS2L        IFEQ      'S'
019600950216     C                   MOVEL     000           DS1LNP
019700950216     C                   CLEAR                   KPJBU
019800950216     C                   MOVEL     DSLS01        KPJBU
019900000406     C                   CALL      'BCH10'
020000000406     C                   PARM                    KPJBA
020100920925     C                   END
020200920916     C*
020300920916     C                   ADD       1             B
020400920916     C                   END
020500920916     C                   END
020600940504     C*
020700940504     C                   END
020800920916     C**
020900001027     C* SOTTOMETTO TRASMISSIONE STATISTICHE PARTENZE IN KSDIFFE
021000040923     C***                MOVEL     'LSD0'        KCOAZ
021100040923     C***                CALL      'BCH10'
021200040923     C***                PARM                    KPJBA
021300001027     C* SOTTOMETTO TRASMISSIONE STATISTICHE PARTENZE IN KSSPECL
021400040923     C***                MOVEL     'LSC6'        KCOAZ
021500040923     C***                CALL      'BCH10'
021600040923     C***                PARM                    KPJBA
021700020605      *
021800000000     C     FINE          TAG
021900020605     C                   eval      *inLR = *on
022000900618     C*
022100900618     C* CONTROLLO CAMPI FORMATO -------------------------------------*
022200900618     C     CONTR         BEGSR
022300020605     C                   eval      *IN90 = *off
022400900618     C* DATA STAMPE
022500900618     C     VIDDST        IFNE      0
022600920916     C                   MOVE      VIDDST        G02DAT
022700920916     C                   MOVEL     *BLANK        G02ERR
022800941215     C                   CALL      'XSRDA8'
022900920916     C                   PARM                    WLBDAT
023000920916     C     G02ERR        IFEQ      '1'
023100920916     C                   SETON                                        40  90
023200920916     C                   GOTO      ENDCTR
023300920916     C                   END
023400950216     C                   Z-ADD     G02INV        DS1ADA
023500950120     C                   Z-ADD     G02DAT        VIDDST
023600900618     C                   END
023700010316      * CONTROLLO CHE LA DATA SIA VALIDA
023800010316     C     VIDDST        IFNE      0
023900011018     C     DS1ADA        IFGT      WIDDST
024000011018     C     DS1ADA        ORLT      19400101
024100011018     C     DS1ADA        ORGT      20391231
024200010316     C                   SETON                                        40  90
024300010316     C                   GOTO      ENDCTR
024400010316     C                   END
024500010316     C                   END
024600020605      *
024700900618     C     ENDCTR        ENDSR
024800901018     C*
024900901018     C*--- CARICO TABELLE --------------------------------------------*
025000901018     C     CARTAB        BEGSR
025100941215     C***
025200941215     C* CARICO TABELLA FILIALI GESTITE £1
025300941215     C***
025400020605     C                   CLEAR                   trul06ds
025500941215     C                   MOVE      '£1'          D06COD
025600941215     C                   MOVEL     SIMFEL        D06KEY
025700941215     C                   MOVEL     'L'           D06TLA
025800020605     C                   MOVEL     trul06ds      KPJBU
025900941215     C*
026000941215     C                   CALL      'TRUL06R'
026100941215     C                   PARM                    KPJBA
026200020605     C                   MOVEL     KPJBU         trul06ds
026300950314     C                   MOVEA     T06           L1
026400930422     C*
026500901018     C                   ENDSR
026600950120     C*
026700020605     C*---------------------------------------------------------------*
