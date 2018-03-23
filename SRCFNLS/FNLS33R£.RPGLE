000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941227     H* FNLS31R *-----------------------------------------------------*
000300930913     H*            - BOLLE PER OPARATORE -            FILTRO          *
000400930913     H*---------------------------------------------------------------*
000500941227     FFNLS31D   CF   E             WORKSTN
000600921002     FTABEL00F  IF   E           K DISK
000700930913     D MC              S              3  0 DIM(30)
000800950308     D*
000900950308     D DSLS01        E DS                  INZ
001000930913     D KPJBA         E DS
001100020429     D***                                     1   3 REM
001200020429     D***                                     4   6 REMFIL
001300970901     D* DS PER TRUL06R - CARICAMENTO £X
001400970901     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001500970901     D  LIN                    1     90  0
001600970901     D                                     DIM(30)
001700930913     D*----------------------------------
001800941227     D* PARAMETRI CONTROLLO/INVERSIONE/CALCOLO GIORNI DATA   - XSRDA8 -
001900930913     D*----------------------------------
002000921002     D WLBDAT          DS
002100941227     D  G02DAT                 1      8  0
002200941227     D  G02INV                 9     16  0
002300941227     D  G02ERR                17     17
002400941227     D  G02TGI                18     22  0
002500930913     D*----------------------------------
002600941227     D* PARAMETRI CALCOLO DATA DA GIORNI DATA                - XSRGI8 -
002700930913     D*----------------------------------
002800930913     D WGIDAT          DS
002900941227     D  GIODAT                 1      8  0
003000941227     D  GIOINV                 9     16  0
003100941227     D  GIOTGI                17     21  0
003200930913     D*----------------------------------
003300930913     D* PARAMETRI RICERCA DATI UTENTI                       - X§PARUT -
003400930913     D*----------------------------------
003500921002     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003600930913     D CNCR80        E DS
003700930913     D*----------------------------------
003800930913     D* DS GIORNI DI PULIZIA  - 5A-
003900930913     D*----------------------------------
004000930913     D DS5A          E DS
004100000606     D*----------------------------------
004200000608     D* DS PER FNLSC4R1 - CONTROLLO ELABORAZIONE PER POSTE - PARAMETRI:
004300000608     D* IC4LIN = LINEA
004400000608     D* IC4FLG = I=INTERATTIVA (1 RICHIESTA) .. NON UTILIZZATO B=BATCH
004500000606     D*----------------------------------
004600000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
004700930913     C*---------------------------------------------------------------*
004800930913     C*                    MAIN                                       *
004900930913     C* PROGRAMMA FILTRO STAMPA BOLLE PER OPERATORE                   *
005000930913     C*---------------------------------------------------------------*
005100921002     C*
005200930913     C* DEFINIZIONE VARIABILI, KLIST E PLIST
005300930913     C                   EXSR      DEFVAR
005400930913     C*
005500930913     C* CARICAMENTO FILIALI MECCANIZZATE
005600921002     C                   EXSR      CARTAB
005700900517     C*
005800900524     C                   Z-ADD     0             VIDDSD
005900900524     C                   Z-ADD     0             VIDDSA
006000950308     C                   Z-ADD     0             DS1AAL
006100930913     C*
006200900509     C     FOR01         TAG
006300941227     C                   EXFMT     LS31D01
006400930913     C*
006500930913     C* CMD3 - FINE LAVORO
006600900509     C   KC              GOTO      FINE
006700930913     C*
006800900524     C* CONTROLLO
006900900524     C                   EXSR      CONTR
007000941227     C  NKF
007100900524     COR 90              GOTO      FOR01
007200930913     C*
007300941227     C* CMD6 - ELABORAZIONE
007400950308     C*
007500950308     C                   MOVEL     *BLANKS       DS1FPG
007600950308     C                   Z-ADD     VIDLNP        DS1LNP
007700950308     C                   Z-ADD     VIDDSD        DS1GDA
007800950308     C                   Z-ADD     VIDDSA        DS1GAL
007900950308     C                   MOVEL     DSLS01        KPJBU
008000941227     C                   MOVEL     'LS32'        KCOAZ
008100900523     C                   CALL      'BCH10'
008200900523     C                   PARM                    KPJBA
008300900523     C*
008400000000     C     FINE          TAG
008500000000     C                   SETON                                        LR
008600930913     C*---------------------------------------------------------------*
008700930913     C*                    CONTR                                      *
008800930913     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
008900930913     C*---------------------------------------------------------------*
009000900524     C     CONTR         BEGSR
009100930913     C*
009200900524     C                   SETOFF                                       90
009300930913     C*
009400921002     C* DATA SPEDIZIONE DAL OBBLIGATORIA
009500921002     C                   MOVE      VIDDSD        G02DAT
009600921002     C                   MOVEL     *BLANK        G02ERR
009700941227     C                   CALL      'XSRDA8'
009800921002     C                   PARM                    WLBDAT
009900930913IF  1C     G02ERR        IFEQ      '1'
010000921002     C                   SETON                                        40  90
010100921002     C                   GOTO      ENDCTR
010200930913E   1C                   ENDIF
010300950308     C                   Z-ADD     G02INV        DS1ADA
010400941227     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
010500941227     C                   Z-ADD     G02DAT        VIDDSD
010600900524     C*
010700930913IF  1C     VIDDSA        IFNE      0
010800921002     C                   MOVE      VIDDSA        G02DAT
010900921002     C                   MOVEL     *BLANK        G02ERR
011000941227     C                   CALL      'XSRDA8'
011100921002     C                   PARM                    WLBDAT
011200930913IF  2C     G02ERR        IFEQ      '1'
011300921002     C                   SETON                                        41  90
011400921002     C                   GOTO      ENDCTR
011500930913E   2C                   ENDIF
011600950308     C                   Z-ADD     G02INV        DS1AAL
011700941227     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
011800941227     C                   Z-ADD     G02DAT        VIDDSA
011900930913     C*
012000930913     C* DATA DAL MINORE DATA AL
012100950308IF  2C     DS1ADA        IFGT      DS1AAL
012200930913     C                   SETON                                        4290
012300930913     C                   GOTO      ENDCTR
012400930913E   2C                   ENDIF
012500930913E   1C                   ENDIF
012600930913     C*
012700930913     C* REPERISCO DATA ULTIMO UTILIZZO PROGRAMMA DI PULIZIA BOLLE
012800941227     C                   MOVEL     'FNLS61R'     PGM
012900930913     C                   MOVEL     *BLANKS       DAT
013000930913     C                   MOVEL     *BLANKS       ERR
013100930913     C*
013200930915     C                   CALL      'TRUL49C'
013300930913     C                   PARM                    PGM              10
013400000103     C                   PARM                    DAT               8
013500930913     C                   PARM                    ERR               1
013600930913     C*
013700930913     C* PROGRAMMA IN USO: IMPOSSIBILE REPERIRE LA DATA
013800930913IF  1C     ERR           IFEQ      '1'
013900930913     C                   SETON                                        4490
014000930913     C                   GOTO      ENDCTR
014100930913X   1C                   ELSE
014200930913     C*
014300930913     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
014400930913     C* PULIZIA E TROVO LA DATA AL DI SOTTO DELLA QUALE NON SI POSSONO
014500930913     C* LANCIARE ELABORAZIONI PER MANCANZA DI DATI
014600950203     C                   CLEAR                   G02INV
014700930914     C                   MOVE      DAT           G02INV
014800930914     C*
014900930914     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
015000930914IF  2C     G02INV        IFEQ      *ZEROS
015100930914     C                   Z-ADD     *ZEROS        GIOINV
015200930914X   2C                   ELSE
015300930914     C*
015400930913     C                   Z-ADD     *ZEROS        G02DAT
015500930913     C                   Z-ADD     *ZEROS        G02TGI
015600930913     C                   MOVEL     '3'           G02ERR
015700941227     C                   CALL      'XSRDA8'
015800930913     C                   PARM                    WLBDAT
015900930913     C*
016000930913     C                   MOVEL     '5A'          COD
016100930913     C                   MOVEL     *BLANKS       KEY
016200930913     C                   MOVEL     '1'           KEY
016300930913     C     KTAB          CHAIN     TABEL00F                           30
016400930914IF  3C     *IN30         IFEQ      *ON
016500930913     C                   MOVEL     *BLANKS       DS5A
016600930914X   3C                   ELSE
016700930913     C                   MOVEL     TBLUNI        DS5A
016800930914E   3C                   ENDIF
016900930913     C*
017000930913     C                   SUB       §5ABLP        G02TGI
017100930913     C*
017200930913     C                   Z-ADD     *ZEROS        GIODAT
017300930913     C                   Z-ADD     *ZEROS        GIOINV
017400930913     C                   Z-ADD     G02TGI        GIOTGI
017500930913     C*
017600941227     C                   CALL      'XSRGI8'
017700930913     C                   PARM                    WGIDAT
017800930914E   2C                   ENDIF
017900930913     C*
018000930913     C* DATA DAL MAGGIORE DATA CALCOLATA
018100950308IF  2C     DS1ADA        IFLE      GIOINV
018200930913     C                   SETON                                        4590
018300930913     C                   GOTO      ENDCTR
018400930913E   2C                   ENDIF
018500930913E   1C                   ENDIF
018600921002     C*
018700930913     C* FILIALE FRA QUELLE CARICATE
018800930913IF  1C     VIDLNP        IFNE      0
018900000606      * .. CONTROLLO CHE RISPETTI LE REGOLE DI ELABORAZIONE (PER POSTE)
019000000608     C                   CLEAR                   FNLSC4
019100000608     C                   Z-ADD     VIDLNP        IC4LIN
019200000608     C                   MOVE      'I'           IC4FLG
019300000608     C                   MOVEL     FNLSC4        KPJBU
019400000608     C                   CALL      'FNLSC4R1'
019500000606     C                   PARM                    KPJBA
019600000608     C                   MOVEL     KPJBU         FNLSC4
019700000606      *
019800000608     C     OC4ESE        IFNE      'S'
019900000606     C                   SETON                                        4390
020000000606     C                   GOTO      ENDCTR
020100000606     C                   ENDIF
020200930913E   1C                   ENDIF
020300930913     C*
020400900524     C     ENDCTR        ENDSR
020500930913     C*---------------------------------------------------------------*
020600930913     C*                    CARTAB                                     *
020700930913     C* ROUTINE DI CARICAMENTO FILIALI MECCANIZZATE                   *
020800930913     C*---------------------------------------------------------------*
020900921002     C     CARTAB        BEGSR
021000970901     C                   CLEAR                   DSUL06
021100970901     C                   MOVE      '£1'          D06COD
021200970901     C                   MOVEL     SIMFEL        D06KEY
021300970901     C                   MOVEL     'L'           D06TLA
021400000103     C                   MOVEL     DSUL06        KPJBU
021500970901     C*
021600970901     C                   CALL      'TRUL06R'
021700970901     C                   PARM                    KPJBA
021800970901     C                   MOVEL     KPJBU         DSUL06
021900970901     C                   MOVEA     LIN           MC
022000921002     C*
022100020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
022200020429     C***        REM       IFEQ 'REM'
022300020429     C***        REMFIL    ANDGT*ZEROS
022400020429     C***                  MOVELREMFIL    VIDLNP
022500020429     C***                  MOVELREMFIL    DS1FGS
022600020429     C***                  SETON                     05
022700020429     C***                  ELSE
022800020429     C***                  MOVELSIMFEL    VIDLNP
022900020429     C***                  MOVELSIMFEL    DS1FGS
023000020429     C***                  END
023100020429     C                   Z-ADD     SIMPOU        VIDLNP
023200020429     C                   Z-ADD     SIMPOU        DS1FGS
023300020429     C     SIMTPP        IFEQ      '2'
023400020429     C     SIMTPP        OREQ      *BLANKS
023500020429     C                   SETON                                        05
023600020429     C                   ENDIF
023700921002     C*
023800930913     C                   ENDSR
023900930913     C*---------------------------------------------------------------*
024000930913     C*                    DEFVAR                                     *
024100930913     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
024200930913     C*---------------------------------------------------------------*
024300930913     C     DEFVAR        BEGSR
024400930913     C*
024500930913     C     *ENTRY        PLIST
024600930913     C                   PARM                    KPJBA
024700930913     C*
024800930913     C                   Z-ADD     1             CODUT
024900930913     C                   CALL      'X§PARUT'
025000930913     C                   PARM                    UT§DSE
025100930913     C                   MOVEL     RAGUT         RSUT             20
025200930913     C                   MOVEL     REC80         CNCR80
025300930913     C*
025400941227     C* ACCESSO TABEL00F
025500930913     C     KTAB          KLIST
025600930913     C                   KFLD                    CODUT
025700941227     C                   KFLD                    COD
025800941227     C                   KFLD                    KEY
025900930913     C*
026000941227     C* DEFINIZIONE CAMPI
026100941227     C     *LIKE         DEFINE    TBLCOD        COD
026200941227     C     *LIKE         DEFINE    TBLKEY        KEY
026300930913     C                   ENDSR
