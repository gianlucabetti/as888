000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200961015     H* FNLS14R *-----------------------------------------------------*
000300960321     H*-----------------*                                             *
000400961015     H*           ELENCO VARIZIONI VOLUME                 - FILTRO -  **
000500960321     H*---------------------------------------------------------------*
000600961015     FFNLS14D   CF   E             WORKSTN
000700960321     FAZORG01L  IF   E           K DISK
000800960321     FCNACO00F  IF   E           K DISK
000900960321     D*
001000960321     D* DEFINIZIONE SCHIERE
001100960321     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
001200941223     D*
001300961015     D* PASSAGGIO DATI ALL'ELABORAZIONE BATCH             - FNLS15R -
001400961015     D DSLS15        E DS                  EXTNAME(FNLS15DS) INZ
001500960321     D*
001600960321     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
001700960321     D DSLV50        E DS                  EXTNAME(FNLV50DS)
001800960327     D*
001900920925     D WLBDAT          DS
002000941223     D  G02DAT                 1      8  0
002100941223     D  G02INV                 9     16  0
002200941223     D  G02ERR                17     17
002300941223     D  G02TGI                18     22  0
002400900517     D KPJBA         E DS
002500020429     D***                                     1   3 REM
002600020429     D***                                     4   6 REMFIL
002700920925     D CNCR80        E DS
002800920925     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900960321     D  TCU                  398    697
003000960321     D                                     DIM(50)
003100960321     D  KCU                  698    847P 0
003200960321     D                                     DIM(50)
003300960321     D                                     PACKEVEN
003400960321     D TCUDS           DS
003500960321     D  F1                     1      1
003600960321     D  F3                     3      3
003700960321     D  F2                     2      2
003800960321     D  F4                     4      4
003900960321     D  F56                    5      6
004000960321     C*****************************************************************
004100960321     C* RIEPILOGO INDICATORI
004200960321     C*****************************************************************
004300020429     C* 05    - SONO UN SECONDO LIVELLO
004400960321     C* 06    - CAMBIO FILIALE IN GESTIONE PER CMD18
004500960321     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
004600960327     C* 30/31 - DI COMODO
004700961015     C* 40/41 - ERRORI
004800961015     C* 43/45 - ERRORI
004900960321     C* 90    - INDICATORE DI ERRORE GENERICO
005000960321     C*****************************************************************
005100000000     C     *ENTRY        PLIST
005200000000     C                   PARM                    KPJBA
005300900511     C*---------------------------------------------------------------*
005400960321     C* PULIZIA CAMPI VIDEATA
005500960321     C                   CLEAR                   V1CKSC
005600960321     C                   CLEAR                   V1DKSC
005700960321     C                   CLEAR                   V1CDVD
005800960321     C                   CLEAR                   V1CDVA
005900960321     C                   CLEAR                   V1CLNA
006000960321     C*
006100900509     C     FOR01         TAG
006200961015     C                   EXFMT     LS14D01
006300960321     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
006400960321     C                   CLEAR                   V1CMSG
006500960321     C                   SETOFF                                       28
006600961015     C                   SETOFF                                       4041
006700960321     C                   SETOFF                                       434445
006800941223     C*
006900960321     C* CMD3 - FINE LAVORO
007000900509     C   KC              GOTO      FINE
007100960321     C*
007200960321     C** CMD18- CAMBIO FILIALE
007300960321     C     *INKS         IFEQ      *ON
007400960321     C                   SETON                                        0640
007500960321     C                   GOTO      FOR01
007600960321     C                   ENDIF
007700960321     C*
007800960321     C* 06 ON  - CONTROLLO FILIALE IN GESTIONE
007900960321    2C     *IN06         IFEQ      *ON
008000960321     C                   MOVEL     V1CLNP        WFIL
008100960321     C                   EXSR      CTRFGS
008200960321     C   90              GOTO      FOR01
008300960321    2C                   ENDIF
008400960321     C*
008500960321     C* CONTROLLI FORMATO
008600960321     C                   EXSR      CTRD01
008700941223     C  NKF
008800900524     COR 90              GOTO      FOR01
008900960321     C*
009000960321     C* CMD6 - ELABORAZIONE
009100961015     C                   Z-ADD     V1CLNP        D15LNP
009200961015     C                   Z-ADD     V1CKSC        D15KSC
009300961015     C                   Z-ADD     WDVD          D15DVD
009400961015     C                   Z-ADD     WDVA          D15DVA
009500961015     C                   Z-ADD     V1CLNA        D15LNA
009600961015     C                   Z-ADD     SIMFEL        D15FEL
009700960321     C*
009800960327     C                   CLEAR                   KPJBU
009900961015     C                   MOVEL     DSLS15        KPJBU
010000961015     C                   MOVEL     'LS15'        KCOAZ
010100900523     C                   CALL      'BCH10'
010200900523     C                   PARM                    KPJBA
010300960401     C*
010400920925     C     FINE          TAG
010500000000     C                   SETON                                        LR
010600900524     C*
010700960321     C*--- CONTROLLO FILIALE IN GESTIONE -----------------------------*
010800960321     C     CTRFGS        BEGSR
010900960321     C                   SETOFF                                       90
011000960321     C*
011100960321     C                   CLEAR                   DSLV50
011200960321     C                   MOVEL     KNMUS         D50PRU
011300960321     C                   MOVEL     WFIL          D50FGS
011400960321     C                   CALL      'FNLV50R'
011500960321     C                   PARM                    DSLV50
011600960321     C*
011700960321    1C     D50ERR        IFNE      ' '
011800960321     C                   MOVEL     D50MSG        V1CMSG
011900960321     C                   SETON                                        402890
012000960321     C                   GOTO      ENDCTF
012100960321    1C                   ENDIF
012200960321     C*
012300960321     C     ENDCTF        ENDSR
012400960321     C*
012500960321     C*--- CONTROLLI FORMATO -----------------------------------------*
012600960321     C     CTRD01        BEGSR
012700960321     C                   SETOFF                                       9006
012800960321     C*
012900960321     C****  CODICE CLIENTE  ****
013000960321    1C     V1CKSC        IFGT      0
013100960327     C                   CLEAR                   V1DKSC
013200960321     C*
013300960321     C* CONTROLLO VALIDITA' CODICE CLIENTE
013400960321     C     KCLI          CHAIN     CNACO000                           30
013500960321    2C  N30ACOFLG        IFNE      ' '
013600960321     C                   SETON                                        30
013700960321    2C                   ENDIF
013800960321     C*
013900961016     C* SE NON ESISTE NEL P.d.C. ---> ERRORE
014000960321    2C     *IN30         IFEQ      *ON
014100961016     C                   MOVEL     MSG(2)        V1CMSG
014200961016     C                   SETON                                        412890
014300961016     C                   GOTO      ENDCTR
014400960327   X2C                   ELSE
014500960327     C* DECODIFICA CODICE CLIENTE
014600960327     C                   MOVEL     ACORAG        V1DKSC
014700960321    2C                   ENDIF
014800960321     C*
014900960321   X1C                   ELSE
015000960321     C* SE CODICE CLIENTE A ZERO ---> RICERCA ALFABETICA
015100960321     C*
015200960321    2C     V1DKSC        IFNE      *BLANKS
015300960327     C                   MOVEL     KCI           PARKCI
015400960321     C                   MOVEL     V1DKSC        RAGKSC
015500960321     C                   MOVEL     V1CRSU        PARDUT           30
015600960321     C* PARSTA = 9 --> ESCLUDO ANNULLATI
015700960321     C                   Z-ADD     9             PARSTA
015800981109     C                   CLEAR                   PARDIT
015900000907     C                   Z-ADD     1             PAXNUM
016000980702     C* SOLO CLIENTI DEL S.I.
016100981109     C***                  MOVELO50PRA    PARDIT
016200000907     C                   CALL      'XALFA3BR'
016300960321     C                   PARM                    PARDUT
016400960321     C                   PARM                    CODUT
016500960321     C                   PARM                    RAGKSC
016600960327     C                   PARM                    PARKCI
016700960321     C                   PARM                    PARSTA            1 0
016800980702     C                   PARM                    PARFLR           90
016900980702     C                   PARM                    PARDIT            3
017000000907     C                   PARM                    PAXNUM            2 0
017100000907     C                   PARM                    PAXKCM           80
017200000907     C                   PARM                    PAXKSM          140
017300000907     C                   PARM                    PAXKDM           60
017400960321     C     PARSTA        IFNE      -1
017500000907     C                   MOVEL     PAXKSM        V1CKSC
017600960321     C                   MOVEL     RAGKSC        V1DKSC
017700960321     C                   ENDIF
017800960321     C*
017900960321     C                   SETON                                        90
018000960321     C                   GOTO      ENDCTR
018100960321    2C                   ENDIF
018200960321    1C                   ENDIF
018300960321     C*
018400960321     C****  DATA VARIAZIONE DAL  ****
018500961015    1C     V1CDVD        IFGT      0
018600960321     C                   MOVE      V1CDVD        G02DAT
018700960321     C                   MOVEL     *ZERO         G02INV
018800960321     C                   MOVEL     *BLANKS       G02ERR
018900941223     C                   CALL      'XSRDA8'
019000920925     C                   PARM                    WLBDAT
019100920925     C     G02ERR        IFEQ      '1'
019200960321     C                   MOVEL     MSG(4)        V1CMSG
019300960321     C                   SETON                                        432890
019400920925     C                   GOTO      ENDCTR
019500960321     C                   ENDIF
019600960321     C                   Z-ADD     G02DAT        V1CDVD
019700960321     C                   Z-ADD     G02INV        WDVD
019800900524     C*
019900960321     C****  DATA VARIAZIONE  AL  ****
020000960326    2C     V1CDVA        IFNE      0
020100960321     C                   MOVE      V1CDVA        G02DAT
020200960321     C                   MOVEL     *ZERO         G02INV
020300960321     C                   MOVEL     *BLANKS       G02ERR
020400941223     C                   CALL      'XSRDA8'
020500920925     C                   PARM                    WLBDAT
020600920925     C     G02ERR        IFEQ      '1'
020700960321     C                   MOVEL     MSG(4)        V1CMSG
020800960321     C                   SETON                                        442890
020900920925     C                   GOTO      ENDCTR
021000960321     C                   ENDIF
021100960321     C                   Z-ADD     G02DAT        V1CDVA
021200960321     C                   Z-ADD     G02INV        WDVA
021300941223     C*
021400900524     C* DATA DAL NO MAGGIORE DATA AL
021500960326    3C     WDVD          IFGT      WDVA
021600960321     C                   MOVEL     MSG(5)        V1CMSG
021700960321     C                   SETON                                        432890
021800900524     C                   GOTO      ENDCTR
021900960326    3C                   ENDIF
022000960321     C*
022100960321     C* SE INSERITA DATA AL OCCORRE INSERIRE ANCHE DATA DAL
022200960326    3C     WDVD          IFEQ      0
022300960321     C                   MOVEL     MSG(4)        V1CMSG
022400960321     C                   SETON                                        432890
022500960321     C                   GOTO      ENDCTR
022600960326    3C                   ENDIF
022700960326    2C                   ENDIF
022800961025    1C                   ENDIF
022900941223     C*
023000960321     C****  FILIALE CHE HA ESEGUITO LA VARIAZIONE  ****
023100961025    1C     V1CLNA        IFGT      0
023200960321     C     V1CLNA        CHAIN     AZORG01L                           30
023300960321     C     *IN30         IFEQ      *ON
023400960321     C     ORGFVA        ORNE      ' '
023500960321     C                   MOVEL     MSG(6)        V1CMSG
023600960321     C                   SETON                                        452890
023700960321     C                   GOTO      ENDCTR
023800960321     C                   ENDIF
023900961025    1C                   ENDIF
024000960321     C*
024100900524     C     ENDCTR        ENDSR
024200920925     C*
024300960321     C*--- SR INIZIALE -----------------------------------------------*
024400960321     C     *INZSR        BEGSR
024500960321     C****
024600960321     C** KLIST
024700960321     C****
024800960321     C* ACCESSO CNACO00F
024900960321     C     KCLI          KLIST
025000960321     C                   KFLD                    CODUT
025100960321     C                   KFLD                    KCI
025200960321     C                   KFLD                    V1CKSC
025300960321     C****
025400960321     C** DEFINIZIONE CAMPI
025500960321     C****
025600960321     C     *LIKE         DEFINE    V1CLNP        WFIL
025700961015     C     *LIKE         DEFINE    D15DVD        WDVD
025800961015     C     *LIKE         DEFINE    D15DVA        WDVA
025900960321     C     *LIKE         DEFINE    ACORAG        RAGKSC
026000960327     C     *LIKE         DEFINE    KCI           PARKCI
026100960321     C*
026200960321     C                   Z-ADD     1             CODUT
026300960321     C                   CALL      'X§PARUT'
026400960321     C                   PARM                    UT§DSE
026500960321     C                   MOVEL     RAGUT         V1CRSU
026600960321     C                   MOVEL     REC80         CNCR80
026700960321     C*
026800960321     C****  RICERCA CAPOCONTI  ****
026900960321    1C                   DO        50            X                 2 0
027000960321     C                   MOVE      TCU(X)        TCUDS
027100960321     C     F56           CABNE     'CG'          END1
027200960321     C     F4            COMP      '1'                                    21
027300960321     C     F4            COMP      '2'                                    22
027400960321     C     F4            COMP      '3'                                    23
027500960321     C     F4            COMP      '6'                                    27
027600960321     C** 1 CLIENTI   21
027700960321     C** 2 FORNITORI 22
027800960321     C** 3 AGENTI    23
027900960321     C     F3            COMP      '0'                                242425
028000960321     C     F3            COMP      'I'                                    26
028100960321     C** 0 ITALIA   25
028200960321     C** 1 ESTERO   24
028300960321     ** I CAPO CONTO IVA
028400960321     C   21
028500960321     CAN 24              Z-ADD     KCU(X)        KCE               4 0
028600960321     C   21
028700960321     CAN 25              Z-ADD     KCU(X)        KCI               4 0
028800960321     C   22
028900960321     CAN 24              Z-ADD     KCU(X)        KFE               4 0
029000960321     C   22
029100960321     CAN 25              Z-ADD     KCU(X)        KFI               4 0
029200960321     C   23
029300960321     CAN 24              Z-ADD     KCU(X)        KAE               4 0
029400960321     C   23
029500960321     CAN 25              Z-ADD     KCU(X)        KAI               4 0
029600960321     C   26              Z-ADD     KCU(X)        KIVA              4 0
029700960321     C   27              Z-ADD     KCU(X)        KBNA              4 0
029800960321     C     END1          TAG
029900960321    1C                   END
030000960321     C*
030100960321     C* SPENGO GLI INDICATORI USATI PER LA RICERCA CAPOCONTI
030200960321     C                   SETOFF                                       212223
030300960321     C                   SETOFF                                       242526
030400960321     C                   SETOFF                                       27
030500941223     C*
030600020429     C****  VEDO SE SONO PRIMO O SECONDO LIVELLO ***
030700020429    1C***        REM       IFEQ 'REM'
030800020429     C***        REMFIL    ANDGT*ZEROS
030900020429     C***                  MOVELREMFIL    V1CLNP
031000020429     C***                  SETON                     05
031100020429   X1C***                  ELSE
031200020429     C***                  MOVELSIMFEL    V1CLNP
031300020429    1C***                  ENDIF
031400020429     C                   Z-ADD     SIMPOU        V1CLNP
031500020429     C     SIMTPP        IFEQ      '2'
031600020429     C     SIMTPP        OREQ      *BLANKS
031700020429     C                   SETON                                        05
031800020429     C                   ENDIF
031900920925     C                   ENDSR
032000941223     C*---------------------------------------------------------------*
032100961025**  MSG
032200960321Linea di partenza errata                                                      1
032300960321Cliente Inesistente                                                           2
032400961015                                                                              3
032500960321Data errata                                                                   4
032600960321La data variazione DAL non puo' essere maggiore della data AL                 5
032700961025Punto Operativo inesistente                                                   6
