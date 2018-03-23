000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960321     H* FNLS07R *-----------------------------------------------------*
000300960321     H*-----------------*                                             *
000400960321     H*  LETTERA COMUNICAZIONE VARIAZIONE PESO AI MITTENTI  -FILTRO-  **
000500960321     H*---------------------------------------------------------------*
000600960321     FFNLS07D   CF   E             WORKSTN
000700960321     FAZORG01L  IF   E           K DISK
000800960321     FCNACO00F  IF   E           K DISK
000900960321     D*
001000960321     D* DEFINIZIONE SCHIERE
001100960321     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
001200941223     D*
001300960321     D* PASSAGGIO DATI ALL'ELABORAZIONE BATCH             - FNLS08R -
001400960321     D DSLS08        E DS                  EXTNAME(FNLS08DS) INZ
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
004700960321     C* 40/45 - ERRORI
004800960321     C* 90    - INDICATORE DI ERRORE GENERICO
004900960321     C*****************************************************************
005000000000     C     *ENTRY        PLIST
005100000000     C                   PARM                    KPJBA
005200900511     C*---------------------------------------------------------------*
005300960321     C* PULIZIA CAMPI VIDEATA
005400960321     C                   CLEAR                   V1CKSC
005500960321     C                   CLEAR                   V1DKSC
005600960321     C                   MOVEL     'NO'          V1CRIS
005700960321     C                   CLEAR                   V1CDVD
005800960321     C                   CLEAR                   V1CDVA
005900960321     C                   CLEAR                   V1CLNA
006000960321     C*
006100900509     C     FOR01         TAG
006200960321     C                   EXFMT     LS07D01
006300960321     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
006400960321     C                   CLEAR                   V1CMSG
006500960321     C                   SETOFF                                       28
006600960321     C                   SETOFF                                       404142
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
009100960321     C                   Z-ADD     V1CLNP        D08LNP
009200960321     C                   Z-ADD     V1CKSC        D08KSC
009300960321     C                   MOVEL     V1CRIS        D08RIS
009400960321     C                   Z-ADD     WDVD          D08DVD
009500960321     C                   Z-ADD     WDVA          D08DVA
009600960321     C                   Z-ADD     V1CLNA        D08LNA
009700960326     C                   Z-ADD     KCI           D08KCI
009800960327     C                   Z-ADD     SIMFEL        D08FEL
009900960321     C*
010000960401     C     V1CTST        IFEQ      ' '
010100960401     C     V1CTST        OREQ      'C'
010200960327     C                   CLEAR                   KPJBU
010300960321     C                   MOVEL     DSLS08        KPJBU
010400960321     C                   MOVEL     'LS08'        KCOAZ
010500900523     C                   CALL      'BCH10'
010600900523     C                   PARM                    KPJBA
010700960401     C                   ENDIF
010800920925     C*
010900960401     C     V1CTST        IFEQ      ' '
011000960401     C     V1CTST        OREQ      'B'
011100960401     C                   CLEAR                   KPJBU
011200960401     C                   MOVEL     DSLS08        KPJBU
011300960401     C                   MOVEL     'LS06'        KCOAZ
011400960401     C                   CALL      'BCH10'
011500960401     C                   PARM                    KPJBA
011600960401     C                   ENDIF
011700960401     C*
011800920925     C     FINE          TAG
011900000000     C                   SETON                                        LR
012000900524     C*
012100960321     C*--- CONTROLLO FILIALE IN GESTIONE -----------------------------*
012200960321     C     CTRFGS        BEGSR
012300960321     C                   SETOFF                                       90
012400960321     C*
012500960321     C                   CLEAR                   DSLV50
012600960321     C                   MOVEL     KNMUS         D50PRU
012700960321     C                   MOVEL     WFIL          D50FGS
012800960321     C                   CALL      'FNLV50R'
012900960321     C                   PARM                    DSLV50
013000960321     C*
013100960321    1C     D50ERR        IFNE      ' '
013200960321     C                   MOVEL     D50MSG        V1CMSG
013300960321     C                   SETON                                        402890
013400960321     C                   GOTO      ENDCTF
013500960321    1C                   ENDIF
013600960321     C*
013700960321     C     ENDCTF        ENDSR
013800960321     C*
013900960321     C*--- CONTROLLI FORMATO -----------------------------------------*
014000960321     C     CTRD01        BEGSR
014100960321     C                   SETOFF                                       9006
014200960321     C*
014300960321     C****  CODICE CLIENTE  ****
014400960321    1C     V1CKSC        IFGT      0
014500960327     C                   CLEAR                   V1DKSC
014600960321     C*
014700960321     C* CONTROLLO VALIDITA' CODICE CLIENTE
014800960321     C     KCLI          CHAIN     CNACO000                           30
014900960321    2C  N30ACOFLG        IFNE      ' '
015000960321     C                   SETON                                        30
015100960321    2C                   ENDIF
015200960321     C*
015300961016     C* SE NON ESISTE NEL P.d.C. ---> ERRORE
015400960321    2C     *IN30         IFEQ      *ON
015500961016     C                   MOVEL     MSG(2)        V1CMSG
015600961016     C                   SETON                                        412890
015700961016     C                   GOTO      ENDCTR
015800960327   X2C                   ELSE
015900960327     C* DECODIFICA CODICE CLIENTE
016000960327     C                   MOVEL     ACORAG        V1DKSC
016100960321    2C                   ENDIF
016200960321     C*
016300960321   X1C                   ELSE
016400960321     C* SE CODICE CLIENTE A ZERO ---> RICERCA ALFABETICA
016500960321     C*
016600960321    2C     V1DKSC        IFNE      *BLANKS
016700960327     C                   MOVEL     KCI           PARKCI
016800960321     C                   MOVEL     V1DKSC        RAGKSC
016900960321     C                   MOVEL     V1CRSU        PARDUT           30
017000960321     C* PARSTA = 9 --> ESCLUDO ANNULLATI
017100960321     C                   Z-ADD     9             PARSTA
017200000907     C                   Z-ADD     1             PAXNUM
017300980702     C* SOLO CLIENTI DEL S.I.
017400981109     C***+                 MOVELO50PRA    ORGDIT
017500981109     C                   CLEAR                   PARDIT
017600000907     C                   CALL      'XALFA3BR'
017700960321     C                   PARM                    PARDUT
017800960321     C                   PARM                    CODUT
017900960321     C                   PARM                    RAGKSC
018000960327     C                   PARM                    PARKCI
018100960321     C                   PARM                    PARSTA            1 0
018200980702     C                   PARM                    PARFLR           90
018300980702     C                   PARM                    PARDIT            3
018400000907     C                   PARM                    PAXNUM            2 0
018500000907     C                   PARM                    PAXKCM           80
018600000907     C                   PARM                    PAXKSM          140
018700000907     C                   PARM                    PAXKDM           60
018800960321     C     PARSTA        IFNE      -1
018900000907     C                   MOVEL     PAXKSM        V1CKSC
019000960321     C                   MOVEL     RAGKSC        V1DKSC
019100960321     C                   ENDIF
019200960321     C*
019300960321     C                   SETON                                        90
019400960321     C                   GOTO      ENDCTR
019500960321    2C                   ENDIF
019600960321    1C                   ENDIF
019700960321     C*
019800960321     C****  RISTAMPA  ****
019900960321    1C     V1CRIS        IFEQ      'NO'
020000960321    2C     V1CDVD        IFGT      0
020100960321     C     V1CLNA        ORGT      0
020200960321     C                   MOVEL     MSG(3)        V1CMSG
020300960321     C                   SETON                                        422890
020400960321     C                   GOTO      ENDCTR
020500960321    2C                   ENDIF
020600960326     C*
020700960326   X1C                   ELSE
020800960321     C*
020900960321     C****  DATA VARIAZIONE DAL  ****
021000960321     C                   MOVE      V1CDVD        G02DAT
021100960321     C                   MOVEL     *ZERO         G02INV
021200960321     C                   MOVEL     *BLANKS       G02ERR
021300941223     C                   CALL      'XSRDA8'
021400920925     C                   PARM                    WLBDAT
021500920925     C     G02ERR        IFEQ      '1'
021600960321     C                   MOVEL     MSG(4)        V1CMSG
021700960321     C                   SETON                                        432890
021800920925     C                   GOTO      ENDCTR
021900960321     C                   ENDIF
022000960321     C                   Z-ADD     G02DAT        V1CDVD
022100960321     C                   Z-ADD     G02INV        WDVD
022200900524     C*
022300960321     C****  DATA VARIAZIONE  AL  ****
022400960326    2C     V1CDVA        IFNE      0
022500960321     C                   MOVE      V1CDVA        G02DAT
022600960321     C                   MOVEL     *ZERO         G02INV
022700960321     C                   MOVEL     *BLANKS       G02ERR
022800941223     C                   CALL      'XSRDA8'
022900920925     C                   PARM                    WLBDAT
023000920925     C     G02ERR        IFEQ      '1'
023100960321     C                   MOVEL     MSG(4)        V1CMSG
023200960321     C                   SETON                                        442890
023300920925     C                   GOTO      ENDCTR
023400960321     C                   ENDIF
023500960321     C                   Z-ADD     G02DAT        V1CDVA
023600960321     C                   Z-ADD     G02INV        WDVA
023700941223     C*
023800900524     C* DATA DAL NO MAGGIORE DATA AL
023900960326    3C     WDVD          IFGT      WDVA
024000960321     C                   MOVEL     MSG(5)        V1CMSG
024100960321     C                   SETON                                        432890
024200900524     C                   GOTO      ENDCTR
024300960326    3C                   ENDIF
024400960321     C*
024500960321     C* SE INSERITA DATA AL OCCORRE INSERIRE ANCHE DATA DAL
024600960326    3C     WDVD          IFEQ      0
024700960321     C                   MOVEL     MSG(4)        V1CMSG
024800960321     C                   SETON                                        432890
024900960321     C                   GOTO      ENDCTR
025000960326    3C                   ENDIF
025100960326    2C                   ENDIF
025200941223     C*
025300960321     C****  FILIALE CHE HA ESEGUITO LA VARIAZIONE  ****
025400960326    2C     V1CLNA        IFGT      0
025500960321     C     V1CLNA        CHAIN     AZORG01L                           30
025600960321     C     *IN30         IFEQ      *ON
025700960321     C     ORGFVA        ORNE      ' '
025800960321     C                   MOVEL     MSG(6)        V1CMSG
025900960321     C                   SETON                                        452890
026000960321     C                   GOTO      ENDCTR
026100960321     C                   ENDIF
026200960326    2C                   ENDIF
026300960326    1C                   ENDIF
026400960321     C*
026500900524     C     ENDCTR        ENDSR
026600920925     C*
026700960321     C*--- SR INIZIALE -----------------------------------------------*
026800960321     C     *INZSR        BEGSR
026900960321     C****
027000960321     C** KLIST
027100960321     C****
027200960321     C* ACCESSO CNACO00F
027300960321     C     KCLI          KLIST
027400960321     C                   KFLD                    CODUT
027500960321     C                   KFLD                    KCI
027600960321     C                   KFLD                    V1CKSC
027700960321     C****
027800960321     C** DEFINIZIONE CAMPI
027900960321     C****
028000960321     C     *LIKE         DEFINE    V1CLNP        WFIL
028100960321     C     *LIKE         DEFINE    D08DVD        WDVD
028200960321     C     *LIKE         DEFINE    D08DVA        WDVA
028300960321     C     *LIKE         DEFINE    ACORAG        RAGKSC
028400960327     C     *LIKE         DEFINE    KCI           PARKCI
028500960321     C*
028600960321     C                   Z-ADD     1             CODUT
028700960321     C                   CALL      'X§PARUT'
028800960321     C                   PARM                    UT§DSE
028900960321     C                   MOVEL     RAGUT         V1CRSU
029000960321     C                   MOVEL     REC80         CNCR80
029100960321     C*
029200960321     C****  RICERCA CAPOCONTI  ****
029300960321    1C                   DO        50            X                 2 0
029400960321     C                   MOVE      TCU(X)        TCUDS
029500960321     C     F56           CABNE     'CG'          END1
029600960321     C     F4            COMP      '1'                                    21
029700960321     C     F4            COMP      '2'                                    22
029800960321     C     F4            COMP      '3'                                    23
029900960321     C     F4            COMP      '6'                                    27
030000960321     C** 1 CLIENTI   21
030100960321     C** 2 FORNITORI 22
030200960321     C** 3 AGENTI    23
030300960321     C     F3            COMP      '0'                                242425
030400960321     C     F3            COMP      'I'                                    26
030500960321     C** 0 ITALIA   25
030600960321     C** 1 ESTERO   24
030700960321     ** I CAPO CONTO IVA
030800960321     C   21
030900960321     CAN 24              Z-ADD     KCU(X)        KCE               4 0
031000960321     C   21
031100960321     CAN 25              Z-ADD     KCU(X)        KCI               4 0
031200960321     C   22
031300960321     CAN 24              Z-ADD     KCU(X)        KFE               4 0
031400960321     C   22
031500960321     CAN 25              Z-ADD     KCU(X)        KFI               4 0
031600960321     C   23
031700960321     CAN 24              Z-ADD     KCU(X)        KAE               4 0
031800960321     C   23
031900960321     CAN 25              Z-ADD     KCU(X)        KAI               4 0
032000960321     C   26              Z-ADD     KCU(X)        KIVA              4 0
032100960321     C   27              Z-ADD     KCU(X)        KBNA              4 0
032200960321     C     END1          TAG
032300960321    1C                   END
032400960321     C*
032500960321     C* SPENGO GLI INDICATORI USATI PER LA RICERCA CAPOCONTI
032600960321     C                   SETOFF                                       212223
032700960321     C                   SETOFF                                       242526
032800960321     C                   SETOFF                                       27
032900960327     C*
033000020429     C****  VEDO SE SONO PRIMO O SECONDO LIVELLO ****
033100020429    1C***        REM       IFEQ 'REM'
033200020429     C***        REMFIL    ANDGT*ZEROS
033300020429     C***                  MOVELREMFIL    V1CLNP
033400020429     C***                  SETON                     05
033500020429   X1C***                  ELSE
033600020429     C***                  MOVELSIMFEL    V1CLNP
033700020429    1C***                  ENDIF
033800020429     C                   Z-ADD     SIMPOU        V1CLNP
033900020429     C     SIMTPP        IFEQ      '2'
034000020429     C     SIMTPP        OREQ      *BLANKS
034100020429     C                   SETON                                        05
034200020429     C                   ENDIF
034300920925     C                   ENDSR
034400941223     C*---------------------------------------------------------------*
034500961025**  MSG
034600960321Linea di partenza errata                                                      1
034700960321Cliente Inesistente                                                           2
034800960321Data variazione e Linea da controllare si possono inserire solo per RISTAMPA  3
034900960321Data errata                                                                   4
035000960321La data variazione DAL non puo' essere maggiore della data AL                 5
035100961025Punto Operativo inesistente                                                   6
