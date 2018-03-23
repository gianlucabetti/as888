000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200961119     H* FNLR45R *----------------------------------------------------*
000300961119     H*-----------------*                                            *
000400961120     H*              INTERROGAZIONE EVENTI SEGNACOLLI                *
000500961119     H*--------------------------------------------------------------*
000600961119     FFNLR45D   CF   E             WORKSTN
000700961120     F                                     SFILE(LR45S01:NRR)
000800961129     FFNEVS01L  IF   E           K DISK
000900961120     FFNEVS02L  IF   E           K DISK
001000961120     F                                     RENAME(FNEVS000:FNEVS002)
001100961129     FFNART01L  IF   E           K DISK
001200961129     FFNART27L  IF   E           K DISK
001300961120     F                                     RENAME(FNART000:FNART027)
001400930525     FTABEL00F  IF   E           K DISK
001500961121     FAZORG01L  IF   E           K DISK
001600961120     D*
001700961120     D* DEFINIZIONE SCHIERE
001800961121     D CEV             S              1    DIM(64)                              COMODO X CEV
001900961120     D*
002000961120     D* DS PER FNLR45R - DS DI PASSAGGIO PARAMETRI PER PGM CHIAMANTI
002100961120     D DSLR45        E DS                  EXTNAME(FNLR45DS)
002200961121     D*
002300961121     D* RICHIAMO PGM INTERROGAZ.BOLLE ARRIVI            - FNLR36R -
002400961121     D PARAM1          DS
002500961121     D  PA1AAS                14     17  0
002600961121     D  PA1LNP                18     20  0
002700961121     D  PA1NRS                21     22  0
002800961121     D  PA1NSP                23     29  0
002900961121     D  PA1F03                30     30    INZ
003000961121     D  PA1FLG                31     31    INZ('E')
003100961121     D  PA1PGM                32     41    INZ('FNLR45R   ')
003200961121     D  PA1DSA                42     61    INZ
003300961121     D  PA1CML                62     62    INZ
003400961121     D  PA1DSP                63     70  0 INZ
003500961121     D  PA1RMN                71     85  0 INZ
003600961121     D  PA1LNA                86     88  0 INZ
003700961121     D  PA1LN1                89    108    INZ
003800961121     D  PA1RSD               109    143    INZ
003900961121     D  PA1GIA               144    144    INZ
004000961121     D  PA1FEL               145    147  0 INZ
004100961121     D  PA1TBO               148    148    INZ
004200961120     D*
004300961120     D                 DS
004400961120     D  AA                     1      4  0
004500961120     D  MM                     5      6  0
004600961120     D  GG                     7      8  0
004700961120     D  WDATA                  1      8  0
004800000000     D KPJBA         E DS
004900930526     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005000961119     C*****************************************************************
005100961119     C* RIEPILOGO INDICATORI
005200961119     C*****************************************************************
005300961121     C* 15    - ON--> INTERR.PER SPEDIZ.   OFF--> INTERR.PER SEGNACOLLO
005400961121     C* 19    - ABILITAZ. TASTO F7-BOLLA
005500961119     C* 20 ON - SFLDSP
005600961121     C* 21    - ON--> SFLDSPCTL            OFF--> SFLCLR
005700961120     C* 23 ON - SFLEND
005800961121     C* 29    - READC SUBFILE
005900961120     C* 30/32 - DI COMODO
006000961119     C*****************************************************************
006100000000     C     *ENTRY        PLIST
006200000000     C                   PARM                    KPJBA
006300961120     C                   MOVEL     KPJBU         DSLR45
006400930525     C*---------------------------------------------------------------*
006500961121     C                   SETOFF                                       1519
006600961120     C*
006700961121     C* TIPO LANCIO: D45TLA = "C" -           CHIUDO CON LR
006800961121     C*              D45TLA = "L" - ELABORO E CHIUDO CON LR
006900961121     C*              D45TLA = " " - ELABORO E CHIUDO IN  RETRN
007000961120    1C     D45TLA        IFNE      'C'
007100930525     C*
007200961120     C****  P U L I Z I A   S U B F I L E  ****
007300940726     C                   SETOFF                                       202123
007400961120     C                   WRITE     LR45C01
007500961121     C                   SETON                                        202123
007600930525     C*
007700961120     C                   Z-ADD     0             NRR               4 0
007800961120     C                   CLEAR                   W1VOLT
007900930525     C*
008000961120     C****  C A R I C O    S U B F I L E  ****
008100961121     C***
008200961121     C* INTERROGAZIONE PER NUMERO  S P E D I Z I O N E  ( NSP>0 )
008300961121     C***
008400961120    2C     D45NSP        IFGT      0
008500961121     C                   SETON                                        15
008600961120     C*
008700961120     C* 1° GIRO: LEGGO FNEVS01L CON KEY SPEDIZIONE E SCARTO I RECORD
008800961120     C*          CON SEGNACOLLO > 0 PER LEGGERLI NEL 2° GIRO
008900961120     C*
009000961120     C     KEVS1         SETLL     FNEVS01L
009100961120     C     KEVS1         READE     FNEVS01L                               30
009200930525     C*
009300961120    3C     *IN30         DOWEQ     *OFF
009400961120     C* ESCLUDO RECORD ANNULLATI
009500961120    4C     EVSATB        IFEQ      ' '
009600930525     C*
009700961120     C* ESCLUDO RECORD SE STO LEGGENDO PER SPEDIZIONE E SEGNACOLLO>0
009800961120     C* (LO TRATTERO' NEL GIRO PER SEGNACOLLO)
009900961120    5C     EVSNSC        IFEQ      0
010000961120     C*
010100961120     C* RIEMPIO SUBFILE
010200961120     C                   EXSR      RIES01
010300961120    5C                   ENDIF
010400961120    4C                   ENDIF
010500930525     C*
010600961120     C     KEVS1         READE     FNEVS01L                               30
010700961120    3C                   ENDDO
010800930525     C*
010900961120     C*
011000961120     C* 2° GIRO: LEGGO I SEGNACOLLI DELLA SPEDIZIONE E PER OGNUNO DI
011100961120     C*          ESSI LEGGO FNEVS02L CON KEY SEGNACOLLO
011200961120     C*
011300961120     C     KEVS1         SETLL     FNART01L
011400961120     C     KEVS1         READE     FNART01L                               30
011500961120     C*
011600961120    3C     *IN30         DOWEQ     *OFF
011700961120     C*
011800961120     C     KART          SETLL     FNEVS02L
011900961120     C     KART          READE     FNEVS02L                               31
012000961120     C*
012100961120    4C     *IN31         DOWEQ     *OFF
012200961120     C* ESCLUDO RECORD ANNULLATI
012300961120    5C     EVSATB        IFEQ      ' '
012400961121     C*
012500961121     C* SE LA KEY SPEDIZIONE E' VUOTA LA RIEMPIO CON I DATI PASSATI
012600961121    6C     EVSNSP        IFEQ      0
012700961121     C                   MOVEL     D45AAS        EVSAAS
012800961121     C                   MOVEL     D45FIL        EVSLNP
012900961121     C                   MOVEL     D45NRS        EVSNRS
013000961121     C                   MOVEL     D45NSP        EVSNSP
013100961121    6C                   ENDIF
013200961120     C* RIEMPIO SUBFILE
013300961120     C                   EXSR      RIES01
013400961120    5C                   ENDIF
013500961120     C*
013600961120     C     KART          READE     FNEVS02L                               31
013700961120    4C                   ENDDO
013800961120     C*
013900961120     C     KEVS1         READE     FNART01L                               30
014000961120    3C                   ENDDO
014100961120     C*
014200961120   X2C                   ELSE
014300961120     C***
014400961121     C* INTERROGAZIONE PER NUMERO  S E G N A C O L L O  ( NSC>0 )
014500961121     C***
014600961121     C                   SETOFF                                       15
014700961120     C*
014800961120     C     KEVS2         SETLL     FNEVS02L
014900961120     C     KEVS2         READE     FNEVS02L                               30
015000961120     C*
015100961120    3C     *IN30         DOWEQ     *OFF
015200961120     C* ESCLUDO RECORD ANNULLATI
015300961120    4C     EVSATB        IFEQ      ' '
015400961120     C* SE EVENTO CON NUMERO SPEDIZIONE = 0 CONTROLLO SE C'E' LA BOLLA
015500961120     C*   IN MODO DA VISUALIZZARE ANCHE IL NUMERO SPEDIZIONE
015600961120    5C     EVSNSP        IFEQ      0
015700961120    6C     W1VOLT        IFEQ      ' '
015800961120     C                   EXSR      RICBOL
015900961120     C                   MOVEL     '1'           W1VOLT            1
016000961120    6C                   ENDIF
016100961120   X5C                   ELSE
016200961120     C                   MOVEL     '1'           W1VOLT
016300961120    5C                   ENDIF
016400961120     C* RIEMPIO SUBFILE
016500961120     C                   EXSR      RIES01
016600961120    4C                   ENDIF
016700961120     C*
016800961120     C     KEVS2         READE     FNEVS02L                               30
016900961120    3C                   ENDDO
017000961120     C*
017100961121     C* 19 ON  - ABILITAZ. TASTO F7-BOLLA
017200961121    3C     NRR           IFGT      0
017300961121     C                   SETON                                        19
017400961121    3C                   ENDIF
017500961120    2C                   ENDIF
017600961120     C*
017700961120     C****  E M E T T O    S U B F I L E  ****
017800961120    2C     NRR           IFEQ      0
017900961120     C                   SETOFF                                       20
018000961120    2C                   ENDIF
018100961120     C*
018200961121     C                   Z-ADD     1             REC
018300961120     C     FOR01         TAG
018400961121     C                   WRITE     LR45T01
018500961120     C                   WRITE     LR45Z01
018600961121     C  N20              WRITE     LR45D03
018700961120     C                   EXFMT     LR45C01
018800961121     C*
018900961121     C* CMD3  - FINE LAVORO
019000961121     C   KC              GOTO      FINE
019100961120     C*
019200961121     C* CMD7  - INTERROGAZIONE BOLLA
019300961121    2C  N15*INKG         IFEQ      *ON
019400961121     C                   RESET                   PARAM1
019500961121     C                   MOVE      EVSAAS        PA1AAS
019600961121     C                   MOVE      EVSLNP        PA1LNP
019700961121     C                   MOVE      EVSNRS        PA1NRS
019800961121     C                   MOVE      EVSNSP        PA1NSP
019900961121     C                   MOVEL     PARAM1        KPJBU
020000961121     C                   CALL      'FNLR36R'
020100961121     C                   PARM                    KPJBA
020200961122     C                   GOTO      FOR01
020300961121    2C                   ENDIF
020400961121     C*
020500961121     C****  CONTROLLO SE E' STATO SELEZIONATO QUALCOSA  ****
020600961122    2C     NRR           IFGT      0
020700961122     C*
020800961121     C                   READC     LR45S01                                29
020900961121     C   29              GOTO      FINE
021000961121     C*
021100961122    3C     *IN29         DOWEQ     *OFF
021200961121     C*
021300961122    4C     VSCSCE        IFEQ      'I'
021400961121     C* PULISCO IL CAMPO DI SCELTA
021500961121     C     NRR           CHAIN     LR45S01                            32
021600961121     C                   CLEAR                   VSCSCE
021700961121     C                   UPDATE    LR45S01
021800961121     C*
021900961121     C* RIEMPIO CAMPI 2° VIDEATA
022000961121     C                   EXSR      RIED02
022100961121     C* EMETTO 2° VIDEATA
022200961121     C                   WRITE     LR45T01
022300961121     C                   EXFMT     LR45D02
022400961121     C* CMD3  - FINE LAVORO
022500961121     C   KC              GOTO      FINE
022600961122    4C                   ENDIF
022700961121     C*
022800961121     C                   READC     LR45S01                                29
022900961122     C*
023000961122     C* CMD12 - RITORNO / ENTER --> SE NON E' STATO SELEZIONATO NIENTE
023100961122     C*                             ALTRO RITORNO A VIDEATA PRECEDENTE
023200961122     C   29              GOTO      FOR01
023300961122    3C                   ENDDO
023400961122    2C                   ENDIF
023500930525     C*
023600961121     C     FINE          TAG
023700961120    1C                   ENDIF
023800961120     C*
023900961120     C* CHIUSURA PGM
024000961121    1C     D45TLA        IFEQ      ' '
024100961120     C                   RETURN
024200961120   X1C                   ELSE
024300961120     C                   SETON                                        LR
024400961120    1C                   ENDIF
024500961120     C*
024600961120     C*--- RIEMPIO SUBFILE -------------------------------------------*
024700961120     C     RIES01        BEGSR
024800961120     C*
024900961120     C                   MOVEL     EVSFLS        VSCFLS
025000961120     C                   MOVEL     EVSLNA        VSCLNA
025100961120     C                   MOVEL     EVSNRS        VSCSR1
025200961120     C                   MOVEL     EVSNSC        VSCNSC
025300961120     C                   MOVEL     EVSNPS        VSCNPS
025400961120     C                   MOVE      EVSAAS        VSCAAS
025500961120     C                   MOVEL     EVSLNP        VSCLNP
025600961120     C                   MOVEL     EVSNRS        VSCSR2
025700961120     C                   MOVEL     EVSNSP        VSCNSP
025800961120     C* DATA EVENTO
025900961120     C                   Z-ADD     EVSDEV        WDATA
026000961120     C                   Z-ADD     MM            GGMM              4 0
026100961120     C                   MOVEL     GG            GGMM
026200961121     C                   Z-ADD     AA            WHDEV                          *DATA 8 CARATTERI
026300961121     C                   MOVEL     GGMM          WHDEV
026400961121     C                   Z-ADD     WHDEV         VSCDEV
026500961121     C                   MOVEL     GGMM          VSCDEV                         *DATA 6 CARATTERI
026600961120     C* CAUSALE EVENTO
026700961120     C                   MOVEL     *BLANKS       VSCCEV
026800961120     C                   MOVEA     *BLANKS       CEV
026900961120     C                   MOVEL     '8E'          KCOD
027000961120     C                   MOVEL     *BLANKS       KKEY
027100961120     C                   MOVEL     EVSCEV        KKEY
027200961120     C     KTAB          CHAIN     TABEL00F                           32
027300961120     C  N32              MOVEA     TBLUNI        CEV(5)
027400961120     C   32              MOVEA     *BLANKS       CEV(5)
027500961120     C                   MOVEA     EVSCEV        CEV(1)
027600961120     C                   MOVEA     '-'           CEV(4)
027700961121     C                   MOVEA     CEV           WHCEV                          *CEV 64 CARATTERI
027800961121     C                   MOVEL     WHCEV         VSCCEV                         *CEV 20 CARATTERI
027900961120     C* DATA
028000961120     C                   Z-ADD     EVSDAT        WDATA
028100961120     C                   Z-ADD     MM            GGMM
028200961120     C                   MOVEL     GG            GGMM
028300961121     C                   Z-ADD     AA            WHDAT                          *DATA 8 CARATTERI
028400961121     C                   MOVEL     GGMM          WHDAT
028500961121     C                   Z-ADD     WHDAT         VSCDAT
028600961121     C                   MOVEL     GGMM          VSCDAT                         *DATA 6 CARATTERI
028700961120     C*
028800961120     C                   MOVEL     EVSDES        VSCDES
028900961120     C                   MOVEL     EVSFIL        VSCFIL
029000961120     C*
029100961120     C                   ADD       1             NRR
029200961120     C                   WRITE     LR45S01
029300961120     C*
029400961120     C                   ENDSR
029500961120     C*
029600961120     C*--- RICERCO LA BOLLA ------------------------------------------*
029700961120     C     RICBOL        BEGSR
029800961120     C     KART2         CHAIN     FNART27L                           32
029900961120    1C     *IN32         IFEQ      *OFF
030000961120     C                   MOVEL     ARTAAS        EVSAAS
030100961120     C                   MOVEL     ARTLNP        EVSLNP
030200961120     C                   MOVEL     ARTNRS        EVSNRS
030300961120     C                   MOVEL     ARTNSP        EVSNSP
030400961120    1C                   ENDIF
030500961120     C                   ENDSR
030600961121     C*
030700961121     C*--- RIEMPIO DATI 2° VIDEATA -----------------------------------*
030800961121     C     RIED02        BEGSR
030900961121     C*
031000961121     C                   MOVEL     VSCFLS        V2CFLS
031100961121     C                   MOVEL     VSCLNA        V2CLA1
031200961121     C                   MOVEL     VSCSR1        V2CSR1
031300961121     C                   MOVEL     VSCNSC        V2CNSC
031400961121     C                   MOVEL     VSCNPS        V2CNPS
031500961121     C                   MOVE      VSCAAS        V2CAAS
031600961121     C                   MOVEL     VSCLNP        V2CLNP
031700961121     C                   MOVEL     VSCSR2        V2CSR2
031800961121     C                   MOVEL     VSCNSP        V2CNSP
031900961121     C                   MOVEL     VSCLNA        V2CLA2
032000961121     C                   MOVEL     WHDEV         V2CDEV
032100961121     C                   MOVEL     WHCEV         V2CCEV
032200961121     C                   MOVEL     VSCDES        V2CDES
032300961121     C                   MOVEL     WHDAT         V2CDAT
032400961121     C                   MOVEL     VSCFIL        V2CFIL
032500961121     C     V2CFIL        CHAIN     AZORG01L                           32
032600961121     C     *IN32         IFEQ      *OFF
032700961121     C                   MOVEL     ORGDES        V2DFIL
032800961121     C                   ELSE
032900961121     C                   CLEAR                   V2DFIL
033000961121     C                   ENDIF
033100961121     C*
033200961121     C                   ENDSR
033300961121     C*
033400961120     C*--- SR INIZIALE -----------------------------------------------*
033500940726     C     *INZSR        BEGSR
033600961120     C*
033700961120     C* ACCESSO FNEVS01L / FNART01L
033800961120     C     KEVS1         KLIST
033900961120     C                   KFLD                    D45AAS
034000961120     C                   KFLD                    D45FIL
034100961120     C                   KFLD                    D45NRS
034200961120     C                   KFLD                    D45NSP
034300961120     C* ACCESSO FNEVS02L
034400961120     C     KART          KLIST
034500961120     C                   KFLD                    ARTFLS
034600961120     C                   KFLD                    ARTLNA
034700961120     C                   KFLD                    ARTNRS
034800961120     C                   KFLD                    ARTNSC
034900961120     C     KEVS2         KLIST
035000961120     C                   KFLD                    D45FIL
035100961120     C                   KFLD                    D45LNA
035200961120     C                   KFLD                    D45NRS
035300961120     C                   KFLD                    D45SCN
035400961120     C* ACCESSO FNART27L
035500961120     C     KART2         KLIST
035600961120     C                   KFLD                    EVSFLS
035700961120     C                   KFLD                    EVSLNA
035800961120     C                   KFLD                    EVSNRS
035900961120     C                   KFLD                    EVSNSC
036000961120     C* ACCESSO TABEL00F
036100961120     C     KTAB          KLIST
036200940726     C                   KFLD                    CODUT
036300940726     C                   KFLD                    KCOD
036400940726     C                   KFLD                    KKEY
036500940726     C*
036600961120     C* REPERISCO LA RAGIONE SOCIALE UTENTE
036700961120     C                   Z-ADD     1             CODUT
036800961120     C                   CALL      'X§PARUT'
036900961120     C                   PARM                    UT§DSE
037000961120     C                   MOVEL     RAGUT         VTCRSU
037100961120     C*
037200961120     C* DEFINIZIONE CAMPI
037300961120     C     *LIKE         DEFINE    TBLCOD        KCOD
037400961120     C     *LIKE         DEFINE    TBLKEY        KKEY
037500940726     C*
037600940726     C                   ENDSR
