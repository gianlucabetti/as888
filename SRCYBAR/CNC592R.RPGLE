000100000000     F*****************************************************************
000200000000     F* RICERCA ALFABETICA SU ANAGRAFICA  BANCHE    - CNABI00F -
000300000000     F*****************************************************************
000400000000     FCNABI00F  IF   E           K DISK
000500000000     FCNABI02L  IF   E           K DISK
000600000000     F                                     RENAME(CNABI000:CNABI999)
000700890727     FCNC592D   CF   E             WORKSTN
000800000000     F                                     SFILE(S1:Y)
000900000000     D ER              S             60    DIM(5) CTDATA PERRCD(1)              ERRORI A V
001000000000     D KPJBA         E DS
001100920601     D PARAM           DS
001200920601     D  ABIP                   1      5  0
001300920601     D  CABP                   6     10  0
001400920601     D  ISTP                  11     50
001500920601     D  PROP                  51     52
001600920601     D  FLGP                  53     53
001700890727     C*
001800890727     C*----------------------------------------------------*
001900890727     C*  CHIAVI UTILIZZATE A PROGRAMMA                    -*
002000890727     C*----------------------------------------------------*
002100890727     C*
002200890727     C     KEYV          KLIST
002300890727     C                   KFLD                    ABIW
002400890727     C                   KFLD                    CABW
002500890727     C*
002600890727     C     KEYW          KLIST
002700890727     C                   KFLD                    ABI               5 0
002800890727     C                   KFLD                    CAB               5 0
002900890727     C*
003000890727     C     KEY           KLIST
003100900726     C                   KFLD                    VABI
003200900726     C                   KFLD                    VCAB
003300900717     C     KEYIST        KLIST
003400900717     C                   KFLD                    CHIAVE
003500900726     C                   KFLD                    你BI
003600900726     C                   KFLD                    低AB
003700890727     C*
003800890727     C*----------------------------------------------------*
003900890727     C*
004000000000     C     *ENTRY        PLIST
004100000000     C                   PARM                    KPJBA
004200920601     C                   MOVEL     KPJBU         PARAM
004300000000     C*
004400000000     C     PRIMO         TAG
004500920703     C* SE TROVATO ERRORE ESCO SE RICHIAMAMTO
004600920703     C   90FLGP          IFEQ      'R'
004700920703     C                   GOTO      FINE
004800920703     C                   END
004900920703     C*
005000920601     C                   SETOFF                                       26
005100920601     C                   EXSR      PULIZ
005200920601     C                   EXSR      PULCAM
005300920601     C* PROVENGO DA RICERCA
005400930624    1C     FLGP          IFEQ      'R'
005500920601     C                   Z-ADD     ABIP          ABIW
005600920601     C                   Z-ADD     CABP          CABW
005700920601     C                   MOVEL     ISTP          ISTW
005800920601     C                   MOVEL     PROP          PROW
005900930624   X1C                   ELSE
006000920601     C                   Z-ADD     0             ABIW
006100920601     C                   Z-ADD     0             CABW
006200920601     C                   MOVEL     *BLANKS       ISTW
006300920601     C                   MOVEL     *BLANKS       PROW
006400000000     C*
006500000000     C                   EXFMT     F1
006600000000     C                   SETOFF                                       90
006700000000     C*
006800890727     C   KL              GOTO      FINE
006900930624    1C                   END
007000930624     C*
007100930624     C     ABIW          IFEQ      *ZEROS
007200930624     C     CABW          ANDEQ     *ZEROS
007300930624     C     ISTW          ANDEQ     *BLANKS
007400930624     C     PROW          ANDEQ     *BLANKS
007500930624     C                   SETON                                        90
007600930624     C                   GOTO      PRIMO
007700930624     C                   END
007800000000     C*
007900000000     C     ABIW          IFNE      *ZEROS
008000000000     C     CABW          IFEQ      *ZEROS
008100000000     C                   Z-ADD     ABIW          ABI
008200000000     C                   Z-ADD     *ZEROS        CAB
008300000000     C                   EXSR      CARICA
008400900726     C                   Z-ADD     1             Y
008500000000     C                   GOTO      SUBE
008600000000     C                   ELSE
008700000000     C     KEYV          CHAIN     CNABI00F                           90
008800900601     C  N90ABIANN        COMP      '*'                                    90
008900000000     C   90              MOVEL     ER(1)         ERRORE
009000920601     C*
009100920601     C* SE PER RICERCA MEMORIZZO ED ESCO
009200920601     C     FLGP          IFEQ      'R'
009300920601     C                   MOVEL     ABIW          ABIP
009400920601     C                   MOVEL     CABW          CABP
009500920601     C                   MOVEL     PARAM         KPJBU
009600920601     C                   GOTO      FINE
009700920601     C                   ELSE
009800920601     C                   Z-ADD     ABIW          VABI
009900920601     C                   Z-ADD     CABW          VCAB
010000900716     C                   EXSR      VID2
010100900726     C                   GOTO      PRIMO
010200000000     C                   END
010300920601     C                   END
010400000000     C                   END
010500000000     C*
010600000000     C     ISTW          IFNE      *BLANKS
010700900716     C     PROW          ORNE      *BLANKS
010800930624     C     ISTW          OREQ      *BLANKS
010900930624     C     PROW          ANDEQ     *BLANKS
011000900716     C                   SETON                                            26
011100900716     C                   MOVEL     ISTW          CHIAVE
011200000000     C                   EXSR      CARIC1
011300000000     C                   END
011400000000     C*
011500000000     C     SUBE          TAG
011600000000     C*
011700900716     C                   WRITE     CMD
011800890727     C                   EXFMT     SC1
011900920601     C   KLFLGP          IFEQ      'R'
012000920601     C                   GOTO      FINE
012100920601     C                   ELSE
012200920601     C                   GOTO      PRIMO
012300920601     C                   END
012400900716     C*PROSEGUI RICERCA
012500900716     C   07              DO
012600900716     C                   MOVE      *BLANKS       CHIAVE           80
012700900726     C                   MOVEL     兌ST          CHIAVE
012800900716     C                   EXSR      CARIC1
012900900724     C                   GOTO      SUBE
013000900716     C                   END
013100900716     C*
013200900724     C                   Z-ADD     0             A
013300900726     C                   Z-ADD     Y             即                4 0
013400900724     C                   DO        *HIVAL        A
013500900724     C     A             CHAIN     S1                                 11
013600900716     C   11              GOTO      SUBE
013700900713     C     VSCEL         IFEQ      'X'
013800920601     C*
013900920601     C* SE PER RICERCA MEMORIZZO ED ESCO
014000920601     C     FLGP          IFEQ      'R'
014100920601     C                   MOVEL     VABI          ABIP
014200920601     C                   MOVEL     VCAB          CABP
014300920601     C                   MOVEL     PARAM         KPJBU
014400920601     C                   GOTO      FINE
014500920601     C                   ELSE
014600920601     C*
014700900716     C                   EXSR      VID2
014800920601     C                   END
014900920601     C*
015000900716     C                   MOVEL     *BLANKS       VSCEL
015100900726     C                   Z-ADD     即            Y
015200900716     C                   UPDATE    S1
015300900716     C   KL              GOTO      PRIMO
015400900716     C  NKL              GOTO      SUBE
015500000000     C                   END
015600900716     C                   END
015700900716     C**
015800890727     C*
015900890727     C*----------------------------------------------------*
016000890727     C*
016100000000     C     FINE          TAG
016200000000     C                   SETON                                        LR
016300890727     C*
016400890727     C*----------------------------------------------------*
016500890727     C*
016600900716     C     VID2          BEGSR
016700900716     C     KEY           CHAIN     CNABI00F                           23
016800900716     C  N23ABIANN        COMP      '*'                                    23
016900900726     C   23              MOVEL     ER(1)         ERRORE
017000900726     C   23              GOTO      PRIMO
017100900716     C*
017200900716     C*
017300900726     C  N23              DO
017400900726     C                   MOVEL     ABIIST        VIST
017500900716     C                   MOVEL     ABIAGE        AGEV
017600900716     C                   MOVEL     ABIND         INDV
017700900716     C                   MOVEL     ABILOC        LOCV
017800900716     C                   MOVEL     ABICOM        COMV
017900900726     C                   Z-ADD     ABICAP        CAPV
018000900716     C*
018100900726     C                   END
018200900716     C                   EXFMT     F2
018300900716     C                   ENDSR
018400890727     C/EJECT
018500890727     C*
018600890727     C*----------------------------------------------------*
018700890727     C*-   CARICA =   CARICA SUB-FILE                     -*
018800890727     C*----------------------------------------------------*
018900890727     C*
019000000000     C     CARICA        BEGSR
019100000000     C*
019200000000     C*
019300000000     C     KEYW          SETLL     CNABI00F
019400000000     C*
019500000000     C                   DO        *HIVAL
019600000000     C     ABI           READE     CNABI00F                               23
019700000000     C   23              GOTO      OUT
019800900601     C     ABIANN        CABEQ     '*'           刨OT
019900000000     C*
020000900713     C                   MOVEL     ABIIST        VIST
020100900713     C                   MOVEL     ABIAGE        VAGE
020200900726     C                   Z-ADD     ABIABI        VABI
020300900726     C                   Z-ADD     ABICAB        VCAB
020400900726     C                   MOVEL     ABIPRO        VPRO
020500900713     C                   ADD       1             Y
020600000000     C                   WRITE     S1
020700000000     C*
020800900601     C     刨OT          TAG
020900000000     C                   END
021000000000     C*
021100000000     C     OUT           TAG
021200000000     C*
021300000000     C     Y             IFEQ      0
021400000000     C                   SETON                                        90
021500000000     C                   MOVEL     ER(2)         ERRORE
021600000000     C                   GOTO      PRIMO
021700000000     C                   END
021800000000     C*
021900000000     C*
022000000000     C                   ENDSR
022100890727     C*
022200890727     C*----------------------------------------------------*
022300890727     C*
022400890727     C/EJECT
022500890727     C*
022600890727     C*----------------------------------------------------*
022700890727     C*-   CARIC1 =   CARICA SUB-FILE                     -*
022800890727     C*----------------------------------------------------*
022900890727     C*
023000000000     C     CARIC1        BEGSR
023100000000     C*
023200000000     C*
023300900717     C  N07CHIAVE        SETLL     CNABI02L
023400900724     C   07KEYIST        SETLL     CNABI02L
023500000000     C*
023600900724     C                   Z-ADD     0             A                 4 0
023700900724     C                   DO        *HIVAL
023800030909     C     A             IFGE      20
023900900716     C                   GOTO      FUORI
024000900716     C                   END
024100000000     C                   READ      CNABI02L                               22
024200000000     C   22              GOTO      FUORI
024300000000     C*
024400900601     C     ABIANN        CABEQ     '*'           壯SOT
024500900716     C     PROW          IFGT      *BLANKS
024600900713     C     ABIPRO        ANDNE     PROW
024700900713     C                   GOTO      壯SOT
024800900713     C                   END
024900900716     C                   MOVEL     *BLANKS       VSCEL
025000900713     C                   MOVEL     ABIIST        VIST
025100900713     C                   MOVEL     ABIAGE        VAGE
025200900726     C                   Z-ADD     ABIABI        VABI
025300900726     C                   Z-ADD     ABICAB        VCAB
025400900726     C                   MOVEL     ABIPRO        VPRO
025500900726     C                   MOVEL     *BLANKS       兌ST             20
025600900726     C                   MOVEL     VIST          兌ST
025700900726     C                   Z-ADD     ABIABI        你BI              5 0
025800900726     C                   Z-ADD     ABICAB        低AB              5 0
025900900726     C                   ADD       1             Y
026000900724     C                   ADD       1             A
026100000000     C                   WRITE     S1
026200000000     C*
026300900601     C*
026400900601     C     壯SOT         TAG
026500000000     C                   END
026600000000     C*
026700000000     C     FUORI         TAG
026800000000     C*
026900000000     C     Y             IFEQ      0
027000000000     C                   SETON                                        90
027100000000     C                   MOVEL     ER(2)         ERRORE
027200000000     C                   GOTO      PRIMO
027300000000     C                   END
027400000000     C*
027500000000     C                   ENDSR
027600900724     C*----------------------------------------------------*
027700900724     C*-   PULIZ  =   PULIZIA SUB-FILE                    -*
027800900724     C*----------------------------------------------------*
027900900724     C     PULIZ         BEGSR
028000900724     C                   SETON                                        55
028100900724     C                   WRITE     SC1                                          ** PULIZIA
028200900724     C                   SETOFF                                       55
028300900724     C                   Z-ADD     0             Y                 4 0
028400900724     C                   ENDSR
028500900726     C*----------------------------------------------------*
028600900726     C*-   PULCAM =   PULIZIA CAMPI                       -*
028700900726     C*----------------------------------------------------*
028800900726     C     PULCAM        BEGSR
028900900726     C                   MOVEL     *BLANKS       VIST
029000900726     C                   Z-ADD     0             VABI
029100900726     C                   Z-ADD     0             VCAB
029200900726     C                   MOVEL     *BLANKS       VAGE
029300900726     C                   MOVEL     *BLANKS       VPRO
029400900726     C                   MOVEL     *BLANKS       AGEV
029500900726     C                   MOVEL     *BLANKS       INDV
029600900726     C                   MOVEL     *BLANKS       LOCV
029700900726     C                   MOVEL     *BLANKS       COMV
029800900726     C                   Z-ADD     0             CAPV
029900900726     C                   ENDSR
030000000000**
030100000000Codice ABI/CAB non presente in Anagrafica !
030200000000Nessun elemento ricercato in Anagrafica !
0303000000003
0304000000004
0305000000005
