000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941119     H* TRTC30R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400921021     H* - PREPARA FILE GUIDA PER SCARICO ARCHIVI PER CLIENTI SU P.C. *
000500000000     H*--------------------------------------------------------------*
000600921021     FTABEL00F  IF   E           K DISK
000700921021     FFLTCL00F  O    E             DISK
000800921021     FCNACO00F  IF   E           K DISK
000900921023     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            RMTCMD
001000921021     D CST             S             15    DIM(4) CTDATA PERRCD(1)              COSTANTI
001100921021     D TCUDS           DS
001200921021     D  F1                     1      1
001300921021     D  F3                     3      3
001400921021     D  F2                     2      2
001500921021     D  F4                     4      4
001600921021     D  F56                    5      6
001700921021     D CNCR80        E DS
001800921021     D DS3K          E DS
001900921021     D DS4C          E DS
002000921021     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002100921021     D  TCU                  398    697
002200921021     D                                     DIM(50)                              TAB FLAG CNT
002300921021     D  KCU                  698    847P 0
002400921021     D                                     DIM(50)                              TAB CAPO CONTO
002500921021     D                                     PACKEVEN
002600921021     D  DTU                  848    895P 0
002700921021     D                                     DIM(12)                              DATE PARAM
002800921021     D                                     PACKEVEN
002900921021     C*-------------------------------------------------
003000941118     C     *ENTRY        PLIST
003100941118     C                   PARM                    §LIBRF           10
003200941118     C                   PARM                    §LIBRO            9
003300941118     C                   MOVEA     §LIBRO        CMD(6)
003400921021     C                   Z-ADD     1             CODUT
003500921021     C                   CALL      'X§PARUT'
003600921021     C                   PARM                    UT§DSE
003700921021     C                   MOVEL     RAGUT         RSUT             20
003800921021     C                   MOVEL     REC80         CNCR80
003900921021     C*--- RICERCA CAPOCONTI------------------------------------------*
004000921021     C                   DO        50            X                 2 0
004100921021     C                   MOVE      TCU(X)        TCUDS
004200921021     C     F56           CABNE     'CG'          END1
004300921021     C     F4            COMP      '1'                                    21
004400921021     C     F4            COMP      '2'                                    22
004500921021     C     F4            COMP      '3'                                    23
004600921021     C     F4            COMP      '6'                                    27
004700921021     C** 1 CLIENTI   21
004800921021     C** 2 FORNITORI 22
004900921021     C** 3 AGENTI    23
005000921021     C     F3            COMP      '0'                                242425
005100921021     C     F3            COMP      'I'                                    26
005200921021     C** 0 ITALIA   25
005300921021     C** 1 ESTERO   24
005400921021     ** I CAPO CONTO IVA
005500921021     C   21
005600921021     CAN 24              Z-ADD     KCU(X)        KCE               4 0
005700921021     C   21
005800921021     CAN 25              Z-ADD     KCU(X)        KCI               4 0
005900921021     C   22
006000921021     CAN 24              Z-ADD     KCU(X)        KFE               4 0
006100921021     C   22
006200921021     CAN 25              Z-ADD     KCU(X)        KFI               4 0
006300921021     C   23
006400921021     CAN 24              Z-ADD     KCU(X)        KAE               4 0
006500921021     C   23
006600921021     CAN 25              Z-ADD     KCU(X)        KAI               4 0
006700921021     C   26              Z-ADD     KCU(X)        KIVA              4 0
006800921021     C   27              Z-ADD     KCU(X)        KBNA              4 0
006900921021     C     END1          TAG
007000921021     C                   END
007100921021     C*
007200921021     C     SIMFEL        COMP      *ZERO                                  90     SEDE
007300921021     C*
007400921021     C     KACO          KLIST                                                  *
007500921021     C                   KFLD                    CODUT                          *
007600921021     C                   KFLD                    KCI                            *
007700921021     C                   KFLD                    KSC               7 0          *
007800921021     C     KTB           KLIST                                                   *
007900921021     C                   KFLD                    CODUT                          *
008000921021     C                   KFLD                    COD               2            *
008100921021     C*
008200941118     C* LIBRERIA = QUELLA PASSATA
008300921021     C*
008400921021     C     *LIKE         DEFINE    TCLDFL        §DFL
008500921021     C     *LIKE         DEFINE    TCLNOT        §NOT
008600921021     C*
008700921021     C   90              MOVEL     '4C'          COD
008800921021     C  N90              MOVEL     '3K'          COD
008900921021     C*
009000921021     C     KTB           SETLL     TABEL
009100900509     C                   DO        *HIVAL
009200940110     C     KTB           READE     TABEL                                  58
009300911017     C  N58              DO
009400921021     C  N90              MOVEL     TBLUNI        DS3K
009500921021     C   90              MOVEL     TBLUNI        DS4C
009600921021     C                   MOVEL     TBLKEY        KSC
009700921023     C     KACO          CHAIN     CNACO00F                           54
009800921021     C*
009900921021     C   90              EXSR      SR4C
010000921021     C  N90              EXSR      SR3K
010100921021     C*
010200910205     C*
010300900619     C                   END
010400900509     C  N58              END
010500900509     C*
010600000000     C                   SETON                                        LR
010700910205     C*
010800921021     C/SPACE
010900921021     C*-----------------------------------------------------
011000921021     C* ARCHIVI IN FILIALE
011100921021     C*-----------------------------------------------------
011200921021     C     SR3K          BEGSR
011300040830      *
011400040830      * DATI CONSEGNA
011500921021     C                   MOVE      *BLANK        §FILE            10
011600941119     C                   MOVE      *BLANK        §FILES           10
011700921021     C                   MOVE      *BLANK        §MBR             10
011800921021     C                   MOVE      *BLANK        §MBRX             8
011900941118     C                   MOVE      *ZERO         §NRR             10 0
012000040830      *
012100921021     C     §3KCSD        IFEQ      'PC'                                         SOLO P.C.
012200921021     C     KSC           ANDEQ     §3KCKS                                       = CLI UNIF.
012300921021     C                   MOVEL     CST(4)        §DFL
012400921021     C                   MOVE      *BLANK        §NOT
012500921021     C                   MOVEL     §3KCGC        §NOT
012600941118     C                   MOVEL     'M'           §MBRX
012700941118     C                   MOVE      KSC           §MBRX
012800941118     C                   MOVEL     §MBRX         §MBR
012900941118     C                   MOVEL     'FNVAC00T'    §FILE                          NUOVO FILE
013000941119     C                   MOVEL     'FNVAC00S'    §FILES                         STORICO
013100941118     C                   MOVE      *ZERO         §NRR
013200941118     C                   CALL      'TRUL51C'
013300941118     C                   PARM                    §LIBRF
013400941118     C                   PARM                    §FILE
013500941118     C                   PARM                    §MBR
013600941118     C                   PARM                    §NRR
013700941118     C     §NRR          IFNE      *ZERO
013800941118     C     §NRR          ANDNE     9999999999
013900941118     C                   EXSR      SCRIVI
014000921021     C                   END
014100941118     C*
014200921021     C                   END
014300921021     C* DATI GIACENZA
014400921021     C                   MOVE      *BLANK        §FILE            10
014500941119     C                   MOVE      *BLANK        §FILES           10
014600921021     C                   MOVE      *BLANK        §MBR             10
014700921021     C                   MOVE      *BLANK        §MBRX             8
014800040830      *
014900921021     C     §3KGSD        IFEQ      'PC'                                         SOLO P.C.
015000921021     C     KSC           ANDEQ     §3KGKS                                       = CLI UNIF.
015100921021     C                   MOVEL     CST(2)        §DFL
015200921021     C                   MOVE      *BLANK        §NOT
015300921021     C                   MOVEL     §3KGGC        §NOT
015400941118     C                   MOVEL     'M'           §MBRX
015500941118     C                   MOVE      KSC           §MBRX
015600941118     C                   MOVEL     §MBRX         §MBR
015700941118     C                   MOVEL     'FNVAG00T'    §FILE                          NUOVO FILE
015800941119     C                   MOVEL     'FNVAG00S'    §FILES                         STORICO
015900941118     C                   MOVE      *ZERO         §NRR
016000941118     C                   CALL      'TRUL51C'
016100941118     C                   PARM                    §LIBRF
016200941118     C                   PARM                    §FILE
016300941118     C                   PARM                    §MBR
016400941118     C                   PARM                    §NRR
016500941118     C     §NRR          IFNE      *ZERO
016600941118     C     §NRR          ANDNE     9999999999
016700941118     C                   EXSR      SCRIVI
016800921021     C                   END
016900941118     C*
017000921021     C                   END
017100921021     C                   ENDSR
017200921021     C/SPACE
017300921021     C*-----------------------------------------------------
017400921021     C* ARCHIVI IN SEDE
017500921021     C*-----------------------------------------------------
017600921021     C     SR4C          BEGSR
017700921021     C* DATI CONSEGNA
017800921021     C                   MOVE      *BLANK        §FILE            10
017900941119     C                   MOVE      *BLANK        §FILES           10
018000921021     C                   MOVE      *BLANK        §MBR             10
018100921021     C                   MOVE      *BLANK        §MBRX             8
018200921021     C                   MOVE      *ZERO         §NRR             10 0
018300040830      *
018400921021     C     §4CTSD        IFEQ      'PC'                                         SOLO P.C.
018500921021     C     KSC           ANDEQ     §4CCKS                                       = CLI UNIF.
018600921021     C                   MOVEL     CST(1)        §DFL
018700921021     C                   MOVE      *BLANK        §NOT
018800921021     C                   MOVEL     §4CDTS        §NOT
018900941118     C                   MOVEL     'M'           §MBRX
019000941118     C                   MOVE      KSC           §MBRX
019100941118     C                   MOVEL     §MBRX         §MBR
019200941118     C                   MOVEL     'TNVAS00T'    §FILE                          NUOVO FILE
019300941119     C                   MOVEL     'TNVAS00S'    §FILES                         STORICO
019400941118     C                   MOVE      *ZERO         §NRR
019500941118     C                   CALL      'TRUL51C'
019600941118     C                   PARM                    §LIBRF
019700941118     C                   PARM                    §FILE
019800941118     C                   PARM                    §MBR
019900941118     C                   PARM                    §NRR
020000941118     C     §NRR          IFNE      *ZERO
020100941118     C     §NRR          ANDNE     9999999999
020200941118     C                   EXSR      SCRIVI
020300921021     C                   END
020400941118     C*
020500921021     C                   END
020600921021     C* DATI FATTURAZIONE
020700921021     C                   MOVE      *BLANK        §FILE            10
020800941119     C                   MOVE      *BLANK        §FILES           10
020900921021     C                   MOVE      *BLANK        §MBR             10
021000921021     C                   MOVE      *BLANK        §MBRX             8
021100921021     C                   MOVE      *ZERO         §NRR             10 0
021200921021     C     §4CFSD        IFEQ      'PC'                                         SOLO P.C.
021300060203     C     §4CFSD        OREQ      'W2'                                         PC + WW
021400921021     C     KSC           ANDEQ     §4CFKS                                       = CLI UNIF.
021500921021     C                   MOVEL     CST(3)        §DFL
021600921021     C                   MOVE      *BLANK        §NOT
021700921021     C                   MOVEL     §4CFNT        §NOT
021800941118     C                   MOVEL     'M'           §MBRX
021900941118     C                   MOVE      KSC           §MBRX
022000941118     C                   MOVEL     §MBRX         §MBR
022100000124     C                   MOVEL     'TIVAF00T'    §FILE                          NUOVO FILE
022200000124     C                   MOVEL     'TIVAF00S'    §FILES                         STORICO
022300941118     C                   MOVE      *ZERO         §NRR
022400941118     C                   CALL      'TRUL51C'
022500941118     C                   PARM                    §LIBRF
022600941118     C                   PARM                    §FILE
022700941118     C                   PARM                    §MBR
022800941118     C                   PARM                    §NRR
022900941118     C     §NRR          IFNE      *ZERO
023000941118     C     §NRR          ANDNE     9999999999
023100941118     C                   EXSR      SCRIVI
023200921021     C                   END
023300050126      * se in tabella 4C c'è impostato §4CRFO = 'X' devo scrivere anche il
023400050126      * nuovo membro sul file FLTCL
023500050126     c                   If        §4cRfo = 'X'
023600050126     c                   Movel     'M'           §mbrx
023700050126     c                   Move      §4cMbx        §mbrx
023800050126     c                   Movel     §mbrx         §mbr
023900050126     c                   Move      *zeros        §nrr
024000050126     c                   Call      'TRUL51C'
024100050126     c                   Parm                    §librf
024200050126     c                   Parm                    §file
024300050126     c                   Parm                    §mbr
024400050126     c                   Parm                    §nrr
024500050126     c                   If        §nrr <> *zeros and
024600050126     c                             §nrr <> 9999999999
024700050126     c                   ExSr      Scrivi
024800050126     c                   EndIf
024900050126     c                   EndIf
025000921021     C                   END
025100921021     C                   ENDSR
025200921021     C/SPACE
025300921021     C*-----------------------------------------------------
025400921021     C* SCRIVE RECORD
025500921021     C*-----------------------------------------------------
025600921021     C     SCRIVI        BEGSR
025700941118     C                   MOVEL     §LIBRF        TCLLIB
025800921021     C                   MOVEL     §FILE         TCLFIL
025900921021     C                   MOVEL     §MBR          TCLMBR
026000921021     C                   MOVE      *BLANK        TCLRMT
026100941119     C                   MOVEA     §LIBRF        CMD(30)
026200921021     C                   MOVEA     §FILE         CMD(43)
026300921023     C                   MOVEA     §MBR          CMD(56)
026400941119     C                   MOVEA     §FILES        CMD(69)
026500921023     C                   MOVEA     CMD           §CMD80           80
026600921021     C                   MOVE      *BLANK        TCLRMT
026700921021     C                   MOVEL     §CMD80        TCLRMT
026800921021     C                   Z-ADD     §NRR          TCLREC
026900921021     C                   MOVEL     ACORAG        TCLDCL
027000921021     C                   MOVEL     §DFL          TCLDFL
027100921021     C                   MOVEL     §NOT          TCLNOT
027200921021     C                   WRITE     FLTCL
027300921021     C                   ENDSR
027400921021** RMTCMD
027500040830CALL GAITRAOBJ/TRTC20C PARM('GAITRA201 ' 'FNVAC00T  ' 'M12345678 ' 'FNVAC00S  ')
027600921021** COSTANTI
027700921021CONTRASSEGNI
027800921021GIACENZE
027900921021FATTURAZIONE
028000921021DATI CONSEGNA
