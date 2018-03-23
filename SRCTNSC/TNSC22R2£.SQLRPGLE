000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300941103      * TNSC22R1                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600931012      *           STAMPA  ORDINATA X DATA/NUM. DOCUM.               *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000940218      * 01 -OVERFLOW                                                 *
001100940218      * 02 -TITOLO: 'FILIALI: STAMPA BOLLE C/A PER RIF.MITT. NUM.'   *
001200940218      * 20 -CONTRASSEGNO INCASSATO E NON PAGATO                      *
001300940218      * 78 -CHAIN SU AZORG.                                          *
001400940218      * 80 -READ SU FLCSB04L                                         *
001500940218      * 81 -CHAIN SU CNABI.                                          *
001600940218?     *--------------------------------------------------------------*
001700941103     FTABEL00F  IF   E           K DISK
001800020910     F                                     USROPN
001900941103     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002000020910     F                                     USROPN
002100931014     FCNABI00F  IF   E           K DISK
002200941103     FTNSC22P   O    E             PRINTER OFLIND(*IN01)
002300941103     D*--------------------------------------------------------------*
002400941103     D CCV             S              3    DIM(100)
002500000103     D DVA             S              1    DIM(100)
002600931011?     *--------------------------------------------------------------*
002700931011?     *  DS                                                          *
002800931011?     *--------------------------------------------------------------*
002900931011     D KPJBA         E DS
003000931011      *
003100010614     D YEURDS        E DS                  EXTNAME(YEURCODS)
003200941103     D TNSC20        E DS                  EXTNAME(TNSC20DS)
003300941103     D DSCV          E DS
003400000103     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003500000103     D DGED          E DS
003600020910     D FnCsb000      E DS
003700020910     D                                     EXTNAME(FNCSB00F)
003800020910     D                                     INZ
003900931011      *
004000941103     D WLBDA8          DS
004100941103     D  G02DAT                 1      8  0
004200941103     D  G02INV                 9     16  0
004300941103     D  G02ERR                17     17
004400941103     D  G02TGI                18     22  0
004500931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004600931011     D CNCR80        E DS
004700931011      *
004800931011     D                SDS
004900931011     D  NOMPG                  1     10
005000931014     D*
005100931014     D COST1           C                   CONST('CONTANTE            ')
005200941103?     *--------------------------------------------------------------*
005300941103?     *  FILE                                                        *
005400941103?     *--------------------------------------------------------------*
005500941103     IAZORG01L  AA
005600941103     I                             P    4    5 0ORGFIL
005700941103     I                                 14   33  ORGDES
005800931011      *---------------------------------------------------------------*
005900931011      *   C I C L O      P R I N C I P A L E                          *
006000931011      *---------------------------------------------------------------*
006100020910
006200020910     C/EXEC SQL
006300020910     C+ DECLARE FNCSB00F CURSOR FOR SELECT * FROM FNCSB00F WHERE CSBRMN =
006400020910     C+ :D20NRM AND CSBLNP = :D20LNP AND (CSBAAS * 10000) + CSBMGS
006500020910     C+ BETWEEN :D20SDD AND :D20SDA FOR READ ONLY
006600020910     C/END-EXEC
006700020910
006800020910     C/EXEC SQL
006900020910     C+ OPEN FNCSB00F
007000020910     C/END-EXEC
007100020910
007200020910     C/EXEC SQL
007300020910     C+ FETCH NEXT FROM FNCSB00F INTO :FnCsb000
007400020910     C/END-EXEC
007500020910
007600020910     C                   EVAL      *IN80 = (SQLCod = 100 OR SQLCod < 0)
007700020910
007800931012     C     *IN80         DOWEQ     '0'
007900931012     C                   EXSR      STAMPA
008000020910
008100020910     C/EXEC SQL
008200020910     C+ FETCH NEXT FROM FNCSB00F INTO :FnCsb000
008300020910     C/END-EXEC
008400020910
008500020910     C                   EVAL      *IN80 = (SQLCod = 100 OR SQLCod < 0)
008600020910
008700931011     C                   END
008800020910
008900020910     C/EXEC SQL
009000020910     C+ CLOSE FNCSB00F
009100020910     C/END-EXEC
009200020910
009300931013     C     TOT           IFGT      0
009400931013     C   01              WRITE     TES00
009500931013     C   01              SETOFF                                       01
009600931013     C                   WRITE     TOT00
009700931013     C                   END
009800000103     C* Se ci sono dei C/Assegni in Euro stampo il totale relativo
009900000103     C     TOTEU         IFGT      0
010000000103     C   01              WRITE     TES00
010100000103     C   01              SETOFF                                       01
010200000103     C                   WRITE     TOTE0
010300000103     C                   END
010400941103     C*
010500941103     C     TOTV          IFGT      0
010600941103     C   01              WRITE     TES00
010700941103     C   01              SETOFF                                       01
010800011002     C                   MOVEL     §GEDCN        VALV
010900941103     C                   WRITE     TOTV0
011000941103     C                   END
011100931011     C*
011200941103     C                   MOVEL     TNSC20        KPJBU
011300931011     C                   SETON                                        LR
011400931011     C*----------------------------------------------------*
011500931012     C*   STAMPA: Trasferimento dati da file a stampa      *
011600931011     C*----------------------------------------------------*
011700931012     C     STAMPA        BEGSR
011800931011     C*
011900931013     C* SOLO CONTRASSEGNI INCASSATI NON PAGATI
012000941103     C     D20TCR        IFEQ      'I'
012100931013     C     CSBDDC        COMP      0                                      20
012200931013     C  N20CSBDDP        COMP      0                                  2020
012300931013     C                   END
012400931013     C* SOLO CONTRASSEGNI PAGATI
012500941103     C     D20TCR        IFEQ      'P'
012600931013     C     CSBNDP        COMP      0                                      20
012700931013     C                   END
012800931013     C*  Test Tipo Contrassegni
012900931013     C     *IN20         IFEQ      '0'
013000931012     C                   MOVEL     CSBLNA        LNA
013100931013     C                   MOVEL     CSBLNP        LNP
013200941103     C                   Z-ADD     CSBAAS        AA
013300931012     C                   MOVE      CSBNRA        NDC
013400941103     C                   MOVE      CSBRMN        RNM
013500941103     C                   MOVE      CSBDTE        XDTAMG
013600941103     C                   Z-ADD     0             XDTGMA
013700941103     C                   Z-ADD     2             XDTSTA
013800941103     C                   CALL      'XDATA'       PLDAT
013900941103     C                   MOVE      XDTGMA        DDC
014000931013     C                   MOVE      CSBNRS        NRS
014100931013     C                   MOVE      CSBNSP        NSP
014200941103     C                   MOVE      CSBAAS        WAA2              2 0
014300941103     C                   MOVEL     WAA2          XDTAMG
014400941103     C                   MOVE      CSBMGS        XDTAMG
014500941103     C                   Z-ADD     0             XDTGMA
014600941103     C                   Z-ADD     2             XDTSTA
014700941103     C                   CALL      'XDATA'       PLDAT
014800941103     C                   MOVEL     XDTGMA        DSP
014900941103     C                   MOVEL     CSBVCA        VCA
015000011002     C     VCA           IFEQ      *BLANK
015100011002     C                   MOVE      'ITL'         VCA
015200011002     C                   END
015300941103     C                   Z-ADD     CSBCAS        IMP
015400941103     C* Controllo la divisa
015500941103     C     VCA           IFNE      *BLANKS
015600941103     C                   Z-ADD     1             XX
015700941103     C     VCA           LOOKUP    CCV(XX)                                32
015800941103     C                   END
015900941103     C* Somma nel totale corretto
016000941103     C     VCA           IFEQ      *BLANKS
016100011001     C     VCA           OREQ      §GEDCN
016200941103     C                   ADD       CSBCAS        TOT
016300941103     C                   ELSE
016400000103     C     DVA(XX)       IFEQ      'S'
016500000103     C     *IN32         ANDEQ     '1'
016600000103     C                   ADD       CSBCAS        TOTEU
016700000103     C                   ELSE
016800010614     C* Per convertire gli importi in valute estere utilizzo
016900010614     C* la routine YEURCO in quanto il cambio indicato sul
017000010614     C* file dei C/assegni di filiale non è affidabile
017100010614     C                   CLEAR                   YEURDS
017200010614     C                   MOVEL     VCA           YECDVI
017300010614     C                   Z-ADD     CSBCAS        YECIMI
017400011001     C                   MOVEL     §GEDCN        YECDVO
017500010614     C* passo al pgm io il nr. dei decimali della divisa corrente
017600010614     C* in base a quanto memorizzato sulla tabella CV
017700010614     C                   MOVE      NRDEC         YECDCO
017800010614     C                   MOVEL     'H'           YECTAR
017900010614     C                   CALL      'YEURCO'
018000010614     C                   PARM                    YEURDS
018100010614     C                   Z-ADD     YECIMO        IMPV             13 3
018200010614     C                   ADD       IMPV          TOTV
018300010614     C                   END
018400000103     C                   END
018500941103     C*
018600931013     C                   MOVE      CSBABI        ABI
018700931013     C                   MOVE      CSBCAB        CAB
018800931014     C                   MOVEL     *BLANKS       BAI
018900931014     C     NDC           IFEQ      *BLANKS
019000931014     C                   MOVEL     COST1         BAI
019100931014     C                   END
019200931012     C     KEYABI        CHAIN     CNABI00F                           81
019300931012     C  N81              MOVEL     ABIIST        BAI
019400931013     C                   MOVEL     CSBTPA        ASS
019500931013     C                   MOVE      CSBTPI        ASS
019600931013     C                   MOVE      CSBCDI        CDM
019700931013     C                   MOVEL     CSBRSM        RGM
019800931013     C                   MOVEL     CSBRSD        RGD
019900931014     C                   MOVE      *BLANK        DOC
020000931013     C*  Tipo Documento
020100931013     C     CSBFPC        IFEQ      '2'
020200931013     C                   MOVE      'BON.'        DOC
020300931013     C                   ELSE
020400931014     C     CSBPAB        IFEQ      99999
020500931014     C     CSBPCA        ANDEQ     99999
020600931014     C                   MOVE      'LETT'        DOC
020700931014     C                   ELSE
020800931014     C     CSBPAB        IFNE      0
020900931014     C     CSBPCA        ORNE      0
021000931013     C                   MOVE      'ASS.'        DOC
021100931013     C                   END
021200931013     C                   END
021300931014     C                   END
021400931013     C*
021500941104     C                   MOVE      CSBDDP        XDTAMG
021600941104     C                   Z-ADD     0             XDTGMA
021700941104     C                   Z-ADD     2             XDTSTA
021800931013     C                   CALL      'XDATA'       PLDAT
021900931013     C                   MOVE      XDTGMA        DDP
022000931013     C                   MOVE      CSBNDP        NDP
022100931013     C                   MOVEL     *BLANK        BAP
022200931013     C                   MOVE      CSBPAB        ABI
022300931013     C                   MOVE      CSBPCA        CAB
022400931013     C     KEYABI        CHAIN     CNABI00F                           81
022500931013     C  N81              MOVEL     ABIIST        BAP
022600931013     C                   MOVE      CSBDGC        XDTAMG
022700931013     C                   Z-ADD     0             XDTGMA
022800931013     C                   Z-ADD     2             XDTSTA
022900931013     C                   CALL      'XDATA'       PLDAT
023000931013     C                   MOVE      XDTGMA        DGC
023100931112     C                   MOVEL     *ZERO         LNPP
023200931112     C                   MOVEL     *ZERO         AAP
023300931112     C                   MOVE      *ZERO         NRSP
023400931112     C                   MOVE      *ZERO         NSPP
023500931112     C*
023600931112     C*  Stampo riferimnti spedizione pagamento contrassegno solo se
023700931112     C*  diverso da originale
023800020910     C     CSBLNP        IFNE      CSBPLP
023900020910     C     CSBAAS        ORNE      CSBPAA
024000020910     C     CSBNRS        ORNE      CSBPNS
024100020910     C     CSBNSP        ORNE      CSBPSP
024200931013     C                   MOVEL     CSBPLP        LNPP
024300931013     C                   MOVEL     CSBPAA        AAP
024400931112     C                   MOVE      CSBPNS        NRSP
024500931112     C                   MOVE      CSBPSP        NSPP
024600931112     C                   END
024700931013     C   01              WRITE     TES00
024800931013     C   01              SETOFF                                       01
024900931013     C                   WRITE     DET00
025000931013     C                   END
025100931012     C*
025200931011     C                   ENDSR
025300931011      *---------------------------------------------------------------*
025400941103      *  *INZSR: OPERAZIONI INIZIALI                                  *
025500931011      *---------------------------------------------------------------*
025600941103     C     *INZSR        BEGSR
025700931011      *---------------------------------------------------------------*
025800941103     C     *ENTRY        PLIST
025900931011     C                   PARM                    KPJBA
026000941103     C                   MOVEL     KPJBU         TNSC20
026100931012     C*-
026200931012     C* PLIST per richiamo Routine XDATA
026300931012     C     PLDAT         PLIST
026400931012     C                   PARM                    XDTAMG            6 0
026500931012     C                   PARM                    XDTGMA            6 0
026600931012     C                   PARM                    XDTSTA            1 0
026700931012     C*
026800931011     C                   Z-ADD     1             CODUT
026900931011     C                   CALL      'X§PARUT'
027000931011     C                   PARM                    UT§DSE
027100931011     C                   MOVEL     RAGUT         RSUT             20
027200931011     C                   MOVEL     REC80         CNCR80
027300931011     C                   MOVEL     KNMUS         §SEI              6
027400931011     C*
027500931012     C     KEYRMN        KLIST
027600941103     C                   KFLD                    D20NRM
027700941103     C*
027800941103     C     KEYABI        KLIST
027900941103     C                   KFLD                    ABI               5 0
028000941103     C                   KFLD                    CAB               5 0
028100941103     C*
028200941103     C     KTAB          KLIST
028300941103     C                   KFLD                    KKUT
028400941103     C                   KFLD                    KCOD
028500941103     C                   Z-ADD     1             KKUT
028600941103     C*----------------------------------------------------*
028700941103     C*  Definizione variabili
028800941103     C     *LIKE         DEFINE    TBLKUT        KKUT
028900941103     C     *LIKE         DEFINE    TBLCOD        KCOD
029000941103     C*----------------------------------------------------*
029100931012     C                   MOVEL     'N'           £FINE             1
029200941103     C                   MOVE      SIMFEL        ORGFIL
029300020910     C                   OPEN      AZORG01L
029400941103     C     ORGFIL        CHAIN     AZORG01L                           78
029500020910     C                   CLOSE     AZORG01L
029600931014     C  N78              MOVEL     ORGDES        FILIAL
029700931012     C*----------------------------------------------------*
029800931013     C*  Scrittura Prima pagina
029900941103     C                   CLEAR                   WLBDA8
030000931013     C                   MOVEL     NOMPG         NOMPGM
030100941103     C                   MOVE      D20INT        S22SEL
030200941103     C                   MOVE      D20ANN        S22AAS
030300941103     C                   MOVE      D20LNP        S22LNP
030400941103     C                   MOVE      D20NSR        S22NSR
030500941103     C                   MOVE      D20NRM        S22NRM
030600941103     C                   MOVE      D20SDD        G02INV
030700941103     C                   Z-ADD     0             G02DAT
030800941103     C                   MOVEL     '3'           G02ERR
030900941103     C                   CALL      'XSRDA8'
031000941103     C                   PARM                    WLBDA8
031100941103     C                   MOVE      G02DAT        S22SDD
031200941103     C                   MOVE      D20SDA        G02INV
031300941103     C                   Z-ADD     0             G02DAT
031400941103     C                   MOVEL     '3'           G02ERR
031500941103     C                   CALL      'XSRDA8'
031600941103     C                   PARM                    WLBDA8
031700941103     C                   MOVE      G02DAT        S22SDA
031800941103     C                   MOVE      D20SCD        S22SCD
031900941103     C                   MOVEL     D20SRA        S22SRS
032000941103     C                   MOVE      D20PDD        G02INV
032100941103     C                   Z-ADD     0             G02DAT
032200941103     C                   MOVEL     '3'           G02ERR
032300941103     C                   CALL      'XSRDA8'
032400941103     C                   PARM                    WLBDA8
032500941103     C                   MOVE      G02DAT        S22PDD
032600941103     C                   MOVE      D20PDA        G02INV
032700941103     C                   Z-ADD     0             G02DAT
032800941103     C                   MOVEL     '3'           G02ERR
032900941103     C                   CALL      'XSRDA8'
033000941103     C                   PARM                    WLBDA8
033100941103     C                   MOVE      G02DAT        S22PDA
033200941103     C                   MOVE      D20PND        S22PND
033300941103     C                   MOVE      D20PTN        S22PTI
033400941103     C                   MOVE      D20PCD        S22PCD
033500941103     C                   MOVEL     D20PRA        S22PRS
033600941103     C                   MOVE      D20TCR        S22TCA
033700931013     C*
033800931013     C     S22TCA        IFEQ      ' '
033900931013     C                   MOVEL     'TUTTI'       S22DCA
034000931013     C                   ELSE
034100931013     C     S22TCA        IFEQ      'I'
034200931013     C                   MOVEL     'INCASSAT'    S22DCA
034300931013     C                   MOVE      'I      '     S22DCA
034400931013     C                   ELSE
034500931013     C     S22TCA        IFEQ      'P'
034600931013     C                   MOVEL     'PAGATI  '    S22DCA
034700931013     C                   END
034800931013     C                   END
034900931013     C                   END
035000931013     C*
035100931014     C                   SETON                                        02
035200931013     C                   MOVEL     *ALL'-'       CMP198
035300931014     C                   MOVEL     *ALL'-'       CM1198
035400931013     C                   WRITE     PAG00
035500931013     C*
035600941103     C                   SETON                                        01
035700941103     C*----------------------------------------------------*
035800000103     C*  RECUPERO DIVISA MONETA CORRENTE
035900000103     C                   CLEAR                   DGED
036000000103     C                   CLEAR                   DSBS02
036100000103     C                   MOVEL     'C'           T02MOD
036200000103     C                   MOVEL     KNSIF         T02SIF
036300000103     C                   MOVEL     'GED'         T02COD
036400000103     C                   MOVEL     '1'           T02KE1
036500000103     C                   CALL      'TIBS02R'
036600000103     C                   PARM                    KPJBA
036700000103     C                   PARM                    DSBS02
036800000103     C*
036900000103    3C     T02ERR        IFEQ      ' '
037000000103     C                   MOVEL     T02UNI        DGED
037100011001     C                   MOVEL     §GEDCN        DIVCOR
037200000103    3C                   ENDIF
037300000103     C*----------------------------------------------------*
037400000103     C* Carico tabella cambi/divise
037500020910     C                   OPEN      TABEL00F
037600000103     C                   Z-ADD     0             XX                3 0
037700000103     C                   MOVEL     'CV'          KCOD
037800000103     C     KTAB          CHAIN     TABEL                              31
037900000103     C     *IN31         DOWEQ     '0'
038000000103     C     XX            ANDLT     100
038100000103     C                   MOVEL     TBLKEY        WDIV              3
038200000103     C     TBLFLG        IFEQ      ' '
038300000103     C                   ADD       1             XX
038400000103     C                   MOVEL     TBLKEY        CCV(XX)
038500000103     C                   MOVEL     TBLUNI        DSCV
038600000103     C                   MOVEL     §CVDVA        DVA(XX)
038700000103     C     §CVDVA        IFEQ      'S'
038800011001     C     WDIV          ANDNE     §GEDCN
038900000103     C                   MOVEL     WDIV          DIVALT
039000000103     C                   END
039100000103     C* Memorizzo nr. decimali della divisa corrente
039200011001     C     WDIV          IFEQ      §GEDCN
039300000103     C                   Z-ADD     §CVDEC        NRDEC             2 0
039400000103     C                   END
039500000103     C                   END
039600000103     C     KTAB          READE     TABEL                                  31
039700000103     C                   END
039800020910     C                   CLOSE     TABEL00F
039900941103     C*----------------------------------------------------*
040000931011     C                   ENDSR
