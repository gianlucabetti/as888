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
002200120213     Ftncsm03l  IF   E           K DISK
002300941103     FTNSC22P   O    E             PRINTER OFLIND(*IN01)
002400941103     D*--------------------------------------------------------------*
002500941103     D CCV             S              3    DIM(100)
002600000103     D DVA             S              1    DIM(100)
002700931011?     *--------------------------------------------------------------*
002800931011?     *  DS                                                          *
002900931011?     *--------------------------------------------------------------*
003000931011     D KPJBA         E DS
003100931011      *
003200010614     D YEURDS        E DS                  EXTNAME(YEURCODS)
003300941103     D TNSC20        E DS                  EXTNAME(TNSC20DS)
003400941103     D DSCV          E DS
003500000103     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003600000103     D DGED          E DS
003700020910     D FnCsb000      E DS
003800020910     D                                     EXTNAME(FNCSB00F)
003900020910     D                                     INZ
004000931011      *
004100941103     D WLBDA8          DS
004200941103     D  G02DAT                 1      8  0
004300941103     D  G02INV                 9     16  0
004400941103     D  G02ERR                17     17
004500941103     D  G02TGI                18     22  0
004600931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004700931011     D CNCR80        E DS
004800931011      *
004900931011     D                SDS
005000931011     D  NOMPG                  1     10
005100931014     D*
005200931014     D COST1           C                   CONST('CONTANTE            ')
005300941103?     *--------------------------------------------------------------*
005400941103?     *  FILE                                                        *
005500941103?     *--------------------------------------------------------------*
005600941103     IAZORG01L  AA
005700941103     I                             P    4    5 0ORGFIL
005800941103     I                                 14   33  ORGDES
005900931011      *---------------------------------------------------------------*
006000931011      *   C I C L O      P R I N C I P A L E                          *
006100931011      *---------------------------------------------------------------*
006200020910
006300020910     C/EXEC SQL
006400020910     C+ DECLARE FNCSB00F CURSOR FOR SELECT * FROM FNCSB00F WHERE CSBRMN =
006500020910     C+ :D20NRM AND CSBLNP = :D20LNP AND (CSBAAS * 10000) + CSBMGS
006600020910     C+ BETWEEN :D20SDD AND :D20SDA FOR READ ONLY
006700020910     C/END-EXEC
006800020910
006900020910     C/EXEC SQL
007000020910     C+ OPEN FNCSB00F
007100020910     C/END-EXEC
007200020910
007300020910     C/EXEC SQL
007400020910     C+ FETCH NEXT FROM FNCSB00F INTO :FnCsb000
007500020910     C/END-EXEC
007600020910
007700020910     C                   EVAL      *IN80 = (SQLCod = 100 OR SQLCod < 0)
007800020910
007900931012     C     *IN80         DOWEQ     '0'
008000931012     C                   EXSR      STAMPA
008100020910
008200020910     C/EXEC SQL
008300020910     C+ FETCH NEXT FROM FNCSB00F INTO :FnCsb000
008400020910     C/END-EXEC
008500020910
008600020910     C                   EVAL      *IN80 = (SQLCod = 100 OR SQLCod < 0)
008700020910
008800931011     C                   END
008900020910
009000020910     C/EXEC SQL
009100020910     C+ CLOSE FNCSB00F
009200020910     C/END-EXEC
009300020910
009400931013     C     TOT           IFGT      0
009500931013     C   01              WRITE     TES00
009600931013     C   01              SETOFF                                       01
009700931013     C                   WRITE     TOT00
009800931013     C                   END
009900000103     C* Se ci sono dei C/Assegni in Euro stampo il totale relativo
010000000103     C     TOTEU         IFGT      0
010100000103     C   01              WRITE     TES00
010200000103     C   01              SETOFF                                       01
010300000103     C                   WRITE     TOTE0
010400000103     C                   END
010500941103     C*
010600941103     C     TOTV          IFGT      0
010700941103     C   01              WRITE     TES00
010800941103     C   01              SETOFF                                       01
010900011002     C                   MOVEL     §GEDCN        VALV
011000941103     C                   WRITE     TOTV0
011100941103     C                   END
011200931011     C*
011300941103     C                   MOVEL     TNSC20        KPJBU
011400931011     C                   SETON                                        LR
011500931011     C*----------------------------------------------------*
011600931012     C*   STAMPA: Trasferimento dati da file a stampa      *
011700931011     C*----------------------------------------------------*
011800931012     C     STAMPA        BEGSR
011900931011     C*
012000931013     C* SOLO CONTRASSEGNI INCASSATI NON PAGATI
012100941103     C     D20TCR        IFEQ      'I'
012200931013     C     CSBDDC        COMP      0                                      20
012300931013     C  N20CSBDDP        COMP      0                                  2020
012400931013     C                   END
012500931013     C* SOLO CONTRASSEGNI PAGATI
012600941103     C     D20TCR        IFEQ      'P'
012700931013     C     CSBNDP        COMP      0                                      20
012800931013     C                   END
012900931013     C*  Test Tipo Contrassegni
013000931013     C     *IN20         IFEQ      '0'
013100931012     C                   MOVEL     CSBLNA        LNA
013200931013     C                   MOVEL     CSBLNP        LNP
013300941103     C                   Z-ADD     CSBAAS        AA
013400941103     C                   MOVE      CSBRMN        RNM
013500931013     C                   MOVE      CSBNRS        NRS
013600931013     C                   MOVE      CSBNSP        NSP
013700941103     C                   MOVE      CSBAAS        WAA2              2 0
013800941103     C                   MOVEL     WAA2          XDTAMG
013900941103     C                   MOVE      CSBMGS        XDTAMG
014000941103     C                   Z-ADD     0             XDTGMA
014100941103     C                   Z-ADD     2             XDTSTA
014200941103     C                   CALL      'XDATA'       PLDAT
014300941103     C                   MOVEL     XDTGMA        DSP
014400941103     C                   MOVEL     CSBVCA        VCA
014500011002     C     VCA           IFEQ      *BLANK
014600011002     C                   MOVE      'ITL'         VCA
014700011002     C                   END
014800941103     C                   Z-ADD     CSBCAS        IMP
014900941103     C* Controllo la divisa
015000941103     C     VCA           IFNE      *BLANKS
015100941103     C                   Z-ADD     1             XX
015200941103     C     VCA           LOOKUP    CCV(XX)                                32
015300941103     C                   END
015400941103     C* Somma nel totale corretto
015500941103     C     VCA           IFEQ      *BLANKS
015600011001     C     VCA           OREQ      §GEDCN
015700941103     C                   ADD       CSBCAS        TOT
015800941103     C                   ELSE
015900000103     C     DVA(XX)       IFEQ      'S'
016000000103     C     *IN32         ANDEQ     '1'
016100000103     C                   ADD       CSBCAS        TOTEU
016200000103     C                   ELSE
016300010614     C* Per convertire gli importi in valute estere utilizzo
016400010614     C* la routine YEURCO in quanto il cambio indicato sul
016500010614     C* file dei C/assegni di filiale non è affidabile
016600010614     C                   CLEAR                   YEURDS
016700010614     C                   MOVEL     VCA           YECDVI
016800010614     C                   Z-ADD     CSBCAS        YECIMI
016900011001     C                   MOVEL     §GEDCN        YECDVO
017000010614     C* passo al pgm io il nr. dei decimali della divisa corrente
017100010614     C* in base a quanto memorizzato sulla tabella CV
017200010614     C                   MOVE      NRDEC         YECDCO
017300010614     C                   MOVEL     'H'           YECTAR
017400010614     C                   CALL      'YEURCO'
017500010614     C                   PARM                    YEURDS
017600010614     C                   Z-ADD     YECIMO        IMPV             13 3
017700010614     C                   ADD       IMPV          TOTV
017800010614     C                   END
017900000103     C                   END
018000941103     C*
018100120220     c                   if        %subst(csbnra: 10: 1)=' '
018200120213     c                   setoff                                       44
018300120210     C                   MOVE      CSBNRA        NDC
018400120210     C                   MOVE      CSBDTE        XDTAMG
018500120210     C                   Z-ADD     0             XDTGMA
018600120210     C                   Z-ADD     2             XDTSTA
018700120210     C                   CALL      'XDATA'       PLDAT
018800120210     C                   MOVE      XDTGMA        DDC
018900931013     C                   MOVE      CSBABI        ABI
019000931013     C                   MOVE      CSBCAB        CAB
019100931014     C                   MOVEL     *BLANKS       BAI
019200931012     C     KEYABI        CHAIN     CNABI00F                           81
019300931012     C  N81              MOVEL     ABIIST        BAI
019400931013     C                   MOVEL     CSBTPA        ASS
019500931013     C                   MOVE      CSBTPI        ASS
019600120213     c                   end
019700120213     c*
019800120213     C     NDC           IFEQ      *BLANKS
019900120213     C                   MOVEL     COST1         BAI
020000120213     C                   END
020100931013     C                   MOVE      CSBCDI        CDM
020200931013     C                   MOVEL     CSBRSM        RGM
020300931013     C                   MOVEL     CSBRSD        RGD
020400931014     C                   MOVE      *BLANK        DOC
020500931013     C*  Tipo Documento
020600931013     C     CSBFPC        IFEQ      '2'
020700931013     C                   MOVE      'BON.'        DOC
020800931013     C                   ELSE
020900931014     C     CSBPAB        IFEQ      99999
021000931014     C     CSBPCA        ANDEQ     99999
021100931014     C                   MOVE      'LETT'        DOC
021200931014     C                   ELSE
021300931014     C     CSBPAB        IFNE      0
021400931014     C     CSBPCA        ORNE      0
021500931013     C                   MOVE      'ASS.'        DOC
021600931013     C                   END
021700931013     C                   END
021800931014     C                   END
021900931013     C*
022000941104     C                   MOVE      CSBDDP        XDTAMG
022100941104     C                   Z-ADD     0             XDTGMA
022200941104     C                   Z-ADD     2             XDTSTA
022300931013     C                   CALL      'XDATA'       PLDAT
022400931013     C                   MOVE      XDTGMA        DDP
022500931013     C                   MOVE      CSBNDP        NDP
022600931013     C                   MOVEL     *BLANK        BAP
022700931013     C                   MOVE      CSBPAB        ABI
022800931013     C                   MOVE      CSBPCA        CAB
022900931013     C     KEYABI        CHAIN     CNABI00F                           81
023000931013     C  N81              MOVEL     ABIIST        BAP
023100931013     C                   MOVE      CSBDGC        XDTAMG
023200931013     C                   Z-ADD     0             XDTGMA
023300931013     C                   Z-ADD     2             XDTSTA
023400931013     C                   CALL      'XDATA'       PLDAT
023500931013     C                   MOVE      XDTGMA        DGC
023600931112     C                   MOVEL     *ZERO         LNPP
023700931112     C                   MOVEL     *ZERO         AAP
023800931112     C                   MOVE      *ZERO         NRSP
023900931112     C                   MOVE      *ZERO         NSPP
024000931112     C*
024100931112     C*  Stampo riferimnti spedizione pagamento contrassegno solo se
024200931112     C*  diverso da originale
024300020910     C     CSBLNP        IFNE      CSBPLP
024400020910     C     CSBAAS        ORNE      CSBPAA
024500020910     C     CSBNRS        ORNE      CSBPNS
024600020910     C     CSBNSP        ORNE      CSBPSP
024700931013     C                   MOVEL     CSBPLP        LNPP
024800931013     C                   MOVEL     CSBPAA        AAP
024900931112     C                   MOVE      CSBPNS        NRSP
025000931112     C                   MOVE      CSBPSP        NSPP
025100931112     C                   END
025200120220     c                   if        %subst(csbnra: 10: 1)=' '
025300931013     C   01              WRITE     TES00
025400931013     C   01              SETOFF                                       01
025500931013     C                   WRITE     DET00
025600120213     c                   else
025700120213     c                   exsr      srass
025800931013     C                   END
025900120213     C                   END
026000931012     C*
026100931011     C                   ENDSR
026200931011      *---------------------------------------------------------------*
026300120213      *  stampa rif assegni
026400931011      *---------------------------------------------------------------*
026500120213     C     srass         BEGSR
026600120213     c                   setoff                                       44
026700120213     c     csbnra        setll     tncsm03l
026800120213     c                   do        *hival
026900120213     c     csbnra        reade     tncsm03l
027000120213     c                   if        %eof(tncsm03l)
027100120213     c                   leave
027200120213     c                   end
027300120213     C                   MOVE      CSmNRA        NDC
027400120213     C                   MOVE      CSmDTE        XDTAMG
027500120213     C                   Z-ADD     0             XDTGMA
027600120213     C                   Z-ADD     2             XDTSTA
027700120213     C                   CALL      'XDATA'       PLDAT
027800120213     C                   MOVE      XDTGMA        DDC
027900120213     C                   MOVE      CSmABI        ABI
028000120213     C                   MOVE      CSmCAB        CAB
028100120213     C                   MOVEL     *BLANKS       BAI
028200120213     C     KEYABI        CHAIN     CNABI00F                           81
028300120213     C  N81              MOVEL     ABIIST        BAI
028400120213     C                   MOVEL     CSmTPA        ASS
028500120213     C                   MOVE      CSmTPI        ASS
028600120213     C   01              WRITE     TES00
028700120213     C   01              SETOFF                                       01
028800120213     C   01              SETOFF                                       44
028900120213     C                   WRITE     DET00
029000120213     c                   seton                                        44
029100120213     c                   enddo
029200120213     C*
029300120213     C                   ENDSR
029400120213      *---------------------------------------------------------------*
029500120213      *  *INZSR: OPERAZIONI INIZIALI                                  *
029600120213      *---------------------------------------------------------------*
029700120213     C     *INZSR        BEGSR
029800931011      *---------------------------------------------------------------*
029900941103     C     *ENTRY        PLIST
030000931011     C                   PARM                    KPJBA
030100941103     C                   MOVEL     KPJBU         TNSC20
030200931012     C*-
030300931012     C* PLIST per richiamo Routine XDATA
030400931012     C     PLDAT         PLIST
030500931012     C                   PARM                    XDTAMG            6 0
030600931012     C                   PARM                    XDTGMA            6 0
030700931012     C                   PARM                    XDTSTA            1 0
030800931012     C*
030900931011     C                   Z-ADD     1             CODUT
031000931011     C                   CALL      'X§PARUT'
031100931011     C                   PARM                    UT§DSE
031200931011     C                   MOVEL     RAGUT         RSUT             20
031300931011     C                   MOVEL     REC80         CNCR80
031400931011     C                   MOVEL     KNMUS         §SEI              6
031500931011     C*
031600931012     C     KEYRMN        KLIST
031700941103     C                   KFLD                    D20NRM
031800941103     C*
031900941103     C     KEYABI        KLIST
032000941103     C                   KFLD                    ABI               5 0
032100941103     C                   KFLD                    CAB               5 0
032200941103     C*
032300941103     C     KTAB          KLIST
032400941103     C                   KFLD                    KKUT
032500941103     C                   KFLD                    KCOD
032600941103     C                   Z-ADD     1             KKUT
032700941103     C*----------------------------------------------------*
032800941103     C*  Definizione variabili
032900941103     C     *LIKE         DEFINE    TBLKUT        KKUT
033000941103     C     *LIKE         DEFINE    TBLCOD        KCOD
033100941103     C*----------------------------------------------------*
033200931012     C                   MOVEL     'N'           £FINE             1
033300941103     C                   MOVE      SIMFEL        ORGFIL
033400020910     C                   OPEN      AZORG01L
033500941103     C     ORGFIL        CHAIN     AZORG01L                           78
033600020910     C                   CLOSE     AZORG01L
033700931014     C  N78              MOVEL     ORGDES        FILIAL
033800931012     C*----------------------------------------------------*
033900931013     C*  Scrittura Prima pagina
034000941103     C                   CLEAR                   WLBDA8
034100931013     C                   MOVEL     NOMPG         NOMPGM
034200941103     C                   MOVE      D20INT        S22SEL
034300941103     C                   MOVE      D20ANN        S22AAS
034400941103     C                   MOVE      D20LNP        S22LNP
034500941103     C                   MOVE      D20NSR        S22NSR
034600941103     C                   MOVE      D20NRM        S22NRM
034700941103     C                   MOVE      D20SDD        G02INV
034800941103     C                   Z-ADD     0             G02DAT
034900941103     C                   MOVEL     '3'           G02ERR
035000941103     C                   CALL      'XSRDA8'
035100941103     C                   PARM                    WLBDA8
035200941103     C                   MOVE      G02DAT        S22SDD
035300941103     C                   MOVE      D20SDA        G02INV
035400941103     C                   Z-ADD     0             G02DAT
035500941103     C                   MOVEL     '3'           G02ERR
035600941103     C                   CALL      'XSRDA8'
035700941103     C                   PARM                    WLBDA8
035800941103     C                   MOVE      G02DAT        S22SDA
035900941103     C                   MOVE      D20SCD        S22SCD
036000941103     C                   MOVEL     D20SRA        S22SRS
036100941103     C                   MOVE      D20PDD        G02INV
036200941103     C                   Z-ADD     0             G02DAT
036300941103     C                   MOVEL     '3'           G02ERR
036400941103     C                   CALL      'XSRDA8'
036500941103     C                   PARM                    WLBDA8
036600941103     C                   MOVE      G02DAT        S22PDD
036700941103     C                   MOVE      D20PDA        G02INV
036800941103     C                   Z-ADD     0             G02DAT
036900941103     C                   MOVEL     '3'           G02ERR
037000941103     C                   CALL      'XSRDA8'
037100941103     C                   PARM                    WLBDA8
037200941103     C                   MOVE      G02DAT        S22PDA
037300941103     C                   MOVE      D20PND        S22PND
037400941103     C                   MOVE      D20PTN        S22PTI
037500941103     C                   MOVE      D20PCD        S22PCD
037600941103     C                   MOVEL     D20PRA        S22PRS
037700941103     C                   MOVE      D20TCR        S22TCA
037800931013     C*
037900931013     C     S22TCA        IFEQ      ' '
038000931013     C                   MOVEL     'TUTTI'       S22DCA
038100931013     C                   ELSE
038200931013     C     S22TCA        IFEQ      'I'
038300931013     C                   MOVEL     'INCASSAT'    S22DCA
038400931013     C                   MOVE      'I      '     S22DCA
038500931013     C                   ELSE
038600931013     C     S22TCA        IFEQ      'P'
038700931013     C                   MOVEL     'PAGATI  '    S22DCA
038800931013     C                   END
038900931013     C                   END
039000931013     C                   END
039100931013     C*
039200931014     C                   SETON                                        02
039300931013     C                   MOVEL     *ALL'-'       CMP198
039400931014     C                   MOVEL     *ALL'-'       CM1198
039500931013     C                   WRITE     PAG00
039600931013     C*
039700941103     C                   SETON                                        01
039800941103     C*----------------------------------------------------*
039900000103     C*  RECUPERO DIVISA MONETA CORRENTE
040000000103     C                   CLEAR                   DGED
040100000103     C                   CLEAR                   DSBS02
040200000103     C                   MOVEL     'C'           T02MOD
040300000103     C                   MOVEL     KNSIF         T02SIF
040400000103     C                   MOVEL     'GED'         T02COD
040500000103     C                   MOVEL     '1'           T02KE1
040600000103     C                   CALL      'TIBS02R'
040700000103     C                   PARM                    KPJBA
040800000103     C                   PARM                    DSBS02
040900000103     C*
041000000103    3C     T02ERR        IFEQ      ' '
041100000103     C                   MOVEL     T02UNI        DGED
041200011001     C                   MOVEL     §GEDCN        DIVCOR
041300000103    3C                   ENDIF
041400000103     C*----------------------------------------------------*
041500000103     C* Carico tabella cambi/divise
041600020910     C                   OPEN      TABEL00F
041700000103     C                   Z-ADD     0             XX                3 0
041800000103     C                   MOVEL     'CV'          KCOD
041900000103     C     KTAB          CHAIN     TABEL                              31
042000000103     C     *IN31         DOWEQ     '0'
042100000103     C     XX            ANDLT     100
042200000103     C                   MOVEL     TBLKEY        WDIV              3
042300000103     C     TBLFLG        IFEQ      ' '
042400000103     C                   ADD       1             XX
042500000103     C                   MOVEL     TBLKEY        CCV(XX)
042600000103     C                   MOVEL     TBLUNI        DSCV
042700000103     C                   MOVEL     §CVDVA        DVA(XX)
042800000103     C     §CVDVA        IFEQ      'S'
042900011001     C     WDIV          ANDNE     §GEDCN
043000000103     C                   MOVEL     WDIV          DIVALT
043100000103     C                   END
043200000103     C* Memorizzo nr. decimali della divisa corrente
043300011001     C     WDIV          IFEQ      §GEDCN
043400000103     C                   Z-ADD     §CVDEC        NRDEC             2 0
043500000103     C                   END
043600000103     C                   END
043700000103     C     KTAB          READE     TABEL                                  31
043800000103     C                   END
043900020910     C                   CLOSE     TABEL00F
044000941103     C*----------------------------------------------------*
044100931011     C                   ENDSR
