000100940218     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300120213      * TRAS22R3                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600931013      *          STAMPA  ORDINATA X DATI DI SPEDIZIONE               *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000940218      * 01 -OVERFLOW                                                 *
001100940218      * 03 -TITOLO: 'FILIALI: STAMPA BOLLE C/A PER DATA/NUM. SPED'   *
001200940218      * 20 -CONTRASSEGNO INCASSATO E NON PAGATO                      *
001300940218      * 78 -CHAIN SU AZORG.                                          *
001400940218      * 80 -READ SU FLCSB03L                                         *
001500940218      * 81 -CHAIN SU CNABI.                                          *
001600940218?     *--------------------------------------------------------------*
001700941103     FTABEL00F  IF   E           K DISK
001800941103     FFNCSB03L  IF   E           K DISK
001900941103     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002000941103     FCNABI00F  IF   E           K DISK
002100120213     Ftncsm03l  IF   E           K DISK
002200941103     FTNSC22P   O    E             PRINTER OFLIND(*IN01)
002300941103     D*--------------------------------------------------------------*
002400941103     D CCV             S              3    DIM(100)
002500000103     D DVA             S              1    DIM(100)
002600941103?     *--------------------------------------------------------------*
002700941103?     *  DS                                                          *
002800941103?     *--------------------------------------------------------------*
002900941103     D KPJBA         E DS
003000941103      *
003100010615     D YEURDS        E DS                  EXTNAME(YEURCODS)
003200941103     D TNSC20        E DS                  EXTNAME(TNSC20DS)
003300941103     D DSCV          E DS
003400000103     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003500000103     D DGED          E DS
003600941103      *
003700941103     D WLBDA8          DS
003800941103     D  G02DAT                 1      8  0
003900941103     D  G02INV                 9     16  0
004000941103     D  G02ERR                17     17
004100941103     D  G02TGI                18     22  0
004200941103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004300941103     D CNCR80        E DS
004400941103      *
004500941103      *  Ds x numero spedizione contrass.
004600941103     D                 DS
004700941103     D  CSBLNP                 1      3  0
004800941103     D  CSBAAS                 4      7  0
004900941103     D  CSBNRS                 8      9  0
005000941103     D  CSBNSP                10     16  0
005100941103     D  NUMSPE                 1     16  0
005200941103      *
005300941103      *  Ds x numero spedizione pagamento
005400941103     D                 DS
005500941103     D  CSBPLP                 1      3  0
005600941103     D  CSBPAA                 4      7  0
005700941103     D  CSBPNS                 8      9  0
005800941103     D  CSBPSP                10     16  0
005900941103     D  SPEPAG                 1     16  0
006000941103      *
006100941103     D                SDS
006200941103     D  NOMPG                  1     10
006300941103     D*
006400941103     D COST1           C                   CONST('CONTANTE            ')
006500011001     I*******             CMB       100  9 3             CAMBIO DIVISA ITA.
006600941103?     *--------------------------------------------------------------*
006700941103?     *  FILE                                                        *
006800941103?     *--------------------------------------------------------------*
006900941103     IAZORG01L  AA
007000941103     I                             P    4    5 0ORGFIL
007100941103     I                                 14   33  ORGDES
007200931011      *---------------------------------------------------------------*
007300931011      *   C I C L O      P R I N C I P A L E                          *
007400931011      *---------------------------------------------------------------*
007500931012     C*  PRIMA LETTURA
007600941103     C     KEYSPE        SETLL     FNCSB03L
007700941103     C                   READ      FNCSB03L                               80
007800941103     C                   MOVEL     CSBAAS        WDSP              8 0
007900941103     C                   MOVE      CSBMGS        WDSP              8 0
008000931012     C*  LOOP
008100931012     C     *IN80         DOWEQ     '0'
008200941103     C     WDSP          ANDLE     D20SDA
008300931012     C                   EXSR      STAMPA
008400941103     C                   READ      FNCSB03L                               80
008500941103     C                   MOVEL     CSBAAS        WDSP              8 0
008600941103     C                   MOVE      CSBMGS        WDSP              8 0
008700931011     C                   END
008800931011     C*
008900931013     C     TOT           IFGT      0
009000931013     C   01              WRITE     TES00
009100931013     C   01              SETOFF                                       01
009200931013     C                   WRITE     TOT00
009300931013     C                   END
009400000103     C* Se ci sono dei C/Assegni in Euro stampo il totale relativo
009500000103     C     TOTEU         IFGT      0
009600000103     C   01              WRITE     TES00
009700000103     C   01              SETOFF                                       01
009800000103     C                   WRITE     TOTE0
009900000103     C                   END
010000941103     C*
010100941103     C     TOTV          IFGT      0
010200941103     C   01              WRITE     TES00
010300941103     C   01              SETOFF                                       01
010400011002     C                   MOVEL     §GEDCN        VALV
010500941103     C                   WRITE     TOTV0
010600941103     C                   END
010700931011     C*
010800941103     C                   MOVEL     TNSC20        KPJBU
010900931011     C                   SETON                                        LR
011000931011     C*----------------------------------------------------*
011100931012     C*   STAMPA: Trasferimento dati da file a stampa      *
011200931011     C*----------------------------------------------------*
011300931012     C     STAMPA        BEGSR
011400931011     C*
011500931013     C* SOLO CONTRASSEGNI INCASSATI NON PAGATI
011600941103     C     D20TCR        IFEQ      'I'
011700931013     C     CSBDDC        COMP      0                                      20
011800931013     C  N20CSBDDP        COMP      0                                  2020
011900931013     C                   END
012000931013     C* SOLO CONTRASSEGNI PAGATI
012100941103     C     D20TCR        IFEQ      'P'
012200931013     C     CSBNDP        COMP      0                                      20
012300931013     C                   END
012400931013     C*  Test Tipo Contrassegni
012500931013     C     *IN20         IFEQ      '0'
012600931013     C* Scrivo record se corrisponde al range richiesto
012700941103     C     D20SCD        IFEQ      0
012800941103     C     D20SCD        OREQ      CSBCDI
012900000103     C* Scrivo record se divisa corrisponde
013000000103     C     D20SDV        IFEQ      *BLANKS
013100000103     C     D20SDV        OREQ      CSBVCA
013200011001     C     D20SDV        OREQ      §GEDCN
013300000103     C     CSBVCA        ANDEQ     *BLANKS
013400931013     C*  Test. x Ragione sociale
013500941103     C     D20SRA        IFNE      *BLANK
013600931013     C                   SETOFF                                       40
013700931013     C*  Loop di ricerca sottostringa nel campo del subfile
013800931013     C                   DO        X             T                 2 0
013900941103     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
014000941103     C     RESULT        COMP      D20SRA                                 40
014100931013     C  N40              END
014200931013     C*
014300931013     C                   ELSE
014400931013     C                   SETON                                        40
014500931013     C                   END
014600931013     C*
014700931013     C     *IN40         IFEQ      '1'
014800931014     C                   MOVEL     CSBRMN        RNM
014900931014     C                   MOVEL     CSBLNA        LNA
015000931013     C                   MOVEL     CSBLNP        LNP
015100950626     C                   MOVE      CSBAAS        AA
015200931013     C                   MOVE      CSBNRS        NRS
015300931013     C                   MOVE      CSBNSP        NSP
015400941103     C                   MOVE      CSBAAS        WAA2              2 0
015500941103     C                   MOVEL     WAA2          XDTAMG
015600941103     C                   MOVE      CSBMGS        XDTAMG
015700931013     C                   Z-ADD     0             XDTGMA
015800931013     C                   Z-ADD     2             XDTSTA
015900931013     C                   CALL      'XDATA'       PLDAT
016000931014     C                   MOVEL     XDTGMA        DSP
016100941103     C                   MOVEL     CSBVCA        VCA
016200011002     C     VCA           IFEQ      *BLANK
016300011002     C                   MOVE      'ITL'         VCA
016400011002     C                   END
016500941103     C                   Z-ADD     CSBCAS        IMP
016600941103     C* Controllo la divisa
016700941103     C     VCA           IFNE      *BLANKS
016800941103     C                   Z-ADD     1             XX
016900941103     C     VCA           LOOKUP    CCV(XX)                                32
017000941103     C                   END
017100941103     C* Somma nel totale corretto
017200941103     C     VCA           IFEQ      *BLANKS
017300011001     C     VCA           OREQ      §GEDCN
017400941103     C                   ADD       CSBCAS        TOT
017500941103     C                   ELSE
017600000103     C     DVA(XX)       IFEQ      'S'
017700000103     C     *IN32         ANDEQ     '1'
017800000103     C                   ADD       CSBCAS        TOTEU
017900000103     C                   ELSE
018000010615     C* Per convertire gli importi in valute estere utilizzo
018100010615     C* la routine YEURCO in quanto il cambio indicato sul
018200010615     C* file dei C/assegni di filiale non è affidabile
018300010615     C                   CLEAR                   YEURDS
018400010615     C                   MOVEL     VCA           YECDVI
018500010615     C                   Z-ADD     CSBCAS        YECIMI
018600011001     C                   MOVEL     §GEDCN        YECDVO
018700010615     C* passo al pgm io il nr. dei decimali della divisa corrente
018800010615     C* in base a quanto memorizzato sulla tabella CV
018900010615     C                   MOVE      NRDEC         YECDCO
019000010615     C                   MOVEL     'H'           YECTAR
019100010615     C                   CALL      'YEURCO'
019200010615     C                   PARM                    YEURDS
019300010615     C                   Z-ADD     YECIMO        IMPV             13 3
019400941103     C                   ADD       IMPV          TOTV
019500941103     C                   END
019600000103     C                   END
019700941103     C*
019800120220     c                   if        %subst(csbnra: 10: 1)=' '
019900120213     c                   setoff                                       44
020000120213     C                   MOVE      CSBNRA        NDC
020100120213     C                   MOVE      CSBDTE        XDTAMG
020200120213     C                   Z-ADD     0             XDTGMA
020300120213     C                   Z-ADD     2             XDTSTA
020400120213     C                   CALL      'XDATA'       PLDAT
020500120213     C                   MOVE      XDTGMA        DDC
020600931013     C                   MOVE      CSBABI        ABI
020700931013     C                   MOVE      CSBCAB        CAB
020800931014     C                   MOVEL     *BLANKS       BAI
020900931012     C     KEYABI        CHAIN     CNABI00F                           81
021000931012     C  N81              MOVEL     ABIIST        BAI
021100931013     C                   MOVEL     CSBTPA        ASS
021200931013     C                   MOVE      CSBTPI        ASS
021300120213     c                   end
021400120213     c*
021500120213     C     NDC           IFEQ      *BLANKS
021600120213     C                   MOVEL     COST1         BAI
021700120213     C                   END
021800931013     C                   MOVE      CSBCDI        CDM
021900931013     C                   MOVEL     CSBRSM        RGM
022000931013     C                   MOVEL     CSBRSD        RGD
022100931014     C                   MOVEL     *BLANK        DOC
022200931013     C*  Tipo Documento
022300931013     C     CSBFPC        IFEQ      '2'
022400931013     C                   MOVE      'BON.'        DOC
022500931013     C                   ELSE
022600931014     C     CSBPAB        IFEQ      99999
022700931014     C     CSBPCA        ANDEQ     99999
022800931014     C                   MOVE      'LETT'        DOC
022900931014     C                   ELSE
023000931014     C     CSBPAB        IFNE      0
023100931014     C     CSBPCA        ORNE      0
023200931013     C                   MOVE      'ASS.'        DOC
023300931013     C                   END
023400931014     C                   END
023500931013     C                   END
023600931013     C*
023700931013     C                   MOVE      CSBDDP        XDTAMG
023800931013     C                   Z-ADD     0             XDTGMA
023900931013     C                   Z-ADD     2             XDTSTA
024000931013     C                   CALL      'XDATA'       PLDAT
024100931013     C                   MOVE      XDTGMA        DDP
024200931013     C                   MOVE      CSBNDP        NDP
024300931013     C                   MOVEL     *BLANK        BAP
024400931013     C                   MOVE      CSBPAB        ABI
024500931013     C                   MOVE      CSBPCA        CAB
024600931013     C     KEYABI        CHAIN     CNABI00F                           81
024700931013     C  N81              MOVEL     ABIIST        BAP
024800931013     C                   MOVE      CSBDGC        XDTAMG
024900931013     C                   Z-ADD     0             XDTGMA
025000931013     C                   Z-ADD     2             XDTSTA
025100931013     C                   CALL      'XDATA'       PLDAT
025200931013     C                   MOVE      XDTGMA        DGC
025300931112     C                   MOVEL     *ZERO         LNPP
025400931112     C                   MOVEL     *ZERO         AAP
025500931112     C                   MOVE      *ZERO         NRSP
025600931112     C                   MOVE      *ZERO         NSPP
025700931112     C*
025800931112     C*  Stampo riferimnti spedizione pagamento contrassegno solo se
025900931112     C*  diverso da originale
026000931112     C     NUMSPE        IFNE      SPEPAG
026100931112     C                   MOVEL     CSBPLP        LNPP
026200931112     C                   MOVEL     CSBPAA        AAP
026300931112     C                   MOVE      CSBPNS        NRSP
026400931112     C                   MOVE      CSBPSP        NSPP
026500931112     C                   END
026600120220     c                   if        %subst(csbnra: 10: 1)=' '
026700931013     C   01              WRITE     TES00
026800931013     C   01              SETOFF                                       01
026900931013     C                   WRITE     DET00
027000120213     c                   else
027100120213     c                   exsr      srass
027200120213     C                   END
027300931013     C                   END
027400000103     C                   END
027500000103     C                   END
027600931013     C                   END
027700931012     C*
027800931011     C                   ENDSR
027900120213      *---------------------------------------------------------------*
028000120213      *  stampa rif assegni
028100120213      *---------------------------------------------------------------*
028200120213     C     srass         BEGSR
028300120213     c                   setoff                                       44
028400120213     c     csbnra        setll     tncsm03l
028500120213     c                   do        *hival
028600120213     c     csbnra        reade     tncsm03l
028700120213     c                   if        %eof(tncsm03l)
028800120213     c                   leave
028900120213     c                   end
029000120213     C                   MOVE      CSmNRA        NDC
029100120213     C                   MOVE      CSmDTE        XDTAMG
029200120213     C                   Z-ADD     0             XDTGMA
029300120213     C                   Z-ADD     2             XDTSTA
029400120213     C                   CALL      'XDATA'       PLDAT
029500120213     C                   MOVE      XDTGMA        DDC
029600120213     C                   MOVE      CSmABI        ABI
029700120213     C                   MOVE      CSmCAB        CAB
029800120213     C                   MOVEL     *BLANKS       BAI
029900120213     C     KEYABI        CHAIN     CNABI00F                           81
030000120213     C  N81              MOVEL     ABIIST        BAI
030100120213     C                   MOVEL     CSmTPA        ASS
030200120213     C                   MOVE      CSmTPI        ASS
030300120213     C   01              WRITE     TES00
030400120213     C   01              SETOFF                                       01
030500120213     C   01              SETOFF                                       44
030600120213     C                   WRITE     DET00
030700120213     c                   seton                                        44
030800120213     c                   enddo
030900120213     C*
031000120213     C                   ENDSR
031100931011      *---------------------------------------------------------------*
031200941103      *  *INZSR: OPERAZIONI INIZIALI                                  *
031300931011      *---------------------------------------------------------------*
031400941103     C     *INZSR        BEGSR
031500931011      *---------------------------------------------------------------*
031600931011     C     *ENTRY        PLIST
031700931011     C                   PARM                    KPJBA
031800941103     C                   MOVEL     KPJBU         TNSC20
031900931012     C*-
032000931012     C* PLIST per richiamo Routine XDATA
032100931012     C     PLDAT         PLIST
032200931012     C                   PARM                    XDTAMG            6 0
032300931012     C                   PARM                    XDTGMA            6 0
032400931012     C                   PARM                    XDTSTA            1 0
032500931012     C*
032600931011     C                   Z-ADD     1             CODUT
032700931011     C                   CALL      'X§PARUT'
032800931011     C                   PARM                    UT§DSE
032900931011     C*          CODUT     CABEQ-1        FINE
033000931011     C                   MOVEL     RAGUT         RSUT             20
033100931011     C                   MOVEL     REC80         CNCR80
033200931011     C                   MOVEL     KNMUS         §SEI              6
033300931011     C*
033400931013     C     KEYSPE        KLIST
033500941103     C                   KFLD                    KAAS
033600941103     C                   KFLD                    KMGS
033700931012     C*
033800931012     C     KEYABI        KLIST
033900931013     C                   KFLD                    ABI               5 0
034000931013     C                   KFLD                    CAB               5 0
034100941103     C*
034200941103     C     KTAB          KLIST
034300941103     C                   KFLD                    KKUT
034400941103     C                   KFLD                    KCOD
034500941103     C                   Z-ADD     1             KKUT
034600941103     C*----------------------------------------------------*
034700941103     C*  Definizione variabili
034800941103     C     *LIKE         DEFINE    TBLKUT        KKUT
034900941103     C     *LIKE         DEFINE    TBLCOD        KCOD
035000941103     C     *LIKE         DEFINE    CSBAAS        KAAS
035100941103     C     *LIKE         DEFINE    CSBMGS        KMGS
035200941103     C*----------------------------------------------------*
035300941103     C                   MOVEL     D20SDD        KAAS
035400941103     C                   MOVE      D20SDD        KMGS
035500941103     C                   MOVE      SIMFEL        ORGFIL
035600941103     C     ORGFIL        CHAIN     AZORG01L                           78
035700941103     C  N78              MOVEL     ORGDES        FILIAL
035800931012     C*----------------------------------------------------*
035900931014     C                   SETON                                        03
036000941103     C                   MOVE      '  '          D20SRA
036100931012     C*----------------------------------------------------*
036200931013     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
036300941103     C     '  '          SCAN      D20SRA        LENGHT            2 0
036400931013     C                   SUB       1             LENGHT
036500941103     C     35            SUB       LENGHT        X                 2 0
036600931013     C*----------------------------------------------------*
036700931013     C*  Scrittura Prima pagina
036800931013     C                   MOVEL     NOMPG         NOMPGM
036900941103     C                   MOVE      D20INT        S22SEL
037000941103     C                   MOVE      D20ANN        S22AAS
037100941103     C                   MOVE      D20LNP        S22LNP
037200941103     C                   MOVE      D20NSR        S22NSR
037300941103     C                   MOVE      D20NRM        S22NRM
037400941103     C                   MOVE      D20SDD        G02INV
037500941103     C                   Z-ADD     0             G02DAT
037600941103     C                   MOVEL     '3'           G02ERR
037700941103     C                   CALL      'XSRDA8'
037800941103     C                   PARM                    WLBDA8
037900941103     C                   MOVE      G02DAT        S22SDD
038000941103     C                   MOVE      D20SDA        G02INV
038100941103     C                   Z-ADD     0             G02DAT
038200941103     C                   MOVEL     '3'           G02ERR
038300941103     C                   CALL      'XSRDA8'
038400941103     C                   PARM                    WLBDA8
038500941103     C                   MOVE      G02DAT        S22SDA
038600941103     C                   MOVE      D20SCD        S22SCD
038700941103     C                   MOVEL     D20SRA        S22SRS
038800941103     C                   MOVE      D20PDD        G02INV
038900941103     C                   Z-ADD     0             G02DAT
039000941103     C                   MOVEL     '3'           G02ERR
039100941103     C                   CALL      'XSRDA8'
039200941103     C                   PARM                    WLBDA8
039300941103     C                   MOVE      G02DAT        S22PDD
039400941103     C                   MOVE      D20PDA        G02INV
039500941103     C                   Z-ADD     0             G02DAT
039600941103     C                   MOVEL     '3'           G02ERR
039700941103     C                   CALL      'XSRDA8'
039800941103     C                   PARM                    WLBDA8
039900941103     C                   MOVE      G02DAT        S22PDA
040000941103     C                   MOVE      D20PND        S22PND
040100941103     C                   MOVE      D20PTN        S22PTI
040200941103     C                   MOVE      D20PCD        S22PCD
040300941103     C                   MOVEL     D20PRA        S22PRS
040400941103     C                   MOVE      D20TCR        S22TCA
040500931013     C*
040600931013     C     S22TCA        IFEQ      ' '
040700931013     C                   MOVEL     'TUTTI'       S22DCA
040800931013     C                   ELSE
040900931013     C     S22TCA        IFEQ      'I'
041000931013     C                   MOVEL     'INCASSAT'    S22DCA
041100931013     C                   MOVE      'I      '     S22DCA
041200931013     C                   ELSE
041300931013     C     S22TCA        IFEQ      'P'
041400931013     C                   MOVEL     'PAGATI  '    S22DCA
041500931013     C                   END
041600931013     C                   END
041700931013     C                   END
041800931013     C*
041900931014     C                   SETON                                        03
042000931013     C                   MOVEL     *ALL'-'       CMP198
042100931014     C                   MOVEL     *ALL'-'       CM1198
042200931013     C                   WRITE     PAG00
042300931013     C*
042400931014     C                   SETON                                        01
042500941103     C*----------------------------------------------------*
042600000103     C*  RECUPERO DIVISA MONETA CORRENTE
042700000103     C                   CLEAR                   DGED
042800000103     C                   CLEAR                   DSBS02
042900000103     C                   MOVEL     'C'           T02MOD
043000000103     C                   MOVEL     KNSIF         T02SIF
043100000103     C                   MOVEL     'GED'         T02COD
043200000103     C                   MOVEL     '1'           T02KE1
043300000103     C                   CALL      'TIBS02R'
043400000103     C                   PARM                    KPJBA
043500000103     C                   PARM                    DSBS02
043600000103     C*
043700000103    3C     T02ERR        IFEQ      ' '
043800000103     C                   MOVEL     T02UNI        DGED
043900011001     C                   MOVEL     §GEDCN        DIVCOR
044000000103    3C                   ENDIF
044100000103     C*----------------------------------------------------*
044200000103     C* Carico tabella cambi/divise
044300000103     C                   Z-ADD     0             XX                3 0
044400000103     C                   MOVEL     'CV'          KCOD
044500000103     C     KTAB          CHAIN     TABEL                              31
044600000103     C     *IN31         DOWEQ     '0'
044700000103     C     XX            ANDLT     100
044800000103     C                   MOVEL     TBLKEY        WDIV              3
044900000103     C     TBLFLG        IFEQ      ' '
045000000103     C                   ADD       1             XX
045100000103     C                   MOVEL     TBLKEY        CCV(XX)
045200000103     C                   MOVEL     TBLUNI        DSCV
045300000103     C                   MOVEL     §CVDVA        DVA(XX)
045400011001     C*******              MOVEL§CVCMB    CMB,XX
045500000103     C     §CVDVA        IFEQ      'S'
045600011001     C     WDIV          ANDNE     §GEDCN
045700000103     C                   MOVEL     WDIV          DIVALT
045800000103     C                   END
045900000103     C* Memorizzo nr. decimali della divisa corrente
046000011001     C     WDIV          IFEQ      §GEDCN
046100010615     C                   Z-ADD     §CVDEC        NRDEC             2 0
046200000103     C                   END
046300000103     C                   END
046400000103     C     KTAB          READE     TABEL                                  31
046500000103     C                   END
046600941103     C*----------------------------------------------------*
046700931011     C                   ENDSR
