000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300931012      * TRAS22R1                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600931013      *          STAMPA  ORDINATA X DATI DI PAGAMENTO                *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000940218      * 01 -OVERFLOW                                                 *
001100940218      * 04 -TITOLO: 'FILIALI: STAMPA BOLLE C/A PER DATA PAGAMENTO'   *
001200940218      * 20 -CONTRASSEGNO INCASSATO E NON PAGATO                      *
001300940218      * 78 -CHAIN SU AZORG.                                          *
001400940218      * 80 -READ SU FLCSB02L                                         *
001500940218      * 81 -CHAIN SU CNABI.                                          *
001600940218?     *--------------------------------------------------------------*
001700941103     FTABEL00F  IF   E           K DISK
001800941103     FFNCSB02L  IF   E           K DISK
001900941103     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002000120213     Ftncsm03l  IF   E           K DISK
002100941103     FCNABI00F  IF   E           K DISK
002200941103     FTNSC22P   O    E             PRINTER OFLIND(*IN01)
002300941103     D*--------------------------------------------------------------*
002400941103     D CCV             S              3    DIM(100)
002500000103     D DVA             S              1    DIM(100)
002600011001     D*****               CMB       100  9 3             CAMBIO DIVISA ITA.
002700020506     D*UNI             S              3    DIM(29)
002800020506     D £1              S              3  0 DIM(30)
002900941103?     *--------------------------------------------------------------*
003000941103?     *  DS                                                          *
003100941103?     *--------------------------------------------------------------*
003200941103     D KPJBA         E DS
003300941103      *
003400010615     D YEURDS        E DS                  EXTNAME(YEURCODS)
003500941103     D TNSC20        E DS                  EXTNAME(TNSC20DS)
003600941103     D DSCV          E DS
003700000103     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003800000103     D DGED          E DS
003900020506     d trul06ds      e ds
004000020506     d  lin                    1     90  0 dim(30)
004100941103      *
004200941103     D WLBDA8          DS
004300941103     D  G02DAT                 1      8  0
004400941103     D  G02INV                 9     16  0
004500941103     D  G02ERR                17     17
004600941103     D  G02TGI                18     22  0
004700941103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800941103     D CNCR80        E DS
004900941103      *
005000941103      *  Ds x numero spedizione contrass.
005100941103     D                 DS
005200941103     D  CSBLNP                 1      3  0
005300941103     D  CSBAAS                 4      7  0
005400941103     D  CSBNRS                 8      9  0
005500941103     D  CSBNSP                10     16  0
005600941103     D  NUMSPE                 1     16  0
005700941103      *
005800941103      *  Ds x numero spedizione pagamento
005900941103     D                 DS
006000941103     D  CSBPLP                 1      3  0
006100941103     D  CSBPAA                 4      7  0
006200941103     D  CSBPNS                 8      9  0
006300941103     D  CSBPSP                10     16  0
006400941103     D  SPEPAG                 1     16  0
006500941103      *
006600941103     D                SDS
006700941103     D  NOMPG                  1     10
006800941103     D*
006900941103     D COST1           C                   CONST('CONTANTE            ')
007000941103?     *--------------------------------------------------------------*
007100941103?     *  FILE                                                        *
007200941103?     *--------------------------------------------------------------*
007300941103     IAZORG01L  AA
007400941103     I                             P    4    5 0ORGFIL
007500941103     I                                 14   33  ORGDES
007600931011      *---------------------------------------------------------------*
007700931011      *   C I C L O      P R I N C I P A L E                          *
007800931011      *---------------------------------------------------------------*
007900931012     C*  PRIMA LETTURA
008000941103     C     KEYPAG        SETLL     FNCSB02L
008100941103     C                   READ      FNCSB02L                               80
008200931012     C*  LOOP
008300931012     C     *IN80         DOWEQ     '0'
008400941103     C     CSBDDP        ANDLE     D20PDA
008500931012     C                   EXSR      STAMPA
008600941103     C                   READ      FNCSB02L                               80
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
010000000103     C*
010100931011     C*
010200941103     C     TOTV          IFGT      0
010300941103     C   01              WRITE     TES00
010400941103     C   01              SETOFF                                       01
010500011002     C                   MOVEL     §GEDCN        VALV
010600941103     C                   WRITE     TOTV0
010700941103     C                   END
010800941103     C*
010900941103     C                   MOVEL     TNSC20        KPJBU
011000931011     C                   SETON                                        LR
011100931011     C*----------------------------------------------------*
011200931012     C*   STAMPA: Trasferimento dati da file a stampa      *
011300931011     C*----------------------------------------------------*
011400931012     C     STAMPA        BEGSR
011500931011     C*
011600931013     C* SOLO CONTRASSEGNI INCASSATI NON PAGATI
011700941103     C     D20TCR        IFEQ      'I'
011800931013     C     CSBDDC        COMP      0                                      20
011900931013     C  N20CSBDDP        COMP      0                                  2020
012000931013     C                   END
012100931013     C* SOLO CONTRASSEGNI PAGATI
012200941103     C     D20TCR        IFEQ      'P'
012300931013     C     CSBNDP        COMP      0                                      20
012400931013     C                   END
012500931013     C*  Test Tipo Contrassegni
012600931013     C     *IN20         IFEQ      '0'
012700931013     C*  Test  x Numero documento
012800941103     C     D20PND        IFEQ      0
012900941103     C     D20PND        OREQ      CSBNDP
013000931013     C*  Test  x tipo intestazione
013100941103     C     D20PTN        IFEQ      *BLANKS
013200941103     C     D20PTN        OREQ      'M'
013300931202     C     CSBTPI        ANDEQ     'M'
013400941103     C     D20PTN        OREQ      'B'
013500931202     C     CSBTPI        ANDEQ     ' '
013600931013     C* Scrivo record se corrisponde al range richiesto
013700941103     C     D20PCD        IFEQ      0
013800941103     C     D20PCD        OREQ      CSBCDI
013900000103     C* Scrivo record se divisa corrisponde
014000000103     C     D20PDV        IFEQ      *BLANKS
014100000103     C     D20PDV        OREQ      CSBVCA
014200011001     C     D20PDV        OREQ      §GEDCN
014300000103     C     CSBVCA        ANDEQ     *BLANKS
014400931013     C*  Test. x Ragione sociale
014500941103     C     D20PRA        IFNE      *BLANK
014600931013     C                   SETOFF                                       40
014700931013     C*  Loop di ricerca sottostringa nel campo del subfile
014800931013     C                   DO        X             T                 2 0
014900941103     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
015000941103     C     RESULT        COMP      D20PRA                                 40
015100931013     C  N40              END
015200931013     C*
015300931013     C                   ELSE
015400931013     C                   SETON                                        40
015500931013     C                   END
015600931013     C*
015700931013     C     *IN40         IFEQ      '1'
015800931014     C                   MOVE      CSBRMN        RNM
015900931014     C                   MOVEL     CSBLNA        LNA
016000931013     C                   MOVEL     CSBLNP        LNP
016100931012     C                   MOVEL     CSBAAS        AA
016200931013     C                   MOVE      CSBNRS        NRS
016300931013     C                   MOVE      CSBNSP        NSP
016400941103     C                   MOVE      CSBAAS        WAA2              2 0
016500941103     C                   MOVEL     WAA2          XDTAMG
016600941103     C                   MOVE      CSBMGS        XDTAMG
016700941103     C                   Z-ADD     0             XDTGMA
016800941103     C                   Z-ADD     2             XDTSTA
016900941103     C                   CALL      'XDATA'       PLDAT
017000941103     C                   MOVEL     XDTGMA        DSP
017100941103     C                   MOVEL     CSBVCA        VCA
017200011002     C     VCA           IFEQ      *BLANK
017300011002     C                   MOVE      'ITL'         VCA
017400011002     C                   END
017500941103     C                   Z-ADD     CSBCAS        IMP
017600941103     C* Controllo la divisa
017700941103     C     VCA           IFNE      *BLANKS
017800941103     C                   Z-ADD     1             XX
017900941103     C     VCA           LOOKUP    CCV(XX)                                32
018000941103     C                   END
018100941103     C* Somma nel totale corretto
018200960624     C     CSBPLP        LOOKUP    £1                                     35
018300941103     C     VCA           IFEQ      *BLANKS
018400011001     C     VCA           OREQ      §GEDCN
018500960403     C     NUMSPE        IFEQ      SPEPAG
018600960403     C     SPEPAG        OREQ      0
018700960624     C     *IN35         OREQ      '0'
018800941103     C                   ADD       CSBCAS        TOT
018900960403     C                   END
019000000103     C                   ELSE
019100000103     C     DVA(XX)       IFEQ      'S'
019200000103     C     *IN32         ANDEQ     '1'
019300000103     C                   ADD       CSBCAS        TOTEU
019400000103     C                   ELSE
019500010615     C* Per convertire gli importi in valute estere utilizzo
019600010615     C* la routine YEURCO in quanto il cambio indicato sul
019700010615     C* file dei C/assegni di filiale non è affidabile
019800010615     C                   CLEAR                   YEURDS
019900010615     C                   MOVEL     VCA           YECDVI
020000010615     C                   Z-ADD     CSBCAS        YECIMI
020100011001     C                   MOVEL     §GEDCN        YECDVO
020200010615     C* passo al pgm io il nr. dei decimali della divisa corrente
020300010615     C* in base a quanto memorizzato sulla tabella CV
020400010615     C                   MOVE      NRDEC         YECDCO
020500010615     C                   MOVEL     'H'           YECTAR
020600010615     C                   CALL      'YEURCO'
020700010615     C                   PARM                    YEURDS
020800010615     C                   Z-ADD     YECIMO        IMPV             13 3
020900000103     C*
021000960403     C     NUMSPE        IFEQ      SPEPAG
021100960403     C     SPEPAG        OREQ      0
021200960624     C     *IN35         OREQ      '0'
021300941103     C                   ADD       IMPV          TOTV
021400941103     C                   END
021500960403     C                   END
021600000103     C                   END
021700941103     C*
021800120220     c                   if        %subst(csbnra: 10: 1)=' '
021900120213     c                   setoff                                       44
022000120213     C                   MOVE      CSBNRA        NDC
022100120213     C                   MOVE      CSBDTE        XDTAMG
022200120213     C                   Z-ADD     0             XDTGMA
022300120213     C                   Z-ADD     2             XDTSTA
022400120213     C                   CALL      'XDATA'       PLDAT
022500120213     C                   MOVE      XDTGMA        DDC
022600931116     C                   MOVEL     *BLANKS       BAI
022700931013     C                   MOVE      CSBABI        ABI
022800931013     C                   MOVE      CSBCAB        CAB
022900931012     C     KEYABI        CHAIN     CNABI00F                           81
023000931012     C  N81              MOVEL     ABIIST        BAI
023100931013     C                   MOVEL     CSBTPA        ASS
023200931013     C                   MOVE      CSBTPI        ASS
023300120213     c                   end
023400120213     c*
023500120213     C     NDC           IFEQ      *BLANKS
023600120213     C                   MOVEL     COST1         BAI
023700120213     C                   END
023800931013     C                   MOVE      CSBCDI        CDM
023900931013     C                   MOVEL     CSBRSM        RGM
024000931013     C                   MOVEL     CSBRSD        RGD
024100931013     C*  Tipo Documento
024200931013     C     CSBFPC        IFEQ      '2'
024300931013     C                   MOVE      'BON.'        DOC
024400931013     C                   ELSE
024500931014     C     CSBPAB        IFEQ      99999
024600931116     C     CSBPCA        ANDEQ     99999
024700931014     C                   MOVE      'LETT'        DOC
024800931014     C                   ELSE
024900931014     C     CSBPAB        IFNE      0
025000931014     C     CSBPCA        ORNE      0
025100931013     C                   MOVE      'ASS.'        DOC
025200931013     C                   END
025300931013     C                   END
025400931014     C                   END
025500931013     C*
025600931013     C                   MOVE      CSBDDP        XDTAMG
025700931013     C                   Z-ADD     0             XDTGMA
025800931013     C                   Z-ADD     2             XDTSTA
025900931013     C                   CALL      'XDATA'       PLDAT
026000931013     C                   MOVE      XDTGMA        DDP
026100931013     C                   MOVE      CSBNDP        NDP
026200931013     C                   MOVEL     *BLANK        BAP
026300931013     C                   MOVE      CSBPAB        ABI
026400931013     C                   MOVE      CSBPCA        CAB
026500931013     C     KEYABI        CHAIN     CNABI00F                           81
026600931013     C  N81              MOVEL     ABIIST        BAP
026700931013     C                   MOVE      CSBDGC        XDTAMG
026800931013     C                   Z-ADD     0             XDTGMA
026900931013     C                   Z-ADD     2             XDTSTA
027000931013     C                   CALL      'XDATA'       PLDAT
027100931013     C                   MOVE      XDTGMA        DGC
027200931112     C                   MOVEL     *ZERO         LNPP
027300931112     C                   MOVEL     *ZERO         AAP
027400931112     C                   MOVE      *ZERO         NRSP
027500931112     C                   MOVE      *ZERO         NSPP
027600931112     C*
027700931112     C*  Stampo riferimnti spedizione pagamento contrassegno solo se
027800931112     C*  diverso da originale
027900931112     C     NUMSPE        IFNE      SPEPAG
028000960624     C     SPEPAG        OREQ      0
028100960624     C     *IN35         OREQ      '0'
028200931112     C                   MOVEL     CSBPLP        LNPP
028300931112     C                   MOVEL     CSBPAA        AAP
028400931112     C                   MOVE      CSBPNS        NRSP
028500931112     C                   MOVE      CSBPSP        NSPP
028600931112     C                   END
028700120220     c                   if        %subst(csbnra: 10: 1)=' '
028800931013     C   01              WRITE     TES00
028900931013     C   01              SETOFF                                       01
029000931013     C                   WRITE     DET00
029100120213     c                   else
029200120213     c                   exsr      srass
029300120213     C                   END
029400931013     C                   END
029500931013     C                   END
029600931013     C                   END
029700931013     C                   END
029800931013     C                   END
029900000103     C                   END
030000931012     C*
030100931011     C                   ENDSR
030200120213      *---------------------------------------------------------------*
030300120213      *  stampa rif assegni
030400120213      *---------------------------------------------------------------*
030500120213     C     srass         BEGSR
030600120213     c                   setoff                                       44
030700120213     c     csbnra        setll     tncsm03l
030800120213     c                   do        *hival
030900120213     c     csbnra        reade     tncsm03l
031000120213     c                   if        %eof(tncsm03l)
031100120213     c                   leave
031200120213     c                   end
031300120213     C                   MOVE      CSmNRA        NDC
031400120213     C                   MOVE      CSmDTE        XDTAMG
031500120213     C                   Z-ADD     0             XDTGMA
031600120213     C                   Z-ADD     2             XDTSTA
031700120213     C                   CALL      'XDATA'       PLDAT
031800120213     C                   MOVE      XDTGMA        DDC
031900120213     C                   MOVE      CSmABI        ABI
032000120213     C                   MOVE      CSmCAB        CAB
032100120213     C                   MOVEL     *BLANKS       BAI
032200120213     C     KEYABI        CHAIN     CNABI00F                           81
032300120213     C  N81              MOVEL     ABIIST        BAI
032400120213     C                   MOVEL     CSmTPA        ASS
032500120213     C                   MOVE      CSmTPI        ASS
032600120213     C   01              WRITE     TES00
032700120213     C   01              SETOFF                                       01
032800120213     C   01              SETOFF                                       44
032900120213     C                   WRITE     DET00
033000120213     c                   seton                                        44
033100120213     c                   enddo
033200120213     C*
033300120213     C                   ENDSR
033400931011      *---------------------------------------------------------------*
033500941103      *  *INZSR: OPERAZIONI INIZIALI                                  *
033600931011      *---------------------------------------------------------------*
033700941103     C     *INZSR        BEGSR
033800931011      *---------------------------------------------------------------*
033900931011     C     *ENTRY        PLIST
034000931011     C                   PARM                    KPJBA
034100941103     C                   MOVEL     KPJBU         TNSC20
034200931012     C*-
034300931012     C* PLIST per richiamo Routine XDATA
034400931012     C     PLDAT         PLIST
034500931012     C                   PARM                    XDTAMG            6 0
034600931012     C                   PARM                    XDTGMA            6 0
034700931012     C                   PARM                    XDTSTA            1 0
034800931012     C*
034900931011     C                   Z-ADD     1             CODUT
035000931011     C                   CALL      'X§PARUT'
035100931011     C                   PARM                    UT§DSE
035200931011     C*          CODUT     CABEQ-1        FINE
035300931011     C                   MOVEL     RAGUT         RSUT             20
035400931011     C                   MOVEL     REC80         CNCR80
035500931011     C                   MOVEL     KNMUS         §SEI              6
035600931014     C*
035700931013     C     KEYPAG        KLIST
035800941103     C                   KFLD                    D20PDD
035900941103     C                   KFLD                    D20PND
036000931012     C*
036100931012     C     KEYABI        KLIST
036200931013     C                   KFLD                    ABI               5 0
036300931013     C                   KFLD                    CAB               5 0
036400941103     C*
036500941103     C     KTAB          KLIST
036600941103     C                   KFLD                    KKUT
036700941103     C                   KFLD                    KCOD
036800960624     C     KTAB1         KLIST
036900960624     C                   KFLD                    KKUT
037000960624     C                   KFLD                    KCOD
037100960624     C                   KFLD                    KKEY
037200941103     C                   Z-ADD     1             KKUT
037300941103     C*----------------------------------------------------*
037400941103     C*  Definizione variabili
037500941103     C     *LIKE         DEFINE    TBLKUT        KKUT
037600941103     C     *LIKE         DEFINE    TBLCOD        KCOD
037700960624     C     *LIKE         DEFINE    TBLKEY        KKEY
037800941103     C*----------------------------------------------------*
037900941103     C                   MOVEL     'N'           £FINE             1
038000941103     C                   MOVE      SIMFEL        ORGFIL
038100941103     C     ORGFIL        CHAIN     AZORG01L                           78
038200941103     C  N78              MOVEL     ORGDES        FILIAL
038300931012     C*----------------------------------------------------*
038400941103     C                   MOVE      '  '          D20PRA
038500931012     C*----------------------------------------------------*
038600931013     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
038700941103     C     '  '          SCAN      D20PRA        LENGHT            2 0
038800931013     C                   SUB       1             LENGHT
038900941103     C     35            SUB       LENGHT        X                 2 0
039000931013     C*----------------------------------------------------*
039100931013     C*  Scrittura Prima pagina
039200931013     C                   MOVEL     NOMPG         NOMPGM
039300941103     C                   MOVE      D20INT        S22SEL
039400941103     C                   MOVE      D20ANN        S22AAS
039500941103     C                   MOVE      D20LNP        S22LNP
039600941103     C                   MOVE      D20NSR        S22NSR
039700941103     C                   MOVE      D20NRM        S22NRM
039800941103     C                   MOVE      D20SDD        G02INV
039900941103     C                   Z-ADD     0             G02DAT
040000941103     C                   MOVEL     '3'           G02ERR
040100941103     C                   CALL      'XSRDA8'
040200941103     C                   PARM                    WLBDA8
040300941103     C                   MOVE      G02DAT        S22SDD
040400941103     C                   MOVE      D20SDA        G02INV
040500941103     C                   Z-ADD     0             G02DAT
040600941103     C                   MOVEL     '3'           G02ERR
040700941103     C                   CALL      'XSRDA8'
040800941103     C                   PARM                    WLBDA8
040900941103     C                   MOVE      G02DAT        S22SDA
041000941103     C                   MOVE      D20SCD        S22SCD
041100941103     C                   MOVEL     D20SRA        S22SRS
041200941103     C                   MOVE      D20PDD        G02INV
041300941103     C                   Z-ADD     0             G02DAT
041400941103     C                   MOVEL     '3'           G02ERR
041500941103     C                   CALL      'XSRDA8'
041600941103     C                   PARM                    WLBDA8
041700941103     C                   MOVE      G02DAT        S22PDD
041800941103     C                   MOVE      D20PDA        G02INV
041900941103     C                   Z-ADD     0             G02DAT
042000941103     C                   MOVEL     '3'           G02ERR
042100941103     C                   CALL      'XSRDA8'
042200941103     C                   PARM                    WLBDA8
042300941103     C                   MOVE      G02DAT        S22PDA
042400941103     C                   MOVE      D20PND        S22PND
042500941103     C                   MOVE      D20PTN        S22PTI
042600941103     C                   MOVE      D20PCD        S22PCD
042700941103     C                   MOVEL     D20PRA        S22PRS
042800941103     C                   MOVE      D20TCR        S22TCA
042900931013     C*
043000931013     C     S22TCA        IFEQ      ' '
043100931013     C                   MOVEL     'TUTTI'       S22DCA
043200931013     C                   ELSE
043300931013     C     S22TCA        IFEQ      'I'
043400931013     C                   MOVEL     'INCASSAT'    S22DCA
043500931013     C                   MOVE      'I      '     S22DCA
043600931013     C                   ELSE
043700931013     C     S22TCA        IFEQ      'P'
043800931013     C                   MOVEL     'PAGATI  '    S22DCA
043900931013     C                   END
044000931013     C                   END
044100931013     C                   END
044200931013     C*
044300931014     C                   SETON                                        04
044400931013     C                   WRITE     PAG00
044500931013     C                   MOVEL     *ALL'-'       CMP198
044600931014     C                   MOVEL     *ALL'-'       CM1198
044700931013     C*
044800931013     C                   SETON                                        01
044900941103     C*----------------------------------------------------*
045000000103     C*  RECUPERO DIVISA MONETA CORRENTE
045100000103     C                   CLEAR                   DGED
045200000103     C                   CLEAR                   DSBS02
045300000103     C                   MOVEL     'C'           T02MOD
045400000103     C                   MOVEL     KNSIF         T02SIF
045500000103     C                   MOVEL     'GED'         T02COD
045600000103     C                   MOVEL     '1'           T02KE1
045700000103     C                   CALL      'TIBS02R'
045800000103     C                   PARM                    KPJBA
045900000103     C                   PARM                    DSBS02
046000000103     C*
046100000103    3C     T02ERR        IFEQ      ' '
046200000103     C                   MOVEL     T02UNI        DGED
046300011001     C                   MOVEL     §GEDCN        DIVCOR
046400000103    3C                   ENDIF
046500000103     C*----------------------------------------------------*
046600000103     C* Carico tabella cambi/divise
046700000103     C                   Z-ADD     0             XX                3 0
046800000103     C                   MOVEL     'CV'          KCOD
046900000103     C     KTAB          CHAIN     TABEL                              31
047000000103     C     *IN31         DOWEQ     '0'
047100000103     C     XX            ANDLT     100
047200000103     C                   MOVEL     TBLKEY        WDIV              3
047300000103     C     TBLFLG        IFEQ      ' '
047400000103     C                   ADD       1             XX
047500000103     C                   MOVEL     TBLKEY        CCV(XX)
047600000103     C                   MOVEL     TBLUNI        DSCV
047700000103     C                   MOVEL     §CVDVA        DVA(XX)
047800011001     C******               MOVEL§CVCMB    CMB,XX
047900000103     C     §CVDVA        IFEQ      'S'
048000011001     C     WDIV          ANDNE     §GEDCN
048100000103     C                   MOVEL     WDIV          DIVALT
048200000103     C                   END
048300000103     C* Memorizzo nr. decimali della divisa corrente
048400011001     C     WDIV          IFEQ      §GEDCN
048500000103     C                   Z-ADD     §CVDEC        NRDEC             2 0
048600000103     C                   END
048700000103     C                   END
048800000103     C     KTAB          READE     TABEL                                  31
048900000103     C                   END
049000960624     C* Carico £1
049100020506     c                   clear                   trul06ds
049200020506     c                   eval      d06cod = '£1'
049300020506     c                   movel     simfel        d06key
049400020506     c                   movel     trul06ds      kpjbu
049500020506     c                   call      'TRUL06R'
049600020506     c                   parm                    kpjba
049700020506     c                   movel     kpjbu         trul06ds
049800020506     c                   movea     lin           £1
049900020506     C*                  Z-ADD     0             £1
050000020506     C*                  MOVEL     '£1'          KCOD
050100020506     C*                  MOVEL     *BLANKS       KKEY
050200020506     C*                  MOVEL     SIMFEL        KKEY
050300020506     C*    KTAB1         CHAIN     TABEL                              31
050400020506     C*    *IN31         IFEQ      '0'
050500020506     C*    TBLFLG        ANDEQ     ' '
050600020506     C*                  MOVEA     TBLUNI        UNI
050700020506     C*                  Z-ADD     0             XY                3 0
050800020506     C*                  DO        29            XX
050900020506     C*    UNI(XX)       IFGT      *ZEROS
051000020506     C*                  ADD       1             XY
051100020506     C*                  MOVEL     UNI(XX)       £1(XY)
051200020506     C*                  END
051300020506     C*                  END
051400020506     C*                  END
051500960624     C*  SE £1 = 0  --> SIMFEL
051600020506     C*                  XFOOT     £1            W£1               4 0
051700020506     C*    W£1           IFEQ      0
051800020506     C*                  Z-ADD     SIMFEL        £1(1)
051900020506     C*                  END
052000941103     C*----------------------------------------------------*
052100931011     C                   ENDSR
