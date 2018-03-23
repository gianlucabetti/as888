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
002000941103     FCNABI00F  IF   E           K DISK
002100941103     FTNSC22P   O    E             PRINTER OFLIND(*IN01)
002200941103     D*--------------------------------------------------------------*
002300941103     D CCV             S              3    DIM(100)
002400000103     D DVA             S              1    DIM(100)
002500011001     D*****               CMB       100  9 3             CAMBIO DIVISA ITA.
002600020506     D*UNI             S              3    DIM(29)
002700020506     D £1              S              3  0 DIM(30)
002800941103?     *--------------------------------------------------------------*
002900941103?     *  DS                                                          *
003000941103?     *--------------------------------------------------------------*
003100941103     D KPJBA         E DS
003200941103      *
003300010615     D YEURDS        E DS                  EXTNAME(YEURCODS)
003400941103     D TNSC20        E DS                  EXTNAME(TNSC20DS)
003500941103     D DSCV          E DS
003600000103     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003700000103     D DGED          E DS
003701020506     d trul06ds      e ds
003702020506     d  lin                    1     90  0 dim(30)
003800941103      *
003900941103     D WLBDA8          DS
004000941103     D  G02DAT                 1      8  0
004100941103     D  G02INV                 9     16  0
004200941103     D  G02ERR                17     17
004300941103     D  G02TGI                18     22  0
004400941103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004500941103     D CNCR80        E DS
004600941103      *
004700941103      *  Ds x numero spedizione contrass.
004800941103     D                 DS
004900941103     D  CSBLNP                 1      3  0
005000941103     D  CSBAAS                 4      7  0
005100941103     D  CSBNRS                 8      9  0
005200941103     D  CSBNSP                10     16  0
005300941103     D  NUMSPE                 1     16  0
005400941103      *
005500941103      *  Ds x numero spedizione pagamento
005600941103     D                 DS
005700941103     D  CSBPLP                 1      3  0
005800941103     D  CSBPAA                 4      7  0
005900941103     D  CSBPNS                 8      9  0
006000941103     D  CSBPSP                10     16  0
006100941103     D  SPEPAG                 1     16  0
006200941103      *
006300941103     D                SDS
006400941103     D  NOMPG                  1     10
006500941103     D*
006600941103     D COST1           C                   CONST('CONTANTE            ')
006700941103?     *--------------------------------------------------------------*
006800941103?     *  FILE                                                        *
006900941103?     *--------------------------------------------------------------*
007000941103     IAZORG01L  AA
007100941103     I                             P    4    5 0ORGFIL
007200941103     I                                 14   33  ORGDES
007300931011      *---------------------------------------------------------------*
007400931011      *   C I C L O      P R I N C I P A L E                          *
007500931011      *---------------------------------------------------------------*
007600931012     C*  PRIMA LETTURA
007700941103     C     KEYPAG        SETLL     FNCSB02L
007800941103     C                   READ      FNCSB02L                               80
007900931012     C*  LOOP
008000931012     C     *IN80         DOWEQ     '0'
008100941103     C     CSBDDP        ANDLE     D20PDA
008200931012     C                   EXSR      STAMPA
008300941103     C                   READ      FNCSB02L                               80
008400931011     C                   END
008500931011     C*
008600931013     C     TOT           IFGT      0
008700931013     C   01              WRITE     TES00
008800931013     C   01              SETOFF                                       01
008900931013     C                   WRITE     TOT00
009000931013     C                   END
009100000103     C* Se ci sono dei C/Assegni in Euro stampo il totale relativo
009200000103     C     TOTEU         IFGT      0
009300000103     C   01              WRITE     TES00
009400000103     C   01              SETOFF                                       01
009500000103     C                   WRITE     TOTE0
009600000103     C                   END
009700000103     C*
009800931011     C*
009900941103     C     TOTV          IFGT      0
010000941103     C   01              WRITE     TES00
010100941103     C   01              SETOFF                                       01
010200011002     C                   MOVEL     §GEDCN        VALV
010300941103     C                   WRITE     TOTV0
010400941103     C                   END
010500941103     C*
010600941103     C                   MOVEL     TNSC20        KPJBU
010700931011     C                   SETON                                        LR
010800931011     C*----------------------------------------------------*
010900931012     C*   STAMPA: Trasferimento dati da file a stampa      *
011000931011     C*----------------------------------------------------*
011100931012     C     STAMPA        BEGSR
011200931011     C*
011300931013     C* SOLO CONTRASSEGNI INCASSATI NON PAGATI
011400941103     C     D20TCR        IFEQ      'I'
011500931013     C     CSBDDC        COMP      0                                      20
011600931013     C  N20CSBDDP        COMP      0                                  2020
011700931013     C                   END
011800931013     C* SOLO CONTRASSEGNI PAGATI
011900941103     C     D20TCR        IFEQ      'P'
012000931013     C     CSBNDP        COMP      0                                      20
012100931013     C                   END
012200931013     C*  Test Tipo Contrassegni
012300931013     C     *IN20         IFEQ      '0'
012400931013     C*  Test  x Numero documento
012500941103     C     D20PND        IFEQ      0
012600941103     C     D20PND        OREQ      CSBNDP
012700931013     C*  Test  x tipo intestazione
012800941103     C     D20PTN        IFEQ      *BLANKS
012900941103     C     D20PTN        OREQ      'M'
013000931202     C     CSBTPI        ANDEQ     'M'
013100941103     C     D20PTN        OREQ      'B'
013200931202     C     CSBTPI        ANDEQ     ' '
013300931013     C* Scrivo record se corrisponde al range richiesto
013400941103     C     D20PCD        IFEQ      0
013500941103     C     D20PCD        OREQ      CSBCDI
013600000103     C* Scrivo record se divisa corrisponde
013700000103     C     D20PDV        IFEQ      *BLANKS
013800000103     C     D20PDV        OREQ      CSBVCA
013900011001     C     D20PDV        OREQ      §GEDCN
014000000103     C     CSBVCA        ANDEQ     *BLANKS
014100931013     C*  Test. x Ragione sociale
014200941103     C     D20PRA        IFNE      *BLANK
014300931013     C                   SETOFF                                       40
014400931013     C*  Loop di ricerca sottostringa nel campo del subfile
014500931013     C                   DO        X             T                 2 0
014600941103     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
014700941103     C     RESULT        COMP      D20PRA                                 40
014800931013     C  N40              END
014900931013     C*
015000931013     C                   ELSE
015100931013     C                   SETON                                        40
015200931013     C                   END
015300931013     C*
015400931013     C     *IN40         IFEQ      '1'
015500931014     C                   MOVE      CSBRMN        RNM
015600931014     C                   MOVEL     CSBLNA        LNA
015700931013     C                   MOVEL     CSBLNP        LNP
015800931012     C                   MOVEL     CSBAAS        AA
015900931012     C                   MOVE      CSBNRA        NDC
016000931013     C                   MOVE      CSBDTE        XDTAMG
016100931013     C                   Z-ADD     0             XDTGMA
016200931013     C                   Z-ADD     2             XDTSTA
016300931013     C                   CALL      'XDATA'       PLDAT
016400931013     C                   MOVE      XDTGMA        DDC
016500931013     C                   MOVE      CSBNRS        NRS
016600931013     C                   MOVE      CSBNSP        NSP
016700941103     C                   MOVE      CSBAAS        WAA2              2 0
016800941103     C                   MOVEL     WAA2          XDTAMG
016900941103     C                   MOVE      CSBMGS        XDTAMG
017000941103     C                   Z-ADD     0             XDTGMA
017100941103     C                   Z-ADD     2             XDTSTA
017200941103     C                   CALL      'XDATA'       PLDAT
017300941103     C                   MOVEL     XDTGMA        DSP
017400941103     C                   MOVEL     CSBVCA        VCA
017500011002     C     VCA           IFEQ      *BLANK
017600011002     C                   MOVE      'ITL'         VCA
017700011002     C                   END
017800941103     C                   Z-ADD     CSBCAS        IMP
017900941103     C* Controllo la divisa
018000941103     C     VCA           IFNE      *BLANKS
018100941103     C                   Z-ADD     1             XX
018200941103     C     VCA           LOOKUP    CCV(XX)                                32
018300941103     C                   END
018400941103     C* Somma nel totale corretto
018500960624     C     CSBPLP        LOOKUP    £1                                     35
018600941103     C     VCA           IFEQ      *BLANKS
018700011001     C     VCA           OREQ      §GEDCN
018800960403     C     NUMSPE        IFEQ      SPEPAG
018900960403     C     SPEPAG        OREQ      0
019000960624     C     *IN35         OREQ      '0'
019100941103     C                   ADD       CSBCAS        TOT
019200960403     C                   END
019300000103     C                   ELSE
019400000103     C     DVA(XX)       IFEQ      'S'
019500000103     C     *IN32         ANDEQ     '1'
019600000103     C                   ADD       CSBCAS        TOTEU
019700000103     C                   ELSE
019800010615     C***                  Z-ADDCSBCMB    CAMBIO  94
019900010615     C***        CSBCMP    IFNE 0
020000010615     C***                  Z-ADDCSBCMP    CAMBIO
020100010615     C***                  END
020200010615     C***                  Z-ADDCMB,XX    CAMBIO  94
020300010615     C***        CSBCMB    IFNE 0
020400010615     C***                  Z-ADDCSBCMB    CAMBIO  94
020500010615     C***                  END
020600010615     C***        CSBCMP    IFNE 0
020700010615     C***                  Z-ADDCSBCMP    CAMBIO
020800010615     C***                  END
020900010615     C***                  SELEC
021000010615     C***        NRDEC     WHEQ 2
021100010615     C***        CAMBIO    MULT CSBCAS    IMPV2  122H
021200010615     C***                  Z-ADDIMPV2     IMPV
021300010615     C***        NRDEC     WHEQ 1
021400010615     C***        CAMBIO    MULT CSBCAS    IMPV1  111H
021500010615     C***                  Z-ADDIMPV1     IMPV
021600010615     C***        NRDEC     WHEQ 0
021700010615     C***        CAMBIO    MULT CSBCAS    IMPV0  100H
021800010615     C***                  Z-ADDIMPV0     IMPV
021900010615     C***                  OTHER
022000010615     C***        CAMBIO    MULT CSBCAS    IMPV   133
022100010615     C***                  ENDSL
022200010615     C*
022300010615     C* Per convertire gli importi in valute estere utilizzo
022400010615     C* la routine YEURCO in quanto il cambio indicato sul
022500010615     C* file dei C/assegni di filiale non è affidabile
022600010615     C                   CLEAR                   YEURDS
022700010615     C                   MOVEL     VCA           YECDVI
022800010615     C                   Z-ADD     CSBCAS        YECIMI
022900011001     C                   MOVEL     §GEDCN        YECDVO
023000010615     C* passo al pgm io il nr. dei decimali della divisa corrente
023100010615     C* in base a quanto memorizzato sulla tabella CV
023200010615     C                   MOVE      NRDEC         YECDCO
023300010615     C                   MOVEL     'H'           YECTAR
023400010615     C                   CALL      'YEURCO'
023500010615     C                   PARM                    YEURDS
023600010615     C                   Z-ADD     YECIMO        IMPV             13 3
023700000103     C*
023800960403     C     NUMSPE        IFEQ      SPEPAG
023900960403     C     SPEPAG        OREQ      0
024000960624     C     *IN35         OREQ      '0'
024100941103     C                   ADD       IMPV          TOTV
024200941103     C                   END
024300960403     C                   END
024400000103     C                   END
024500941103     C*
024600931116     C                   MOVEL     *BLANKS       BAI
024700931116     C     NDC           IFEQ      *BLANKS
024800931116     C                   MOVEL     COST1         BAI
024900931116     C                   END
025000931013     C                   MOVE      CSBABI        ABI
025100931013     C                   MOVE      CSBCAB        CAB
025200931012     C     KEYABI        CHAIN     CNABI00F                           81
025300931012     C  N81              MOVEL     ABIIST        BAI
025400931013     C                   MOVEL     CSBTPA        ASS
025500931013     C                   MOVE      CSBTPI        ASS
025600931013     C                   MOVE      CSBCDI        CDM
025700931013     C                   MOVEL     CSBRSM        RGM
025800931013     C                   MOVEL     CSBRSD        RGD
025900931013     C*  Tipo Documento
026000931013     C     CSBFPC        IFEQ      '2'
026100931013     C                   MOVE      'BON.'        DOC
026200931013     C                   ELSE
026300931014     C     CSBPAB        IFEQ      99999
026400931116     C     CSBPCA        ANDEQ     99999
026500931014     C                   MOVE      'LETT'        DOC
026600931014     C                   ELSE
026700931014     C     CSBPAB        IFNE      0
026800931014     C     CSBPCA        ORNE      0
026900931013     C                   MOVE      'ASS.'        DOC
027000931013     C                   END
027100931013     C                   END
027200931014     C                   END
027300931013     C*
027400931013     C                   MOVE      CSBDDP        XDTAMG
027500931013     C                   Z-ADD     0             XDTGMA
027600931013     C                   Z-ADD     2             XDTSTA
027700931013     C                   CALL      'XDATA'       PLDAT
027800931013     C                   MOVE      XDTGMA        DDP
027900931013     C                   MOVE      CSBNDP        NDP
028000931013     C                   MOVEL     *BLANK        BAP
028100931013     C                   MOVE      CSBPAB        ABI
028200931013     C                   MOVE      CSBPCA        CAB
028300931013     C     KEYABI        CHAIN     CNABI00F                           81
028400931013     C  N81              MOVEL     ABIIST        BAP
028500931013     C                   MOVE      CSBDGC        XDTAMG
028600931013     C                   Z-ADD     0             XDTGMA
028700931013     C                   Z-ADD     2             XDTSTA
028800931013     C                   CALL      'XDATA'       PLDAT
028900931013     C                   MOVE      XDTGMA        DGC
029000931112     C                   MOVEL     *ZERO         LNPP
029100931112     C                   MOVEL     *ZERO         AAP
029200931112     C                   MOVE      *ZERO         NRSP
029300931112     C                   MOVE      *ZERO         NSPP
029400931112     C*
029500931112     C*  Stampo riferimnti spedizione pagamento contrassegno solo se
029600931112     C*  diverso da originale
029700931112     C     NUMSPE        IFNE      SPEPAG
029800960624     C     SPEPAG        OREQ      0
029900960624     C     *IN35         OREQ      '0'
030000931112     C                   MOVEL     CSBPLP        LNPP
030100931112     C                   MOVEL     CSBPAA        AAP
030200931112     C                   MOVE      CSBPNS        NRSP
030300931112     C                   MOVE      CSBPSP        NSPP
030400931112     C                   END
030500931013     C   01              WRITE     TES00
030600931013     C   01              SETOFF                                       01
030700931013     C                   WRITE     DET00
030800931013     C                   END
030900931013     C                   END
031000931013     C                   END
031100931013     C                   END
031200931013     C                   END
031300000103     C                   END
031400931012     C*
031500931011     C                   ENDSR
031600931011      *---------------------------------------------------------------*
031700941103      *  *INZSR: OPERAZIONI INIZIALI                                  *
031800931011      *---------------------------------------------------------------*
031900941103     C     *INZSR        BEGSR
032000931011      *---------------------------------------------------------------*
032100931011     C     *ENTRY        PLIST
032200931011     C                   PARM                    KPJBA
032300941103     C                   MOVEL     KPJBU         TNSC20
032400931012     C*-
032500931012     C* PLIST per richiamo Routine XDATA
032600931012     C     PLDAT         PLIST
032700931012     C                   PARM                    XDTAMG            6 0
032800931012     C                   PARM                    XDTGMA            6 0
032900931012     C                   PARM                    XDTSTA            1 0
033000931012     C*
033100931011     C                   Z-ADD     1             CODUT
033200931011     C                   CALL      'X§PARUT'
033300931011     C                   PARM                    UT§DSE
033400931011     C*          CODUT     CABEQ-1        FINE
033500931011     C                   MOVEL     RAGUT         RSUT             20
033600931011     C                   MOVEL     REC80         CNCR80
033700931011     C                   MOVEL     KNMUS         §SEI              6
033800931014     C*
033900931013     C     KEYPAG        KLIST
034000941103     C                   KFLD                    D20PDD
034100941103     C                   KFLD                    D20PND
034200931012     C*
034300931012     C     KEYABI        KLIST
034400931013     C                   KFLD                    ABI               5 0
034500931013     C                   KFLD                    CAB               5 0
034600941103     C*
034700941103     C     KTAB          KLIST
034800941103     C                   KFLD                    KKUT
034900941103     C                   KFLD                    KCOD
035000960624     C     KTAB1         KLIST
035100960624     C                   KFLD                    KKUT
035200960624     C                   KFLD                    KCOD
035300960624     C                   KFLD                    KKEY
035400941103     C                   Z-ADD     1             KKUT
035500941103     C*----------------------------------------------------*
035600941103     C*  Definizione variabili
035700941103     C     *LIKE         DEFINE    TBLKUT        KKUT
035800941103     C     *LIKE         DEFINE    TBLCOD        KCOD
035900960624     C     *LIKE         DEFINE    TBLKEY        KKEY
036000941103     C*----------------------------------------------------*
036100941103     C                   MOVEL     'N'           £FINE             1
036200941103     C                   MOVE      SIMFEL        ORGFIL
036300941103     C     ORGFIL        CHAIN     AZORG01L                           78
036400941103     C  N78              MOVEL     ORGDES        FILIAL
036500931012     C*----------------------------------------------------*
036600941103     C                   MOVE      '  '          D20PRA
036700931012     C*----------------------------------------------------*
036800931013     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
036900941103     C     '  '          SCAN      D20PRA        LENGHT            2 0
037000931013     C                   SUB       1             LENGHT
037100941103     C     35            SUB       LENGHT        X                 2 0
037200931013     C*----------------------------------------------------*
037300931013     C*  Scrittura Prima pagina
037400931013     C                   MOVEL     NOMPG         NOMPGM
037500941103     C                   MOVE      D20INT        S22SEL
037600941103     C                   MOVE      D20ANN        S22AAS
037700941103     C                   MOVE      D20LNP        S22LNP
037800941103     C                   MOVE      D20NSR        S22NSR
037900941103     C                   MOVE      D20NRM        S22NRM
038000941103     C                   MOVE      D20SDD        G02INV
038100941103     C                   Z-ADD     0             G02DAT
038200941103     C                   MOVEL     '3'           G02ERR
038300941103     C                   CALL      'XSRDA8'
038400941103     C                   PARM                    WLBDA8
038500941103     C                   MOVE      G02DAT        S22SDD
038600941103     C                   MOVE      D20SDA        G02INV
038700941103     C                   Z-ADD     0             G02DAT
038800941103     C                   MOVEL     '3'           G02ERR
038900941103     C                   CALL      'XSRDA8'
039000941103     C                   PARM                    WLBDA8
039100941103     C                   MOVE      G02DAT        S22SDA
039200941103     C                   MOVE      D20SCD        S22SCD
039300941103     C                   MOVEL     D20SRA        S22SRS
039400941103     C                   MOVE      D20PDD        G02INV
039500941103     C                   Z-ADD     0             G02DAT
039600941103     C                   MOVEL     '3'           G02ERR
039700941103     C                   CALL      'XSRDA8'
039800941103     C                   PARM                    WLBDA8
039900941103     C                   MOVE      G02DAT        S22PDD
040000941103     C                   MOVE      D20PDA        G02INV
040100941103     C                   Z-ADD     0             G02DAT
040200941103     C                   MOVEL     '3'           G02ERR
040300941103     C                   CALL      'XSRDA8'
040400941103     C                   PARM                    WLBDA8
040500941103     C                   MOVE      G02DAT        S22PDA
040600941103     C                   MOVE      D20PND        S22PND
040700941103     C                   MOVE      D20PTN        S22PTI
040800941103     C                   MOVE      D20PCD        S22PCD
040900941103     C                   MOVEL     D20PRA        S22PRS
041000941103     C                   MOVE      D20TCR        S22TCA
041100931013     C*
041200931013     C     S22TCA        IFEQ      ' '
041300931013     C                   MOVEL     'TUTTI'       S22DCA
041400931013     C                   ELSE
041500931013     C     S22TCA        IFEQ      'I'
041600931013     C                   MOVEL     'INCASSAT'    S22DCA
041700931013     C                   MOVE      'I      '     S22DCA
041800931013     C                   ELSE
041900931013     C     S22TCA        IFEQ      'P'
042000931013     C                   MOVEL     'PAGATI  '    S22DCA
042100931013     C                   END
042200931013     C                   END
042300931013     C                   END
042400931013     C*
042500931014     C                   SETON                                        04
042600931013     C                   WRITE     PAG00
042700931013     C                   MOVEL     *ALL'-'       CMP198
042800931014     C                   MOVEL     *ALL'-'       CM1198
042900931013     C*
043000931013     C                   SETON                                        01
043100941103     C*----------------------------------------------------*
043200000103     C*  RECUPERO DIVISA MONETA CORRENTE
043300000103     C                   CLEAR                   DGED
043400000103     C                   CLEAR                   DSBS02
043500000103     C                   MOVEL     'C'           T02MOD
043600000103     C                   MOVEL     KNSIF         T02SIF
043700000103     C                   MOVEL     'GED'         T02COD
043800000103     C                   MOVEL     '1'           T02KE1
043900000103     C                   CALL      'TIBS02R'
044000000103     C                   PARM                    KPJBA
044100000103     C                   PARM                    DSBS02
044200000103     C*
044300000103    3C     T02ERR        IFEQ      ' '
044400000103     C                   MOVEL     T02UNI        DGED
044500011001     C                   MOVEL     §GEDCN        DIVCOR
044600000103    3C                   ENDIF
044700000103     C*----------------------------------------------------*
044800000103     C* Carico tabella cambi/divise
044900000103     C                   Z-ADD     0             XX                3 0
045000000103     C                   MOVEL     'CV'          KCOD
045100000103     C     KTAB          CHAIN     TABEL                              31
045200000103     C     *IN31         DOWEQ     '0'
045300000103     C     XX            ANDLT     100
045400000103     C                   MOVEL     TBLKEY        WDIV              3
045500000103     C     TBLFLG        IFEQ      ' '
045600000103     C                   ADD       1             XX
045700000103     C                   MOVEL     TBLKEY        CCV(XX)
045800000103     C                   MOVEL     TBLUNI        DSCV
045900000103     C                   MOVEL     §CVDVA        DVA(XX)
046000011001     C******               MOVEL§CVCMB    CMB,XX
046100000103     C     §CVDVA        IFEQ      'S'
046200011001     C     WDIV          ANDNE     §GEDCN
046300000103     C                   MOVEL     WDIV          DIVALT
046400000103     C                   END
046500000103     C* Memorizzo nr. decimali della divisa corrente
046600011001     C     WDIV          IFEQ      §GEDCN
046700000103     C                   Z-ADD     §CVDEC        NRDEC             2 0
046800000103     C                   END
046900000103     C                   END
047000000103     C     KTAB          READE     TABEL                                  31
047100000103     C                   END
047200960624     C* Carico £1
047201020506     c                   clear                   trul06ds
047202020506     c                   eval      d06cod = '£1'
047203020506     c                   movel     simfel        d06key
047204020506     c                   movel     trul06ds      kpjbu
047205020506     c                   call      'TRUL06R'
047206020506     c                   parm                    kpjba
047207020506     c                   movel     kpjbu         trul06ds
047208020506     c                   movea     lin           £1
047300020506     C*                  Z-ADD     0             £1
047400020506     C*                  MOVEL     '£1'          KCOD
047500020506     C*                  MOVEL     *BLANKS       KKEY
047600020506     C*                  MOVEL     SIMFEL        KKEY
047700020506     C*    KTAB1         CHAIN     TABEL                              31
047800020506     C*    *IN31         IFEQ      '0'
047900020506     C*    TBLFLG        ANDEQ     ' '
048000020506     C*                  MOVEA     TBLUNI        UNI
048100020506     C*                  Z-ADD     0             XY                3 0
048200020506     C*                  DO        29            XX
048300020506     C*    UNI(XX)       IFGT      *ZEROS
048400020506     C*                  ADD       1             XY
048500020506     C*                  MOVEL     UNI(XX)       £1(XY)
048600020506     C*                  END
048700020506     C*                  END
048800020506     C*                  END
048900960624     C*  SE £1 = 0  --> SIMFEL
049000020506     C*                  XFOOT     £1            W£1               4 0
049100020506     C*    W£1           IFEQ      0
049200020506     C*                  Z-ADD     SIMFEL        £1(1)
049300020506     C*                  END
049400941103     C*----------------------------------------------------*
049500931011     C                   ENDSR
