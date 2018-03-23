000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300931011      * TRAS21R1                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600931011      *          SELEZIONE DATI DI PAGAMENTO: NR.DOCUM.              *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000931012      * 31 -OFF VISUALIZZAZIONE  RECORD DI CONTROLLO                 *
001100931012      * 30 -OFF VISUALIZZAZIONE SUBFILE                              *
001200931011      * 70 -SFLNXTCHG                                                *
001300931011      * 80 -INDICATORE X CHAIN                                       *
001400931011      * 81 -SFLEND                                                   *
001500931011      * 98 -ROLLUP                                                   *
001600931011?     *--------------------------------------------------------------*
001700931011      * KC -FINE LAVORO                                              *
001800931011      * KG -INTERROGAZIONE BOLLE                                     *
001900931011      * KL -RITORNO                                                  *
002000931011?     *--------------------------------------------------------------*
002100941102     FTABEL00F  IF   E           K DISK
002200941102     FTNSC21D   CF   E             WORKSTN
002300941102     F                                     SFILE(SC21S01:NRR)
002400941102?     *--------------------------------------------------------------*
002500941102      *  Schiere per caricamento dati valute
002600941102     D CCV             S              3    DIM(100)
002700991231     D DVA             S              1    DIM(100)
002800931011?     *--------------------------------------------------------------*
002900931011?     *  DS                                                          *
003000931011?     *--------------------------------------------------------------*
003100941102     D DSCV          E DS
003200931011     D KPJBA         E DS
003300991231     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003400991231     D DGED          E DS
003500010614     D YEURDS        E DS                  EXTNAME(YEURCODS)
003600020909     D FnCsb000      E DS
003700020909     D                                     EXTNAME(FNCSB00F)
003800020909     D                                     INZ
003900931011      *
004000941102     D WLBDA8          DS
004100941102     D  G02DAT                 1      8  0
004200941102     D  G02INV                 9     16  0
004300941102     D  G02ERR                17     17
004400941102     D  G02TGI                18     22  0
004500941102     D TNSC20        E DS                  EXTNAME(TNSC20DS)
004600931011      *
004700931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800931011     D CNCR80        E DS
004900931011      *
005000931011     D                SDS
005100931011     D  NOMPG                  1     10
005200931011      *---------------------------------------------------------------*
005300931011      *   C I C L O      P R I N C I P A L E                          *
005400931011      *---------------------------------------------------------------*
005500020909
005600020909     C/EXEC SQL
005700020909     C+ DECLARE FNCSB00F CURSOR FOR SELECT * FROM FNCSB00F WHERE CSBRMN =
005800020909     C+ :D20NRM AND CSBLNP = :D20LNP AND (CSBAAS * 10000) + CSBMGS
005900020909     C+ BETWEEN :D20SDD AND :D20SDA FOR READ ONLY
006000020909     C/END-EXEC
006100020909
006200931011     C*  CARICAMENTO SUBFILE
006300931011     C                   EXSR      CARSFL
006400931011     C*
006500931011     C*  LOOP
006600931011     C     £FINE         DOUEQ     'S'
006700931011     C*
006800931011     C     TIPVID        CASEQ     '1'           GESVD1
006900931011     C     TIPVID        CASEQ     '2'           GESVD2
007000931011     C                   END
007100931011     C*
007200931011     C                   END
007300020909
007400020909     C/EXEC SQL
007500020909     C+ CLOSE FNCSB00F
007600020909     C/END-EXEC
007700020909
007800931011     C*
007900941102     C                   MOVEL     TNSC20        KPJBU
008000931011     C                   SETON                                        LR
008100931011      *---------------------------------------------------------------*
008200931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
008300931011      *---------------------------------------------------------------*
008400931011     C     CARSFL        BEGSR
008500931011     C*
008600931011     C*  Pulisco subfile
008700931011     C                   SETON                                        3031
008800941102     C                   WRITE     SC21C01
008900931012     C                   SETOFF                                       31
009000931011     C*
009100931011     C*  Caricamento subfile
009200020909
009300020909     C/EXEC SQL
009400020909     C+ OPEN FNCSB00F
009500020909     C/END-EXEC
009600020909
009700020909     C/EXEC SQL
009800020909     C+ FETCH NEXT FROM FNCSB00F INTO :FnCsb000
009900020909     C/END-EXEC
010000020909
010100020909     C                   EVAL      *IN81 = (SQLCod = 100 OR SQLCod < 0)
010200020909
010300020909     C*    KEYRMN        SETLL     FNCSB04L
010400020909     C*    KEYRMN        READE     FNCSB04L                               81
010500931011     C*
010600931011     C                   Z-ADD     0             NRR               4 0
010700931013     C                   MOVEL     '1'           TIPVID            1
010800931011     C*
010900931011     C     *IN81         IFEQ      '0'
011000931012     C                   SETOFF                                       30
011100931011     C                   EXSR      ROLLUP
011200931011     C                   END
011300931011     C*
011400931013     C     NRR           IFNE      0
011500931013     C                   MOVEL     '2'           TIPVID            1
011600931013     C                   SETOFF                                       30
011700931013     C                   END
011800931013     C*
011900931012     C                   MOVEL     NOMPG         NOMPGM
012000931011     C*
012100931011     C                   ENDSR
012200931011     C*----------------------------------------------------*
012300931011     C*   ROLLUP: Caricamento di una pagina                *
012400931011     C*----------------------------------------------------*
012500931011     C     ROLLUP        BEGSR
012600931011     C*
012700931011     C     *IN81         IFEQ      '0'
012800931014     C                   Z-ADD     SAVRE1        NRR
012900931011     C                   Z-ADD     0             NRIG              4 0
013000931011     C*
013100931013     C     NRIG          DOWLT     14
013200931011     C     *IN81         ANDEQ     '0'
013300931012     C* Scrivo record
013400931013     C*
013500931013     C* SOLO CONTRASSEGNI INCASSATI NON PAGATI
013600941102     C     D20TCR        IFEQ      'I'
013700931013     C     CSBDDC        COMP      0                                      20
013800931013     C  N20CSBDDP        COMP      0                                  2020
013900931013     C                   END
014000931013     C* SOLO CONTRASSEGNI PAGATI
014100941102     C     D20TCR        IFEQ      'P'
014200931013     C     CSBNDP        COMP      0                                      20
014300931013     C                   END
014400931013     C*  Test Tipo Contrassegni
014500931013     C     *IN20         IFEQ      '0'
014600931012     C                   MOVEL     *BLANKS       VSEL
014700931012     C                   MOVEL     CSBAAS        ANNO
014800941103     C                   MOVEL     CSBMGS        MM                2 0
014900941102     C                   MOVE      CSBMGS        GG                2 0
015000931012     C                   MOVEL     GG            VDAT
015100941102     C                   MOVE      MM            VDAT
015200931012     C                   MOVE      CSBLNP        VLNP1
015300931012     C                   MOVE      CSBNRS        VSER1
015400931012     C                   MOVE      CSBNSP        VSPE1
015500931012     C                   MOVEL     CSBRSM        VMIT
015600931012     C                   MOVEL     CSBRSD        VDES
015700931012     C                   MOVE      CSBRMN        VRIF
015800941102     C                   MOVE      CSBVCA        VVCA
015900011002     C     VVCA          IFEQ      *BLANK
016000011002     C                   MOVE      'ITL'         VVCA
016100011002     C                   END
016200941102     C                   Z-ADD     CSBCAS        VIMP
016300941102     C* Controllo la divisa
016400941102     C     VVCA          IFNE      *BLANKS
016500941102     C                   Z-ADD     1             XX
016600941102     C     VVCA          LOOKUP    CCV(XX)                                32
016700941102     C                   END
016800941102     C* Somma nel totale corretto
016900941102     C     VVCA          IFEQ      *BLANKS
017000010928     C     VVCA          OREQ      §GEDCN
017100941102     C                   ADD       CSBCAS        TOT
017200941102     C                   ELSE
017300991231     C     DVA(XX)       IFEQ      'S'
017400991231     C     *IN32         ANDEQ     '1'
017500991231     C                   ADD       CSBCAS        TOTEUR
017600991231     C  N08              SETON                                        08
017700991231     C                   ELSE
017800010614     C* Per convertire gli importi in valute estere utilizzo
017900010614     C* la routine YEURCO in quanto il cambio indicato sul
018000010614     C* file dei C/assegni di filiale non è affidabile
018100010614     C                   CLEAR                   YEURDS
018200010614     C                   MOVEL     VVCA          YECDVI
018300010614     C                   Z-ADD     CSBCAS        YECIMI
018400010928     C                   MOVEL     §GEDCN        YECDVO
018500010614     C* passo al pgm io il nr. dei decimali della divisa corrente
018600010614     C* in base a quanto memorizzato sulla tabella CV
018700010614     C                   MOVE      NRDEC         YECDCO
018800010614     C                   MOVEL     'H'           YECTAR
018900010614     C                   CALL      'YEURCO'
019000010614     C                   PARM                    YEURDS
019100010614     C                   Z-ADD     YECIMO        IMPV             13 3
019200000103     C*
019300941102     C                   ADD       IMPV          TOTV
019400941102     C  N07              SETON                                        07
019500941102     C                   END
019600991231     C                   END
019700931012     C                   ADD       1             NRR
019800931012     C                   ADD       1             NRIG
019900941102     C                   WRITE     SC21S01
020000931013     C                   END
020100931012     C*
020200020909     C*    KEYRMN        READE     FNCSB04L                               81
020300020909
020400020909     C/EXEC SQL
020500020909     C+ FETCH NEXT FROM FNCSB00F INTO :FnCsb000
020600020909     C/END-EXEC
020700020909
020800020909     C                   EVAL      *IN81 = (SQLCod = 100 OR SQLCod < 0)
020900020909
021000931011     C                   END
021100931011     C*
021200931011     C*
021300931014     C     NRR           IFGT      SAVRE1
021400931014     C     SAVRE1        ADD       1             DSPNR1
021500931014     C                   Z-ADD     NRR           SAVRE1            4 0
021600931014     C                   END
021700931014     C*
021800931011     C                   END
021900931011     C*
022000931011     C                   ENDSR
022100931011     C*----------------------------------------------------*
022200931011     C*  Routine gestione operazioni prima videata         *
022300931011     C*----------------------------------------------------*
022400931011     C     GESVD1        BEGSR
022500931011     C*-                                                   *
022600931012     C                   SETON                                        30
022700941102     C                   WRITE     SC21Z02
022800941102     C                   WRITE     SC21D02
022900941102     C                   EXFMT     SC21C01
023000931011     C*                                                    *
023100931011     C* Fine Programma                                     *
023200931011     C     *INKL         IFEQ      '1'
023300931011     C                   MOVEL     'S'           £FINE
023400931011     C                   END
023500931011     C*-                                                   *
023600931011     C                   ENDSR
023700931011     C*----------------------------------------------------*
023800931011     C*  Routine gestione operazioni subfile               *
023900931011     C*----------------------------------------------------*
024000931011     C     GESVD2        BEGSR
024100931011     C*-                                                   *
024200941102     C                   WRITE     SC21Z01
024300941102     C                   WRITE     SC21D01
024400941102     C                   EXFMT     SC21C01
024500931011     C*                                                    *
024600931011     C* Fine Programma                                     *
024700931011     C     *INKC         IFEQ      '1'
024800931011     C                   MOVEL     'S'           £FINE
024900941102     C                   MOVE      3             D20CMD
025000931011     C                   GOTO      FINVD2
025100931011     C                   END
025200931011     C* Ritorno                                            *
025300931011     C     *INKL         IFEQ      '1'
025400931011     C                   MOVEL     'S'           £FINE
025500941102     C                   MOVE      12            D20CMD
025600931011     C                   GOTO      FINVD2
025700931011     C                   END
025800931011     C* ROLLUP
025900931011     C   98              EXSR      ROLLUP
026000931011     C* Interrogazione Bolle                               *
026100931011     C     *INKG         IFEQ      '1'
026200931011     C                   MOVEL     *BLANKS       KPJBU
026300170125     C***                CALL      'FNLS04R'
026400170125     C***                PARM                    KPJBA
026401170125     C                   CALL      'FNLRU6R'
026402170125     C                   PARM                    KPJBA
026500931011     C                   END
026600931011     C*                                                    *
026700931011     C                   EXSR      CNTVD2
026800931011     C*                                                    *
026900931012     C     FINVD2        ENDSR
027000931011     C*----------------------------------------------------*
027100931011     C*   CNTVD2: Controlli subfile                        *
027200931011     C*----------------------------------------------------*
027300931011     C     CNTVD2        BEGSR
027400931011     C*                                                    *
027500931011     C                   Z-ADD     1             NRR
027600941103     C                   MOVEL     ' '           VSEL
027700941102     C                   READC     SC21S01                                82
027800931011     C*                                                    *
027900931011     C     *IN82         DOWEQ     '0'
028000931011     C*                                                    *
028100941103     C     VSEL          IFEQ      '1'
028200931011     C                   MOVEL     *BLANKS       KPJBU
028300931012     C                   Z-ADD     NRR           SAVREC            4 0
028400941102     C                   MOVE      ANNO          D20ANN
028500941102     C                   MOVE      VLNP1         D20LNP
028600941102     C                   MOVE      VSER1         D20NSR
028700941102     C                   MOVE      VSPE1         D20NSP
028800941102     C                   MOVEL     TNSC20        KPJBU
028900931012     C* Interrogazione                                     *
029000941102     C                   CALL      'TNSC23R'
029100931012     C                   PARM                    KPJBA
029200931011     C*
029300931012     C                   Z-ADD     SAVREC        NRR
029400931014     C                   Z-ADD     SAVREC        DSPNR1
029500941102     C     NRR           CHAIN     SC21S01                            90
029600931011     C                   SETON                                        70
029700931011     C                   MOVEL     ' '           VSEL
029800941102     C  N90              UPDATE    SC21S01
029900931011     C                   END
030000931011     C*
030100941102     C                   READC     SC21S01                                82
030200931011     C                   END
030300931012     C*
030400931011     C                   ENDSR
030500931011      *---------------------------------------------------------------*
030600941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
030700931011      *---------------------------------------------------------------*
030800941102     C     *INZSR        BEGSR
030900931011      *---------------------------------------------------------------*
031000931011     C     *ENTRY        PLIST
031100931011     C                   PARM                    KPJBA
031200941102     C                   MOVEL     KPJBU         TNSC20
031300931012     C*
031400931011     C                   Z-ADD     1             CODUT
031500931011     C                   CALL      'X§PARUT'
031600931011     C                   PARM                    UT§DSE
031700931011     C*          CODUT     CABEQ-1        FINE
031800931011     C                   MOVEL     RAGUT         RSUT             20
031900931011     C                   MOVEL     REC80         CNCR80
032000931011     C                   MOVEL     KNMUS         §SEI              6
032100931011     C*
032200931012     C     KEYRMN        KLIST
032300941102     C                   KFLD                    D20NRM
032400941102     C*
032500941102     C     KTAB          KLIST
032600941102     C                   KFLD                    KKUT
032700941102     C                   KFLD                    KCOD
032800941102     C                   Z-ADD     1             KKUT
032900931012     C*----------------------------------------------------*
033000941102     C*  Definizione variabili
033100941102     C     *LIKE         DEFINE    TBLKUT        KKUT
033200941102     C     *LIKE         DEFINE    TBLCOD        KCOD
033300941102     C*----------------------------------------------------*
033400931012     C                   MOVEL     'N'           £FINE             1
033500931014     C                   SETON                                        02
033600941102     C                   MOVE      D20NRM        NRMIT
033700991231     C*----------------------------------------------------*
033800010928     C*  RECUPERO DIVISA MONETA CONTO
033900991231     C                   CLEAR                   DGED
034000991231     C                   CLEAR                   DSBS02
034100991231     C                   MOVEL     'C'           T02MOD
034200991231     C                   MOVEL     KNSIF         T02SIF
034300991231     C                   MOVEL     'GED'         T02COD
034400991231     C                   MOVEL     '1'           T02KE1
034500991231     C                   CALL      'TIBS02R'
034600991231     C                   PARM                    KPJBA
034700991231     C                   PARM                    DSBS02
034800991231     C*
034900991231    3C     T02ERR        IFEQ      ' '
035000991231     C                   MOVEL     T02UNI        DGED
035100010928     C                   MOVEL     §GEDCN        DIVCOR
035200991231    3C                   ENDIF
035300991231     C*----------------------------------------------------*
035400991231     C* Carico tabella cambi/divise
035500991231     C                   Z-ADD     0             XX                3 0
035600991231     C                   MOVEL     'CV'          KCOD
035700991231     C     KTAB          CHAIN     TABEL                              31
035800991231     C     *IN31         DOWEQ     '0'
035900991231     C     XX            ANDLT     100
036000991231     C                   MOVEL     TBLKEY        WDIV              3
036100991231     C     TBLFLG        IFEQ      ' '
036200991231     C                   ADD       1             XX
036300991231     C                   MOVEL     TBLKEY        CCV(XX)
036400991231     C                   MOVEL     TBLUNI        DSCV
036500991231     C                   MOVEL     §CVDVA        DVA(XX)
036600011001     C*****                MOVEL§CVCMB    CMB,XX
036700991231     C     §CVDVA        IFEQ      'S'
036800010928     C     WDIV          ANDNE     §GEDCN
036900991231     C                   MOVEL     WDIV          DIVALT
037000991231     C                   END
037100010928     C* Memorizzo nr. decimali della divisa DI CONTO
037200010928     C     WDIV          IFEQ      §GEDCN
037300000103     C                   Z-ADD     §CVDEC        NRDEC             2 0
037400000103     C                   END
037500991231     C                   END
037600991231     C     KTAB          READE     TABEL                                  31
037700991231     C                   END
037800931012     C*----------------------------------------------------*
037900931011     C                   ENDSR
