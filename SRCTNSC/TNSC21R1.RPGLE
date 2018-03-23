000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300941102      * TNSC21R1                                                     *
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
001900931011      * KJ -STAMPA                                                   *
002000931011      * KI -SFLDROP                                                  *
002100931011      * KL -RITORNO                                                  *
002200931011?     *--------------------------------------------------------------*
002300941102     FFNCSB02L  IF   E           K DISK
002400931011     FCNABI00F  IF   E           K DISK
002500960624     FTABEL00F  IF   E           K DISK
002600941102     FTNSC21D1  CF   E             WORKSTN
002700941102     F                                     SFILE(SC21S01:NRR)
002800960624?     *--------------------------------------------------------------*
002900020506     D*UNI             S              3    DIM(29)
003000020506     D £1              S              3  0 DIM(30)
003100931011?     *--------------------------------------------------------------*
003200931011?     *  DS                                                          *
003300931011?     *--------------------------------------------------------------*
003400991231     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003500991231     D DGED          E DS
003600931011     D* DS x controllo rotture documento pagamento
003700931011     D DSPAG           DS
003800941102     D  CSBDDP                 1      8  0
003900941102     D  CSBNDP                 9     15  0
004000941102     D  CSBCDI                16     22  0
004100931011     D*
004200931011     D SAVDS           DS
004300941102     D  SAVDDP                 1      8  0
004400941102     D  SAVNDP                 9     15  0
004500941102     D  SAVCDI                16     22  0
004600931011     D KPJBA         E DS
004601020506     d trul06ds      e ds
004602020506     d  lin                    1     90  0 dim(30)
004700931011      *
004800941102     D TNSC20        E DS                  EXTNAME(TNSC20DS)
004900931011      *
005000931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005100931011     D CNCR80        E DS
005200941102     D*
005300941102     D WLBDA8          DS
005400941102     D  G02DAT                 1      8  0
005500941102     D  G02INV                 9     16  0
005600941102     D  G02ERR                17     17
005700941102     D  G02TGI                18     22  0
005800960403      *
005900960403      *  Ds x numero spedizione contrass.
006000960403     D                 DS
006100960403     D  CSBLNP                 1      3  0
006200960403     D  CSBAAS                 4      7  0
006300960403     D  CSBNRS                 8      9  0
006400960403     D  CSBNSP                10     16  0
006500960403     D  NUMSPE                 1     16  0
006600960403      *
006700960403      *  Ds x numero spedizione pagamento
006800960403     D                 DS
006900960403     D  CSBPLP                 1      3  0
007000960403     D  CSBPAA                 4      7  0
007100960403     D  CSBPNS                 8      9  0
007200960403     D  CSBPSP                10     16  0
007300960403     D  SPEPAG                 1     16  0
007400931011      *
007500931011     D                SDS
007600931011     D  NOMPG                  1     10
007700020102     D*
007800020102     D YEurCoDS      E DS
007900020102     D                                     INZ
008000931011      *---------------------------------------------------------------*
008100931011      *   COSTANTI TIPO DOCUMENTO                                     *
008200931014     D COST1           C                   CONST('BONIFICO      ')
008300931014     D COST2           C                   CONST('ASSEGNO       ')
008400931011     D COST3           C                   CONST('LETT. MITTENTI')
008500931011      *---------------------------------------------------------------*
008600931011      *   C I C L O      P R I N C I P A L E                          *
008700931011      *---------------------------------------------------------------*
008800931011     C*  CARICAMENTO SUBFILE
008900931011     C                   EXSR      CARSFL
009000931011     C*
009100931011     C*  LOOP
009200931011     C     £FINE         DOUEQ     'S'
009300931011     C*
009400931011     C     TIPVID        CASEQ     '1'           GESVD1
009500931011     C     TIPVID        CASEQ     '2'           GESVD2
009600931011     C                   END
009700931011     C*
009800931011     C                   END
009900931011     C*
010000941102     C                   MOVEL     TNSC20        KPJBU
010100931011     C                   SETON                                        LR
010200931011      *---------------------------------------------------------------*
010300931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
010400931011      *---------------------------------------------------------------*
010500931011     C     CARSFL        BEGSR
010600931011     C*
010700931011     C*  Pulisco subfile
010800931011     C                   SETON                                        3031
010900941102     C                   WRITE     SC21C01
011000931012     C                   SETOFF                                       31
011100931013     C                   Z-ADD     1             DSPNR1
011200931011     C*
011300931011     C*  Caricamento subfile
011400941102     C     KEYPAG        SETLL     FNCSB02L
011500941102     C                   READ      FNCSB02L                               81
011600931011     C*
011700931011     C                   Z-ADD     0             NRR               4 0
011800931013     C                   MOVEL     '1'           TIPVID            1
011900931011     C*
012000931011     C     *IN81         IFEQ      '0'
012100941102     C     CSBDDP        ANDLE     D20PDA
012200931012     C                   SETOFF                                       30
012300931011     C                   EXSR      SAVDAT
012400931011     C                   EXSR      ROLLUP
012500931011     C                   END
012600931013     C*
012700931013     C     NRR           IFNE      0
012800931013     C                   MOVEL     '2'           TIPVID            1
012900931013     C                   SETOFF                                       30
013000931013     C                   END
013100931011     C*
013200931012     C                   MOVEL     NOMPG         NOMPGM
013300931014     C                   Z-ADD     NRR           SAVRE1            4 0
013400931011     C*
013500931011     C                   ENDSR
013600931011     C*----------------------------------------------------*
013700931011     C*   ROLLUP: Caricamento di una pagina                *
013800931011     C*----------------------------------------------------*
013900931011     C     ROLLUP        BEGSR
014000931011     C*
014100931013     C*
014200931011     C     *IN81         IFEQ      '0'
014300941102     C     CSBDDP        ANDLE     D20PDA
014400931014     C                   Z-ADD     SAVRE1        NRR
014500931011     C                   Z-ADD     0             NRIG              4 0
014600931011     C*
014700931013     C     NRIG          DOWLT     16
014800931011     C     *IN81         ANDEQ     '0'
014900941102     C     CSBDDP        ANDLE     D20PDA
015000931011     C* Scrivo record a rottura codice reigstrazione
015100931011     C     DSPAG         IFNE      SAVDS
015200931011     C                   EXSR      WRIREC
015300931011     C                   EXSR      SAVDAT
015400931011     C                   END
015500960624     C     CSBPLP        LOOKUP    £1                                     35
015600960403     C     NUMSPE        IFEQ      SPEPAG
015700960403     C     SPEPAG        OREQ      0
015800960624     C     *IN35         OREQ      '0'
015900020102     C                   IF        CsbVca = 'ITL'
016000020102     C                   CLEAR                   YEurCoDS
016100020102     C                   EVAL      YEcDvI = CsbVca
016200020102     C                   EVAL      YEcImI = CsbCas
016300020102     C                   EVAL      YEcDvO = 'EUR'
016400020102     C                   CALL      'YEURCO'
016500020102     C                   PARM                    YEurCoDS
016600020102     C                   EVAL      ImpAss = ImpAss + YecImO
016700020102     C                   ELSE
016800941102     C                   ADD       CSBCAS        IMPASS
016900020102     C                   ENDIF
017000960403     C                   END
017100941102     C                   READ      FNCSB02L                               81
017200931011     C                   END
017300931011     C*
017400941102     C     SAVDDP        IFLE      D20PDA
017500931013     C     NRIG          ANDLT     16
017600931011     C                   EXSR      WRIREC
017700931013     C                   EXSR      SAVDAT
017800931011     C                   END
017900931011     C*
018000941102     C  N81CSBDDP        COMP      D20PDA                             81
018100931014     C     NRR           IFGT      SAVRE1
018200931014     C     SAVRE1        ADD       1             DSPNR1
018300931014     C                   Z-ADD     NRR           SAVRE1            4 0
018400931014     C                   END
018500931011     C                   END
018600931011     C*
018700931011     C                   ENDSR
018800931011     C*----------------------------------------------------*
018900931011     C*   SAVDAT: Salvataggio dati                         *
019000931011     C*----------------------------------------------------*
019100931011     C     SAVDAT        BEGSR
019200931011     C*
019300931013     C                   MOVEL     DSPAG         SAVDS
019400931013     C                   MOVEL     'N'           WRTREC            1
019500931013     C*
019600931013     C* SOLO CONTRASSEGNI INCASSATI NON PAGATI
019700941102     C     D20TCR        IFEQ      'I'
019800931013     C     CSBDDC        COMP      0                                      20
019900931013     C  N20CSBDDP        COMP      0                                  2020
020000931013     C                   END
020100931013     C* SOLO CONTRASSEGNI PAGATI
020200941102     C     D20TCR        IFEQ      'P'
020300931013     C     CSBNDP        COMP      0                                      20
020400931013     C                   END
020500931013     C*  Test Tipo Contrassegni
020600931013     C     *IN20         IFEQ      '0'
020700931013     C*  Test  x tipo intestazione
020800941102     C     D20PTN        IFEQ      *BLANKS
020900941102     C     D20PTN        OREQ      'M'
021000931013     C     CSBTPI        ANDEQ     'M'
021100941102     C     D20PTN        OREQ      'B'
021200931013     C     CSBTPI        ANDEQ     ' '
021300991230     C*  Test  x codice
021400991230     C     D20PCD        IFEQ      0
021500991230     C     D20PCD        OREQ      CSBCDI
021600991231     C* Scrivo record se divisa corrisponde
021700991231     C     D20PDV        IFEQ      *BLANKS
021800991231     C     D20PDV        OREQ      CSBVCA
021900010928     C     D20PDV        OREQ      §GEDCN
022000991231     C     CSBVCA        ANDEQ     *BLANKS
022100931013     C*  Test. x Ragione sociale
022200941102     C     D20PRA        IFNE      *BLANK
022300931013     C                   SETOFF                                       40
022400931013     C*  Loop di ricerca sottostringa nel campo del subfile
022500931013     C                   DO        X             T                 2 0
022600941102     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
022700941102     C     RESULT        COMP      D20PRA                                 40
022800931013     C  N40              END
022900931013     C*
023000931013     C                   ELSE
023100931013     C                   SETON                                        40
023200931013     C                   END
023300931013     C*
023400931013     C     *IN40         IFEQ      '1'
023500931013     C                   MOVEL     'S'           WRTREC
023600931011     C                   MOVEL     CSBRSM        SAVRSM
023700941102     C                   MOVEL     CSBVCA        SAVVCA
023800011002     C     SAVVCA        IFEQ      *BLANK
023900011002     C                   MOVE      'ITL'         SAVVCA
024000011002     C                   END
024100020102     C                   IF        SavVca = 'ITL'
024200020102     C                   EVAL      SavVca = 'EUR'
024300020102     C                   ENDIF
024400931012     C                   Z-ADD     CSBCDI        SAVCDI
024500931013     C                   MOVE      CSBPAB        SAVABI
024600931013     C                   MOVE      CSBPCA        SAVCAB
024700931011     C                   MOVE      CSBFPC        SAVFPC
024800931011     C                   MOVE      CSBTPI        SAVTPI
024900931011     C                   MOVE      CSBTPA        SAVTPA
025000931013     C                   END
025100991231     C                   END
025200931013     C                   END
025300931013     C                   END
025400931013     C                   END
025500941102     C*
025600931011     C                   Z-ADD     0             IMPASS
025700931011     C*
025800931011     C                   ENDSR
025900931011     C*----------------------------------------------------*
026000931011     C*   WRIREC: Scrittura record                         *
026100931011     C*----------------------------------------------------*
026200931011     C     WRIREC        BEGSR
026300931011     C*
026400931013     C     WRTREC        IFEQ      'S'
026500931013     C                   MOVEL     *BLANKS       VSEL
026600931011     C                   MOVEL     SAVRSM        MITT
026700941102     C                   MOVEL     SAVVCA        VALDOC
026800011002     C     VALDOC        IFEQ      *BLANK
026900011002     C                   MOVE      'ITL'         VALDOC
027000011002     C                   END
027100941102     C                   Z-ADD     SAVDDP        G02INV
027200941102     C                   Z-ADD     0             G02DAT
027300941102     C                   MOVEL     '3'           G02ERR
027400941102     C                   CALL      'XSRDA8'
027500941102     C                   PARM                    WLBDA8
027600941102     C                   Z-ADD     G02DAT        DTPAG
027700931011     C                   Z-ADD     SAVNDP        NRDOC
027800931011     C                   Z-ADD     SAVCDI        CODINT
027900931011     C                   Z-ADD     IMPASS        IMPDOC
028000931011     C                   MOVEL     *BLANK        BANCA
028100931014     C                   MOVEL     *BLANK        TPDOC
028200931011     C*-------------------------------------------------------*
028300931011     C*  Test  Tipo Documento                                 *
028400931011     C     SAVFPC        IFEQ      '2'
028500931011     C*-------------------------------------------------------*
028600931012     C                   MOVEL     COST1         TPDOC
028700931011     C     KEYABI        CHAIN     CNABI00F                           80
028800931011     C  N80              MOVEL     ABIIST        BANCA
028900931011     C*-------------------------------------------------------*
029000931011     C                   ELSE
029100931011     C*-------------------------------------------------------*
029200931014     C     SAVABI        IFEQ      99999
029300931014     C     SAVCAB        ANDEQ     99999
029400931012     C                   MOVEL     COST3         TPDOC
029500931011     C                   ELSE
029600931014     C     SAVABI        IFNE      0
029700931014     C     SAVCAB        ORNE      0
029800931011     C                   MOVE      SAVABI        ABIABI
029900931011     C                   MOVE      SAVCAB        ABICAB
030000931012     C                   MOVEL     COST2         TPDOC
030100931011     C     KEYABI        CHAIN     CNABI00F                           80
030200931011     C  N80              MOVEL     ABIIST        BANCA
030300931011     C                   END
030400931014     C                   END
030500931011     C*-------------------------------------------------------*
030600931011     C                   END
030700931011     C*-------------------------------------------------------*
030800931011     C                   ADD       1             NRR
030900931011     C                   ADD       1             NRIG
031000941102     C                   WRITE     SC21S01
031100931013     C                   END
031200931011     C*
031300931011     C                   ENDSR
031400931011     C*----------------------------------------------------*
031500931011     C*  Routine gestione operazioni prima videata         *
031600931011     C*----------------------------------------------------*
031700931011     C     GESVD1        BEGSR
031800931011     C*-                                                   *
031900931012     C                   SETON                                        30
032000941103     C                   WRITE     SC21Z02
032100941103     C                   WRITE     SC21D02
032200941102     C                   EXFMT     SC21C01
032300931011     C*                                                    *
032400931011     C* Fine Programma                                     *
032500931011     C     *INKL         IFEQ      '1'
032600931011     C                   MOVEL     'S'           £FINE
032700931011     C                   END
032800931011     C*-                                                   *
032900931011     C                   ENDSR
033000931011     C*----------------------------------------------------*
033100931011     C*  Routine gestione operazioni subfile               *
033200931011     C*----------------------------------------------------*
033300931011     C     GESVD2        BEGSR
033400931011     C*-                                                   *
033500941102     C  N21              WRITE     SC21Z01
033600941102     C                   EXFMT     SC21C01
033700931011     C*                                                    *
033800931011     C* Fine Programma                                     *
033900931011     C     *INKC         IFEQ      '1'
034000931011     C                   MOVEL     'S'           £FINE
034100941102     C                   MOVE      3             D20CMD
034200931011     C                   GOTO      FINVD2
034300931011     C                   END
034400931011     C* Ritorno                                            *
034500931011     C     *INKL         IFEQ      '1'
034600931011     C                   MOVEL     'S'           £FINE
034700941102     C                   MOVE      12            D20CMD
034800931011     C                   GOTO      FINVD2
034900931011     C                   END
035000931011     C* ROLLUP
035100931013     C     *IN98         IFEQ      '1'
035200931013     C                   EXSR      ROLLUP
035300931013     C                   END
035400931011     C* Interrogazione Bolle                               *
035500931011     C     *INKG         IFEQ      '1'
035600931011     C                   MOVEL     *BLANKS       KPJBU
035700170124     C**                 CALL      'FNLS04R'
035800170124     C***                PARM                    KPJBA
035801170124     C                   CALL      'FNLRU6R'
035802170124     C                   PARM                    KPJBA
035900931011     C                   END
036000931011     C*                                                    *
036100931011     C                   EXSR      CNTVD2
036200931011     C*                                                    *
036300931012     C     FINVD2        ENDSR
036400931011     C*----------------------------------------------------*
036500931011     C*   CNTVD2: Controlli subfile                        *
036600931011     C*----------------------------------------------------*
036700931011     C     CNTVD2        BEGSR
036800931011     C*                                                    *
036900931011     C                   Z-ADD     1             NRR
037000931014     C                   MOVEL     'N'           SELEZ             1
037100941102     C                   READC     SC21S01                                82
037200931011     C*                                                    *
037300931011     C     *IN82         DOWEQ     '0'
037400931011     C*                                                    *
037500941103     C     VSEL          IFEQ      '1'
037600941102     C                   Z-ADD     D20PDA        SAVPDA            8 0
037700931014     C                   MOVEL     'S'           SELEZ             1
037800931011     C                   MOVEL     *BLANKS       KPJBU
037900931012     C                   Z-ADD     NRR           SAVREC            4 0
038000941102     C                   Z-ADD     DTPAG         G02DAT
038100941102     C                   Z-ADD     0             G02INV
038200941102     C                   MOVEL     ' '           G02ERR
038300941102     C                   CALL      'XSRDA8'
038400941102     C                   PARM                    WLBDA8
038500941102     C                   Z-ADD     G02INV        D20PDD
038600941102     C                   Z-ADD     G02INV        D20PDA
038700941102     C                   Z-ADD     G02DAT        DTPAG
038800941102     C                   Z-ADD     NRDOC         D20PND
038900941102     C                   MOVEL     D20PRA        SAVPRA           25
039000941102     C                   MOVEL     CODINT        D20PCD
039100941102     C                   MOVEL     TNSC20        KPJBU
039200931012     C* Stampa                                             *
039300931012     C     *INKJ         IFEQ      '1'
039400941102     C                   CALL      'TNSC22R4'
039500931012     C                   PARM                    KPJBA
039600931012     C                   ELSE
039700931012     C* Interrogazione                                     *
039800941102     C                   CALL      'TNSC21R4'
039900931012     C                   PARM                    KPJBA
040000931012     C                   END
040100931014     C*
040200941102     C                   MOVEL     KPJBU         TNSC20
040300941102     C                   MOVEL     SAVPRA        D20PRA
040400941102     C     D20CMD        IFEQ      3
040500931014     C                   MOVEL     'S'           £FINE
040600931014     C                   END
040700931011     C*
040800941102     C                   Z-ADD     SAVPDA        D20PDA
040900931012     C                   Z-ADD     SAVREC        NRR
041000931014     C                   Z-ADD     SAVREC        DSPNR1
041100941102     C     NRR           CHAIN     SC21S01                            90
041200931011     C                   SETON                                        70
041300931011     C                   MOVEL     ' '           VSEL
041400941102     C  N90              UPDATE    SC21S01
041500931011     C                   END
041600931011     C*
041700941102     C                   READC     SC21S01                                82
041800931011     C                   END
041900931011     C*
042000931014     C   KJSELEZ         COMP      'S'                                2121
042100931012     C*
042200931011     C                   ENDSR
042300931011      *---------------------------------------------------------------*
042400941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
042500931011      *---------------------------------------------------------------*
042600941102     C     *INZSR        BEGSR
042700931011      *---------------------------------------------------------------*
042800931011     C     *ENTRY        PLIST
042900931011     C                   PARM                    KPJBA
043000941102     C                   MOVEL     KPJBU         TNSC20
043100931012     C*
043200931011     C                   Z-ADD     1             CODUT
043300931011     C                   CALL      'X§PARUT'
043400931011     C                   PARM                    UT§DSE
043500931011     C*          CODUT     CABEQ-1        FINE
043600931011     C                   MOVEL     RAGUT         RSUT             20
043700931011     C                   MOVEL     REC80         CNCR80
043800931011     C                   MOVEL     KNMUS         §SEI              6
043900931011     C*
044000931011     C     KEYABI        KLIST
044100931011     C                   KFLD                    SAVABI
044200931011     C                   KFLD                    SAVCAB
044300931011     C*
044400931012     C     KEYPAG        KLIST
044500941102     C                   KFLD                    D20PDD
044600931013     C*
044700960624     C     KTAB1         KLIST
044800960624     C                   KFLD                    KKUT
044900960624     C                   KFLD                    KCOD
045000960624     C                   KFLD                    KKEY
045100960624     C                   Z-ADD     1             KKUT
045200931011     C*
045300931011     C*  Definizione Variabili
045400931011     C     *LIKE         DEFINE    CSBRSM        SAVRSM
045500931011     C     *LIKE         DEFINE    CSBABI        SAVABI
045600931011     C     *LIKE         DEFINE    CSBCAB        SAVCAB
045700931011     C     *LIKE         DEFINE    CSBFPC        SAVFPC
045800931011     C     *LIKE         DEFINE    CSBTPI        SAVTPI
045900941102     C     *LIKE         DEFINE    CSBTPA        SAVTPA
046000941102     C     *LIKE         DEFINE    CSBVCA        SAVVCA
046100941102     C     *LIKE         DEFINE    CSBCAS        IMPASS          + 1
046200960624     C     *LIKE         DEFINE    TBLKUT        KKUT
046300960624     C     *LIKE         DEFINE    TBLCOD        KCOD
046400960624     C     *LIKE         DEFINE    TBLKEY        KKEY
046500931012     C*----------------------------------------------------*
046600931012     C                   MOVEL     'N'           £FINE             1
046700941102     C                   MOVE      '  '          D20PRA
046800931012     C*----------------------------------------------------*
046900941102     C     '  '          SCAN      D20PRA        LENGHT            2 0
047000931013     C                   SUB       1             LENGHT
047100941102     C     35            SUB       LENGHT        X                 2 0
047200991231     C*----------------------------------------------------*
047300991231     C*  RECUPERO DIVISA MONETA CORRENTE
047400991231     C                   CLEAR                   DGED
047500991231     C                   CLEAR                   DSBS02
047600991231     C                   MOVEL     'C'           T02MOD
047700991231     C                   MOVEL     KNSIF         T02SIF
047800991231     C                   MOVEL     'GED'         T02COD
047900991231     C                   MOVEL     '1'           T02KE1
048000991231     C                   CALL      'TIBS02R'
048100991231     C                   PARM                    KPJBA
048200991231     C                   PARM                    DSBS02
048300991231     C*
048400991231    3C     T02ERR        IFEQ      ' '
048500991231     C                   MOVEL     T02UNI        DGED
048600991231    3C                   ENDIF
048700960624     C* Carico £1
048701020506     c                   clear                   trul06ds
048702020506     c                   eval      d06cod = '£1'
048703020506     c                   movel     simfel        d06key
048704020506     c                   movel     trul06ds      kpjbu
048705020506     c                   call      'TRUL06R'
048706020506     c                   parm                    kpjba
048707020506     c                   movel     kpjbu         trul06ds
048708020506     c                   movea     lin           £1
048800020506     C*                  Z-ADD     0             £1
048900020506     C*                  MOVEL     '£1'          KCOD
049000020506     C*                  MOVEL     *BLANKS       KKEY
049100020506     C*                  MOVEL     SIMFEL        KKEY
049200020506     C*    KTAB1         CHAIN     TABEL                              31
049300020506     C*    *IN31         IFEQ      '0'
049400020506     C*    TBLFLG        ANDEQ     ' '
049500020506     C*                  MOVEA     TBLUNI        UNI
049600020506     C*                  Z-ADD     0             XY                3 0
049700020506     C*                  DO        29            XX                3 0
049800020506     C*    UNI(XX)       IFGT      *ZEROS
049900020506     C*                  ADD       1             XY
050000020506     C*                  MOVEL     UNI(XX)       £1(XY)
050100020506     C*                  END
050200020506     C*                  END
050300020506     C*                  END
050400960624     C*  SE £1 = 0  --> SIMFEL
050500020506     C*                  XFOOT     £1            W£1               4 0
050600020506     C*    W£1           IFEQ      0
050700020506     C*                  Z-ADD     SIMFEL        £1(1)
050800020506     C*                  END
050900931013     C*----------------------------------------------------*
051000931011     C                   ENDSR
