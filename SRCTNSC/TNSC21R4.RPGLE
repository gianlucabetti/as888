000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300941103      * TNSC21R4                                                     *
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
002100941103     FTABEL00F  IF   E           K DISK
002200941103     FFNCSB02L  IF   E           K DISK
002300941103     FTNSC21D   CF   E             WORKSTN
002400941103     F                                     SFILE(SC21S01:NRR)
002500931011?     *--------------------------------------------------------------*
002600941103      *  Schiere per caricamento dati valute
002700941103     D CCV             S              3    DIM(100)
002800991231     D DVA             S              1    DIM(100)
002900011001     D*****               CMB       100  9 3             CAMBIO DIVISA ITA.
003000020506     D*UNI             S              3    DIM(29)
003100020506     D £1              S              3  0 DIM(30)
003200941103?     *--------------------------------------------------------------*
003300931011?     *  DS                                                          *
003400931011?     *--------------------------------------------------------------*
003500991231     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003600991231     D DGED          E DS
003700931011     D KPJBA         E DS
003800941103     D DSCV          E DS
003801020506     d trul06ds      e ds
003802020506     d  lin                    1     90  0 dim(30)
003900941103      *
004000010614     D YEURDS        E DS                  EXTNAME(YEURCODS)
004100941103     D WLBDA8          DS
004200941103     D  G02DAT                 1      8  0
004300941103     D  G02INV                 9     16  0
004400941103     D  G02ERR                17     17
004500941103     D  G02TGI                18     22  0
004600931011      *
004700941103     D TNSC20        E DS                  EXTNAME(TNSC20DS)
004800931011      *
004900931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005000931011     D CNCR80        E DS
005100960403      *
005200960403      *  Ds x numero spedizione contrass.
005300960403     D                 DS
005400960403     D  CSBLNP                 1      3  0
005500960403     D  CSBAAS                 4      7  0
005600960403     D  CSBNRS                 8      9  0
005700960403     D  CSBNSP                10     16  0
005800960403     D  NUMSPE                 1     16  0
005900960403      *
006000960403      *  Ds x numero spedizione pagamento
006100960403     D                 DS
006200960403     D  CSBPLP                 1      3  0
006300960403     D  CSBPAA                 4      7  0
006400960403     D  CSBPNS                 8      9  0
006500960403     D  CSBPSP                10     16  0
006600960403     D  SPEPAG                 1     16  0
006700931011      *
006800931011     D                SDS
006900931011     D  NOMPG                  1     10
007000931011      *---------------------------------------------------------------*
007100931011      *   C I C L O      P R I N C I P A L E                          *
007200931011      *---------------------------------------------------------------*
007300931011     C*  CARICAMENTO SUBFILE
007400931011     C                   EXSR      CARSFL
007500931011     C*
007600931011     C*  LOOP
007700931011     C     £FINE         DOUEQ     'S'
007800931011     C*
007900931011     C     TIPVID        CASEQ     '1'           GESVD1
008000931011     C     TIPVID        CASEQ     '2'           GESVD2
008100931011     C                   END
008200931011     C*
008300931011     C                   END
008400931011     C*
008500941103     C                   MOVEL     TNSC20        KPJBU
008600931011     C                   SETON                                        LR
008700931011      *---------------------------------------------------------------*
008800931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
008900931011      *---------------------------------------------------------------*
009000931011     C     CARSFL        BEGSR
009100931011     C*
009200931011     C*  Pulisco subfile
009300931011     C                   SETON                                        3031
009400941103     C                   WRITE     SC21C01
009500931012     C                   SETOFF                                       31
009600931011     C*
009700931011     C*  Caricamento subfile
009800941103     C     KEYPAG        SETLL     FNCSB02L
009900941103     C                   READ      FNCSB02L                               81
010000931011     C*
010100931011     C                   Z-ADD     0             NRR               4 0
010200931012     C                   MOVEL     '1'           TIPVID            1
010300931011     C*
010400931011     C     *IN81         IFEQ      '0'
010500941103     C     CSBDDP        ANDLE     D20PDA
010600931011     C                   EXSR      ROLLUP
010700931011     C                   END
010800931012     C*
010900931012     C     NRR           IFNE      0
011000931012     C                   MOVEL     '2'           TIPVID            1
011100931012     C                   SETOFF                                       30
011200931012     C                   END
011300931011     C*
011400931012     C                   MOVEL     NOMPG         NOMPGM
011500931011     C*
011600931011     C                   ENDSR
011700931011     C*----------------------------------------------------*
011800931011     C*   ROLLUP: Caricamento di una pagina                *
011900931011     C*----------------------------------------------------*
012000931011     C     ROLLUP        BEGSR
012100931011     C*
012200931011     C     *IN81         IFEQ      '0'
012300931011     C                   Z-ADD     0             NRIG              4 0
012400931014     C                   Z-ADD     SAVRE1        NRR
012500931011     C*
012600931013     C     NRIG          DOWLT     14
012700941103     C     CSBDDP        ANDLE     D20PDA
012800931011     C     *IN81         ANDEQ     '0'
012900931013     C*
013000931013     C* SOLO CONTRASSEGNI INCASSATI NON PAGATI
013100941103     C     D20TCR        IFEQ      'I'
013200931013     C     CSBDDC        COMP      0                                      20
013300931013     C  N20CSBDDP        COMP      0                                  2020
013400931013     C                   END
013500931013     C* SOLO CONTRASSEGNI PAGATI
013600941103     C     D20TCR        IFEQ      'P'
013700931013     C     CSBNDP        COMP      0                                      20
013800931013     C                   END
013900931013     C*  Test Tipo Contrassegni
014000931013     C     *IN20         IFEQ      '0'
014100931012     C*  Test  x Numero documento
014200941103     C     D20PND        IFEQ      0
014300941103     C     D20PND        OREQ      CSBNDP
014400931012     C*  Test  x tipo intestazione
014500941103     C     D20PTN        IFEQ      *BLANKS
014600941103     C     D20PTN        OREQ      'M'
014700931202     C     CSBTPI        ANDEQ     'M'
014800941103     C     D20PTN        OREQ      'B'
014900931202     C     CSBTPI        ANDEQ     ' '
015000931012     C*  Test  x codice
015100941103     C     D20PCD        IFEQ      0
015200941103     C     D20PCD        OREQ      CSBCDI
015300991231     C* Scrivo record se divisa corrisponde
015400991231     C     D20PDV        IFEQ      *BLANKS
015500991231     C     D20PDV        OREQ      CSBVCA
015600011001     C     D20PDV        OREQ      §GEDCN
015700991231     C     CSBVCA        ANDEQ     *BLANKS
015800931012     C*  Test. x Ragione sociale
015900941103     C     D20PRA        IFNE      *BLANK
016000931012     C                   SETOFF                                       40
016100931012     C*  Loop di ricerca sottostringa nel campo del subfile
016200931012     C                   DO        X             T                 2 0
016300941103     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
016400941103     C     RESULT        COMP      D20PRA                                 40
016500931012     C  N40              END
016600931012     C*
016700931012     C                   ELSE
016800931012     C                   SETON                                        40
016900931012     C                   END
017000931012     C*
017100931012     C     *IN40         IFEQ      '1'
017200931012     C                   MOVEL     *BLANKS       VSEL
017300931012     C                   MOVEL     CSBAAS        ANNO
017400941103     C                   MOVEL     CSBMGS        MM                2 0
017500941103     C                   MOVE      CSBMGS        GG                2 0
017600931012     C                   MOVEL     GG            VDAT
017700941103     C                   MOVE      MM            VDAT
017800931012     C                   MOVE      CSBLNP        VLNP1
017900931012     C                   MOVE      CSBNRS        VSER1
018000931012     C                   MOVE      CSBNSP        VSPE1
018100931012     C                   MOVEL     CSBRSM        VMIT
018200931012     C                   MOVEL     CSBRSD        VDES
018300931012     C                   MOVE      CSBRMN        VRIF
018400941103     C                   Z-ADD     CSBCAS        VIMP
018500011002     C                   MOVEL     CSBVCA        VVCA
018600011002     C     VVCA          IFEQ      *BLANK
018700011002     C                   MOVE      'ITL'         VVCA
018800011002     C                   END
018900941103     C* Controllo la divisa
019000941103     C     VVCA          IFNE      *BLANKS
019100941103     C                   Z-ADD     1             XX
019200941103     C     VVCA          LOOKUP    CCV(XX)                                32
019300941103     C                   END
019400960624     C     CSBPLP        LOOKUP    £1                                     35
019500941103     C* Somma nel totale corretto
019600941103     C     VVCA          IFEQ      *BLANKS
019700011001     C     VVCA          OREQ      §GEDCN
019800960403     C     NUMSPE        IFEQ      SPEPAG
019900960403     C     SPEPAG        OREQ      0
020000960624     C     *IN35         OREQ      '0'
020100941103     C                   ADD       CSBCAS        TOT
020200960403     C                   END
020300991231     C                   ELSE
020400991231     C     DVA(XX)       IFEQ      'S'
020500991231     C     *IN32         ANDEQ     '1'
020600991231     C     NUMSPE        IFEQ      SPEPAG
020700991231     C     SPEPAG        OREQ      0
020800991231     C     *IN35         OREQ      '0'
020900991231     C                   ADD       CSBCAS        TOTEUR
021000991231     C                   END
021100991231     C  N08              SETON                                        08
021200941103     C                   ELSE
021300010614     C***                  Z-ADDCMB,XX    CAMBIO  94
021400010614     C***        CSBCMB    IFNE 0
021500010614     C***                  Z-ADDCSBCMB    CAMBIO  94
021600010614     C***                  END
021700010614     C***        CSBCMP    IFNE 0
021800010614     C***                  Z-ADDCSBCMP    CAMBIO
021900010614     C***                  END
022000010614     C***                  SELEC
022100010614     C***        NRDEC     WHEQ 2
022200010614     C***        CAMBIO    MULT CSBCAS    IMPV2  122H
022300010614     C***                  Z-ADDIMPV2     IMPV
022400010614     C***        NRDEC     WHEQ 1
022500010614     C***        CAMBIO    MULT CSBCAS    IMPV1  111H
022600010614     C***                  Z-ADDIMPV1     IMPV
022700010614     C***        NRDEC     WHEQ 0
022800010614     C***        CAMBIO    MULT CSBCAS    IMPV0  100H
022900010614     C***                  Z-ADDIMPV0     IMPV
023000010614     C***                  OTHER
023100010614     C***        CAMBIO    MULT CSBCAS    IMPV   133
023200010614     C***                  ENDSL
023300010614     C*
023400010614     C* Per convertire gli importi in valute estere utilizzo
023500010614     C* la routine YEURCO in quanto il cambio indicato sul
023600010614     C* file dei C/assegni di filiale non è affidabile
023700010614     C                   CLEAR                   YEURDS
023800010614     C                   MOVEL     VVCA          YECDVI
023900010614     C                   Z-ADD     CSBCAS        YECIMI
024000011001     C                   MOVEL     §GEDCN        YECDVO
024100010614     C* passo al pgm io il nr. dei decimali della divisa corrente
024200010614     C* in base a quanto memorizzato sulla tabella CV
024300010614     C                   MOVE      NRDEC         YECDCO
024400010614     C                   MOVEL     'H'           YECTAR
024500010614     C                   CALL      'YEURCO'
024600010614     C                   PARM                    YEURDS
024700010614     C                   Z-ADD     YECIMO        IMPV             13 3
024800000103     C*
024900960403     C     NUMSPE        IFEQ      SPEPAG
025000960403     C     SPEPAG        OREQ      0
025100960624     C     *IN35         OREQ      '0'
025200941103     C                   ADD       IMPV          TOTV
025300960403     C                   END
025400941103     C  N07              SETON                                        07
025500941103     C                   END
025600991231     C                   END
025700931012     C                   ADD       1             NRR
025800931012     C                   ADD       1             NRIG
025900941103     C                   WRITE     SC21S01
026000931012     C                   END
026100991231     C                   END
026200931012     C                   END
026300931012     C                   END
026400931012     C                   END
026500931013     C                   END
026600931012     C*
026700941103     C                   READ      FNCSB02L                               81
026800931011     C                   END
026900931011     C*
027000931011     C*
027100941103     C  N81CSBDDP        COMP      D20PDA                             81
027200931014     C     NRR           IFGT      SAVRE1
027300931014     C     SAVRE1        ADD       1             DSPNR1
027400931014     C                   Z-ADD     NRR           SAVRE1            4 0
027500931011     C                   END
027600931011     C*
027700931014     C                   END
027800931014     C*
027900931011     C                   ENDSR
028000931011     C*----------------------------------------------------*
028100931011     C*  Routine gestione operazioni prima videata         *
028200931011     C*----------------------------------------------------*
028300931011     C     GESVD1        BEGSR
028400931011     C*-                                                   *
028500931012     C                   SETON                                        30
028600941103     C                   WRITE     SC21Z02
028700941103     C                   WRITE     SC21D02
028800941103     C                   EXFMT     SC21C01
028900931011     C*                                                    *
029000931011     C* Fine Programma                                     *
029100931011     C     *INKL         IFEQ      '1'
029200931011     C                   MOVEL     'S'           £FINE
029300931011     C                   END
029400931011     C*-                                                   *
029500931011     C                   ENDSR
029600931011     C*----------------------------------------------------*
029700931011     C*  Routine gestione operazioni subfile               *
029800931011     C*----------------------------------------------------*
029900931011     C     GESVD2        BEGSR
030000931011     C*-                                                   *
030100941103     C                   WRITE     SC21Z01
030200941103     C                   WRITE     SC21D01
030300941103     C                   EXFMT     SC21C01
030400931011     C*                                                    *
030500931011     C* Fine Programma                                     *
030600931011     C     *INKC         IFEQ      '1'
030700931011     C                   MOVEL     'S'           £FINE
030800941103     C                   MOVE      3             D20CMD
030900931011     C                   GOTO      FINVD2
031000931011     C                   END
031100931011     C* Ritorno                                            *
031200931011     C     *INKL         IFEQ      '1'
031300931011     C                   MOVEL     'S'           £FINE
031400941103     C                   MOVE      12            D20CMD
031500931011     C                   GOTO      FINVD2
031600931011     C                   END
031700931011     C* ROLLUP
031800931011     C   98              EXSR      ROLLUP
031900931011     C* Interrogazione Bolle                               *
032000931011     C     *INKG         IFEQ      '1'
032100931011     C                   MOVEL     *BLANKS       KPJBU
032200170125     C*****              CALL      'FNLS04R'
032201170125     C                   CALL      'FNLRU6R'
032300931011     C                   PARM                    KPJBA
032400931011     C                   END
032500931011     C*                                                    *
032600931011     C                   EXSR      CNTVD2
032700931011     C*                                                    *
032800931012     C     FINVD2        ENDSR
032900931011     C*----------------------------------------------------*
033000931011     C*   CNTVD2: Controlli subfile                        *
033100931011     C*----------------------------------------------------*
033200931011     C     CNTVD2        BEGSR
033300931011     C*                                                    *
033400931011     C                   Z-ADD     1             NRR
033500941103     C                   MOVEL     ' '           VSEL
033600941103     C                   READC     SC21S01                                82
033700931011     C*                                                    *
033800931011     C     *IN82         DOWEQ     '0'
033900931011     C*                                                    *
034000941103     C     VSEL          IFEQ      '1'
034100931011     C                   MOVEL     *BLANKS       KPJBU
034200931012     C                   Z-ADD     NRR           SAVREC            4 0
034300941103     C                   MOVE      ANNO          D20ANN
034400941103     C                   MOVE      VLNP1         D20LNP
034500941103     C                   MOVE      VSER1         D20NSR
034600941103     C                   MOVE      VSPE1         D20NSP
034700941103     C                   MOVEL     TNSC20        KPJBU
034800931012     C* Interrogazione                                     *
034900941103     C                   CALL      'TNSC23R'
035000931012     C                   PARM                    KPJBA
035100931011     C*
035200931012     C                   Z-ADD     SAVREC        NRR
035300931014     C                   Z-ADD     SAVREC        DSPNR1
035400941103     C     NRR           CHAIN     SC21S01                            90
035500931011     C                   SETON                                        70
035600931011     C                   MOVEL     ' '           VSEL
035700941103     C  N90              UPDATE    SC21S01
035800931011     C                   END
035900931011     C*
036000941103     C                   READC     SC21S01                                82
036100931011     C                   END
036200931012     C*
036300931011     C                   ENDSR
036400931011      *---------------------------------------------------------------*
036500941103      *  *INZSR: OPERAZIONI INIZIALI                                  *
036600931011      *---------------------------------------------------------------*
036700941103     C     *INZSR        BEGSR
036800931011      *---------------------------------------------------------------*
036900931011     C     *ENTRY        PLIST
037000931011     C                   PARM                    KPJBA
037100941103     C                   MOVEL     KPJBU         TNSC20
037200931012     C*
037300931011     C                   Z-ADD     1             CODUT
037400931011     C                   CALL      'X§PARUT'
037500931011     C                   PARM                    UT§DSE
037600931011     C*          CODUT     CABEQ-1        FINE
037700931011     C                   MOVEL     RAGUT         RSUT             20
037800931011     C                   MOVEL     REC80         CNCR80
037900931011     C                   MOVEL     KNMUS         §SEI              6
038000931011     C*
038100931012     C     KEYPAG        KLIST
038200941103     C                   KFLD                    D20PDD
038300941103     C                   KFLD                    D20PND
038400941103     C*
038500941103     C     KTAB          KLIST
038600941103     C                   KFLD                    KKUT
038700941103     C                   KFLD                    KCOD
038800960624     C     KTAB1         KLIST
038900960624     C                   KFLD                    KKUT
039000960624     C                   KFLD                    KCOD
039100960624     C                   KFLD                    KKEY
039200960624     C                   Z-ADD     1             KKUT
039300941103     C                   Z-ADD     1             KKUT
039400941103     C*----------------------------------------------------*
039500941103     C*  Definizione variabili
039600941103     C     *LIKE         DEFINE    TBLKUT        KKUT
039700941103     C     *LIKE         DEFINE    TBLCOD        KCOD
039800960624     C     *LIKE         DEFINE    TBLKEY        KKEY
039900931012     C*----------------------------------------------------*
040000941103     C                   SETON                                        04
040100931012     C                   MOVEL     'N'           £FINE             1
040200941103     C                   CLEAR                   WLBDA8
040300941103     C                   MOVE      D20PDD        G02INV
040400941103     C                   Z-ADD     0             G02DAT
040500941103     C                   MOVEL     '3'           G02ERR
040600941103     C                   CALL      'XSRDA8'
040700941103     C                   PARM                    WLBDA8
040800941103     C                   MOVE      G02DAT        PDD
040900941103     C                   MOVE      D20PDA        G02INV
041000941103     C                   Z-ADD     0             G02DAT
041100941103     C                   MOVEL     '3'           G02ERR
041200941103     C                   CALL      'XSRDA8'
041300941103     C                   PARM                    WLBDA8
041400941103     C                   MOVE      G02DAT        PDA
041500941103     C                   MOVE      '  '          D20PRA
041600941103     C     '  '          SCAN      D20PRA        LENGHT            2 0
041700941103     C                   SUB       1             LENGHT
041800941103     C     35            SUB       LENGHT        X                 2 0
041900941103     C                   MOVE      D20PND        NDC
042000991231     C*----------------------------------------------------*
042100991231     C*  RECUPERO DIVISA MONETA CORRENTE
042200991231     C                   CLEAR                   DGED
042300991231     C                   CLEAR                   DSBS02
042400991231     C                   MOVEL     'C'           T02MOD
042500991231     C                   MOVEL     KNSIF         T02SIF
042600991231     C                   MOVEL     'GED'         T02COD
042700991231     C                   MOVEL     '1'           T02KE1
042800991231     C                   CALL      'TIBS02R'
042900991231     C                   PARM                    KPJBA
043000991231     C                   PARM                    DSBS02
043100991231     C*
043200991231    3C     T02ERR        IFEQ      ' '
043300991231     C                   MOVEL     T02UNI        DGED
043400011001     C                   MOVEL     §GEDCN        DIVCOR
043500991231    3C                   ENDIF
043600991231     C*----------------------------------------------------*
043700991231     C* Carico tabella cambi/divise
043800991231     C                   Z-ADD     0             XX                3 0
043900991231     C                   MOVEL     'CV'          KCOD
044000991231     C     KTAB          CHAIN     TABEL                              31
044100991231     C     *IN31         DOWEQ     '0'
044200991231     C     XX            ANDLT     100
044300991231     C                   MOVEL     TBLKEY        WDIV              3
044400991231     C     TBLFLG        IFEQ      ' '
044500991231     C                   ADD       1             XX
044600991231     C                   MOVEL     TBLKEY        CCV(XX)
044700991231     C                   MOVEL     TBLUNI        DSCV
044800991231     C                   MOVEL     §CVDVA        DVA(XX)
044900011001     C*****                MOVEL§CVCMB    CMB,XX
045000991231     C     §CVDVA        IFEQ      'S'
045100011001     C     WDIV          ANDNE     §GEDCN
045200991231     C                   MOVEL     WDIV          DIVALT
045300991231     C                   END
045400000103     C* Memorizzo nr. decimali della divisa corrente
045500011001     C     WDIV          IFEQ      §GEDCN
045600000103     C                   Z-ADD     §CVDEC        NRDEC             2 0
045700000103     C                   END
045800991231     C                   END
045900991231     C     KTAB          READE     TABEL                                  31
046000991231     C                   END
046100960624     C* Carico £1
046101020506     c                   clear                   trul06ds
046102020506     c                   eval      d06cod = '£1'
046103020506     c                   movel     simfel        d06key
046104020506     c                   movel     trul06ds      kpjbu
046105020506     c                   call      'TRUL06R'
046106020506     c                   parm                    kpjba
046107020506     c                   movel     kpjbu         trul06ds
046108020506     c                   movea     lin           £1
046200020506     C*                  Z-ADD     0             £1
046300020506     C*                  MOVEL     '£1'          KCOD
046400020506     C*                  MOVEL     *BLANKS       KKEY
046500020506     C*                  MOVEL     SIMFEL        KKEY
046600020506     C*    KTAB1         CHAIN     TABEL                              31
046700020506     C*    *IN31         IFEQ      '0'
046800020506     C*    TBLFLG        ANDEQ     ' '
046900020506     C*                  MOVEA     TBLUNI        UNI
047000020506     C*                  Z-ADD     0             XY                3 0
047100020506     C*                  DO        29            XX
047200020506     C*    UNI(XX)       IFGT      *ZEROS
047300020506     C*                  ADD       1             XY
047400020506     C*                  MOVEL     UNI(XX)       £1(XY)
047500020506     C*                  END
047600020506     C*                  END
047700020506     C*                  END
047800960624     C*  SE £1 = 0  --> SIMFEL
047900020506     C*                  XFOOT     £1            W£1               4 0
048000020506     C*    W£1           IFEQ      0
048100020506     C*                  Z-ADD     SIMFEL        £1(1)
048200020506     C*                  END
048300931012     C*----------------------------------------------------*
048400931011     C                   ENDSR
