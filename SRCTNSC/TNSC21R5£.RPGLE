000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300960917      * TNSC21R5                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600960917      *          SELEZIONE STATO C/ASSEGNO-CLIENTE                   *
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
002200020910     F                                     usropn
002300960917     FFNCSB05L  IF   E           K DISK
002400941102     FTNSC21D   CF   E             WORKSTN
002500941102     F                                     SFILE(SC21S01:NRR)
002600941102?     *--------------------------------------------------------------*
002700941102      *  Schiere per caricamento dati valute
002800941102     D CCV             S              3    DIM(100)
002900991231     D DVA             S              1    DIM(100)
003000931011?     *--------------------------------------------------------------*
003100931011?     *  DS                                                          *
003200931011?     *--------------------------------------------------------------*
003300931011     D KPJBA         E DS
003400941103      *
003500991231     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003600010614     D YEURDS        E DS                  EXTNAME(YEURCODS)
003700991231     D DGED          E DS
003800941103     D DSCV          E DS
003900941102      *
004000941102     D WLBDA8          DS
004100941102     D  G02DAT                 1      8  0
004200941102     D  G02INV                 9     16  0
004300941102     D  G02ERR                17     17
004400941102     D  G02TGI                18     22  0
004500941102     D TNSC20        E DS                  EXTNAME(TNSC20DS)
004600931011      *
004700931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800931011     D CNCR80        E DS
004900020910     D TRUL31DS      E DS
005000020910     D                                     inz
005100020910     D  POG                           3  0
005200020910     D                                     dim(250)
005300020910     D                                     overlay(O31POG)
005400931011      *
005500931011     D                SDS
005600931011     D  NOMPG                  1     10
005700020910     D xx              S              5I 0
005800020910     D pogultimo       S              5I 0
005900020910     D                                     inz(%elem(pog))
006000020910     D wrkpoc          S              3  0
006100020910     D RecCsbValido    S              1
006200931011      *---------------------------------------------------------------*
006300931011      *   C I C L O      P R I N C I P A L E                          *
006400931011      *---------------------------------------------------------------*
006500931011     C*  CARICAMENTO SUBFILE
006600931011     C                   EXSR      CARSFL
006700931011     C*
006800931011     C*  LOOP
006900931011     C     £FINE         DOUEQ     'S'
007000931011     C*
007100931011     C     TIPVID        CASEQ     '1'           GESVD1
007200931011     C     TIPVID        CASEQ     '2'           GESVD2
007300931011     C                   END
007400931011     C*
007500931011     C                   END
007600931011     C*
007700941102     C                   MOVEL     TNSC20        KPJBU
007800931011     C                   SETON                                        LR
007900931011      *---------------------------------------------------------------*
008000931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
008100931011      *---------------------------------------------------------------*
008200931011     C     CARSFL        BEGSR
008300931011     C*
008400931011     C*  Pulisco subfile
008500931011     C                   SETON                                        3031
008600941102     C                   WRITE     SC21C01
008700931012     C                   SETOFF                                       31
008800931011     C*
008900931011     C*  Caricamento subfile
009000960917     C                   Z-ADD     D20STA        KSTA
009100960917     C                   Z-ADD     D20TCD        KCDI
009200960917     C     D20TCD        IFEQ      0
009300960917     C     KSTA          CHAIN     FNCSB05L                           81
009400960917     C                   ELSE
009500960917     C     KCSB          CHAIN     FNCSB05L                           81
009600960917     C                   END
009700931011     C*
009800931011     C                   Z-ADD     0             NRR               4 0
009900931012     C                   MOVEL     '1'           TIPVID            1
010000931011     C*
010100931011     C     *IN81         IFEQ      '0'
010200931011     C                   EXSR      ROLLUP
010300931011     C                   END
010400931011     C*
010500931012     C     NRR           IFNE      0
010600931012     C                   SETOFF                                       30
010700931012     C                   MOVEL     '2'           TIPVID
010800931012     C                   END
010900931012     C*
011000931012     C                   MOVEL     NOMPG         NOMPGM
011100931011     C*
011200931011     C                   ENDSR
011300931011     C*----------------------------------------------------*
011400931011     C*   ROLLUP: Caricamento di una pagina                *
011500931011     C*----------------------------------------------------*
011600931011     C     ROLLUP        BEGSR
011700931011     C*
011800931011     C     *IN81         IFEQ      '0'
011900931014     C                   Z-ADD     SAVRE1        NRR
012000931011     C                   Z-ADD     0             NRIG              4 0
012100931011     C*
012200931013     C     NRIG          DOWLT     14
012300931011     C     *IN81         ANDEQ     '0'
012400960917     C*
012500020910     C                   eval      RecCsbValido = *on
012600931012     C*  Test. x Ragione sociale
012700960917     C     D20TRA        IFNE      *BLANK
012800020910     C                   SETOFF                                       40
012900931012     C*  Loop di ricerca sottostringa nel campo del subfile
013000931012     C                   DO        X             T                 2 0
013100941102     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
013200960917     C     RESULT        COMP      D20TRA                                 40
013300931012     C  N40              END
013400020910     C                   if        not *in40
013500020910     C                   eval      RecCsbValido = *off
013600020910     C                   endif
013700931012     C                   END
013800020910
013900020910     ** Se non è stata chiesta la selezione di un cliente
014000020910     ** seleziono solo i PO dello stesso terminal dell'utente.
014100020910     C                   if        d20tcd = 0 and d20tra = *blank
014200020910     C                   eval      *in40 = *off
014300020910     C                   movel     csbcdi        wrkpoc
014400020910     C                   eval      xx = 1
014500020910     C                   dow       xx <= pogultimo
014600020910     C                   if        csblnp = pog(xx) or wrkpoc = pog(xx)
014700020910     C                   eval      *in40 = *on
014800020910     C                   leave
014900020910     C                   endif
015000020910     C                   eval      xx = xx + 1
015100020910     C                   enddo
015200020910     C                   if        not *in40
015300020910     C                   eval      RecCsbValido = *off
015400020910     C                   endif
015500020910     C                   endif
015600020910
015700020910     ** Record di FNCSB00F valido, lo stampo.
015800020910     C                   IF        RecCsbValido = *on
015900931012     C                   MOVEL     *BLANKS       VSEL
016000931012     C                   MOVEL     CSBAAS        ANNO
016100941102     C                   MOVEL     CSBMGS        MM                2 0
016200941102     C                   MOVE      CSBMGS        GG                2 0
016300931012     C                   MOVEL     GG            VDAT
016400941102     C                   MOVE      MM            VDAT
016500931012     C                   MOVE      CSBLNP        VLNP1
016600931012     C                   MOVE      CSBNRS        VSER1
016700931012     C                   MOVE      CSBNSP        VSPE1
016800931012     C                   MOVEL     CSBRSM        VMIT
016900931012     C                   MOVEL     CSBRSD        VDES
017000931012     C                   MOVE      CSBRMN        VRIF
017100941102     C                   MOVE      CSBVCA        VVCA
017200011002     C     VVCA          IFEQ      *BLANK
017300011002     C                   MOVE      'ITL'         VVCA
017400011002     C                   END
017500941102     C                   Z-ADD     CSBCAS        VIMP
017600941102     C* Controllo la divisa
017700941102     C     VVCA          IFNE      *BLANKS
017800941102     C                   Z-ADD     1             XX
017900941102     C     VVCA          LOOKUP    CCV(XX)                                32
018000941102     C                   END
018100941102     C* Somma nel totale corretto
018200941102     C     VVCA          IFEQ      *BLANKS
018300011001     C     VVCA          OREQ      §GEDCN
018400941102     C                   ADD       CSBCAS        TOT
018500991231     C                   ELSE
018600991231     C     DVA(XX)       IFEQ      'S'
018700991231     C     *IN32         ANDEQ     '1'
018800991231     C                   ADD       CSBCAS        TOTEUR
018900991231     C  N08              SETON                                        08
019000941102     C                   ELSE
019100010614     C* Per convertire gli importi in valute estere utilizzo
019200010614     C* la routine YEURCO in quanto il cambio indicato sul
019300010614     C* file dei C/assegni di filiale non è affidabile
019400010614     C                   CLEAR                   YEURDS
019500010614     C                   MOVEL     VVCA          YECDVI
019600010614     C                   Z-ADD     CSBCAS        YECIMI
019700011001     C                   MOVEL     §GEDCN        YECDVO
019800010614     C* passo al pgm io il nr. dei decimali della divisa corrente
019900010614     C* in base a quanto memorizzato sulla tabella CV
020000010614     C                   MOVE      NRDEC         YECDCO
020100010614     C                   MOVEL     'H'           YECTAR
020200010614     C                   CALL      'YEURCO'
020300010614     C                   PARM                    YEURDS
020400010614     C                   Z-ADD     YECIMO        IMPV             13 3
020500000103     C*
020600941102     C                   ADD       IMPV          TOTV
020700941102     C  N07              SETON                                        07
020800941102     C                   END
020900991231     C                   END
021000931012     C                   ADD       1             NRR
021100931012     C                   ADD       1             NRIG
021200941103     C                   WRITE     SC21S01
021300931012     C                   END
021400931012     C*
021500960917     C     D20TCD        IFEQ      0
021600960917     C     KSTA          READE     FNCSB05L                               81
021700960917     C                   ELSE
021800960917     C     KCSB          READE     FNCSB05L                               81
021900960917     C                   END
022000931011     C                   END
022100931011     C*
022200931014     C     NRR           IFGT      SAVRE1
022300931014     C     SAVRE1        ADD       1             DSPNR1
022400931014     C                   Z-ADD     NRR           SAVRE1            4 0
022500931014     C                   END
022600931014     C*
022700931011     C                   END
022800931011     C*
022900931011     C                   ENDSR
023000931011     C*----------------------------------------------------*
023100931011     C*  Routine gestione operazioni prima videata         *
023200931011     C*----------------------------------------------------*
023300931011     C     GESVD1        BEGSR
023400931011     C*-                                                   *
023500931012     C                   SETON                                        30
023600941102     C                   WRITE     SC21Z02
023700941102     C                   WRITE     SC21D02
023800941103     C                   EXFMT     SC21C01
023900931011     C*                                                    *
024000931011     C* Fine Programma                                     *
024100931011     C     *INKL         IFEQ      '1'
024200931011     C                   MOVEL     'S'           £FINE
024300931011     C                   END
024400931011     C*-                                                   *
024500931011     C                   ENDSR
024600931011     C*----------------------------------------------------*
024700931011     C*  Routine gestione operazioni subfile               *
024800931011     C*----------------------------------------------------*
024900931011     C     GESVD2        BEGSR
025000931011     C*-                                                   *
025100941103     C                   WRITE     SC21Z01
025200941103     C                   WRITE     SC21D01
025300941103     C                   EXFMT     SC21C01
025400931011     C*                                                    *
025500931011     C* Fine Programma                                     *
025600931011     C     *INKC         IFEQ      '1'
025700931011     C                   MOVEL     'S'           £FINE
025800941102     C                   MOVE      3             D20CMD
025900931011     C                   GOTO      FINVD2
026000931011     C                   END
026100931011     C* Ritorno                                            *
026200931011     C     *INKL         IFEQ      '1'
026300931011     C                   MOVEL     'S'           £FINE
026400941102     C                   MOVE      12            D20CMD
026500931011     C                   GOTO      FINVD2
026600931011     C                   END
026700931011     C* ROLLUP
026800931011     C   98              EXSR      ROLLUP
026900931011     C* Interrogazione Bolle                               *
027000931011     C     *INKG         IFEQ      '1'
027100931011     C                   MOVEL     *BLANKS       KPJBU
027200941102     C                   CALL      'FNLS04R'
027300931011     C                   PARM                    KPJBA
027400931011     C                   END
027500931011     C*                                                    *
027600931011     C                   EXSR      CNTVD2
027700931011     C*                                                    *
027800931012     C     FINVD2        ENDSR
027900931011     C*----------------------------------------------------*
028000931011     C*   CNTVD2: Controlli subfile                        *
028100931011     C*----------------------------------------------------*
028200931011     C     CNTVD2        BEGSR
028300931011     C*                                                    *
028400931011     C                   Z-ADD     1             NRR
028500941103     C                   MOVEL     ' '           VSEL
028600941102     C                   READC     SC21S01                                82
028700931011     C*                                                    *
028800931011     C     *IN82         DOWEQ     '0'
028900931011     C*                                                    *
029000941103     C     VSEL          IFEQ      '1'
029100931011     C                   MOVEL     *BLANKS       KPJBU
029200931012     C                   Z-ADD     NRR           SAVREC            4 0
029300941102     C                   MOVE      ANNO          D20ANN
029400941102     C                   MOVE      VLNP1         D20LNP
029500941102     C                   MOVE      VSER1         D20NSR
029600941102     C                   MOVE      VSPE1         D20NSP
029700941102     C                   MOVEL     TNSC20        KPJBU
029800931012     C* Interrogazione                                     *
029900941103     C                   CALL      'TNSC23R'
030000931012     C                   PARM                    KPJBA
030100931011     C*
030200931012     C                   Z-ADD     SAVREC        NRR
030300931014     C                   Z-ADD     SAVREC        DSPNR1
030400941102     C     NRR           CHAIN     SC21S01                            90
030500931011     C                   SETON                                        70
030600931011     C                   MOVEL     ' '           VSEL
030700941102     C  N90              UPDATE    SC21S01
030800931011     C                   END
030900931011     C*
031000941102     C                   READC     SC21S01                                82
031100931011     C                   END
031200931012     C*
031300931011     C                   ENDSR
031400931011      *---------------------------------------------------------------*
031500941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
031600931011      *---------------------------------------------------------------*
031700941102     C     *INZSR        BEGSR
031800931011      *---------------------------------------------------------------*
031900931011     C     *ENTRY        PLIST
032000931011     C                   PARM                    KPJBA
032100941102     C                   MOVEL     KPJBU         TNSC20
032200931012     C*
032300931011     C                   Z-ADD     1             CODUT
032400931011     C                   CALL      'X§PARUT'
032500931011     C                   PARM                    UT§DSE
032600931011     C*          CODUT     CABEQ-1        FINE
032700931011     C                   MOVEL     RAGUT         RSUT             20
032800931011     C                   MOVEL     REC80         CNCR80
032900931011     C                   MOVEL     KNMUS         §SEI              6
033000931011     C*
033100960917     C     KCSB          KLIST
033200960917     C                   KFLD                    KSTA
033300960917     C                   KFLD                    KCDI
033400941103     C*
033500960917     C     KTAB1         KLIST
033600941103     C                   KFLD                    KKUT
033700941103     C                   KFLD                    KCOD
033800960917     C     KTAB2         KLIST
033900960917     C                   KFLD                    KKUT
034000960917     C                   KFLD                    KCOD
034100960917     C                   KFLD                    KKEY
034200941103     C                   Z-ADD     1             KKUT
034300960918     C                   MOVEL     'N'           £FINE             1
034400941103     C*----------------------------------------------------*
034500941103     C*  Definizione variabili
034600941103     C     *LIKE         DEFINE    TBLKUT        KKUT
034700941103     C     *LIKE         DEFINE    TBLCOD        KCOD
034800960917     C     *LIKE         DEFINE    TBLKEY        KKEY
034900960917     C     *LIKE         DEFINE    CSBSTA        KSTA
035000960917     C     *LIKE         DEFINE    CSBCDI        KCDI
035100931012     C*----------------------------------------------------*
035200960917     C                   SETON                                        05
035300960917     C     '  '          SCAN      D20TRA        LENGHT            2 0
035400931012     C                   SUB       1             LENGHT
035500941102     C     35            SUB       LENGHT        X                 2 0
035600991231     C*----------------------------------------------------*
035700991231     C*  RECUPERO DIVISA MONETA CORRENTE
035800991231     C                   CLEAR                   DGED
035900991231     C                   CLEAR                   DSBS02
036000991231     C                   MOVEL     'C'           T02MOD
036100991231     C                   MOVEL     KNSIF         T02SIF
036200991231     C                   MOVEL     'GED'         T02COD
036300991231     C                   MOVEL     '1'           T02KE1
036400991231     C                   CALL      'TIBS02R'
036500991231     C                   PARM                    KPJBA
036600991231     C                   PARM                    DSBS02
036700991231     C*
036800991231    3C     T02ERR        IFEQ      ' '
036900991231     C                   MOVEL     T02UNI        DGED
037000011001     C                   MOVEL     §GEDCN        DIVCOR
037100991231    3C                   ENDIF
037200991231     C*----------------------------------------------------*
037300020910     C                   open      tabel00f
037400991231     C* Carico tabella cambi/divise
037500020910     C                   Z-ADD     0             XX
037600991231     C                   MOVEL     'CV'          KCOD
037700991231     C     KTAB1         CHAIN     TABEL                              31
037800991231     C     *IN31         DOWEQ     '0'
037900991231     C     XX            ANDLT     100
038000991231     C                   MOVEL     TBLKEY        WDIV              3
038100991231     C     TBLFLG        IFEQ      ' '
038200991231     C                   ADD       1             XX
038300991231     C                   MOVEL     TBLKEY        CCV(XX)
038400991231     C                   MOVEL     TBLUNI        DSCV
038500991231     C                   MOVEL     §CVDVA        DVA(XX)
038600991231     C     §CVDVA        IFEQ      'S'
038700011001     C     WDIV          ANDNE     §GEDCN
038800991231     C                   MOVEL     WDIV          DIVALT
038900991231     C                   END
039000000103     C* Memorizzo nr. decimali della divisa corrente
039100011001     C     WDIV          IFEQ      §GEDCN
039200000103     C                   Z-ADD     §CVDEC        NRDEC             2 0
039300000103     C                   END
039400991231     C                   END
039500991231     C     KTAB1         READE     TABEL                                  31
039600991231     C                   END
039700931012     C*----------------------------------------------------*
039800960917     C* Carico tabella STATO C/ASSE.
039900020910     C                   Z-ADD     0             XX
040000960917     C                   MOVEL     '4S'          KCOD
040100960917     C                   MOVE      *BLANKS       KKEY
040200960917     C                   MOVE      D20STA        KKEY
040300960917     C                   MOVEL     D20STA        V1CSTA
040400960917     C     KTAB2         CHAIN     TABEL                              31
040500960917     C     *IN31         IFEQ      '0'
040600960917     C                   MOVEL     TBLUNI        V1DSTA
040700960917     C                   END
040800020910     C                   close     tabel00f
040900020910
041000020910     ** Con il PO utente reperisco i PO del terminal.
041100020910     C                   eval      I31Abi = 'TP'
041200020910     C                   eval      I31CPO = SimPOU
041300020910     C                   call      'TRUL31R'
041400020910     C                   parm                    KPJBA
041500020910     C                   parm                    TRUL31DS
041600020910     ** Reperisco l'ultimo elemento pieno.
041700020910     C                   eval      pogultimo = 1
041800020910     C     *zero         lookup    pog(pogultimo)                         01
041900020910     C                   if        %equal
042000020910     C                   eval      pogultimo = pogultimo - 1
042100020910     C                   endif
042200020910     C*----------------------------------------------------*
042300931011     C                   ENDSR
