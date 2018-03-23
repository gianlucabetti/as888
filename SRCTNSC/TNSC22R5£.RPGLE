000100940218     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300960918      * TRAS22R5                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600960918      *          STAMPA  ORDINATA X STATO C/ASS.-CLIENTE             *
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
001800020910     F                                     usropn
001900960918     FFNCSB05L  IF   E           K DISK
002000941103     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002100020910     F                                     usropn
002200941103     FCNABI00F  IF   E           K DISK
002300941103     FTNSC22P   O    E             PRINTER OFLIND(*IN01)
002400941103     D*--------------------------------------------------------------*
002500941103     D CCV             S              3    DIM(100)
002600000103     D DVA             S              1    DIM(100)
002700941103?     *--------------------------------------------------------------*
002800941103?     *  DS                                                          *
002900941103?     *--------------------------------------------------------------*
003000941103     D KPJBA         E DS
003100941103      *
003200010615     D YEURDS        E DS                  EXTNAME(YEURCODS)
003300941103     D TNSC20        E DS                  EXTNAME(TNSC20DS)
003400941103     D DSCV          E DS
003500000103     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003600000103     D DGED          E DS
003700941103      *
003800941103     D WLBDA8          DS
003900941103     D  G02DAT                 1      8  0
004000941103     D  G02INV                 9     16  0
004100941103     D  G02ERR                17     17
004200941103     D  G02TGI                18     22  0
004300941103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400941103     D CNCR80        E DS
004500020910     D TRUL31DS      E DS
004600020910     D                                     inz
004700020910     D  POG                           3  0
004800020910     D                                     dim(250)
004900020910     D                                     overlay(O31POG)
005000020910     D                 DS
005100020910     D  CSBPLP                 1      3  0
005200020910     D  CSBPAA                 4      7  0
005300020910     D  CSBPNS                 8      9  0
005400020910     D  CSBPSP                10     16  0
005500941103     D                SDS
005600941103     D  NOMPG                  1     10
005700020910     D xx              S              5I 0
005800020910     D pogultimo       S              5I 0
005900020910     D                                     inz(%elem(pog))
006000020910     D wrkpoc          S              3  0
006100020910     D RecCsbValido    S              1
006200941103     D*
006300941103     D COST1           C                   CONST('CONTANTE            ')
006400941103?     *--------------------------------------------------------------*
006500941103?     *  FILE                                                        *
006600941103?     *--------------------------------------------------------------*
006700941103     IAZORG01L  AA
006800941103     I                             P    4    5 0ORGFIL
006900941103     I                                 14   33  ORGDES
007000931011      *---------------------------------------------------------------*
007100931011      *   C I C L O      P R I N C I P A L E                          *
007200931011      *---------------------------------------------------------------*
007300931012     C*  PRIMA LETTURA
007400960918     C     D20TCD        IFEQ      0
007500960918     C     KSTA          CHAIN     FNCSB05L                           80
007600960918     C                   ELSE
007700960918     C     KCSB          CHAIN     FNCSB05L                           80
007800960918     C                   END
007900931012     C*  LOOP
008000931012     C     *IN80         DOWEQ     '0'
008100931012     C                   EXSR      STAMPA
008200960918     C     D20TCD        IFEQ      0
008300960918     C     KSTA          READE     FNCSB05L                               80
008400960918     C                   ELSE
008500960918     C     KCSB          READE     FNCSB05L                               80
008600960918     C                   END
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
011500020910     C                   eval      RecCsbValido = *on
011600931013     C*  Test. x Ragione sociale
011700960918     C     D20TRA        IFNE      *BLANK
011800931013     C                   SETOFF                                       40
011900931013     C*  Loop di ricerca sottostringa nel campo del subfile
012000931013     C                   DO        X             T                 2 0
012100941103     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
012200960918     C     RESULT        COMP      D20TRA                                 40
012300931013     C  N40              END
012400020910     C                   if        not *in40
012500020910     C                   eval      RecCsbValido = *off
012600020910     C                   endif
012700931013     C                   END
012800020910
012900020910     ** Se non è stata chiesta la selezione di un cliente
013000020910     ** seleziono solo i PO dello stesso terminal dell'utente.
013100020910     C                   if        d20tcd = 0 and d20tra = *blank
013200020910     C                   eval      *in40 = *off
013300020910     C                   movel     csbcdi        wrkpoc
013400020910     C                   eval      xx = 1
013500020910     C                   dow       xx <= pogultimo
013600020910     C                   if        csblnp = pog(xx) or wrkpoc = pog(xx)
013700020910     C                   eval      *in40 = *on
013800020910     C                   leave
013900020910     C                   endif
014000020910     C                   eval      xx = xx + 1
014100020910     C                   enddo
014200020910     C                   if        not *in40
014300020910     C                   eval      RecCsbValido = *off
014400020910     C                   endif
014500020910     C                   endif
014600020910
014700020910     ** Record di FNCSB00F valido, lo stampo.
014800020910     C                   IF        RecCsbValido = *on
014900931014     C                   MOVEL     CSBRMN        RNM
015000931014     C                   MOVEL     CSBLNA        LNA
015100931013     C                   MOVEL     CSBLNP        LNP
015200950626     C                   MOVE      CSBAAS        AA
015300931012     C                   MOVE      CSBNRA        NDC
015400931013     C                   MOVE      CSBDTE        XDTAMG
015500931013     C                   Z-ADD     0             XDTGMA
015600931013     C                   Z-ADD     2             XDTSTA
015700931013     C                   CALL      'XDATA'       PLDAT
015800931013     C                   MOVE      XDTGMA        DDC
015900931013     C                   MOVE      CSBNRS        NRS
016000931013     C                   MOVE      CSBNSP        NSP
016100941103     C                   MOVE      CSBAAS        WAA2              2 0
016200941103     C                   MOVEL     WAA2          XDTAMG
016300941103     C                   MOVE      CSBMGS        XDTAMG
016400931013     C                   Z-ADD     0             XDTGMA
016500931013     C                   Z-ADD     2             XDTSTA
016600931013     C                   CALL      'XDATA'       PLDAT
016700931014     C                   MOVEL     XDTGMA        DSP
016800941103     C                   MOVEL     CSBVCA        VCA
016900011002     C     VCA           IFEQ      *BLANK
017000011002     C                   MOVE      'ITL'         VCA
017100011002     C                   END
017200941103     C                   Z-ADD     CSBCAS        IMP
017300941103     C* Controllo la divisa
017400941103     C     VCA           IFNE      *BLANKS
017500941103     C                   Z-ADD     1             XX
017600941103     C     VCA           LOOKUP    CCV(XX)                                32
017700941103     C                   END
017800941103     C* Somma nel totale corretto
017900941103     C     VCA           IFEQ      *BLANKS
018000011001     C     VCA           OREQ      §GEDCN
018100941103     C                   ADD       CSBCAS        TOT
018200941103     C                   ELSE
018300000103     C     DVA(XX)       IFEQ      'S'
018400000103     C     *IN32         ANDEQ     '1'
018500000103     C                   ADD       CSBCAS        TOTEU
018600000103     C                   ELSE
018700010615     C*
018800010615     C* Per convertire gli importi in valute estere utilizzo
018900010615     C* la routine YEURCO in quanto il cambio indicato sul
019000010615     C* file dei C/assegni di filiale non è affidabile
019100010615     C                   CLEAR                   YEURDS
019200010615     C                   MOVEL     VCA           YECDVI
019300010615     C                   Z-ADD     CSBCAS        YECIMI
019400011001     C                   MOVEL     §GEDCN        YECDVO
019500010615     C* passo al pgm io il nr. dei decimali della divisa corrente
019600010615     C* in base a quanto memorizzato sulla tabella CV
019700010615     C                   MOVE      NRDEC         YECDCO
019800010615     C                   MOVEL     'H'           YECTAR
019900010615     C                   CALL      'YEURCO'
020000010615     C                   PARM                    YEURDS
020100010615     C                   Z-ADD     YECIMO        IMPV             13 3
020200010615     C                   ADD       IMPV          TOTV
020300941103     C                   END
020400000103     C                   END
020500941103     C*
020600931013     C                   MOVE      CSBABI        ABI
020700931013     C                   MOVE      CSBCAB        CAB
020800931014     C                   MOVEL     *BLANKS       BAI
020900931014     C     NDC           IFEQ      *BLANKS
021000931014     C                   MOVEL     COST1         BAI
021100931014     C                   END
021200931012     C     KEYABI        CHAIN     CNABI00F                           81
021300931012     C  N81              MOVEL     ABIIST        BAI
021400931013     C                   MOVEL     CSBTPA        ASS
021500931013     C                   MOVE      CSBTPI        ASS
021600931013     C                   MOVE      CSBCDI        CDM
021700931013     C                   MOVEL     CSBRSM        RGM
021800931013     C                   MOVEL     CSBRSD        RGD
021900931014     C                   MOVEL     *BLANK        DOC
022000931013     C*  Tipo Documento
022100931013     C     CSBFPC        IFEQ      '2'
022200931013     C                   MOVE      'BON.'        DOC
022300931013     C                   ELSE
022400931014     C     CSBPAB        IFEQ      99999
022500931014     C     CSBPCA        ANDEQ     99999
022600931014     C                   MOVE      'LETT'        DOC
022700931014     C                   ELSE
022800931014     C     CSBPAB        IFNE      0
022900931014     C     CSBPCA        ORNE      0
023000931013     C                   MOVE      'ASS.'        DOC
023100931013     C                   END
023200931014     C                   END
023300931013     C                   END
023400931013     C*
023500931013     C                   MOVE      CSBDDP        XDTAMG
023600931013     C                   Z-ADD     0             XDTGMA
023700931013     C                   Z-ADD     2             XDTSTA
023800931013     C                   CALL      'XDATA'       PLDAT
023900931013     C                   MOVE      XDTGMA        DDP
024000931013     C                   MOVE      CSBNDP        NDP
024100931013     C                   MOVEL     *BLANK        BAP
024200931013     C                   MOVE      CSBPAB        ABI
024300931013     C                   MOVE      CSBPCA        CAB
024400931013     C     KEYABI        CHAIN     CNABI00F                           81
024500931013     C  N81              MOVEL     ABIIST        BAP
024600931013     C                   MOVE      CSBDGC        XDTAMG
024700931013     C                   Z-ADD     0             XDTGMA
024800931013     C                   Z-ADD     2             XDTSTA
024900931013     C                   CALL      'XDATA'       PLDAT
025000931013     C                   MOVE      XDTGMA        DGC
025100931112     C                   MOVEL     *ZERO         LNPP
025200931112     C                   MOVEL     *ZERO         AAP
025300931112     C                   MOVE      *ZERO         NRSP
025400931112     C                   MOVE      *ZERO         NSPP
025500931112     C*
025600931112     C*  Stampo riferimnti spedizione pagamento contrassegno solo se
025700931112     C*  diverso da originale
025800020910     C     CSBLNP        IFNE      CSBPLP
025900020910     C     CSBAAS        ORNE      CSBPAA
026000020910     C     CSBNRS        ORNE      CSBPNS
026100020910     C     CSBNSP        ORNE      CSBPSP
026200931112     C                   MOVEL     CSBPLP        LNPP
026300931112     C                   MOVEL     CSBPAA        AAP
026400931112     C                   MOVE      CSBPNS        NRSP
026500931112     C                   MOVE      CSBPSP        NSPP
026600931112     C                   END
026700931013     C   01              WRITE     TES00
026800931013     C   01              SETOFF                                       01
026900931013     C                   WRITE     DET00
027000931013     C                   END
027100931012     C*
027200931011     C                   ENDSR
027300931011      *---------------------------------------------------------------*
027400941103      *  *INZSR: OPERAZIONI INIZIALI                                  *
027500931011      *---------------------------------------------------------------*
027600941103     C     *INZSR        BEGSR
027700931011      *---------------------------------------------------------------*
027800931011     C     *ENTRY        PLIST
027900931011     C                   PARM                    KPJBA
028000941103     C                   MOVEL     KPJBU         TNSC20
028100931012     C*-
028200931012     C* PLIST per richiamo Routine XDATA
028300931012     C     PLDAT         PLIST
028400931012     C                   PARM                    XDTAMG            6 0
028500931012     C                   PARM                    XDTGMA            6 0
028600931012     C                   PARM                    XDTSTA            1 0
028700931012     C*
028800931011     C                   Z-ADD     1             CODUT
028900931011     C                   CALL      'X§PARUT'
029000931011     C                   PARM                    UT§DSE
029100931011     C                   MOVEL     RAGUT         RSUT             20
029200931011     C                   MOVEL     REC80         CNCR80
029300931011     C                   MOVEL     KNMUS         §SEI              6
029400931011     C*
029500960918     C     KCSB          KLIST
029600960918     C                   KFLD                    KSTA
029700960918     C                   KFLD                    KCDI
029800931012     C*
029900931012     C     KEYABI        KLIST
030000931013     C                   KFLD                    ABI               5 0
030100931013     C                   KFLD                    CAB               5 0
030200941103     C*
030300960918     C     KTAB1         KLIST
030400941103     C                   KFLD                    KKUT
030500941103     C                   KFLD                    KCOD
030600960918     C     KTAB2         KLIST
030700960918     C                   KFLD                    KKUT
030800960918     C                   KFLD                    KCOD
030900960918     C                   KFLD                    KKEY
031000941103     C                   Z-ADD     1             KKUT
031100941103     C*----------------------------------------------------*
031200941103     C*  Definizione variabili
031300941103     C     *LIKE         DEFINE    TBLKUT        KKUT
031400941103     C     *LIKE         DEFINE    TBLCOD        KCOD
031500960918     C     *LIKE         DEFINE    TBLKEY        KKEY
031600960918     C     *LIKE         DEFINE    CSBSTA        KSTA
031700960918     C     *LIKE         DEFINE    CSBCDI        KCDI
031800941103     C*----------------------------------------------------*
031900960918     C                   MOVE      D20STA        KSTA
032000960918     C                   MOVE      D20TCD        KCDI
032100941103     C                   MOVE      SIMFEL        ORGFIL
032200020910     C                   open      AZORG01L
032300941103     C     ORGFIL        CHAIN     AZORG01L                           78
032400020910     C                   close     AZORG01L
032500941103     C  N78              MOVEL     ORGDES        FILIAL
032600931012     C*----------------------------------------------------*
032700960918     C                   SETON                                        05
032800960918     C                   MOVE      '  '          D20TRA
032900931012     C*----------------------------------------------------*
033000931013     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
033100960918     C     '  '          SCAN      D20TRA        LENGHT            2 0
033200931013     C                   SUB       1             LENGHT
033300941103     C     35            SUB       LENGHT        X                 2 0
033400931013     C*----------------------------------------------------*
033500931013     C*  Scrittura Prima pagina
033600931013     C                   MOVEL     NOMPG         NOMPGM
033700941103     C                   MOVE      D20INT        S22SEL
033800941103     C                   MOVE      D20ANN        S22AAS
033900941103     C                   MOVE      D20LNP        S22LNP
034000941103     C                   MOVE      D20NSR        S22NSR
034100941103     C                   MOVE      D20NRM        S22NRM
034200941103     C                   MOVE      D20SDD        G02INV
034300941103     C                   Z-ADD     0             G02DAT
034400941103     C                   MOVEL     '3'           G02ERR
034500941103     C                   CALL      'XSRDA8'
034600941103     C                   PARM                    WLBDA8
034700941103     C                   MOVE      G02DAT        S22SDD
034800941103     C                   MOVE      D20SDA        G02INV
034900941103     C                   Z-ADD     0             G02DAT
035000941103     C                   MOVEL     '3'           G02ERR
035100941103     C                   CALL      'XSRDA8'
035200941103     C                   PARM                    WLBDA8
035300941103     C                   MOVE      G02DAT        S22SDA
035400941103     C                   MOVE      D20SCD        S22SCD
035500941103     C                   MOVEL     D20SRA        S22SRS
035600941103     C                   MOVE      D20PDD        G02INV
035700941103     C                   Z-ADD     0             G02DAT
035800941103     C                   MOVEL     '3'           G02ERR
035900941103     C                   CALL      'XSRDA8'
036000941103     C                   PARM                    WLBDA8
036100941103     C                   MOVE      G02DAT        S22PDD
036200941103     C                   MOVE      D20PDA        G02INV
036300941103     C                   Z-ADD     0             G02DAT
036400941103     C                   MOVEL     '3'           G02ERR
036500941103     C                   CALL      'XSRDA8'
036600941103     C                   PARM                    WLBDA8
036700941103     C                   MOVE      G02DAT        S22PDA
036800941103     C                   MOVE      D20PND        S22PND
036900941103     C                   MOVE      D20PTN        S22PTI
037000941103     C                   MOVE      D20PCD        S22PCD
037100941103     C                   MOVEL     D20PRA        S22PRS
037200941103     C                   MOVE      D20TCR        S22TCA
037300960918     C                   MOVE      D20TCD        S22TCD
037400960918     C                   MOVEL     D20TRA        S22TRS
037500960918     C                   MOVE      D20STA        S22STA
037600931013     C*
037700931013     C     S22TCA        IFEQ      ' '
037800931013     C                   MOVEL     'TUTTI'       S22DCA
037900931013     C                   ELSE
038000931013     C     S22TCA        IFEQ      'I'
038100931013     C                   MOVEL     'INCASSAT'    S22DCA
038200931013     C                   MOVE      'I      '     S22DCA
038300931013     C                   ELSE
038400931013     C     S22TCA        IFEQ      'P'
038500931013     C                   MOVEL     'PAGATI  '    S22DCA
038600931013     C                   END
038700931013     C                   END
038800931013     C                   END
038900931013     C*
039000931013     C                   MOVEL     *ALL'-'       CMP198
039100931014     C                   MOVEL     *ALL'-'       CM1198
039200931013     C                   WRITE     PAG00
039300960919     C                   SETON                                        01
039400941103     C*----------------------------------------------------*
039500000103     C*  RECUPERO DIVISA MONETA CORRENTE
039600000103     C                   CLEAR                   DGED
039700000103     C                   CLEAR                   DSBS02
039800000103     C                   MOVEL     'C'           T02MOD
039900000103     C                   MOVEL     KNSIF         T02SIF
040000000103     C                   MOVEL     'GED'         T02COD
040100000103     C                   MOVEL     '1'           T02KE1
040200000103     C                   CALL      'TIBS02R'
040300000103     C                   PARM                    KPJBA
040400000103     C                   PARM                    DSBS02
040500000103     C*
040600000103    3C     T02ERR        IFEQ      ' '
040700000103     C                   MOVEL     T02UNI        DGED
040800011001     C                   MOVEL     §GEDCN        DIVCOR
040900000103    3C                   ENDIF
041000000103     C* Carico tabella cambi/divise
041100020910     C                   open      tabel00f
041200020910     C                   Z-ADD     0             XX
041300000103     C                   MOVEL     'CV'          KCOD
041400000103     C     KTAB1         CHAIN     TABEL                              31
041500000103     C     *IN31         DOWEQ     '0'
041600000103     C     XX            ANDLT     100
041700000103     C                   MOVEL     TBLKEY        WDIV              3
041800000103     C     TBLFLG        IFEQ      ' '
041900000103     C                   ADD       1             XX
042000000103     C                   MOVEL     TBLKEY        CCV(XX)
042100000103     C                   MOVEL     TBLUNI        DSCV
042200000103     C                   MOVEL     §CVDVA        DVA(XX)
042300000103     C     §CVDVA        IFEQ      'S'
042400011001     C     WDIV          ANDNE     §GEDCN
042500000103     C                   MOVEL     WDIV          DIVALT
042600000103     C                   END
042700000103     C* Memorizzo nr. decimali della divisa corrente
042800011001     C     WDIV          IFEQ      §GEDCN
042900000103     C                   Z-ADD     §CVDEC        NRDEC             2 0
043000000103     C                   END
043100000103     C                   END
043200000103     C     KTAB1         READE     TABEL                                  31
043300000103     C                   END
043400960918     C*----------------------------------------------------*
043500960918     C* Carico tabella STATO/CLIENTE
043600960918     C                   MOVEL     'CV'          KCOD
043700960918     C                   MOVEL     D20STA        KKEY
043800960918     C     KTAB2         CHAIN     TABEL                              31
043900960918     C     *IN31         IFEQ      '0'
044000960918     C     TBLFLG        ANDEQ     ' '
044100960918     C                   MOVEL     TBLUNI        S22DST
044200960918     C                   END
044300020910     C                   close     tabel00f
044400020910
044500020910     ** Con il PO utente reperisco i PO del terminal.
044600020910     C                   eval      I31Abi = 'TP'
044700020910     C                   eval      I31CPO = SimPOU
044800020910     C                   call      'TRUL31R'
044900020910     C                   parm                    KPJBA
045000020910     C                   parm                    TRUL31DS
045100020910     ** Reperisco l'ultimo elemento pieno.
045200020910     C                   eval      pogultimo = 1
045300020910     C     *zero         lookup    pog(pogultimo)                         01
045400020910     C                   if        %equal
045500020910     C                   eval      pogultimo = pogultimo - 1
045600020910     C                   endif
045700941103     C*----------------------------------------------------*
045800931011     C                   ENDSR
