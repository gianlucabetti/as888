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
002300120213     Ftncsm03l  IF   E           K DISK
002400941103     FTNSC22P   O    E             PRINTER OFLIND(*IN01)
002500941103     D*--------------------------------------------------------------*
002600941103     D CCV             S              3    DIM(100)
002700000103     D DVA             S              1    DIM(100)
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
003800941103      *
003900941103     D WLBDA8          DS
004000941103     D  G02DAT                 1      8  0
004100941103     D  G02INV                 9     16  0
004200941103     D  G02ERR                17     17
004300941103     D  G02TGI                18     22  0
004400941103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004500941103     D CNCR80        E DS
004600020910     D TRUL31DS      E DS
004700020910     D                                     inz
004800020910     D  POG                           3  0
004900020910     D                                     dim(250)
005000020910     D                                     overlay(O31POG)
005100020910     D                 DS
005200020910     D  CSBPLP                 1      3  0
005300020910     D  CSBPAA                 4      7  0
005400020910     D  CSBPNS                 8      9  0
005500020910     D  CSBPSP                10     16  0
005600941103     D                SDS
005700941103     D  NOMPG                  1     10
005800020910     D xx              S              5I 0
005900020910     D pogultimo       S              5I 0
006000020910     D                                     inz(%elem(pog))
006100020910     D wrkpoc          S              3  0
006200020910     D RecCsbValido    S              1
006300941103     D*
006400941103     D COST1           C                   CONST('CONTANTE            ')
006500941103?     *--------------------------------------------------------------*
006600941103?     *  FILE                                                        *
006700941103?     *--------------------------------------------------------------*
006800941103     IAZORG01L  AA
006900941103     I                             P    4    5 0ORGFIL
007000941103     I                                 14   33  ORGDES
007100931011      *---------------------------------------------------------------*
007200931011      *   C I C L O      P R I N C I P A L E                          *
007300931011      *---------------------------------------------------------------*
007400931012     C*  PRIMA LETTURA
007500960918     C     D20TCD        IFEQ      0
007600960918     C     KSTA          CHAIN     FNCSB05L                           80
007700960918     C                   ELSE
007800960918     C     KCSB          CHAIN     FNCSB05L                           80
007900960918     C                   END
008000931012     C*  LOOP
008100931012     C     *IN80         DOWEQ     '0'
008200931012     C                   EXSR      STAMPA
008300960918     C     D20TCD        IFEQ      0
008400960918     C     KSTA          READE     FNCSB05L                               80
008500960918     C                   ELSE
008600960918     C     KCSB          READE     FNCSB05L                               80
008700960918     C                   END
008800931011     C                   END
008900931011     C*
009000931013     C     TOT           IFGT      0
009100931013     C   01              WRITE     TES00
009200931013     C   01              SETOFF                                       01
009300931013     C                   WRITE     TOT00
009400931013     C                   END
009500000103     C* Se ci sono dei C/Assegni in Euro stampo il totale relativo
009600000103     C     TOTEU         IFGT      0
009700000103     C   01              WRITE     TES00
009800000103     C   01              SETOFF                                       01
009900000103     C                   WRITE     TOTE0
010000000103     C                   END
010100941103     C*
010200941103     C     TOTV          IFGT      0
010300941103     C   01              WRITE     TES00
010400941103     C   01              SETOFF                                       01
010500011002     C                   MOVEL     §GEDCN        VALV
010600941103     C                   WRITE     TOTV0
010700941103     C                   END
010800931011     C*
010900941103     C                   MOVEL     TNSC20        KPJBU
011000931011     C                   SETON                                        LR
011100931011     C*----------------------------------------------------*
011200931012     C*   STAMPA: Trasferimento dati da file a stampa      *
011300931011     C*----------------------------------------------------*
011400931012     C     STAMPA        BEGSR
011500931011     C*
011600020910     C                   eval      RecCsbValido = *on
011700931013     C*  Test. x Ragione sociale
011800960918     C     D20TRA        IFNE      *BLANK
011900931013     C                   SETOFF                                       40
012000931013     C*  Loop di ricerca sottostringa nel campo del subfile
012100931013     C                   DO        X             T                 2 0
012200941103     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
012300960918     C     RESULT        COMP      D20TRA                                 40
012400931013     C  N40              END
012500020910     C                   if        not *in40
012600020910     C                   eval      RecCsbValido = *off
012700020910     C                   endif
012800931013     C                   END
012900020910
013000020910     ** Se non è stata chiesta la selezione di un cliente
013100020910     ** seleziono solo i PO dello stesso terminal dell'utente.
013200020910     C                   if        d20tcd = 0 and d20tra = *blank
013300020910     C                   eval      *in40 = *off
013400020910     C                   movel     csbcdi        wrkpoc
013500020910     C                   eval      xx = 1
013600020910     C                   dow       xx <= pogultimo
013700020910     C                   if        csblnp = pog(xx) or wrkpoc = pog(xx)
013800020910     C                   eval      *in40 = *on
013900020910     C                   leave
014000020910     C                   endif
014100020910     C                   eval      xx = xx + 1
014200020910     C                   enddo
014300020910     C                   if        not *in40
014400020910     C                   eval      RecCsbValido = *off
014500020910     C                   endif
014600020910     C                   endif
014700020910
014800020910     ** Record di FNCSB00F valido, lo stampo.
014900020910     C                   IF        RecCsbValido = *on
015000931014     C                   MOVEL     CSBRMN        RNM
015100931014     C                   MOVEL     CSBLNA        LNA
015200931013     C                   MOVEL     CSBLNP        LNP
015300950626     C                   MOVE      CSBAAS        AA
015400931013     C                   MOVE      CSBNRS        NRS
015500931013     C                   MOVE      CSBNSP        NSP
015600941103     C                   MOVE      CSBAAS        WAA2              2 0
015700941103     C                   MOVEL     WAA2          XDTAMG
015800941103     C                   MOVE      CSBMGS        XDTAMG
015900931013     C                   Z-ADD     0             XDTGMA
016000931013     C                   Z-ADD     2             XDTSTA
016100931013     C                   CALL      'XDATA'       PLDAT
016200931014     C                   MOVEL     XDTGMA        DSP
016300941103     C                   MOVEL     CSBVCA        VCA
016400011002     C     VCA           IFEQ      *BLANK
016500011002     C                   MOVE      'ITL'         VCA
016600011002     C                   END
016700941103     C                   Z-ADD     CSBCAS        IMP
016800941103     C* Controllo la divisa
016900941103     C     VCA           IFNE      *BLANKS
017000941103     C                   Z-ADD     1             XX
017100941103     C     VCA           LOOKUP    CCV(XX)                                32
017200941103     C                   END
017300941103     C* Somma nel totale corretto
017400941103     C     VCA           IFEQ      *BLANKS
017500011001     C     VCA           OREQ      §GEDCN
017600941103     C                   ADD       CSBCAS        TOT
017700941103     C                   ELSE
017800000103     C     DVA(XX)       IFEQ      'S'
017900000103     C     *IN32         ANDEQ     '1'
018000000103     C                   ADD       CSBCAS        TOTEU
018100000103     C                   ELSE
018200010615     C*
018300010615     C* Per convertire gli importi in valute estere utilizzo
018400010615     C* la routine YEURCO in quanto il cambio indicato sul
018500010615     C* file dei C/assegni di filiale non è affidabile
018600010615     C                   CLEAR                   YEURDS
018700010615     C                   MOVEL     VCA           YECDVI
018800010615     C                   Z-ADD     CSBCAS        YECIMI
018900011001     C                   MOVEL     §GEDCN        YECDVO
019000010615     C* passo al pgm io il nr. dei decimali della divisa corrente
019100010615     C* in base a quanto memorizzato sulla tabella CV
019200010615     C                   MOVE      NRDEC         YECDCO
019300010615     C                   MOVEL     'H'           YECTAR
019400010615     C                   CALL      'YEURCO'
019500010615     C                   PARM                    YEURDS
019600010615     C                   Z-ADD     YECIMO        IMPV             13 3
019700010615     C                   ADD       IMPV          TOTV
019800941103     C                   END
019900000103     C                   END
020000941103     C*
020100120220     c                   if        %subst(csbnra: 10: 1)=' '
020200120213     c                   setoff                                       44
020300120213     C                   MOVE      CSBNRA        NDC
020400120213     C                   MOVE      CSBDTE        XDTAMG
020500120213     C                   Z-ADD     0             XDTGMA
020600120213     C                   Z-ADD     2             XDTSTA
020700120213     C                   CALL      'XDATA'       PLDAT
020800120213     C                   MOVE      XDTGMA        DDC
020900931013     C                   MOVE      CSBABI        ABI
021000931013     C                   MOVE      CSBCAB        CAB
021100931014     C                   MOVEL     *BLANKS       BAI
021200931012     C     KEYABI        CHAIN     CNABI00F                           81
021300931012     C  N81              MOVEL     ABIIST        BAI
021400931013     C                   MOVEL     CSBTPA        ASS
021500931013     C                   MOVE      CSBTPI        ASS
021600120213     c                   end
021700120213     c*
021800120213     C     NDC           IFEQ      *BLANKS
021900120213     C                   MOVEL     COST1         BAI
022000120213     C                   END
022100931013     C                   MOVE      CSBCDI        CDM
022200931013     C                   MOVEL     CSBRSM        RGM
022300931013     C                   MOVEL     CSBRSD        RGD
022400931014     C                   MOVEL     *BLANK        DOC
022500931013     C*  Tipo Documento
022600931013     C     CSBFPC        IFEQ      '2'
022700931013     C                   MOVE      'BON.'        DOC
022800931013     C                   ELSE
022900931014     C     CSBPAB        IFEQ      99999
023000931014     C     CSBPCA        ANDEQ     99999
023100931014     C                   MOVE      'LETT'        DOC
023200931014     C                   ELSE
023300931014     C     CSBPAB        IFNE      0
023400931014     C     CSBPCA        ORNE      0
023500931013     C                   MOVE      'ASS.'        DOC
023600931013     C                   END
023700931014     C                   END
023800931013     C                   END
023900931013     C*
024000931013     C                   MOVE      CSBDDP        XDTAMG
024100931013     C                   Z-ADD     0             XDTGMA
024200931013     C                   Z-ADD     2             XDTSTA
024300931013     C                   CALL      'XDATA'       PLDAT
024400931013     C                   MOVE      XDTGMA        DDP
024500931013     C                   MOVE      CSBNDP        NDP
024600931013     C                   MOVEL     *BLANK        BAP
024700931013     C                   MOVE      CSBPAB        ABI
024800931013     C                   MOVE      CSBPCA        CAB
024900931013     C     KEYABI        CHAIN     CNABI00F                           81
025000931013     C  N81              MOVEL     ABIIST        BAP
025100931013     C                   MOVE      CSBDGC        XDTAMG
025200931013     C                   Z-ADD     0             XDTGMA
025300931013     C                   Z-ADD     2             XDTSTA
025400931013     C                   CALL      'XDATA'       PLDAT
025500931013     C                   MOVE      XDTGMA        DGC
025600931112     C                   MOVEL     *ZERO         LNPP
025700931112     C                   MOVEL     *ZERO         AAP
025800931112     C                   MOVE      *ZERO         NRSP
025900931112     C                   MOVE      *ZERO         NSPP
026000931112     C*
026100931112     C*  Stampo riferimnti spedizione pagamento contrassegno solo se
026200931112     C*  diverso da originale
026300020910     C     CSBLNP        IFNE      CSBPLP
026400020910     C     CSBAAS        ORNE      CSBPAA
026500020910     C     CSBNRS        ORNE      CSBPNS
026600020910     C     CSBNSP        ORNE      CSBPSP
026700931112     C                   MOVEL     CSBPLP        LNPP
026800931112     C                   MOVEL     CSBPAA        AAP
026900931112     C                   MOVE      CSBPNS        NRSP
027000931112     C                   MOVE      CSBPSP        NSPP
027100931112     C                   END
027200120220     c                   if        %subst(csbnra: 10: 1)=' '
027300931013     C   01              WRITE     TES00
027400931013     C   01              SETOFF                                       01
027500931013     C                   WRITE     DET00
027600120213     c                   else
027700120213     c                   exsr      srass
027800120213     C                   END
027900931013     C                   END
028000931012     C*
028100931011     C                   ENDSR
028200120213      *---------------------------------------------------------------*
028300120213      *  stampa rif assegni
028400120213      *---------------------------------------------------------------*
028500120213     C     srass         BEGSR
028600120213     c                   setoff                                       44
028700120213     c     csbnra        setll     tncsm03l
028800120213     c                   do        *hival
028900120213     c     csbnra        reade     tncsm03l
029000120213     c                   if        %eof(tncsm03l)
029100120213     c                   leave
029200120213     c                   end
029300120213     C                   MOVE      CSmNRA        NDC
029400120213     C                   MOVE      CSmDTE        XDTAMG
029500120213     C                   Z-ADD     0             XDTGMA
029600120213     C                   Z-ADD     2             XDTSTA
029700120213     C                   CALL      'XDATA'       PLDAT
029800120213     C                   MOVE      XDTGMA        DDC
029900120213     C                   MOVE      CSmABI        ABI
030000120213     C                   MOVE      CSmCAB        CAB
030100120213     C                   MOVEL     *BLANKS       BAI
030200120213     C     KEYABI        CHAIN     CNABI00F                           81
030300120213     C  N81              MOVEL     ABIIST        BAI
030400120213     C                   MOVEL     CSmTPA        ASS
030500120213     C                   MOVE      CSmTPI        ASS
030600120213     C   01              WRITE     TES00
030700120213     C   01              SETOFF                                       01
030800120213     C   01              SETOFF                                       44
030900120213     C                   WRITE     DET00
031000120213     c                   seton                                        44
031100120213     c                   enddo
031200120213     C*
031300120213     C                   ENDSR
031400931011      *---------------------------------------------------------------*
031500941103      *  *INZSR: OPERAZIONI INIZIALI                                  *
031600931011      *---------------------------------------------------------------*
031700941103     C     *INZSR        BEGSR
031800931011      *---------------------------------------------------------------*
031900931011     C     *ENTRY        PLIST
032000931011     C                   PARM                    KPJBA
032100941103     C                   MOVEL     KPJBU         TNSC20
032200931012     C*-
032300931012     C* PLIST per richiamo Routine XDATA
032400931012     C     PLDAT         PLIST
032500931012     C                   PARM                    XDTAMG            6 0
032600931012     C                   PARM                    XDTGMA            6 0
032700931012     C                   PARM                    XDTSTA            1 0
032800931012     C*
032900931011     C                   Z-ADD     1             CODUT
033000931011     C                   CALL      'X§PARUT'
033100931011     C                   PARM                    UT§DSE
033200931011     C                   MOVEL     RAGUT         RSUT             20
033300931011     C                   MOVEL     REC80         CNCR80
033400931011     C                   MOVEL     KNMUS         §SEI              6
033500931011     C*
033600960918     C     KCSB          KLIST
033700960918     C                   KFLD                    KSTA
033800960918     C                   KFLD                    KCDI
033900931012     C*
034000931012     C     KEYABI        KLIST
034100931013     C                   KFLD                    ABI               5 0
034200931013     C                   KFLD                    CAB               5 0
034300941103     C*
034400960918     C     KTAB1         KLIST
034500941103     C                   KFLD                    KKUT
034600941103     C                   KFLD                    KCOD
034700960918     C     KTAB2         KLIST
034800960918     C                   KFLD                    KKUT
034900960918     C                   KFLD                    KCOD
035000960918     C                   KFLD                    KKEY
035100941103     C                   Z-ADD     1             KKUT
035200941103     C*----------------------------------------------------*
035300941103     C*  Definizione variabili
035400941103     C     *LIKE         DEFINE    TBLKUT        KKUT
035500941103     C     *LIKE         DEFINE    TBLCOD        KCOD
035600960918     C     *LIKE         DEFINE    TBLKEY        KKEY
035700960918     C     *LIKE         DEFINE    CSBSTA        KSTA
035800960918     C     *LIKE         DEFINE    CSBCDI        KCDI
035900941103     C*----------------------------------------------------*
036000960918     C                   MOVE      D20STA        KSTA
036100960918     C                   MOVE      D20TCD        KCDI
036200941103     C                   MOVE      SIMFEL        ORGFIL
036300020910     C                   open      AZORG01L
036400941103     C     ORGFIL        CHAIN     AZORG01L                           78
036500020910     C                   close     AZORG01L
036600941103     C  N78              MOVEL     ORGDES        FILIAL
036700931012     C*----------------------------------------------------*
036800960918     C                   SETON                                        05
036900960918     C                   MOVE      '  '          D20TRA
037000931012     C*----------------------------------------------------*
037100931013     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
037200960918     C     '  '          SCAN      D20TRA        LENGHT            2 0
037300931013     C                   SUB       1             LENGHT
037400941103     C     35            SUB       LENGHT        X                 2 0
037500931013     C*----------------------------------------------------*
037600931013     C*  Scrittura Prima pagina
037700931013     C                   MOVEL     NOMPG         NOMPGM
037800941103     C                   MOVE      D20INT        S22SEL
037900941103     C                   MOVE      D20ANN        S22AAS
038000941103     C                   MOVE      D20LNP        S22LNP
038100941103     C                   MOVE      D20NSR        S22NSR
038200941103     C                   MOVE      D20NRM        S22NRM
038300941103     C                   MOVE      D20SDD        G02INV
038400941103     C                   Z-ADD     0             G02DAT
038500941103     C                   MOVEL     '3'           G02ERR
038600941103     C                   CALL      'XSRDA8'
038700941103     C                   PARM                    WLBDA8
038800941103     C                   MOVE      G02DAT        S22SDD
038900941103     C                   MOVE      D20SDA        G02INV
039000941103     C                   Z-ADD     0             G02DAT
039100941103     C                   MOVEL     '3'           G02ERR
039200941103     C                   CALL      'XSRDA8'
039300941103     C                   PARM                    WLBDA8
039400941103     C                   MOVE      G02DAT        S22SDA
039500941103     C                   MOVE      D20SCD        S22SCD
039600941103     C                   MOVEL     D20SRA        S22SRS
039700941103     C                   MOVE      D20PDD        G02INV
039800941103     C                   Z-ADD     0             G02DAT
039900941103     C                   MOVEL     '3'           G02ERR
040000941103     C                   CALL      'XSRDA8'
040100941103     C                   PARM                    WLBDA8
040200941103     C                   MOVE      G02DAT        S22PDD
040300941103     C                   MOVE      D20PDA        G02INV
040400941103     C                   Z-ADD     0             G02DAT
040500941103     C                   MOVEL     '3'           G02ERR
040600941103     C                   CALL      'XSRDA8'
040700941103     C                   PARM                    WLBDA8
040800941103     C                   MOVE      G02DAT        S22PDA
040900941103     C                   MOVE      D20PND        S22PND
041000941103     C                   MOVE      D20PTN        S22PTI
041100941103     C                   MOVE      D20PCD        S22PCD
041200941103     C                   MOVEL     D20PRA        S22PRS
041300941103     C                   MOVE      D20TCR        S22TCA
041400960918     C                   MOVE      D20TCD        S22TCD
041500960918     C                   MOVEL     D20TRA        S22TRS
041600960918     C                   MOVE      D20STA        S22STA
041700931013     C*
041800931013     C     S22TCA        IFEQ      ' '
041900931013     C                   MOVEL     'TUTTI'       S22DCA
042000931013     C                   ELSE
042100931013     C     S22TCA        IFEQ      'I'
042200931013     C                   MOVEL     'INCASSAT'    S22DCA
042300931013     C                   MOVE      'I      '     S22DCA
042400931013     C                   ELSE
042500931013     C     S22TCA        IFEQ      'P'
042600931013     C                   MOVEL     'PAGATI  '    S22DCA
042700931013     C                   END
042800931013     C                   END
042900931013     C                   END
043000931013     C*
043100931013     C                   MOVEL     *ALL'-'       CMP198
043200931014     C                   MOVEL     *ALL'-'       CM1198
043300931013     C                   WRITE     PAG00
043400960919     C                   SETON                                        01
043500941103     C*----------------------------------------------------*
043600000103     C*  RECUPERO DIVISA MONETA CORRENTE
043700000103     C                   CLEAR                   DGED
043800000103     C                   CLEAR                   DSBS02
043900000103     C                   MOVEL     'C'           T02MOD
044000000103     C                   MOVEL     KNSIF         T02SIF
044100000103     C                   MOVEL     'GED'         T02COD
044200000103     C                   MOVEL     '1'           T02KE1
044300000103     C                   CALL      'TIBS02R'
044400000103     C                   PARM                    KPJBA
044500000103     C                   PARM                    DSBS02
044600000103     C*
044700000103    3C     T02ERR        IFEQ      ' '
044800000103     C                   MOVEL     T02UNI        DGED
044900011001     C                   MOVEL     §GEDCN        DIVCOR
045000000103    3C                   ENDIF
045100000103     C* Carico tabella cambi/divise
045200020910     C                   open      tabel00f
045300020910     C                   Z-ADD     0             XX
045400000103     C                   MOVEL     'CV'          KCOD
045500000103     C     KTAB1         CHAIN     TABEL                              31
045600000103     C     *IN31         DOWEQ     '0'
045700000103     C     XX            ANDLT     100
045800000103     C                   MOVEL     TBLKEY        WDIV              3
045900000103     C     TBLFLG        IFEQ      ' '
046000000103     C                   ADD       1             XX
046100000103     C                   MOVEL     TBLKEY        CCV(XX)
046200000103     C                   MOVEL     TBLUNI        DSCV
046300000103     C                   MOVEL     §CVDVA        DVA(XX)
046400000103     C     §CVDVA        IFEQ      'S'
046500011001     C     WDIV          ANDNE     §GEDCN
046600000103     C                   MOVEL     WDIV          DIVALT
046700000103     C                   END
046800000103     C* Memorizzo nr. decimali della divisa corrente
046900011001     C     WDIV          IFEQ      §GEDCN
047000000103     C                   Z-ADD     §CVDEC        NRDEC             2 0
047100000103     C                   END
047200000103     C                   END
047300000103     C     KTAB1         READE     TABEL                                  31
047400000103     C                   END
047500960918     C*----------------------------------------------------*
047600960918     C* Carico tabella STATO/CLIENTE
047700960918     C                   MOVEL     'CV'          KCOD
047800960918     C                   MOVEL     D20STA        KKEY
047900960918     C     KTAB2         CHAIN     TABEL                              31
048000960918     C     *IN31         IFEQ      '0'
048100960918     C     TBLFLG        ANDEQ     ' '
048200960918     C                   MOVEL     TBLUNI        S22DST
048300960918     C                   END
048400020910     C                   close     tabel00f
048500020910
048600020910     ** Con il PO utente reperisco i PO del terminal.
048700020910     C                   eval      I31Abi = 'TP'
048800020910     C                   eval      I31CPO = SimPOU
048900020910     C                   call      'TRUL31R'
049000020910     C                   parm                    KPJBA
049100020910     C                   parm                    TRUL31DS
049200020910     ** Reperisco l'ultimo elemento pieno.
049300020910     C                   eval      pogultimo = 1
049400020910     C     *zero         lookup    pog(pogultimo)                         01
049500020910     C                   if        %equal
049600020910     C                   eval      pogultimo = pogultimo - 1
049700020910     C                   endif
049800941103     C*----------------------------------------------------*
049900931011     C                   ENDSR
