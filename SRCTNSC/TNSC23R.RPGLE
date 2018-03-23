000100931008     H DECEDIT('0,') DATEDIT(*DMY.)
000200931008?     *--------------------------------------------------------------*
000300941103      * TNSC23R                                                      *
000400931008      *       *-----------------------------------------*            *
000500931008      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600931013      *          VISUALIZZAZIONE DATI BOLLA                          *
000700931008      *       *-----------------------------------------*            *
000800931008      *                                                              *
000900931008?     *--------------------------------------------------------------*
001000940218      * 20 -CONTRASSEGNO INCASSATO E NON PAGATO                      *
001100940218      * 80 -READ SU FLCSB01L                                         *
001200940218      * 81 -CHAIN SU CNABI.                                          *
001300940218?     *--------------------------------------------------------------*
001400941103     FFNCSB01L  IF   E           K DISK
001500931013     FCNABI00F  IF   E           K DISK
001600970408     FTABEL00F  IF   E           K DISK
001700941103     FTNSC23D   CF   E             WORKSTN
001800970408?     *--------------------------------------------------------------*
001900970408?     *  SCHIERA                                                     *
002000970408?     *--------------------------------------------------------------*
002100970408     D C4S             S              1  0 DIM(100)
002200970408     D D4S             S             90    DIM(100)
002300931008?     *--------------------------------------------------------------*
002400931008?     *  DS                                                          *
002500931008?     *--------------------------------------------------------------*
002600931008     D KPJBA         E DS
002700931008      *
002800941103     D TNSC20        E DS                  EXTNAME(TNSC20DS)
002900970408     D DS4S          E DS
003000120210     D tnsc30ds      E DS
003100931008      *
003200931008     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003300931008     D CNCR80        E DS
003400931013     D*
003500931116      *  Ds x numero spedizione contrass.
003600931116     D                 DS
003700931116     D  CSBLNP                 1      3  0
003800941103     D  CSBAAS                 4      7  0
003900941103     D  CSBNRS                 8      9  0
004000941103     D  CSBNSP                10     16  0
004100941103     D  NUMSPE                 1     16  0
004200931116      *
004300931116      *  Ds x numero spedizione pagamento
004400931116     D                 DS
004500931116     D  CSBPLP                 1      3  0
004600941103     D  CSBPAA                 4      7  0
004700941103     D  CSBPNS                 8      9  0
004800941103     D  CSBPSP                10     16  0
004900941103     D  SPEPAG                 1     16  0
005000941103      *
005100941103     D WLBDA8          DS
005200941103     D  G02DAT                 1      8  0
005300941103     D  G02INV                 9     16  0
005400941103     D  G02ERR                17     17
005500941103     D  G02TGI                18     22  0
005600931008     D                SDS
005700931008     D  NOMPG                  1     10
005800931008      *---------------------------------------------------------------*
005900931008      *   C I C L O      P R I N C I P A L E                          *
006000931008      *---------------------------------------------------------------*
006100931013     C*  CARICAMENTO DATI BOLLA IN PRIMA VIDEATA
006200931013     C                   EXSR      CARDAT
006300931008     C*
006400931008     C*  LOOP
006500931008     C     £FINE         DOUEQ     'S'
006600931008     C*
006700931008     C     TIPVID        CASEQ     '1'           GESVD1
006800931013     C     TIPVID        CASEQ     '2'           GESVD2
006900931008     C                   END
007000931008     C*
007100931008     C                   END
007200120210     c                   movel(p)  tnsc20        kpjbu
007300931008     C*
007400931008     C                   SETON                                        LR
007500931008      *---------------------------------------------------------------*
007600931013      *  CARDAT: INIZIALIZZAZIONE PARAMETRI VIDEO                     *
007700931008      *---------------------------------------------------------------*
007800931013     C     CARDAT        BEGSR
007900931008     C*
008000931008     C                   MOVEL     NOMPG         NOMPGM
008100931013     C*
008200931013     C*  CONTROLLO ESISTENZA BOLLA
008300941103     C                   MOVE      D20ANN        CSBAAS
008400941103     C                   MOVE      D20LNP        CSBLNP
008500941103     C                   MOVE      D20NSR        CSBNRS
008600941103     C                   MOVE      D20NSP        CSBNSP
008700931013     C*
008800941103     C     KEYBOL        CHAIN     FNCSB01L                           80
008900931013     C     *IN80         IFEQ      '0'
009000970408     C* Carico stato C/Assegno e relativa decodifica
009100970408     C                   MOVEL     CSBSTA        V1CSTA
009200970408     C                   Z-ADD     1             X                 4 0
009300970408     C     CSBSTA        LOOKUP    C4S(X)                                 31
009400970408     C                   MOVEL     D4S(X)        V1DSTA
009500970408     C* SOLO CONTRASSEGNI INCASSATI NON PAGATI
009600970408     C     D20TCR        IFEQ      'I'
009700931013     C     CSBDDC        COMP      0                                      20
009800931013     C  N20CSBDDP        COMP      0                                  2020
009900931013     C                   END
010000931013     C* SOLO CONTRASSEGNI PAGATI
010100941103     C     D20TCR        IFEQ      'P'
010200931013     C     CSBNDP        COMP      0                                      20
010300931013     C                   END
010400931013     C                   END
010500931013     C*
010600931013     C*  Test Tipo Contrassegni
010700931013     C     *IN20         IFEQ      '0'
010800931013     C     *IN80         ANDEQ     '0'
010900931013     C                   MOVEL     '1'           TIPVID            1
011000931013     C                   MOVEL     CSBLNA        LNA
011100931013     C                   MOVEL     CSBLNP        LNP
011200941103     C                   MOVE      CSBRMA        RFA
011300931013     C                   MOVE      CSBNRA        NRA
011400120220     c                   if        %subst(csbnra: 10: 1)= ' '
011500120210     c                   setoff                                       44
011600120210     c                   else
011700120210     c                   seton                                        44
011800120210     c                   end
011900941103     C                   MOVE      CSBDDC        G02INV
012000941103     C                   Z-ADD     0             G02DAT
012100941103     C                   MOVEL     '3'           G02ERR
012200941103     C                   CALL      'XSRDA8'
012300941103     C                   PARM                    WLBDA8
012400941103     C                   MOVE      G02DAT        DDC
012500941103     C                   MOVE      CSBDTE        G02INV
012600941103     C                   Z-ADD     0             G02DAT
012700941103     C                   MOVEL     '3'           G02ERR
012800941103     C                   CALL      'XSRDA8'
012900941103     C                   PARM                    WLBDA8
013000941103     C                   MOVE      G02DAT        DTE
013100931013     C                   MOVE      CSBNDT        NDT
013200941103     C                   MOVEL     CSBAAS        G02INV
013300941103     C                   MOVE      CSBMGS        G02INV
013400941103     C                   Z-ADD     0             G02DAT
013500941103     C                   MOVEL     '3'           G02ERR
013600941103     C                   CALL      'XSRDA8'
013700941103     C                   PARM                    WLBDA8
013800941103     C                   MOVE      G02DAT        DSP
013900931013     C                   MOVE      CSBNRS        NRS
014000931013     C                   MOVE      CSBNSP        NSP
014100941103     C                   MOVE      CSBCMB        CMB
014200941103     C                   MOVE      CSBCMP        CMP
014300941103     C                   MOVE      CSBVCA        VCA
014400011002     C     VCA           IFEQ      *BLANK
014500011002     C                   MOVE      'ITL'         VCA
014600011002     C                   END
014700941103     C                   Z-ADD     CSBCAS        ASS
014800931013     C                   MOVE      CSBABI        ABI
014900931013     C                   MOVE      CSBCAB        CAB
015000931013     C                   MOVEL     *BLANK        BAI
015100931013     C                   MOVE      CSBABI        ABIABI
015200931013     C                   MOVE      CSBCAB        ABICAB
015300931013     C     KEYABI        CHAIN     CNABI00F                           81
015400931013     C  N81              MOVEL     ABIIST        BAI
015500931013     C                   MOVE      CSBCDI        CDI
015600931013     C                   MOVEL     CSBRMN        RMN
015700931013     C                   MOVEL     CSBRSM        RSM
015800931013     C                   MOVEL     CSBRSD        RSD
015900931013     C                   MOVEL     CSBTPI        TPI
016000931111     C*  Tipo Documento
016100931111     C     CSBFPC        IFEQ      '2'
016200931111     C                   MOVE      'BON. '       DECTIP
016300931111     C                   ELSE
016400931111     C     CSBPAB        IFEQ      99999
016500931111     C     CSBPCA        ANDEQ     99999
016600931111     C                   MOVE      'LETT.'       DECTIP
016700931111     C                   ELSE
016800931111     C     CSBPAB        IFNE      0
016900931111     C     CSBPCA        ORNE      0
017000931111     C                   MOVE      'ASS. '       DECTIP
017100931111     C                   END
017200931111     C                   END
017300931111     C                   END
017400931013     C*----------------------------------------------------*
017500931013     C*  Tipo ASSEGNO
017600931013     C                   MOVEL     '     '       TIP
017700931013     C     CSBFPC        IFEQ      ' '
017800120210     C     CSBnra        ANDNE     ' '
017900931013     C*
018000931013     C     CSBTPA        IFEQ      'B'
018100931013     C                   MOVEL     'BANCA'       TIP
018200931013     C                   MOVE      'RIO  '       TIP
018300931013     C                   ELSE
018400931013     C     CSBTPA        IFEQ      'C'
018500931013     C                   MOVEL     'CIRCO'       TIP
018600931013     C                   MOVE      'LARE '       TIP
018700931013     C                   ELSE
018800931013     C     CSBTPA        IFEQ      'V'
018900931013     C                   MOVEL     'VAGLIA'      TIP
019000931013     C                   ELSE
019100931013     C     CSBTPA        IFEQ      'P'
019200931013     C                   MOVEL     'POSTALE'     TIP
019300931013     C                   END
019400931013     C                   END
019500931013     C                   END
019600931013     C                   END
019700931013     C*
019800931013     C                   END
019900931013     C*----------------------------------------------------*
020000941103     C                   MOVE      CSBDDP        G02INV
020100941103     C                   Z-ADD     0             G02DAT
020200941103     C                   MOVEL     '3'           G02ERR
020300941103     C                   CALL      'XSRDA8'
020400941103     C                   PARM                    WLBDA8
020500941103     C                   MOVE      G02DAT        DDP
020600931013     C                   MOVE      CSBNDP        NDP
020700931013     C                   MOVEL     *BLANK        BAP
020800931013     C                   MOVE      CSBPAB        ABIABI
020900931013     C                   MOVE      CSBPCA        ABICAB
021000931013     C     KEYABI        CHAIN     CNABI00F                           81
021100931013     C  N81              MOVEL     ABIIST        BAP
021200941103     C                   MOVE      CSBDGC        G02INV
021300941103     C                   Z-ADD     0             G02DAT
021400941103     C                   MOVEL     '3'           G02ERR
021500941103     C                   CALL      'XSRDA8'
021600941103     C                   PARM                    WLBDA8
021700941103     C                   MOVE      G02DAT        DGC
021800931116     C                   MOVEL     *ZERO         LNPP
021900931116     C                   MOVEL     *ZERO         AAP
022000931116     C                   MOVE      *ZERO         NRSP
022100931116     C                   MOVE      *ZERO         NSPP
022200931116     C*
022300931116     C*  Stampo riferimnti spedizione pagamento contrassegno solo se
022400931116     C*  diverso da originale
022500931116     C     NUMSPE        IFNE      SPEPAG
022600931013     C                   MOVEL     CSBPLP        LNPP
022700931013     C                   MOVEL     CSBPAA        AAP
022800931111     C                   MOVE      CSBPNS        NRSP
022900931111     C                   MOVE      CSBPSP        NSPP
023000931116     C                   END
023100931116     C*
023200931013     C                   ELSE
023300931013     C                   MOVEL     '2'           TIPVID
023400931013     C                   END
023500931011     C                   ENDSR
023600931011      *---------------------------------------------------------------*
023700931013      *  GESVD1: GESTIONE VIDEATA VISUALIZZAZIONE DATI BOLLA          *
023800931011      *---------------------------------------------------------------*
023900931013     C     GESVD1        BEGSR
024000931011     C*
024100941103     C                   EXFMT     SC23D01
024200120210     C*  multiassegni
024300120210     c   ki              exsr      srmulti
024400931011     C*  Fine elaborazione
024500120210     C   kl              MOVEL     'S'           £FINE
024600931011     C*
024700931013     C                   ENDSR
024800931013      *---------------------------------------------------------------*
024900931013      *  GESVD2: GESTIONE VIDEATA BOLLA NON TROVATA                   *
025000931013      *---------------------------------------------------------------*
025100931013     C     GESVD2        BEGSR
025200931013     C*
025300941103     C                   EXFMT     SC23D02
025400120210     C*  Fine elaborazione
025500120210     C   kl              MOVEL     'S'           £FINE
025600931013     C*
025700931013     C                   ENDSR
025800120210      *-----------------------------------------------------***********
025900120210      * richiamo pgm multiassegni
026000120210      *-----------------------------------------------------***********
026100120210     C     srmulti       BEGSR
026200120210     c* interroga multiassegni
026300120210     c                   clear                   tnsc30ds
026400120210     c                   eval      sc30CMTI = 'N'
026500120210     c                   eval      sc30AMBI = 'P'
026600120210     c                   eval      sc30TPCI = 'V'
026700120210     c                   eval      sc30AASI = csbaas
026800120210     c                   eval      sc30LNPI = csblnp
026900120210     c                   eval      sc30NRSI = csbnrs
027000120210     c                   eval      sc30NSPI = csbnsp
027100120210     c                   movel(p)  tnsc30ds      kpjbu
027200120210     c                   call      'TNSC30R'
027300120210     c                   parm                    kpjba
027400120210     C*
027500120210     c                   endsr
027600931011      *---------------------------------------------------------------*
027700941103      *  *INZSR: OPERAZIONI INIZIALI                                  *
027800931011      *---------------------------------------------------------------*
027900941103     C     *INZSR        BEGSR
028000931011      *---------------------------------------------------------------*
028100931011     C     *ENTRY        PLIST
028200931011     C                   PARM                    KPJBA
028300941103     C                   MOVEL     KPJBU         TNSC20
028400931011      *
028500931011     C                   Z-ADD     1             CODUT
028600931011     C                   CALL      'X§PARUT'
028700931011     C                   PARM                    UT§DSE
028800931011     C*          CODUT     CABEQ-1        FINE
028900931011     C                   MOVEL     RAGUT         RSUT             20
029000931011     C                   MOVEL     REC80         CNCR80
029100931011     C                   MOVEL     KNMUS         §SEI              6
029200931013     C*
029300931013     C*  CHIAVI
029400931013     C     KEYBOL        KLIST
029500931013     C                   KFLD                    CSBAAS
029600931013     C                   KFLD                    CSBLNP
029700931013     C                   KFLD                    CSBNRS
029800931013     C                   KFLD                    CSBNSP
029900931013     C*
030000931013     C     KEYABI        KLIST
030100931013     C                   KFLD                    ABIABI
030200931013     C                   KFLD                    ABICAB
030300970408     C*
030400970408     C     KEYTAB        KLIST
030500970408     C                   KFLD                    TBLKUT
030600970408     C                   KFLD                    TBLCOD
030700931011     C*----------------------------------------------------*
030800970408     C* Eseguo caricamento tabella 4S
030900970408     C                   Z-ADD     1             TBLKUT
031000970408     C                   MOVEL     '4S'          TBLCOD
031100970408     C                   Z-ADD     0             X
031200970408     C                   Z-ADD     0             C4S
031300970408     C                   MOVEA     *BLANKS       D4S
031400970408     C     KEYTAB        CHAIN     TABEL00F                           31
031500970408     C     *IN31         DOWEQ     '0'
031600970408     C     X             ANDLT     100
031700970408     C     TBLFLG        IFEQ      *BLANKS
031800970408     C                   ADD       1             X
031900970408     C                   MOVE      TBLKEY        C4S(X)
032000970408     C                   MOVEL     TBLUNI        D4S(X)
032100970408     C                   END
032200970408     C     KEYTAB        READE     TABEL00F                               31
032300970408     C                   END
032400931011     C*----------------------------------------------------*
032500970408     C                   MOVEL     'N'           £FINE             1
032600970408     C*----------------------------------------------------*
032700931011     C                   ENDSR
