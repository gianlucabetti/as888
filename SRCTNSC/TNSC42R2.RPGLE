000100140325     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300950710      * TRAS42R2                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600950710      *        STAMPA DATI SELEZ. RIFER.MITTENTE NUMERICO            *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000950710     FTABEL00F  IF   E           K DISK
001100950710     FCNABI00F  IF   E           K DISK
001200950706     FTNCSA01L  IF   E           K DISK
001300950706     FTNCSB06L  IF   E           K DISK
001400120215     FTNCSm03L  IF   E           K DISK
001500950710     FTNSC42P   O    E             PRINTER OFLIND(*IN01)
001600950706?     *--------------------------------------------------------------*
001700950706?     *  SCHIERE                                                     *
001800950706?     *--------------------------------------------------------------*
001900950710     D CCV             S              3    DIM(100)
002000000107     D DVA             S              1    DIM(100)
002100011001     D*****               CMB       100  9 3             CAMBIO DIVISA ITA.
002200931011?     *--------------------------------------------------------------*
002300931011?     *  DS                                                          *
002400931011?     *--------------------------------------------------------------*
002500931011     D KPJBA         E DS
002600010615     D YEURDS        E DS                  EXTNAME(YEURCODS)
002700931011      *
002800950706     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900980127     D  TCU                  398    697
003000980127     D                                     DIM(50)
003100980127     D  KCU                  698    847P 0
003200980127     D                                     DIM(50)
003300980127     D                                     PACKEVEN
003400980127     D TCUDS           DS
003500980127     D  F34                    3      4
003600980127     D  F56                    5      6
003700950706     D CNCR80        E DS
003800000107     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003900000107     D DGED          E DS
004000950710     D DSCV          E DS
004100950706     D*
004200950706     D*
004300941102     D WLBDA8          DS
004400941102     D  G02DAT                 1      8  0
004500941102     D  G02INV                 9     16  0
004600941102     D  G02ERR                17     17
004700941102     D  G02TGI                18     22  0
004800980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
004900980127     D CNACO         E DS                  EXTNAME(CNACO00F)
005000980127     D CNIND         E DS                  EXTNAME(CNIND00F)
005100980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
005200980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
005300010223     D*FTRUL15    E DSTRUL15DS
005400950706     D TNSC40        E DS                  EXTNAME(TNSC40DS)
005500931011      *
005600931011     D                SDS
005700931011     D  NOMPG                  1     10
005800950710     D*
005900950710     D COST1           C                   CONST('CONTANTE            ')
006000950710     D COST2           C                   CONST('ANNULLATO           ')
006100120215     D*
006200120215     d mnra            s                   like(csmnra)
006300120215     d mabi            s                   like(csmabi)
006400120215     d mcab            s                   like(csmcab)
006500120215      *
006600931011      *---------------------------------------------------------------*
006700931011      *   C I C L O      P R I N C I P A L E                          *
006800931011      *---------------------------------------------------------------*
006900950710     C*  Posizionamento iniziale x stampa
007000950710     C     KCSB2         SETLL     TNCSB06L
007100950710     C     KRMN          READE     TNCSB06L                               23
007200950710     C                   Z-ADD     CSBMGS        WDTSPE            8 0
007300950710     C                   MOVEL     CSBAAS        WDTSPE            8 0
007400931011     C*
007500931011     C*  LOOP
007600950710     C     *IN23         DOWEQ     '0'
007700950707     C     WDTSPE        ANDLE     D40SA1
007800950706     C* Se ho selezionato una sola lnp. gestisco posiz. su quella
007900950706     C     D40LP1        IFNE      0
008000950706     C                   EXSR      REDLNP
008100950706     C                   ELSE
008200950707     C                   EXSR      GESREC
008300950706     C                   END
008400931011     C                   END
008500950710     C*
008600950710     C     TOT           IFGT      0
008700950710     C   01              WRITE     TES00
008800950710     C   01              SETOFF                                       01
008900950710     C                   WRITE     TOT00
009000950710     C                   END
009100000107     C*
009200000107     C     TOTE          IFGT      0
009300000107     C   01              WRITE     TES00
009400000107     C   01              SETOFF                                       01
009500000107     C                   WRITE     TOTE0
009600000107     C                   END
009700950710     C*
009800950710     C     TOTV          IFGT      0
009900950710     C   01              WRITE     TES00
010000950710     C   01              SETOFF                                       01
010100950710     C                   WRITE     TOTV0
010200950710     C                   END
010300950710     C*
010400971222     C* RICHIAMO PGM X CHIUSURA IN LR
010500980127     C                   CLEAR                   TIBS69
010600980127     C                   MOVEL     'C'           I69TLA
010700980127     C                   CALL      'TIBS69R'
010800980127     C                   PARM                    TIBS69
010900980127     C                   PARM                    CNACO
011000980127     C                   PARM                    CNIND
011100980127     C                   PARM                    CNCLP
011200980127     C                   PARM                    FNCLS
011300971222     C*
011400950710     C                   SETON                                        LR
011500950706     C*----------------------------------------------------*
011600950706     C*   REDLNP: Lettura con selez. per LNP               *
011700950706     C*----------------------------------------------------*
011800950706     C     REDLNP        BEGSR
011900950706     C*
012000950707     C     CSBLNP        IFLT      D40LP1
012100950707     C     KCSB2         SETLL     TNCSB06L
012200950706     C     KRMN          READE     TNCSB06L                               23
012300950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
012400950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
012500950707     C                   Z-ADD     CSBMGS        KMGS
012600950707     C                   Z-ADD     CSBAAS        KAAS
012700950706     C                   END
012800950706     C*  Leggo lnp.
012900950706     C     *IN23         DOWEQ     '0'
013000950707     C     CSBLNP        ANDEQ     D40LP1
013100950706     C     WDTSPE        ANDLE     D40SA1
013200950706     C                   EXSR      GESREC
013300950706     C                   END
013400950706     C*
013500950711     C                   Z-ADD     CSBAAS        KAAS
013600950711     C                   Z-ADD     CSBMGS        KMGS
013700950707     C     CSBLNP        IFGT      D40LP1
013800950706     C     KCSB1         SETGT     TNCSB06L
013900950707     C     KRMN          READE     TNCSB06L                               23
014000950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
014100950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
014200950707     C                   Z-ADD     CSBMGS        KMGS
014300950707     C                   Z-ADD     CSBAAS        KAAS
014400950707     C                   END
014500950706     C*
014600950706     C                   ENDSR
014700120215     C*----------------------------------------------------*
014800120215     C*   srmulti - per multiassegni
014900120215     C*----------------------------------------------------*
015000120215     C     srmulti       BEGSR
015100120215     C*
015200120215     c                   eval      mnra = *hival
015300120215     c                   eval      mabi = *hival
015400120215     c                   eval      mcab = *hival
015500120215     c     csbnra        setll     tncsm03l
015600120215     c                   do        *hival
015700120215     c     csbnra        reade     tncsm03l
015800120215     c                   if        %eof(tncsm03l)
015900120215     c                   leave
016000120215     c                   end
016100120215     C                   if        csmnra = WNRA
016200120215     c                   eval      mnra = csmnra
016300120215     c                   eval      mabi = csmabi
016400120215     c                   eval      mcab = csmcab
016500120215     c                   leave
016600120215     c                   end
016700120215     c                   enddo
016800120215     C*
016900120215     C                   ENDSR
017000950706     C*----------------------------------------------------*
017100950710     C*   GESREC: Stampa contassegni selezionati           *
017200950706     C*----------------------------------------------------*
017300950706     C     GESREC        BEGSR
017400950706     C*----------------------------------------------------*
017500950706     C* Se il documento è valido controllo se il record rientra
017600950706     C* nei limiti richiesti
017700950706     C                   MOVEL     D40NAS        WNRA             10
017800120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
017900120220     c                             csbtpi = 'M' and csbabi = 0
018000120215     c                   exsr      srmulti
018100120215     c                   else
018200120215     c                   eval      mnra = csbnra
018300120215     c                   eval      mabi = csbabi
018400120215     c                   eval      mcab = csbcab
018500120215     c                   end
018600950706     C*----------------------------------------------------*
018700950706     C     D40ABI        IFEQ      *ZEROS
018800120215     C     D40ABI        OREQ      mABI
018900950720     C     D40CAB        IFEQ      *ZEROS
019000120215     C     D40CAB        OREQ      mCAB
019001140325     C     D40imp        IFEQ      *ZEROS
019002140325     C     D40imp        OREQ      csbcas
019100950720     C     WNRA          IFEQ      *BLANKS
019200950720     C     WNRA          OREQ      *ZEROS
019300120215     C     WNRA          OREQ      mNRA
019400950706     C*----------------------------------------------------*
019500950706     C* Selezione per stato C/Assegno
019600950706     C     D40TCR        IFEQ      ' '
019700950706     C     D40TCR        OREQ      'D'
019800950706     C     CSBDDC        ANDEQ     0
019900960418     C     CSBSTA        ANDEQ     0
020000950706     C     D40TCR        OREQ      'G'
020100950706     C     CSBSTA        ANDEQ     2
020200950706     C     D40TCR        OREQ      'A'
020300950706     C     CSBSTA        ANDEQ     9
020400950706     C     D40TCR        OREQ      'S'
020500950706     C     CSBSTA        ANDEQ     5
020600010112     C     D40TCR        OREQ      '3'
020700010112     C     CSBSTA        ANDEQ     3
020800950706     C     D40TCR        OREQ      'I'
020900950706     C     CSBDDC        ANDGT     0
021000950706     C     CSBDDP        ANDEQ     0
021100950706     C     D40TCR        OREQ      'P'
021200950706     C     CSBDDP        ANDGT     0
021300960916     C     D40TCR        OREQ      'T'
021400960916     C     CSBSTA        ANDEQ     6
021500960916     C     D40TCR        OREQ      'R'
021600960916     C     CSBSTA        ANDEQ     7
021700960916     C     D40TCR        OREQ      'O'
021800960916     C     CSBSTA        ANDEQ     8
021900950710     C*  Spedizione
022000950710     C                   MOVE      CSBAAS        AAS
022100950706     C                   MOVEL     CSBMGS        MM                2 0
022200950706     C                   MOVE      CSBMGS        GG                2 0
022300950710     C                   MOVEL     GG            DSP
022400950710     C                   MOVE      MM            DSP
022500950710     C                   MOVE      CSBLNA        LNA
022600950710     C                   MOVE      CSBLNP        LNP
022700950710     C                   MOVE      CSBNRS        NRS
022800950710     C                   MOVE      CSBNSP        NSP
022900950710     C*  C/Assegno
023000950710     C                   MOVE      CSBVCA        VCA
023100950710     C                   Z-ADD     CSBCAS        IMP
023200120215     C                   MOVEL     *BLANKS       BAI
023300950710     C*  Dati Incasso
023400120220     c                   if        %subst(csbnra: 10: 1)=' '
023500120215     c                   setoff                                       44
023600950710     C                   MOVE      CSBNRA        NDC
023700950710     C                   MOVE      CSBDTE        XDTAMG
023800950710     C                   Z-ADD     0             XDTGMA
023900950710     C                   Z-ADD     2             XDTSTA
024000950710     C                   CALL      'XDATA'       PLDAT
024100950710     C                   MOVE      XDTGMA        DDC
024200950710     C*  Decodifica banca incasso
024300950710     C                   Z-ADD     CSBABI        KABI
024400950710     C                   Z-ADD     CSBCAB        KCAB
024500950710     C                   MOVEL     *BLANKS       ABIIST
024600950710     C     KABI1         CHAIN     CNABI00F                           31
024700950710     C                   MOVEL     ABIIST        BAI
024800950710     C*  Tipo C/Ass.
024900950710     C                   MOVE      CSBTPI        ASS
025000950710     C                   MOVEL     CSBTPA        ASS
025100120215     c                   end
025200120215     c*
025300120215     C     NDC           IFEQ      *BLANKS
025400120215     C                   MOVEL     COST1         BAI
025500120215     C                   END
025600950710     C*  Dati Mittente
025700950710     C                   MOVE      CSBRMN        RMN
025800950710     C                   MOVE      CSBCDI        CDM
025900950714     C                   MOVE      CSBCDI        W8899             4 0
026000950710     C     W8899         IFEQ      8888
026100950710     C     W8899         OREQ      9999
026200950710     C                   Z-ADD     CSBAAS        KAAS
026300950710     C                   Z-ADD     CSBLNP        KLNP
026400950710     C                   Z-ADD     CSBNRS        KNRS
026500950710     C                   Z-ADD     CSBNSP        KNSP
026600950710     C                   MOVEL     *BLANKS       CSARSD
026700950710     C     KCSA          CHAIN     TNCSA01L                           31
026800950710     C                   MOVEL     CSARSD        RGM
026900950710     C                   ELSE
027000950710     C                   MOVEL     *BLANKS       ACORAG
027100980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
027200980127     C                   CLEAR                   TIBS69
027300980127     C                   CLEAR                   CNACO
027400980127     C                   CLEAR                   CNIND
027500980127     C                   CLEAR                   CNCLP
027600980127     C                   CLEAR                   FNCLS
027700980127     C                   MOVEL     ' '           I69TLA
027800980127     C                   MOVEL     KNSIF         I69SIF
027900980127     C                   Z-ADD     CSBCDI        I69KAC
028000980127     C                   CALL      'TIBS69R'
028100980127     C                   PARM                    TIBS69
028200980127     C                   PARM                    CNACO
028300980127     C                   PARM                    CNIND
028400980127     C                   PARM                    CNCLP
028500980127     C                   PARM                    FNCLS
028600980127     C     O69ERR        IFEQ      *BLANK
028700950710     C                   MOVEL     ACORAG        RGM
028800950710     C                   END
028900980127     C                   END
029000950710     C*  Destinatario
029100950710     C                   MOVEL     CSBRSD        RGD
029200950710     C*  Tipo Documento
029300950710     C     CSBFPC        IFEQ      '2'
029400950710     C                   MOVE      'BON.'        DOC
029500950710     C                   ELSE
029600950710     C     CSBBNA        IFEQ      9999999
029700950710     C                   MOVE      'LETT'        DOC
029800950710     C                   ELSE
029900950710     C     CSBBNA        IFNE      0
030000950710     C                   MOVE      'ASS.'        DOC
030100950710     C                   END
030200950710     C                   END
030300950710     C                   END
030400950710     C*  Dati pagamento
030500950710     C                   MOVE      CSBNDP        NDP
030600950710     C                   MOVE      CSBDDP        XDTAMG
030700950710     C                   Z-ADD     0             XDTGMA
030800950710     C                   Z-ADD     2             XDTSTA
030900950710     C                   CALL      'XDATA'       PLDAT
031000950710     C                   MOVE      XDTGMA        DDP
031100950710     C*  Decodifica banca pagamento
031200950710     C     CSBSTA        IFNE      9
031300960115     C     CSBDDP        IFNE      0
031400980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
031500980127     C                   CLEAR                   TIBS69
031600980127     C                   CLEAR                   CNACO
031700980127     C                   CLEAR                   CNIND
031800980127     C                   CLEAR                   CNCLP
031900980127     C                   CLEAR                   FNCLS
032000980127     C                   MOVEL     ' '           I69TLA
032100980127     C                   MOVEL     KNSIF         I69SIF
032200980127     C                   Z-ADD     KCB           I69KCC
032300980127     C                   Z-ADD     CSBBNA        I69KAC
032400950710     C                   MOVEL     *BLANKS       ACORAG
032500980127     C                   CALL      'TIBS69R'
032600980127     C                   PARM                    TIBS69
032700980127     C                   PARM                    CNACO
032800980127     C                   PARM                    CNIND
032900980127     C                   PARM                    CNCLP
033000980127     C                   PARM                    FNCLS
033100980127     C     O69ERR        IFEQ      *BLANK
033200950710     C                   MOVEL     ACORAG        BAP
033300980127     C                   END
033400960115     C                   ELSE
033500960115     C                   MOVEL     *BLANKS       BAP
033600960115     C                   END
033700950710     C                   ELSE
033800950710     C                   MOVEL     COST2         BAP
033900950710     C                   END
034000950710     C*  Dati giacenzao
034100950710     C                   MOVE      CSBFGC        FGC
034200950710     C                   MOVE      CSBNGC        NGC
034300950710     C                   MOVE      CSBDGC        XDTAMG
034400950710     C                   Z-ADD     0             XDTGMA
034500950710     C                   Z-ADD     2             XDTSTA
034600950710     C                   CALL      'XDATA'       PLDAT
034700950710     C                   MOVE      XDTGMA        DGC
034800120220     c                   if        %subst(csbnra: 10: 1)=' '
034900950710     C   01              WRITE     TES00
035000950710     C   01              SETOFF                                       01
035100950710     C                   WRITE     DET00
035200120215     c                   else
035300120215     c                   exsr      srass
035400120215     C                   END
035500950710     C* Somma nel totale corretto
035600950710     C     CSBSTA        IFNE      9
035700960418     C     D40TCR        OREQ      'A'
035800950710     C     VCA           IFNE      *BLANKS
035900950710     C                   Z-ADD     1             XX
036000950710     C     VCA           LOOKUP    CCV(XX)                                32
036100950710     C                   END
036200950710     C*
036300950710     C     VCA           IFEQ      *BLANKS
036400011001     C     VCA           OREQ      §GEDCN
036500950710     C                   ADD       CSBCAS        TOT
036600000107     C                   ELSE
036700000107     C     DVA(XX)       IFEQ      'S'
036800000107     C     *IN32         ANDEQ     '1'
036900000107     C                   ADD       CSBCAS        TOTE
037000950710     C                   ELSE
037100000107     C*
037200010615     C* Per convertire gli importi in valute estere utilizzo
037300010615     C* la routine YEURCO in quanto il cambio indicato sul
037400010615     C* file dei C/assegni di filiale non è affidabile
037500010615     C                   CLEAR                   YEURDS
037600010615     C                   MOVEL     VCA           YECDVI
037700010615     C                   Z-ADD     CSBCAS        YECIMI
037800011001     C                   MOVEL     §GEDCN        YECDVO
037900010615     C* passo al pgm io il nr. dei decimali della divisa corrente
038000010615     C* in base a quanto memorizzato sulla tabella CV
038100010615     C                   MOVE      NRDEC         YECDCO
038200010615     C                   MOVEL     'H'           YECTAR
038300010615     C                   CALL      'YEURCO'
038400010615     C                   PARM                    YEURDS
038500010615     C                   Z-ADD     YECIMO        IMPV             13 3
038600950710     C                   ADD       IMPV          TOTV
038700971222     C                   END
038800000107     C                   END
038900950710     C                   END
039000950710     C                   END
039100950710     C*
039200950706     C                   END
039300950720     C                   END
039301140325     C                   END
039400950720     C                   END
039500950706     C*
039600950706     C     KRMN          READE     TNCSB06L                               23
039700950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
039800950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
039900950706     C*
040000950706     C                   ENDSR
040100120215      *---------------------------------------------------------------*
040200120215      *  stampa rif assegni
040300120215      *---------------------------------------------------------------*
040400120215     C     srass         BEGSR
040500120215     c                   setoff                                       44
040600120215     c     csbnra        setll     tncsm03l
040700120215     c                   do        *hival
040800120215     c     csbnra        reade     tncsm03l
040900120215     c                   if        %eof(tncsm03l)
041000120215     c                   leave
041100120215     c                   end
041200120215     C                   MOVE      CSmNRA        NDC
041300120215     C                   MOVE      CSmDTE        XDTAMG
041400120215     C                   Z-ADD     0             XDTGMA
041500120215     C                   Z-ADD     2             XDTSTA
041600120215     C                   CALL      'XDATA'       PLDAT
041700120215     C                   MOVE      XDTGMA        DDC
041800120215     C                   MOVE      CSmABI        kABI
041900120215     C                   MOVE      CSmCAB        kCAB
042000120215     C                   MOVEL     *BLANKS       BAI
042100120215     C     Kabi1         CHAIN     CNABI00F                           81
042200120215     C  N81              MOVEL     ABIIST        BAI
042300120215     C                   MOVEL     CSmTPA        ASS
042400120215     C                   MOVE      CSmTPI        ASS
042500120215     C   01              WRITE     TES00
042600120215     C   01              SETOFF                                       01
042700120215     C   01              SETOFF                                       44
042800120215     C                   WRITE     DET00
042900120215     c                   seton                                        44
043000120215     c                   enddo
043100120215     C*
043200120215     C                   ENDSR
043300931011      *---------------------------------------------------------------*
043400941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
043500931011      *---------------------------------------------------------------*
043600941102     C     *INZSR        BEGSR
043700931011      *---------------------------------------------------------------*
043800931011     C     *ENTRY        PLIST
043900931011     C                   PARM                    KPJBA
044000950706     C                   MOVEL     KPJBU         TNSC40
044100950710     C* PLIST per richiamo Routine XDATA
044200950710     C     PLDAT         PLIST
044300950710     C                   PARM                    XDTAMG            6 0
044400950710     C                   PARM                    XDTGMA            6 0
044500950710     C                   PARM                    XDTSTA            1 0
044600931012     C*
044700931011     C                   Z-ADD     1             CODUT
044800931011     C                   CALL      'X§PARUT'
044900931011     C                   PARM                    UT§DSE
045000931011     C*          CODUT     CABEQ-1        FINE
045100931011     C                   MOVEL     RAGUT         RSUT             20
045200931011     C                   MOVEL     REC80         CNCR80
045300931011     C                   MOVEL     KNMUS         §SEI              6
045400950706     C*
045500950706     C                   Z-ADD     1             X                 3 0
045600950710     C                   Z-ADD     0             KCB
045700950706      *  Ricerco capoconto clienti
045800950706     C     X             DOWLE     50
045900950710     C     KCB           ANDEQ     0
046000950706     C                   MOVE      TCU(X)        TCUDS
046100950706     C     F56           IFEQ      'CG'
046200950710     C     F34           IFEQ      '06'
046300950710     C                   Z-ADD     KCU(X)        KCB               4 0
046400950710     C                   END
046500950710     C                   END
046600950706     C                   ADD       1             X
046700950706     C                   END
046800950706     C*----------------------------------------------------*
046900950706     C*  Definizione CHIAVI
047000950706     C     KCSB1         KLIST
047100950706     C                   KFLD                    KRMN
047200950706     C                   KFLD                    KAAS
047300950706     C                   KFLD                    KMGS
047400950706     C     KCSB2         KLIST
047500950706     C                   KFLD                    KRMN
047600950706     C                   KFLD                    KAAS
047700950706     C                   KFLD                    KMGS
047800950706     C                   KFLD                    KLNP
047900950710     C     KTAB          KLIST
048000950710     C                   KFLD                    KKUT
048100950710     C                   KFLD                    KCOD
048200950706     C     KCSA          KLIST
048300950706     C                   KFLD                    KAAS
048400950706     C                   KFLD                    KLNP
048500950706     C                   KFLD                    KNRS
048600950706     C                   KFLD                    KNSP
048700950710     C     KABI1         KLIST
048800950710     C                   KFLD                    KABI
048900950710     C                   KFLD                    KCAB
049000941102     C                   Z-ADD     1             KKUT
049100931012     C*----------------------------------------------------*
049200941102     C*  Definizione variabili
049300950706     C     *LIKE         DEFINE    CSBRMN        KRMN
049400950706     C     *LIKE         DEFINE    CSBAAS        KAAS
049500950706     C     *LIKE         DEFINE    CSBMGS        KMGS
049600950706     C     *LIKE         DEFINE    CSALNP        KLNP
049700950706     C     *LIKE         DEFINE    CSANRS        KNRS
049800950706     C     *LIKE         DEFINE    CSANSP        KNSP
049900980127     C     *LIKE         DEFINE    TBLKUT        KKUT
050000950710     C     *LIKE         DEFINE    TBLCOD        KCOD
050100950710     C     *LIKE         DEFINE    ABIABI        KABI
050200950710     C     *LIKE         DEFINE    ABICAB        KCAB
050300941102     C*----------------------------------------------------*
050400950706     C*  Inizializzazione delle variabii
050500950710     C                   MOVEL     NOMPG         NOMPGM
050600950710     C                   MOVE      D40INT        S42SEL
050700950710     C                   MOVE      D40AAS        S42AAS
050800950710     C                   MOVE      D40LNP        S42LNP
050900950710     C                   MOVE      D40NRS        S42NRS
051000950710     C                   MOVE      D40NRM        S42NRM
051100950710     C                   MOVE      D40LP1        S42LP1
051200950710     C                   MOVE      D40SD1        G02INV
051300950710     C                   Z-ADD     0             G02DAT
051400950710     C                   MOVEL     '3'           G02ERR
051500950710     C                   CALL      'XSRDA8'
051600950710     C                   PARM                    WLBDA8
051700950710     C                   MOVE      G02DAT        S42SD1
051800950710     C                   MOVE      D40SA1        G02INV
051900950710     C                   Z-ADD     0             G02DAT
052000950710     C                   MOVEL     '3'           G02ERR
052100950710     C                   CALL      'XSRDA8'
052200950710     C                   PARM                    WLBDA8
052300950710     C                   MOVE      G02DAT        S42SA1
052400950710     C                   MOVE      D40SDD        G02INV
052500950710     C                   Z-ADD     0             G02DAT
052600950710     C                   MOVEL     '3'           G02ERR
052700950710     C                   CALL      'XSRDA8'
052800950710     C                   PARM                    WLBDA8
052900950710     C                   MOVE      G02DAT        S42SDD
053000950710     C                   MOVE      D40SDA        G02INV
053100950710     C                   Z-ADD     0             G02DAT
053200950710     C                   MOVEL     '3'           G02ERR
053300950710     C                   CALL      'XSRDA8'
053400950710     C                   PARM                    WLBDA8
053500950710     C                   MOVE      G02DAT        S42SDA
053600950710     C                   MOVE      D40SCD        S42SCD
053700950710     C                   MOVE      D40PDD        G02INV
053800950710     C                   Z-ADD     0             G02DAT
053900950710     C                   MOVEL     '3'           G02ERR
054000950710     C                   CALL      'XSRDA8'
054100950710     C                   PARM                    WLBDA8
054200950710     C                   MOVE      G02DAT        S42PDD
054300950710     C                   MOVE      D40PDA        G02INV
054400950710     C                   Z-ADD     0             G02DAT
054500950710     C                   MOVEL     '3'           G02ERR
054600950710     C                   CALL      'XSRDA8'
054700950710     C                   PARM                    WLBDA8
054800950710     C                   MOVE      G02DAT        S42PDA
054900950710     C                   MOVE      D40PND        S42PND
055000950710     C                   MOVE      D40PTN        S42PTI
055100950710     C                   MOVE      D40PCD        S42PCD
055200950710     C                   MOVE      D40TCR        S42TCA
055300950710     C                   MOVE      D40NDT        S42NDT
055400950710     C                   MOVE      D40RGP        S42RGP
055500950710     C                   MOVE      D40DDT        G02INV
055600950710     C                   Z-ADD     0             G02DAT
055700950710     C                   MOVEL     '3'           G02ERR
055800950710     C                   CALL      'XSRDA8'
055900950710     C                   PARM                    WLBDA8
056000950710     C                   MOVE      G02DAT        S42DDC
056100950710     C                   MOVE      D40ABI        S42ABI
056200950710     C                   MOVE      D40CAB        S42CAB
056300950710     C                   MOVE      D40NAS        S42NRA
056301140325     C                   MOVE      D40imp        S42imp
056400950710     C*
056500950710     C     S42TCA        IFEQ      ' '
056600950710     C                   MOVEL     'TUTTI'       S42DCA
056700950710     C                   END
056800950710     C     S42TCA        IFEQ      'I'
056900950710     C                   MOVEL     'INCASSAT'    S42DCA
057000950710     C                   MOVE      'I      '     S42DCA
057100950710     C                   END
057200950710     C     S42TCA        IFEQ      'P'
057300950710     C                   MOVEL     'PAGATI  '    S42DCA
057400950710     C                   END
057500950710     C     S42TCA        IFEQ      'G'
057600950710     C                   MOVEL     'GIACENTI'    S42DCA
057700950710     C                   END
057800950710     C     S42TCA        IFEQ      'S'
057900950710     C                   MOVEL     'SOSPESI '    S42DCA
058000950710     C                   END
058100950710     C     S42TCA        IFEQ      'D'
058200950710     C                   MOVEL     'DA INCAS'    S42DCA
058300950710     C                   MOVE      'SARE   '     S42DCA
058400950710     C                   END
058500950710     C     S42TCA        IFEQ      'A'
058600950710     C                   MOVEL     'ANNULLAT'    S42DCA
058700950710     C                   MOVE      'I      '     S42DCA
058800950710     C                   END
058900950710     C*
059000950710     C                   SETON                                        02
059100950710     C                   MOVEL     *ALL'-'       CMP198
059200950710     C                   MOVEL     *ALL'-'       CM1198
059300950710     C                   WRITE     PAG00
059400950710     C*
059500950710     C                   SETON                                        01
059600950706     C                   Z-ADD     D40NRM        KRMN
059700950706     C                   Z-ADD     D40SD1        KMGS
059800950706     C                   MOVEL     D40SD1        KAAS
059900950706     C                   MOVE      D40LP1        KLNP
060000931014     C                   SETON                                        02
060100950710     C*----------------------------------------------------*
060200000107     C*  RECUPERO DIVISA MONETA CORRENTE
060300000107     C                   CLEAR                   DGED
060400000107     C                   CLEAR                   DSBS02
060500000107     C                   MOVEL     'C'           T02MOD
060600000107     C                   MOVEL     KNSIF         T02SIF
060700000107     C                   MOVEL     'GED'         T02COD
060800000107     C                   MOVEL     '1'           T02KE1
060900000107     C                   CALL      'TIBS02R'
061000000107     C                   PARM                    KPJBA
061100000107     C                   PARM                    DSBS02
061200000107     C*
061300000107    3C     T02ERR        IFEQ      ' '
061400000107     C                   MOVEL     T02UNI        DGED
061500011001     C                   MOVEL     §GEDCN        DIVCOR
061600000107    3C                   ENDIF
061700000107     C*----------------------------------------------------*
061800000107     C* Carico tabella cambi/divise
061900000107     C                   Z-ADD     0             XX                3 0
062000000107     C                   MOVEL     'CV'          KCOD
062100000107     C     KTAB          CHAIN     TABEL                              31
062200000107     C     *IN31         DOWEQ     '0'
062300000107     C     XX            ANDLT     100
062400000107     C                   MOVEL     TBLKEY        WDIV              3
062500000107     C     TBLFLG        IFEQ      ' '
062600000107     C                   ADD       1             XX
062700000107     C                   MOVEL     TBLKEY        CCV(XX)
062800000107     C                   MOVEL     TBLUNI        DSCV
062900000107     C                   MOVEL     §CVDVA        DVA(XX)
063000011001     C******               MOVEL§CVCMB    CMB,XX
063100000107     C     §CVDVA        IFEQ      'S'
063200011001     C     WDIV          ANDNE     §GEDCN
063300000107     C                   MOVEL     WDIV          DIVALT
063400000107     C                   END
063500000107     C* Memorizzo nr. decimali della divisa corrente
063600011001     C     WDIV          IFEQ      §GEDCN
063700000107     C                   Z-ADD     §CVDEC        NRDEC             2 0
063800000107     C                   END
063900000107     C                   END
064000000107     C     KTAB          READE     TABEL                                  31
064100000107     C                   END
064200010223     C*F                   CLEARTRUL15
064300931012     C*----------------------------------------------------*
064400931011     C                   ENDSR
