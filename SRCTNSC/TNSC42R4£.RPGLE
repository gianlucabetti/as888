000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300950710      * TRAS42R4                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600950710      *        STAMPA CONTRASSEGNI SELEZ.DATI PAGAMENTO              *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000950710     FTABEL00F  IF   E           K DISK
001100950710     FCNABI00F  IF   E           K DISK
001200950706     FTNCSA01L  IF   E           K DISK
001300950710     FTNCSB04L  IF   E           K DISK
001400120215     FTNCSm03L  IF   E           K DISK
001500950710     FTNSC42P   O    E             PRINTER OFLIND(*IN01)
001600950706?     *--------------------------------------------------------------*
001700950706?     *  SCHIERE                                                     *
001800950706?     *--------------------------------------------------------------*
001900000110     D CCV             S              3    DIM(100)
002000000110     D DVA             S              1    DIM(100)
002100931011?     *--------------------------------------------------------------*
002200931011?     *  DS                                                          *
002300931011?     *--------------------------------------------------------------*
002400931011     D KPJBA         E DS
002500010615     D YEURDS        E DS                  EXTNAME(YEURCODS)
002600000110     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002700000110     D DGED          E DS
002800931011      *
002900950706     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003000950706     D  TCU                  398    697
003100950706     D                                     DIM(50)
003200950706     D  KCU                  698    847P 0
003300950706     D                                     DIM(50)
003400950706     D                                     PACKEVEN
003500950706     D CNCR80        E DS
003600950710     D DSCV          E DS
003700950706     D*
003800950706     D TCUDS           DS
003900950706     D  F34                    3      4
004000950706     D  F56                    5      6
004100950706     D*
004200941102     D WLBDA8          DS
004300941102     D  G02DAT                 1      8  0
004400941102     D  G02INV                 9     16  0
004500941102     D  G02ERR                17     17
004600941102     D  G02TGI                18     22  0
004700980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
004800980127     D CNACO         E DS                  EXTNAME(CNACO00F)
004900980127     D CNIND         E DS                  EXTNAME(CNIND00F)
005000980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
005100980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
005200950706     D TNSC40        E DS                  EXTNAME(TNSC40DS)
005300931011      *
005400931011     D                SDS
005500931011     D  NOMPG                  1     10
005600950710     D*
005700950710     D COST1           C                   CONST('CONTANTE            ')
005800950710     D COST2           C                   CONST('ANNULLATO           ')
005900120215     D*
006000120215     d mnra            s                   like(csmnra)
006100120215     d mabi            s                   like(csmabi)
006200120215     d mcab            s                   like(csmcab)
006300120215      *
006400931011      *---------------------------------------------------------------*
006500931011      *   C I C L O      P R I N C I P A L E                          *
006600931011      *---------------------------------------------------------------*
006700950710     C*  Posizionamento iniziale x stampa
006800950710     C     KCSB2         SETLL     TNCSB04L
006900950710     C                   READ      TNCSB04L                               23
007000931011     C*
007100931011     C*  LOOP
007200950710     C     *IN23         DOWEQ     '0'
007300950710     C     CSBDDP        ANDLE     D40PDA
007400950706     C* Se ho selezionato una sola lnp. gestisco posiz. su quella
007500950710     C     D40PND        IFNE      0
007600950710     C     D40PCD        IFEQ      0
007700950710     C                   EXSR      REDDOC
007800950710     C                   ELSE
007900950710     C*  altrimenti x documento - cliente
008000950710     C                   EXSR      REDCLI
008100950710     C                   END
008200950710     C                   ELSE
008300950707     C                   EXSR      GESREC
008400950706     C                   END
008500931011     C                   END
008600950710     C*
008700971223     C* Totali
008800950710     C     TOT           IFGT      0
008900950710     C   01              WRITE     TES00
009000950710     C   01              SETOFF                                       01
009100950710     C                   WRITE     TOT00
009200950710     C                   END
009300000110     C*
009400000110     C     TOTE          IFGT      0
009500000110     C   01              WRITE     TES00
009600000110     C   01              SETOFF                                       01
009700000110     C                   WRITE     TOTE0
009800000110     C                   END
009900950710     C*
010000950710     C     TOTV          IFGT      0
010100950710     C   01              WRITE     TES00
010200950710     C   01              SETOFF                                       01
010300950710     C                   WRITE     TOTV0
010400950710     C                   END
010500990615     C* FINE PGM
010600980127     C                   CLEAR                   TIBS69
010700980127     C                   MOVEL     'C'           I69TLA
010800980127     C                   CALL      'TIBS69R'
010900980127     C                   PARM                    TIBS69
011000980127     C                   PARM                    CNACO
011100980127     C                   PARM                    CNIND
011200980127     C                   PARM                    CNCLP
011300980127     C                   PARM                    FNCLS
011400950710     C*
011500950710     C                   SETON                                        LR
011600950710     C*----------------------------------------------------*
011700950710     C*   REDDOC: Lettura con selez. per DOCUMENTO         *
011800950710     C*----------------------------------------------------*
011900950710     C     REDDOC        BEGSR
012000950710     C*
012100950710     C     CSBNDP        IFLT      D40PND
012200950710     C     KCSB2         SETLL     TNCSB04L
012300950710     C                   READ      TNCSB04L                               23
012400950710     C                   Z-ADD     CSBDDP        KDDP
012500950710     C                   END
012600950710     C*  Leggo lnp.
012700950710     C     *IN23         DOWEQ     '0'
012800950710     C     CSBNDP        ANDEQ     D40PND
012900950710     C     CSBDDP        ANDLE     D40PDA
013000950710     C                   EXSR      GESREC
013100950710     C                   END
013200950710     C*
013300950711     C                   Z-ADD     CSBDDP        KDDP
013400950710     C     CSBNDP        IFGT      D40PND
013500950710     C     KCSB1         SETGT     TNCSB04L
013600950710     C                   READ      TNCSB04L                               23
013700950710     C                   Z-ADD     CSBDDP        KDDP
013800950710     C                   END
013900950710     C*
014000950710     C                   ENDSR
014100950710     C*----------------------------------------------------*
014200950710     C*   REDCLI: Lettura con selez. per CLIENTE           *
014300950710     C*----------------------------------------------------*
014400950710     C     REDCLI        BEGSR
014500950710     C*
014600950710     C     CSBNDP        IFLT      D40PND
014700950710     C     CSBNDP        OREQ      D40PND
014800950710     C     CSBCDI        ANDLT     D40PCD
014900950710     C     KCSB2         SETLL     TNCSB04L
015000950710     C                   READ      TNCSB04L                               23
015100950710     C                   Z-ADD     CSBDDP        KDDP
015200950710     C                   END
015300950710     C*  Leggo lnp.
015400950710     C     *IN23         DOWEQ     '0'
015500950710     C     CSBCDI        ANDEQ     D40PCD
015600950710     C     CSBNDP        ANDEQ     D40PND
015700950710     C     CSBDDP        ANDLE     D40PDA
015800950710     C                   EXSR      GESREC
015900950710     C                   END
016000950710     C*
016100950711     C                   Z-ADD     CSBDDP        KDDP
016200950710     C     CSBNDP        IFGT      D40PND
016300950710     C     CSBNDP        OREQ      D40PND
016400950710     C     CSBCDI        ANDGT     D40PCD
016500950710     C     KCSB1         SETGT     TNCSB04L
016600950710     C                   READ      TNCSB04L                               23
016700950710     C                   Z-ADD     CSBDDP        KDDP
016800950710     C                   END
016900950710     C*
017000950710     C                   ENDSR
017100120215     C*----------------------------------------------------*
017200120215     C*   srmulti - per multiassegni
017300120215     C*----------------------------------------------------*
017400120215     C     srmulti       BEGSR
017500120215     C*
017600120215     c                   eval      mnra = *hival
017700120215     c                   eval      mabi = *hival
017800120215     c                   eval      mcab = *hival
017900120215     c     csbnra        setll     tncsm03l
018000120215     c                   do        *hival
018100120215     c     csbnra        reade     tncsm03l
018200120215     c                   if        %eof(tncsm03l)
018300120215     c                   leave
018400120215     c                   end
018500120215     C                   if        csmnra = WNRA
018600120215     c                   eval      mnra = csmnra
018700120215     c                   eval      mabi = csmabi
018800120215     c                   eval      mcab = csmcab
018900120215     c                   leave
019000120215     c                   end
019100120215     c                   enddo
019200120215     C*
019300120215     C                   ENDSR
019400950706     C*----------------------------------------------------*
019500950710     C*   GESREC: Stampa contassegni selezionati           *
019600950706     C*----------------------------------------------------*
019700950706     C     GESREC        BEGSR
019800950706     C*----------------------------------------------------*
019900950706     C* Se il documento è valido controllo se il record rientra
020000950706     C* nei limiti richiesti
020100950706     C                   MOVEL     D40NAS        WNRA             10
020200120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
020300120220     c                             csbtpi = 'M' and csbabi = 0
020400120215     c                   exsr      srmulti
020500120215     c                   else
020600120215     c                   eval      mnra = csbnra
020700120215     c                   eval      mabi = csbabi
020800120215     c                   eval      mcab = csbcab
020900120215     c                   end
021000950706     C*----------------------------------------------------*
021100950706     C     D40ABI        IFEQ      *ZEROS
021200120215     C     D40ABI        OREQ      mABI
021300950720     C     D40CAB        IFEQ      *ZEROS
021400120215     C     D40CAB        OREQ      mCAB
021500950720     C     WNRA          IFEQ      *BLANKS
021600950720     C     WNRA          OREQ      *ZEROS
021700120215     C     WNRA          OREQ      mNRA
021800950706     C*----------------------------------------------------*
021900950710     C*  Test  x tipo intestazione
022000950710     C     D40PTN        IFEQ      *BLANKS
022100950710     C     D40PTN        OREQ      'M'
022200950710     C     CSBTPI        ANDEQ     'M'
022300950710     C     D40PTN        OREQ      'B'
022400950710     C     CSBTPI        ANDEQ     ' '
022500000110     C*  Test  x DIVISA
022600000110     C     D40PDV        IFEQ      *BLANKS
022700000110     C     D40PDV        OREQ      CSBVCA
022800011001     C     D40PDV        OREQ      §GEDCN
022900000110     C     CSBVCA        ANDEQ     *BLANKS
023000971223     C*----------------------------------------------------*
023100971223     C* Selezione per stato C/Assegno
023200971223     C     D40TCR        IFEQ      'T'
023300971223     C     CSBSTA        ANDEQ     6
023400971223     C     D40TCR        OREQ      'R'
023500971223     C     CSBSTA        ANDEQ     7
023600971223     C     D40TCR        OREQ      'P'
023700971223     C     CSBSTA        ANDEQ     0
023800950710     C*  Test  x codice
023900950710     C     D40PCD        IFEQ      0
024000950710     C     D40PCD        OREQ      CSBCDI
024100950710     C*  Spedizione
024200950710     C                   MOVE      CSBAAS        AAS
024300950706     C                   MOVEL     CSBMGS        MM                2 0
024400950706     C                   MOVE      CSBMGS        GG                2 0
024500950710     C                   MOVEL     GG            DSP
024600950710     C                   MOVE      MM            DSP
024700950710     C                   MOVE      CSBLNA        LNA
024800950710     C                   MOVE      CSBLNP        LNP
024900950710     C                   MOVE      CSBNRS        NRS
025000950710     C                   MOVE      CSBNSP        NSP
025100950710     C*  C/Assegno
025200950710     C                   MOVE      CSBVCA        VCA
025300950710     C                   Z-ADD     CSBCAS        IMP
025400120215     C                   MOVEL     *BLANKS       BAI
025500950710     C*  Dati Incasso
025600120220     c                   if        %subst(csbnra: 10: 1)=' '
025700120215     c                   setoff                                       44
025800950710     C                   MOVE      CSBNRA        NDC
025900950710     C                   MOVE      CSBDTE        XDTAMG
026000950710     C                   Z-ADD     0             XDTGMA
026100950710     C                   Z-ADD     2             XDTSTA
026200950710     C                   CALL      'XDATA'       PLDAT
026300950710     C                   MOVE      XDTGMA        DDC
026400950710     C*  Decodifica banca incasso
026500950710     C                   Z-ADD     CSBABI        KABI
026600950710     C                   Z-ADD     CSBCAB        KCAB
026700950710     C                   MOVEL     *BLANKS       ABIIST
026800950710     C     KABI1         CHAIN     CNABI00F                           31
026900950710     C                   MOVEL     ABIIST        BAI
027000950710     C*  Tipo C/Ass.
027100950710     C                   MOVE      CSBTPI        ASS
027200950710     C                   MOVEL     CSBTPA        ASS
027300120215     c                   end
027400120215     c*
027500120215     C     NDC           IFEQ      *BLANKS
027600120215     C                   MOVEL     COST1         BAI
027700120215     C                   END
027800950710     C*  Dati Mittente
027900950710     C                   MOVE      CSBRMN        RMN
028000950710     C                   MOVE      CSBCDI        CDM
028100950714     C                   MOVE      CSBCDI        W8899             4 0
028200950710     C     W8899         IFEQ      8888
028300950710     C     W8899         OREQ      9999
028400950710     C                   Z-ADD     CSBAAS        KAAS
028500950710     C                   Z-ADD     CSBLNP        KLNP
028600950710     C                   Z-ADD     CSBNRS        KNRS
028700950710     C                   Z-ADD     CSBNSP        KNSP
028800950710     C                   MOVEL     *BLANKS       CSARSD
028900950710     C     KCSA          CHAIN     TNCSA01L                           31
029000950710     C                   MOVEL     CSARSD        RGM
029100950710     C                   ELSE
029200950710     C                   MOVEL     *BLANKS       ACORAG
029300980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
029400980127     C                   CLEAR                   TIBS69
029500980127     C                   CLEAR                   CNACO
029600980127     C                   CLEAR                   CNIND
029700980127     C                   CLEAR                   CNCLP
029800980127     C                   CLEAR                   FNCLS
029900980127     C                   MOVEL     ' '           I69TLA
030000980127     C                   MOVEL     KNSIF         I69SIF
030100980127     C                   Z-ADD     CSBCDI        I69KAC
030200980127     C                   CALL      'TIBS69R'
030300980127     C                   PARM                    TIBS69
030400980127     C                   PARM                    CNACO
030500980127     C                   PARM                    CNIND
030600980127     C                   PARM                    CNCLP
030700980127     C                   PARM                    FNCLS
030800980127     C     O69ERR        IFEQ      *BLANK
030900950710     C                   MOVEL     ACORAG        RGM
031000950710     C                   END
031100980127     C                   END
031200950710     C*  Destinatario
031300950710     C                   MOVEL     CSBRSD        RGD
031400950710     C*  Tipo Documento
031500950710     C     CSBFPC        IFEQ      '2'
031600950710     C                   MOVE      'BON.'        DOC
031700950710     C                   ELSE
031800950710     C     CSBBNA        IFEQ      9999999
031900950710     C                   MOVE      'LETT'        DOC
032000950710     C                   ELSE
032100950710     C     CSBBNA        IFNE      0
032200950710     C                   MOVE      'ASS.'        DOC
032300950710     C                   END
032400950710     C                   END
032500950710     C                   END
032600950710     C*  Dati pagamento
032700950710     C                   MOVE      CSBNDP        NDP
032800950710     C                   MOVE      CSBDDP        XDTAMG
032900950710     C                   Z-ADD     0             XDTGMA
033000950710     C                   Z-ADD     2             XDTSTA
033100950710     C                   CALL      'XDATA'       PLDAT
033200950710     C                   MOVE      XDTGMA        DDP
033300950710     C*  Decodifica banca pagamento
033400950710     C     CSBSTA        IFNE      9
033500960115     C     CSBDDP        IFNE      0
033600980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
033700980127     C                   CLEAR                   TIBS69
033800980127     C                   CLEAR                   CNACO
033900980127     C                   CLEAR                   CNIND
034000980127     C                   CLEAR                   CNCLP
034100980127     C                   CLEAR                   FNCLS
034200980127     C                   MOVEL     ' '           I69TLA
034300980127     C                   MOVEL     KNSIF         I69SIF
034400980127     C                   Z-ADD     KCB           I69KCC
034500980127     C                   Z-ADD     CSBBNA        I69KAC
034600980127     C                   MOVEL     *BLANKS       ACORAG
034700980127     C                   CALL      'TIBS69R'
034800980127     C                   PARM                    TIBS69
034900980127     C                   PARM                    CNACO
035000980127     C                   PARM                    CNIND
035100980127     C                   PARM                    CNCLP
035200980127     C                   PARM                    FNCLS
035300980127     C     O69ERR        IFEQ      *BLANK
035400950710     C                   MOVEL     ACORAG        BAP
035500980127     C                   END
035600960115     C                   ELSE
035700960115     C                   MOVEL     *BLANKS       BAP
035800960115     C                   END
035900950710     C                   ELSE
036000950710     C                   MOVEL     COST2         BAP
036100950710     C                   END
036200950710     C*  Dati giacenzao
036300950710     C                   MOVE      CSBFGC        FGC
036400950710     C                   MOVE      CSBNGC        NGC
036500950710     C                   MOVE      CSBDGC        XDTAMG
036600950710     C                   Z-ADD     0             XDTGMA
036700950710     C                   Z-ADD     2             XDTSTA
036800950710     C                   CALL      'XDATA'       PLDAT
036900950710     C                   MOVE      XDTGMA        DGC
037000120220     c                   if        %subst(csbnra: 10: 1)=' '
037100950710     C   01              WRITE     TES00
037200950710     C   01              SETOFF                                       01
037300950710     C                   WRITE     DET00
037400120215     c                   else
037500120215     c                   exsr      srass
037600120215     C                   END
037700950710     C* Somma nel totale corretto
037800950710     C     CSBSTA        IFNE      9
037900950710     C     VCA           IFNE      *BLANKS
038000950710     C                   Z-ADD     1             XX
038100950710     C     VCA           LOOKUP    CCV(XX)                                32
038200950710     C                   END
038300950710     C*
038400950710     C     VCA           IFEQ      *BLANKS
038500011001     C     VCA           OREQ      §GEDCN
038600950710     C                   ADD       CSBCAS        TOT
038700000110     C                   ELSE
038800010619     C     DVA(XX)       IFEQ      'S'
038900010619     C     *IN32         ANDEQ     '1'
039000010619     C                   ADD       CSBCAS        TOTE
039100010619     C                   ELSE
039200010615     C* Per convertire gli importi in valute estere utilizzo
039300010615     C* la routine YEURCO in quanto il cambio indicato sul
039400010615     C* file dei C/assegni di filiale non è affidabile
039500010615     C                   CLEAR                   YEURDS
039600010615     C                   MOVEL     VCA           YECDVI
039700010615     C                   Z-ADD     CSBCAS        YECIMI
039800011001     C                   MOVEL     §GEDCN        YECDVO
039900010615     C* passo al pgm io il nr. dei decimali della divisa corrente
040000010615     C* in base a quanto memorizzato sulla tabella CV
040100010615     C                   MOVE      NRDEC         YECDCO
040200010615     C                   MOVEL     'H'           YECTAR
040300010615     C                   CALL      'YEURCO'
040400010615     C                   PARM                    YEURDS
040500010615     C                   Z-ADD     YECIMO        IMPV             13 3
040600950710     C                   ADD       IMPV          TOTV
040700950710     C                   END
040800000110     C                   END
040900010619     C                   END
041000950710     C*
041100950710     C                   END
041200000110     C                   END
041300960911     C                   END
041400950710     C                   END
041500950706     C                   END
041600950720     C                   END
041700971223     C                   END
041800950706     C*
041900950710     C                   READ      TNCSB04L                               23
042000950706     C*
042100950706     C                   ENDSR
042200120215      *---------------------------------------------------------------*
042300120215      *  stampa rif assegni
042400120215      *---------------------------------------------------------------*
042500120215     C     srass         BEGSR
042600120215     c                   setoff                                       44
042700120215     c     csbnra        setll     tncsm03l
042800120215     c                   do        *hival
042900120215     c     csbnra        reade     tncsm03l
043000120215     c                   if        %eof(tncsm03l)
043100120215     c                   leave
043200120215     c                   end
043300120215     C                   MOVE      CSmNRA        NDC
043400120215     C                   MOVE      CSmDTE        XDTAMG
043500120215     C                   Z-ADD     0             XDTGMA
043600120215     C                   Z-ADD     2             XDTSTA
043700120215     C                   CALL      'XDATA'       PLDAT
043800120215     C                   MOVE      XDTGMA        DDC
043900120215     C                   MOVE      CSmABI        kABI
044000120215     C                   MOVE      CSmCAB        kCAB
044100120215     C                   MOVEL     *BLANKS       BAI
044200120215     C     Kabi1         CHAIN     CNABI00F                           81
044300120215     C  N81              MOVEL     ABIIST        BAI
044400120215     C                   MOVEL     CSmTPA        ASS
044500120215     C                   MOVE      CSmTPI        ASS
044600120215     C   01              WRITE     TES00
044700120215     C   01              SETOFF                                       01
044800120215     C   01              SETOFF                                       44
044900120215     C                   WRITE     DET00
045000120215     c                   seton                                        44
045100120215     c                   enddo
045200120215     C*
045300120215     C                   ENDSR
045400931011      *---------------------------------------------------------------*
045500941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
045600931011      *---------------------------------------------------------------*
045700941102     C     *INZSR        BEGSR
045800931011      *---------------------------------------------------------------*
045900931011     C     *ENTRY        PLIST
046000931011     C                   PARM                    KPJBA
046100950706     C                   MOVEL     KPJBU         TNSC40
046200950710     C* PLIST per richiamo Routine XDATA
046300950710     C     PLDAT         PLIST
046400950710     C                   PARM                    XDTAMG            6 0
046500950710     C                   PARM                    XDTGMA            6 0
046600950710     C                   PARM                    XDTSTA            1 0
046700931012     C*
046800931011     C                   Z-ADD     1             CODUT
046900931011     C                   CALL      'X§PARUT'
047000931011     C                   PARM                    UT§DSE
047100931011     C*          CODUT     CABEQ-1        FINE
047200931011     C                   MOVEL     RAGUT         RSUT             20
047300931011     C                   MOVEL     REC80         CNCR80
047400931011     C                   MOVEL     KNMUS         §SEI              6
047500950706     C*
047600950706     C                   Z-ADD     1             X                 3 0
047700950710     C                   Z-ADD     0             KCB
047800950706      *  Ricerco capoconto clienti
047900950706     C     X             DOWLE     50
048000950710     C     KCB           ANDEQ     0
048100950706     C                   MOVE      TCU(X)        TCUDS
048200950706     C     F56           IFEQ      'CG'
048300950710     C     F34           IFEQ      '06'
048400950710     C                   Z-ADD     KCU(X)        KCB               4 0
048500950710     C                   END
048600950710     C                   END
048700950706     C                   ADD       1             X
048800950706     C                   END
048900950706     C*----------------------------------------------------*
049000950706     C*  Definizione CHIAVI
049100950710     C     KCSB1         KLIST
049200950710     C                   KFLD                    KDDP
049300950710     C     KCSB2         KLIST
049400950710     C                   KFLD                    KDDP
049500950710     C                   KFLD                    KNDP
049600950710     C                   KFLD                    KCDI
049700950710     C     KTAB          KLIST
049800950710     C                   KFLD                    KKUT
049900950710     C                   KFLD                    KCOD
050000950706     C     KCSA          KLIST
050100950706     C                   KFLD                    KAAS
050200950706     C                   KFLD                    KLNP
050300950706     C                   KFLD                    KNRS
050400950706     C                   KFLD                    KNSP
050500950710     C     KABI1         KLIST
050600950710     C                   KFLD                    KABI
050700950710     C                   KFLD                    KCAB
050800971223     C     KCSB3         KLIST
050900971223     C                   KFLD                    KAAS
051000971223     C                   KFLD                    KLNP
051100971223     C                   KFLD                    KNRS
051200971223     C                   KFLD                    KNSP
051300941102     C                   Z-ADD     1             KKUT
051400931012     C*----------------------------------------------------*
051500941102     C*  Definizione variabili
051600950706     C     *LIKE         DEFINE    CSBAAS        KAAS
051700950710     C     *LIKE         DEFINE    CSBCDI        KCDI
051800950710     C     *LIKE         DEFINE    CSBDDP        KDDP
051900950710     C     *LIKE         DEFINE    CSBNDP        KNDP
052000950706     C     *LIKE         DEFINE    CSALNP        KLNP
052100950706     C     *LIKE         DEFINE    CSANRS        KNRS
052200950706     C     *LIKE         DEFINE    CSANSP        KNSP
052300980127     C     *LIKE         DEFINE    TBLKUT        KKUT
052400950710     C     *LIKE         DEFINE    TBLCOD        KCOD
052500950710     C     *LIKE         DEFINE    ABIABI        KABI
052600950710     C     *LIKE         DEFINE    ABICAB        KCAB
052700941102     C*----------------------------------------------------*
052800950706     C*  Inizializzazione delle variabii
052900950710     C                   MOVEL     NOMPG         NOMPGM
053000950710     C                   MOVE      D40INT        S42SEL
053100950710     C                   MOVE      D40AAS        S42AAS
053200950710     C                   MOVE      D40LNP        S42LNP
053300950710     C                   MOVE      D40NRS        S42NRS
053400950710     C                   MOVE      D40NRM        S42NRM
053500950710     C                   MOVE      D40LP1        S42LP1
053600950710     C                   MOVE      D40SD1        G02INV
053700950710     C                   Z-ADD     0             G02DAT
053800950710     C                   MOVEL     '3'           G02ERR
053900950710     C                   CALL      'XSRDA8'
054000950710     C                   PARM                    WLBDA8
054100950710     C                   MOVE      G02DAT        S42SD1
054200950710     C                   MOVE      D40SA1        G02INV
054300950710     C                   Z-ADD     0             G02DAT
054400950710     C                   MOVEL     '3'           G02ERR
054500950710     C                   CALL      'XSRDA8'
054600950710     C                   PARM                    WLBDA8
054700950710     C                   MOVE      G02DAT        S42SA1
054800950710     C                   MOVE      D40SDD        G02INV
054900950710     C                   Z-ADD     0             G02DAT
055000950710     C                   MOVEL     '3'           G02ERR
055100950710     C                   CALL      'XSRDA8'
055200950710     C                   PARM                    WLBDA8
055300950710     C                   MOVE      G02DAT        S42SDD
055400950710     C                   MOVE      D40SDA        G02INV
055500950710     C                   Z-ADD     0             G02DAT
055600950710     C                   MOVEL     '3'           G02ERR
055700950710     C                   CALL      'XSRDA8'
055800950710     C                   PARM                    WLBDA8
055900950710     C                   MOVE      G02DAT        S42SDA
056000950710     C                   MOVE      D40SCD        S42SCD
056100950710     C                   MOVE      D40PDD        G02INV
056200950710     C                   Z-ADD     0             G02DAT
056300950710     C                   MOVEL     '3'           G02ERR
056400950710     C                   CALL      'XSRDA8'
056500950710     C                   PARM                    WLBDA8
056600950710     C                   MOVE      G02DAT        S42PDD
056700950710     C                   MOVE      D40PDA        G02INV
056800950710     C                   Z-ADD     0             G02DAT
056900950710     C                   MOVEL     '3'           G02ERR
057000950710     C                   CALL      'XSRDA8'
057100950710     C                   PARM                    WLBDA8
057200950710     C                   MOVE      G02DAT        S42PDA
057300950710     C                   MOVE      D40PND        S42PND
057400950710     C                   MOVE      D40PTN        S42PTI
057500950710     C                   MOVE      D40PCD        S42PCD
057600950710     C                   MOVE      D40TCR        S42TCA
057700950710     C                   MOVE      D40NDT        S42NDT
057800950710     C                   MOVE      D40RGP        S42RGP
057900950710     C                   MOVE      D40DDT        G02INV
058000950710     C                   Z-ADD     0             G02DAT
058100950710     C                   MOVEL     '3'           G02ERR
058200950710     C                   CALL      'XSRDA8'
058300950710     C                   PARM                    WLBDA8
058400950710     C                   MOVE      G02DAT        S42DDC
058500950710     C                   MOVE      D40ABI        S42ABI
058600950710     C                   MOVE      D40CAB        S42CAB
058700950710     C                   MOVE      D40NAS        S42NRA
058800950710     C*
058900950710     C     S42TCA        IFEQ      ' '
059000950710     C                   MOVEL     'TUTTI'       S42DCA
059100950710     C                   END
059200950710     C     S42TCA        IFEQ      'I'
059300950710     C                   MOVEL     'INCASSAT'    S42DCA
059400950710     C                   MOVE      'I      '     S42DCA
059500950710     C                   END
059600950710     C     S42TCA        IFEQ      'P'
059700950710     C                   MOVEL     'PAGATI  '    S42DCA
059800950710     C                   END
059900950710     C     S42TCA        IFEQ      'G'
060000950710     C                   MOVEL     'GIACENTI'    S42DCA
060100950710     C                   END
060200950710     C     S42TCA        IFEQ      'S'
060300950710     C                   MOVEL     'SOSPESI '    S42DCA
060400950710     C                   END
060500950710     C     S42TCA        IFEQ      'D'
060600950710     C                   MOVEL     'DA INCAS'    S42DCA
060700950710     C                   MOVE      'SARE   '     S42DCA
060800950710     C                   END
060900950710     C     S42TCA        IFEQ      'A'
061000950710     C                   MOVEL     'ANNULLAT'    S42DCA
061100950710     C                   MOVE      'I      '     S42DCA
061200950710     C                   END
061300950710     C*
061400950710     C                   SETON                                        04
061500950710     C                   MOVEL     *ALL'-'       CMP198
061600950710     C                   MOVEL     *ALL'-'       CM1198
061700950710     C                   WRITE     PAG00
061800950710     C*
061900950710     C                   SETON                                        01
062000950710     C                   Z-ADD     D40PCD        KCDI
062100950710     C                   Z-ADD     D40PDD        KDDP
062200950710     C                   MOVEL     D40PND        KNDP
062300950710     C*----------------------------------------------------*
062400000110     C*  RECUPERO DIVISA MONETA CORRENTE
062500000110     C                   CLEAR                   DGED
062600000110     C                   CLEAR                   DSBS02
062700000110     C                   MOVEL     'C'           T02MOD
062800000110     C                   MOVEL     KNSIF         T02SIF
062900000110     C                   MOVEL     'GED'         T02COD
063000000110     C                   MOVEL     '1'           T02KE1
063100000110     C                   CALL      'TIBS02R'
063200000110     C                   PARM                    KPJBA
063300000110     C                   PARM                    DSBS02
063400000110     C*
063500000110    3C     T02ERR        IFEQ      ' '
063600000110     C                   MOVEL     T02UNI        DGED
063700011001     C                   MOVEL     §GEDCN        DIVCOR
063800000110    3C                   ENDIF
063900000110     C*----------------------------------------------------*
064000000110     C* Carico tabella cambi/divise
064100000110     C                   Z-ADD     0             XX                3 0
064200000110     C                   MOVEL     'CV'          KCOD
064300000110     C     KTAB          CHAIN     TABEL                              31
064400000110     C     *IN31         DOWEQ     '0'
064500000110     C     XX            ANDLT     100
064600000110     C                   MOVEL     TBLKEY        WDIV              3
064700000110     C     TBLFLG        IFEQ      ' '
064800000110     C                   ADD       1             XX
064900000110     C                   MOVEL     TBLKEY        CCV(XX)
065000000110     C                   MOVEL     TBLUNI        DSCV
065100000110     C                   MOVEL     §CVDVA        DVA(XX)
065200011001     C*****                MOVEL§CVCMB    CMB,XX
065300000110     C     §CVDVA        IFEQ      'S'
065400011001     C     WDIV          ANDNE     §GEDCN
065500000110     C                   MOVEL     WDIV          DIVALT
065600000110     C                   END
065700000110     C* Memorizzo nr. decimali della divisa corrente
065800011001     C     WDIV          IFEQ      §GEDCN
065900000110     C                   Z-ADD     §CVDEC        NRDEC             2 0
066000000110     C                   END
066100000110     C                   END
066200000110     C     KTAB          READE     TABEL                                  31
066300000110     C                   END
066400931012     C*----------------------------------------------------*
066500931011     C                   ENDSR
