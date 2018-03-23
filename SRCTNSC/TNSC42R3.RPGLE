000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300950710      * TRAS42R3                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600950710      *        STAMPA SELEZIONE DATE SPEDIZIONE/CLIENTE              *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000950710     FTABEL00F  IF   E           K DISK
001100950710     FCNABI00F  IF   E           K DISK
001200950706     FTNCSA01L  IF   E           K DISK
001300120215     FTNCSB05L  IF   E           K DISK
001400120215     FTNCSm03L  IF   E           K DISK
001500950710     FTNSC42P   O    E             PRINTER OFLIND(*IN01)
001600950706?     *--------------------------------------------------------------*
001700950706?     *  SCHIERE                                                     *
001800950706?     *--------------------------------------------------------------*
001900950710     D CCV             S              3    DIM(100)
002000000110     D DVA             S              1    DIM(100)
002100931011?     *--------------------------------------------------------------*
002200931011?     *  DS                                                          *
002300931011?     *--------------------------------------------------------------*
002400931011     D KPJBA         E DS
002500000110     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002600000110     D DGED          E DS
002700931011      *
002800010615     D YEURDS        E DS                  EXTNAME(YEURCODS)
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
006300931011      *---------------------------------------------------------------*
006400931011      *   C I C L O      P R I N C I P A L E                          *
006500931011      *---------------------------------------------------------------*
006600950710     C*  Posizionamento iniziale x stampa
006700950710     C     KCSB1         SETLL     TNCSB05L
006800950710     C                   READ      TNCSB05L                               23
006900950710     C                   Z-ADD     CSBMGS        WDTSPE            8 0
007000950710     C                   MOVEL     CSBAAS        WDTSPE            8 0
007100931011     C*
007200931011     C*  LOOP
007300950710     C     *IN23         DOWEQ     '0'
007400950710     C     WDTSPE        ANDLE     D40SDA
007500950706     C* Se ho selezionato una sola lnp. gestisco posiz. su quella
007600950710     C     D40SCD        IFEQ      0
007700950710     C                   EXSR      GESREC
007800950710     C                   ELSE
007900950710     C                   EXSR      REDCLI
008000950706     C                   END
008100931011     C                   END
008200950710     C*
008300950710     C     TOT           IFGT      0
008400950710     C   01              WRITE     TES00
008500950710     C   01              SETOFF                                       01
008600950710     C                   WRITE     TOT00
008700950710     C                   END
008800000110     C*
008900000110     C     TOTE          IFGT      0
009000000110     C   01              WRITE     TES00
009100000110     C   01              SETOFF                                       01
009200000110     C                   WRITE     TOTE0
009300000110     C                   END
009400950710     C*
009500950710     C     TOTV          IFGT      0
009600950710     C   01              WRITE     TES00
009700950710     C   01              SETOFF                                       01
009800950710     C                   WRITE     TOTV0
009900950710     C                   END
010000950710     C*
010100971222     C* RICHIAMO PGM X CHIUSURA IN LR
010200980127     C                   CLEAR                   TIBS69
010300980127     C                   MOVEL     'C'           I69TLA
010400980127     C                   CALL      'TIBS69R'
010500980127     C                   PARM                    TIBS69
010600980127     C                   PARM                    CNACO
010700980127     C                   PARM                    CNIND
010800980127     C                   PARM                    CNCLP
010900980127     C                   PARM                    FNCLS
011000971222     C*
011100971222      *
011200950710     C                   SETON                                        LR
011300950710     C*----------------------------------------------------*
011400950710     C*   REDCLI: Lettura con selez. per CLIENTE           *
011500950710     C*----------------------------------------------------*
011600950710     C     REDCLI        BEGSR
011700950710     C*
011800950710     C     CSBCDI        IFLT      D40SCD
011900950710     C     KCSB2         SETLL     TNCSB05L
012000950710     C                   READ      TNCSB05L                               23
012100950710     C                   Z-ADD     CSBMGS        WDTSPE            8 0
012200950710     C                   MOVEL     CSBAAS        WDTSPE            8 0
012300950710     C                   Z-ADD     CSBMGS        KMGS
012400950710     C                   Z-ADD     CSBAAS        KAAS
012500950710     C                   END
012600950710     C*  Leggo lnp.
012700950710     C     *IN23         DOWEQ     '0'
012800950710     C     CSBCDI        ANDEQ     D40SCD
012900950710     C     WDTSPE        ANDLE     D40SDA
013000950710     C                   EXSR      GESREC
013100950710     C                   END
013200950710     C*
013300950711     C                   Z-ADD     CSBAAS        KAAS
013400950711     C                   Z-ADD     CSBMGS        KMGS
013500950710     C     CSBCDI        IFGT      D40SCD
013600950710     C     KCSB1         SETGT     TNCSB05L
013700950710     C                   READ      TNCSB05L                               23
013800950710     C                   Z-ADD     CSBMGS        WDTSPE            8 0
013900950710     C                   MOVEL     CSBAAS        WDTSPE            8 0
014000950710     C                   Z-ADD     CSBMGS        KMGS
014100950710     C                   Z-ADD     CSBAAS        KAAS
014200950710     C                   END
014300950710     C*
014400950710     C                   ENDSR
014500120215     C*----------------------------------------------------*
014600120215     C*   srmulti - per multiassegni
014700120215     C*----------------------------------------------------*
014800120215     C     srmulti       BEGSR
014900120215     C*
015000120215     c                   eval      mnra = *hival
015100120215     c                   eval      mabi = *hival
015200120215     c                   eval      mcab = *hival
015300120215     c     csbnra        setll     tncsm03l
015400120215     c                   do        *hival
015500120215     c     csbnra        reade     tncsm03l
015600120215     c                   if        %eof(tncsm03l)
015700120215     c                   leave
015800120215     c                   end
015900120215     C                   if        csmnra = WNRA
016000120215     c                   eval      mnra = csmnra
016100120215     c                   eval      mabi = csmabi
016200120215     c                   eval      mcab = csmcab
016300120215     c                   leave
016400120215     c                   end
016500120215     c                   enddo
016600120215     C*
016700120215     C                   ENDSR
016800950706     C*----------------------------------------------------*
016900950710     C*   GESREC: Stampa contassegni selezionati           *
017000950706     C*----------------------------------------------------*
017100950706     C     GESREC        BEGSR
017200950706     C*----------------------------------------------------*
017300950706     C* Se il documento è valido controllo se il record rientra
017400950706     C* nei limiti richiesti
017500950706     C                   MOVEL     D40NAS        WNRA             10
017600120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
017700120220     c                             csbtpi = 'M' and csbabi = 0
017800120215     c                   exsr      srmulti
017900120215     c                   else
018000120215     c                   eval      mnra = csbnra
018100120215     c                   eval      mabi = csbabi
018200120215     c                   eval      mcab = csbcab
018300120215     c                   end
018400950706     C*----------------------------------------------------*
018500950706     C     D40ABI        IFEQ      *ZEROS
018600120215     C     D40ABI        OREQ      mABI
018700950720     C     D40CAB        IFEQ      *ZEROS
018800120215     C     D40CAB        OREQ      mCAB
018900140325     C     D40imp        IFEQ      *ZEROS
019000140325     C     D40imp        OREQ      csbcas
019100950720     C     WNRA          IFEQ      *BLANKS
019200950720     C     WNRA          OREQ      *ZEROS
019300120215     C     WNRA          OREQ      mNRA
019400950706     C*----------------------------------------------------*
019500950706     C* Selezione per stato C/Assegno
019600950706     C     D40TCR        IFEQ      ' '
019700950706     C     D40TCR        OREQ      'D'
019800950706     C     CSBDDC        ANDEQ     0
019900960417     C     CSBSTA        ANDEQ     0
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
028900971222     C                   END
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
032400980127     C                   MOVEL     *BLANKS       ACORAG
032500980127     C                   CALL      'TIBS69R'
032600980127     C                   PARM                    TIBS69
032700980127     C                   PARM                    CNACO
032800980127     C                   PARM                    CNIND
032900980127     C                   PARM                    CNCLP
033000980127     C                   PARM                    FNCLS
033100980127     C     O69ERR        IFEQ      *BLANK
033200950710     C                   MOVEL     ACORAG        BAP
033300000110     C                   ELSE
033400000110     C                   MOVEL     *BLANKS       BAP
033500980127     C                   END
033600960115     C                   ELSE
033700960115     C                   MOVEL     *BLANKS       BAP
033800960115     C                   END
033900950710     C                   ELSE
034000950710     C                   MOVEL     COST2         BAP
034100950710     C                   END
034200950710     C*  Dati giacenzao
034300950710     C                   MOVE      CSBFGC        FGC
034400950710     C                   MOVE      CSBNGC        NGC
034500950710     C                   MOVE      CSBDGC        XDTAMG
034600950710     C                   Z-ADD     0             XDTGMA
034700950710     C                   Z-ADD     2             XDTSTA
034800950710     C                   CALL      'XDATA'       PLDAT
034900950710     C                   MOVE      XDTGMA        DGC
035000120220     c                   if        %subst(csbnra: 10: 1)=' '
035100950710     C   01              WRITE     TES00
035200950710     C   01              SETOFF                                       01
035300950710     C                   WRITE     DET00
035400120215     c                   else
035500120215     c                   exsr      srass
035600120215     C                   END
035700950710     C* Somma nel totale corretto
035800950710     C     CSBSTA        IFNE      9
035900960418     C     D40TCR        OREQ      'A'
036000950710     C     VCA           IFNE      *BLANKS
036100950710     C                   Z-ADD     1             XX
036200950710     C     VCA           LOOKUP    CCV(XX)                                32
036300950710     C                   END
036400950710     C*
036500950710     C     VCA           IFEQ      *BLANKS
036600011001     C     VCA           OREQ      §GEDCN
036700950710     C                   ADD       CSBCAS        TOT
036800950710     C                   ELSE
036900010619     C     DVA(XX)       IFEQ      'S'
037000010619     C     *IN32         ANDEQ     '1'
037100010619     C                   ADD       CSBCAS        TOTE
037200010619     C                   ELSE
037300010615     C* Per convertire gli importi in valute estere utilizzo
037400010615     C* la routine YEURCO in quanto il cambio indicato sul
037500010615     C* file dei C/assegni di filiale non è affidabile
037600010615     C                   CLEAR                   YEURDS
037700010615     C                   MOVEL     VCA           YECDVI
037800010615     C                   Z-ADD     CSBCAS        YECIMI
037900011001     C                   MOVEL     §GEDCN        YECDVO
038000010615     C* passo al pgm io il nr. dei decimali della divisa corrente
038100010615     C* in base a quanto memorizzato sulla tabella CV
038200010615     C                   MOVE      NRDEC         YECDCO
038300010615     C                   MOVEL     'H'           YECTAR
038400010615     C                   CALL      'YEURCO'
038500010615     C                   PARM                    YEURDS
038600010615     C                   Z-ADD     YECIMO        IMPV             13 3
038700950710     C                   ADD       IMPV          TOTV
038800950710     C                   END
038900010619     C                   END
039000950710     C*
039100950706     C                   END
039200000110     C                   END
039300950720     C                   END
039400140325     C                   END
039500950720     C                   END
039600971222     C                   END
039700950706     C*
039800950710     C                   READ      TNCSB05L                               23
039900950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
040000950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
040100950706     C*
040200950706     C                   ENDSR
040300120215      *---------------------------------------------------------------*
040400120215      *  stampa rif assegni
040500120215      *---------------------------------------------------------------*
040600120215     C     srass         BEGSR
040700120215     c                   setoff                                       44
040800120215     c     csbnra        setll     tncsm03l
040900120215     c                   do        *hival
041000120215     c     csbnra        reade     tncsm03l
041100120215     c                   if        %eof(tncsm03l)
041200120215     c                   leave
041300120215     c                   end
041400120215     C                   MOVE      CSmNRA        NDC
041500120215     C                   MOVE      CSmDTE        XDTAMG
041600120215     C                   Z-ADD     0             XDTGMA
041700120215     C                   Z-ADD     2             XDTSTA
041800120215     C                   CALL      'XDATA'       PLDAT
041900120215     C                   MOVE      XDTGMA        DDC
042000120215     C                   MOVE      CSmABI        kABI
042100120215     C                   MOVE      CSmCAB        kCAB
042200120215     C                   MOVEL     *BLANKS       BAI
042300120215     C     Kabi1         CHAIN     CNABI00F                           81
042400120215     C  N81              MOVEL     ABIIST        BAI
042500120215     C                   MOVEL     CSmTPA        ASS
042600120215     C                   MOVE      CSmTPI        ASS
042700120215     C   01              WRITE     TES00
042800120215     C   01              SETOFF                                       01
042900120215     C   01              SETOFF                                       44
043000120215     C                   WRITE     DET00
043100120215     c                   seton                                        44
043200120215     c                   enddo
043300120215     C*
043400120215     C                   ENDSR
043500931011      *---------------------------------------------------------------*
043600941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
043700931011      *---------------------------------------------------------------*
043800941102     C     *INZSR        BEGSR
043900931011      *---------------------------------------------------------------*
044000931011     C     *ENTRY        PLIST
044100931011     C                   PARM                    KPJBA
044200950706     C                   MOVEL     KPJBU         TNSC40
044300950710     C* PLIST per richiamo Routine XDATA
044400950710     C     PLDAT         PLIST
044500950710     C                   PARM                    XDTAMG            6 0
044600950710     C                   PARM                    XDTGMA            6 0
044700950710     C                   PARM                    XDTSTA            1 0
044800931012     C*
044900931011     C                   Z-ADD     1             CODUT
045000931011     C                   CALL      'X§PARUT'
045100931011     C                   PARM                    UT§DSE
045200931011     C*          CODUT     CABEQ-1        FINE
045300931011     C                   MOVEL     RAGUT         RSUT             20
045400931011     C                   MOVEL     REC80         CNCR80
045500931011     C                   MOVEL     KNMUS         §SEI              6
045600950706     C*
045700950706     C                   Z-ADD     1             X                 3 0
045800950710     C                   Z-ADD     0             KCB
045900950706      *  Ricerco capoconto clienti
046000950706     C     X             DOWLE     50
046100950710     C     KCB           ANDEQ     0
046200950706     C                   MOVE      TCU(X)        TCUDS
046300950706     C     F56           IFEQ      'CG'
046400950710     C     F34           IFEQ      '06'
046500950710     C                   Z-ADD     KCU(X)        KCB               4 0
046600950710     C                   END
046700950710     C                   END
046800950706     C                   ADD       1             X
046900950706     C                   END
047000950706     C*----------------------------------------------------*
047100950706     C*  Definizione CHIAVI
047200950710     C     KCSB1         KLIST
047300950710     C                   KFLD                    KAAS
047400950710     C                   KFLD                    KMGS
047500950710     C     KCSB2         KLIST
047600950710     C                   KFLD                    KAAS
047700950710     C                   KFLD                    KMGS
047800950710     C                   KFLD                    KCDI
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
049300950710     C     *LIKE         DEFINE    CSBCDI        KCDI
049400950706     C     *LIKE         DEFINE    CSBAAS        KAAS
049500950706     C     *LIKE         DEFINE    CSBMGS        KMGS
049600950706     C     *LIKE         DEFINE    CSALNP        KLNP
049700950706     C     *LIKE         DEFINE    CSANRS        KNRS
049800950706     C     *LIKE         DEFINE    CSANSP        KNSP
049900980127     C     *LIKE         DEFINE    TBLKUT        KKUT
050000980127     C     *LIKE         DEFINE    TBLCOD        KCOD
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
056400140411     C                   MOVE      D40imp        S42imp
056500950710     C*
056600950710     C     S42TCA        IFEQ      ' '
056700950710     C                   MOVEL     'TUTTI'       S42DCA
056800950710     C                   END
056900950710     C     S42TCA        IFEQ      'I'
057000950710     C                   MOVEL     'INCASSAT'    S42DCA
057100950710     C                   MOVE      'I      '     S42DCA
057200950710     C                   END
057300950710     C     S42TCA        IFEQ      'P'
057400950710     C                   MOVEL     'PAGATI  '    S42DCA
057500950710     C                   END
057600950710     C     S42TCA        IFEQ      'G'
057700950710     C                   MOVEL     'GIACENTI'    S42DCA
057800950710     C                   END
057900950710     C     S42TCA        IFEQ      'S'
058000950710     C                   MOVEL     'SOSPESI '    S42DCA
058100950710     C                   END
058200950710     C     S42TCA        IFEQ      'D'
058300950710     C                   MOVEL     'DA INCAS'    S42DCA
058400950710     C                   MOVE      'SARE   '     S42DCA
058500950710     C                   END
058600950710     C     S42TCA        IFEQ      'A'
058700950710     C                   MOVEL     'ANNULLAT'    S42DCA
058800950710     C                   MOVE      'I      '     S42DCA
058900950710     C                   END
059000950710     C*
059100950710     C                   SETON                                        03
059200950710     C                   MOVEL     *ALL'-'       CMP198
059300950710     C                   MOVEL     *ALL'-'       CM1198
059400950710     C                   WRITE     PAG00
059500950710     C*
059600950710     C                   SETON                                        01
059700950710     C                   Z-ADD     D40SCD        KCDI
059800950710     C                   Z-ADD     D40SDD        KMGS
059900950710     C                   MOVEL     D40SDD        KAAS
060000950710     C*----------------------------------------------------*
060100000110     C*  RECUPERO DIVISA MONETA CORRENTE
060200000110     C                   CLEAR                   DGED
060300000110     C                   CLEAR                   DSBS02
060400000110     C                   MOVEL     'C'           T02MOD
060500000110     C                   MOVEL     KNSIF         T02SIF
060600000110     C                   MOVEL     'GED'         T02COD
060700000110     C                   MOVEL     '1'           T02KE1
060800000110     C                   CALL      'TIBS02R'
060900000110     C                   PARM                    KPJBA
061000000110     C                   PARM                    DSBS02
061100000110     C*
061200000110    3C     T02ERR        IFEQ      ' '
061300000110     C                   MOVEL     T02UNI        DGED
061400011001     C                   MOVEL     §GEDCN        DIVCOR
061500000110    3C                   ENDIF
061600000110     C*----------------------------------------------------*
061700000110     C* Carico tabella cambi/divise
061800000110     C                   Z-ADD     0             XX                3 0
061900000110     C                   MOVEL     'CV'          KCOD
062000000110     C     KTAB          CHAIN     TABEL                              31
062100000110     C     *IN31         DOWEQ     '0'
062200000110     C     XX            ANDLT     100
062300000110     C                   MOVEL     TBLKEY        WDIV              3
062400000110     C     TBLFLG        IFEQ      ' '
062500000110     C                   ADD       1             XX
062600000110     C                   MOVEL     TBLKEY        CCV(XX)
062700000110     C                   MOVEL     TBLUNI        DSCV
062800000110     C                   MOVEL     §CVDVA        DVA(XX)
062900011001     C******               MOVEL§CVCMB    CMB,XX
063000000110     C     §CVDVA        IFEQ      'S'
063100011001     C     WDIV          ANDNE     §GEDCN
063200000110     C                   MOVEL     WDIV          DIVALT
063300000110     C                   END
063400000110     C* Memorizzo nr. decimali della divisa corrente
063500011001     C     WDIV          IFEQ      §GEDCN
063600000110     C                   Z-ADD     §CVDEC        NRDEC             2 0
063700000110     C                   END
063800000110     C                   END
063900000110     C     KTAB          READE     TABEL                                  31
064000000110     C                   END
064100931012     C*----------------------------------------------------*
064200931011     C                   ENDSR
