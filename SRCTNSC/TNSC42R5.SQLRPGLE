000100151027     /*END
000200151027     H DECEDIT('0,') DATEDIT(*DMY.)
000300151027     h option(*nodebugio)
000400151027     h alwnull(*inputonly)
000500151027?     *--------------------------------------------------------------*
000600151027      * TNSC42R5                                                     *
000700151027      *       *-----------------------------------------*            *
000800151027      *           INTERROGAZIONE CONTRASSEGNI  X SEDE                *
000900151027      *         STAMPA CONTRASSEGNI SELEZ.  ID BONIFICO              *
001000151027      *       *-----------------------------------------*            *
001100151027      *                                                              *
001200151027?     *--------------------------------------------------------------*
001300151027     FTABEL00F  IF   E           K DISK
001400151027     FCNABI00F  IF   E           K DISK
001500151027     FTNCSA01L  IF   E           K DISK
001600151027     FTNCSm03L  IF   E           K DISK
001700151027     FTNSC42P   O    E             PRINTER OFLIND(*IN01)
001800151027?     *--------------------------------------------------------------*
001900151027?     *  SCHIERE                                                     *
002000151027?     *--------------------------------------------------------------*
002100151027     D CCV             S              3    DIM(100)
002200151027     D DVA             S              1    DIM(100)
002300151027?     *--------------------------------------------------------------*
002400151027?     *  DS                                                          *
002500151027?     *--------------------------------------------------------------*
002600151027     D KPJBA         E DS
002700151027     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002800151027     D DGED          E DS
002900151027      *
003000151027     D YEURDS        E DS                  EXTNAME(YEURCODS)
003100151027     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003200151027     D  TCU                  398    697
003300151027     D                                     DIM(50)
003400151027     D  KCU                  698    847P 0
003500151027     D                                     DIM(50)
003600151027     D                                     PACKEVEN
003700151027     D CNCR80        E DS
003800151027     D DSCV          E DS
003900151027      *
004000151027     d ds_TNCSB      e ds                  extname(TNCSB00F)
004100151027     D*
004200151027     D TCUDS           DS
004300151027     D  F34                    3      4
004400151027     D  F56                    5      6
004500151027      *
004600151027     D TIBS69        E DS                  EXTNAME(TIBS69DS)
004700151027     D CNACO         E DS                  EXTNAME(CNACO00F)
004800151027     D CNIND         E DS                  EXTNAME(CNIND00F)
004900151027     D CNCLP         E DS                  EXTNAME(CNCLP00F)
005000151027     D FNCLS         E DS                  EXTNAME(FNCLS00F)
005100151027     D TNSC40        E DS                  EXTNAME(TNSC40DS)
005200151027     D*
005300151027     D WLBDA8          DS
005400151027     D  G02DAT                 1      8  0
005500151027     D  G02INV                 9     16  0
005600151027     D  G02ERR                17     17
005700151027     D  G02TGI                18     22  0
005800151027      *
005900151027     D                SDS
006000151027     D  NOMPG                  1     10
006100151027?     *--------------------------------------------------------------*
006200151027?     *  VARIABILI                                                   *
006300151027?     *--------------------------------------------------------------*
006400151027     d mnra            s                   like(csmnra)
006500151027     d mabi            s                   like(csmabi)
006600151027     d mcab            s                   like(csmcab)
006700151027      * -?Stringhe SQL da eseguire?
006800151027     d wSQL            s          32740    inz   varying
006900151027?     *--------------------------------------------------------------*
007000151027?     *  COSTANTI TIPO DOCUMENTO                                     *
007100151027?     *--------------------------------------------------------------*
007200151027     D COST1           C                   CONST('CONTANTE            ')
007300151027     D COST2           C                   CONST('ANNULLATO           ')
007400151027      *---------------------------------------------------------------*
007500151027      *   C I C L O      P R I N C I P A L E                          *
007600151027      *---------------------------------------------------------------*
007700151027     C                   SETON                                        LR
007800151027     C*
007900151027     C*  Posizionamento iniziale x stampa
008000151027     c                   exsr      sr_PrepSQL
008100151027     c                   exsr      sr_ReadCursor
008200151027     C*
008300151027     C*  LOOP
008400151027     C     *IN23         DOWEQ     '0'
008500151027     C                   EXSR      GESREC
008600151027     C                   ENDDO
008700151027     C*
008800151027     C     TOT           IFGT      0
008900151027     C   01              WRITE     TES00
009000151027     C   01              SETOFF                                       01
009100151027     C                   WRITE     TOT00
009200151027     C                   ENDIF
009300151027     C*
009400151027     C     TOTE          IFGT      0
009500151027     C   01              WRITE     TES00
009600151027     C   01              SETOFF                                       01
009700151027     C                   WRITE     TOTE0
009800151027     C                   ENDIF
009900151027     C*
010000151027     C     TOTV          IFGT      0
010100151027     C   01              WRITE     TES00
010200151027     C   01              SETOFF                                       01
010300151027     C                   WRITE     TOTV0
010400151027     C                   ENDIF
010500151027     C*
010600151027     C*  OPERAZIONI FINALI / USCITA DAL *PGM
010700151027     C                   exsr      sr_RoutEnd
010800151027     C*----------------------------------------------------*
010900151027     C*   srmulti - per multiassegni
011000151027     C*----------------------------------------------------*
011100151027     C     srmulti       BEGSR
011200151027     C*
011300151027     c                   eval      mnra = *hival
011400151027     c                   eval      mabi = *hival
011500151027     c                   eval      mcab = *hival
011600151027     c     csbnra        setll     tncsm03l
011700151027     c                   do        *hival
011800151027     c     csbnra        reade     tncsm03l
011900151027     c                   if        %eof(tncsm03l)
012000151027     c                   leave
012100151027     c                   endif
012200151027     C                   if        csmnra = WNRA
012300151027     c                   eval      mnra = csmnra
012400151027     c                   eval      mabi = csmabi
012500151027     c                   eval      mcab = csmcab
012600151027     c                   leave
012700151027     c                   endif
012800151027     c                   enddo
012900151027     C*
013000151027     C                   ENDSR
013100151027     C*----------------------------------------------------*
013200151027     C*   GESREC: Stampa contassegni selezionati           *
013300151027     C*----------------------------------------------------*
013400151027     C     GESREC        BEGSR
013500151027     C*
013600151027     C* Se il documento è valido controllo se il record rientra
013700151027     C* nei limiti richiesti
013800151027     C                   MOVEL     D40NAS        WNRA             10
013900151027if  1c                   if        %subst(csbnra: 10: 1)<>' ' and
014000151027     c                             csbtpi = 'M' and csbabi = 0
014100151027     c                   exsr      srmulti
014200151027x   1c                   else
014300151027     c                   eval      mnra = csbnra
014400151027     c                   eval      mabi = csbabi
014500151027     c                   eval      mcab = csbcab
014600151027e   1c                   endif
014700151027     C*----------------------------------------------------*
014800151027if  1C     D40ABI        IFEQ      *ZEROS
014900151027     C     D40ABI        OREQ      mABI
015000151027if  2C     D40CAB        IFEQ      *ZEROS
015100151027     C     D40CAB        OREQ      mCAB
015200151027if  3C     D40imp        IFEQ      *ZEROS
015300151027     C     D40imp        OREQ      csbcas
015400151027if  4C     WNRA          IFEQ      *BLANKS
015500151027     C     WNRA          OREQ      *ZEROS
015600151027     C     WNRA          OREQ      mNRA
015700151027     C*----------------------------------------------------*
015800151027     C* Selezione per stato C/Assegno
015900151027if  5C     D40TCR        IFEQ      ' '
016000151027     C     D40TCR        OREQ      'D'
016100151027     C     CSBDDC        ANDEQ     0
016200151027     C     CSBSTA        ANDEQ     0
016300151027     C     D40TCR        OREQ      'G'
016400151027     C     CSBSTA        ANDEQ     2
016500151027     C     D40TCR        OREQ      'A'
016600151027     C     CSBSTA        ANDEQ     9
016700151027     C     D40TCR        OREQ      'S'
016800151027     C     CSBSTA        ANDEQ     5
016900151027     C     D40TCR        OREQ      '3'
017000151027     C     CSBSTA        ANDEQ     3
017100151027     C     D40TCR        OREQ      'I'
017200151027     C     CSBDDC        ANDGT     0
017300151027     C     CSBDDP        ANDEQ     0
017400151027     C     D40TCR        OREQ      'P'
017500151027     C     CSBDDP        ANDGT     0
017600151027     C     D40TCR        OREQ      'T'
017700151027     C     CSBSTA        ANDEQ     6
017800151027     C     D40TCR        OREQ      'R'
017900151027     C     CSBSTA        ANDEQ     7
018000151027     C     D40TCR        OREQ      'O'
018100151027     C     CSBSTA        ANDEQ     8
018200151027     C*  Spedizione
018300151027     C                   MOVE      CSBAAS        AAS
018400151027     C                   MOVEL     CSBMGS        MM                2 0
018500151027     C                   MOVE      CSBMGS        GG                2 0
018600151027     C                   MOVEL     GG            DSP
018700151027     C                   MOVE      MM            DSP
018800151027     C                   MOVE      CSBLNA        LNA
018900151027     C                   MOVE      CSBLNP        LNP
019000151027     C                   MOVE      CSBNRS        NRS
019100151027     C                   MOVE      CSBNSP        NSP
019200151027     C*  C/Assegno
019300151027     C                   MOVE      CSBVCA        VCA
019400151027     C                   Z-ADD     CSBCAS        IMP
019500151027     C                   MOVEL     *BLANKS       BAI
019600151027     C*  Dati Incasso
019700151027if  6c                   if        %subst(csbnra: 10: 1)=' '
019800151027     c                   setoff                                       44
019900151027     C                   MOVE      CSBNRA        NDC
020000151027     C                   MOVE      CSBDTE        XDTAMG
020100151027     C                   Z-ADD     0             XDTGMA
020200151027     C                   Z-ADD     2             XDTSTA
020300151027     C                   CALL      'XDATA'       PLDAT
020400151027     C                   MOVE      XDTGMA        DDC
020500151027     C*  Decodifica banca incasso
020600151027     C                   Z-ADD     CSBABI        KABI
020700151027     C                   Z-ADD     CSBCAB        KCAB
020800151027     C                   MOVEL     *BLANKS       ABIIST
020900151027     C     KABI1         CHAIN     CNABI00F                           31
021000151027     C                   MOVEL     ABIIST        BAI
021100151027     C*  Tipo C/Ass.
021200151027     C                   MOVE      CSBTPI        ASS
021300151027     C                   MOVEL     CSBTPA        ASS
021400151027e   6c                   endif
021500151027     c*
021600151027if  6C     NDC           IFEQ      *BLANKS
021700151027     C                   MOVEL     COST1         BAI
021800151027e   6C                   ENDIF
021900151027     C*  Dati Mittente
022000151027     C                   MOVE      CSBRMN        RMN
022100151027     C                   MOVE      CSBCDI        CDM
022200151027     C                   MOVE      CSBCDI        W8899             4 0
022300151027if  6C     W8899         IFEQ      8888
022400151027     C     W8899         OREQ      9999
022500151027     C                   Z-ADD     CSBAAS        KAAS
022600151027     C                   Z-ADD     CSBLNP        KLNP
022700151027     C                   Z-ADD     CSBNRS        KNRS
022800151027     C                   Z-ADD     CSBNSP        KNSP
022900151027     C                   MOVEL     *BLANKS       CSARSD
023000151027     C     KCSA          CHAIN     TNCSA01L                           31
023100151027     C                   MOVEL     CSARSD        RGM
023200151027x   6C                   ELSE
023300151027     C                   MOVEL     *BLANKS       ACORAG
023400151027      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
023500151027     C                   CLEAR                   TIBS69
023600151027     C                   CLEAR                   CNACO
023700151027     C                   CLEAR                   CNIND
023800151027     C                   CLEAR                   CNCLP
023900151027     C                   CLEAR                   FNCLS
024000151027     C                   MOVEL     ' '           I69TLA
024100151027     C                   MOVEL     KNSIF         I69SIF
024200151027     C                   Z-ADD     CSBCDI        I69KAC
024300151027     C                   CALL      'TIBS69R'
024400151027     C                   PARM                    TIBS69
024500151027     C                   PARM                    CNACO
024600151027     C                   PARM                    CNIND
024700151027     C                   PARM                    CNCLP
024800151027     C                   PARM                    FNCLS
024900151027if  7C     O69ERR        IFEQ      *BLANK
025000151027     C                   MOVEL     ACORAG        RGM
025100151027e   7C                   ENDIF
025200151027e   6C                   ENDIF
025300151027     C*  Destinatario
025400151027     C                   MOVEL     CSBRSD        RGD
025500151027     C*  Tipo Documento
025600151027if  6C     CSBFPC        IFEQ      '2'
025700151027     C                   MOVE      'BON.'        DOC
025800151027x   6C                   ELSE
025900151027if  7C     CSBBNA        IFEQ      9999999
026000151027     C                   MOVE      'LETT'        DOC
026100151027x   7C                   ELSE
026200151027if  8C     CSBBNA        IFNE      0
026300151027     C                   MOVE      'ASS.'        DOC
026400151027e   8C                   ENDIF
026500151027e   7C                   ENDIF
026600151027e   6C                   ENDIF
026700151027     C*  Dati pagamento
026800151027     C                   MOVE      CSBNDP        NDP
026900151027     C                   MOVE      CSBDDP        XDTAMG
027000151027     C                   Z-ADD     0             XDTGMA
027100151027     C                   Z-ADD     2             XDTSTA
027200151027     C                   CALL      'XDATA'       PLDAT
027300151027     C                   MOVE      XDTGMA        DDP
027400151027     C*  Decodifica banca pagamento
027500151027if  6C     CSBSTA        IFNE      9
027600151027if  7C     CSBDDP        IFNE      0
027700151027      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
027800151027     C                   CLEAR                   TIBS69
027900151027     C                   CLEAR                   CNACO
028000151027     C                   CLEAR                   CNIND
028100151027     C                   CLEAR                   CNCLP
028200151027     C                   CLEAR                   FNCLS
028300151027     C                   MOVEL     ' '           I69TLA
028400151027     C                   MOVEL     KNSIF         I69SIF
028500151027     C                   Z-ADD     KCB           I69KCC
028600151027     C                   Z-ADD     CSBBNA        I69KAC
028700151027     C                   MOVEL     *BLANKS       ACORAG
028800151027     C                   CALL      'TIBS69R'
028900151027     C                   PARM                    TIBS69
029000151027     C                   PARM                    CNACO
029100151027     C                   PARM                    CNIND
029200151027     C                   PARM                    CNCLP
029300151027     C                   PARM                    FNCLS
029400151027if  8C     O69ERR        IFEQ      *BLANK
029500151027     C                   MOVEL     ACORAG        BAP
029600151027x   8C                   ELSE
029700151027     C                   MOVEL     *BLANKS       BAP
029800151027e   8C                   ENDIF
029900151027x   7C                   ELSE
030000151027     C                   MOVEL     *BLANKS       BAP
030100151027e   7C                   ENDIF
030200151027x   6C                   ELSE
030300151027     C                   MOVEL     COST2         BAP
030400151027e   6C                   ENDIF
030500151027     C*  Dati giacenzao
030600151027     C                   MOVE      CSBFGC        FGC
030700151027     C                   MOVE      CSBNGC        NGC
030800151027     C                   MOVE      CSBDGC        XDTAMG
030900151027     C                   Z-ADD     0             XDTGMA
031000151027     C                   Z-ADD     2             XDTSTA
031100151027     C                   CALL      'XDATA'       PLDAT
031200151027     C                   MOVE      XDTGMA        DGC
031300151027if  6c                   if        %subst(csbnra: 10: 1)=' '
031400151027     C   01              WRITE     TES00
031500151027     C   01              SETOFF                                       01
031600151027     C                   WRITE     DET00
031700151027x   6c                   else
031800151027     c                   exsr      srass
031900151027e   6C                   ENDif
032000151027     C* Somma nel totale corretto
032100151027if  6C     CSBSTA        IFNE      9
032200151027     C     D40TCR        OREQ      'A'
032300151027if  7C     VCA           IFNE      *BLANKS
032400151027     C                   Z-ADD     1             XX
032500151027     C     VCA           LOOKUP    CCV(XX)                                32
032600151027e   7C                   ENDIF
032700151027     C*
032800151027if  7C     VCA           IFEQ      *BLANKS
032900151027     C     VCA           OREQ      §GEDCN
033000151027     C                   ADD       CSBCAS        TOT
033100151027x   7C                   ELSE
033200151027if  8C     DVA(XX)       IFEQ      'S'
033300151027     C     *IN32         ANDEQ     '1'
033400151027     C                   ADD       CSBCAS        TOTE
033500151027x   8C                   ELSE
033600151027     C*
033700151027     C* Per convertire gli importi in valute estere utilizzo
033800151027     C* la routine YEURCO in quanto il cambio indicato sul
033900151027     C* file dei C/assegni di filiale non è affidabile
034000151027     C                   CLEAR                   YEURDS
034100151027     C                   MOVEL     VCA           YECDVI
034200151027     C                   Z-ADD     CSBCAS        YECIMI
034300151027     C                   MOVEL     §GEDCN        YECDVO
034400151027     C* passo al pgm io il nr. dei decimali della divisa corrente
034500151027     C* in base a quanto memorizzato sulla tabella CV
034600151027     C                   MOVE      NRDEC         YECDCO
034700151027     C                   MOVEL     'H'           YECTAR
034800151027     C                   CALL      'YEURCO'
034900151027     C                   PARM                    YEURDS
035000151027     C                   Z-ADD     YECIMO        IMPV             13 3
035100151027     C                   ADD       IMPV          TOTV
035200151027e   8C                   ENDIF
035300151027e   7C                   ENDIF
035400151027     C*
035500151027e   6C                   ENDIF
035600151027e   5C                   ENDIF
035700151027e   4C                   ENDIF
035800151027e   3C                   ENDIF
035900151027e   2C                   ENDIF
036000151027e   1C                   ENDIF
036100151027     C*
036200151027     C                   exsr      sr_ReadCursor
036300151027     C                   Z-ADD     CSBMGS        WDTSPE            8 0
036400151027     C                   MOVEL     CSBAAS        WDTSPE            8 0
036500151027     C*
036600151027     C                   ENDSR
036700151027      *---------------------------------------------------------------*
036800151027      *  stampa rif assegni
036900151027      *---------------------------------------------------------------*
037000151027     C     srass         BEGSR
037100151027     c                   setoff                                       44
037200151027     c     csbnra        setll     tncsm03l
037300151027     c                   do        *hival
037400151027     c     csbnra        reade     tncsm03l
037500151027     c                   if        %eof(tncsm03l)
037600151027     c                   leave
037700151027     c                   endif
037800151027     C                   MOVE      CSmNRA        NDC
037900151027     C                   MOVE      CSmDTE        XDTAMG
038000151027     C                   Z-ADD     0             XDTGMA
038100151027     C                   Z-ADD     2             XDTSTA
038200151027     C                   CALL      'XDATA'       PLDAT
038300151027     C                   MOVE      XDTGMA        DDC
038400151027     C                   MOVE      CSmABI        kABI
038500151027     C                   MOVE      CSmCAB        kCAB
038600151027     C                   MOVEL     *BLANKS       BAI
038700151027     C     Kabi1         CHAIN     CNABI00F                           81
038800151027     C  N81              MOVEL     ABIIST        BAI
038900151027     C                   MOVEL     CSmTPA        ASS
039000151027     C                   MOVE      CSmTPI        ASS
039100151027     C   01              WRITE     TES00
039200151027     C   01              SETOFF                                       01
039300151027     C   01              SETOFF                                       44
039400151027     C                   WRITE     DET00
039500151027     c                   seton                                        44
039600151027     c                   enddo
039700151027     C*
039800151027     C                   ENDSR
039900151027      *---------------------------------------------------------------*
040000151027      *  *INZSR: OPERAZIONI INIZIALI                                  *
040100151027      *---------------------------------------------------------------*
040200151027     C     *INZSR        BEGSR
040300151027      *---------------------------------------------------------------*
040400151027     C     *ENTRY        PLIST
040500151027     C                   PARM                    KPJBA
040600151027     C                   MOVEL     KPJBU         TNSC40
040700151027     C* PLIST per richiamo Routine XDATA
040800151027     C     PLDAT         PLIST
040900151027     C                   PARM                    XDTAMG            6 0
041000151027     C                   PARM                    XDTGMA            6 0
041100151027     C                   PARM                    XDTSTA            1 0
041200151027     C*
041300151027     C                   Z-ADD     1             CODUT
041400151027     C                   CALL      'X§PARUT'
041500151027     C                   PARM                    UT§DSE
041600151027     C*
041700151027     C                   MOVEL     RAGUT         RSUT             20
041800151027     C                   MOVEL     REC80         CNCR80
041900151027     C                   MOVEL     KNMUS         §SEI              6
042000151027     C*
042100151027     C                   Z-ADD     1             X                 3 0
042200151027     C                   Z-ADD     0             KCB
042300151027      *  Ricerco capoconto clienti
042400151027     C     X             DOWLE     50
042500151027     C     KCB           ANDEQ     0
042600151027     C                   MOVE      TCU(X)        TCUDS
042700151027     C     F56           IFEQ      'CG'
042800151027     C     F34           IFEQ      '06'
042900151027     C                   Z-ADD     KCU(X)        KCB               4 0
043000151027     C                   ENDIF
043100151027     C                   ENDIF
043200151027     C                   ADD       1             X
043300151027     C                   ENDDO
043400151027     C*----------------------------------------------------*
043500151027     C*  Definizione CHIAVI
043600151027     C     KTAB          KLIST
043700151027     C                   KFLD                    KKUT
043800151027     C                   KFLD                    KCOD
043900151027     C     KCSA          KLIST
044000151027     C                   KFLD                    KAAS
044100151027     C                   KFLD                    KLNP
044200151027     C                   KFLD                    KNRS
044300151027     C                   KFLD                    KNSP
044400151027     C     KABI1         KLIST
044500151027     C                   KFLD                    KABI
044600151027     C                   KFLD                    KCAB
044700151027     C                   Z-ADD     1             KKUT
044800151027     C*----------------------------------------------------*
044900151027     C*  Definizione variabili
045000151027     C     *LIKE         DEFINE    CSBAAS        KAAS
045100151027     C     *LIKE         DEFINE    CSALNP        KLNP
045200151027     C     *LIKE         DEFINE    CSANRS        KNRS
045300151027     C     *LIKE         DEFINE    CSANSP        KNSP
045400151027     C     *LIKE         DEFINE    TBLKUT        KKUT
045500151027     C     *LIKE         DEFINE    TBLCOD        KCOD
045600151027     C     *LIKE         DEFINE    ABIABI        KABI
045700151027     C     *LIKE         DEFINE    ABICAB        KCAB
045800151027     C*----------------------------------------------------*
045900151027     C*  Inizializzazione delle variabii
046000151027     C                   MOVEL     NOMPG         NOMPGM
046100151027     C                   MOVE      D40INT        S42SEL
046200151027     C                   MOVE      D40AAS        S42AAS
046300151027     C                   MOVE      D40LNP        S42LNP
046400151027     C                   MOVE      D40NRS        S42NRS
046500151027     C                   MOVE      D40NRM        S42NRM
046600151027     C                   MOVE      D40LP1        S42LP1
046700151027     C                   MOVE      D40SD1        G02INV
046800151027     C                   Z-ADD     0             G02DAT
046900151027     C                   MOVEL     '3'           G02ERR
047000151027     C                   CALL      'XSRDA8'
047100151027     C                   PARM                    WLBDA8
047200151027     C                   MOVE      G02DAT        S42SD1
047300151027     C                   MOVE      D40SA1        G02INV
047400151027     C                   Z-ADD     0             G02DAT
047500151027     C                   MOVEL     '3'           G02ERR
047600151027     C                   CALL      'XSRDA8'
047700151027     C                   PARM                    WLBDA8
047800151027     C                   MOVE      G02DAT        S42SA1
047900151027     C                   MOVE      D40SDD        G02INV
048000151027     C                   Z-ADD     0             G02DAT
048100151027     C                   MOVEL     '3'           G02ERR
048200151027     C                   CALL      'XSRDA8'
048300151027     C                   PARM                    WLBDA8
048400151027     C                   MOVE      G02DAT        S42SDD
048500151027     C                   MOVE      D40SDA        G02INV
048600151027     C                   Z-ADD     0             G02DAT
048700151027     C                   MOVEL     '3'           G02ERR
048800151027     C                   CALL      'XSRDA8'
048900151027     C                   PARM                    WLBDA8
049000151027     C                   MOVE      G02DAT        S42SDA
049100151027     C                   MOVE      D40SCD        S42SCD
049200151027     C                   MOVE      D40PDD        G02INV
049300151027     C                   Z-ADD     0             G02DAT
049400151027     C                   MOVEL     '3'           G02ERR
049500151027     C                   CALL      'XSRDA8'
049600151027     C                   PARM                    WLBDA8
049700151027     C                   MOVE      G02DAT        S42PDD
049800151027     C                   MOVE      D40PDA        G02INV
049900151027     C                   Z-ADD     0             G02DAT
050000151027     C                   MOVEL     '3'           G02ERR
050100151027     C                   CALL      'XSRDA8'
050200151027     C                   PARM                    WLBDA8
050300151027     C                   MOVE      G02DAT        S42PDA
050400151027     C                   MOVE      D40PND        S42PND
050500151027     C                   MOVE      D40PTN        S42PTI
050600151027     C                   MOVE      D40PCD        S42PCD
050700151027     C                   MOVE      D40TCR        S42TCA
050800151027     C                   MOVE      D40DDT        G02INV
050900151027     C                   Z-ADD     0             G02DAT
051000151027     C                   MOVEL     '3'           G02ERR
051100151027     C                   CALL      'XSRDA8'
051200151027     C                   PARM                    WLBDA8
051300151027     C                   MOVE      G02DAT        S42DDC
051400151027     C                   MOVE      D40NDT        S42NDT
051500151027     C                   MOVE      D40RGP        S42RGP
051600151027     C                   Z-ADD     D40NBN        S42NBN
051700151027      *
051800151027     C                   MOVE      D40ABI        S42ABI
051900151027     C                   MOVE      D40CAB        S42CAB
052000151027     C                   MOVE      D40NAS        S42NRA
052100151027     C                   MOVE      D40imp        S42imp
052200151027     C*
052300151027     C     S42TCA        IFEQ      ' '
052400151027     C                   MOVEL     'TUTTI'       S42DCA
052500151027     C                   END
052600151027     C     S42TCA        IFEQ      'I'
052700151027     C                   MOVEL     'INCASSAT'    S42DCA
052800151027     C                   MOVE      'I      '     S42DCA
052900151027     C                   END
053000151027     C     S42TCA        IFEQ      'P'
053100151027     C                   MOVEL     'PAGATI  '    S42DCA
053200151027     C                   END
053300151027     C     S42TCA        IFEQ      'G'
053400151027     C                   MOVEL     'GIACENTI'    S42DCA
053500151027     C                   END
053600151027     C     S42TCA        IFEQ      'S'
053700151027     C                   MOVEL     'SOSPESI '    S42DCA
053800151027     C                   END
053900151027     C     S42TCA        IFEQ      'D'
054000151027     C                   MOVEL     'DA INCAS'    S42DCA
054100151027     C                   MOVE      'SARE   '     S42DCA
054200151027     C                   END
054300151027     C     S42TCA        IFEQ      'A'
054400151027     C                   MOVEL     'ANNULLAT'    S42DCA
054500151027     C                   MOVE      'I      '     S42DCA
054600151027     C                   END
054700151027     C*
054800151027     C                   eval      *in05 = *on
054900151027     C*
055000151027     C                   MOVEL     *ALL'-'       CMP198
055100151027     C                   MOVEL     *ALL'-'       CM1198
055200151027     C                   WRITE     PAG00
055300151027     C*
055400151027     C                   SETON                                        01
055500151027     C*----------------------------------------------------*
055600151027     C*  RECUPERO DIVISA MONETA CORRENTE
055700151027     C                   CLEAR                   DGED
055800151027     C                   CLEAR                   DSBS02
055900151027     C                   MOVEL     'C'           T02MOD
056000151027     C                   MOVEL     KNSIF         T02SIF
056100151027     C                   MOVEL     'GED'         T02COD
056200151027     C                   MOVEL     '1'           T02KE1
056300151027     C                   CALL      'TIBS02R'
056400151027     C                   PARM                    KPJBA
056500151027     C                   PARM                    DSBS02
056600151027     C*
056700151027    3C     T02ERR        IFEQ      ' '
056800151027     C                   MOVEL     T02UNI        DGED
056900151027     C                   MOVEL     §GEDCN        DIVCOR
057000151027    3C                   ENDIF
057100151027     C*----------------------------------------------------*
057200151027     C* Carico tabella cambi/divise
057300151027     C                   Z-ADD     0             XX                3 0
057400151027     C                   MOVEL     'CV'          KCOD
057500151027     C     KTAB          CHAIN     TABEL                              31
057600151027     C     *IN31         DOWEQ     '0'
057700151027     C     XX            ANDLT     100
057800151027     C                   MOVEL     TBLKEY        WDIV              3
057900151027     C     TBLFLG        IFEQ      ' '
058000151027     C                   ADD       1             XX
058100151027     C                   MOVEL     TBLKEY        CCV(XX)
058200151027     C                   MOVEL     TBLUNI        DSCV
058300151027     C                   MOVEL     §CVDVA        DVA(XX)
058400151027     C     §CVDVA        IFEQ      'S'
058500151027     C     WDIV          ANDNE     §GEDCN
058600151027     C                   MOVEL     WDIV          DIVALT
058700151027     C                   ENDIF
058800151027     C* Memorizzo nr. decimali della divisa corrente
058900151027     C     WDIV          IFEQ      §GEDCN
059000151027     C                   Z-ADD     §CVDEC        NRDEC             2 0
059100151027     C                   ENDIF
059200151027     C                   ENDIF
059300151027     C     KTAB          READE     TABEL                                  31
059400151027     C                   ENDDO
059500151027     C*----------------------------------------------------*
059600151027     C                   ENDSR
059700151027      *
059800151027      *---------------------------------------------------------------*
059900151027      *?Preparazione stringa SQL per estrazione dati da TNCSB00F.    ?*
060000151027      *---------------------------------------------------------------*
060100151027     c     sr_PrepSQL    BEGSR
060200151027      *
060300151027      * -?Impostazione opzioni per SQL?
060400151027     c/exec sql
060500151027     c+     set  option  DynUsrPrf = *Owner,
060600151027     c+                  CloSqlCsr = *EndMod
060700151027     c/end-exec
060800151027      *
060900151027     c                   eval      wSQL = 'select TNCSB00F.* +
061000151028     c                                       from YHBOS00F inner join +
061100151028     c                                            TNCSB00F +
061200151027     c                                         on CSBaas = BONaas +
061300151027     c                                        and CSBlnp = BONlnp +
061400151027     c                                        and CSBnrs = BONnrs +
061500151027     c                                        and CSBnsp = BONnsp +
061600151027     c                                        and CSBtbl = BONtbl +
061700151027     c                                      where BONnbn = ' +
061800151027     c                                    %trim( %editc( D40nbn : '3' ) )
061900151027     c                                  + ' order by CSBaas, CSBlnp, +
062000151027     c                                               CSBnrs, CSBnsp'
062100151027      *
062200151027      * -?Dichiarazione cursore?
062300151027     c/exec sql
062400151027     c+     prepare S1   from :wSQL
062500151027     c/end-exec
062600151027     c/exec sql
062700151027     c+     declare C1   cursor for S1
062800151027     c/end-exec
062900151027      *
063000151027     c                   if        SQLcode < *zero
063100151027     c                   eval      *in23 = *on
063200151027     c                   exsr      sr_PrintERR
063300151027     c                   endif
063400151027      *
063500151027      * -?Apertura del cursore?
063600151027     c/exec sql
063700151027     c+     open C1
063800151027     c/end-exec
063900151027      *
064000151027     c                   if        SQLcode < *zero
064100151027     c                   eval      *in23 = *on
064200151027     c                   exsr      sr_CloseCursor
064300151027     c                   exsr      sr_PrintERR
064400151027     c                   endif
064500151027      *
064600151027     c                   ENDSR
064700151027      *
064800151027      *---------------------------------------------------------------*
064900151027      *?Lettura cursore.                                             ?*
065000151027      *---------------------------------------------------------------*
065100151027     c     sr_ReadCursor BEGSR
065200151027      *
065300151027     c                   eval      *in23 = *off
065400151027     c                   clear                   ds_TNCSB
065500151027      *
065600151027     c/exec SQL
065700151027     c+     fetch next   from C1   into :ds_TNCSB
065800151027     c/end-exec
065900151027      *
066000151027     c                   select
066100151027     c                   when      SQLcode = 100
066200151027     c                   eval      *in23 = *on
066300151027     c                   exsr      sr_CloseCursor
066400151027     c                   when      SQLcode < *zero
066500151027     c                   eval      *in23 = *on
066600151027     c                   exsr      sr_CloseCursor
066700151027     c                   exsr      sr_PrintERR
066800151027     c                   endsl
066900151027      *
067000151027     c                   ENDSR
067100151027      *
067200151027      *---------------------------------------------------------------*
067300151027      *?Chiusura cursore.                                            ?*
067400151027      *---------------------------------------------------------------*
067500151027     c     sr_CloseCursorBEGSR
067600151027      *
067700151027      * -?Chiusura del cursore?
067800151027     c/exec sql
067900151027     c+     close C1
068000151027     c/end-exec
068100151027      *
068200151027     c                   ENDSR
068300151027      *
068400151027      *---------------------------------------------------------------*
068500151027      *?Stampa segnalazione dell'errore rilevato via SQL             ?*
068600151027      *---------------------------------------------------------------*
068700151027     c     sr_PrintERR   BEGSR
068800151027      *
068900151027      * -?Stampa del Dump?
069000151027     c                   Dump(A)
069100151027      *
069200151027      * -?Stampa del Job-Log?
069300151027     c                   eval      Qcmd = 'DSPJOBLOG job(*) output(*print)'
069400151027     c                   eval      Qlen = %len( %trim( Qcmd ) )
069500151027     c                   call      'QCMDEXC'
069600151027     c                   parm                    Qcmd             35
069700151027     c                   parm                    Qlen             15 5
069800151027      *
069900151027      * -?Chiusura del programma?
070000151027     c                   exsr      sr_RoutEnd
070100151027      *
070200151027     c                   ENDSR
070300151027      *
070400151027      *---------------------------------------------------------------*
070500151027      *?Operazioni Finali.                                           ?*
070600151027      *---------------------------------------------------------------*
070700151027     c     sr_RoutEnd    BEGSR
070800151027      *
070900151027     C* RICHIAMO PGM X CHIUSURA IN LR
071000151027     C                   CLEAR                   TIBS69
071100151027     C                   MOVEL     'C'           I69TLA
071200151027     C                   CALL      'TIBS69R'
071300151027     C                   PARM                    TIBS69
071400151027     C                   PARM                    CNACO
071500151027     C                   PARM                    CNIND
071600151027     C                   PARM                    CNCLP
071700151027     C                   PARM                    FNCLS
071800151027      *
071900151027     c                   ENDSR
