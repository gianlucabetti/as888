000100891011     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200940412      * TRUL14R  *---------------------------------------------------*
000300971108      *       - CAMBIO SISTEMA INFORMATIVO MODULO BASE PROGET -      *
000400940412      *--------------------------------------------------------------*
000500900103     FKFSIF01L  IF   E           K DISK
000600940412     FKPPRF01L  IF   E           K DISK
000700940412     FKPMNT01L  IF   E           K DISK    USROPN
000800940412     FTRUL14D   CF   E             WORKSTN
000900940412     F                                     SFILE(UL14SF1:REC1)
001000020605
001100020605      * Schiere comandi
001200020605     D cmd             S             80    DIM(2) CTDATA PERRCD(1)              comandi
001300020605
001400020605     D lenght          S             15  5 INZ(80)                              *Lunghezza cmd CLP
001500020605     D comman          S             80                                         *valore cmd CLP
001600020605
001700940412     D* SCHIERE X CTL ESISTENZA MENU INIZIALE S.I.
001800971110     D CNCR80        E DS
001900971110     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002000900303     D KPJBA         E DS
002100991103     D C201            C                   CONST('GAITRA201')
002200991103     D C101            C                   CONST('SDITRA101')
002300971108     D PARAM           DS
002400971016     D  PARRIC                 1      1
002500971016     D  PARSIF                 2     11
002600940412     D* LA PRIMA LIB. DELLA DS CONTIENE AZIONI E MENU DEL S.I.
002700940412     D KLBSI           DS           170
002800940412     D  DSLIB1                 1     10
002900900103     C                   EXSR      DEFCAM
003000940412      *----------------------------------------------------*
003100940412      *---------- GESTIONE SUBFILE ------------------------*
003200940412      *----------------------------------------------------*
003300891011     C                   Z-ADD     0             REC1              5 0          *
003400891011     C                   MOVE      *BLANKS       SCELTA                         *
003500940412      *---------- PULIZIA SLFCT :                          *
003600891011     C                   SETON                                            70    *
003700940412     C                   WRITE     UL14SC1                                      *
003800891011     C                   SETOFF                                       7071      *
003900940412      *---------- CONTROLLI PER CARICAMENTO SFL :          *
004000891011     C     SOPRA         TAG                                                    *
004100900103     C                   READ      KFSIF00                                91    *
004200891011     C   91              GOTO      EMISF1                                       *
004300900103     C     KNMSI         CABEQ     'ACGGAA'      SOPRA
004400021007     C     KNMSI         CABEQ     'FILTRA201'   SOPRA
004500021009     C     KNMSI         CABEQ     'PROBAS   '   SOPRA
004600900103     C                   MOVE      KNMSI         W4                4
004700911230     C                   MOVEL     W4            W2                2
004800920924     C                   MOVEL     W2            W1                1            P=PROVE
004900900103     C                   MOVEL     KNMSI         W6                6
005000910115     C                   MOVEL     KNMSI         W7                7
005100971110     C                   MOVEL     KNMSI         WTSIFF            3
005200940412      *
005300940124     C     WPRF          CASEQ     'EDP'         UTEDP                          EDP
005400971231     C     WPRF          CASEQ     'ISP'         UTEDP                          ISPETT.
005500980709     C     WPRF          CASEQ     'INT'         UTEDP                          INTERNAZ.
005600981105     C     WPRF          CASEQ     'DIV'         UTEDP                          SEG.DIST.
005700940124     C     WPRF          CASEQ     'PAG'         UTPAG                          PAGHE
005800971108     C     WPRF          CASEQ     'SDI'         UTSDI                          SDI
005900991103     C     WPRF          CASEQ     'ASC'         UTASC                          ASC99
006000980313     C     WPRF6         CASEQ     'CLI001'      UTEDPS                         GABRIELE
006100970422     C     WPRF6         CASEQ     'FIN001'      UTEDPS                         GABRIELE
006200940124     C     WPRF6         CASEQ     'GEN001'      UTEDPS                         STEFANO
006300940124     C                   END
006400940412      *
006500940412      *  TUTTI GLI ALTRI
006600940124     C     WPRF          IFNE      'EDP'
006700940124     C     WPRF          ANDNE     'PAG'
006800971231     C     WPRF          ANDNE     'ISP'
006900981105     C     WPRF          ANDNE     'DIV'
007000971108     C     WPRF          ANDNE     'SDI'
007100991103     C     WPRF          ANDNE     'ASC'
007200940124     C     WPRF6         ANDNE     'CLI001'
007300970422     C     WPRF6         ANDNE     'FIN001'
007400940124     C     WPRF6         ANDNE     'GEN001'
007500920924     C     W1            CABEQ     'P'           SOPRA
007600901023     C     W6            IFNE      'GAITRA'
007700971108     C     W6            ANDNE     'SDITRA'
007800901023     C                   GOTO      SOPRA
007900901023     C                   END
008000940124     C                   END
008100971231     C     SIMFEL        IFGT      0                                            FILIALE
008200971231     C     W6            IFNE      'GAITRA'
008300971231     C     W6            ANDNE     'SDITRA'
008400020625     C     W6            andne     'FILTRA'
008500050225     c* Solo per utente EDP anche S.I. GAIZCAMBIO
008600050225     c                   if        wprf= 'EDP' and w6='GAIZCA'
008700050225     c                   else
008800971231     C                   GOTO      SOPRA
008900050225     C                   ENDif
009000050225     C                   END
009100990730     C* IN FILIALE : PER TUTTI I PROFILI , TRANNE QUELLI "SPECIALI",
009200990730     C* VISUALIZZO TUTTI I SIS.INF. "GAI" E "SDI"     ECCETTO
009300990730     C* LA 201 O LA 101 DEL RAMO AZIENDALE DIVERSO DA QUELLO DI COLLEGAM.
009400990730     C     WPRF          IFNE      'EDP'
009500971231     C     WPRF          ANDNE     'ISP'
009600980709     C     WPRF          ANDNE     'INT'
009700990123     C     WPRF          ANDNE     'OPR'
009800981105     C     WPRF          ANDNE     'DIV'
009900980313     C     WPRF6         ANDNE     'REM101'
010000030908     C     WPRF          ANDNE     'DAN'
010100990730     C*
010200990730     C     WTSIFV        IFEQ      'GAI'
010300990730     C     KNMSI         ANDEQ     SIF101
010400990730     C     WTSIFV        OREQ      'SDI'
010500990730     C     KNMSI         ANDEQ     SIF201
010600971231     C                   GOTO      SOPRA
010700990730     C                   ENDIF
010800990730     C*
010900971231     C                   END
011000971231     C                   END
011100940412      *
011200900103     C                   MOVEL     KNMSI         VSIF                           *
011300900103     C                   MOVEL     KDSSI         VDES                           *
011400940412      *---------- CARICAMENTO SFL :                        *
011500891011     C     REC1          ADD       1             REC1                 71        *
011600940412     C                   WRITE     UL14SF1                                      *
011700891011     C                   GOTO      SOPRA                                        *
011800940412      *---------- EMISSIONE SFL :                          *
011900891011     C     EMISF1        TAG                                                    *
012000891011     C                   SETON                                            10    *
012100940412     C                   WRITE     UL14D1                                       *TESTATA
012200940412     C                   EXFMT     UL14SC1                                      *
012300900303     C   KC              GOTO      FINE                                         *
012400940412      *---------- LETTURA RECORD VARIATI :                 *
012500940412     C   71              READC     UL14SF1                                91    *
012600891011     C  N71                                                                     *
012700891011     COR 91              GOTO      EMISF1                                       *
012800891011     C     SCELTA        CABNE     'X'           EMISF1                         *
012900900103     C                   MOVE      VSIF          NEWSIF           10            *
013000940412      *----------------------------------------------------*
013100940412      *
013200940412     C     VSIF          CHAIN     KFSIF01L                           20
0133009404122    C     *IN20         IFEQ      *ON
013400940412     C                   GOTO      EMISF1
0135009404122E   C                   ELSE
013600940412      * CTL ESISTENZA MENU INIZIALE
013700940412     C                   EXSR      CTRMNU
0138009404122-   C                   ENDIF
013900940412      *
014000940412     C     *IN22         CABEQ     *ON           EMISF1                         ERRORE
014100940412      *
014200940412      * PER CAMBIARE IL S.I. MI BASTA RIEMPIRE IL CAMPO KNSIF
014300971016     C  N99              MOVE      VSIF          KNSIF
014400971016     C   99              MOVE      VSIF          PARSIF
014500940412      *
014600940412     C     FINE          TAG
014700971016     C   99              MOVEL     PARAM         KPJBU
014800940412     C                   SETON                                            LR
014900940412      **************************************************************************
015000940412      *   SISTEMI INFORMATIVI ABILITATI EDP
015100940412      **************************************************************************
015200940124     C     UTEDP         BEGSR
015300940124     C     W6            CABEQ     'AURIGA'      SOPRA
015400940124     C     W6            CABEQ     'AURIG0'      SOPRA
015500010222     C     W6            CABEQ     'AURIGC'      SOPRA
015600010821     C     W6            CABEQ     'EURIGA'      SOPRA
015700010821     C     W6            CABEQ     'EURIG0'      SOPRA
015800010821     C     W6            CABEQ     'EURIGC'      SOPRA
015900030218     C     W6            CABEQ     'ZURIGA'      SOPRA
016000030218     C     W6            CABEQ     'ZURIG0'      SOPRA
016100030218     C     W6            CABEQ     'ZURIGC'      SOPRA
016200940124     C                   ENDSR
016300991103      **************************************************************************
016400991103      *   SISTEMI INFORMATIVI ABILITATI SDI
016500991103      **************************************************************************
016600991103     C     UTASC         BEGSR
016700991103     C     KNMSI         IFNE      C201
016800991103     C     KNMSI         ANDNE     C101
016900991103     C                   GOTO      SOPRA
017000991103     C                   ENDIF
017100991103     C                   ENDSR
017200971108      **************************************************************************
017300971108      *   SISTEMI INFORMATIVI ABILITATI SDI
017400971108      **************************************************************************
017500971108     C     UTSDI         BEGSR
017600971110     C     W7            CABNE     'SDITRA1'     SOPRA
017700971108     C                   ENDSR
017800940412      **************************************************************************
017900940412      *   SISTEMI INFORMATIVI ABILITATI GABRIELE/STEFANO
018000940412      **************************************************************************
018100940124     C     UTEDPS        BEGSR
018200940124     C     W6            CABEQ     'AURIGA'      SOPRA
018300940124     C     W6            CABEQ     'AURIG0'      SOPRA
018400010222     C     W6            CABEQ     'AURIGC'      SOPRA
018500010821     C     W6            CABEQ     'EURIGA'      SOPRA
018600010821     C     W6            CABEQ     'EURIG0'      SOPRA
018700010821     C     W6            CABEQ     'EURIGC'      SOPRA
018800030218     C     W6            CABEQ     'ZURIGA'      SOPRA
018900030218     C     W6            CABEQ     'ZURIG0'      SOPRA
019000030218     C     W6            CABEQ     'ZURIGC'      SOPRA
019100940124     C                   ENDSR
019200940420      **************************************************************************
019300971108      *   SISTEMI INFORMATIVI ABILITATI 2XX+ 1XX +PROVA
019400940420      **************************************************************************
019500940420     C     UTEDPR        BEGSR
019600940420     C     W6            IFNE      'GAITRA2'
019700971108     C     W6            ANDNE     'SDITRA1'
019800940420     C                   GOTO      SOPRA
019900940420     C                   END
020000940420     C                   ENDSR
020100940412      **************************************************************************
020200940412      *   SISTEMI INFORMATIVI ABILITATI PAGHE
020300940124     C     UTPAG         BEGSR
020400940124     C     W6            IFNE      'AURIGA'
020500940124     C     W6            ANDNE     'AURIG0'
020600010222     C     W6            ANDNE     'AURIGC'
020700010821     C     W6            ANDNE     'EURIGA'
020800010821     C     W6            ANDNE     'EURIG0'
020900010821     C     W6            ANDNE     'EURIGC'
021000030218     C     W6            ANDNE     'ZURIGA'
021100030218     C     W6            ANDNE     'ZURIG0'
021200030218     C     W6            ANDNE     'ZURIGC'
021300940124     C     W7            ANDNE     'GAITRA2'
021400971108     C     W7            ANDNE     'SDITRA1'
021500940124     C                   GOTO      SOPRA
021600940124     C                   END
021700971110     C     W1            CABEQ     'P'           SOPRA
021800940124     C                   ENDSR
021900940412      **************************************************************************
022000940412      *       DEFINIZIONE CAMPI
022100940412      **************************************************************************
022200900103     C     DEFCAM        BEGSR
022300940412     C     *ENTRY        PLIST
022400940412     C                   PARM                    KPJBA
022500971016     C                   MOVEL     KPJBU         PARAM
022600020605     c
022700020605      * Elimino le dataara relative al controllo utenti
022800020605     c                   clear                   comman
022900020605     c                   movel(P)  cmd(1)        comman
023000020605     c                   call      'QCMDEXC'                            90
023100020605     c                   parm                    comman
023200020605     c                   parm                    lenght
023300020605      * elimino la seconda dataara
023400020605     c                   clear                   comman
023500020605     c                   movel(P)  cmd(2)        comman
023600020605     c                   call      'QCMDEXC'                            90
023700020605     c                   parm                    comman
023800020605     c                   parm                    lenght
023900020605     c
024000971110     C** MI SALVO IL S.I. DI ENTRATA
024100971110     C                   MOVEL     KNSIF         WTSIFV            3            GAI/SDI
024200990730     C* MEM. SIF CON I DATI OPERATIVI
024300990730     C                   MOVEL     'GAITRA'      SIF201           10
024400990730     C                   MOVE      '201 '        SIF201
024500990730     C                   MOVEL     'SDITRA'      SIF101           10
024600990730     C                   MOVE      '101 '        SIF101
024700990730     C*
024800971110     C** DETERMINO SE IN SEDE O IN FILIALE
024900971110     C                   Z-ADD     1             CODUT             1 0
025000971110     C                   CALL      'X§PARUT'
025100971110     C                   PARM                    UT§DSE
025200971110     C                   MOVEL     REC80         CNCR80
025300971016     C** PGM RICHIAMATO: DEVO PASSARE IL SISTEMA INFORMATIVO INVECE
025400971016     C**     CHE IMPOSTARLO
025500971016     C     PARRIC        IFEQ      'S'
025600971016     C                   SETON                                        99
025700971016     C                   CLEAR                   PARSIF
025800971016     C                   ENDIF
025900971016     C**
026000900113     C                   MOVEL     KNMUS         WPRF              3            TIPO PROFILO
026100910115     C                   MOVEL     KNMUS         WPRF5             5            PROFILO
026200910205     C                   MOVEL     KNMUS         WPRF6             6            PROFILO
026300940412      *
026400940412      * REPERISCO MENU INIZIALE ATTUALMENTE UTILIZZATO
026500940412     C     KNMUS         CHAIN     KPPRF01L                           50
026600940412     C                   MOVE      PRFNMI        WMNUIN            4
026700940412     C     WMNUIN        IFEQ      *BLANK
026800940412     C     KNSIF         CHAIN     KFSIF01L                           51
026900940412     C                   MOVE      KMNSI         WMNUIN
027000940412     C                   ENDIF
027100900103     C                   ENDSR
027200940412      **************************************************************************
027300940412      *    CONTROLLA ESISTENZA MENU INIZIALE NEL S.I.
027400940412      **************************************************************************
027500940412     C     CTRMNU        BEGSR
027600961118     C                   OPEN      KPMNT01L                             22
027700940412     C     *IN22         IFEQ      *OFF
027800940412     C     WMNUIN        CHAIN     KPMNT01L                           22
027900940412     C                   ENDIF
028000940412     C                   ENDSR
028100020605
028200020605**   cmd - comandi
028300020605DLTDTAARA DTAARA(QTEMP/§AZUTE)
028400020605DLTDTAARA DTAARA(QTEMP/§DATIUTE)
