000100141127     H*
000200141127     H*
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400940223      *
000500100322      *      PGM di FILTRO.
000600100322      *
000700940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000800940620      *  03           PROTECT TUTTI I CAMPI
000900030123      *  09           PROTECT dei campi voce C/E
001000940223      *  21           GENERICO OPERAZIONI I/O
001100940223      *  22           GENERICO ERRORE OPERAZIONI I/O
001200940223      *  30           SFLDSP
001300940223      * N31           SFLCLR
001400940128      *  31           SFLDSPCTL
001500940128      *  32           SFLNXTCHG
001600940128      *  33           SFLEND
001700940128      *  39           OF PRTF
001800940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001900940317    > *  Specificare l'uso dei singoli indicatori
002000940315      *  50 <---> 98  ERRORI SU VIDEO
002100940317    > *  Specificare l'uso dei singoli indicatori
002200940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002300940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002400940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002500940128      *  99           INDIC. GENERALE DI ERRORE
002600940128     F*----------------------------------------------------*
002700100121     Fazorg01L  IF   E           K DISK
002800100322$001 FTNTBAUTD1 CF   E             WORKSTN
002900940201     F                                     INFDS(DSFMT)
003000940128     D*----------------------------------------------------*
003100100322     D* Passaggio Parametri
003200100322     D kpjba         E DS
003300100322      *
003400100322      *----------------------------------------------------*
003500100322      * La local Data Area sostituisce i parametri passati fra programmi
003600100322      * *LDA Local Data Area per passare i totali da mettere in testata
003700100322     D LDA_Selez       DS
003800100322     D  lda_AUT                1      1
003900100322     D  lda_COOP               2      2
004000100322     D  lda_AFDE               3      3
004100100324     D  lda_AUTMTC             4      4
004200100324     D  lda_AUTSDT             5      5
004300100324     D  lda_AUTRET             6      6
004400100324     D  lda_AUTCON             7      7
004500100324     D  lda_AUTCRS             8      8
004600100324     D  lda_AUTVTCC            9      9
004700100324     D  lda_AUTMTCC           10     10
004800100324     D  lda_AUTSDTC           11     11
004900100324     D  lda_AUTRETC           12     12
005000100324     D  lda_AUTCRSC           13     13
005100100324     D  lda_AUTCONC           14     14
005200100324     D  lda_RIEABI            15     15
005300100324     D  lda_AFFMTC            16     16
005400100324     D  lda_AFFSDT            17     17
005500100324     D  lda_AFFRET            18     18
005600100324     D  lda_AFFCON            19     19
005700100324     D  lda_AFFCRS            20     20
005800100324     D  lda_AFFGVSTD          21     21
005900100324     D  lda_AFFAGV            22     22
006000100324     D  lda_AFFCPSTD          23     23
006100100324     D  lda_AFFARRM           24     24
006200100324     D  lda_AUTCPRRE          25     25
006300100324     D  lda_AUTGSTM           26     26
006400100324     D  lda_AUTSTDCC          27     27
006500120906     D  lda_AUTgfir           28     28
006600120906     D  lda_AFFgfir           29     29
006700120906     D  lda_COPgfir           30     30
006800120906     D  lda_AUTgfPRE          31     31
006900120906     D  lda_lda                1     31    DIM(31)
007000100322     D*-------------
007100100322     D DSP_Selez       DS
007200100322     D  D1AUT                  1      1
007300100322     D  D1COOP                 2      2
007400100322     D  D1AFDE                 3      3
007500100324     D  D1AUTMTC               4      4
007600100324     D  D1AUTSDT               5      5
007700100324     D  D1AUTRET               6      6
007800100324     D  D1AUTCON               7      7
007900100324     D  D1AUTCRS               8      8
008000100324     D  D1AUTVTCC              9      9
008100100324     D  D1AUTMTCC             10     10
008200100324     D  D1AUTSDTC             11     11
008300100324     D  D1AUTRETC             12     12
008400100324     D  D1AUTCRSC             13     13
008500100324     D  D1AUTCONC             14     14
008600100324     D  D1RIEABI              15     15
008700100324     D  D1AFFMTC              16     16
008800100324     D  D1AFFSDT              17     17
008900100324     D  D1AFFRET              18     18
009000100324     D  D1AFFCON              19     19
009100100324     D  D1AFFCRS              20     20
009200100324     D  D1AFFGVSTD            21     21
009300100324     D  D1AFFAGV              22     22
009400100324     D  D1AFFCPSTD            23     23
009500100324     D  D1AFFARRM             24     24
009600100324     D  D1AUTCPRRE            25     25
009700100324     D  D1AUTGSTM             26     26
009800100324     D  D1AUTSTDCC            27     27
009900120906     D  D1AUTgfir             28     28
010000120906     D  D1AFFgfir             29     29
010100120906     D  D1COPgfir             30     30
010200120906     D  D1AUTgfPRE            31     31
010300120906     D  dsp_lda                1     31    DIM(31)
010400940201     D*-------------
010500030204     D  S_TASTO        S                   like($Tasto)
010600940201     D DSFMT           DS
010700940506     D  $TASTO               369    369
010800940201     D  NRG                  370    370
010900940201     D  NCL                  371    371
011000940201     D*-------------
011100940201     D* posizione cursore
011200940201     D CURSOR          DS
011300940223     D  RIRI                   1      2B 0 INZ
011400940201     D  R$$                    2      2
011500940223     D  COCO                   3      4B 0 INZ
011600940201     D  C$$                    4      4
011700940207     D*-------------
011800940207     D* Reperimento nome PGM
011900940207     D STATUS         SDS           333
012000940207     D  DSPGM            *PROC
012100940225     D*-------------
012200940225     D* COSTANTI
012300940225     D*-------------
012400940506     D* Tasti di funzione
012500940506     D F01             C                   CONST(X'31')
012600940506     D F02             C                   CONST(X'32')
012700940506     D F03             C                   CONST(X'33')
012800940506     D F04             C                   CONST(X'34')
012900940506     D F05             C                   CONST(X'35')
013000940506     D F06             C                   CONST(X'36')
013100940506     D F07             C                   CONST(X'37')
013200940506     D F08             C                   CONST(X'38')
013300940506     D F09             C                   CONST(X'39')
013400940506     D F10             C                   CONST(X'3A')
013500940506     D F11             C                   CONST(X'3B')
013600940506     D F12             C                   CONST(X'3C')
013700940506     D F13             C                   CONST(X'B1')
013800940506     D F14             C                   CONST(X'B2')
013900940506     D F15             C                   CONST(X'B3')
014000940506     D F16             C                   CONST(X'B4')
014100940506     D F17             C                   CONST(X'B5')
014200940506     D F18             C                   CONST(X'B6')
014300940506     D F19             C                   CONST(X'B7')
014400940506     D F20             C                   CONST(X'B8')
014500940506     D F21             C                   CONST(X'B9')
014600940506     D F22             C                   CONST(X'BA')
014700940506     D F23             C                   CONST(X'BB')
014800940506     D F24             C                   CONST(X'BC')
014900940506     D ENTER           C                   CONST(X'F1')
015000940506     D ROLDWN          C                   CONST(X'F4')
015100940506     D ROLLUP          C                   CONST(X'F5')
015200940506     D*-------------
015300941108     D DATA            C                   CONST('0001-01-01')
015400100322     D OPz_FILTRO      C                   CONST('F I L T R O')
015500940127     C*----------------------------------------------------*
015600940127     C*                MAIN LINE PROGRAM
015700940127     C*----------------------------------------------------*
015800100121     c                   exsr      INIZIO
015900940223     C*
016000940223     C     $FINE         DOWEQ     *OFF
016100940202     C     $GEST         CASEQ     'D1'          GESD1
016200940117     C                   END
016300940117     C                   END
016400940325     C* fine programma
016500030113     C                   Seton                                        LR
016600940131     C************************************************************
016700100121     C* OPERAZIONI INIZIALI
016800100121     C************************************************************
016900100121     C     INIZIO        BEGSR
017000100121     C*
017100100121     C* Reperimento parametri
017200100121     C     *ENTRY        PLIST
017300100322     C                   PARM                    kpjba
017400100322     C*
017500100322     C* definisce la Local Data Area per selezionare i profili
017600100322     C     *DTAARA       DEFINE    *LDA          LDA_Selez
017700100322     C*
017800100121     C* Variabili per gestione videate
017900100121     C                   MOVE      'D1'          $GEST
018000100121     C                   MOVE      *OFF          $FINE
018100100121     C                   MOVE      *ON           $INZD1
018200100121     C*
018300100121     C* Variabili appoggio
018400100121     C                   Z-ADD     0             CURR
018500100121     C                   Z-ADD     0             CURC
018600100121     C*
018700100121     C* Valorizzazione campi univoci testate
018800100121     C                   MOVEL     KNSIF         NOMSIF
018900100121     C                   MOVEL     DSPGM         NOMPGM
019000100121     C*
019100100121     C* Inizializzazione/Decodifica prima videata
019200100121     C                   EXSR      INZD1
019300100121     C                   MOVE      *OFF          $INZD1
019400100121     C*
019500100121     C                   ENDSR
019600100121     C************************************************************
019700100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
019800100121     C************************************************************
019900100121     C     DEFCAM        BEGSR
020000100121     C*
020100100121     C* klist
020200100121      *
020300100121     C* Variabili per gestione videate
020400100121     C*
020500100121     C                   MOVE      *BLANK        $GEST             2
020600100121     C                   MOVE      *BLANK        $FINE             1
020700100121     C                   MOVE      *BLANK        $INZD1            1
020800100121     C*
020900100121     C* Variabili appoggio
021000100121     C                   Z-ADD     0             CURR              2 0
021100100121     C                   Z-ADD     0             CURC              2 0
021200100121     C                   MOVE      *ZEROS        WIN              99
021300100121     C* Indici
021400100121     C                   Z-ADD     0             X                 3 0
021500100121     C                   Z-ADD     0             Y                 3 0
021600100121     C*
021700100121     C                   ENDSR
021800100121     C************************************************************
021900940510     C* GESTIONE VIDEO RECORD D1
022000940131     C************************************************************
022100940127     C     GESD1         BEGSR
022200030113      *
022300940225     C* inizializzazione videata
022400940223     C     $INZD1        IFEQ      *ON
022500940127     C                   EXSR      INZD1
022600940223     C                   MOVE      *OFF          $INZD1
022700940117     C                   END
022800100322     C*
022900100322     c                   movel     Opz_FILTRO    D1Fun
023000940131     C*
023100100322     c                   setoff                                       99
023200940620     C* Immissione/Modifica/Copia
023300030113     C*               *----------------*
023400940620     C                   EXFMT     D1
023500030113     C*               *----------------*
023600030114     C*
023700030114     C     *IN99         IFEQ      *OFF
023800030114     C*
0239000301141    C                   SELECT
024000030114     C* F12=Ritorno
024100030114     C     $TASTO        WHENEQ    F12
024200030114     C                   EXSR      F12D1
024300030114     C*
024400100322     C* F17=SELEZIONA TUTTO
024500100322     C     $TASTO        WHENEQ    F17
024600100322     C                   EXSR      F17D1
024700100322     C*
0248000301141O   C                   OTHER
024900030114     C*
025000030114     C                   EXSR      CTRD1
025100030113     C*
025200940616     C     *IN99         IFEQ      *OFF
025300940616     C* F6
0254000302052    C     $TASTO        ifeq      F06
025500940224     C* eseguo aggiornamento
025600940315    >C                   EXSR      AGGANA
025700940224     C* eseguo operazioni del dopo-aggiornamento
025800030114     C  N99              EXSR      GESAGG
025900030205     c                   end
026000030114     C*
026100030114     C                   ENDIF
026200030114     C*
0263000301142-   C                   ENDSL
026400030114     C*
0265000301142-   C                   ENDIF
026600030114     C*
026700940117     C                   ENDSR
026800940117      ************************************************************
026900940207      * INIZIALIZZAZIONE VIDEATA DATI
027000940117      ************************************************************
027100940127     C     INZD1         BEGSR
027200940131     C*
027300940615     C                   CLEAR                   D1
027400100322     C*
027500100324     c                   goto      salta
027600100324     C*
027700100322     C*   Legge comunque la Local Data Area per riempire i campi a Video
027800100322     c                   in        LDA_Selez
027900100322     C                   eval       D1AUTMTC   =   LDA_AUTMTC
028000100322     C                   eval       D1RIEABI   =   LDA_RIEABI
028100100322     C                   eval       D1AUTSDT   =   LDA_AUTSDT
028200100322     C                   eval       D1AUTRET   =   LDA_AUTRET
028300100322     C                   eval       D1AFFMTC   =   LDA_AFFMTC
028400100322     C                   eval       D1AUTCON   =   LDA_AUTCON
028500100322     C                   eval       D1AFFSDT   =   LDA_AFFSDT
028600100322     C                   eval       D1AUTCRS   =   LDA_AUTCRS
028700100322     C                   eval       D1AFFRET   =   LDA_AFFRET
028800100322     C                   eval       D1AFFCON   =   LDA_AFFCON
028900100322     C                   eval       D1AUTVTCC  =   LDA_AUTVTCC
029000100322     C                   eval       D1AFFCRS   =   LDA_AFFCRS
029100100322     C                   eval       D1AUTMTCC  =   LDA_AUTMTCC
029200100322     C                   eval       D1AUTSDTC  =   LDA_AUTSDTC
029300100322     C                   eval       D1AUTRETC  =   LDA_AUTRETC
029400100322     C                   eval       D1AUTCRSC  =   LDA_AUTCRSC
029500100322     C                   eval       D1AUTCONC  =   LDA_AUTCONC
029600100322     C                   eval       D1AFFGVSTD =   LDA_AFFGVSTD
029700100322     C                   eval       D1AFFAGV   =   LDA_AFFAGV
029800100322     C                   eval       D1AFFCPSTD =   LDA_AFFCPSTD
029900100322     C                   eval       D1AFFARRM  =   LDA_AFFARRM
030000100322     C                   eval       D1AUTCPRRE =   LDA_AUTCPRRE
030100100322     C                   eval       D1AUTGSTM  =   LDA_AUTGSTM
030200100322     C                   eval       D1AUTSTDCC =   LDA_AUTSTDCC
030300100322     C                   eval       D1AUT      =   LDA_AUT
030400100322     C                   eval       D1COOP     =   LDA_COOP
030500100322     C                   eval       D1AFDE     =   LDA_AFDE
030600120906     C                   eval       D1AUTgfir  =   LDA_AUTgfir
030700120906     C                   eval       D1AFFgfir  =   LDA_AFFgfir
030800120906     C                   eval       D1COPgfir  =   LDA_COPgfir
030900120906     C                   eval       D1AUTgfPRE =   LDA_AUTgfPRE
031000100322     C*   poi la rilascia
031100100322     c                   out       LDA_Selez
031200100324     C*
031300100324     c     salta         tag
031400100324     C*
031500100322     c     ' '           lookup    lda_lda                                17
031600100322     C*
031700100322     C*
031800940117     C                   ENDSR
031900940207     C/EJECT
032000940309     C************************************************************
032100940309     C* GESTIONE F12 VIDEO D1
032200940309     C************************************************************
032300940309     C     F12D1         BEGSR
032400940309     C*
032500940309     C                   MOVE      *ON           $FINE
032600940309     C*
032700940309     C                   ENDSR
032800100322     C/EJECT
032900100322     C************************************************************
033000100322     C* GESTIONE F17 VIDEO D1
033100100322     C************************************************************
033200100322     C     F17D1         BEGSR
033300100322     C*
033400100324     C   17              MOVE      *ALL'S'       dsp_LDA
033500100324     C  n17              MOVE      *ALL' '       dsp_LDA
033600100322     C*
033700100324     c                   if        *in17
033800100324     c                   setoff                                       17
033900100324     c                   else
034000100324     c                   seton                                        17
034100100324     c                   end
034200100324     C*
034300100322     C                   ENDSR
034400940131     C************************************************************
034500940207     C* CONTROLLO VIDEATA
034600940131     C************************************************************
034700940127     C     CTRD1         BEGSR
034800940131     C*
034900100322     C                   MOVE      *ALL'0'       IN4098           49
035000100322     C                   MOVEA     IN4098        *IN(40)
035100940127     C                   SETOFF                                       99
035200030123      *
035300940127     C* non imposto pos. del cursore
035400940127     C                   Z-ADD     0             H1RIGA
035500940127     C                   Z-ADD     0             H1COLO
035600100322     C*
035700100322     c     'S'           lookup    dsp_lda                                21
035800100322     c                   if        not *IN21
035900100322     c                   seton                                        41  99
036000100322     c                   end
036100100322      *
036200100322      *  x eventuali decodifiche
036300940302     C     *IN99         IFEQ      *ON
036400940510     C     $GEST         ANDEQ     'D1'
036500940117     C                   MOVEA     *IN           WIN
036600100322     C                   MOVE      *ALL'0'       IN4098           49
036700100322     C                   MOVEA     IN4098        *IN(40)
036800940614     C                   WRITE     D1
036900940117     C                   MOVEA     WIN           *IN
037000940302     C                   ENDIF
037100940127     C*
037200940117     C                   ENDSR
037300030204     C/EJECT
037400940224     C************************************************************
037500940315    >C* AGGIORNAMENTO ANAGRAFICA
037600940224     C************************************************************
037700940315    >C     AGGANA        BEGSR
037800100322     C*
037900100322     C*   Legge comunque la Local Data Area per riempire i campi a Video
038000100322     c                   in        LDA_Selez
038100100322     C                   eval       LDA_AUTMTC  =   D1AUTMTC
038200100322     C                   eval       LDA_RIEABI  =   D1RIEABI
038300100322     C                   eval       LDA_AUTSDT  =   D1AUTSDT
038400100322     C                   eval       LDA_AUTRET   =   D1AUTRET
038500100322     C                   eval       LDA_AFFMTC   =   D1AFFMTC
038600100322     C                   eval       LDA_AUTCON   =   D1AUTCON
038700100322     C                   eval       LDA_AFFSDT   =   D1AFFSDT
038800100322     C                   eval       LDA_AUTCRS   =   D1AUTCRS
038900100322     C                   eval       LDA_AFFRET   =   D1AFFRET
039000100322     C                   eval       LDA_AFFCON   =   D1AFFCON
039100100322     C                   eval       LDA_AUTVTCC  =   D1AUTVTCC
039200100322     C                   eval       LDA_AFFCRS   =   D1AFFCRS
039300100322     C                   eval       LDA_AUTMTCC  =   D1AUTMTCC
039400100322     C                   eval       LDA_AUTSDTC  =   D1AUTSDTC
039500100322     C                   eval       LDA_AUTRETC  =   D1AUTRETC
039600100322     C                   eval       LDA_AUTCRSC  =   D1AUTCRSC
039700100322     C                   eval       LDA_AUTCONC  =   D1AUTCONC
039800100322     C                   eval       LDA_AFFGVSTD =   D1AFFGVSTD
039900100322     C                   eval       LDA_AFFAGV   =   D1AFFAGV
040000100322     C                   eval       LDA_AFFCPSTD =   D1AFFCPSTD
040100100322     C                   eval       LDA_AFFARRM  =   D1AFFARRM
040200100322     C                   eval       LDA_AUTCPRRE =   D1AUTCPRRE
040300100322     C                   eval       LDA_AUTGSTM  =   D1AUTGSTM
040400100322     C                   eval       LDA_AUTSTDCC =   D1AUTSTDCC
040500100322     C                   eval       LDA_AUT      =   D1AUT
040600100322     C                   eval       LDA_COOP     =   D1COOP
040700100322     C                   eval       LDA_AFDE     =   D1AFDE
040800120906     C                   eval       LDA_AUTgfir  =   D1AUTgfir
040900120906     C                   eval       LDA_AFFgfir  =   D1AFFgfir
041000120906     C                   eval       LDA_COPgfir  =   D1COPgfir
041100120906     C                   eval       LDA_AUTgfPRE =   D1AUTgfPRE
041200100322     C*   poi la aggiorna con le selezioni fatte a video
041300100322     c                   out       LDA_Selez
041400940203     C*
041500940203     C                   ENDSR
041600940224     C************************************************************
041700940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
041800940224     C************************************************************
041900940224     C     GESAGG        BEGSR
042000940224     C*
042100100322     C                   MOVE      *ON           $FINE
042200940224     C*
042300940224     C                   ENDSR
042400940131     C************************************************************
