000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
006500100324     D  lda_lda                1     27    DIM(27)
006600100322     D*-------------
006700100322     D DSP_Selez       DS
006800100322     D  D1AUT                  1      1
006900100322     D  D1COOP                 2      2
007000100322     D  D1AFDE                 3      3
007100100324     D  D1AUTMTC               4      4
007200100324     D  D1AUTSDT               5      5
007300100324     D  D1AUTRET               6      6
007400100324     D  D1AUTCON               7      7
007500100324     D  D1AUTCRS               8      8
007600100324     D  D1AUTVTCC              9      9
007700100324     D  D1AUTMTCC             10     10
007800100324     D  D1AUTSDTC             11     11
007900100324     D  D1AUTRETC             12     12
008000100324     D  D1AUTCRSC             13     13
008100100324     D  D1AUTCONC             14     14
008200100324     D  D1RIEABI              15     15
008300100324     D  D1AFFMTC              16     16
008400100324     D  D1AFFSDT              17     17
008500100324     D  D1AFFRET              18     18
008600100324     D  D1AFFCON              19     19
008700100324     D  D1AFFCRS              20     20
008800100324     D  D1AFFGVSTD            21     21
008900100324     D  D1AFFAGV              22     22
009000100324     D  D1AFFCPSTD            23     23
009100100324     D  D1AFFARRM             24     24
009200100324     D  D1AUTCPRRE            25     25
009300100324     D  D1AUTGSTM             26     26
009400100324     D  D1AUTSTDCC            27     27
009500100324     D  dsp_lda                1     27    DIM(27)
009600940201     D*-------------
009700030204     D  S_TASTO        S                   like($Tasto)
009800940201     D DSFMT           DS
009900940506     D  $TASTO               369    369
010000940201     D  NRG                  370    370
010100940201     D  NCL                  371    371
010200940201     D*-------------
010300940201     D* posizione cursore
010400940201     D CURSOR          DS
010500940223     D  RIRI                   1      2B 0 INZ
010600940201     D  R$$                    2      2
010700940223     D  COCO                   3      4B 0 INZ
010800940201     D  C$$                    4      4
010900940207     D*-------------
011000940207     D* Reperimento nome PGM
011100940207     D STATUS         SDS           333
011200940207     D  DSPGM            *PROC
011300940225     D*-------------
011400940225     D* COSTANTI
011500940225     D*-------------
011600940506     D* Tasti di funzione
011700940506     D F01             C                   CONST(X'31')
011800940506     D F02             C                   CONST(X'32')
011900940506     D F03             C                   CONST(X'33')
012000940506     D F04             C                   CONST(X'34')
012100940506     D F05             C                   CONST(X'35')
012200940506     D F06             C                   CONST(X'36')
012300940506     D F07             C                   CONST(X'37')
012400940506     D F08             C                   CONST(X'38')
012500940506     D F09             C                   CONST(X'39')
012600940506     D F10             C                   CONST(X'3A')
012700940506     D F11             C                   CONST(X'3B')
012800940506     D F12             C                   CONST(X'3C')
012900940506     D F13             C                   CONST(X'B1')
013000940506     D F14             C                   CONST(X'B2')
013100940506     D F15             C                   CONST(X'B3')
013200940506     D F16             C                   CONST(X'B4')
013300940506     D F17             C                   CONST(X'B5')
013400940506     D F18             C                   CONST(X'B6')
013500940506     D F19             C                   CONST(X'B7')
013600940506     D F20             C                   CONST(X'B8')
013700940506     D F21             C                   CONST(X'B9')
013800940506     D F22             C                   CONST(X'BA')
013900940506     D F23             C                   CONST(X'BB')
014000940506     D F24             C                   CONST(X'BC')
014100940506     D ENTER           C                   CONST(X'F1')
014200940506     D ROLDWN          C                   CONST(X'F4')
014300940506     D ROLLUP          C                   CONST(X'F5')
014400940506     D*-------------
014500941108     D DATA            C                   CONST('0001-01-01')
014600100322     D OPz_FILTRO      C                   CONST('F I L T R O')
014700940127     C*----------------------------------------------------*
014800940127     C*                MAIN LINE PROGRAM
014900940127     C*----------------------------------------------------*
015000100121     c                   exsr      INIZIO
015100940223     C*
015200940223     C     $FINE         DOWEQ     *OFF
015300940202     C     $GEST         CASEQ     'D1'          GESD1
015400940117     C                   END
015500940117     C                   END
015600940325     C* fine programma
015700030113     C                   Seton                                        LR
015800940131     C************************************************************
015900100121     C* OPERAZIONI INIZIALI
016000100121     C************************************************************
016100100121     C     INIZIO        BEGSR
016200100121     C*
016300100121     C* Reperimento parametri
016400100121     C     *ENTRY        PLIST
016500100322     C                   PARM                    kpjba
016600100322     C*
016700100322     C* definisce la Local Data Area per selezionare i profili
016800100322     C     *DTAARA       DEFINE    *LDA          LDA_Selez
016900100322     C*
017000100121     C* Variabili per gestione videate
017100100121     C                   MOVE      'D1'          $GEST
017200100121     C                   MOVE      *OFF          $FINE
017300100121     C                   MOVE      *ON           $INZD1
017400100121     C*
017500100121     C* Variabili appoggio
017600100121     C                   Z-ADD     0             CURR
017700100121     C                   Z-ADD     0             CURC
017800100121     C*
017900100121     C* Valorizzazione campi univoci testate
018000100121     C                   MOVEL     KNSIF         NOMSIF
018100100121     C                   MOVEL     DSPGM         NOMPGM
018200100121     C*
018300100121     C* Inizializzazione/Decodifica prima videata
018400100121     C                   EXSR      INZD1
018500100121     C                   MOVE      *OFF          $INZD1
018600100121     C*
018700100121     C                   ENDSR
018800100121     C************************************************************
018900100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
019000100121     C************************************************************
019100100121     C     DEFCAM        BEGSR
019200100121     C*
019300100121     C* klist
019400100121      *
019500100121     C* Variabili per gestione videate
019600100121     C*
019700100121     C                   MOVE      *BLANK        $GEST             2
019800100121     C                   MOVE      *BLANK        $FINE             1
019900100121     C                   MOVE      *BLANK        $INZD1            1
020000100121     C*
020100100121     C* Variabili appoggio
020200100121     C                   Z-ADD     0             CURR              2 0
020300100121     C                   Z-ADD     0             CURC              2 0
020400100121     C                   MOVE      *ZEROS        WIN              99
020500100121     C* Indici
020600100121     C                   Z-ADD     0             X                 3 0
020700100121     C                   Z-ADD     0             Y                 3 0
020800100121     C*
020900100121     C                   ENDSR
021000100121     C************************************************************
021100940510     C* GESTIONE VIDEO RECORD D1
021200940131     C************************************************************
021300940127     C     GESD1         BEGSR
021400030113      *
021500940225     C* inizializzazione videata
021600940223     C     $INZD1        IFEQ      *ON
021700940127     C                   EXSR      INZD1
021800940223     C                   MOVE      *OFF          $INZD1
021900940117     C                   END
022000100322     C*
022100100322     c                   movel     Opz_FILTRO    D1Fun
022200940131     C*
022300100322     c                   setoff                                       99
022400940620     C* Immissione/Modifica/Copia
022500030113     C*               *----------------*
022600940620     C                   EXFMT     D1
022700030113     C*               *----------------*
022800030114     C*
022900030114     C     *IN99         IFEQ      *OFF
023000030114     C*
0231000301141    C                   SELECT
023200030114     C* F12=Ritorno
023300030114     C     $TASTO        WHENEQ    F12
023400030114     C                   EXSR      F12D1
023500030114     C*
023600100322     C* F17=SELEZIONA TUTTO
023700100322     C     $TASTO        WHENEQ    F17
023800100322     C                   EXSR      F17D1
023900100322     C*
0240000301141O   C                   OTHER
024100030114     C*
024200030114     C                   EXSR      CTRD1
024300030113     C*
024400940616     C     *IN99         IFEQ      *OFF
024500940616     C* F6
0246000302052    C     $TASTO        ifeq      F06
024700940224     C* eseguo aggiornamento
024800940315    >C                   EXSR      AGGANA
024900940224     C* eseguo operazioni del dopo-aggiornamento
025000030114     C  N99              EXSR      GESAGG
025100030205     c                   end
025200030114     C*
025300030114     C                   ENDIF
025400030114     C*
0255000301142-   C                   ENDSL
025600030114     C*
0257000301142-   C                   ENDIF
025800030114     C*
025900940117     C                   ENDSR
026000940117      ************************************************************
026100940207      * INIZIALIZZAZIONE VIDEATA DATI
026200940117      ************************************************************
026300940127     C     INZD1         BEGSR
026400940131     C*
026500940615     C                   CLEAR                   D1
026600100322     C*
026700100324     c                   goto      salta
026800100324     C*
026900100322     C*   Legge comunque la Local Data Area per riempire i campi a Video
027000100322     c                   in        LDA_Selez
027100100322     C                   eval       D1AUTMTC   =   LDA_AUTMTC
027200100322     C                   eval       D1RIEABI   =   LDA_RIEABI
027300100322     C                   eval       D1AUTSDT   =   LDA_AUTSDT
027400100322     C                   eval       D1AUTRET   =   LDA_AUTRET
027500100322     C                   eval       D1AFFMTC   =   LDA_AFFMTC
027600100322     C                   eval       D1AUTCON   =   LDA_AUTCON
027700100322     C                   eval       D1AFFSDT   =   LDA_AFFSDT
027800100322     C                   eval       D1AUTCRS   =   LDA_AUTCRS
027900100322     C                   eval       D1AFFRET   =   LDA_AFFRET
028000100322     C                   eval       D1AFFCON   =   LDA_AFFCON
028100100322     C                   eval       D1AUTVTCC  =   LDA_AUTVTCC
028200100322     C                   eval       D1AFFCRS   =   LDA_AFFCRS
028300100322     C                   eval       D1AUTMTCC  =   LDA_AUTMTCC
028400100322     C                   eval       D1AUTSDTC  =   LDA_AUTSDTC
028500100322     C                   eval       D1AUTRETC  =   LDA_AUTRETC
028600100322     C                   eval       D1AUTCRSC  =   LDA_AUTCRSC
028700100322     C                   eval       D1AUTCONC  =   LDA_AUTCONC
028800100322     C                   eval       D1AFFGVSTD =   LDA_AFFGVSTD
028900100322     C                   eval       D1AFFAGV   =   LDA_AFFAGV
029000100322     C                   eval       D1AFFCPSTD =   LDA_AFFCPSTD
029100100322     C                   eval       D1AFFARRM  =   LDA_AFFARRM
029200100322     C                   eval       D1AUTCPRRE =   LDA_AUTCPRRE
029300100322     C                   eval       D1AUTGSTM  =   LDA_AUTGSTM
029400100322     C                   eval       D1AUTSTDCC =   LDA_AUTSTDCC
029500100322     C                   eval       D1AUT      =   LDA_AUT
029600100322     C                   eval       D1COOP     =   LDA_COOP
029700100322     C                   eval       D1AFDE     =   LDA_AFDE
029800100322     C*   poi la rilascia
029900100322     c                   out       LDA_Selez
030000100324     C*
030100100324     c     salta         tag
030200100324     C*
030300100322     c     ' '           lookup    lda_lda                                17
030400100322     C*
030500100322     C*
030600940117     C                   ENDSR
030700940207     C/EJECT
030800940309     C************************************************************
030900940309     C* GESTIONE F12 VIDEO D1
031000940309     C************************************************************
031100940309     C     F12D1         BEGSR
031200940309     C*
031300940309     C                   MOVE      *ON           $FINE
031400940309     C*
031500940309     C                   ENDSR
031600100322     C/EJECT
031700100322     C************************************************************
031800100322     C* GESTIONE F17 VIDEO D1
031900100322     C************************************************************
032000100322     C     F17D1         BEGSR
032100100322     C*
032200100324     C   17              MOVE      *ALL'S'       dsp_LDA
032300100324     C  n17              MOVE      *ALL' '       dsp_LDA
032400100322     C*
032500100324     c                   if        *in17
032600100324     c                   setoff                                       17
032700100324     c                   else
032800100324     c                   seton                                        17
032900100324     c                   end
033000100324     C*
033100100322     C                   ENDSR
033200940131     C************************************************************
033300940207     C* CONTROLLO VIDEATA
033400940131     C************************************************************
033500940127     C     CTRD1         BEGSR
033600940131     C*
033700100322     C                   MOVE      *ALL'0'       IN4098           49
033800100322     C                   MOVEA     IN4098        *IN(40)
033900940127     C                   SETOFF                                       99
034000030123      *
034100940127     C* non imposto pos. del cursore
034200940127     C                   Z-ADD     0             H1RIGA
034300940127     C                   Z-ADD     0             H1COLO
034400100322     C*
034500100322     c     'S'           lookup    dsp_lda                                21
034600100322     c                   if        not *IN21
034700100322     c                   seton                                        41  99
034800100322     c                   end
034900100322      *
035000100322      *  x eventuali decodifiche
035100940302     C     *IN99         IFEQ      *ON
035200940510     C     $GEST         ANDEQ     'D1'
035300940117     C                   MOVEA     *IN           WIN
035400100322     C                   MOVE      *ALL'0'       IN4098           49
035500100322     C                   MOVEA     IN4098        *IN(40)
035600940614     C                   WRITE     D1
035700940117     C                   MOVEA     WIN           *IN
035800940302     C                   ENDIF
035900940127     C*
036000940117     C                   ENDSR
036100030204     C/EJECT
036200940224     C************************************************************
036300940315    >C* AGGIORNAMENTO ANAGRAFICA
036400940224     C************************************************************
036500940315    >C     AGGANA        BEGSR
036600100322     C*
036700100322     C*   Legge comunque la Local Data Area per riempire i campi a Video
036800100322     c                   in        LDA_Selez
036900100322     C                   eval       LDA_AUTMTC  =   D1AUTMTC
037000100322     C                   eval       LDA_RIEABI  =   D1RIEABI
037100100322     C                   eval       LDA_AUTSDT  =   D1AUTSDT
037200100322     C                   eval       LDA_AUTRET   =   D1AUTRET
037300100322     C                   eval       LDA_AFFMTC   =   D1AFFMTC
037400100322     C                   eval       LDA_AUTCON   =   D1AUTCON
037500100322     C                   eval       LDA_AFFSDT   =   D1AFFSDT
037600100322     C                   eval       LDA_AUTCRS   =   D1AUTCRS
037700100322     C                   eval       LDA_AFFRET   =   D1AFFRET
037800100322     C                   eval       LDA_AFFCON   =   D1AFFCON
037900100322     C                   eval       LDA_AUTVTCC  =   D1AUTVTCC
038000100322     C                   eval       LDA_AFFCRS   =   D1AFFCRS
038100100322     C                   eval       LDA_AUTMTCC  =   D1AUTMTCC
038200100322     C                   eval       LDA_AUTSDTC  =   D1AUTSDTC
038300100322     C                   eval       LDA_AUTRETC  =   D1AUTRETC
038400100322     C                   eval       LDA_AUTCRSC  =   D1AUTCRSC
038500100322     C                   eval       LDA_AUTCONC  =   D1AUTCONC
038600100322     C                   eval       LDA_AFFGVSTD =   D1AFFGVSTD
038700100322     C                   eval       LDA_AFFAGV   =   D1AFFAGV
038800100322     C                   eval       LDA_AFFCPSTD =   D1AFFCPSTD
038900100322     C                   eval       LDA_AFFARRM  =   D1AFFARRM
039000100322     C                   eval       LDA_AUTCPRRE =   D1AUTCPRRE
039100100322     C                   eval       LDA_AUTGSTM  =   D1AUTGSTM
039200100322     C                   eval       LDA_AUTSTDCC =   D1AUTSTDCC
039300100322     C                   eval       LDA_AUT      =   D1AUT
039400100322     C                   eval       LDA_COOP     =   D1COOP
039500100322     C                   eval       LDA_AFDE     =   D1AFDE
039600100322     C*   poi la aggiorna con le selezioni fatte a video
039700100322     c                   out       LDA_Selez
039800940203     C*
039900940203     C                   ENDSR
040000940224     C************************************************************
040100940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
040200940224     C************************************************************
040300940224     C     GESAGG        BEGSR
040400940224     C*
040500100322     C                   MOVE      *ON           $FINE
040600940224     C*
040700940224     C                   ENDSR
040800940131     C************************************************************
