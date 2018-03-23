000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400940223      *
000500940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000600940620      *  03           PROTECT TUTTI I CAMPI
000700030123      *  09           PROTECT dei campi voce C/E
000800940223      *  21           GENERICO OPERAZIONI I/O
000900940223      *  22           GENERICO ERRORE OPERAZIONI I/O
001000940223      *  30           SFLDSP
001100940223      * N31           SFLCLR
001200940128      *  31           SFLDSPCTL
001300940128      *  32           SFLNXTCHG
001400940128      *  33           SFLEND
001500940128      *  39           OF PRTF
001600940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001700940317    > *  Specificare l'uso dei singoli indicatori
001800940315      *  50 <---> 98  ERRORI SU VIDEO
001900940317    > *  Specificare l'uso dei singoli indicatori
002000940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002100940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002200940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002300940128      *  99           INDIC. GENERALE DI ERRORE
002400940128     F*----------------------------------------------------*
002500090225$001 FTRTCTB1D2 CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700090225     FEDSTBL02L UF A E           K DISK
002800090226     FEDtab01l  iF   E           K DISK
002900940128     D*----------------------------------------------------*
003000940127     D* Passaggio Parametri
003100940127     D KPJBA         E DS
003200030113      *-------------
003300030113     D* Parametri in ricezione
003400030113     D  TABDS          DS
003500030113     D  XTAOPZ                 1      2
003600030113     D  XTARET                 3      3
003700030113     D  XTAOPR                 4      4
003800030113     D  XTAERR                 5      5
003900030113     D  XTAKEY                 6     13
004000940201     D*-------------
004100030204     D  S_TASTO        S                   like($Tasto)
004200940201     D DSFMT           DS
004300940506     D  $TASTO               369    369
004400940201     D  NRG                  370    370
004500940201     D  NCL                  371    371
004600940201     D*-------------
004700940201     D* posizione cursore
004800940201     D CURSOR          DS
004900940223     D  RIRI                   1      2B 0 INZ
005000940201     D  R$$                    2      2
005100940223     D  COCO                   3      4B 0 INZ
005200940201     D  C$$                    4      4
005300940207     D*-------------
005400940207     D* Reperimento nome PGM
005500940207     D STATUS         SDS           333
005600940207     D  DSPGM            *PROC
005700940225     D*-------------
005800940225     D* COSTANTI
005900940225     D*-------------
006000940506     D* Tasti di funzione
006100940506     D F01             C                   CONST(X'31')
006200940506     D F02             C                   CONST(X'32')
006300940506     D F03             C                   CONST(X'33')
006400940506     D F04             C                   CONST(X'34')
006500940506     D F05             C                   CONST(X'35')
006600940506     D F06             C                   CONST(X'36')
006700940506     D F07             C                   CONST(X'37')
006800940506     D F08             C                   CONST(X'38')
006900940506     D F09             C                   CONST(X'39')
007000940506     D F10             C                   CONST(X'3A')
007100940506     D F11             C                   CONST(X'3B')
007200940506     D F12             C                   CONST(X'3C')
007300940506     D F13             C                   CONST(X'B1')
007400940506     D F14             C                   CONST(X'B2')
007500940506     D F15             C                   CONST(X'B3')
007600940506     D F16             C                   CONST(X'B4')
007700940506     D F17             C                   CONST(X'B5')
007800940506     D F18             C                   CONST(X'B6')
007900940506     D F19             C                   CONST(X'B7')
008000940506     D F20             C                   CONST(X'B8')
008100940506     D F21             C                   CONST(X'B9')
008200940506     D F22             C                   CONST(X'BA')
008300940506     D F23             C                   CONST(X'BB')
008400940506     D F24             C                   CONST(X'BC')
008500940506     D ENTER           C                   CONST(X'F1')
008600940506     D ROLDWN          C                   CONST(X'F4')
008700940506     D ROLLUP          C                   CONST(X'F5')
008800940506     D*-------------
008900941108     D DATA            C                   CONST('0001-01-01')
009000030114     D Annull          C                   CONST('  ANNULLATO  ')
009100030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009200030114     D OPz_Var         C                   CONST('VARIAZIONE')
009300030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009400030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009500940127     C*----------------------------------------------------*
009600940127     C*                MAIN LINE PROGRAM
009700940127     C*----------------------------------------------------*
009800940223     C*
009900940223     C     $FINE         DOWEQ     *OFF
010000940202     C     $GEST         CASEQ     'D1'          GESD1
010100940117     C                   END
010200940117     C                   END
010300940325     C* fine programma
010400030113     c                   movel     tabds         kpjbu
010500030113     C                   Seton                                        LR
010600940131     C************************************************************
010700940510     C* GESTIONE VIDEO RECORD D1
010800940131     C************************************************************
010900940127     C     GESD1         BEGSR
011000030113      *
011100940225     C* inizializzazione videata
011200940223     C     $INZD1        IFEQ      *ON
011300940127     C                   EXSR      INZD1
011400940223     C                   MOVE      *OFF          $INZD1
011500940117     C                   END
011600030113      *
011700940225     C* emissione piede videata se proveniente da altra
011800940225     C* salvataggio valore formato di provenienza
011900940207     C     $LASTV        IFNE      'D1'
012000940614     C                   WRITE     T1
012100940202     C                   MOVE      $LASTV        $LASTG
012200940202     C                   MOVE      'D1'          $LASTV
012300940202     C                   END
012400940131     C*
012500940203     C                   SELECT
012600940203     C* Annullamento
012700030114     C* Visualizzazione
012800030114    >C     xtaopz        WHENEQ    '01'
012900030114     c                   movel     Opz_Imm       D1Fun
013000030114    >C     xtaopz        WHENEQ    '05'
013100030114     c                   movel     Opz_Vis       D1Fun
013200030114     C                   SETON                                        020306
013300030114    >C     xtaopz        WHENEQ    '04'
013400030114     c                   movel     Opz_Del       D1Fun
013500030114     C                   SETON                                        0203
013600030114     C                   SETOFF                                         06
013700940620     C* Manutenzione
013800030113    >C     xtaopz        WHENEQ    '02'
013900030114     c                   movel     Opz_Var       D1Fun
014000030114     C                   SETON                                        02
014100030114     C                   SETOFF                                       0306
014200940620     C*
014300940620     C                   OTHER
014400090226     C*
014500030114     C                   SETOFF                                       020306
014600940203     C                   ENDSL
014700030114     C*
014800030114     c                   SETOFF                                         99
014900940620     C* Immissione/Modifica/Copia
015000030113     C*               *----------------*
015100940620     C                   EXFMT     D1
015200030113     C*               *----------------*
015300030114     C*
015400030114     C     *IN99         IFEQ      *OFF
015500030114     C*
0156000301141    C                   SELECT
015700030114     C* F3=Fine
015800030114     C     $TASTO        WHENEQ    F03
015900030114     C                   EXSR      F03D1
016000030114     C* F12=Ritorno
016100030114     C     $TASTO        WHENEQ    F12
016200030114     C                   EXSR      F12D1
016300030114     C*
0164000301141O   C                   OTHER
016500030114     C*
016600030114     C                   EXSR      CTRD1
016700030113     C*
016800940616     C     *IN99         IFEQ      *OFF
016900940616     C* F6
0170000302052    C     $TASTO        ifeq      F06
0171000301152    C     $TASTO        oreq      F23
017200940224     C* eseguo aggiornamento
017300940315    >C                   EXSR      AGGANA
017400940224     C* eseguo operazioni del dopo-aggiornamento
017500030114     C  N99              EXSR      GESAGG
017600030205     c                   end
017700030114     C*
017800030114     C                   ENDIF
017900030114     C*
0180000301142-   C                   ENDSL
018100030114     C*
0182000301142-   C                   ENDIF
018300030114     C*
018400940117     C                   ENDSR
018500940117      ************************************************************
018600940207      * INIZIALIZZAZIONE VIDEATA DATI
018700940117      ************************************************************
018800940127     C     INZD1         BEGSR
018900940131     C*
019000940615     C                   CLEAR                   D1
019100090226     C                   move      KEYsegmento   d1SEG
019200090226     C                   MOVEl     KEYTABella    D1COD
019300090226     C                   MOVEl     KEYCliente    d1UNB
019400090226     C                   MOVEl     KEYTABella    h1COD
019500090226     C                   MOVEl     KEYCliente    h1UNB
019600940207     C*
019700940224     C* valorizzo la chiave univoca per reperire i dati
019800940224     C* se non immissione
0199000301141   >C     xtaopz        IFNE      '01'
020000030204      *
020100940315    >C*-------------------------------------------------------------
020200940315    >C* impostare i campi chiave ricevuti
020300940315    >C*-------------------------------------------------------------
020400090226     C     K_edsTBL2     chain(N)  edstbl02l
020500940614     C*
020600090226     C                   IF        %Found(edstbl02l)
020700030113     C*
020800940224     C* riempio la videata
020900940224     C                   EXSR      RIED1
021000030113      *
021100940224     C* richiamo routine dei ctrl per decodificarla
021200940614    >C                   EXSR      CTRD1
021300030113      *
021400940224     C                   MOVE      *ALL'0'       IN5098
021500940224     C                   MOVEA     IN5098        *IN(50)
021600030113      *
021700940224     C* definizione indicatori su video
021800940224     C                   SETOFF                                       02
0219009402243    C                   SELECT
022000030114    >C     xtaopz        WHENEQ    '02'
022100940224     C                   SETON                                        02
022200030114    >C     xtaopz        WHENEQ    '05'
0223009402243-   C                   ENDSL
022400940224     C*
0225009402242-   C                   ENDIF
022600030114     C*
0227000301141   >C                   Else
022800030114     C* Se in Immissione nuovo record
022900030114     c                   exsr      NewD1
023000940223     C*
0231009402241-   C                   ENDIF
023200940224     C*
023300940117     C                   ENDSR
023400940207     C/EJECT
023500940207     C************************************************************
023600940510     C* RIEMPIMENTO VIDEATA  D1
023700940207     C************************************************************
023800030114     C     NEWD1         BEGSR
023900940207     C*
024000030114     c                   clear                   d1
024100090226     C                   move      KEYsegmento   d1SEG
024200030114     C*
024300030114     C                   ENDSR
024400030114     C************************************************************
024500030114     C* RIEMPIMENTO VIDEATA  D1
024600030114     C************************************************************
024700030114     C     RIED1         BEGSR
024800030114     C*
024900090226     C                   MOVEL     ETBUNB        D1unb
025000090226     C                   MOVEL     ETBSGM        D1seg
025100090226     C                   MOVEL     ETBVALSGM     D1cod
025200090226     C                   MOVEL     ETBDESCRI     D1des
025300090226     C                   MOVEL     ETBDATI       D1vab1
025400090226     C                   MOVE      ETBDATI       D1vab2
025500030204     C*
025600940207     C                   ENDSR
025700940309     C************************************************************
025800940309     C* GESTIONE F03 VIDEO D1
025900940309     C************************************************************
026000940309     C     F03D1         BEGSR
026100940309     C*
026200940309     C                   MOVE      *ON           $FINE
026300030113    >C                   MOVE      '1'           xtaret
026400940325     C*
026500940325     C                   ENDSR
026600940309     C************************************************************
026700940309     C* GESTIONE F12 VIDEO D1
026800940309     C************************************************************
026900940309     C     F12D1         BEGSR
027000940309     C*
027100940309     C                   MOVE      *ON           $FINE
027200940309     C*
027300940309     C                   ENDSR
027400940131     C************************************************************
027500940207     C* CONTROLLO VIDEATA
027600940131     C************************************************************
027700940127     C     CTRD1         BEGSR
027800940131     C*
027900940127     C                   SETOFF                                       99
028000030123      *
028100940127     C* non imposto pos. del cursore
028200940127     C                   Z-ADD     0             H1RIGA
028300940127     C                   Z-ADD     0             H1COLO
028400940621     C*
028500940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0286000301141   >C     xtaopz        IFNE      '04'
028700971110     C*
028800971110     C* controllo codice vuoto
028900971110$017 C     D1COD         IFEQ      *ALL' '
029000971110     C                   SETON                                        5099
029100090226     C                   Else
029200090226     C*** se già presente
029300090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
029400090226    >C     xtaopz        IFEQ      '01'
029500090226    >C     xtaopz        oreq      '03'
029600090226     c                   movel     d1seg         KEYsegmento
029700090226     c                   MOVEl     D1COD         KEYTABella
029800090226$007 c                   MOVE      d1unb         KEYCliente
029900090226     C     K_edstbl2     chain(N)  edstbl02L
030000090226     c                   if        %Found(edstbl02L)
030100090226     C                   SETON                                        5199
030200090226     C                   EndIF
030300090226     C                   ENDIF
030400090226     C                   ENDIF
030500030114     C*
030600030114     C* controllo codice vuoto
030700090226$017 C     D1vab1        IFEQ      *ALL' '
030800090226$017 C     D1vab2        andEQ     *ALL' '
030900090226     C                   SETON                                        5299
031000030114     C                   ENDIF
031100090226     C*
031200090226     C* controllo codice vuoto
031300090226$017 C     D1DES         IFEQ      *ALL' '
031400090226     C                   SETON                                        5399
031500090226     C                   ENDIF
031600090226     C*
031700090226     C* controllo codice vuoto
031800090226$017 C     D1unb         IFne      *blank
031900090226     c                   eval      tabcod = 'PT'
032000090226     c                   eval      tabkey = d1unb
032100090226     C     k_edtab       chain     edtab01l
032200090226     c                   if        not %Found(edtab01l)
032300090226     C**** se sbagliato il codice UNB
032400090226     C                   SETON                                        5499
032500090226     c                   end
032600090226     C                   ENDIF
032700030114      *
0328009402241-   C                   END
032900940131     C*
033000940302     C     *IN99         IFEQ      *ON
033100940510     C     $GEST         ANDEQ     'D1'
033200940117     C                   MOVEA     *IN           WIN
033300940128     C                   MOVE      *ALL'0'       IN5098           49
033400940128     C                   MOVEA     IN5098        *IN(50)
033500940614     C                   WRITE     D1
033600940117     C                   MOVEA     WIN           *IN
033700940302     C                   ENDIF
033800940127     C*
033900940117     C                   ENDSR
034000030204     C/EJECT
034100030204     C************************************************************
034200030204    >C* AGGIORNAMENTO ANAGRAFICA
034300030204     C************************************************************
034400030204    >C     FLDAGG        BEGSR
034500030204     C*
034600030204     C*  campi chiave da scrivere e aggiornare
034700090226$007 C                   MOVE      d1unb         ETBUNB
034800090226     C                   movel     d1seg         ETBSGM
034900090226     C                   MOVEl     D1COD         ETBVALSGM
035000090226     C*
035100090226     C                   eval      KEYTABella  = h1cod
035200090226     C                   eval      KEYCliente  = h1unb
035300030204     C*
035400030204     C                   ENDSR
035500940207     C/EJECT
035600940224     C************************************************************
035700940315    >C* AGGIORNAMENTO ANAGRAFICA
035800940224     C************************************************************
035900940315    >C     AGGANA        BEGSR
036000940207     C*
036100030204     C                   exsr      FLDAGG
036200030204     C*
036300940207     C                   SELECT
036400030204     C*
036500030114    >C     xtaopz        WHENEQ    '01'
036600030114    >C     xtaopz        OREQ      '03'
036700030204     C*
036800090226    >C                   EXSR      RIEANA
036900090226     c                   write     edstbl000                            59
037000940318     C* se rcd non scrivibile attivo errore
037100030114    >C   59              SETON                                          99
037200030204      *
037300030114    >C     xtaopz        WHENEQ    '02'
037400030204      *
037500090226     C     K_edsTBL2     chain     edstbl02L
037600030204     C                   exsr      FLDAGG
037700090226    >C                   EXSR      RIEANA
037800090226     c                   update    edstbl000                            59
037900030114    >C   59              SETON                                          99
038000030204      *
038100030114    >C     xtaopz        WHENEQ    '04'
038200090226     C     K_edsTBL2     chain     edstbl02L
038300090226     c                   delete    edstbl000                            59
038400030114    >C   59              SETON                                          99
038500940621     C*
038600940207     C                   ENDSL
038700940203     C*
038800940203     C                   ENDSR
038900940224     C************************************************************
039000940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
039100940224     C************************************************************
039200090226     C     RIEANA        BEGSR
039300030114     C*
039400090226     c                   movel     d1des         ETBDESCRI
039500090226     c                   movel     d1vab1        ETBDATI
039600090226     c                   move      d1vab2        ETBDATI
039700030114     C*
039800940207     C                   ENDSR
039900940224     C************************************************************
040000940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
040100940224     C************************************************************
040200940224     C     GESAGG        BEGSR
040300940224     C*
040400940224     C* segnalo al pgm chiamante l'avvenuta conferma
040500030114    >C                   MOVE      *ON           xtaopr
040600940224     C* segnalo al pgm chiamante l'aver premuto l'F6
040700030114    >C                   MOVE      '0'           xtaret
040800940224     C*
0409009402241    C                   SELECT
041000940224     C* nel caso di immissione, ripristino la videata iniziale
041100030114    >C     xtaopz        WHENEQ    '01'
041200940224     C                   MOVE      *ON           $INZD1
041300030114     C                   MOVE      *ON           $FINE
041400940224     C* altrimenti ritorno al pgm chiamante
041500940224     C                   OTHER
041600940224     C                   MOVE      *ON           $FINE
0417009402241-   C                   ENDSL
041800940224     C*
041900940224     C                   ENDSR
042000940131     C************************************************************
042100940131     C* OPERAZIONI INIZIALI
042200940131     C************************************************************
042300940131     C     *INZSR        BEGSR
042400940131     C*
042500940127     C* Reperimento parametri
042600940117     C     *ENTRY        PLIST
042700940117     C                   PARM                    KPJBA
042800090225     C                   PARM                    KEYsegmento       3
042900090225     C                   PARM                    KEYTABella       35
043000090225     C                   PARM                    KEYCliente       35
043100090225      *
043200030113     C                   movel     kpjbu         tabds
043300030115     c                   setoff                                       08
043400030115     C*
043500940223     C* Variabili per gestione videate
043600940223     C                   MOVE      'D1'          $GEST
043700940223     C                   MOVE      *OFF          $FINE
043800940223     C                   MOVE      *ON           $INZD1
043900940224     C                   MOVE      *BLANK        $LASTG
044000940224     C                   MOVE      *BLANK        $LASTV
044100940506     C                   Z-ADD     0             $ULKD1            3 0
044200940127     C*
044300940223     C* Variabili appoggio
044400940224     C                   Z-ADD     0             CURR
044500940224     C                   Z-ADD     0             CURC
044600940207     C*
044700940207     C* Valorizzazione campi univoci testate
044800940614     C                   CLEAR                   T1
044900940614     C                   MOVEL     KNSIF         NOMSIF
045000030113     C******             MOVEL     XSCDSI        NOMDIT
045100941027     C                   MOVEL     DSPGM         NOMPGM
045200940224     C*
045300940224     C* Inizializzazione/Decodifica prima videata
045400940224     C                   EXSR      INZD1
045500940224     C                   MOVE      *OFF          $INZD1
045600940127     C*
045700940117     C                   ENDSR
045800940128     C************************************************************
045900940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
046000940223     C************************************************************
046100940223     C     DEFCAM        BEGSR
046200940223     C*
046300940223     C* klist
046400090226     C     k_edstbl2     kLIST
046500090226     C                   kfld                    KEYsegmento       3
046600090226     C                   kfld                    KEYTABella       35
046700090226     C                   kfld                    KEYCliente       35
046800090226      *
046900090226     C     k_edtab       kLIST
047000090226     C                   kfld                    tabcod
047100090226     C                   kfld                    tabkey
047200940207     C*
047300940223     C* Variabili per gestione videate
047400940223     C*
047500940223     C                   MOVE      *BLANK        $GEST             2
047600940223     C                   MOVE      *BLANK        $FINE             1
047700940223     C                   MOVE      *BLANK        $INZD1            1
047800940223     C                   MOVE      *BLANK        $LASTG            2
047900940223     C                   MOVE      *BLANK        $LASTV            2
048000940223     C*
048100940223     C* Variabili appoggio
048200940223     C                   Z-ADD     0             CURR              2 0
048300940223     C                   Z-ADD     0             CURC              2 0
048400940223     C                   MOVE      *ZEROS        WIN              99
048500940224     C                   MOVE      *BLANK        WTBERR            1
048600940223     C*
048700940223     C* Indici
048800940223     C                   Z-ADD     0             X                 3 0
048900940223     C                   Z-ADD     0             Y                 3 0
049000940127     C*
049100940117     C                   ENDSR
049200940128     C************************************************************
