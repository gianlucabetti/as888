000100970214     H DECEDIT('0,') DATEDIT(*DMY.)
000200940223      *
000300940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000400940620      *  03           PROTECT TUTTI I CAMPI
000500030123      *  09           PROTECT dei campi voce C/E
000600940223      *  21           GENERICO OPERAZIONI I/O
000700940223      *  22           GENERICO ERRORE OPERAZIONI I/O
000800940223      *  30           SFLDSP
000900940223      * N31           SFLCLR
001000940128      *  31           SFLDSPCTL
001100940128      *  32           SFLNXTCHG
001200940128      *  33           SFLEND
001300940128      *  39           OF PRTF
001400940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001500940317    > *  Specificare l'uso dei singoli indicatori
001600940315      *  50 <---> 98  ERRORI SU VIDEO
001700940317    > *  Specificare l'uso dei singoli indicatori
001800940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
001900940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002000940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002100940128      *  99           INDIC. GENERALE DI ERRORE
002200940128     F*----------------------------------------------------*
002300090225$001 FTRTCTB1D2 CF   E             WORKSTN
002400940201     F                                     INFDS(DSFMT)
002500090225     FEDSTBL02L UF A E           K DISK
002600090226     FEDtab01l  iF   E           K DISK
002700940128     D*----------------------------------------------------*
002800940127     D* Passaggio Parametri
002900940127     D KPJBA         E DS
003000030113      *-------------
003100030113     D* Parametri in ricezione
003200030113     D  TABDS          DS
003300030113     D  XTAOPZ                 1      2
003400030113     D  XTARET                 3      3
003500030113     D  XTAOPR                 4      4
003600030113     D  XTAERR                 5      5
003700030113     D  XTAKEY                 6     13
003800940201     D*-------------
003900030204     D  S_TASTO        S                   like($Tasto)
004000940201     D DSFMT           DS
004100940506     D  $TASTO               369    369
004200940201     D  NRG                  370    370
004300940201     D  NCL                  371    371
004400940201     D*-------------
004500940201     D* posizione cursore
004600940201     D CURSOR          DS
004700940223     D  RIRI                   1      2B 0 INZ
004800940201     D  R$$                    2      2
004900940223     D  COCO                   3      4B 0 INZ
005000940201     D  C$$                    4      4
005100940207     D*-------------
005200940207     D* Reperimento nome PGM
005300940207     D STATUS         SDS           333
005400940207     D  DSPGM            *PROC
005500940225     D*-------------
005600940225     D* COSTANTI
005700940225     D*-------------
005800940506     D* Tasti di funzione
005900940506     D F01             C                   CONST(X'31')
006000940506     D F02             C                   CONST(X'32')
006100940506     D F03             C                   CONST(X'33')
006200940506     D F04             C                   CONST(X'34')
006300940506     D F05             C                   CONST(X'35')
006400940506     D F06             C                   CONST(X'36')
006500940506     D F07             C                   CONST(X'37')
006600940506     D F08             C                   CONST(X'38')
006700940506     D F09             C                   CONST(X'39')
006800940506     D F10             C                   CONST(X'3A')
006900940506     D F11             C                   CONST(X'3B')
007000940506     D F12             C                   CONST(X'3C')
007100940506     D F13             C                   CONST(X'B1')
007200940506     D F14             C                   CONST(X'B2')
007300940506     D F15             C                   CONST(X'B3')
007400940506     D F16             C                   CONST(X'B4')
007500940506     D F17             C                   CONST(X'B5')
007600940506     D F18             C                   CONST(X'B6')
007700940506     D F19             C                   CONST(X'B7')
007800940506     D F20             C                   CONST(X'B8')
007900940506     D F21             C                   CONST(X'B9')
008000940506     D F22             C                   CONST(X'BA')
008100940506     D F23             C                   CONST(X'BB')
008200940506     D F24             C                   CONST(X'BC')
008300940506     D ENTER           C                   CONST(X'F1')
008400940506     D ROLDWN          C                   CONST(X'F4')
008500940506     D ROLLUP          C                   CONST(X'F5')
008600940506     D*-------------
008700941108     D DATA            C                   CONST('0001-01-01')
008800030114     D Annull          C                   CONST('  ANNULLATO  ')
008900030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009000030114     D OPz_Var         C                   CONST('VARIAZIONE')
009100030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009200030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009300940127     C*----------------------------------------------------*
009400940127     C*                MAIN LINE PROGRAM
009500940127     C*----------------------------------------------------*
009600940223     C*
009700940223     C     $FINE         DOWEQ     *OFF
009800940202     C     $GEST         CASEQ     'D1'          GESD1
009900940117     C                   END
010000940117     C                   END
010100940325     C* fine programma
010200030113     c                   movel     tabds         kpjbu
010300030113     C                   Seton                                        LR
010400940131     C************************************************************
010500940510     C* GESTIONE VIDEO RECORD D1
010600940131     C************************************************************
010700940127     C     GESD1         BEGSR
010800030113      *
010900940225     C* inizializzazione videata
011000940223     C     $INZD1        IFEQ      *ON
011100940127     C                   EXSR      INZD1
011200940223     C                   MOVE      *OFF          $INZD1
011300940117     C                   END
011400030113      *
011500940225     C* emissione piede videata se proveniente da altra
011600940225     C* salvataggio valore formato di provenienza
011700940207     C     $LASTV        IFNE      'D1'
011800940614     C                   WRITE     T1
011900940202     C                   MOVE      $LASTV        $LASTG
012000940202     C                   MOVE      'D1'          $LASTV
012100940202     C                   END
012200940131     C*
012300940203     C                   SELECT
012400940203     C* Annullamento
012500030114     C* Visualizzazione
012600030114    >C     xtaopz        WHENEQ    '01'
012700030114     c                   movel     Opz_Imm       D1Fun
012800030114    >C     xtaopz        WHENEQ    '05'
012900030114     c                   movel     Opz_Vis       D1Fun
013000030114     C                   SETON                                        020306
013100030114    >C     xtaopz        WHENEQ    '04'
013200030114     c                   movel     Opz_Del       D1Fun
013300030114     C                   SETON                                        0203
013400030114     C                   SETOFF                                         06
013500940620     C* Manutenzione
013600030113    >C     xtaopz        WHENEQ    '02'
013700030114     c                   movel     Opz_Var       D1Fun
013800030114     C                   SETON                                        02
013900030114     C                   SETOFF                                       0306
014000940620     C*
014100940620     C                   OTHER
014200090226     C*
014300030114     C                   SETOFF                                       020306
014400940203     C                   ENDSL
014500100715     C*
014600100715     C                   SETOFF                                         72
014700100715$017 C     D1vabfld      IFNE      *ALL' '
014800100715     C                   SETON                                          72
014900100715     C                   ENDIF
015000030114     C*
015100030114     c                   SETOFF                                         99
015200940620     C* Immissione/Modifica/Copia
015300030113     C*               *----------------*
015400940620     C                   EXFMT     D1
015500030113     C*               *----------------*
015600030114     C*
015700030114     C     *IN99         IFEQ      *OFF
015800030114     C*
0159000301141    C                   SELECT
016000030114     C* F3=Fine
016100030114     C     $TASTO        WHENEQ    F03
016200030114     C                   EXSR      F03D1
016300030114     C* F12=Ritorno
016400030114     C     $TASTO        WHENEQ    F12
016500030114     C                   EXSR      F12D1
016600030114     C*
0167000301141O   C                   OTHER
016800030114     C*
016900030114     C                   EXSR      CTRD1
017000030113     C*
017100940616     C     *IN99         IFEQ      *OFF
017200940616     C* F6
0173000302052    C     $TASTO        ifeq      F06
0174000301152    C     $TASTO        oreq      F23
017500940224     C* eseguo aggiornamento
017600940315    >C                   EXSR      AGGANA
017700940224     C* eseguo operazioni del dopo-aggiornamento
017800030114     C  N99              EXSR      GESAGG
017900030205     c                   end
018000030114     C*
018100030114     C                   ENDIF
018200030114     C*
0183000301142-   C                   ENDSL
018400030114     C*
0185000301142-   C                   ENDIF
018600030114     C*
018700940117     C                   ENDSR
018800940117      ************************************************************
018900940207      * INIZIALIZZAZIONE VIDEATA DATI
019000940117      ************************************************************
019100940127     C     INZD1         BEGSR
019200940131     C*
019300940615     C                   CLEAR                   D1
019400090226     C                   move      KEYsegmento   d1SEG
019500090226     C                   MOVEl     KEYTABella    D1COD
019600090226     C                   MOVEl     KEYCliente    d1UNB
019700090226     C                   MOVEl     KEYTABella    h1COD
019800090226     C                   MOVEl     KEYCliente    h1UNB
019900940207     C*
020000940224     C* valorizzo la chiave univoca per reperire i dati
020100940224     C* se non immissione
0202000301141   >C     xtaopz        IFNE      '01'
020300030204      *
020400940315    >C*-------------------------------------------------------------
020500940315    >C* impostare i campi chiave ricevuti
020600940315    >C*-------------------------------------------------------------
020700090226     C     K_edsTBL2     chain(N)  edstbl02l
020800940614     C*
020900090226     C                   IF        %Found(edstbl02l)
021000030113     C*
021100940224     C* riempio la videata
021200940224     C                   EXSR      RIED1
021300100715      * se strutturale cambia in sola visualizzazione
021400100715    >C   72              eval       xtaopz = '05'
021500030113      *
021600940224     C* richiamo routine dei ctrl per decodificarla
021700940614    >C                   EXSR      CTRD1
021800030113      *
021900940224     C                   MOVE      *ALL'0'       IN5098
022000940224     C                   MOVEA     IN5098        *IN(50)
022100030113      *
022200940224     C* definizione indicatori su video
022300940224     C                   SETOFF                                       02
0224009402243    C                   SELECT
022500030114    >C     xtaopz        WHENEQ    '02'
022600940224     C                   SETON                                        02
022700030114    >C     xtaopz        WHENEQ    '05'
0228009402243-   C                   ENDSL
022900940224     C*
0230009402242-   C                   ENDIF
023100030114     C*
0232000301141   >C                   Else
023300030114     C* Se in Immissione nuovo record
023400030114     c                   exsr      NewD1
023500940223     C*
0236009402241-   C                   ENDIF
023700940224     C*
023800940117     C                   ENDSR
023900940207     C/EJECT
024000940207     C************************************************************
024100940510     C* RIEMPIMENTO VIDEATA  D1
024200940207     C************************************************************
024300030114     C     NEWD1         BEGSR
024400940207     C*
024500030114     c                   clear                   d1
024600090226     C                   move      KEYsegmento   d1SEG
024700030114     C*
024800030114     C                   ENDSR
024900030114     C************************************************************
025000030114     C* RIEMPIMENTO VIDEATA  D1
025100030114     C************************************************************
025200030114     C     RIED1         BEGSR
025300030114     C*
025400090226     C                   MOVEL     ETBUNB        D1unb
025500090226     C                   MOVEL     ETBSGM        D1seg
025600090226     C                   MOVEL     ETBVALSGM     D1cod
025700090226     C                   MOVEL     ETBDESCRI     D1des
025800100715     C                   MOVE      ETBdescri     D1vabFLD
025900100715     C                   SETOFF                                         72
026000100715$017 C     D1vabfld      IFNE      *ALL' '
026100100715     C                   SETON                                          72
026200100715     C                   ENDIF
026300090226     C                   MOVEL     ETBDATI       D1vab1
026400090226     C                   MOVE      ETBDATI       D1vab2
026500030204     C*
026600940207     C                   ENDSR
026700940309     C************************************************************
026800940309     C* GESTIONE F03 VIDEO D1
026900940309     C************************************************************
027000940309     C     F03D1         BEGSR
027100940309     C*
027200940309     C                   MOVE      *ON           $FINE
027300030113    >C                   MOVE      '1'           xtaret
027400940325     C*
027500940325     C                   ENDSR
027600940309     C************************************************************
027700940309     C* GESTIONE F12 VIDEO D1
027800940309     C************************************************************
027900940309     C     F12D1         BEGSR
028000940309     C*
028100940309     C                   MOVE      *ON           $FINE
028200940309     C*
028300940309     C                   ENDSR
028400940131     C************************************************************
028500940207     C* CONTROLLO VIDEATA
028600940131     C************************************************************
028700940127     C     CTRD1         BEGSR
028800940131     C*
028900100715     C                   SETOFF                                       9972
029000100715$017 C     D1vabfld      IFNE      *ALL' '
029100100715     C                   SETON                                          72
029200100715     C                   ENDIF
029300030123      *
029400940127     C* non imposto pos. del cursore
029500940127     C                   Z-ADD     0             H1RIGA
029600940127     C                   Z-ADD     0             H1COLO
029700940621     C*
029800940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0299000301141   >C     xtaopz        IFNE      '04'
030000971110     C*
030100971110     C* controllo codice vuoto
030200971110$017 C     D1COD         IFEQ      *ALL' '
030300971110     C                   SETON                                        5099
030400090226     C                   Else
030500090226     C*** se già presente
030600090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
030700090226    >C     xtaopz        IFEQ      '01'
030800090226    >C     xtaopz        oreq      '03'
030900090226     c                   movel     d1seg         KEYsegmento
031000090226     c                   MOVEl     D1COD         KEYTABella
031100090226$007 c                   MOVE      d1unb         KEYCliente
031200090226     C     K_edstbl2     chain(N)  edstbl02L
031300090226     c                   if        %Found(edstbl02L)
031400090226     C                   SETON                                        5199
031500090226     C                   EndIF
031600090226     C                   ENDIF
031700090226     C                   ENDIF
031800030114     C*
031900030114     C* controllo codice vuoto
032000090226$017 C     D1vab1        IFEQ      *ALL' '
032100090226$017 C     D1vab2        andEQ     *ALL' '
032200100715$017 C     D1vabfld      andEQ     *ALL' '
032300090226     C                   SETON                                        5299
032400030114     C                   ENDIF
032500090226     C*
032600090226     C* controllo codice vuoto
032700090226$017 C     D1DES         IFEQ      *ALL' '
032800090226     C                   SETON                                        5399
032900090226     C                   ENDIF
033000090226     C*
033100090226     C* controllo codice vuoto
033200090226$017 C     D1unb         IFne      *blank
033300090226     c                   eval      tabcod = 'PT'
033400090226     c                   eval      tabkey = d1unb
033500090226     C     k_edtab       chain     edtab01l
033600090226     c                   if        not %Found(edtab01l)
033700090226     C**** se sbagliato il codice UNB
033800090226     C                   SETON                                        5499
033900090226     c                   end
034000090226     C                   ENDIF
034100030114      *
0342009402241-   C                   END
034300940131     C*
034400940302     C     *IN99         IFEQ      *ON
034500940510     C     $GEST         ANDEQ     'D1'
034600940117     C                   MOVEA     *IN           WIN
034700940128     C                   MOVE      *ALL'0'       IN5098           49
034800940128     C                   MOVEA     IN5098        *IN(50)
034900940614     C                   WRITE     D1
035000940117     C                   MOVEA     WIN           *IN
035100940302     C                   ENDIF
035200940127     C*
035300940117     C                   ENDSR
035400030204     C/EJECT
035500030204     C************************************************************
035600030204    >C* AGGIORNAMENTO ANAGRAFICA
035700030204     C************************************************************
035800030204    >C     FLDAGG        BEGSR
035900030204     C*
036000030204     C*  campi chiave da scrivere e aggiornare
036100090226$007 C                   MOVE      d1unb         ETBUNB
036200090226     C                   movel     d1seg         ETBSGM
036300090226     C                   MOVEl     D1COD         ETBVALSGM
036400090226     C*
036500090226     C                   eval      KEYTABella  = h1cod
036600090226     C                   eval      KEYCliente  = h1unb
036700030204     C*
036800030204     C                   ENDSR
036900940207     C/EJECT
037000940224     C************************************************************
037100940315    >C* AGGIORNAMENTO ANAGRAFICA
037200940224     C************************************************************
037300940315    >C     AGGANA        BEGSR
037400940207     C*
037500030204     C                   exsr      FLDAGG
037600030204     C*
037700940207     C                   SELECT
037800030204     C*
037900030114    >C     xtaopz        WHENEQ    '01'
038000030114    >C     xtaopz        OREQ      '03'
038100030204     C*
038200090226    >C                   EXSR      RIEANA
038300090226     c                   write     edstbl000                            59
038400940318     C* se rcd non scrivibile attivo errore
038500030114    >C   59              SETON                                          99
038600030204      *
038700030114    >C     xtaopz        WHENEQ    '02'
038800030204      *
038900090226     C     K_edsTBL2     chain     edstbl02L
039000030204     C                   exsr      FLDAGG
039100090226    >C                   EXSR      RIEANA
039200090226     c                   update    edstbl000                            59
039300030114    >C   59              SETON                                          99
039400030204      *
039500030114    >C     xtaopz        WHENEQ    '04'
039600090226     C     K_edsTBL2     chain     edstbl02L
039700090226     c                   delete    edstbl000                            59
039800030114    >C   59              SETON                                          99
039900940621     C*
040000940207     C                   ENDSL
040100940203     C*
040200940203     C                   ENDSR
040300940224     C************************************************************
040400940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
040500940224     C************************************************************
040600090226     C     RIEANA        BEGSR
040700030114     C*
040800090226     c                   movel     d1des         ETBDESCRI
040900100715     c                   move      D1vabFLD      ETBDESCRI
041000090226     c                   movel     d1vab1        ETBDATI
041100090226     c                   move      d1vab2        ETBDATI
041200030114     C*
041300940207     C                   ENDSR
041400940224     C************************************************************
041500940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
041600940224     C************************************************************
041700940224     C     GESAGG        BEGSR
041800940224     C*
041900940224     C* segnalo al pgm chiamante l'avvenuta conferma
042000030114    >C                   MOVE      *ON           xtaopr
042100940224     C* segnalo al pgm chiamante l'aver premuto l'F6
042200030114    >C                   MOVE      '0'           xtaret
042300940224     C*
0424009402241    C                   SELECT
042500940224     C* nel caso di immissione, ripristino la videata iniziale
042600030114    >C     xtaopz        WHENEQ    '01'
042700940224     C                   MOVE      *ON           $INZD1
042800030114     C                   MOVE      *ON           $FINE
042900940224     C* altrimenti ritorno al pgm chiamante
043000940224     C                   OTHER
043100940224     C                   MOVE      *ON           $FINE
0432009402241-   C                   ENDSL
043300940224     C*
043400940224     C                   ENDSR
043500940131     C************************************************************
043600940131     C* OPERAZIONI INIZIALI
043700940131     C************************************************************
043800940131     C     *INZSR        BEGSR
043900940131     C*
044000940127     C* Reperimento parametri
044100940117     C     *ENTRY        PLIST
044200940117     C                   PARM                    KPJBA
044300090225     C                   PARM                    KEYsegmento       3
044400090225     C                   PARM                    KEYTABella       35
044500090225     C                   PARM                    KEYCliente       35
044600090225      *
044700030113     C                   movel     kpjbu         tabds
044800030115     c                   setoff                                       08
044900030115     C*
045000940223     C* Variabili per gestione videate
045100940223     C                   MOVE      'D1'          $GEST
045200940223     C                   MOVE      *OFF          $FINE
045300940223     C                   MOVE      *ON           $INZD1
045400940224     C                   MOVE      *BLANK        $LASTG
045500940224     C                   MOVE      *BLANK        $LASTV
045600940506     C                   Z-ADD     0             $ULKD1            3 0
045700940127     C*
045800940223     C* Variabili appoggio
045900940224     C                   Z-ADD     0             CURR
046000940224     C                   Z-ADD     0             CURC
046100940207     C*
046200940207     C* Valorizzazione campi univoci testate
046300940614     C                   CLEAR                   T1
046400940614     C                   MOVEL     KNSIF         NOMSIF
046500030113     C******             MOVEL     XSCDSI        NOMDIT
046600941027     C                   MOVEL     DSPGM         NOMPGM
046700940224     C*
046800940224     C* Inizializzazione/Decodifica prima videata
046900940224     C                   EXSR      INZD1
047000940224     C                   MOVE      *OFF          $INZD1
047100940127     C*
047200940117     C                   ENDSR
047300940128     C************************************************************
047400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
047500940223     C************************************************************
047600940223     C     DEFCAM        BEGSR
047700940223     C*
047800940223     C* klist
047900090226     C     k_edstbl2     kLIST
048000090226     C                   kfld                    KEYsegmento       3
048100090226     C                   kfld                    KEYTABella       35
048200090226     C                   kfld                    KEYCliente       35
048300090226      *
048400090226     C     k_edtab       kLIST
048500090226     C                   kfld                    tabcod
048600090226     C                   kfld                    tabkey
048700940207     C*
048800940223     C* Variabili per gestione videate
048900940223     C*
049000940223     C                   MOVE      *BLANK        $GEST             2
049100940223     C                   MOVE      *BLANK        $FINE             1
049200940223     C                   MOVE      *BLANK        $INZD1            1
049300940223     C                   MOVE      *BLANK        $LASTG            2
049400940223     C                   MOVE      *BLANK        $LASTV            2
049500940223     C*
049600940223     C* Variabili appoggio
049700940223     C                   Z-ADD     0             CURR              2 0
049800940223     C                   Z-ADD     0             CURC              2 0
049900940223     C                   MOVE      *ZEROS        WIN              99
050000940224     C                   MOVE      *BLANK        WTBERR            1
050100940223     C*
050200940223     C* Indici
050300940223     C                   Z-ADD     0             X                 3 0
050400940223     C                   Z-ADD     0             Y                 3 0
050500940127     C*
050600940117     C                   ENDSR
050700940128     C************************************************************
