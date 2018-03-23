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
002300110902$001 FTII156D2  CF   E             WORKSTN
002400940201     F                                     INFDS(DSFMT)
002500110901     Fantab01l  UF A E           K DISK
002600940128     D*----------------------------------------------------*
002700110902$003 D EXTDS         E DS                  EXTNAME(ANTAB156) INZ
002800030113      *-------------
002900940127     D* Passaggio Parametri
003000940127     D KPJBA         E DS
003100030113      *-------------
003200030113     D* Parametri in ricezione
003300030113     D  TABDS          DS
003400030113     D  XTAOPZ                 1      2
003500030113     D  XTARET                 3      3
003600030113     D  XTAOPR                 4      4
003700030113     D  XTAERR                 5      5
003800110902     D  XTAKEY                 6     10
003900940201     D*-------------
004000030204     D  S_TASTO        S                   like($Tasto)
004100940201     D DSFMT           DS
004200940506     D  $TASTO               369    369
004300940201     D  NRG                  370    370
004400940201     D  NCL                  371    371
004500940201     D*-------------
004600940201     D* posizione cursore
004700940201     D CURSOR          DS
004800940223     D  RIRI                   1      2B 0 INZ
004900940201     D  R$$                    2      2
005000940223     D  COCO                   3      4B 0 INZ
005100940201     D  C$$                    4      4
005200940207     D*-------------
005300940207     D* Reperimento nome PGM
005400940207     D STATUS         SDS           333
005500940207     D  DSPGM            *PROC
005600940225     D*-------------
005700940225     D* COSTANTI
005800940225     D*-------------
005900940506     D* Tasti di funzione
006000940506     D F01             C                   CONST(X'31')
006100940506     D F02             C                   CONST(X'32')
006200940506     D F03             C                   CONST(X'33')
006300940506     D F04             C                   CONST(X'34')
006400940506     D F05             C                   CONST(X'35')
006500940506     D F06             C                   CONST(X'36')
006600940506     D F07             C                   CONST(X'37')
006700940506     D F08             C                   CONST(X'38')
006800940506     D F09             C                   CONST(X'39')
006900940506     D F10             C                   CONST(X'3A')
007000940506     D F11             C                   CONST(X'3B')
007100940506     D F12             C                   CONST(X'3C')
007200940506     D F13             C                   CONST(X'B1')
007300940506     D F14             C                   CONST(X'B2')
007400940506     D F15             C                   CONST(X'B3')
007500940506     D F16             C                   CONST(X'B4')
007600940506     D F17             C                   CONST(X'B5')
007700940506     D F18             C                   CONST(X'B6')
007800940506     D F19             C                   CONST(X'B7')
007900940506     D F20             C                   CONST(X'B8')
008000940506     D F21             C                   CONST(X'B9')
008100940506     D F22             C                   CONST(X'BA')
008200940506     D F23             C                   CONST(X'BB')
008300940506     D F24             C                   CONST(X'BC')
008400940506     D ENTER           C                   CONST(X'F1')
008500940506     D ROLDWN          C                   CONST(X'F4')
008600940506     D ROLLUP          C                   CONST(X'F5')
008700940506     D*-------------
008800941108     D DATA            C                   CONST('0001-01-01')
008900030114     D Annull          C                   CONST('  ANNULLATO  ')
009000030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009100030114     D OPz_Var         C                   CONST('VARIAZIONE')
009200030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009300030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009400940127     C*----------------------------------------------------*
009500940127     C*                MAIN LINE PROGRAM
009600940127     C*----------------------------------------------------*
009700940223     C*
009800940223     C     $FINE         DOWEQ     *OFF
009900940202     C     $GEST         CASEQ     'D1'          GESD1
010000940117     C                   END
010100940117     C                   END
010200940325     C* fine programma
010300030113     c                   movel     tabds         kpjbu
010400030113     C                   Seton                                        LR
010500940131     C************************************************************
010600940510     C* GESTIONE VIDEO RECORD D1
010700940131     C************************************************************
010800940127     C     GESD1         BEGSR
010900030113      *
011000940225     C* inizializzazione videata
011100940223     C     $INZD1        IFEQ      *ON
011200940127     C                   EXSR      INZD1
011300940223     C                   MOVE      *OFF          $INZD1
011400940117     C                   END
011500030113      *
011600940225     C* emissione piede videata se proveniente da altra
011700940225     C* salvataggio valore formato di provenienza
011800940207     C     $LASTV        IFNE      'D1'
011900940614     C                   WRITE     T1
012000940202     C                   MOVE      $LASTV        $LASTG
012100940202     C                   MOVE      'D1'          $LASTV
012200940202     C                   END
012300940131     C*
012400940203     C                   SELECT
012500940203     C* Annullamento
012600030114     C* Visualizzazione
012700030114    >C     xtaopz        WHENEQ    '01'
012800030114     c                   movel     Opz_Imm       D1Fun
012900030114    >C     xtaopz        WHENEQ    '05'
013000030114     c                   movel     Opz_Vis       D1Fun
013100030114     C                   SETON                                        020306
013200030114    >C     xtaopz        WHENEQ    '04'
013300030114     c                   movel     Opz_Del       D1Fun
013400110902     C                   SETON                                        02
013500030114     C                   SETOFF                                         06
013600940620     C* Manutenzione
013700030113    >C     xtaopz        WHENEQ    '02'
013800030114     c                   movel     Opz_Var       D1Fun
013900030114     C                   SETON                                        02
014000030114     C                   SETOFF                                       0306
014100940620     C*
014200940620     C                   OTHER
014300940620     C*
014400030114     C                   SETOFF                                       020306
014500940203     C                   ENDSL
014600030114     C*
014700030114     c                   SETOFF                                         99
014800940620     C* Immissione/Modifica/Copia
014900030113     C*               *----------------*
015000940620     C                   EXFMT     D1
015100030113     C*               *----------------*
015200030114     C*
015300030114     C     *IN99         IFEQ      *OFF
015400030114     C*
0155000301141    C                   SELECT
015600030114     C* F3=Fine
015700030114     C     $TASTO        WHENEQ    F03
015800030114     C                   EXSR      F03D1
015900030114     C* F12=Ritorno
016000030114     C     $TASTO        WHENEQ    F12
016100030114     C                   EXSR      F12D1
016200030114     C*
0163000301141O   C                   OTHER
016400030114     C*
016500030114     C                   EXSR      CTRD1
016600030113     C*
016700940616     C     *IN99         IFEQ      *OFF
016800940616     C* F6
0169000302052    C     $TASTO        ifeq      F06
0170000301152    C     $TASTO        oreq      F23
017100030204     C* A chi Trasmette la Tabella ?
017200110901     c****               Exsr      ACHI
017300940224     C* eseguo aggiornamento
017400940315    >C                   EXSR      AGGANA
017500940224     C* eseguo operazioni del dopo-aggiornamento
017600030114     C  N99              EXSR      GESAGG
017700030205     c                   end
017800030114     C*
017900030114     C                   ENDIF
018000030114     C*
0181000301142-   C                   ENDSL
018200030114     C*
0183000301142-   C                   ENDIF
018400030114     C*
018500940117     C                   ENDSR
018600940117      ************************************************************
018700940207      * INIZIALIZZAZIONE VIDEATA DATI
018800940117      ************************************************************
018900940127     C     INZD1         BEGSR
019000940131     C*
019100940615     C                   CLEAR                   D1
019200110901     C                   move      '156'         TABgru
019300110902     C                   MOVE      xtakey        D1COD
019400110902     C                   MOVE      *all'0'       tabCOD
019500110902     C                   MOVE      xtakey        tabCOD
019600940207     C*
019700940224     C* valorizzo la chiave univoca per reperire i dati
019800940224     C* se non immissione
0199000301141   >C     xtaopz        IFNE      '01'
020000030204      *
020100940315    >C*-------------------------------------------------------------
020200940315    >C* impostare i campi chiave ricevuti
020300940315    >C*-------------------------------------------------------------
020400110901     C     KTBL          chain(N)  antab01L
020500940614     C*
020600110901     C                   IF        %Found(antab01L)
020700110902     C                   MOVEL     tabDES        EXTDS
020800110901     c*******            if        TblFLG = '*'
020900110901     c*******            seton                                        08
021000110901     C*******            MOVEl     Annull        D1Ann
021100110901     C*******            end
021200030113     C*
021300940224     C* riempio la videata
021400940224     C                   EXSR      RIED1
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
024600030114     C*
024700030114     C                   ENDSR
024800030114     C************************************************************
024900030114     C* RIEMPIMENTO VIDEATA  D1
025000030114     C************************************************************
025100030114     C     RIED1         BEGSR
025200030114     C*
025300030113     C* descrizione
025400110901     c                   eval      d1DES = DES156
025500110901     c                   eval      D1GRU = GRU156V
025600110901     c                   eval      D1FI1 = FI1156V
025700110901     c                   eval      D1FI2 = FI2156V
025800110901     c                   eval      D1FI3 = FI3156V
025900030204     C*
026000940207     C                   ENDSR
026100940309     C************************************************************
026200940309     C* GESTIONE F03 VIDEO D1
026300940309     C************************************************************
026400940309     C     F03D1         BEGSR
026500940309     C*
026600940309     C                   MOVE      *ON           $FINE
026700030113    >C                   MOVE      '1'           xtaret
026800940325     C*
026900940325     C                   ENDSR
027000940309     C************************************************************
027100940309     C* GESTIONE F12 VIDEO D1
027200940309     C************************************************************
027300940309     C     F12D1         BEGSR
027400940309     C*
027500940309     C                   MOVE      *ON           $FINE
027600940309     C*
027700940309     C                   ENDSR
027800940131     C************************************************************
027900940207     C* CONTROLLO VIDEATA
028000940131     C************************************************************
028100940127     C     CTRD1         BEGSR
028200940131     C*
028300940127     C                   SETOFF                                       99
028400030123      *
028500940127     C* non imposto pos. del cursore
028600940127     C                   Z-ADD     0             H1RIGA
028700940127     C                   Z-ADD     0             H1COLO
028800940621     C*
028900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0290000301141   >C     xtaopz        IFNE      '04'
029100971110     C*
029200971110     C* controllo codice vuoto
029300971110$017 C     D1COD         IFEQ      *ALL' '
029400971110     C                   SETON                                        5099
029500971110     C                   ENDIF
029600030114     C*
029700030114     C* controllo codice vuoto
029800030114$017 C     D1DES         IFEQ      *ALL' '
029900030114     C                   SETON                                        5199
030000030114     C                   ENDIF
030100050511     C*
030200940207     C*
030300940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
030400030114    >C     xtaopz        IFEQ      '01'
030500030114    >C     xtaopz        oreq      '03'
030600110902     c                   move      *all'0'       tabCOD
030700110902     c                   move      d1cod         tabCOD
030800110901     C     KTBL          chain(N)  antab01L
030900110901     c                   if        %Found(antab01L)
031000030114     C                   SETON                                        5299
031100030114     C                   EndIF
031200030114     C                   ENDIF
031300030114      *
0314009402241-   C                   END
031500940131     C*
031600940302     C     *IN99         IFEQ      *ON
031700940510     C     $GEST         ANDEQ     'D1'
031800940117     C                   MOVEA     *IN           WIN
031900940128     C                   MOVE      *ALL'0'       IN5098           49
032000940128     C                   MOVEA     IN5098        *IN(50)
032100940614     C                   WRITE     D1
032200940117     C                   MOVEA     WIN           *IN
032300940302     C                   ENDIF
032400940127     C*
032500940117     C                   ENDSR
032600960305     C/EJECT
032700030204     C************************************************************
032800030204     C* A chi deve trasmettere
032900030204     C************************************************************
033000030204     C     ACHI          BEGSR
033100030204     C*
033200030204     c                   eval      S_Tasto = $Tasto
033300030204     C*
033400030204     c                   move      '1'           t1trs
033500030204     c                   clear                   t1fil
033600030204     c     su_win        tag
033700030204     C                   Exfmt     W1Tras
033800030204     c                   if        t1trs = '2' and
033900030204     c                             t1fil = 0
034000030204     c                   seton                                        77
034100030204     c                   goto      su_win
034200030204     c                   end
034300030204     C*
034400030204     c                   eval      $Tasto = S_Tasto
034500030204     C*
034600030204     C                   ENDSR
034700030204     C/EJECT
034800030204     C************************************************************
034900030204    >C* AGGIORNAMENTO ANAGRAFICA
035000030204     C************************************************************
035100030204    >C     FLDAGG        BEGSR
035200030204     C*
035300030204     C*  campi chiave da scrivere e aggiornare
035400110902     C                   move      *all'0'       tabCOD
035500110902     C                   MOVE      D1COD         tabCOD
035600030204     C*
035700030204     C                   ENDSR
035800940207     C/EJECT
035900940224     C************************************************************
036000940315    >C* AGGIORNAMENTO ANAGRAFICA
036100940224     C************************************************************
036200940315    >C     AGGANA        BEGSR
036300940207     C*
036400030204     C                   exsr      FLDAGG
036500030204     C*
036600940207     C                   SELECT
036700030204     C*
036800030114    >C     xtaopz        WHENEQ    '01'
036900030114    >C     xtaopz        OREQ      '03'
037000030204     C*
037100110901     C                   move      '156'         TABgru
037200110901    >C                   EXSR      RIEANA_156
037300110901     c                   write     ANTAB000                             59
037400940318     C* se rcd non scrivibile attivo errore
037500030114    >C   59              SETON                                          99
037600030204      *
037700030114    >C     xtaopz        WHENEQ    '02'
037800030204      *
037900110901     C                   move      '156'         TABgru
038000110901     C     KTBL          chain     antab01L
038100030204     C                   exsr      FLDAGG
038200110901    >C                   EXSR      RIEANA_156
038300110901     c                   update    ANTAB000                             59
038400030114    >C   59              SETON                                          99
038500030204      *
038600030114    >C     xtaopz        WHENEQ    '04'
038700110901     C                   move      '156'         TABgru
038800110901     C     KTBL          chain     antab01L
038900030115     c                   if        $tasto = F23
039000110901     c                   delete    antab000                             59
039100030115     c                   else
039200110901     c                   delete    antab000                             59
039300030115     c                   end
039400030114    >C   59              SETON                                          99
039500940621     C*
039600940207     C                   ENDSL
039700940203     C*
039800940203     C                   ENDSR
039900940224     C************************************************************
040000940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
040100940224     C************************************************************
040200110901     C     RIEANA_156    BEGSR
040300030114     C*
040400110901     C                   CLEAR                   tabDES
040500030114     C* descrizione
040600110901     c                   movel     d1DES         DES156
040700110901     c                   movel     D1GRU         GRU156V
040800110901     c                   movel     D1FI1         FI1156V
040900110901     c                   movel     D1FI2         FI2156V
041000110901     c                   movel     D1FI3         FI3156V
041100030114     C*
041200110902     C                   MOVEL     EXTDS         tabDES
041300030114     C*
041400940207     C                   ENDSR
041500940224     C************************************************************
041600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
041700940224     C************************************************************
041800940224     C     GESAGG        BEGSR
041900940224     C*
042000940224     C* segnalo al pgm chiamante l'avvenuta conferma
042100030114    >C                   MOVE      *ON           xtaopr
042200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
042300030114    >C                   MOVE      '0'           xtaret
042400940224     C*
0425009402241    C                   SELECT
042600940224     C* nel caso di immissione, ripristino la videata iniziale
042700030114    >C     xtaopz        WHENEQ    '01'
042800940224     C                   MOVE      *ON           $INZD1
042900030114     C                   MOVE      *ON           $FINE
043000940224     C* altrimenti ritorno al pgm chiamante
043100940224     C                   OTHER
043200940224     C                   MOVE      *ON           $FINE
0433009402241-   C                   ENDSL
043400940224     C*
043500940224     C                   ENDSR
043600940131     C************************************************************
043700940131     C* OPERAZIONI INIZIALI
043800940131     C************************************************************
043900940131     C     *INZSR        BEGSR
044000940131     C*
044100940127     C* Reperimento parametri
044200940127     C*
044300940117     C     *ENTRY        PLIST
044400940117     C                   PARM                    KPJBA
044500030113     C                   movel     kpjbu         tabds
044600030115     c                   setoff                                       08
044700030115     C*
044800940223     C* Variabili per gestione videate
044900940223     C                   MOVE      'D1'          $GEST
045000940223     C                   MOVE      *OFF          $FINE
045100940223     C                   MOVE      *ON           $INZD1
045200940224     C                   MOVE      *BLANK        $LASTG
045300940224     C                   MOVE      *BLANK        $LASTV
045400940506     C                   Z-ADD     0             $ULKD1            3 0
045500940127     C*
045600940223     C* Variabili appoggio
045700940224     C                   Z-ADD     0             CURR
045800940224     C                   Z-ADD     0             CURC
045900940207     C*
046000940207     C* Valorizzazione campi univoci testate
046100940614     C                   CLEAR                   T1
046200940614     C                   MOVEL     KNSIF         NOMSIF
046300030113     C******             MOVEL     XSCDSI        NOMDIT
046400941027     C                   MOVEL     DSPGM         NOMPGM
046500030113     C*
046600110901     C                   move      '156'         TABgru
046700110902     C                   move      *all'0'       tabCOD
046800110902     C                   move      xtakey        tabCOD
046900940224     C*
047000940224     C* Inizializzazione/Decodifica prima videata
047100940224     C                   EXSR      INZD1
047200940224     C                   MOVE      *OFF          $INZD1
047300940127     C*
047400940117     C                   ENDSR
047500940128     C************************************************************
047600940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
047700940223     C************************************************************
047800940223     C     DEFCAM        BEGSR
047900940223     C*
048000940223     C* klist
048100030113     C     KTBL          KLIST
048200110901     C                   KFLD                    tabgru
048300110901     C                   KFLD                    tabcod
048400940207     C*
048500940223     C* Variabili per gestione videate
048600940223     C*
048700940223     C                   MOVE      *BLANK        $GEST             2
048800940223     C                   MOVE      *BLANK        $FINE             1
048900940223     C                   MOVE      *BLANK        $INZD1            1
049000940223     C                   MOVE      *BLANK        $LASTG            2
049100940223     C                   MOVE      *BLANK        $LASTV            2
049200940223     C*
049300940223     C* Variabili appoggio
049400940223     C                   Z-ADD     0             CURR              2 0
049500940223     C                   Z-ADD     0             CURC              2 0
049600940223     C                   MOVE      *ZEROS        WIN              99
049700940224     C                   MOVE      *BLANK        WTBERR            1
049800940223     C*
049900940223     C* Indici
050000940223     C                   Z-ADD     0             X                 3 0
050100940223     C                   Z-ADD     0             Y                 3 0
050200940127     C*
050300940117     C                   ENDSR
050400940128     C************************************************************
