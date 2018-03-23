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
002500110902$001 FTII155D2  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700110901     Fantab01l  UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900110901$003 D EXTDS155      E DS                  EXTNAME(ANTAB155) INZ
003000030113      *-------------
003100940127     D* Passaggio Parametri
003200940127     D KPJBA         E DS
003300030113      *-------------
003400030113     D* Parametri in ricezione
003500030113     D  TABDS          DS
003600030113     D  XTAOPZ                 1      2
003700030113     D  XTARET                 3      3
003800030113     D  XTAOPR                 4      4
003900030113     D  XTAERR                 5      5
004000110902     D  XTAKEY                 6      8
004100940201     D*-------------
004200030204     D  S_TASTO        S                   like($Tasto)
004300940201     D DSFMT           DS
004400940506     D  $TASTO               369    369
004500940201     D  NRG                  370    370
004600940201     D  NCL                  371    371
004700940201     D*-------------
004800940201     D* posizione cursore
004900940201     D CURSOR          DS
005000940223     D  RIRI                   1      2B 0 INZ
005100940201     D  R$$                    2      2
005200940223     D  COCO                   3      4B 0 INZ
005300940201     D  C$$                    4      4
005400940207     D*-------------
005500940207     D* Reperimento nome PGM
005600940207     D STATUS         SDS           333
005700940207     D  DSPGM            *PROC
005800940225     D*-------------
005900940225     D* COSTANTI
006000940225     D*-------------
006100940506     D* Tasti di funzione
006200940506     D F01             C                   CONST(X'31')
006300940506     D F02             C                   CONST(X'32')
006400940506     D F03             C                   CONST(X'33')
006500940506     D F04             C                   CONST(X'34')
006600940506     D F05             C                   CONST(X'35')
006700940506     D F06             C                   CONST(X'36')
006800940506     D F07             C                   CONST(X'37')
006900940506     D F08             C                   CONST(X'38')
007000940506     D F09             C                   CONST(X'39')
007100940506     D F10             C                   CONST(X'3A')
007200940506     D F11             C                   CONST(X'3B')
007300940506     D F12             C                   CONST(X'3C')
007400940506     D F13             C                   CONST(X'B1')
007500940506     D F14             C                   CONST(X'B2')
007600940506     D F15             C                   CONST(X'B3')
007700940506     D F16             C                   CONST(X'B4')
007800940506     D F17             C                   CONST(X'B5')
007900940506     D F18             C                   CONST(X'B6')
008000940506     D F19             C                   CONST(X'B7')
008100940506     D F20             C                   CONST(X'B8')
008200940506     D F21             C                   CONST(X'B9')
008300940506     D F22             C                   CONST(X'BA')
008400940506     D F23             C                   CONST(X'BB')
008500940506     D F24             C                   CONST(X'BC')
008600940506     D ENTER           C                   CONST(X'F1')
008700940506     D ROLDWN          C                   CONST(X'F4')
008800940506     D ROLLUP          C                   CONST(X'F5')
008900940506     D*-------------
009000941108     D DATA            C                   CONST('0001-01-01')
009100030114     D Annull          C                   CONST('  ANNULLATO  ')
009200030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009300030114     D OPz_Var         C                   CONST('VARIAZIONE')
009400030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009500030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009600940127     C*----------------------------------------------------*
009700940127     C*                MAIN LINE PROGRAM
009800940127     C*----------------------------------------------------*
009900940223     C*
010000940223     C     $FINE         DOWEQ     *OFF
010100940202     C     $GEST         CASEQ     'D1'          GESD1
010200940117     C                   END
010300940117     C                   END
010400940325     C* fine programma
010500030113     c                   movel     tabds         kpjbu
010600030113     C                   Seton                                        LR
010700940131     C************************************************************
010800940510     C* GESTIONE VIDEO RECORD D1
010900940131     C************************************************************
011000940127     C     GESD1         BEGSR
011100030113      *
011200940225     C* inizializzazione videata
011300940223     C     $INZD1        IFEQ      *ON
011400940127     C                   EXSR      INZD1
011500940223     C                   MOVE      *OFF          $INZD1
011600940117     C                   END
011700030113      *
011800940225     C* emissione piede videata se proveniente da altra
011900940225     C* salvataggio valore formato di provenienza
012000940207     C     $LASTV        IFNE      'D1'
012100940614     C                   WRITE     T1
012200940202     C                   MOVE      $LASTV        $LASTG
012300940202     C                   MOVE      'D1'          $LASTV
012400940202     C                   END
012500940131     C*
012600940203     C                   SELECT
012700940203     C* Annullamento
012800030114     C* Visualizzazione
012900030114    >C     xtaopz        WHENEQ    '01'
013000030114     c                   movel     Opz_Imm       D1Fun
013100030114    >C     xtaopz        WHENEQ    '05'
013200030114     c                   movel     Opz_Vis       D1Fun
013300030114     C                   SETON                                        020306
013400030114    >C     xtaopz        WHENEQ    '04'
013500030114     c                   movel     Opz_Del       D1Fun
013600110902     C                   SETON                                        02
013700030114     C                   SETOFF                                         06
013800940620     C* Manutenzione
013900030113    >C     xtaopz        WHENEQ    '02'
014000030114     c                   movel     Opz_Var       D1Fun
014100030114     C                   SETON                                        02
014200030114     C                   SETOFF                                       0306
014300940620     C*
014400940620     C                   OTHER
014500940620     C*
014600030114     C                   SETOFF                                       020306
014700940203     C                   ENDSL
014800030114     C*
014900030114     c                   SETOFF                                         99
015000940620     C* Immissione/Modifica/Copia
015100030113     C*               *----------------*
015200940620     C                   EXFMT     D1
015300030113     C*               *----------------*
015400030114     C*
015500030114     C     *IN99         IFEQ      *OFF
015600030114     C*
0157000301141    C                   SELECT
015800030114     C* F3=Fine
015900030114     C     $TASTO        WHENEQ    F03
016000030114     C                   EXSR      F03D1
016100030114     C* F12=Ritorno
016200030114     C     $TASTO        WHENEQ    F12
016300030114     C                   EXSR      F12D1
016400030114     C*
0165000301141O   C                   OTHER
016600030114     C*
016700030114     C                   EXSR      CTRD1
016800030113     C*
016900940616     C     *IN99         IFEQ      *OFF
017000940616     C* F6
0171000302052    C     $TASTO        ifeq      F06
0172000301152    C     $TASTO        oreq      F23
017300030204     C* A chi Trasmette la Tabella ?
017400110901     c****               Exsr      ACHI
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
019400110901     C                   move      '155'         TABgru
019500030113     C                   MOVEl     xtakey        D1COD
019600110901     C                   MOVE      *all'0'       tabCOD
019700110901     C                   MOVE      xtakey        tabCOD
019800940207     C*
019900940224     C* valorizzo la chiave univoca per reperire i dati
020000940224     C* se non immissione
0201000301141   >C     xtaopz        IFNE      '01'
020200030204      *
020300940315    >C*-------------------------------------------------------------
020400940315    >C* impostare i campi chiave ricevuti
020500940315    >C*-------------------------------------------------------------
020600110901     C     KTBL          chain(N)  antab01L
020700940614     C*
020800110901     C                   IF        %Found(antab01L)
020900110901     C                   MOVEL     tabDES        EXTDS155
021000110901     c*******            if        TblFLG = '*'
021100110901     c*******            seton                                        08
021200110901     C*******            MOVEl     Annull        D1Ann
021300110901     C*******            end
021400030113     C*
021500940224     C* riempio la videata
021600940224     C                   EXSR      RIED1
021700030113      *
021800940224     C* richiamo routine dei ctrl per decodificarla
021900940614    >C                   EXSR      CTRD1
022000030113      *
022100940224     C                   MOVE      *ALL'0'       IN5098
022200940224     C                   MOVEA     IN5098        *IN(50)
022300030113      *
022400940224     C* definizione indicatori su video
022500940224     C                   SETOFF                                       02
0226009402243    C                   SELECT
022700030114    >C     xtaopz        WHENEQ    '02'
022800940224     C                   SETON                                        02
022900030114    >C     xtaopz        WHENEQ    '05'
0230009402243-   C                   ENDSL
023100940224     C*
0232009402242-   C                   ENDIF
023300030114     C*
0234000301141   >C                   Else
023500030114     C* Se in Immissione nuovo record
023600030114     c                   exsr      NewD1
023700940223     C*
0238009402241-   C                   ENDIF
023900940224     C*
024000940117     C                   ENDSR
024100940207     C/EJECT
024200940207     C************************************************************
024300940510     C* RIEMPIMENTO VIDEATA  D1
024400940207     C************************************************************
024500030114     C     NEWD1         BEGSR
024600940207     C*
024700030114     c                   clear                   d1
024800030114     C*
024900030114     C                   ENDSR
025000030114     C************************************************************
025100030114     C* RIEMPIMENTO VIDEATA  D1
025200030114     C************************************************************
025300030114     C     RIED1         BEGSR
025400030114     C*
025500030113     C* descrizione
025600110901     c                   eval      d1DES = DES155
025700110901     c                   eval      D1KCC = KCC155V
025800110901     c                   eval      D1KSC = KSC155V
025900110901     c                   eval      D1ESE = ESE155V
026000110901     c                   eval      D1ALI = ALI155V
026100110901     c                   eval      D1VOC = VOC155V
026200110901     c                   eval      D1PE1 = PE1155V
026300110901     c                   eval      D1PE2 = PE2155V
026400110901     c                   eval      D1PE3 = PE3155V
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
028900940127     C                   SETOFF                                       99
029000030123      *
029100940127     C* non imposto pos. del cursore
029200940127     C                   Z-ADD     0             H1RIGA
029300940127     C                   Z-ADD     0             H1COLO
029400940621     C*
029500940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0296000301141   >C     xtaopz        IFNE      '04'
029700971110     C*
029800971110     C* controllo codice vuoto
029900971110$017 C     D1COD         IFEQ      *ALL' '
030000971110     C                   SETON                                        5099
030100971110     C                   ENDIF
030200030114     C*
030300030114     C* controllo codice vuoto
030400030114$017 C     D1DES         IFEQ      *ALL' '
030500030114     C                   SETON                                        5199
030600030114     C                   ENDIF
030700050511     C*
030701110908$017 C     D1pe1         IFne      *blank
030702110908$017 C     D1pe2         andNE     *blank
030703110908$017 C     D1pe1         ORne      *blank
030704110908$017 C     D1pe3         andNE     *blank
030705110908$017 C     D1pe2         ORne      *blank
030706110908$017 C     D1pe3         andNE     *blank
030707110908     C                   SETON                                        5299
030708110908     C                   ENDIF
030709110908     C*
030800940207     C*
030900940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
031000030114    >C     xtaopz        IFEQ      '01'
031100030114    >C     xtaopz        oreq      '03'
031200110901     c                   move      *all'0'       tabCOD
031300110901     c                   move      d1cod         tabCOD
031400110901     C     KTBL          chain(N)  antab01L
031500110901     c                   if        %Found(antab01L)
031600110908     C                   SETON                                        5399
031700030114     C                   EndIF
031800030114     C                   ENDIF
031900030114      *
0320009402241-   C                   END
032100940131     C*
032200940302     C     *IN99         IFEQ      *ON
032300940510     C     $GEST         ANDEQ     'D1'
032400940117     C                   MOVEA     *IN           WIN
032500940128     C                   MOVE      *ALL'0'       IN5098           49
032600940128     C                   MOVEA     IN5098        *IN(50)
032700940614     C                   WRITE     D1
032800940117     C                   MOVEA     WIN           *IN
032900940302     C                   ENDIF
033000940127     C*
033100940117     C                   ENDSR
033200960305     C/EJECT
033300030204     C************************************************************
033400030204     C* A chi deve trasmettere
033500030204     C************************************************************
033600030204     C     ACHI          BEGSR
033700030204     C*
033800030204     c                   eval      S_Tasto = $Tasto
033900030204     C*
034000030204     c                   move      '1'           t1trs
034100030204     c                   clear                   t1fil
034200030204     c     su_win        tag
034300030204     C                   Exfmt     W1Tras
034400030204     c                   if        t1trs = '2' and
034500030204     c                             t1fil = 0
034600030204     c                   seton                                        77
034700030204     c                   goto      su_win
034800030204     c                   end
034900030204     C*
035000030204     c                   eval      $Tasto = S_Tasto
035100030204     C*
035200030204     C                   ENDSR
035300030204     C/EJECT
035400030204     C************************************************************
035500030204    >C* AGGIORNAMENTO ANAGRAFICA
035600030204     C************************************************************
035700030204    >C     FLDAGG        BEGSR
035800030204     C*
035900030204     C*  campi chiave da scrivere e aggiornare
036000110901     C                   move      *all'0'       tabCOD
036100110901     C                   MOVE      D1COD         tabCOD
036200030204     C*
036300030204     C                   ENDSR
036400940207     C/EJECT
036500940224     C************************************************************
036600940315    >C* AGGIORNAMENTO ANAGRAFICA
036700940224     C************************************************************
036800940315    >C     AGGANA        BEGSR
036900940207     C*
037000030204     C                   exsr      FLDAGG
037100030204     C*
037200940207     C                   SELECT
037300030204     C*
037400030114    >C     xtaopz        WHENEQ    '01'
037500030114    >C     xtaopz        OREQ      '03'
037600030204     C*
037700110901     C                   move      '155'         TABgru
037800110901    >C                   EXSR      RIEANA_155
037900110901     c                   write     ANTAB000                             59
038000940318     C* se rcd non scrivibile attivo errore
038100030114    >C   59              SETON                                          99
038200030204      *
038300030114    >C     xtaopz        WHENEQ    '02'
038400030204      *
038500110901     C                   move      '155'         TABgru
038600110901     C     KTBL          chain     antab01L
038700030204     C                   exsr      FLDAGG
038800110901    >C                   EXSR      RIEANA_155
038900110901     c                   update    ANTAB000                             59
039000030114    >C   59              SETON                                          99
039100030204      *
039200030114    >C     xtaopz        WHENEQ    '04'
039300110901     C                   move      '155'         TABgru
039400110901     C     KTBL          chain     antab01L
039500030115     c                   if        $tasto = F23
039600110901     c                   delete    antab000                             59
039700030115     c                   else
039800110901     c                   delete    antab000                             59
039900030115     c                   end
040000030114    >C   59              SETON                                          99
040100940621     C*
040200940207     C                   ENDSL
040300940203     C*
040400940203     C                   ENDSR
040500940224     C************************************************************
040600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
040700940224     C************************************************************
040800110901     C     RIEANA_155    BEGSR
040900030114     C*
041000110901     C                   CLEAR                   tabDES
041100030114     C* descrizione
041200110901     c                   movel     d1DES         DES155
041300110901     c                   movel     D1KCC         KCC155V
041400110901     c                   movel     D1KSC         KSC155V
041500110901     c                   movel     D1ESE         ESE155V
041600110901     c                   movel     D1ALI         ALI155V
041700110901     c                   movel     D1VOC         VOC155V
041800110901     c                   movel     D1PE1         PE1155V
041900110901     c                   movel     D1PE2         PE2155V
042000110901     c                   movel     D1PE3         PE3155V
042100030114     C*
042200110901     C                   MOVEL     EXTDS155      tabDES
042300030114     C*
042400940207     C                   ENDSR
042500940224     C************************************************************
042600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
042700940224     C************************************************************
042800940224     C     GESAGG        BEGSR
042900940224     C*
043000940224     C* segnalo al pgm chiamante l'avvenuta conferma
043100030114    >C                   MOVE      *ON           xtaopr
043200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
043300030114    >C                   MOVE      '0'           xtaret
043400940224     C*
0435009402241    C                   SELECT
043600940224     C* nel caso di immissione, ripristino la videata iniziale
043700030114    >C     xtaopz        WHENEQ    '01'
043800940224     C                   MOVE      *ON           $INZD1
043900030114     C                   MOVE      *ON           $FINE
044000940224     C* altrimenti ritorno al pgm chiamante
044100940224     C                   OTHER
044200940224     C                   MOVE      *ON           $FINE
0443009402241-   C                   ENDSL
044400940224     C*
044500940224     C                   ENDSR
044600940131     C************************************************************
044700940131     C* OPERAZIONI INIZIALI
044800940131     C************************************************************
044900940131     C     *INZSR        BEGSR
045000940131     C*
045100940127     C* Reperimento parametri
045200940127     C*
045300940117     C     *ENTRY        PLIST
045400940117     C                   PARM                    KPJBA
045500030113     C                   movel     kpjbu         tabds
045600030115     c                   setoff                                       08
045700030115     C*
045800940223     C* Variabili per gestione videate
045900940223     C                   MOVE      'D1'          $GEST
046000940223     C                   MOVE      *OFF          $FINE
046100940223     C                   MOVE      *ON           $INZD1
046200940224     C                   MOVE      *BLANK        $LASTG
046300940224     C                   MOVE      *BLANK        $LASTV
046400940506     C                   Z-ADD     0             $ULKD1            3 0
046500940127     C*
046600940223     C* Variabili appoggio
046700940224     C                   Z-ADD     0             CURR
046800940224     C                   Z-ADD     0             CURC
046900940207     C*
047000940207     C* Valorizzazione campi univoci testate
047100940614     C                   CLEAR                   T1
047200940614     C                   MOVEL     KNSIF         NOMSIF
047300030113     C******             MOVEL     XSCDSI        NOMDIT
047400941027     C                   MOVEL     DSPGM         NOMPGM
047500030113     C*
047600110901     C                   move      '155'         TABgru
047700110901     C                   move      *all'0'       tabCOD
047800110901     C                   move      xtakey        tabCOD
047900940224     C*
048000940224     C* Inizializzazione/Decodifica prima videata
048100940224     C                   EXSR      INZD1
048200940224     C                   MOVE      *OFF          $INZD1
048300940127     C*
048400940117     C                   ENDSR
048500940128     C************************************************************
048600940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
048700940223     C************************************************************
048800940223     C     DEFCAM        BEGSR
048900940223     C*
049000940223     C* klist
049100030113     C     KTBL          KLIST
049200110901     C                   KFLD                    tabgru
049300110901     C                   KFLD                    tabcod
049400940207     C*
049500940223     C* Variabili per gestione videate
049600940223     C*
049700940223     C                   MOVE      *BLANK        $GEST             2
049800940223     C                   MOVE      *BLANK        $FINE             1
049900940223     C                   MOVE      *BLANK        $INZD1            1
050000940223     C                   MOVE      *BLANK        $LASTG            2
050100940223     C                   MOVE      *BLANK        $LASTV            2
050200940223     C*
050300940223     C* Variabili appoggio
050400940223     C                   Z-ADD     0             CURR              2 0
050500940223     C                   Z-ADD     0             CURC              2 0
050600940223     C                   MOVE      *ZEROS        WIN              99
050700940224     C                   MOVE      *BLANK        WTBERR            1
050800940223     C*
050900940223     C* Indici
051000940223     C                   Z-ADD     0             X                 3 0
051100940223     C                   Z-ADD     0             Y                 3 0
051200940127     C*
051300940117     C                   ENDSR
051400940128     C************************************************************
