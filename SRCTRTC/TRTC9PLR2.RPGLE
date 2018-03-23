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
002300141021$001 FTRTC9PLD2 CF   E             WORKSTN
002400940201     F                                     INFDS(DSFMT)
002500141021     FedPFL00F  UF A E             DISK
002600141021     F                                     INFDS(PFLDS)
002700141021     FedPFL01L  iF   E           K DISK    rename(edpfl000:edpfl001)
002800141022     F                                     INFDS(PFL1DS)
002900111221     Fedtab01L  iF   E           K DISK
003000141022     Fazorg01L  iF   E           K DISK
003100940128     D*----------------------------------------------------*
003200940127     D* Passaggio Parametri
003300940127     D KPJBA         E DS
003400141021     D*-------------
003500141021     D* Parametri in ricezione
003600141021     D  TABDS        E DS                  EXTNAME(TRTC9PLDS)
003700111221     D*-------------
003800100618     D UTEDSE0F      E DS
003900100618     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
004000100618     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
004100100618      * Ds scomposzione tipo capoconti
004200100618     D TCUDS           DS
004300100618     D  F34                    3      4
004400100618     D  F56                    5      6
004500131011     D*-------------
004600141021     d PFLds           ds
004700141021     d  PFL_nrr              397    400b 0
004800141022     D*-------------
004900141022     d PFL1ds          ds
005000141022     d  PFL1_nrr             397    400b 0
005100111221     D*-------------
005200030204     D  S_TASTO        S                   like($Tasto)
005300940201     D DSFMT           DS
005400940506     D  $TASTO               369    369
005500940201     D  NRG                  370    370
005600940201     D  NCL                  371    371
005700940201     D*-------------
005800940201     D* posizione cursore
005900940201     D CURSOR          DS
006000940223     D  RIRI                   1      2B 0 INZ
006100940201     D  R$$                    2      2
006200940223     D  COCO                   3      4B 0 INZ
006300940201     D  C$$                    4      4
006400940207     D*-------------
006500940207     D* Reperimento nome PGM
006600940207     D STATUS         SDS           333
006700940207     D  DSPGM            *PROC
006800141022     D*-------------
006900141022     d dataiso         s               d   datfmt(*iso)
007000141022     d dataeur         s               d   datfmt(*eur)
007100141021     D*-------------
007200141022     D OG143         E DS
007300141022     D*-------------
007400141022     D EDidsPT       E DS
007500141022     D COD_PT          s             35    DIM(100)
007600141022     D LNA_PT          s              3S 0 DIM(100)
007700141022     D UNI_pt          s             90    DIM(100)
007800141021     D*-------------
007900141022     d  savLUOGO       s                   Like(pflLUOGO)
008000141022     d  savLINEA       s                   Like(pflLINEAA)
008100141022      *
008200141022     D WLBDA8          DS
008300141022     D  G02DAT                 1      8  0
008400141022     D  G02INV                 9     16  0
008500141022     D  G02ERR                17     17
008600141022     D  G02TGI                18     22  0
008700141022      *
008800141022     D*------------->>>>>>>>>>>>>>>>>>
008900940225     D* COSTANTI
009000940225     D*-------------
009100940506     D* Tasti di funzione
009200940506     D F01             C                   CONST(X'31')
009300940506     D F02             C                   CONST(X'32')
009400940506     D F03             C                   CONST(X'33')
009500940506     D F04             C                   CONST(X'34')
009600940506     D F05             C                   CONST(X'35')
009700940506     D F06             C                   CONST(X'36')
009800940506     D F07             C                   CONST(X'37')
009900940506     D F08             C                   CONST(X'38')
010000940506     D F09             C                   CONST(X'39')
010100940506     D F10             C                   CONST(X'3A')
010200940506     D F11             C                   CONST(X'3B')
010300940506     D F12             C                   CONST(X'3C')
010400940506     D F13             C                   CONST(X'B1')
010500940506     D F14             C                   CONST(X'B2')
010600940506     D F15             C                   CONST(X'B3')
010700940506     D F16             C                   CONST(X'B4')
010800940506     D F17             C                   CONST(X'B5')
010900940506     D F18             C                   CONST(X'B6')
011000940506     D F19             C                   CONST(X'B7')
011100940506     D F20             C                   CONST(X'B8')
011200940506     D F21             C                   CONST(X'B9')
011300940506     D F22             C                   CONST(X'BA')
011400940506     D F23             C                   CONST(X'BB')
011500940506     D F24             C                   CONST(X'BC')
011600940506     D ENTER           C                   CONST(X'F1')
011700940506     D ROLDWN          C                   CONST(X'F4')
011800940506     D ROLLUP          C                   CONST(X'F5')
011900940506     D*-------------
012000941108     D DATA            C                   CONST('0001-01-01')
012100030114     D Annull          C                   CONST('  ANNULLATO  ')
012200100323     D OPz_Immissione  C                   CONST('IMMISSIONE')
012300100323     D OPz_Variazione  C                   CONST('VARIAZIONE')
012400100323     D OPz_Annulla     C                   CONST(' ANNULLA  ')
012500100323     D OPz_Visualizza  C                   CONST('VISUALIZZA')
012600100323     D OPz_Riattiva    C                   CONST(' RIATTIVA ')
012700940127     C*----------------------------------------------------*
012800940127     C*                MAIN LINE PROGRAM
012900940127     C*----------------------------------------------------*
013000100121     c                   exsr      INIZIO
013100940223     C*
013200940223     C     $FINE         DOWEQ     *OFF
013300940202     C     $GEST         CASEQ     'D1'          GESD1
013400940117     C                   END
013500940117     C                   END
013600940325     C* fine programma
013700030113     c                   movel     tabds         kpjbu
013800030113     C                   Seton                                        LR
013900940131     C************************************************************
014000100121     C* OPERAZIONI INIZIALI
014100100121     C************************************************************
014200100121     C     INIZIO        BEGSR
014300100121     C*
014400100121     C* Reperimento parametri
014500100121     C     *ENTRY        PLIST
014600100121     C                   PARM                    KPJBA
014700111221     C*
014800111221     C                   eval        TABDS = KPJBU
014900141021     C*
015000141022     c                   eval      savLINEA  =    xtaKLNA
015100141022     c                   eval      savLUOGO  =    xtaKLUO
015200100121     c                   setoff                                       08
015300100121     C*
015400100121     C* Variabili per gestione videate
015500100121     C                   MOVE      'D1'          $GEST
015600100121     C                   MOVE      *OFF          $FINE
015700100121     C                   MOVE      *ON           $INZD1
015800100121     C                   MOVE      *BLANK        $LASTG
015900100121     C                   MOVE      *BLANK        $LASTV
016000100121     C                   Z-ADD     0             $ULKD1            3 0
016100100121     C* Variabili appoggio
016200100121     C                   Z-ADD     0             CURR
016300100121     C                   Z-ADD     0             CURC
016400100618      *
016500100618     C* RECUPERO DATI DELL'UTENTE
016600100618     C                   Z-ADD     1             CODUT             1 0
016700100618     C                   CALL      'XPARUT'
016800100618     C                   PARM                    UTEDSE0F
016900100618     C                   MOVEL     RAGUT         RSUT             20
017000100618     C*
017100100618     C* RICERCA CAPOCONTI
017200100618     C                   DO        50            X
017300100618     C                   MOVE      TCU(X)        TCUDS
017400100618     C     F56           IFEQ      'CG'
017500100618     C     F34           ANDEQ     '01'
017600100618     C                   Z-ADD     KCU(X)        KCI               4 0
017700100618     C                   END
017800100618     C                   END
017900100121     C*
018000100121     C* Valorizzazione campi univoci testate
018100100121     C                   CLEAR                   T1
018200100121     C                   MOVEL     KNSIF         NOMSIF
018300100121     C******             MOVEL     XSCDSI        NOMDIT
018400100121     C                   MOVEL     DSPGM         NOMPGM
018500111221      *------------
018600111221     C* Caricamento Tabella Partner esteri
018700111221     C                   Z-ADD     0             X                 3 0
018800141022     C                   MOVEL     'PT'          TABCOD
018900111221     C     TABCOD        CHAIN     EDTAB01L                           30
019000111221     C     *IN30         DOWEQ     '0'
019100141022      *
019200111221     C     TABFLG        IFEQ      *BLANKS
019300141022     c                   movel     tabUNI        edidsPT
019400141022      *
019500141022     c     §ptLNP        chain     azorg01l
019600141022     c                   if        %Found(azorg01l)
019700141022     c                   movel     orgde3        og143
019800141022      *  Solo x PARTNER EEX
019900141022     c                   if        §OGNTW = 'EEX'
020000111221     C                   ADD       1             X
020100141022     C                   MOVEL     TABKEY        COD_PT(X)
020200141022     C                   MOVEL     TABUNI        UNI_pt(X)
020300141022     C                   MOVEL     TABUNI        edidsPT
020400141022     C                   MOVEL     §ptLNP        LNA_pt(X)
020500141022     c                   end
020600141022     c                   end
020700141022      *
020800111221     C                   END
020900111221     C     TABCOD        READE     EDTAB01L                               30
021000111221     C                   END
021100111221      *------------
021200100121     C* Inizializzazione/Decodifica prima videata
021300100121     C                   EXSR      INZD1
021400100121     C                   MOVE      *OFF          $INZD1
021500100121     C*
021600100121     C                   ENDSR
021700100121     C************************************************************
021800100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
021900100121     C************************************************************
022000100121     C     DEFCAM        BEGSR
022100100121     C*
022200100121     C* klist
022300141022     c     key_PFL1      Klist
022400141022     c                   kfld                    pflLINEAA
022500141022     c     key_PFL2      Klist
022600141022     c                   kfld                    pflLINEAA
022700141022     c                   kfld                    pflLUOGO
022800111221      *
022900100121     C* Variabili per gestione videate
023000100121     C                   MOVE      *BLANK        $GEST             2
023100100121     C                   MOVE      *BLANK        $FINE             1
023200100121     C                   MOVE      *BLANK        $INZD1            1
023300100121     C                   MOVE      *BLANK        $LASTG            2
023400100121     C                   MOVE      *BLANK        $LASTV            2
023500100121     C*
023600100121     C* Variabili appoggio
023700100121     C                   Z-ADD     0             CURR              2 0
023800100121     C                   Z-ADD     0             CURC              2 0
023900100121     C                   MOVE      *ZEROS        WIN              99
024000100121     C* Indici
024100100121     C                   Z-ADD     0             X                 3 0
024200100121     C                   Z-ADD     0             Y                 3 0
024300100121     C*
024400100121     C                   ENDSR
024500100121     C************************************************************
024600940510     C* GESTIONE VIDEO RECORD D1
024700940131     C************************************************************
024800940127     C     GESD1         BEGSR
024900030113      *
025000940225     C* inizializzazione videata
025100940223     C     $INZD1        IFEQ      *ON
025200940127     C                   EXSR      INZD1
025300940223     C                   MOVE      *OFF          $INZD1
025400940117     C                   END
025500030113      *
025600940225     C* emissione piede videata se proveniente da altra
025700940225     C* salvataggio valore formato di provenienza
025800940207     C     $LASTV        IFNE      'D1'
025900940614     C                   WRITE     T1
026000940202     C                   MOVE      $LASTV        $LASTG
026100940202     C                   MOVE      'D1'          $LASTV
026200940202     C                   END
026300940131     C*
026400940203     C                   SELECT
026500100323     C* Inserimento e Copia
026600141022    >C     xtaopz        WHENEQ    '10'
026700100121    >C     xtaopz        OREQ      '03'
026800100323     c                   movel     Opz_ImmissioneD1Fun
026900100323     C* Visualizzazione
027000030114    >C     xtaopz        WHENEQ    '05'
027100100323     c                   movel     Opz_VisualizzaD1Fun
027200030114     C                   SETON                                        020306
027300100323     C* Annulla
027400030114    >C     xtaopz        WHENEQ    '04'
027500100323     c                   movel     Opz_Annulla   D1Fun
027600030114     C                   SETON                                        0203
027700030114     C                   SETOFF                                         06
027800100323     C* Ripristina
027900100323    >C     xtaopz        WHENEQ    '08'
028000100323     c                   movel     OPz_Riattiva  D1Fun
028100100323     C                   SETON                                        02  03
028200100323     C                   SETON                                        06  18
028300940620     C* Manutenzione
028400030113    >C     xtaopz        WHENEQ    '02'
028500100323     c                   movel     Opz_VariazioneD1Fun
028600030114     C                   SETON                                        02
028700030114     C                   SETOFF                                       0306
028800940620     C*
028900940620     C                   OTHER
029000090226     C*
029100030114     C                   SETOFF                                       020306
029200940203     C                   ENDSL
029300030114     C*
029400030114     c                   SETOFF                                         99
029500940620     C* Immissione/Modifica/Copia
029600030113     C*               *----------------*
029700940620     C                   EXFMT     D1
029800030113     C*               *----------------*
029900141022     c                   setoff                                       44
030000030114     C*
030100030114     C     *IN99         IFEQ      *OFF
030200030114     C*
0303000301141    C                   SELECT
030400030114     C* F3=Fine
030500030114     C     $TASTO        WHENEQ    F03
030600030114     C                   EXSR      F03D1
030700030114     C* F12=Ritorno
030800030114     C     $TASTO        WHENEQ    F12
030900030114     C                   EXSR      F12D1
031000030114     C*
0311000301141O   C                   OTHER
031200030114     C*
031300030114     C                   EXSR      CTRD1
031400030113     C*
031500940616     C     *IN99         IFEQ      *OFF
031600940616     C* F6
0317000302052    C     $TASTO        ifeq      F06
0318001003232    C     $TASTO        oreq      F18
0319000301152    C     $TASTO        oreq      F23
032000940224     C* eseguo aggiornamento
032100141022     C*
032200940315    >C                   EXSR      AGGANA
032300141022     C*
032400940224     C* eseguo operazioni del dopo-aggiornamento
032500030114     C  N99              EXSR      GESAGG
032600141022     C*
032700030205     c                   end
032800030114     C*
032900030114     C                   ENDIF
033000030114     C*
0331000301142-   C                   ENDSL
033200030114     C*
0333000301142-   C                   ENDIF
033400030114     C*
033500940117     C                   ENDSR
033600940117      ************************************************************
033700940207      * INIZIALIZZAZIONE VIDEATA DATI
033800940117      ************************************************************
033900940127     C     INZD1         BEGSR
034000940131     C*
034100940615     C                   CLEAR                   D1
034200141022     c                   setoff                                       44
034300100121     C*
034400141022     C                   move      XTAKLNA       d1linea
034500141022     C                   move      XTAKLUO       d1luogo
034600131011     C                   z-add     xtaNREC       h1NREC
034700100618      *
034800940224     C* valorizzo la chiave univoca per reperire i dati
034900940224     C* se non immissione
0350001410221   >C     xtaopz        IFNE      '10'
035100940315    >C*-------------------------------------------------------------
035200940315    >C* impostare i campi chiave ricevuti
035300940315    >C*-------------------------------------------------------------
035400141022     C     h1Nrec        chain(N)  edPFL00F
035500141022     C                   IF        %Found(edPFL00f)
035600940224     C* riempio la videata
035700940224     C                   EXSR      RIED1
035800030113      *
035900940224     C* richiamo routine dei ctrl per decodificarla
036000940614    >C                   EXSR      CTRD1
036100030113      *
036200940224     C                   MOVE      *ALL'0'       IN5098
036300940224     C                   MOVEA     IN5098        *IN(50)
036400030113      *
036500940224     C* definizione indicatori su video
036600940224     C                   SETOFF                                       02
0367009402243    C                   SELECT
036800030114    >C     xtaopz        WHENEQ    '02'
036900940224     C                   SETON                                        02
037000030114    >C     xtaopz        WHENEQ    '05'
0371009402243-   C                   ENDSL
037200940224     C*
0373009402242-   C                   ENDIF
037400030114     C*
0375000301141   >C                   Else
037600030114     C* Se in Immissione nuovo record
037700030114     c                   exsr      NewD1
0378009402241-   C                   ENDIF
037900940224     C*
038000940117     C                   ENDSR
038100940207     C/EJECT
038200940207     C************************************************************
038300940510     C* RIEMPIMENTO VIDEATA  D1
038400940207     C************************************************************
038500030114     C     NEWD1         BEGSR
038600940207     C*
038700030114     c                   clear                   d1
038800141022     C                   eval      D1LINEA   = savLINEA
038900141023     C                   eval      D1LUOGO   = '?'
039000100322     C*
039100030114     C                   ENDSR
039200030114     C************************************************************
039300030114     C* RIEMPIMENTO VIDEATA  D1
039400030114     C************************************************************
039500030114     C     RIED1         BEGSR
039600030114     C*
039700141022     C                   eval      D1LINEA   = PFLLINEAA
039800141022     C                   eval      D1LUOGO   = PFLLUOGO
039900141022     C                   eval      D1DES     = PFLDESCR
040000141022     C                   eval      D1PLIST   = PFLCODPFL
040100141022     C                   eval      H1DEC     = PFLDTDEC
040200141022     C                   eval      H1SCA     = PFLDTSCA
040300141022     c                   move      pflDTDEC      dataiso
040400141022     c                   move      dataiso       dataeur
040500141022     c                   move      dataeur       D1Dec
040600141022     c                   move      pflDTSCA      dataiso
040700141022     c                   move      dataiso       dataeur
040800141022     c                   move      dataeur       D1Sca
040900100322     C*
041000111221     C                   Z-ADD     1             XX                3 0
041100141022     C     D1LINEA       LOOKUP    LNA_PT(XX)                             32
041200141022     C   32              MOVEL     UNI_pt(XX)    edidspt
041300111221     C*
041400100121     c                   setoff                                       08
041500030204     C*
041600940207     C                   ENDSR
041700940309     C************************************************************
041800940309     C* GESTIONE F03 VIDEO D1
041900940309     C************************************************************
042000940309     C     F03D1         BEGSR
042100940309     C*
042200940309     C                   MOVE      *ON           $FINE
042300030113    >C                   MOVE      '1'           xtaret
042400940325     C*
042500940325     C                   ENDSR
042600940309     C************************************************************
042700940309     C* GESTIONE F12 VIDEO D1
042800940309     C************************************************************
042900940309     C     F12D1         BEGSR
043000940309     C*
043100940309     C                   MOVE      *ON           $FINE
043200141022    >C     xtaopz        ifeq      '10'
043300100325    >C                   MOVE      '2'           xtaret
043400100325     c                   end
043500940309     C*
043600940309     C                   ENDSR
043700940131     C************************************************************
043800940207     C* CONTROLLO VIDEATA
043900940131     C************************************************************
044000940127     C     CTRD1         BEGSR
044100940131     C*
044200940127     C                   SETOFF                                       99
044300030123      *
044400940127     C* non imposto pos. del cursore
044500940127     C                   Z-ADD     0             H1RIGA
044600940127     C                   Z-ADD     0             H1COLO
044700940621     C*
044800940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0449000301141   >C     xtaopz        IFNE      '04'
045000971110     C*
045100141022      * -------------
045200141022      *   Controlli sui campi
045300141022      * -------------
045400141022      *  se non è stato immesso nulla
045500141022      * LINEA EEX
045600141022     C                   Z-ADD     1             XX                3 0
045700141022     C     d1LINEA       LOOKUP    LNA_PT(XX)                             32
045800141022     c                   if        not *in32
045900141022     c                   seton                                        5699
046000141022     c                   elseif        *in32
046100141022     C                   MOVEL     UNI_pt(XX)    edidspt
046200141022     c                   end
046300141022     C*  Luogo
046400141022     c                   if        D1LUOGO = *blank
046500141022     c                   seton                                        5799
046600141023     C                   Else
046700141023     c                   clear                   pos               3 0
046800141024     c     '?'           SCAN      D1LUOGO       pos
046900141023     c                   if        pos > 0
047000141024     c                   call      'TRTC9PLR3'
047100141024     c                   parm                    parLUOGO         20
047200141024     c                   eval      d1LUOGO = parLUOGO
047300141024     c                   if        D1LUOGO = *blank
047400141024     c                   seton                                        5799
047500141024     c                   end
047600141024     c                   end
047700141023     C                   ENDIF
047800141022     C*  PLIST
047900141022     c                   if        D1PLIST = *blank
048000141022     c                   seton                                        5499
048100141022     C                   ENDIF
048200141022      *  Descrizione
048300141022     c                   if        D1DES = *blank
048400141022     c                   seton                                        5599
048500141022     C                   ENDIF
048600141022      *
048700141022     c                   clear                   h1DEC
048800141022     c                   clear                   h1SCA
048900141022      *   Controllo Date e
049000141022      *     periodi di sovrapposizione
049100141022     c                   if        d1DEC  > 0
049200141022     C                   MOVE      d1DEC         G02DAT
049300141022     C                   MOVEL     *BLANK        G02ERR
049400141022     C                   CALL      'XSRDA8'
049500141022     C                   PARM                    WLBDA8
049600141022     C     G02ERR        IFEQ      '1'
049700141022     C                   SETON                                          5199
049800141022     C                   ElsE
049900141022     c                   move      G02INV        H1DEC
050000141022     C                   END
050100141022     c                   else
050200141022     C                   SETON                                          5199
050300141022     c                   end
050400141022      *
050500141022     c                   if        d1SCA  > 0
050600141022     C                   MOVE      d1SCA         G02DAT
050700141022     C                   MOVEL     *BLANK        G02ERR
050800141022     C                   CALL      'XSRDA8'
050900141022     C                   PARM                    WLBDA8
051000141022     C     G02ERR        IFEQ      '1'
051100141022     C                   SETON                                          5299
051200141022     C                   ElsE
051300141022     c                   move      G02INV        H1SCA
051400141022     C                   END
051500141022     c                   else
051600141022     C                   SETON                                          5299
051700141022     c                   end
051800141022     C*
051900141022     C*   sequenza logica delle date
052000141022     c                   if        H1SCA < H1DEC and not *in99
052100141022     C                   SETON                                          5399
052200141022     c                   end
052300141022     C*
052400141022     C*  Controlli di sovrapposzione con altri records
052500141022     c                   eval         pflLINEAa = d1LINEA
052600141022     c                   eval         pflLUOGO  = d1LUOGO
052700141022      *
052800141022     C     key_PFL2      setll     edPFL01L
052900141022     C     key_PFL2      reade     edPFL01L
053000141022     c                   dow       not %EoF(edPFL01L)
053100141022      * non possono esserci periodi sovrapposti
053200141022     c                   if          H1DEC >= pflDTdec and  H1DEC <= pflDTsca
053300141022     c                               or
053400141022     c                               H1SCA >= pflDTdec and  H1SCA <= pflDTsca
053500141022      * se non è se stesso!!
053600141023      *   oppure in funzione di copia
053700141022     c                   if        PFL1_nrr <> H1NREC
053800141023     c                              or
053900141023     c                             xtaopz = '03'
054000141022     c                   move      pflDTDEC      dataiso
054100141022     c                   move      dataiso       dataeur
054200141022     c                   move      dataeur       DaData
054300141022     c                   move      pflDTSCA      dataiso
054400141022     c                   move      dataiso       dataeur
054500141022     c                   move      dataeur       aData
054600141022     C                   SETON                                        619944
054700131011     C                   Leave
054800141022     C                   End
054900131011     C                   End
055000141022     C     key_PFL2      reade     edPFL01L
055100131011     C                   End
055200100121     C*
055300100121     C* controllo codice tabella
055400141022    >C     xtaopz        IFne      '10'
055500100121    >C     xtaopz        andNE     '03'
055600141022     C     xtaNREC       chain(N)  edPFL00F
055700141022     c                   if        not %Found(edPFL00f)
055800100121     C**** se sbagliato il codice
055900141022     C                   SETON                                        6299
056000090226     c                   end
056100100121     c                   endIF
056200100618      *
056300090226     C                   ENDIF
056400100121      *
056500100325      *
056600100325      * -------------
056700940302     C     *IN99         IFEQ      *ON
056800940510     C     $GEST         ANDEQ     'D1'
056900940117     C                   MOVEA     *IN           WIN
057000940128     C                   MOVE      *ALL'0'       IN5098           49
057100940128     C                   MOVEA     IN5098        *IN(50)
057200940614     C                   WRITE     D1
057300940117     C                   MOVEA     WIN           *IN
057400940302     C                   ENDIF
057500940127     C*
057600940117     C                   ENDSR
057700030204     C/EJECT
057800030204     C************************************************************
057900030204    >C* AGGIORNAMENTO ANAGRAFICA
058000030204     C************************************************************
058100100325    >C     FLDAGG        BEGSR
058200030204     C*
058300030204     C*  campi chiave da scrivere e aggiornare
058400111221     C                   exsr      Imposta_KEY
058500100322     C*
058600100618     c                   exsr      RIEANA
058700100121     C*
058800030204     C                   ENDSR
058900940207     C/EJECT
059000940224     C************************************************************
059100940315    >C* AGGIORNAMENTO ANAGRAFICA
059200940224     C************************************************************
059300940315    >C     AGGANA        BEGSR
059400940207     C*
059500100408     c                   clear                   aggiorna_FLG      1
059600100408     C*
059700141022     C**********         exsr      FLDAGG
059800030204     C*
059900940207     C                   SELECT
060000030204     C*
060100100323      *  Immette/Copia il record
060200141022    >C     xtaopz        WHENEQ    '10'
060300030114    >C     xtaopz        OREQ      '03'
060400030204     C*
060500100408     c                   eval      aggiorna_FLG = 'S'
060600100408     C*
060700090226    >C                   EXSR      RIEANA
060800141022     c                   write     edPFL000                             59
060900940318     C* se rcd non scrivibile attivo errore
061000030114    >C   59              SETON                                          99
061100030204      *
061200100323      *  Modifica il record
061300030114    >C     xtaopz        WHENEQ    '02'
061400100408     c*
061500100408     c                   eval      aggiorna_FLG = 'S'
061600100408     c*
061700141022     C     xtaNREC       chain     edPFL00F
061800141022     C***********        exsr      FLDAGG
061900090226    >C                   EXSR      RIEANA
062000141022     c                   update    edPFL000                             59
062100030114    >C   59              SETON                                          99
062200030204      *
062300100323      *  Cancella il record
062400030114    >C     xtaopz        WHENEQ    '04'
062500100408     c*
062600100408     c                   eval      aggiorna_FLG = 'S'
062700100408     c*
062800141022     C     xtaNREC       chain     edPFL00F
062900141022     c                   delete    edPFL000                             59
063000030114    >C   59              SETON                                          99
063100100323      *
063200940207     C                   ENDSL
063300100408     C*
063400940203     C                   ENDSR
063500141022     C************************************************************
063600141022     C*
063700141022     C************************************************************
063800141022     C     Imposta_KEY   BEGSR
063900141022     C*
064000141022     c                   eval         pflLINEAa = d1LINEA
064100141022     c                   eval         pflLUOGO  = d1LUOGO
064200141022     C*
064300141022     C                   ENDSR
064400940224     C************************************************************
064500940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
064600940224     C************************************************************
064700090226     C     RIEANA        BEGSR
064800030114     C*
064900141022     C                   eval       pflLINEAA  =   D1LINEA
065000141022     C                   eval       pflLUOGO   =   D1LUOGO
065100141022     C                   eval       pflDESCR   =   D1DES
065200141022     C                   eval       pflCODPFL  =   D1PLIST
065300141022     C                   eval       pflDTDEC   =   H1DEC
065400141022     C                   eval       pflDTSCA   =   H1SCA
065500111221     C*
065600111221     C                   Z-ADD     1             XX                3 0
065700141022     C     d1linea       LOOKUP    LNA_PT(XX)                             32
065800141022     C   32              MOVEL     UNI_pt(XX)    edidspt
065900111221     C*
066000100121     C*
066100100324     C                   ENDSR
066200940224     C************************************************************
066300940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
066400940224     C************************************************************
066500940224     C     GESAGG        BEGSR
066600940224     C*
066700940224     C* segnalo al pgm chiamante l'avvenuta conferma
066800030114    >C                   MOVE      *ON           xtaopr
066900940224     C* segnalo al pgm chiamante l'aver premuto l'F6
067000030114    >C                   MOVE      '0'           xtaret
067100940224     C*
0672009402241    C                   SELECT
067300940224     C* nel caso di immissione, ripristino la videata iniziale
067400141022    >C     xtaopz        WHENEQ    '10'
067500940224     C                   MOVE      *ON           $INZD1
067600030114     C                   MOVE      *ON           $FINE
067700940224     C* altrimenti ritorno al pgm chiamante
067800940224     C                   OTHER
067900940224     C                   MOVE      *ON           $FINE
0680009402241-   C                   ENDSL
068100940224     C*
068200940224     C                   ENDSR
