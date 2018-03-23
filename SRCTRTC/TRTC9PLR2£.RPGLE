000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H DECEDIT('0,') DATEDIT(*DMY.)
000300940223      *
000400940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000500940620      *  03           PROTECT TUTTI I CAMPI
000600030123      *  09           PROTECT dei campi voce C/E
000700940223      *  21           GENERICO OPERAZIONI I/O
000800940223      *  22           GENERICO ERRORE OPERAZIONI I/O
000900940223      *  30           SFLDSP
001000940223      * N31           SFLCLR
001100940128      *  31           SFLDSPCTL
001200940128      *  32           SFLNXTCHG
001300940128      *  33           SFLEND
001400940128      *  39           OF PRTF
001500940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001600940317    > *  Specificare l'uso dei singoli indicatori
001700940315      *  50 <---> 98  ERRORI SU VIDEO
001800940317    > *  Specificare l'uso dei singoli indicatori
001900940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002000940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002100940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002200940128      *  99           INDIC. GENERALE DI ERRORE
002300940128     F*----------------------------------------------------*
002400141021$001 FTRTC9PLD2 CF   E             WORKSTN
002500940201     F                                     INFDS(DSFMT)
002600141021     FedPFL00F  UF A E             DISK
002700141021     F                                     INFDS(PFLDS)
002800141021     FedPFL01L  iF   E           K DISK    rename(edpfl000:edpfl001)
002900141022     F                                     INFDS(PFL1DS)
003000111221     Fedtab01L  iF   E           K DISK
003100141022     Fazorg01L  iF   E           K DISK
003200940128     D*----------------------------------------------------*
003300940127     D* Passaggio Parametri
003400940127     D KPJBA         E DS
003500141021     D*-------------
003600141021     D* Parametri in ricezione
003700141021     D  TABDS        E DS                  EXTNAME(TRTC9PLDS)
003800111221     D*-------------
003900100618     D UTEDSE0F      E DS
004000100618     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
004100100618     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
004200100618      * Ds scomposzione tipo capoconti
004300100618     D TCUDS           DS
004400100618     D  F34                    3      4
004500100618     D  F56                    5      6
004600100618      *
004700131011     D*-------------
004800141021     d PFLds           ds
004900141021     d  PFL_nrr              397    400b 0
005000141022     D*-------------
005100141022     d PFL1ds          ds
005200141022     d  PFL1_nrr             397    400b 0
005300111221     D*-------------
005400030204     D  S_TASTO        S                   like($Tasto)
005500940201     D DSFMT           DS
005600940506     D  $TASTO               369    369
005700940201     D  NRG                  370    370
005800940201     D  NCL                  371    371
005900940201     D*-------------
006000940201     D* posizione cursore
006100940201     D CURSOR          DS
006200940223     D  RIRI                   1      2B 0 INZ
006300940201     D  R$$                    2      2
006400940223     D  COCO                   3      4B 0 INZ
006500940201     D  C$$                    4      4
006600940207     D*-------------
006700940207     D* Reperimento nome PGM
006800940207     D STATUS         SDS           333
006900940207     D  DSPGM            *PROC
007000141022     D*-------------
007100141022     d dataiso         s               d   datfmt(*iso)
007200141022     d dataeur         s               d   datfmt(*eur)
007300141021     D*-------------
007400141022     D OG143         E DS
007500141022     D*-------------
007600141022     D EDidsPT       E DS
007700141022     D COD_PT          s             35    DIM(100)
007800141022     D LNA_PT          s              3S 0 DIM(100)
007900141022     D UNI_pt          s             90    DIM(100)
008000141021     D*-------------
008100141022     d  savLUOGO       s                   Like(pflLUOGO)
008200141022     d  savLINEA       s                   Like(pflLINEAA)
008300141022      *
008400141022     D WLBDA8          DS
008500141022     D  G02DAT                 1      8  0
008600141022     D  G02INV                 9     16  0
008700141022     D  G02ERR                17     17
008800141022     D  G02TGI                18     22  0
008900141022      *
009000141022     D*------------->>>>>>>>>>>>>>>>>>
009100940225     D* COSTANTI
009200940225     D*-------------
009300940506     D* Tasti di funzione
009400940506     D F01             C                   CONST(X'31')
009500940506     D F02             C                   CONST(X'32')
009600940506     D F03             C                   CONST(X'33')
009700940506     D F04             C                   CONST(X'34')
009800940506     D F05             C                   CONST(X'35')
009900940506     D F06             C                   CONST(X'36')
010000940506     D F07             C                   CONST(X'37')
010100940506     D F08             C                   CONST(X'38')
010200940506     D F09             C                   CONST(X'39')
010300940506     D F10             C                   CONST(X'3A')
010400940506     D F11             C                   CONST(X'3B')
010500940506     D F12             C                   CONST(X'3C')
010600940506     D F13             C                   CONST(X'B1')
010700940506     D F14             C                   CONST(X'B2')
010800940506     D F15             C                   CONST(X'B3')
010900940506     D F16             C                   CONST(X'B4')
011000940506     D F17             C                   CONST(X'B5')
011100940506     D F18             C                   CONST(X'B6')
011200940506     D F19             C                   CONST(X'B7')
011300940506     D F20             C                   CONST(X'B8')
011400940506     D F21             C                   CONST(X'B9')
011500940506     D F22             C                   CONST(X'BA')
011600940506     D F23             C                   CONST(X'BB')
011700940506     D F24             C                   CONST(X'BC')
011800940506     D ENTER           C                   CONST(X'F1')
011900940506     D ROLDWN          C                   CONST(X'F4')
012000940506     D ROLLUP          C                   CONST(X'F5')
012100940506     D*-------------
012200941108     D DATA            C                   CONST('0001-01-01')
012300030114     D Annull          C                   CONST('  ANNULLATO  ')
012400100323     D OPz_Immissione  C                   CONST('IMMISSIONE')
012500100323     D OPz_Variazione  C                   CONST('VARIAZIONE')
012600100323     D OPz_Annulla     C                   CONST(' ANNULLA  ')
012700100323     D OPz_Visualizza  C                   CONST('VISUALIZZA')
012800100323     D OPz_Riattiva    C                   CONST(' RIATTIVA ')
012900940127     C*----------------------------------------------------*
013000940127     C*                MAIN LINE PROGRAM
013100940127     C*----------------------------------------------------*
013200100121     c                   exsr      INIZIO
013300940223     C*
013400940223     C     $FINE         DOWEQ     *OFF
013500940202     C     $GEST         CASEQ     'D1'          GESD1
013600940117     C                   END
013700940117     C                   END
013800940325     C* fine programma
013900030113     c                   movel     tabds         kpjbu
014000030113     C                   Seton                                        LR
014100940131     C************************************************************
014200100121     C* OPERAZIONI INIZIALI
014300100121     C************************************************************
014400100121     C     INIZIO        BEGSR
014500100121     C*
014600100121     C* Reperimento parametri
014700100121     C     *ENTRY        PLIST
014800100121     C                   PARM                    KPJBA
014900111221     C*
015000111221     C                   eval        TABDS = KPJBU
015100141021     C*
015200141022     c                   eval      savLINEA  =    xtaKLNA
015300141022     c                   eval      savLUOGO  =    xtaKLUO
015400100121     c                   setoff                                       08
015500100121     C*
015600100121     C* Variabili per gestione videate
015700100121     C                   MOVE      'D1'          $GEST
015800100121     C                   MOVE      *OFF          $FINE
015900100121     C                   MOVE      *ON           $INZD1
016000100121     C                   MOVE      *BLANK        $LASTG
016100100121     C                   MOVE      *BLANK        $LASTV
016200100121     C                   Z-ADD     0             $ULKD1            3 0
016300100121     C* Variabili appoggio
016400100121     C                   Z-ADD     0             CURR
016500100121     C                   Z-ADD     0             CURC
016600100618      *
016700100618     C* RECUPERO DATI DELL'UTENTE
016800100618     C                   Z-ADD     1             CODUT             1 0
016900100618     C                   CALL      'XPARUT'
017000100618     C                   PARM                    UTEDSE0F
017100100618     C                   MOVEL     RAGUT         RSUT             20
017200100618     C*
017300100618     C* RICERCA CAPOCONTI
017400100618     C                   DO        50            X
017500100618     C                   MOVE      TCU(X)        TCUDS
017600100618     C     F56           IFEQ      'CG'
017700100618     C     F34           ANDEQ     '01'
017800100618     C                   Z-ADD     KCU(X)        KCI               4 0
017900100618     C                   END
018000100618     C                   END
018100100121     C*
018200100121     C* Valorizzazione campi univoci testate
018300100121     C                   CLEAR                   T1
018400100121     C                   MOVEL     KNSIF         NOMSIF
018500100121     C******             MOVEL     XSCDSI        NOMDIT
018600100121     C                   MOVEL     DSPGM         NOMPGM
018700111221      *------------
018800111221     C* Caricamento Tabella Partner esteri
018900111221     C                   Z-ADD     0             X                 3 0
019000141022     C                   MOVEL     'PT'          TABCOD
019100111221     C     TABCOD        CHAIN     EDTAB01L                           30
019200111221     C     *IN30         DOWEQ     '0'
019300141022      *
019400111221     C     TABFLG        IFEQ      *BLANKS
019500141022     c                   movel     tabUNI        edidsPT
019600141022      *
019700141022     c     §ptLNP        chain     azorg01l
019800141022     c                   if        %Found(azorg01l)
019900141022     c                   movel     orgde3        og143
020000141022      *  Solo x PARTNER EEX
020100141022     c                   if        §OGNTW = 'EEX'
020200111221     C                   ADD       1             X
020300141022     C                   MOVEL     TABKEY        COD_PT(X)
020400141022     C                   MOVEL     TABUNI        UNI_pt(X)
020500141022     C                   MOVEL     TABUNI        edidsPT
020600141022     C                   MOVEL     §ptLNP        LNA_pt(X)
020700141022     c                   end
020800141022     c                   end
020900141022      *
021000111221     C                   END
021100111221     C     TABCOD        READE     EDTAB01L                               30
021200111221     C                   END
021300111221      *------------
021400100121     C* Inizializzazione/Decodifica prima videata
021500100121     C                   EXSR      INZD1
021600100121     C                   MOVE      *OFF          $INZD1
021700100121     C*
021800100121     C                   ENDSR
021900100121     C************************************************************
022000100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
022100100121     C************************************************************
022200100121     C     DEFCAM        BEGSR
022300100121     C*
022400100121     C* klist
022500141022     c     key_PFL1      Klist
022600141022     c                   kfld                    pflLINEAA
022700141022     c     key_PFL2      Klist
022800141022     c                   kfld                    pflLINEAA
022900141022     c                   kfld                    pflLUOGO
023000111221      *
023100100121     C* Variabili per gestione videate
023200100121     C                   MOVE      *BLANK        $GEST             2
023300100121     C                   MOVE      *BLANK        $FINE             1
023400100121     C                   MOVE      *BLANK        $INZD1            1
023500100121     C                   MOVE      *BLANK        $LASTG            2
023600100121     C                   MOVE      *BLANK        $LASTV            2
023700100121     C*
023800100121     C* Variabili appoggio
023900100121     C                   Z-ADD     0             CURR              2 0
024000100121     C                   Z-ADD     0             CURC              2 0
024100100121     C                   MOVE      *ZEROS        WIN              99
024200100121     C* Indici
024300100121     C                   Z-ADD     0             X                 3 0
024400100121     C                   Z-ADD     0             Y                 3 0
024500100121     C*
024600100121     C                   ENDSR
024700100121     C************************************************************
024800940510     C* GESTIONE VIDEO RECORD D1
024900940131     C************************************************************
025000940127     C     GESD1         BEGSR
025100030113      *
025200940225     C* inizializzazione videata
025300940223     C     $INZD1        IFEQ      *ON
025400940127     C                   EXSR      INZD1
025500940223     C                   MOVE      *OFF          $INZD1
025600940117     C                   END
025700030113      *
025800940225     C* emissione piede videata se proveniente da altra
025900940225     C* salvataggio valore formato di provenienza
026000940207     C     $LASTV        IFNE      'D1'
026100940614     C                   WRITE     T1
026200940202     C                   MOVE      $LASTV        $LASTG
026300940202     C                   MOVE      'D1'          $LASTV
026400940202     C                   END
026500940131     C*
026600940203     C                   SELECT
026700100323     C* Inserimento e Copia
026800141022    >C     xtaopz        WHENEQ    '10'
026900100121    >C     xtaopz        OREQ      '03'
027000100323     c                   movel     Opz_ImmissioneD1Fun
027100100323     C* Visualizzazione
027200030114    >C     xtaopz        WHENEQ    '05'
027300100323     c                   movel     Opz_VisualizzaD1Fun
027400030114     C                   SETON                                        020306
027500100323     C* Annulla
027600030114    >C     xtaopz        WHENEQ    '04'
027700100323     c                   movel     Opz_Annulla   D1Fun
027800030114     C                   SETON                                        0203
027900030114     C                   SETOFF                                         06
028000100323     C* Ripristina
028100100323    >C     xtaopz        WHENEQ    '08'
028200100323     c                   movel     OPz_Riattiva  D1Fun
028300100323     C                   SETON                                        02  03
028400100323     C                   SETON                                        06  18
028500940620     C* Manutenzione
028600030113    >C     xtaopz        WHENEQ    '02'
028700100323     c                   movel     Opz_VariazioneD1Fun
028800030114     C                   SETON                                        02
028900030114     C                   SETOFF                                       0306
029000940620     C*
029100940620     C                   OTHER
029200090226     C*
029300030114     C                   SETOFF                                       020306
029400940203     C                   ENDSL
029500030114     C*
029600030114     c                   SETOFF                                         99
029700940620     C* Immissione/Modifica/Copia
029800030113     C*               *----------------*
029900940620     C                   EXFMT     D1
030000030113     C*               *----------------*
030100141022     c                   setoff                                       44
030200030114     C*
030300030114     C     *IN99         IFEQ      *OFF
030400030114     C*
0305000301141    C                   SELECT
030600030114     C* F3=Fine
030700030114     C     $TASTO        WHENEQ    F03
030800030114     C                   EXSR      F03D1
030900030114     C* F12=Ritorno
031000030114     C     $TASTO        WHENEQ    F12
031100030114     C                   EXSR      F12D1
031200030114     C*
0313000301141O   C                   OTHER
031400030114     C*
031500030114     C                   EXSR      CTRD1
031600030113     C*
031700940616     C     *IN99         IFEQ      *OFF
031800940616     C* F6
0319000302052    C     $TASTO        ifeq      F06
0320001003232    C     $TASTO        oreq      F18
0321000301152    C     $TASTO        oreq      F23
032200940224     C* eseguo aggiornamento
032300141022     C*
032400940315    >C                   EXSR      AGGANA
032500141022     C*
032600940224     C* eseguo operazioni del dopo-aggiornamento
032700030114     C  N99              EXSR      GESAGG
032800141022     C*
032900030205     c                   end
033000030114     C*
033100030114     C                   ENDIF
033200030114     C*
0333000301142-   C                   ENDSL
033400030114     C*
0335000301142-   C                   ENDIF
033600030114     C*
033700940117     C                   ENDSR
033800940117      ************************************************************
033900940207      * INIZIALIZZAZIONE VIDEATA DATI
034000940117      ************************************************************
034100940127     C     INZD1         BEGSR
034200940131     C*
034300940615     C                   CLEAR                   D1
034400141022     c                   setoff                                       44
034500100121     C*
034600141022     C                   move      XTAKLNA       d1linea
034700141022     C                   move      XTAKLUO       d1luogo
034800131011     C                   z-add     xtaNREC       h1NREC
034900100618      *
035000940224     C* valorizzo la chiave univoca per reperire i dati
035100940224     C* se non immissione
0352001410221   >C     xtaopz        IFNE      '10'
035300940315    >C*-------------------------------------------------------------
035400940315    >C* impostare i campi chiave ricevuti
035500940315    >C*-------------------------------------------------------------
035600141022     C     h1Nrec        chain(N)  edPFL00F
035700141022     C                   IF        %Found(edPFL00f)
035800940224     C* riempio la videata
035900940224     C                   EXSR      RIED1
036000030113      *
036100940224     C* richiamo routine dei ctrl per decodificarla
036200940614    >C                   EXSR      CTRD1
036300030113      *
036400940224     C                   MOVE      *ALL'0'       IN5098
036500940224     C                   MOVEA     IN5098        *IN(50)
036600030113      *
036700940224     C* definizione indicatori su video
036800940224     C                   SETOFF                                       02
0369009402243    C                   SELECT
037000030114    >C     xtaopz        WHENEQ    '02'
037100940224     C                   SETON                                        02
037200030114    >C     xtaopz        WHENEQ    '05'
0373009402243-   C                   ENDSL
037400940224     C*
0375009402242-   C                   ENDIF
037600030114     C*
0377000301141   >C                   Else
037800030114     C* Se in Immissione nuovo record
037900030114     c                   exsr      NewD1
0380009402241-   C                   ENDIF
038100940224     C*
038200940117     C                   ENDSR
038300940207     C/EJECT
038400940207     C************************************************************
038500940510     C* RIEMPIMENTO VIDEATA  D1
038600940207     C************************************************************
038700030114     C     NEWD1         BEGSR
038800940207     C*
038900030114     c                   clear                   d1
039000141022     C                   eval      D1LINEA   = savLINEA
039100141022     C                   eval      D1LUOGO   = savLUOGO
039200100322     C*
039300030114     C                   ENDSR
039400030114     C************************************************************
039500030114     C* RIEMPIMENTO VIDEATA  D1
039600030114     C************************************************************
039700030114     C     RIED1         BEGSR
039800030114     C*
039900141022     C                   eval      D1LINEA   = PFLLINEAA
040000141022     C                   eval      D1LUOGO   = PFLLUOGO
040100141022     C                   eval      D1DES     = PFLDESCR
040200141022     C                   eval      D1PLIST   = PFLCODPFL
040300141022     C                   eval      H1DEC     = PFLDTDEC
040400141022     C                   eval      H1SCA     = PFLDTSCA
040500141022     c                   move      pflDTDEC      dataiso
040600141022     c                   move      dataiso       dataeur
040700141022     c                   move      dataeur       D1Dec
040800141022     c                   move      pflDTSCA      dataiso
040900141022     c                   move      dataiso       dataeur
041000141022     c                   move      dataeur       D1Sca
041100100322     C*
041200111221     C                   Z-ADD     1             XX                3 0
041300141022     C     D1LINEA       LOOKUP    LNA_PT(XX)                             32
041400141022     C   32              MOVEL     UNI_pt(XX)    edidspt
041500111221     C*
041600100121     c                   setoff                                       08
041700030204     C*
041800940207     C                   ENDSR
041900940309     C************************************************************
042000940309     C* GESTIONE F03 VIDEO D1
042100940309     C************************************************************
042200940309     C     F03D1         BEGSR
042300940309     C*
042400940309     C                   MOVE      *ON           $FINE
042500030113    >C                   MOVE      '1'           xtaret
042600940325     C*
042700940325     C                   ENDSR
042800940309     C************************************************************
042900940309     C* GESTIONE F12 VIDEO D1
043000940309     C************************************************************
043100940309     C     F12D1         BEGSR
043200940309     C*
043300940309     C                   MOVE      *ON           $FINE
043400141022    >C     xtaopz        ifeq      '10'
043500100325    >C                   MOVE      '2'           xtaret
043600100325     c                   end
043700940309     C*
043800940309     C                   ENDSR
043900940131     C************************************************************
044000940207     C* CONTROLLO VIDEATA
044100940131     C************************************************************
044200940127     C     CTRD1         BEGSR
044300940131     C*
044400940127     C                   SETOFF                                       99
044500030123      *
044600940127     C* non imposto pos. del cursore
044700940127     C                   Z-ADD     0             H1RIGA
044800940127     C                   Z-ADD     0             H1COLO
044900940621     C*
045000940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0451000301141   >C     xtaopz        IFNE      '04'
045200971110     C*
045300141022      * -------------
045400141022      *   Controlli sui campi
045500141022      * -------------
045600141022      *  se non è stato immesso nulla
045700141022      * LINEA EEX
045800141022     C                   Z-ADD     1             XX                3 0
045900141022     C     d1LINEA       LOOKUP    LNA_PT(XX)                             32
046000141022     c                   if        not *in32
046100141022     c                   seton                                        5699
046200141022     c                   elseif        *in32
046300141022     C                   MOVEL     UNI_pt(XX)    edidspt
046400141022     c                   end
046500141022     C*  Luogo
046600141022     c                   if        D1LUOGO = *blank
046700141022     c                   seton                                        5799
046800141022     C                   ENDIF
046900141022     C*  PLIST
047000141022     c                   if        D1PLIST = *blank
047100141022     c                   seton                                        5499
047200141022     C                   ENDIF
047300141022      *  Descrizione
047400141022     c                   if        D1DES = *blank
047500141022     c                   seton                                        5599
047600141022     C                   ENDIF
047700141022      *
047800141022     c                   clear                   h1DEC
047900141022     c                   clear                   h1SCA
048000141022      *   Controllo Date e
048100141022      *     periodi di sovrapposizione
048200141022     c                   if        d1DEC  > 0
048300141022     C                   MOVE      d1DEC         G02DAT
048400141022     C                   MOVEL     *BLANK        G02ERR
048500141022     C                   CALL      'XSRDA8'
048600141022     C                   PARM                    WLBDA8
048700141022     C     G02ERR        IFEQ      '1'
048800141022     C                   SETON                                          5199
048900141022     C                   ElsE
049000141022     c                   move      G02INV        H1DEC
049100141022     C                   END
049200141022     c                   else
049300141022     C                   SETON                                          5199
049400141022     c                   end
049500141022      *
049600141022     c                   if        d1SCA  > 0
049700141022     C                   MOVE      d1SCA         G02DAT
049800141022     C                   MOVEL     *BLANK        G02ERR
049900141022     C                   CALL      'XSRDA8'
050000141022     C                   PARM                    WLBDA8
050100141022     C     G02ERR        IFEQ      '1'
050200141022     C                   SETON                                          5299
050300141022     C                   ElsE
050400141022     c                   move      G02INV        H1SCA
050500141022     C                   END
050600141022     c                   else
050700141022     C                   SETON                                          5299
050800141022     c                   end
050900141022     C*
051000141022     C*   sequenza logica delle date
051100141022     c                   if        H1SCA < H1DEC and not *in99
051200141022     C                   SETON                                          5399
051300141022     c                   end
051400141022     C*
051500141022     C*  Controlli di sovrapposzione con altri records
051600141022     c                   eval         pflLINEAa = d1LINEA
051700141022     c                   eval         pflLUOGO  = d1LUOGO
051800141022      *
051900141022     C     key_PFL2      setll     edPFL01L
052000141022     C     key_PFL2      reade     edPFL01L
052100141022     c                   dow       not %EoF(edPFL01L)
052200141022      * non possono esserci periodi sovrapposti
052300141022     c                   if          H1DEC >= pflDTdec and  H1DEC <= pflDTsca
052400141022     c                               or
052500141022     c                               H1SCA >= pflDTdec and  H1SCA <= pflDTsca
052600141022      * se non è se stesso!!
052700141022     c                   if        PFL1_nrr <> H1NREC
052800141022     c                   move      pflDTDEC      dataiso
052900141022     c                   move      dataiso       dataeur
053000141022     c                   move      dataeur       DaData
053100141022     c                   move      pflDTSCA      dataiso
053200141022     c                   move      dataiso       dataeur
053300141022     c                   move      dataeur       aData
053400141022     C                   SETON                                        619944
053500131011     C                   Leave
053600141022     C                   End
053700131011     C                   End
053800141022     C     key_PFL2      reade     edPFL01L
053900131011     C                   End
054000100121     C*
054100100121     C* controllo codice tabella
054200141022    >C     xtaopz        IFne      '10'
054300100121    >C     xtaopz        andNE     '03'
054400141022     C     xtaNREC       chain(N)  edPFL00F
054500141022     c                   if        not %Found(edPFL00f)
054600100121     C**** se sbagliato il codice
054700141022     C                   SETON                                        6299
054800090226     c                   end
054900100121     c                   endIF
055000100618      *
055100090226     C                   ENDIF
055200100121      *
055300100325      *
055400100325      * -------------
055500940302     C     *IN99         IFEQ      *ON
055600940510     C     $GEST         ANDEQ     'D1'
055700940117     C                   MOVEA     *IN           WIN
055800940128     C                   MOVE      *ALL'0'       IN5098           49
055900940128     C                   MOVEA     IN5098        *IN(50)
056000940614     C                   WRITE     D1
056100940117     C                   MOVEA     WIN           *IN
056200940302     C                   ENDIF
056300940127     C*
056400940117     C                   ENDSR
056500030204     C/EJECT
056600030204     C************************************************************
056700030204    >C* AGGIORNAMENTO ANAGRAFICA
056800030204     C************************************************************
056900100325    >C     FLDAGG        BEGSR
057000030204     C*
057100030204     C*  campi chiave da scrivere e aggiornare
057200111221     C                   exsr      Imposta_KEY
057300100322     C*
057400100618     c                   exsr      RIEANA
057500100121     C*
057600030204     C                   ENDSR
057700940207     C/EJECT
057800940224     C************************************************************
057900940315    >C* AGGIORNAMENTO ANAGRAFICA
058000940224     C************************************************************
058100940315    >C     AGGANA        BEGSR
058200940207     C*
058300100408     c                   clear                   aggiorna_FLG      1
058400100408     C*
058500141022     C**********         exsr      FLDAGG
058600030204     C*
058700940207     C                   SELECT
058800030204     C*
058900100323      *  Immette/Copia il record
059000141022    >C     xtaopz        WHENEQ    '10'
059100030114    >C     xtaopz        OREQ      '03'
059200030204     C*
059300100408     c                   eval      aggiorna_FLG = 'S'
059400100408     C*
059500090226    >C                   EXSR      RIEANA
059600141022     c                   write     edPFL000                             59
059700940318     C* se rcd non scrivibile attivo errore
059800030114    >C   59              SETON                                          99
059900030204      *
060000100323      *  Modifica il record
060100030114    >C     xtaopz        WHENEQ    '02'
060200100408     c*
060300100408     c                   eval      aggiorna_FLG = 'S'
060400100408     c*
060500141022     C     xtaNREC       chain     edPFL00F
060600141022     C***********        exsr      FLDAGG
060700090226    >C                   EXSR      RIEANA
060800141022     c                   update    edPFL000                             59
060900030114    >C   59              SETON                                          99
061000030204      *
061100100323      *  Cancella il record
061200030114    >C     xtaopz        WHENEQ    '04'
061300100408     c*
061400100408     c                   eval      aggiorna_FLG = 'S'
061500100408     c*
061600141022     C     xtaNREC       chain     edPFL00F
061700141022     c                   delete    edPFL000                             59
061800030114    >C   59              SETON                                          99
061900100323      *
062000940207     C                   ENDSL
062100100408     C*
062200940203     C                   ENDSR
062300141022     C************************************************************
062400141022     C*
062500141022     C************************************************************
062600141022     C     Imposta_KEY   BEGSR
062700141022     C*
062800141022     c                   eval         pflLINEAa = d1LINEA
062900141022     c                   eval         pflLUOGO  = d1LUOGO
063000141022     C*
063100141022     C                   ENDSR
063200940224     C************************************************************
063300940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
063400940224     C************************************************************
063500090226     C     RIEANA        BEGSR
063600030114     C*
063700141022     C                   eval       pflLINEAA  =   D1LINEA
063800141022     C                   eval       pflLUOGO   =   D1LUOGO
063900141022     C                   eval       pflDESCR   =   D1DES
064000141022     C                   eval       pflCODPFL  =   D1PLIST
064100141022     C                   eval       pflDTDEC   =   H1DEC
064200141022     C                   eval       pflDTSCA   =   H1SCA
064300111221     C*
064400111221     C                   Z-ADD     1             XX                3 0
064500141022     C     d1linea       LOOKUP    LNA_PT(XX)                             32
064600141022     C   32              MOVEL     UNI_pt(XX)    edidspt
064700111221     C*
064800100121     C*
064900100324     C                   ENDSR
065000940224     C************************************************************
065100940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
065200940224     C************************************************************
065300940224     C     GESAGG        BEGSR
065400940224     C*
065500940224     C* segnalo al pgm chiamante l'avvenuta conferma
065600030114    >C                   MOVE      *ON           xtaopr
065700940224     C* segnalo al pgm chiamante l'aver premuto l'F6
065800030114    >C                   MOVE      '0'           xtaret
065900940224     C*
0660009402241    C                   SELECT
066100940224     C* nel caso di immissione, ripristino la videata iniziale
066200141022    >C     xtaopz        WHENEQ    '10'
066300940224     C                   MOVE      *ON           $INZD1
066400030114     C                   MOVE      *ON           $FINE
066500940224     C* altrimenti ritorno al pgm chiamante
066600940224     C                   OTHER
066700940224     C                   MOVE      *ON           $FINE
0668009402241-   C                   ENDSL
066900940224     C*
067000940224     C                   ENDSR
