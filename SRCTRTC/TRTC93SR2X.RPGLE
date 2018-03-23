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
002400111221$001 FTRTC93SD2 CF   E             WORKSTN
002500940201     F                                     INFDS(DSFMT)
002600111221     FedSTS01L  UF A E           K DISK
002700111221     Fedtab01L  iF   E           K DISK
002800940128     D*----------------------------------------------------*
002900940127     D* Passaggio Parametri
003000940127     D KPJBA         E DS
003100100409     D dUte01        E DS
003200111221     D EDidsCE       E DS
003300111221     D COD_CE          s              6    DIM(100)
003400111221     D DES_CE          s             35    DIM(100)
003500100618     D*-------------
003600111221     d  savRAGS        s                   Like(xtaRAGS)
003700111221     d  savUNBCLI      s                   Like(stsUNBCLI)
003800111221     d  savCODSTS      s                   Like(stsCODSTS)
003900111221     d  savSUBSTS      s                   Like(stsSUBSTS)
004000111221     D*-------------
004100100618     D UTEDSE0F      E DS
004200100618     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
004300100618     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
004400100618      * Ds scomposzione tipo capoconti
004500100618     D TCUDS           DS
004600100618     D  F34                    3      4
004700100618     D  F56                    5      6
004800100618      *
004900100325     D*-------------
005000111221     D* Parametri in ricezione
005100111221     D  TABDS          DS
005200111221     D  XTAOPZ                 1      2
005300111221     D  XTARET                 3      3
005400111221     D  XTAOPR                 4      4
005500111221     D  XTAERR                 5      5
005600111221     D  XTAKEY1                6      8
005700111221     D  XTAKEY2                9     11
005800111221     D  XTACOD1               12     14
005900111221     D  XTACOD2               15     17
006000111221     D  XTAUNBcli             18     52
006100111221     D  XTARAGs               53     84
006200111221     D*-------------
006300030204     D  S_TASTO        S                   like($Tasto)
006400940201     D DSFMT           DS
006500940506     D  $TASTO               369    369
006600940201     D  NRG                  370    370
006700940201     D  NCL                  371    371
006800940201     D*-------------
006900940201     D* posizione cursore
007000940201     D CURSOR          DS
007100940223     D  RIRI                   1      2B 0 INZ
007200940201     D  R$$                    2      2
007300940223     D  COCO                   3      4B 0 INZ
007400940201     D  C$$                    4      4
007500940207     D*-------------
007600940207     D* Reperimento nome PGM
007700940207     D STATUS         SDS           333
007800940207     D  DSPGM            *PROC
007900940225     D*-------------
008000940225     D* COSTANTI
008100940225     D*-------------
008200940506     D* Tasti di funzione
008300940506     D F01             C                   CONST(X'31')
008400940506     D F02             C                   CONST(X'32')
008500940506     D F03             C                   CONST(X'33')
008600940506     D F04             C                   CONST(X'34')
008700940506     D F05             C                   CONST(X'35')
008800940506     D F06             C                   CONST(X'36')
008900940506     D F07             C                   CONST(X'37')
009000940506     D F08             C                   CONST(X'38')
009100940506     D F09             C                   CONST(X'39')
009200940506     D F10             C                   CONST(X'3A')
009300940506     D F11             C                   CONST(X'3B')
009400940506     D F12             C                   CONST(X'3C')
009500940506     D F13             C                   CONST(X'B1')
009600940506     D F14             C                   CONST(X'B2')
009700940506     D F15             C                   CONST(X'B3')
009800940506     D F16             C                   CONST(X'B4')
009900940506     D F17             C                   CONST(X'B5')
010000940506     D F18             C                   CONST(X'B6')
010100940506     D F19             C                   CONST(X'B7')
010200940506     D F20             C                   CONST(X'B8')
010300940506     D F21             C                   CONST(X'B9')
010400940506     D F22             C                   CONST(X'BA')
010500940506     D F23             C                   CONST(X'BB')
010600940506     D F24             C                   CONST(X'BC')
010700940506     D ENTER           C                   CONST(X'F1')
010800940506     D ROLDWN          C                   CONST(X'F4')
010900940506     D ROLLUP          C                   CONST(X'F5')
011000940506     D*-------------
011100941108     D DATA            C                   CONST('0001-01-01')
011200030114     D Annull          C                   CONST('  ANNULLATO  ')
011300100323     D OPz_Immissione  C                   CONST('IMMISSIONE')
011400100323     D OPz_Variazione  C                   CONST('VARIAZIONE')
011500100323     D OPz_Annulla     C                   CONST(' ANNULLA  ')
011600100323     D OPz_Visualizza  C                   CONST('VISUALIZZA')
011700100323     D OPz_Riattiva    C                   CONST(' RIATTIVA ')
011800940127     C*----------------------------------------------------*
011900940127     C*                MAIN LINE PROGRAM
012000940127     C*----------------------------------------------------*
012100100121     c                   exsr      INIZIO
012200940223     C*
012300940223     C     $FINE         DOWEQ     *OFF
012400940202     C     $GEST         CASEQ     'D1'          GESD1
012500940117     C                   END
012600940117     C                   END
012700940325     C* fine programma
012800030113     c                   movel     tabds         kpjbu
012900030113     C                   Seton                                        LR
013000940131     C************************************************************
013100100121     C* OPERAZIONI INIZIALI
013200100121     C************************************************************
013300100121     C     INIZIO        BEGSR
013400100121     C*
013500100121     C* Reperimento parametri
013600100121     C     *ENTRY        PLIST
013700100121     C                   PARM                    KPJBA
013800111221     C*
013900111221     C                   eval        TABDS = KPJBU
014000111221     c                   eval      savUNBCLI = xtaUNBCLI
014100111221     c                   eval      savRAGS   = xtaRAGS
014200100121      *
014300100121     c                   setoff                                       08
014400100121     C*
014500100121     C* Variabili per gestione videate
014600100121     C                   MOVE      'D1'          $GEST
014700100121     C                   MOVE      *OFF          $FINE
014800100121     C                   MOVE      *ON           $INZD1
014900100121     C                   MOVE      *BLANK        $LASTG
015000100121     C                   MOVE      *BLANK        $LASTV
015100100121     C                   Z-ADD     0             $ULKD1            3 0
015200100121     C*
015300100121     C* Variabili appoggio
015400100121     C                   Z-ADD     0             CURR
015500100121     C                   Z-ADD     0             CURC
015600100618      *
015700100618     C* RECUPERO DATI DELL'UTENTE
015800100618     C                   Z-ADD     1             CODUT             1 0
015900100618     C                   CALL      'XPARUT'
016000100618     C                   PARM                    UTEDSE0F
016100100618     C                   MOVEL     RAGUT         RSUT             20
016200100618     C*
016300100618     C* RICERCA CAPOCONTI
016400100618     C                   DO        50            X
016500100618     C                   MOVE      TCU(X)        TCUDS
016600100618     C     F56           IFEQ      'CG'
016700100618     C     F34           ANDEQ     '01'
016800100618     C                   Z-ADD     KCU(X)        KCI               4 0
016900100618     C                   END
017000100618     C                   END
017100100121     C*
017200100121     C* Valorizzazione campi univoci testate
017300100121     C                   CLEAR                   T1
017400100121     C                   MOVEL     KNSIF         NOMSIF
017500100121     C******             MOVEL     XSCDSI        NOMDIT
017600100121     C                   MOVEL     DSPGM         NOMPGM
017700100121     C*
017800111221     C* Carica tabella CE per decodificare i significati
017900111221      *------------
018000111221     C* Caricamento Tabella Partner esteri
018100111221     C                   Z-ADD     0             X                 3 0
018200111221     C                   MOVEL     'CE'          TABCOD
018300111221     C     TABCOD        CHAIN     EDTAB01L                           30
018400111221     C     *IN30         DOWEQ     '0'
018500111221     C     TABFLG        IFEQ      *BLANKS
018600111221     C                   ADD       1             X
018700111221     C                   MOVEL     TABKEY        COD_CE(X)
018800111221     C                   MOVEL     TABUNI        EDIdsCE
018900111221     C                   MOVEL     §ceDEC        DES_CE(X)
019000111221     C                   END
019100111221     C     TABCOD        READE     EDTAB01L                               30
019200111221     C                   END
019300111221      *------------
019400100121     C* Inizializzazione/Decodifica prima videata
019500100121     C                   EXSR      INZD1
019600100121     C                   MOVE      *OFF          $INZD1
019700100121     C*
019800100121     C                   ENDSR
019900100121     C************************************************************
020000100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
020100100121     C************************************************************
020200100121     C     DEFCAM        BEGSR
020300100121     C*
020400100121     C* klist
020500111221     c     key_STS       Klist
020600111221     c                   kfld                    stsUNBCLI
020700111221     c                   kfld                    stsCODSTS
020800111221     c                   kfld                    stsSUBSTS
020900111221      *
021000100121     C* Variabili per gestione videate
021100100121     C                   MOVE      *BLANK        $GEST             2
021200100121     C                   MOVE      *BLANK        $FINE             1
021300100121     C                   MOVE      *BLANK        $INZD1            1
021400100121     C                   MOVE      *BLANK        $LASTG            2
021500100121     C                   MOVE      *BLANK        $LASTV            2
021600100121     C*
021700100121     C* Variabili appoggio
021800100121     C                   Z-ADD     0             CURR              2 0
021900100121     C                   Z-ADD     0             CURC              2 0
022000100121     C                   MOVE      *ZEROS        WIN              99
022100100121     C* Indici
022200100121     C                   Z-ADD     0             X                 3 0
022300100121     C                   Z-ADD     0             Y                 3 0
022400100121     C*
022500100121     C                   ENDSR
022600100121     C************************************************************
022700940510     C* GESTIONE VIDEO RECORD D1
022800940131     C************************************************************
022900940127     C     GESD1         BEGSR
023000030113      *
023100940225     C* inizializzazione videata
023200940223     C     $INZD1        IFEQ      *ON
023300940127     C                   EXSR      INZD1
023400940223     C                   MOVE      *OFF          $INZD1
023500940117     C                   END
023600030113      *
023700940225     C* emissione piede videata se proveniente da altra
023800940225     C* salvataggio valore formato di provenienza
023900940207     C     $LASTV        IFNE      'D1'
024000940614     C                   WRITE     T1
024100940202     C                   MOVE      $LASTV        $LASTG
024200940202     C                   MOVE      'D1'          $LASTV
024300940202     C                   END
024400940131     C*
024500940203     C                   SELECT
024600100323     C* Inserimento e Copia
024700030114    >C     xtaopz        WHENEQ    '01'
024800100121    >C     xtaopz        OREQ      '03'
024900100323     c                   movel     Opz_ImmissioneD1Fun
025000100323     C* Visualizzazione
025100030114    >C     xtaopz        WHENEQ    '05'
025200100323     c                   movel     Opz_VisualizzaD1Fun
025300030114     C                   SETON                                        020306
025400100323     C* Annulla
025500030114    >C     xtaopz        WHENEQ    '04'
025600100323     c                   movel     Opz_Annulla   D1Fun
025700030114     C                   SETON                                        0203
025800030114     C                   SETOFF                                         06
025900100323     C* Ripristina
026000100323    >C     xtaopz        WHENEQ    '08'
026100100323     c                   movel     OPz_Riattiva  D1Fun
026200100323     C                   SETON                                        02  03
026300100323     C                   SETON                                        06  18
026400940620     C* Manutenzione
026500030113    >C     xtaopz        WHENEQ    '02'
026600100323     c                   movel     Opz_VariazioneD1Fun
026700030114     C                   SETON                                        02
026800030114     C                   SETOFF                                       0306
026900940620     C*
027000940620     C                   OTHER
027100090226     C*
027200030114     C                   SETOFF                                       020306
027300940203     C                   ENDSL
027400030114     C*
027500030114     c                   SETOFF                                         99
027600940620     C* Immissione/Modifica/Copia
027700030113     C*               *----------------*
027800940620     C                   EXFMT     D1
027900030113     C*               *----------------*
028000030114     C*
028100030114     C     *IN99         IFEQ      *OFF
028200030114     C*
0283000301141    C                   SELECT
028400030114     C* F3=Fine
028500030114     C     $TASTO        WHENEQ    F03
028600030114     C                   EXSR      F03D1
028700030114     C* F12=Ritorno
028800030114     C     $TASTO        WHENEQ    F12
028900030114     C                   EXSR      F12D1
029000030114     C*
0291000301141O   C                   OTHER
029200030114     C*
029300030114     C                   EXSR      CTRD1
029400030113     C*
029500940616     C     *IN99         IFEQ      *OFF
029600940616     C* F6
0297000302052    C     $TASTO        ifeq      F06
0298001003232    C     $TASTO        oreq      F18
0299000301152    C     $TASTO        oreq      F23
030000940224     C* eseguo aggiornamento
030100940315    >C                   EXSR      AGGANA
030200940224     C* eseguo operazioni del dopo-aggiornamento
030300030114     C  N99              EXSR      GESAGG
030400030205     c                   end
030500030114     C*
030600030114     C                   ENDIF
030700030114     C*
0308000301142-   C                   ENDSL
030900030114     C*
0310000301142-   C                   ENDIF
031100030114     C*
031200940117     C                   ENDSR
031300940117      ************************************************************
031400940207      * INIZIALIZZAZIONE VIDEATA DATI
031500940117      ************************************************************
031600940127     C     INZD1         BEGSR
031700940131     C*
031800940615     C                   CLEAR                   D1
031900100121     C*
032000111221     C                   move      xtaKEY1       d1cod
032100111221     C                   move      xtaKEy2       d1cod1
032200100618      *
032300940224     C* valorizzo la chiave univoca per reperire i dati
032400940224     C* se non immissione
0325000301141   >C     xtaopz        IFNE      '01'
032600940315    >C*-------------------------------------------------------------
032700940315    >C* impostare i campi chiave ricevuti
032800940315    >C*-------------------------------------------------------------
032900100121     C                   exsr      Imposta_KEY
033000111221     C     key_STS       chain(N)  edSTS01L
033100111221     C                   IF        %Found(edSTS01L)
033200940224     C* riempio la videata
033300940224     C                   EXSR      RIED1
033400030113      *
033500940224     C* richiamo routine dei ctrl per decodificarla
033600940614    >C                   EXSR      CTRD1
033700030113      *
033800940224     C                   MOVE      *ALL'0'       IN5098
033900940224     C                   MOVEA     IN5098        *IN(50)
034000030113      *
034100940224     C* definizione indicatori su video
034200940224     C                   SETOFF                                       02
0343009402243    C                   SELECT
034400030114    >C     xtaopz        WHENEQ    '02'
034500940224     C                   SETON                                        02
034600030114    >C     xtaopz        WHENEQ    '05'
0347009402243-   C                   ENDSL
034800940224     C*
0349009402242-   C                   ENDIF
035000030114     C*
0351000301141   >C                   Else
035200030114     C* Se in Immissione nuovo record
035300030114     c                   exsr      NewD1
0354009402241-   C                   ENDIF
035500940224     C*
035600940117     C                   ENDSR
035700940207     C/EJECT
035800940207     C************************************************************
035900940510     C* RIEMPIMENTO VIDEATA  D1
036000940207     C************************************************************
036100030114     C     NEWD1         BEGSR
036200940207     C*
036300030114     c                   clear                   d1
036400111221     C                   eval      D1unbcli  = savUNBCLI
036500111221     C                   eval      D1rags    = savRAGS
036600100322     C*
036700030114     C                   ENDSR
036800030114     C************************************************************
036900030114     C* RIEMPIMENTO VIDEATA  D1
037000030114     C************************************************************
037100030114     C     RIED1         BEGSR
037200030114     C*
037300111221     C                   eval      D1rags    = savRAGS
037400111221     C                   eval      D1unbcli  = STSUNBCLI
037500111221     C                   eval      D1cod     = STSCODSTS
037600111221     C                   eval      D1cod1    = STSSUBSTS
037700111221     C                   eval      D1brt     = STSCODSTSB
037800111221     C                   eval      D1brt1    = STSSUBSTSB
037900111221     C                   eval      D1des     = STSDESCRI
038000100322     C*
038100111221     C                   Z-ADD     1             XX                3 0
038200111221     c                   clear                   cod06             6
038300111221     C                   eval      cod06 = d1brt + d1brt1
038400111221     C     Cod06         LOOKUP    COD_CE(XX)                             32
038500111221     C   32              MOVEL     DES_CE(XX)    d1des1
038600111221     C*
038700100121     c                   setoff                                       08
038800030204     C*
038900940207     C                   ENDSR
039000940309     C************************************************************
039100940309     C* GESTIONE F03 VIDEO D1
039200940309     C************************************************************
039300940309     C     F03D1         BEGSR
039400940309     C*
039500940309     C                   MOVE      *ON           $FINE
039600030113    >C                   MOVE      '1'           xtaret
039700940325     C*
039800940325     C                   ENDSR
039900940309     C************************************************************
040000940309     C* GESTIONE F12 VIDEO D1
040100940309     C************************************************************
040200940309     C     F12D1         BEGSR
040300940309     C*
040400940309     C                   MOVE      *ON           $FINE
040500100325    >C     xtaopz        ifeq      '01'
040600100325    >C                   MOVE      '2'           xtaret
040700100325     c                   end
040800940309     C*
040900940309     C                   ENDSR
041000940131     C************************************************************
041100940207     C* CONTROLLO VIDEATA
041200940131     C************************************************************
041300940127     C     CTRD1         BEGSR
041400940131     C*
041500940127     C                   SETOFF                                       99
041600030123      *
041700940127     C* non imposto pos. del cursore
041800940127     C                   Z-ADD     0             H1RIGA
041900940127     C                   Z-ADD     0             H1COLO
042000940621     C*
042100940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0422000301141   >C     xtaopz        IFNE      '04'
042300971110     C*
042400971110     C* controllo codice vuoto
042500111221$017 C     D1cod         Ifeq      *blank
042600100618     C                   SETON                                          5199
042700090226     C                   Else
042800090226     C*** se già presente
042900090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
043000090226    >C     xtaopz        IFEQ      '01'
043100090226    >C     xtaopz        oreq      '03'
043200100121     C                   exsr      Imposta_KEY
043300111221     c                   eval         stsUNBCLI =  d1UNBCLI
043400111221     C     Key_STS       chain(N)  edSTS01L
043500111221     c                   if        %Found(edSTS01L)
043600100121     C                   SETON                                        5499
043700090226     C                   EndIF
043800090226     C                   ENDIF
043900100618     C*
044000090226     C                   ENDIF
044100100121     C*
044200100121     C* controllo codice tabella
044300100121    >C     xtaopz        IFne      '01'
044400100121    >C     xtaopz        andNE     '03'
044500100121     C                   exsr      Imposta_KEY
044600111221     C     key_STS       chain     edSTS01l
044700111221     c                   if        not %Found(edSTS01l)
044800100121     C**** se sbagliato il codice
044900100121     C                   SETON                                        5599
045000090226     c                   end
045100100121     c                   endIF
045200100618      *
045300090226     C                   ENDIF
045400100121      *
045500100325      * -------------
045600111221      *   Controlli sui campi
045700100325      * -------------
045800111221      *  se non è stato immesso nulla
045900111221     c                   if        D1BRT = *blank
046000100618     c                   seton                                        6199
046100100325     C                   ENDIF
046200100325      *
046300111221     c                   if        D1DES = *blank
046400111221     c                   seton                                        6299
046500111221     C                   ENDIF
046600111221      *
046700100325      *    Altri:
046800111221     C*
046900111221     C                   Z-ADD     1             XX                3 0
047000111221     c                   clear                   cod06             6
047100111221     C                   eval      cod06 = d1brt + d1brt1
047200111221     C     Cod06         LOOKUP    COD_CE(XX)                             32
047300111221     C   32              MOVEL     DES_CE(XX)    d1des1
047400111221     C*
047500100325      *
047600100325      * -------------
047700940302     C     *IN99         IFEQ      *ON
047800940510     C     $GEST         ANDEQ     'D1'
047900940117     C                   MOVEA     *IN           WIN
048000940128     C                   MOVE      *ALL'0'       IN5098           49
048100940128     C                   MOVEA     IN5098        *IN(50)
048200940614     C                   WRITE     D1
048300940117     C                   MOVEA     WIN           *IN
048400940302     C                   ENDIF
048500940127     C*
048600940117     C                   ENDSR
048700030204     C/EJECT
048800030204     C************************************************************
048900030204    >C* AGGIORNAMENTO ANAGRAFICA
049000030204     C************************************************************
049100100325    >C     FLDAGG        BEGSR
049200030204     C*
049300030204     C*  campi chiave da scrivere e aggiornare
049400111221     C                   exsr      Imposta_KEY
049500100322     C*
049600100618     c                   exsr      RIEANA
049700100121     C*
049800030204     C                   ENDSR
049900940207     C/EJECT
050000940224     C************************************************************
050100940315    >C* AGGIORNAMENTO ANAGRAFICA
050200940224     C************************************************************
050300940315    >C     AGGANA        BEGSR
050400940207     C*
050500100408     c                   clear                   aggiorna_FLG      1
050600100408     C*
050700030204     C                   exsr      FLDAGG
050800030204     C*
050900940207     C                   SELECT
051000030204     C*
051100100323      *  Immette/Copia il record
051200030114    >C     xtaopz        WHENEQ    '01'
051300030114    >C     xtaopz        OREQ      '03'
051400030204     C*
051500100408     c                   eval      aggiorna_FLG = 'S'
051600100408     C*
051700090226    >C                   EXSR      RIEANA
051800111221     c                   write     edsts000                             59
051900940318     C* se rcd non scrivibile attivo errore
052000030114    >C   59              SETON                                          99
052100030204      *
052200100323      *  Modifica il record
052300030114    >C     xtaopz        WHENEQ    '02'
052400100408     c*
052500100408     c                   eval      aggiorna_FLG = 'S'
052600100408     c*
052700100121     C                   exsr      Imposta_KEY
052800111221     C     Key_STS       chain     edSTS01L
052900030204     C                   exsr      FLDAGG
053000090226    >C                   EXSR      RIEANA
053100111221     c                   update    edSTS000                             59
053200030114    >C   59              SETON                                          99
053300030204      *
053400100323      *  Cancella il record
053500030114    >C     xtaopz        WHENEQ    '04'
053600100408     c*
053700100408     c                   eval      aggiorna_FLG = 'S'
053800100408     c*
053900100121     C                   exsr      Imposta_KEY
054000111221     C     Key_STS       chain     edSTS01L
054100111221     c                   delete    edSTS000                             59
054200030114    >C   59              SETON                                          99
054300100323      *
054400940207     C                   ENDSL
054500100408     C*
054600940203     C                   ENDSR
054700940224     C************************************************************
054800940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
054900940224     C************************************************************
055000090226     C     RIEANA        BEGSR
055100030114     C*
055200111221     C                   eval       STSCODSTS   = D1cod
055300111221     C                   eval       STSSUBSTS   = D1cod1
055400111221     C                   eval       STSCODSTSB  = D1brt
055500111221     C                   eval       STSSUBSTSB  = D1brt1
055600100618     C*
055700111221     C                   MOVEl     D1des         stsDESCRI
055800111221     C                   MOVEl     D1unbCLI      stsUNBCLI
055900111221     C*
056000111221     C                   Z-ADD     1             XX                3 0
056100111221     c                   clear                   cod06             6
056200111221     C                   eval      cod06 = d1brt + d1brt1
056300111221     C     Cod06         LOOKUP    COD_CE(XX)                             32
056400111221     C   32              MOVEL     DES_CE(XX)    d1des1
056500111221     C*
056600100121     C*
056700100324     C                   ENDSR
056800940224     C************************************************************
056900940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
057000940224     C************************************************************
057100940224     C     GESAGG        BEGSR
057200940224     C*
057300940224     C* segnalo al pgm chiamante l'avvenuta conferma
057400030114    >C                   MOVE      *ON           xtaopr
057500940224     C* segnalo al pgm chiamante l'aver premuto l'F6
057600030114    >C                   MOVE      '0'           xtaret
057700940224     C*
0578009402241    C                   SELECT
057900940224     C* nel caso di immissione, ripristino la videata iniziale
058000030114    >C     xtaopz        WHENEQ    '01'
058100940224     C                   MOVE      *ON           $INZD1
058200030114     C                   MOVE      *ON           $FINE
058300940224     C* altrimenti ritorno al pgm chiamante
058400940224     C                   OTHER
058500940224     C                   MOVE      *ON           $FINE
0586009402241-   C                   ENDSL
058700940224     C*
058800940224     C                   ENDSR
058900940131     C************************************************************
059000100121     C*
059100100121     C************************************************************
059200100121     C     Imposta_KEY   BEGSR
059300100121     C*
059400111221     c                   eval         stsUNBCLI = savUNBCLI
059500111221     c                   eval         stsCODSTS =  d1cod
059600111221     c                   eval         stsSUBSTS =  d1cod1
059700100121     C*
059800100121     C                   ENDSR
