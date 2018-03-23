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
002300111221$001 FTRTC93SD2 CF   E             WORKSTN
002400940201     F                                     INFDS(DSFMT)
002500131011     F****edSTS01L  UF A E           K DISK
002600131011     FedSTS00F  UF A E             DISK
002700131011     F                                     INFDS(STSDS)
002800131011     FedSTS01L  iF   E           K DISK    rename(edsts000:edsts001)
002900111221     Fedtab01L  iF   E           K DISK
003000940128     D*----------------------------------------------------*
003100940127     D* Passaggio Parametri
003200940127     D KPJBA         E DS
003300100409     D dUte01        E DS
003400111221     D EDidsCE       E DS
003500111221     D COD_CE          s              6    DIM(100)
003600111221     D DES_CE          s             35    DIM(100)
003700100618     D*-------------
003800111221     d  savRAGS        s                   Like(xtaRAGS)
003900111221     d  savUNBCLI      s                   Like(stsUNBCLI)
004000111221     d  savCODSTS      s                   Like(stsCODSTS)
004100111221     d  savSUBSTS      s                   Like(stsSUBSTS)
004200111221     D*-------------
004300100618     D UTEDSE0F      E DS
004400100618     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
004500100618     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
004600100618      * Ds scomposzione tipo capoconti
004700100618     D TCUDS           DS
004800100618     D  F34                    3      4
004900100618     D  F56                    5      6
005000100618      *
005100100325     D*-------------
005200111221     D* Parametri in ricezione
005300111221     D  TABDS          DS
005400111221     D  XTAOPZ                 1      2
005500111221     D  XTARET                 3      3
005600111221     D  XTAOPR                 4      4
005700111221     D  XTAERR                 5      5
005800111221     D  XTAKEY1                6      8
005900111221     D  XTAKEY2                9     11
006000111221     D  XTACOD1               12     14
006100111221     D  XTACOD2               15     17
006200111221     D  XTAUNBcli             18     52
006300111221     D  XTARAGs               53     84
006400131011     D  XTANREC               85     93  0
006500131011     D*-------------
006600131011     d stsds           ds
006700131011     d  sts_nrr              397    400b 0
006800111221     D*-------------
006900030204     D  S_TASTO        S                   like($Tasto)
007000940201     D DSFMT           DS
007100940506     D  $TASTO               369    369
007200940201     D  NRG                  370    370
007300940201     D  NCL                  371    371
007400940201     D*-------------
007500940201     D* posizione cursore
007600940201     D CURSOR          DS
007700940223     D  RIRI                   1      2B 0 INZ
007800940201     D  R$$                    2      2
007900940223     D  COCO                   3      4B 0 INZ
008000940201     D  C$$                    4      4
008100940207     D*-------------
008200940207     D* Reperimento nome PGM
008300940207     D STATUS         SDS           333
008400940207     D  DSPGM            *PROC
008500940225     D*-------------
008600940225     D* COSTANTI
008700940225     D*-------------
008800940506     D* Tasti di funzione
008900940506     D F01             C                   CONST(X'31')
009000940506     D F02             C                   CONST(X'32')
009100940506     D F03             C                   CONST(X'33')
009200940506     D F04             C                   CONST(X'34')
009300940506     D F05             C                   CONST(X'35')
009400940506     D F06             C                   CONST(X'36')
009500940506     D F07             C                   CONST(X'37')
009600940506     D F08             C                   CONST(X'38')
009700940506     D F09             C                   CONST(X'39')
009800940506     D F10             C                   CONST(X'3A')
009900940506     D F11             C                   CONST(X'3B')
010000940506     D F12             C                   CONST(X'3C')
010100940506     D F13             C                   CONST(X'B1')
010200940506     D F14             C                   CONST(X'B2')
010300940506     D F15             C                   CONST(X'B3')
010400940506     D F16             C                   CONST(X'B4')
010500940506     D F17             C                   CONST(X'B5')
010600940506     D F18             C                   CONST(X'B6')
010700940506     D F19             C                   CONST(X'B7')
010800940506     D F20             C                   CONST(X'B8')
010900940506     D F21             C                   CONST(X'B9')
011000940506     D F22             C                   CONST(X'BA')
011100940506     D F23             C                   CONST(X'BB')
011200940506     D F24             C                   CONST(X'BC')
011300940506     D ENTER           C                   CONST(X'F1')
011400940506     D ROLDWN          C                   CONST(X'F4')
011500940506     D ROLLUP          C                   CONST(X'F5')
011600940506     D*-------------
011700941108     D DATA            C                   CONST('0001-01-01')
011800030114     D Annull          C                   CONST('  ANNULLATO  ')
011900100323     D OPz_Immissione  C                   CONST('IMMISSIONE')
012000100323     D OPz_Variazione  C                   CONST('VARIAZIONE')
012100100323     D OPz_Annulla     C                   CONST(' ANNULLA  ')
012200100323     D OPz_Visualizza  C                   CONST('VISUALIZZA')
012300100323     D OPz_Riattiva    C                   CONST(' RIATTIVA ')
012400940127     C*----------------------------------------------------*
012500940127     C*                MAIN LINE PROGRAM
012600940127     C*----------------------------------------------------*
012700100121     c                   exsr      INIZIO
012800940223     C*
012900940223     C     $FINE         DOWEQ     *OFF
013000940202     C     $GEST         CASEQ     'D1'          GESD1
013100940117     C                   END
013200940117     C                   END
013300940325     C* fine programma
013400030113     c                   movel     tabds         kpjbu
013500030113     C                   Seton                                        LR
013600940131     C************************************************************
013700100121     C* OPERAZIONI INIZIALI
013800100121     C************************************************************
013900100121     C     INIZIO        BEGSR
014000100121     C*
014100100121     C* Reperimento parametri
014200100121     C     *ENTRY        PLIST
014300100121     C                   PARM                    KPJBA
014400111221     C*
014500111221     C                   eval        TABDS = KPJBU
014600111221     c                   eval      savUNBCLI = xtaUNBCLI
014700111221     c                   eval      savRAGS   = xtaRAGS
014800100121      *
014900100121     c                   setoff                                       08
015000100121     C*
015100100121     C* Variabili per gestione videate
015200100121     C                   MOVE      'D1'          $GEST
015300100121     C                   MOVE      *OFF          $FINE
015400100121     C                   MOVE      *ON           $INZD1
015500100121     C                   MOVE      *BLANK        $LASTG
015600100121     C                   MOVE      *BLANK        $LASTV
015700100121     C                   Z-ADD     0             $ULKD1            3 0
015800100121     C*
015900100121     C* Variabili appoggio
016000100121     C                   Z-ADD     0             CURR
016100100121     C                   Z-ADD     0             CURC
016200100618      *
016300100618     C* RECUPERO DATI DELL'UTENTE
016400100618     C                   Z-ADD     1             CODUT             1 0
016500100618     C                   CALL      'XPARUT'
016600100618     C                   PARM                    UTEDSE0F
016700100618     C                   MOVEL     RAGUT         RSUT             20
016800100618     C*
016900100618     C* RICERCA CAPOCONTI
017000100618     C                   DO        50            X
017100100618     C                   MOVE      TCU(X)        TCUDS
017200100618     C     F56           IFEQ      'CG'
017300100618     C     F34           ANDEQ     '01'
017400100618     C                   Z-ADD     KCU(X)        KCI               4 0
017500100618     C                   END
017600100618     C                   END
017700100121     C*
017800100121     C* Valorizzazione campi univoci testate
017900100121     C                   CLEAR                   T1
018000100121     C                   MOVEL     KNSIF         NOMSIF
018100100121     C******             MOVEL     XSCDSI        NOMDIT
018200100121     C                   MOVEL     DSPGM         NOMPGM
018300100121     C*
018400111221     C* Carica tabella CE per decodificare i significati
018500111221      *------------
018600111221     C* Caricamento Tabella Partner esteri
018700111221     C                   Z-ADD     0             X                 3 0
018800111221     C                   MOVEL     'CE'          TABCOD
018900111221     C     TABCOD        CHAIN     EDTAB01L                           30
019000111221     C     *IN30         DOWEQ     '0'
019100111221     C     TABFLG        IFEQ      *BLANKS
019200111221     C                   ADD       1             X
019300111221     C                   MOVEL     TABKEY        COD_CE(X)
019400111221     C                   MOVEL     TABUNI        EDIdsCE
019500111221     C                   MOVEL     §ceDEC        DES_CE(X)
019600111221     C                   END
019700111221     C     TABCOD        READE     EDTAB01L                               30
019800111221     C                   END
019900111221      *------------
020000100121     C* Inizializzazione/Decodifica prima videata
020100100121     C                   EXSR      INZD1
020200100121     C                   MOVE      *OFF          $INZD1
020300100121     C*
020400100121     C                   ENDSR
020500100121     C************************************************************
020600100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
020700100121     C************************************************************
020800100121     C     DEFCAM        BEGSR
020900100121     C*
021000100121     C* klist
021100111221     c     key_STS       Klist
021200111221     c                   kfld                    stsUNBCLI
021300111221     c                   kfld                    stsCODSTS
021400111221     c                   kfld                    stsSUBSTS
021500111221      *
021600100121     C* Variabili per gestione videate
021700100121     C                   MOVE      *BLANK        $GEST             2
021800100121     C                   MOVE      *BLANK        $FINE             1
021900100121     C                   MOVE      *BLANK        $INZD1            1
022000100121     C                   MOVE      *BLANK        $LASTG            2
022100100121     C                   MOVE      *BLANK        $LASTV            2
022200100121     C*
022300100121     C* Variabili appoggio
022400100121     C                   Z-ADD     0             CURR              2 0
022500100121     C                   Z-ADD     0             CURC              2 0
022600100121     C                   MOVE      *ZEROS        WIN              99
022700100121     C* Indici
022800100121     C                   Z-ADD     0             X                 3 0
022900100121     C                   Z-ADD     0             Y                 3 0
023000100121     C*
023100100121     C                   ENDSR
023200100121     C************************************************************
023300940510     C* GESTIONE VIDEO RECORD D1
023400940131     C************************************************************
023500940127     C     GESD1         BEGSR
023600030113      *
023700940225     C* inizializzazione videata
023800940223     C     $INZD1        IFEQ      *ON
023900940127     C                   EXSR      INZD1
024000940223     C                   MOVE      *OFF          $INZD1
024100940117     C                   END
024200030113      *
024300940225     C* emissione piede videata se proveniente da altra
024400940225     C* salvataggio valore formato di provenienza
024500940207     C     $LASTV        IFNE      'D1'
024600940614     C                   WRITE     T1
024700940202     C                   MOVE      $LASTV        $LASTG
024800940202     C                   MOVE      'D1'          $LASTV
024900940202     C                   END
025000940131     C*
025100940203     C                   SELECT
025200100323     C* Inserimento e Copia
025300030114    >C     xtaopz        WHENEQ    '01'
025400100121    >C     xtaopz        OREQ      '03'
025500100323     c                   movel     Opz_ImmissioneD1Fun
025600100323     C* Visualizzazione
025700030114    >C     xtaopz        WHENEQ    '05'
025800100323     c                   movel     Opz_VisualizzaD1Fun
025900030114     C                   SETON                                        020306
026000100323     C* Annulla
026100030114    >C     xtaopz        WHENEQ    '04'
026200100323     c                   movel     Opz_Annulla   D1Fun
026300030114     C                   SETON                                        0203
026400030114     C                   SETOFF                                         06
026500100323     C* Ripristina
026600100323    >C     xtaopz        WHENEQ    '08'
026700100323     c                   movel     OPz_Riattiva  D1Fun
026800100323     C                   SETON                                        02  03
026900100323     C                   SETON                                        06  18
027000940620     C* Manutenzione
027100030113    >C     xtaopz        WHENEQ    '02'
027200100323     c                   movel     Opz_VariazioneD1Fun
027300030114     C                   SETON                                        02
027400030114     C                   SETOFF                                       0306
027500940620     C*
027600940620     C                   OTHER
027700090226     C*
027800030114     C                   SETOFF                                       020306
027900940203     C                   ENDSL
028000030114     C*
028100030114     c                   SETOFF                                         99
028200940620     C* Immissione/Modifica/Copia
028300030113     C*               *----------------*
028400940620     C                   EXFMT     D1
028500030113     C*               *----------------*
028600030114     C*
028700030114     C     *IN99         IFEQ      *OFF
028800030114     C*
0289000301141    C                   SELECT
029000030114     C* F3=Fine
029100030114     C     $TASTO        WHENEQ    F03
029200030114     C                   EXSR      F03D1
029300030114     C* F12=Ritorno
029400030114     C     $TASTO        WHENEQ    F12
029500030114     C                   EXSR      F12D1
029600030114     C*
0297000301141O   C                   OTHER
029800030114     C*
029900030114     C                   EXSR      CTRD1
030000030113     C*
030100940616     C     *IN99         IFEQ      *OFF
030200940616     C* F6
0303000302052    C     $TASTO        ifeq      F06
0304001003232    C     $TASTO        oreq      F18
0305000301152    C     $TASTO        oreq      F23
030600940224     C* eseguo aggiornamento
030700940315    >C                   EXSR      AGGANA
030800940224     C* eseguo operazioni del dopo-aggiornamento
030900030114     C  N99              EXSR      GESAGG
031000030205     c                   end
031100030114     C*
031200030114     C                   ENDIF
031300030114     C*
0314000301142-   C                   ENDSL
031500030114     C*
0316000301142-   C                   ENDIF
031700030114     C*
031800940117     C                   ENDSR
031900940117      ************************************************************
032000940207      * INIZIALIZZAZIONE VIDEATA DATI
032100940117      ************************************************************
032200940127     C     INZD1         BEGSR
032300940131     C*
032400940615     C                   CLEAR                   D1
032500100121     C*
032600111221     C                   move      xtaKEY1       d1cod
032700111221     C                   move      xtaKEy2       d1cod1
032800131011     C                   z-add     xtaNREC       h1NREC
032900100618      *
033000940224     C* valorizzo la chiave univoca per reperire i dati
033100940224     C* se non immissione
0332000301141   >C     xtaopz        IFNE      '01'
033300940315    >C*-------------------------------------------------------------
033400940315    >C* impostare i campi chiave ricevuti
033500940315    >C*-------------------------------------------------------------
033600131011     C*************      exsr      Imposta_KEY
033700131011     C*****key_STS       chain(N)  edSTS01L
033800131011     C     h1Nrec        chain(N)  edSTS00F
033900131011     C                   IF        %Found(edSTS00f)
034000940224     C* riempio la videata
034100940224     C                   EXSR      RIED1
034200030113      *
034300940224     C* richiamo routine dei ctrl per decodificarla
034400940614    >C                   EXSR      CTRD1
034500030113      *
034600940224     C                   MOVE      *ALL'0'       IN5098
034700940224     C                   MOVEA     IN5098        *IN(50)
034800030113      *
034900940224     C* definizione indicatori su video
035000940224     C                   SETOFF                                       02
0351009402243    C                   SELECT
035200030114    >C     xtaopz        WHENEQ    '02'
035300940224     C                   SETON                                        02
035400030114    >C     xtaopz        WHENEQ    '05'
0355009402243-   C                   ENDSL
035600940224     C*
0357009402242-   C                   ENDIF
035800030114     C*
0359000301141   >C                   Else
036000030114     C* Se in Immissione nuovo record
036100030114     c                   exsr      NewD1
0362009402241-   C                   ENDIF
036300940224     C*
036400940117     C                   ENDSR
036500940207     C/EJECT
036600940207     C************************************************************
036700940510     C* RIEMPIMENTO VIDEATA  D1
036800940207     C************************************************************
036900030114     C     NEWD1         BEGSR
037000940207     C*
037100030114     c                   clear                   d1
037200111221     C                   eval      D1unbcli  = savUNBCLI
037300111221     C                   eval      D1rags    = savRAGS
037400100322     C*
037500030114     C                   ENDSR
037600030114     C************************************************************
037700030114     C* RIEMPIMENTO VIDEATA  D1
037800030114     C************************************************************
037900030114     C     RIED1         BEGSR
038000030114     C*
038100111221     C                   eval      D1rags    = savRAGS
038200111221     C                   eval      D1unbcli  = STSUNBCLI
038300111221     C                   eval      D1cod     = STSCODSTS
038400111221     C                   eval      D1cod1    = STSSUBSTS
038500111221     C                   eval      D1brt     = STSCODSTSB
038600111221     C                   eval      D1brt1    = STSSUBSTSB
038700111221     C                   eval      D1des     = STSDESCRI
038800100322     C*
038900111221     C                   Z-ADD     1             XX                3 0
039000111221     c                   clear                   cod06             6
039100111221     C                   eval      cod06 = d1brt + d1brt1
039200111221     C     Cod06         LOOKUP    COD_CE(XX)                             32
039300111221     C   32              MOVEL     DES_CE(XX)    d1des1
039400111221     C*
039500100121     c                   setoff                                       08
039600030204     C*
039700940207     C                   ENDSR
039800940309     C************************************************************
039900940309     C* GESTIONE F03 VIDEO D1
040000940309     C************************************************************
040100940309     C     F03D1         BEGSR
040200940309     C*
040300940309     C                   MOVE      *ON           $FINE
040400030113    >C                   MOVE      '1'           xtaret
040500940325     C*
040600940325     C                   ENDSR
040700940309     C************************************************************
040800940309     C* GESTIONE F12 VIDEO D1
040900940309     C************************************************************
041000940309     C     F12D1         BEGSR
041100940309     C*
041200940309     C                   MOVE      *ON           $FINE
041300100325    >C     xtaopz        ifeq      '01'
041400100325    >C                   MOVE      '2'           xtaret
041500100325     c                   end
041600940309     C*
041700940309     C                   ENDSR
041800940131     C************************************************************
041900940207     C* CONTROLLO VIDEATA
042000940131     C************************************************************
042100940127     C     CTRD1         BEGSR
042200940131     C*
042300940127     C                   SETOFF                                       99
042400030123      *
042500940127     C* non imposto pos. del cursore
042600940127     C                   Z-ADD     0             H1RIGA
042700940127     C                   Z-ADD     0             H1COLO
042800940621     C*
042900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0430000301141   >C     xtaopz        IFNE      '04'
043100971110     C*
043200971110     C* controllo codice vuoto
043300131011     C* con AMAZON si è iniziato ad utilizzare solo il sotto codice
043400131011     C*   quindi tolto il controllo sul codice principale
043500131011$017 C**** D1cod         Ifeq      *blank
043600131011     C****               SETON                                          5199
043700131011     C****               Else
043800090226     C*** se già presente
043900090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
044000090226    >C     xtaopz        IFEQ      '01'
044100090226    >C     xtaopz        oreq      '03'
044200100121     C                   exsr      Imposta_KEY
044300111221     c                   eval         stsUNBCLI =  d1UNBCLI
044400131011     C     Key_STS       setll     edSTS01L
044500131011     C     Key_STS       reade     edSTS01L
044600131011     c                   dow       not %EoF(edSTS01L)
044700131011     c                   if          STSCODSTSB =  d1brt  and
044800131011     c                               STSSUBSTSB =  d1brt1
044900131011     C                   SETON                                        5499
045000131011     C                   Leave
045100131011     C                   End
045200131011     C     Key_STS       reade     edSTS01L
045300131011     C                   End
045400090226     C                   ENDIF
045500100618     C*
045600131011     C**********         ENDIF
045700100121     C*
045800100121     C* controllo codice tabella
045900100121    >C     xtaopz        IFne      '01'
046000100121    >C     xtaopz        andNE     '03'
046100131011     C************       exsr      Imposta_KEY
046200131011     C**** key_STS       chain     edSTS01l
046300131011     C     xtaNREC       chain(N)  edSTS00F
046400131011     c                   if        not %Found(edSTS00f)
046500100121     C**** se sbagliato il codice
046600100121     C                   SETON                                        5599
046700090226     c                   end
046800100121     c                   endIF
046900100618      *
047000090226     C                   ENDIF
047100100121      *
047200100325      * -------------
047300111221      *   Controlli sui campi
047400100325      * -------------
047500111221      *  se non è stato immesso nulla
047600111221     c                   if        D1BRT = *blank
047700100618     c                   seton                                        6199
047800100325     C                   ENDIF
047900100325      *
048000111221     c                   if        D1DES = *blank
048100111221     c                   seton                                        6299
048200111221     C                   ENDIF
048300111221      *
048400100325      *    Altri:
048500111221     C                   Z-ADD     1             XX                3 0
048600111221     c                   clear                   cod06             6
048700111221     C                   eval      cod06 = d1brt + d1brt1
048800111221     C     Cod06         LOOKUP    COD_CE(XX)                             32
048900111221     C   32              MOVEL     DES_CE(XX)    d1des1
049000111221     C*
049100100325      *
049200100325      * -------------
049300940302     C     *IN99         IFEQ      *ON
049400940510     C     $GEST         ANDEQ     'D1'
049500940117     C                   MOVEA     *IN           WIN
049600940128     C                   MOVE      *ALL'0'       IN5098           49
049700940128     C                   MOVEA     IN5098        *IN(50)
049800940614     C                   WRITE     D1
049900940117     C                   MOVEA     WIN           *IN
050000940302     C                   ENDIF
050100940127     C*
050200940117     C                   ENDSR
050300030204     C/EJECT
050400030204     C************************************************************
050500030204    >C* AGGIORNAMENTO ANAGRAFICA
050600030204     C************************************************************
050700100325    >C     FLDAGG        BEGSR
050800030204     C*
050900030204     C*  campi chiave da scrivere e aggiornare
051000111221     C                   exsr      Imposta_KEY
051100100322     C*
051200100618     c                   exsr      RIEANA
051300100121     C*
051400030204     C                   ENDSR
051500940207     C/EJECT
051600940224     C************************************************************
051700940315    >C* AGGIORNAMENTO ANAGRAFICA
051800940224     C************************************************************
051900940315    >C     AGGANA        BEGSR
052000940207     C*
052100100408     c                   clear                   aggiorna_FLG      1
052200100408     C*
052300030204     C                   exsr      FLDAGG
052400030204     C*
052500940207     C                   SELECT
052600030204     C*
052700100323      *  Immette/Copia il record
052800030114    >C     xtaopz        WHENEQ    '01'
052900030114    >C     xtaopz        OREQ      '03'
053000030204     C*
053100100408     c                   eval      aggiorna_FLG = 'S'
053200100408     C*
053300090226    >C                   EXSR      RIEANA
053400111221     c                   write     edsts000                             59
053500940318     C* se rcd non scrivibile attivo errore
053600030114    >C   59              SETON                                          99
053700030204      *
053800100323      *  Modifica il record
053900030114    >C     xtaopz        WHENEQ    '02'
054000100408     c*
054100100408     c                   eval      aggiorna_FLG = 'S'
054200100408     c*
054300131011     C***********        exsr      Imposta_KEY
054400131011     C**** Key_STS       chain     edSTS01L
054500131011     C     xtaNREC       chain     edSTS00F
054600030204     C                   exsr      FLDAGG
054700090226    >C                   EXSR      RIEANA
054800111221     c                   update    edSTS000                             59
054900030114    >C   59              SETON                                          99
055000030204      *
055100100323      *  Cancella il record
055200030114    >C     xtaopz        WHENEQ    '04'
055300100408     c*
055400100408     c                   eval      aggiorna_FLG = 'S'
055500100408     c*
055600131011     C***********        exsr      Imposta_KEY
055700131011     C**** Key_STS       chain     edSTS01L
055800131011     C     xtaNREC       chain     edSTS00F
055900111221     c                   delete    edSTS000                             59
056000030114    >C   59              SETON                                          99
056100100323      *
056200940207     C                   ENDSL
056300100408     C*
056400940203     C                   ENDSR
056500940224     C************************************************************
056600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
056700940224     C************************************************************
056800090226     C     RIEANA        BEGSR
056900030114     C*
057000111221     C                   eval       STSCODSTS   = D1cod
057100111221     C                   eval       STSSUBSTS   = D1cod1
057200111221     C                   eval       STSCODSTSB  = D1brt
057300111221     C                   eval       STSSUBSTSB  = D1brt1
057400100618     C*
057500111221     C                   MOVEl     D1des         stsDESCRI
057600111221     C                   MOVEl     D1unbCLI      stsUNBCLI
057700111221     C*
057800111221     C                   Z-ADD     1             XX                3 0
057900111221     c                   clear                   cod06             6
058000111221     C                   eval      cod06 = d1brt + d1brt1
058100111221     C     Cod06         LOOKUP    COD_CE(XX)                             32
058200111221     C   32              MOVEL     DES_CE(XX)    d1des1
058300111221     C*
058400100121     C*
058500100324     C                   ENDSR
058600940224     C************************************************************
058700940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
058800940224     C************************************************************
058900940224     C     GESAGG        BEGSR
059000940224     C*
059100940224     C* segnalo al pgm chiamante l'avvenuta conferma
059200030114    >C                   MOVE      *ON           xtaopr
059300940224     C* segnalo al pgm chiamante l'aver premuto l'F6
059400030114    >C                   MOVE      '0'           xtaret
059500940224     C*
0596009402241    C                   SELECT
059700940224     C* nel caso di immissione, ripristino la videata iniziale
059800030114    >C     xtaopz        WHENEQ    '01'
059900940224     C                   MOVE      *ON           $INZD1
060000030114     C                   MOVE      *ON           $FINE
060100940224     C* altrimenti ritorno al pgm chiamante
060200940224     C                   OTHER
060300940224     C                   MOVE      *ON           $FINE
0604009402241-   C                   ENDSL
060500940224     C*
060600940224     C                   ENDSR
060700940131     C************************************************************
060800100121     C*
060900100121     C************************************************************
061000100121     C     Imposta_KEY   BEGSR
061100100121     C*
061200111221     c                   eval         stsUNBCLI = savUNBCLI
061300111221     c                   eval         stsCODSTS =  d1cod
061400111221     c                   eval         stsSUBSTS =  d1cod1
061500100121     C*
061600100121     C                   ENDSR
