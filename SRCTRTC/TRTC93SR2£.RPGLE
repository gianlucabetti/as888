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
002600131011     F****edSTS01L  UF A E           K DISK
002700131011     FedSTS00F  UF A E             DISK
002800131011     F                                     INFDS(STSDS)
002900131011     FedSTS01L  iF   E           K DISK    rename(edsts000:edsts001)
003000111221     Fedtab01L  iF   E           K DISK
003100940128     D*----------------------------------------------------*
003200940127     D* Passaggio Parametri
003300940127     D KPJBA         E DS
003400100409     D dUte01        E DS
003500111221     D EDidsCE       E DS
003600111221     D COD_CE          s              6    DIM(100)
003700111221     D DES_CE          s             35    DIM(100)
003800100618     D*-------------
003900111221     d  savRAGS        s                   Like(xtaRAGS)
004000111221     d  savUNBCLI      s                   Like(stsUNBCLI)
004100111221     d  savCODSTS      s                   Like(stsCODSTS)
004200111221     d  savSUBSTS      s                   Like(stsSUBSTS)
004300111221     D*-------------
004400100618     D UTEDSE0F      E DS
004500100618     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
004600100618     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
004700100618      * Ds scomposzione tipo capoconti
004800100618     D TCUDS           DS
004900100618     D  F34                    3      4
005000100618     D  F56                    5      6
005100100618      *
005200100325     D*-------------
005300111221     D* Parametri in ricezione
005400111221     D  TABDS          DS
005500111221     D  XTAOPZ                 1      2
005600111221     D  XTARET                 3      3
005700111221     D  XTAOPR                 4      4
005800111221     D  XTAERR                 5      5
005900111221     D  XTAKEY1                6      8
006000111221     D  XTAKEY2                9     11
006100111221     D  XTACOD1               12     14
006200111221     D  XTACOD2               15     17
006300111221     D  XTAUNBcli             18     52
006400111221     D  XTARAGs               53     84
006500131011     D  XTANREC               85     93  0
006600131011     D*-------------
006700131011     d stsds           ds
006800131011     d  sts_nrr              397    400b 0
006900111221     D*-------------
007000030204     D  S_TASTO        S                   like($Tasto)
007100940201     D DSFMT           DS
007200940506     D  $TASTO               369    369
007300940201     D  NRG                  370    370
007400940201     D  NCL                  371    371
007500940201     D*-------------
007600940201     D* posizione cursore
007700940201     D CURSOR          DS
007800940223     D  RIRI                   1      2B 0 INZ
007900940201     D  R$$                    2      2
008000940223     D  COCO                   3      4B 0 INZ
008100940201     D  C$$                    4      4
008200940207     D*-------------
008300940207     D* Reperimento nome PGM
008400940207     D STATUS         SDS           333
008500940207     D  DSPGM            *PROC
008600940225     D*-------------
008700940225     D* COSTANTI
008800940225     D*-------------
008900940506     D* Tasti di funzione
009000940506     D F01             C                   CONST(X'31')
009100940506     D F02             C                   CONST(X'32')
009200940506     D F03             C                   CONST(X'33')
009300940506     D F04             C                   CONST(X'34')
009400940506     D F05             C                   CONST(X'35')
009500940506     D F06             C                   CONST(X'36')
009600940506     D F07             C                   CONST(X'37')
009700940506     D F08             C                   CONST(X'38')
009800940506     D F09             C                   CONST(X'39')
009900940506     D F10             C                   CONST(X'3A')
010000940506     D F11             C                   CONST(X'3B')
010100940506     D F12             C                   CONST(X'3C')
010200940506     D F13             C                   CONST(X'B1')
010300940506     D F14             C                   CONST(X'B2')
010400940506     D F15             C                   CONST(X'B3')
010500940506     D F16             C                   CONST(X'B4')
010600940506     D F17             C                   CONST(X'B5')
010700940506     D F18             C                   CONST(X'B6')
010800940506     D F19             C                   CONST(X'B7')
010900940506     D F20             C                   CONST(X'B8')
011000940506     D F21             C                   CONST(X'B9')
011100940506     D F22             C                   CONST(X'BA')
011200940506     D F23             C                   CONST(X'BB')
011300940506     D F24             C                   CONST(X'BC')
011400940506     D ENTER           C                   CONST(X'F1')
011500940506     D ROLDWN          C                   CONST(X'F4')
011600940506     D ROLLUP          C                   CONST(X'F5')
011700940506     D*-------------
011800941108     D DATA            C                   CONST('0001-01-01')
011900030114     D Annull          C                   CONST('  ANNULLATO  ')
012000100323     D OPz_Immissione  C                   CONST('IMMISSIONE')
012100100323     D OPz_Variazione  C                   CONST('VARIAZIONE')
012200100323     D OPz_Annulla     C                   CONST(' ANNULLA  ')
012300100323     D OPz_Visualizza  C                   CONST('VISUALIZZA')
012400100323     D OPz_Riattiva    C                   CONST(' RIATTIVA ')
012500940127     C*----------------------------------------------------*
012600940127     C*                MAIN LINE PROGRAM
012700940127     C*----------------------------------------------------*
012800100121     c                   exsr      INIZIO
012900940223     C*
013000940223     C     $FINE         DOWEQ     *OFF
013100940202     C     $GEST         CASEQ     'D1'          GESD1
013200940117     C                   END
013300940117     C                   END
013400940325     C* fine programma
013500030113     c                   movel     tabds         kpjbu
013600030113     C                   Seton                                        LR
013700940131     C************************************************************
013800100121     C* OPERAZIONI INIZIALI
013900100121     C************************************************************
014000100121     C     INIZIO        BEGSR
014100100121     C*
014200100121     C* Reperimento parametri
014300100121     C     *ENTRY        PLIST
014400100121     C                   PARM                    KPJBA
014500111221     C*
014600111221     C                   eval        TABDS = KPJBU
014700111221     c                   eval      savUNBCLI = xtaUNBCLI
014800111221     c                   eval      savRAGS   = xtaRAGS
014900100121      *
015000100121     c                   setoff                                       08
015100100121     C*
015200100121     C* Variabili per gestione videate
015300100121     C                   MOVE      'D1'          $GEST
015400100121     C                   MOVE      *OFF          $FINE
015500100121     C                   MOVE      *ON           $INZD1
015600100121     C                   MOVE      *BLANK        $LASTG
015700100121     C                   MOVE      *BLANK        $LASTV
015800100121     C                   Z-ADD     0             $ULKD1            3 0
015900100121     C*
016000100121     C* Variabili appoggio
016100100121     C                   Z-ADD     0             CURR
016200100121     C                   Z-ADD     0             CURC
016300100618      *
016400100618     C* RECUPERO DATI DELL'UTENTE
016500100618     C                   Z-ADD     1             CODUT             1 0
016600100618     C                   CALL      'XPARUT'
016700100618     C                   PARM                    UTEDSE0F
016800100618     C                   MOVEL     RAGUT         RSUT             20
016900100618     C*
017000100618     C* RICERCA CAPOCONTI
017100100618     C                   DO        50            X
017200100618     C                   MOVE      TCU(X)        TCUDS
017300100618     C     F56           IFEQ      'CG'
017400100618     C     F34           ANDEQ     '01'
017500100618     C                   Z-ADD     KCU(X)        KCI               4 0
017600100618     C                   END
017700100618     C                   END
017800100121     C*
017900100121     C* Valorizzazione campi univoci testate
018000100121     C                   CLEAR                   T1
018100100121     C                   MOVEL     KNSIF         NOMSIF
018200100121     C******             MOVEL     XSCDSI        NOMDIT
018300100121     C                   MOVEL     DSPGM         NOMPGM
018400100121     C*
018500111221     C* Carica tabella CE per decodificare i significati
018600111221      *------------
018700111221     C* Caricamento Tabella Partner esteri
018800111221     C                   Z-ADD     0             X                 3 0
018900111221     C                   MOVEL     'CE'          TABCOD
019000111221     C     TABCOD        CHAIN     EDTAB01L                           30
019100111221     C     *IN30         DOWEQ     '0'
019200111221     C     TABFLG        IFEQ      *BLANKS
019300111221     C                   ADD       1             X
019400111221     C                   MOVEL     TABKEY        COD_CE(X)
019500111221     C                   MOVEL     TABUNI        EDIdsCE
019600111221     C                   MOVEL     §ceDEC        DES_CE(X)
019700111221     C                   END
019800111221     C     TABCOD        READE     EDTAB01L                               30
019900111221     C                   END
020000111221      *------------
020100100121     C* Inizializzazione/Decodifica prima videata
020200100121     C                   EXSR      INZD1
020300100121     C                   MOVE      *OFF          $INZD1
020400100121     C*
020500100121     C                   ENDSR
020600100121     C************************************************************
020700100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
020800100121     C************************************************************
020900100121     C     DEFCAM        BEGSR
021000100121     C*
021100100121     C* klist
021200111221     c     key_STS       Klist
021300111221     c                   kfld                    stsUNBCLI
021400111221     c                   kfld                    stsCODSTS
021500111221     c                   kfld                    stsSUBSTS
021600111221      *
021700100121     C* Variabili per gestione videate
021800100121     C                   MOVE      *BLANK        $GEST             2
021900100121     C                   MOVE      *BLANK        $FINE             1
022000100121     C                   MOVE      *BLANK        $INZD1            1
022100100121     C                   MOVE      *BLANK        $LASTG            2
022200100121     C                   MOVE      *BLANK        $LASTV            2
022300100121     C*
022400100121     C* Variabili appoggio
022500100121     C                   Z-ADD     0             CURR              2 0
022600100121     C                   Z-ADD     0             CURC              2 0
022700100121     C                   MOVE      *ZEROS        WIN              99
022800100121     C* Indici
022900100121     C                   Z-ADD     0             X                 3 0
023000100121     C                   Z-ADD     0             Y                 3 0
023100100121     C*
023200100121     C                   ENDSR
023300100121     C************************************************************
023400940510     C* GESTIONE VIDEO RECORD D1
023500940131     C************************************************************
023600940127     C     GESD1         BEGSR
023700030113      *
023800940225     C* inizializzazione videata
023900940223     C     $INZD1        IFEQ      *ON
024000940127     C                   EXSR      INZD1
024100940223     C                   MOVE      *OFF          $INZD1
024200940117     C                   END
024300030113      *
024400940225     C* emissione piede videata se proveniente da altra
024500940225     C* salvataggio valore formato di provenienza
024600940207     C     $LASTV        IFNE      'D1'
024700940614     C                   WRITE     T1
024800940202     C                   MOVE      $LASTV        $LASTG
024900940202     C                   MOVE      'D1'          $LASTV
025000940202     C                   END
025100940131     C*
025200940203     C                   SELECT
025300100323     C* Inserimento e Copia
025400030114    >C     xtaopz        WHENEQ    '01'
025500100121    >C     xtaopz        OREQ      '03'
025600100323     c                   movel     Opz_ImmissioneD1Fun
025700100323     C* Visualizzazione
025800030114    >C     xtaopz        WHENEQ    '05'
025900100323     c                   movel     Opz_VisualizzaD1Fun
026000030114     C                   SETON                                        020306
026100100323     C* Annulla
026200030114    >C     xtaopz        WHENEQ    '04'
026300100323     c                   movel     Opz_Annulla   D1Fun
026400030114     C                   SETON                                        0203
026500030114     C                   SETOFF                                         06
026600100323     C* Ripristina
026700100323    >C     xtaopz        WHENEQ    '08'
026800100323     c                   movel     OPz_Riattiva  D1Fun
026900100323     C                   SETON                                        02  03
027000100323     C                   SETON                                        06  18
027100940620     C* Manutenzione
027200030113    >C     xtaopz        WHENEQ    '02'
027300100323     c                   movel     Opz_VariazioneD1Fun
027400030114     C                   SETON                                        02
027500030114     C                   SETOFF                                       0306
027600940620     C*
027700940620     C                   OTHER
027800090226     C*
027900030114     C                   SETOFF                                       020306
028000940203     C                   ENDSL
028100030114     C*
028200030114     c                   SETOFF                                         99
028300940620     C* Immissione/Modifica/Copia
028400030113     C*               *----------------*
028500940620     C                   EXFMT     D1
028600030113     C*               *----------------*
028700030114     C*
028800030114     C     *IN99         IFEQ      *OFF
028900030114     C*
0290000301141    C                   SELECT
029100030114     C* F3=Fine
029200030114     C     $TASTO        WHENEQ    F03
029300030114     C                   EXSR      F03D1
029400030114     C* F12=Ritorno
029500030114     C     $TASTO        WHENEQ    F12
029600030114     C                   EXSR      F12D1
029700030114     C*
0298000301141O   C                   OTHER
029900030114     C*
030000030114     C                   EXSR      CTRD1
030100030113     C*
030200940616     C     *IN99         IFEQ      *OFF
030300940616     C* F6
0304000302052    C     $TASTO        ifeq      F06
0305001003232    C     $TASTO        oreq      F18
0306000301152    C     $TASTO        oreq      F23
030700940224     C* eseguo aggiornamento
030800940315    >C                   EXSR      AGGANA
030900940224     C* eseguo operazioni del dopo-aggiornamento
031000030114     C  N99              EXSR      GESAGG
031100030205     c                   end
031200030114     C*
031300030114     C                   ENDIF
031400030114     C*
0315000301142-   C                   ENDSL
031600030114     C*
0317000301142-   C                   ENDIF
031800030114     C*
031900940117     C                   ENDSR
032000940117      ************************************************************
032100940207      * INIZIALIZZAZIONE VIDEATA DATI
032200940117      ************************************************************
032300940127     C     INZD1         BEGSR
032400940131     C*
032500940615     C                   CLEAR                   D1
032600100121     C*
032700111221     C                   move      xtaKEY1       d1cod
032800111221     C                   move      xtaKEy2       d1cod1
032900131011     C                   z-add     xtaNREC       h1NREC
033000100618      *
033100940224     C* valorizzo la chiave univoca per reperire i dati
033200940224     C* se non immissione
0333000301141   >C     xtaopz        IFNE      '01'
033400940315    >C*-------------------------------------------------------------
033500940315    >C* impostare i campi chiave ricevuti
033600940315    >C*-------------------------------------------------------------
033700131011     C*************      exsr      Imposta_KEY
033800131011     C*****key_STS       chain(N)  edSTS01L
033900131011     C     h1Nrec        chain(N)  edSTS00F
034000131011     C                   IF        %Found(edSTS00f)
034100940224     C* riempio la videata
034200940224     C                   EXSR      RIED1
034300030113      *
034400940224     C* richiamo routine dei ctrl per decodificarla
034500940614    >C                   EXSR      CTRD1
034600030113      *
034700940224     C                   MOVE      *ALL'0'       IN5098
034800940224     C                   MOVEA     IN5098        *IN(50)
034900030113      *
035000940224     C* definizione indicatori su video
035100940224     C                   SETOFF                                       02
0352009402243    C                   SELECT
035300030114    >C     xtaopz        WHENEQ    '02'
035400940224     C                   SETON                                        02
035500030114    >C     xtaopz        WHENEQ    '05'
0356009402243-   C                   ENDSL
035700940224     C*
0358009402242-   C                   ENDIF
035900030114     C*
0360000301141   >C                   Else
036100030114     C* Se in Immissione nuovo record
036200030114     c                   exsr      NewD1
0363009402241-   C                   ENDIF
036400940224     C*
036500940117     C                   ENDSR
036600940207     C/EJECT
036700940207     C************************************************************
036800940510     C* RIEMPIMENTO VIDEATA  D1
036900940207     C************************************************************
037000030114     C     NEWD1         BEGSR
037100940207     C*
037200030114     c                   clear                   d1
037300111221     C                   eval      D1unbcli  = savUNBCLI
037400111221     C                   eval      D1rags    = savRAGS
037500100322     C*
037600030114     C                   ENDSR
037700030114     C************************************************************
037800030114     C* RIEMPIMENTO VIDEATA  D1
037900030114     C************************************************************
038000030114     C     RIED1         BEGSR
038100030114     C*
038200111221     C                   eval      D1rags    = savRAGS
038300111221     C                   eval      D1unbcli  = STSUNBCLI
038400111221     C                   eval      D1cod     = STSCODSTS
038500111221     C                   eval      D1cod1    = STSSUBSTS
038600111221     C                   eval      D1brt     = STSCODSTSB
038700111221     C                   eval      D1brt1    = STSSUBSTSB
038800111221     C                   eval      D1des     = STSDESCRI
038900100322     C*
039000111221     C                   Z-ADD     1             XX                3 0
039100111221     c                   clear                   cod06             6
039200111221     C                   eval      cod06 = d1brt + d1brt1
039300111221     C     Cod06         LOOKUP    COD_CE(XX)                             32
039400111221     C   32              MOVEL     DES_CE(XX)    d1des1
039500111221     C*
039600100121     c                   setoff                                       08
039700030204     C*
039800940207     C                   ENDSR
039900940309     C************************************************************
040000940309     C* GESTIONE F03 VIDEO D1
040100940309     C************************************************************
040200940309     C     F03D1         BEGSR
040300940309     C*
040400940309     C                   MOVE      *ON           $FINE
040500030113    >C                   MOVE      '1'           xtaret
040600940325     C*
040700940325     C                   ENDSR
040800940309     C************************************************************
040900940309     C* GESTIONE F12 VIDEO D1
041000940309     C************************************************************
041100940309     C     F12D1         BEGSR
041200940309     C*
041300940309     C                   MOVE      *ON           $FINE
041400100325    >C     xtaopz        ifeq      '01'
041500100325    >C                   MOVE      '2'           xtaret
041600100325     c                   end
041700940309     C*
041800940309     C                   ENDSR
041900940131     C************************************************************
042000940207     C* CONTROLLO VIDEATA
042100940131     C************************************************************
042200940127     C     CTRD1         BEGSR
042300940131     C*
042400940127     C                   SETOFF                                       99
042500030123      *
042600940127     C* non imposto pos. del cursore
042700940127     C                   Z-ADD     0             H1RIGA
042800940127     C                   Z-ADD     0             H1COLO
042900940621     C*
043000940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0431000301141   >C     xtaopz        IFNE      '04'
043200971110     C*
043300971110     C* controllo codice vuoto
043400131011     C* con AMAZON si è iniziato ad utilizzare solo il sotto codice
043500131011     C*   quindi tolto il controllo sul codice principale
043600131011$017 C**** D1cod         Ifeq      *blank
043700131011     C****               SETON                                          5199
043800131011     C****               Else
043900090226     C*** se già presente
044000090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
044100090226    >C     xtaopz        IFEQ      '01'
044200090226    >C     xtaopz        oreq      '03'
044300100121     C                   exsr      Imposta_KEY
044400111221     c                   eval         stsUNBCLI =  d1UNBCLI
044500131011     C     Key_STS       setll     edSTS01L
044600131011     C     Key_STS       reade     edSTS01L
044700131011     c                   dow       not %EoF(edSTS01L)
044800131011     c                   if          STSCODSTSB =  d1brt  and
044900131011     c                               STSSUBSTSB =  d1brt1
045000131011     C                   SETON                                        5499
045100131011     C                   Leave
045200131011     C                   End
045300131011     C     Key_STS       reade     edSTS01L
045400131011     C                   End
045500090226     C                   ENDIF
045600100618     C*
045700131011     C**********         ENDIF
045800100121     C*
045900100121     C* controllo codice tabella
046000100121    >C     xtaopz        IFne      '01'
046100100121    >C     xtaopz        andNE     '03'
046200131011     C************       exsr      Imposta_KEY
046300131011     C**** key_STS       chain     edSTS01l
046400131011     C     xtaNREC       chain(N)  edSTS00F
046500131011     c                   if        not %Found(edSTS00f)
046600100121     C**** se sbagliato il codice
046700100121     C                   SETON                                        5599
046800090226     c                   end
046900100121     c                   endIF
047000100618      *
047100090226     C                   ENDIF
047200100121      *
047300100325      * -------------
047400111221      *   Controlli sui campi
047500100325      * -------------
047600111221      *  se non è stato immesso nulla
047700111221     c                   if        D1BRT = *blank
047800100618     c                   seton                                        6199
047900100325     C                   ENDIF
048000100325      *
048100111221     c                   if        D1DES = *blank
048200111221     c                   seton                                        6299
048300111221     C                   ENDIF
048400111221      *
048500100325      *    Altri:
048600111221     C                   Z-ADD     1             XX                3 0
048700111221     c                   clear                   cod06             6
048800111221     C                   eval      cod06 = d1brt + d1brt1
048900111221     C     Cod06         LOOKUP    COD_CE(XX)                             32
049000111221     C   32              MOVEL     DES_CE(XX)    d1des1
049100111221     C*
049200100325      *
049300100325      * -------------
049400940302     C     *IN99         IFEQ      *ON
049500940510     C     $GEST         ANDEQ     'D1'
049600940117     C                   MOVEA     *IN           WIN
049700940128     C                   MOVE      *ALL'0'       IN5098           49
049800940128     C                   MOVEA     IN5098        *IN(50)
049900940614     C                   WRITE     D1
050000940117     C                   MOVEA     WIN           *IN
050100940302     C                   ENDIF
050200940127     C*
050300940117     C                   ENDSR
050400030204     C/EJECT
050500030204     C************************************************************
050600030204    >C* AGGIORNAMENTO ANAGRAFICA
050700030204     C************************************************************
050800100325    >C     FLDAGG        BEGSR
050900030204     C*
051000030204     C*  campi chiave da scrivere e aggiornare
051100111221     C                   exsr      Imposta_KEY
051200100322     C*
051300100618     c                   exsr      RIEANA
051400100121     C*
051500030204     C                   ENDSR
051600940207     C/EJECT
051700940224     C************************************************************
051800940315    >C* AGGIORNAMENTO ANAGRAFICA
051900940224     C************************************************************
052000940315    >C     AGGANA        BEGSR
052100940207     C*
052200100408     c                   clear                   aggiorna_FLG      1
052300100408     C*
052400030204     C                   exsr      FLDAGG
052500030204     C*
052600940207     C                   SELECT
052700030204     C*
052800100323      *  Immette/Copia il record
052900030114    >C     xtaopz        WHENEQ    '01'
053000030114    >C     xtaopz        OREQ      '03'
053100030204     C*
053200100408     c                   eval      aggiorna_FLG = 'S'
053300100408     C*
053400090226    >C                   EXSR      RIEANA
053500111221     c                   write     edsts000                             59
053600940318     C* se rcd non scrivibile attivo errore
053700030114    >C   59              SETON                                          99
053800030204      *
053900100323      *  Modifica il record
054000030114    >C     xtaopz        WHENEQ    '02'
054100100408     c*
054200100408     c                   eval      aggiorna_FLG = 'S'
054300100408     c*
054400131011     C***********        exsr      Imposta_KEY
054500131011     C**** Key_STS       chain     edSTS01L
054600131011     C     xtaNREC       chain     edSTS00F
054700030204     C                   exsr      FLDAGG
054800090226    >C                   EXSR      RIEANA
054900111221     c                   update    edSTS000                             59
055000030114    >C   59              SETON                                          99
055100030204      *
055200100323      *  Cancella il record
055300030114    >C     xtaopz        WHENEQ    '04'
055400100408     c*
055500100408     c                   eval      aggiorna_FLG = 'S'
055600100408     c*
055700131011     C***********        exsr      Imposta_KEY
055800131011     C**** Key_STS       chain     edSTS01L
055900131011     C     xtaNREC       chain     edSTS00F
056000111221     c                   delete    edSTS000                             59
056100030114    >C   59              SETON                                          99
056200100323      *
056300940207     C                   ENDSL
056400100408     C*
056500940203     C                   ENDSR
056600940224     C************************************************************
056700940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
056800940224     C************************************************************
056900090226     C     RIEANA        BEGSR
057000030114     C*
057100111221     C                   eval       STSCODSTS   = D1cod
057200111221     C                   eval       STSSUBSTS   = D1cod1
057300111221     C                   eval       STSCODSTSB  = D1brt
057400111221     C                   eval       STSSUBSTSB  = D1brt1
057500100618     C*
057600111221     C                   MOVEl     D1des         stsDESCRI
057700111221     C                   MOVEl     D1unbCLI      stsUNBCLI
057800111221     C*
057900111221     C                   Z-ADD     1             XX                3 0
058000111221     c                   clear                   cod06             6
058100111221     C                   eval      cod06 = d1brt + d1brt1
058200111221     C     Cod06         LOOKUP    COD_CE(XX)                             32
058300111221     C   32              MOVEL     DES_CE(XX)    d1des1
058400111221     C*
058500100121     C*
058600100324     C                   ENDSR
058700940224     C************************************************************
058800940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
058900940224     C************************************************************
059000940224     C     GESAGG        BEGSR
059100940224     C*
059200940224     C* segnalo al pgm chiamante l'avvenuta conferma
059300030114    >C                   MOVE      *ON           xtaopr
059400940224     C* segnalo al pgm chiamante l'aver premuto l'F6
059500030114    >C                   MOVE      '0'           xtaret
059600940224     C*
0597009402241    C                   SELECT
059800940224     C* nel caso di immissione, ripristino la videata iniziale
059900030114    >C     xtaopz        WHENEQ    '01'
060000940224     C                   MOVE      *ON           $INZD1
060100030114     C                   MOVE      *ON           $FINE
060200940224     C* altrimenti ritorno al pgm chiamante
060300940224     C                   OTHER
060400940224     C                   MOVE      *ON           $FINE
0605009402241-   C                   ENDSL
060600940224     C*
060700940224     C                   ENDSR
060800940131     C************************************************************
060900100121     C*
061000100121     C************************************************************
061100100121     C     Imposta_KEY   BEGSR
061200100121     C*
061300111221     c                   eval         stsUNBCLI = savUNBCLI
061400111221     c                   eval         stsCODSTS =  d1cod
061500111221     c                   eval         stsSUBSTS =  d1cod1
061600100121     C*
061700100121     C                   ENDSR
