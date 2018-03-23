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
002500100618     FCNACO00F  IF   E           K DISK
002600100622$001 FTNTBQEMD2 CF   E             WORKSTN
002700940201     F                                     INFDS(DSFMT)
002800100121     Ftntbe01L  UF A E           K DISK
002900940128     D*----------------------------------------------------*
003000940127     D* Passaggio Parametri
003100940127     D KPJBA         E DS
003200100622     D dQEM          E DS
003300100409     D dUte01        E DS
003400100618     D*-------------
003500100618     D UTEDSE0F      E DS
003600100618     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
003700100618     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
003800100618      * Ds scomposzione tipo capoconti
003900100618     D TCUDS           DS
004000100618     D  F34                    3      4
004100100618     D  F56                    5      6
004200100618      *
004300100618      *-------------
004400100618$003 D KKCC            S                   Like(acokcc)
004500100618$003 D KKut            S                   Like(acokut)
004600100618$003 D KKSC            S                   Like(acoksc)
004700100618     D*-------------
004800030113     D* Parametri in ricezione
004900030113     D  TABDS          DS
005000030113     D  XTAOPZ                 1      2
005100030113     D  XTARET                 3      3
005200030113     D  XTAOPR                 4      4
005300030113     D  XTAERR                 5      5
005400100409     D  XTAKEY1                6     20
005500100409     D  XTAKEY2               21     33
005600100409     D  XTAcod                31     33
005700100325     D*-------------
005800100325     D DSP_dsCAMPI     DS
005900100618     D  dsp_SKcampi            1     38    DIM(38)
006000100325      *
006100100618     D  D1TRS                  1      1
006200100618     D  D1DET                  2      2
006300100618     D  D1ECC                  3      3
006400100618     D  D1RIF                  4     38
006500100325      *
006600940201     D*-------------
006700030204     D  S_TASTO        S                   like($Tasto)
006800940201     D DSFMT           DS
006900940506     D  $TASTO               369    369
007000940201     D  NRG                  370    370
007100940201     D  NCL                  371    371
007200940201     D*-------------
007300940201     D* posizione cursore
007400940201     D CURSOR          DS
007500940223     D  RIRI                   1      2B 0 INZ
007600940201     D  R$$                    2      2
007700940223     D  COCO                   3      4B 0 INZ
007800940201     D  C$$                    4      4
007900940207     D*-------------
008000940207     D* Reperimento nome PGM
008100940207     D STATUS         SDS           333
008200940207     D  DSPGM            *PROC
008300940225     D*-------------
008400940225     D* COSTANTI
008500940225     D*-------------
008600940506     D* Tasti di funzione
008700940506     D F01             C                   CONST(X'31')
008800940506     D F02             C                   CONST(X'32')
008900940506     D F03             C                   CONST(X'33')
009000940506     D F04             C                   CONST(X'34')
009100940506     D F05             C                   CONST(X'35')
009200940506     D F06             C                   CONST(X'36')
009300940506     D F07             C                   CONST(X'37')
009400940506     D F08             C                   CONST(X'38')
009500940506     D F09             C                   CONST(X'39')
009600940506     D F10             C                   CONST(X'3A')
009700940506     D F11             C                   CONST(X'3B')
009800940506     D F12             C                   CONST(X'3C')
009900940506     D F13             C                   CONST(X'B1')
010000940506     D F14             C                   CONST(X'B2')
010100940506     D F15             C                   CONST(X'B3')
010200940506     D F16             C                   CONST(X'B4')
010300940506     D F17             C                   CONST(X'B5')
010400940506     D F18             C                   CONST(X'B6')
010500940506     D F19             C                   CONST(X'B7')
010600940506     D F20             C                   CONST(X'B8')
010700940506     D F21             C                   CONST(X'B9')
010800940506     D F22             C                   CONST(X'BA')
010900940506     D F23             C                   CONST(X'BB')
011000940506     D F24             C                   CONST(X'BC')
011100940506     D ENTER           C                   CONST(X'F1')
011200940506     D ROLDWN          C                   CONST(X'F4')
011300940506     D ROLLUP          C                   CONST(X'F5')
011400940506     D*-------------
011500941108     D DATA            C                   CONST('0001-01-01')
011600030114     D Annull          C                   CONST('  ANNULLATO  ')
011700100323     D OPz_Immissione  C                   CONST('IMMISSIONE')
011800100323     D OPz_Variazione  C                   CONST('VARIAZIONE')
011900100323     D OPz_Annulla     C                   CONST(' ANNULLA  ')
012000100323     D OPz_Visualizza  C                   CONST('VISUALIZZA')
012100100323     D OPz_Riattiva    C                   CONST(' RIATTIVA ')
012200940127     C*----------------------------------------------------*
012300940127     C*                MAIN LINE PROGRAM
012400940127     C*----------------------------------------------------*
012500100121     c                   exsr      INIZIO
012600940223     C*
012700940223     C     $FINE         DOWEQ     *OFF
012800940202     C     $GEST         CASEQ     'D1'          GESD1
012900940117     C                   END
013000940117     C                   END
013100940325     C* fine programma
013200030113     c                   movel     tabds         kpjbu
013300030113     C                   Seton                                        LR
013400940131     C************************************************************
013500100121     C* OPERAZIONI INIZIALI
013600100121     C************************************************************
013700100121     C     INIZIO        BEGSR
013800100121     C*
013900100121     C* Reperimento parametri
014000100121     C     *ENTRY        PLIST
014100100121     C                   PARM                    KPJBA
014200100121      *
014300100121     C                   movel     kpjbu         tabds
014400100121     c                   setoff                                       08
014500100121     C*
014600100121     C* Variabili per gestione videate
014700100121     C                   MOVE      'D1'          $GEST
014800100121     C                   MOVE      *OFF          $FINE
014900100121     C                   MOVE      *ON           $INZD1
015000100121     C                   MOVE      *BLANK        $LASTG
015100100121     C                   MOVE      *BLANK        $LASTV
015200100121     C                   Z-ADD     0             $ULKD1            3 0
015300100622     C                   MOVEL     'QEM'         tbe_COD           3
015400100622     C                   CLEAR                   dQEM
015500100121     C*
015600100121     C* Variabili appoggio
015700100121     C                   Z-ADD     0             CURR
015800100121     C                   Z-ADD     0             CURC
015900100618      *
016000100618     C* RECUPERO DATI DELL'UTENTE
016100100618     C                   Z-ADD     1             CODUT             1 0
016200100618     C                   CALL      'XPARUT'
016300100618     C                   PARM                    UTEDSE0F
016400100618     C                   MOVEL     RAGUT         RSUT             20
016500100618     C*
016600100618     C* RICERCA CAPOCONTI
016700100618     C                   DO        50            X
016800100618     C                   MOVE      TCU(X)        TCUDS
016900100618     C     F56           IFEQ      'CG'
017000100618     C     F34           ANDEQ     '01'
017100100618     C                   Z-ADD     KCU(X)        KCI               4 0
017200100618     C                   END
017300100618     C                   END
017400100618     C*
017500100121     C*
017600100121     C* Valorizzazione campi univoci testate
017700100121     C                   CLEAR                   T1
017800100121     C                   MOVEL     KNSIF         NOMSIF
017900100121     C******             MOVEL     XSCDSI        NOMDIT
018000100121     C                   MOVEL     DSPGM         NOMPGM
018100100121     C*
018200100121     C* Inizializzazione/Decodifica prima videata
018300100121     C                   EXSR      INZD1
018400100121     C                   MOVE      *OFF          $INZD1
018500100121     C*
018600100121     C                   ENDSR
018700100121     C************************************************************
018800100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
018900100121     C************************************************************
019000100121     C     DEFCAM        BEGSR
019100100121     C*
019200100121     C* klist
019300100618      *
019400100618     C     KACO          KLIST
019500100618     C                   KFLD                    KKUT
019600100618     C                   KFLD                    KKCC
019700100618     C                   KFLD                    KKSC
019800100618      *
019900100121     C     k_tbe01       kLIST
020000100121     C                   kfld                    tbe_cod
020100100121     C                   kfld                    tbe_key1         15
020200100121     C                   kfld                    tbe_key2         15
020300100409      *
020400100409     C     k_tbe011      kLIST
020500100409     C                   kfld                    tbe_cod
020600100409     C                   kfld                    xtakey1
020700100121     C*
020800100121     C* Variabili per gestione videate
020900100121     C*
021000100121     C                   MOVE      *BLANK        $GEST             2
021100100121     C                   MOVE      *BLANK        $FINE             1
021200100121     C                   MOVE      *BLANK        $INZD1            1
021300100121     C                   MOVE      *BLANK        $LASTG            2
021400100121     C                   MOVE      *BLANK        $LASTV            2
021500100121     C*
021600100121     C* Variabili appoggio
021700100121     C                   Z-ADD     0             CURR              2 0
021800100121     C                   Z-ADD     0             CURC              2 0
021900100121     C                   MOVE      *ZEROS        WIN              99
022000100121     C                   MOVE      *BLANK        WTBERR            1
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
032000100618     C                   move      *zeros        a1COD             7
032100100618     C                   eval      a1cod = %Subst(xtaKEY1:1:7)
032200100618     C                   move      a1COD         d1cod
032300100618      *
032400100618      * Decodifico partner
032500100618     C                   Z-ADD     1             KKUT
032600100618     C                   Z-ADD     KCI           KKCC
032700100618     C                   MOVE      *ZEROS        KKSC
032800100618     C                   MOVE      d1COD         KKSC
032900100618     C     KACO          CHAIN     CNACO00F                           21
033000100618     c                   if        %Found(CNACO00F)
033100100618     C                   eval      d1DES  = acoRAG
033200100618     c                   end
033300100121     C*
033400100121     C*
033500940224     C* valorizzo la chiave univoca per reperire i dati
033600940224     C* se non immissione
0337000301141   >C     xtaopz        IFNE      '01'
033800940315    >C*-------------------------------------------------------------
033900940315    >C* impostare i campi chiave ricevuti
034000940315    >C*-------------------------------------------------------------
034100100121     C                   exsr      Imposta_KEY
034200100121     C     K_tbe01       chain(N)  tntbe01L
034300100121     C                   IF        %Found(tntbe01L)
034400940224     C* riempio la videata
034500940224     C                   EXSR      RIED1
034600030113      *
034700940224     C* richiamo routine dei ctrl per decodificarla
034800940614    >C                   EXSR      CTRD1
034900030113      *
035000940224     C                   MOVE      *ALL'0'       IN5098
035100940224     C                   MOVEA     IN5098        *IN(50)
035200030113      *
035300940224     C* definizione indicatori su video
035400940224     C                   SETOFF                                       02
0355009402243    C                   SELECT
035600030114    >C     xtaopz        WHENEQ    '02'
035700940224     C                   SETON                                        02
035800030114    >C     xtaopz        WHENEQ    '05'
0359009402243-   C                   ENDSL
036000940224     C*
0361009402242-   C                   ENDIF
036200030114     C*
0363000301141   >C                   Else
036400030114     C* Se in Immissione nuovo record
036500030114     c                   exsr      NewD1
0366009402241-   C                   ENDIF
036700940224     C*
036800940117     C                   ENDSR
036900940207     C/EJECT
037000940207     C************************************************************
037100940510     C* RIEMPIMENTO VIDEATA  D1
037200940207     C************************************************************
037300030114     C     NEWD1         BEGSR
037400940207     C*
037500030114     c                   clear                   d1
037600100322     C*
037700030114     C                   ENDSR
037800030114     C************************************************************
037900030114     C* RIEMPIMENTO VIDEATA  D1
038000030114     C************************************************************
038100030114     C     RIED1         BEGSR
038200030114     C*
038300100622     c                   eval      dQEM = tbeUNI
038400100622     C                   eval      D1TRS     = §QEMDTB
038500100622     C                   eval      D1DET     = §QEMSGD
038600100622     C                   eval      D1ECC     = §QEMECC
038700100622     C                   eval      D1RIF     = §QEMRIF
038800100322     C*
038900100121     c                   setoff                                       08
039000100121     c                   if        tbeATB <> *blank
039100100121     c                   eval      d1ann = ' A N N U L L A T O '
039200100121     c                   seton                                        08
039300100121     c                   end
039400030204     C*
039500940207     C                   ENDSR
039600940309     C************************************************************
039700940309     C* GESTIONE F03 VIDEO D1
039800940309     C************************************************************
039900940309     C     F03D1         BEGSR
040000940309     C*
040100940309     C                   MOVE      *ON           $FINE
040200030113    >C                   MOVE      '1'           xtaret
040300940325     C*
040400940325     C                   ENDSR
040500940309     C************************************************************
040600940309     C* GESTIONE F12 VIDEO D1
040700940309     C************************************************************
040800940309     C     F12D1         BEGSR
040900940309     C*
041000940309     C                   MOVE      *ON           $FINE
041100100325    >C     xtaopz        ifeq      '01'
041200100325    >C                   MOVE      '2'           xtaret
041300100325     c                   end
041400940309     C*
041500940309     C                   ENDSR
041600940131     C************************************************************
041700940207     C* CONTROLLO VIDEATA
041800940131     C************************************************************
041900940127     C     CTRD1         BEGSR
042000940131     C*
042100940127     C                   SETOFF                                       99
042200030123      *
042300940127     C* non imposto pos. del cursore
042400940127     C                   Z-ADD     0             H1RIGA
042500940127     C                   Z-ADD     0             H1COLO
042600940621     C*
042700940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0428000301141   >C     xtaopz        IFNE      '04'
042900971110     C*
043000971110     C* controllo codice vuoto
043100100618$017 C     D1cod         Ifeq      *ALL'0'
043200100618     C                   SETON                                          5199
043300090226     C                   Else
043400090226     C*** se già presente
043500090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
043600090226    >C     xtaopz        IFEQ      '01'
043700090226    >C     xtaopz        oreq      '03'
043800100121     C                   exsr      Imposta_KEY
043900100121     C     K_tbe01       chain(N)  tntbe01L
044000100121     c                   if        %Found(tntbe01L)
044100100121     C                   SETON                                        5499
044200090226     C                   EndIF
044300090226     C                   ENDIF
044400100618     C*
044500090226     C                   ENDIF
044600100618      *
044700100618      * Decodifico partner
044800100618     C                   Z-ADD     1             KKUT
044900100618     C                   Z-ADD     KCI           KKCC
045000100618     C                   MOVE      *ZEROS        KKSC
045100100618     C                   MOVE      d1COD         KKSC
045200100618     C     KACO          CHAIN     CNACO00F                           21
045300100618     c                   if        not %Found(CNACO00F)
045400100618     C                   SETON                                        5599
045500100618     c                   else
045600100618     C                   eval      d1DES  = acoRAG
045700100618     c                   end
045800100121     C*
045900100121     C* controllo codice tabella
046000100121    >C     xtaopz        IFne      '01'
046100100121    >C     xtaopz        andNE     '03'
046200100121     C                   exsr      Imposta_KEY
046300100121     C     k_tbe01       chain     tntbe01l
046400100121     c                   if        not %Found(tntbe01l)
046500100121     C**** se sbagliato il codice
046600100121     C                   SETON                                        5599
046700090226     c                   end
046800100121     c                   endIF
046900100618      *
047000090226     C                   ENDIF
047100100121      *
047200100325      * -------------
047300100325      *   Controlli sui campi e gruppi di campi
047400100325      * -------------
047500100325      *  se non è stato immesso nulla
047600100325     c                   if        DSP_dsCAMPI = *blank
047700100618     c                   seton                                        6199
047800100325     C                   ENDIF
047900100325      *
048000100325      *    Altri:
048100100325      *
048200100325      * -------------
048300940302     C     *IN99         IFEQ      *ON
048400940510     C     $GEST         ANDEQ     'D1'
048500940117     C                   MOVEA     *IN           WIN
048600940128     C                   MOVE      *ALL'0'       IN5098           49
048700940128     C                   MOVEA     IN5098        *IN(50)
048800940614     C                   WRITE     D1
048900940117     C                   MOVEA     WIN           *IN
049000940302     C                   ENDIF
049100940127     C*
049200940117     C                   ENDSR
049300030204     C/EJECT
049400030204     C************************************************************
049500030204    >C* AGGIORNAMENTO ANAGRAFICA
049600030204     C************************************************************
049700100325    >C     FLDAGG        BEGSR
049800030204     C*
049900030204     C*  campi chiave da scrivere e aggiornare
050000100622     c                   eval      tbecod = 'QEM'
050100100121     c                   clear                   tbeke1
050200100121     c                   clear                   tbeke2
050300100618     C                   move      *zeros        a1COD             7
050400100618     C                   move      d1COD         a1cod
050500100618     c                   eval      tbeke1 = a1cod
050600100322     C*
050700100618     c                   exsr      RIEANA
050800100121     C*
050900100121     c                   clear                   tbeATB
051000100121     C*
051100030204     C                   ENDSR
051200940207     C/EJECT
051300940224     C************************************************************
051400940315    >C* AGGIORNAMENTO ANAGRAFICA
051500940224     C************************************************************
051600940315    >C     AGGANA        BEGSR
051700940207     C*
051800100408     c                   clear                   aggiorna_FLG      1
051900100408     C*
052000030204     C                   exsr      FLDAGG
052100030204     C*
052200940207     C                   SELECT
052300030204     C*
052400100323      *  Immette/Copia il record
052500030114    >C     xtaopz        WHENEQ    '01'
052600030114    >C     xtaopz        OREQ      '03'
052700030204     C*
052800100408     c                   eval      aggiorna_FLG = 'S'
052900100408     C*
053000090226    >C                   EXSR      RIEANA
053100100324     c                   exsr      wind_trasm
053200100121     c                   write     tntbe000                             59
053300940318     C* se rcd non scrivibile attivo errore
053400030114    >C   59              SETON                                          99
053500030204      *
053600100323      *  Modifica il record
053700030114    >C     xtaopz        WHENEQ    '02'
053800100408     c*
053900100408     c                   eval      aggiorna_FLG = 'S'
054000100408     c*
054100100121     C                   exsr      Imposta_KEY
054200100121     C     K_tbe01       chain     tntbe01L
054300030204     C                   exsr      FLDAGG
054400100324     c                   exsr      wind_trasm
054500090226    >C                   EXSR      RIEANA
054600100121     c                   update    tntbe000                             59
054700030114    >C   59              SETON                                          99
054800030204      *
054900100323      *  Cancella il record
055000030114    >C     xtaopz        WHENEQ    '04'
055100100408     c*
055200100408     c                   eval      aggiorna_FLG = 'S'
055300100408     c*
055400100121     C                   exsr      Imposta_KEY
055500100121     C     K_tbe01       chain     tntbe01L
055600100121     c                   move      'A'           tbeATB
055700100324     c                   exsr      wind_trasm
055800100121     c                   update    tntbe000                             59
055900030114    >C   59              SETON                                          99
056000100323      *
056100100323      *  Riattiva il record
056200100323    >C     xtaopz        WHENEQ    '08'
056300100408     c*
056400100408     c                   eval      aggiorna_FLG = 'S'
056500100408     c*
056600100323     C                   exsr      Imposta_KEY
056700100323     C     K_tbe01       chain     tntbe01L
056800100323     c                   move      ' '           tbeATB
056900100324     c                   exsr      wind_trasm
057000100323     c                   update    tntbe000                             59
057100100323    >C   59              SETON                                          99
057200940621     C*
057300940207     C                   ENDSL
057400100408     C*
057500940203     C                   ENDSR
057600940224     C************************************************************
057700940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
057800940224     C************************************************************
057900090226     C     RIEANA        BEGSR
058000030114     C*
058100100622     C                   eval       §QEMDTB = D1TRS
058200100622     C                   eval       §QEMSGD = D1DET
058300100622     C                   eval       §QEMECC = D1ECC
058400100622     C                   eval       §QEMRIF = D1RIF
058500100618     C*
058600100622     C                   MOVEl     DQEM          tbeUNI
058700100121     C*
058800100324     C                   ENDSR
058900100324     C************************************************************
059000100324     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
059100100324     C************************************************************
059200100324     C     Wind_trasm    BEGSR
059300100324     C*
059400100324     C                   mOVEl     tbeFTT        t1trs
059500100324     C                   z-add     tbeFLT        t1fil
059600100324     c                   exfmt     w1tras
059700100324     C                   MOVEl     t1trs         tbeFTT
059800100324     C                   z-add     t1fil         tbeFLT
059900100324     c                   clear                   tbeFTR
060000100324     c                   clear                   tbeDTR
060100030114     C*
060200940207     C                   ENDSR
060300940224     C************************************************************
060400940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
060500940224     C************************************************************
060600940224     C     GESAGG        BEGSR
060700940224     C*
060800940224     C* segnalo al pgm chiamante l'avvenuta conferma
060900030114    >C                   MOVE      *ON           xtaopr
061000940224     C* segnalo al pgm chiamante l'aver premuto l'F6
061100030114    >C                   MOVE      '0'           xtaret
061200940224     C*
0613009402241    C                   SELECT
061400940224     C* nel caso di immissione, ripristino la videata iniziale
061500030114    >C     xtaopz        WHENEQ    '01'
061600940224     C                   MOVE      *ON           $INZD1
061700030114     C                   MOVE      *ON           $FINE
061800940224     C* altrimenti ritorno al pgm chiamante
061900940224     C                   OTHER
062000940224     C                   MOVE      *ON           $FINE
0621009402241-   C                   ENDSL
062200940224     C*
062300940224     C                   ENDSR
062400940131     C************************************************************
062500100121     C*
062600100121     C************************************************************
062700100121     C     Imposta_KEY   BEGSR
062800100121     C*
062900100121     c                   clear                   tbe_Key1
063000100121     c                   clear                   tbe_Key2
063100100618     c                   movel(p)  d1Cod         tbe_Key1
063200100121     C*
063300100121     C                   ENDSR
