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
002300100618     FCNACO00F  IF   E           K DISK
002400100622$001 FTNTBQEMD2 CF   E             WORKSTN
002500940201     F                                     INFDS(DSFMT)
002600100121     Ftntbe01L  UF A E           K DISK
002700940128     D*----------------------------------------------------*
002800940127     D* Passaggio Parametri
002900940127     D KPJBA         E DS
003000100622     D dQEM          E DS
003100100409     D dUte01        E DS
003200100618     D*-------------
003300100618     D UTEDSE0F      E DS
003400100618     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
003500100618     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
003600100618      * Ds scomposzione tipo capoconti
003700100618     D TCUDS           DS
003800100618     D  F34                    3      4
003900100618     D  F56                    5      6
004000100618      *
004100100618      *-------------
004200100618$003 D KKCC            S                   Like(acokcc)
004300100618$003 D KKut            S                   Like(acokut)
004400100618$003 D KKSC            S                   Like(acoksc)
004500100618     D*-------------
004600030113     D* Parametri in ricezione
004700030113     D  TABDS          DS
004800030113     D  XTAOPZ                 1      2
004900030113     D  XTARET                 3      3
005000030113     D  XTAOPR                 4      4
005100030113     D  XTAERR                 5      5
005200100409     D  XTAKEY1                6     20
005300100409     D  XTAKEY2               21     33
005400100409     D  XTAcod                31     33
005500100325     D*-------------
005600100325     D DSP_dsCAMPI     DS
005700100618     D  dsp_SKcampi            1     38    DIM(38)
005800100325      *
005900100618     D  D1TRS                  1      1
006000100618     D  D1DET                  2      2
006100100618     D  D1ECC                  3      3
006200100618     D  D1RIF                  4     38
006300100325      *
006400940201     D*-------------
006500030204     D  S_TASTO        S                   like($Tasto)
006600940201     D DSFMT           DS
006700940506     D  $TASTO               369    369
006800940201     D  NRG                  370    370
006900940201     D  NCL                  371    371
007000940201     D*-------------
007100940201     D* posizione cursore
007200940201     D CURSOR          DS
007300940223     D  RIRI                   1      2B 0 INZ
007400940201     D  R$$                    2      2
007500940223     D  COCO                   3      4B 0 INZ
007600940201     D  C$$                    4      4
007700940207     D*-------------
007800940207     D* Reperimento nome PGM
007900940207     D STATUS         SDS           333
008000940207     D  DSPGM            *PROC
008100940225     D*-------------
008200940225     D* COSTANTI
008300940225     D*-------------
008400940506     D* Tasti di funzione
008500940506     D F01             C                   CONST(X'31')
008600940506     D F02             C                   CONST(X'32')
008700940506     D F03             C                   CONST(X'33')
008800940506     D F04             C                   CONST(X'34')
008900940506     D F05             C                   CONST(X'35')
009000940506     D F06             C                   CONST(X'36')
009100940506     D F07             C                   CONST(X'37')
009200940506     D F08             C                   CONST(X'38')
009300940506     D F09             C                   CONST(X'39')
009400940506     D F10             C                   CONST(X'3A')
009500940506     D F11             C                   CONST(X'3B')
009600940506     D F12             C                   CONST(X'3C')
009700940506     D F13             C                   CONST(X'B1')
009800940506     D F14             C                   CONST(X'B2')
009900940506     D F15             C                   CONST(X'B3')
010000940506     D F16             C                   CONST(X'B4')
010100940506     D F17             C                   CONST(X'B5')
010200940506     D F18             C                   CONST(X'B6')
010300940506     D F19             C                   CONST(X'B7')
010400940506     D F20             C                   CONST(X'B8')
010500940506     D F21             C                   CONST(X'B9')
010600940506     D F22             C                   CONST(X'BA')
010700940506     D F23             C                   CONST(X'BB')
010800940506     D F24             C                   CONST(X'BC')
010900940506     D ENTER           C                   CONST(X'F1')
011000940506     D ROLDWN          C                   CONST(X'F4')
011100940506     D ROLLUP          C                   CONST(X'F5')
011200940506     D*-------------
011300941108     D DATA            C                   CONST('0001-01-01')
011400030114     D Annull          C                   CONST('  ANNULLATO  ')
011500100323     D OPz_Immissione  C                   CONST('IMMISSIONE')
011600100323     D OPz_Variazione  C                   CONST('VARIAZIONE')
011700100323     D OPz_Annulla     C                   CONST(' ANNULLA  ')
011800100323     D OPz_Visualizza  C                   CONST('VISUALIZZA')
011900100323     D OPz_Riattiva    C                   CONST(' RIATTIVA ')
012000940127     C*----------------------------------------------------*
012100940127     C*                MAIN LINE PROGRAM
012200940127     C*----------------------------------------------------*
012300100121     c                   exsr      INIZIO
012400940223     C*
012500940223     C     $FINE         DOWEQ     *OFF
012600940202     C     $GEST         CASEQ     'D1'          GESD1
012700940117     C                   END
012800940117     C                   END
012900940325     C* fine programma
013000030113     c                   movel     tabds         kpjbu
013100030113     C                   Seton                                        LR
013200940131     C************************************************************
013300100121     C* OPERAZIONI INIZIALI
013400100121     C************************************************************
013500100121     C     INIZIO        BEGSR
013600100121     C*
013700100121     C* Reperimento parametri
013800100121     C     *ENTRY        PLIST
013900100121     C                   PARM                    KPJBA
014000100121      *
014100100121     C                   movel     kpjbu         tabds
014200100121     c                   setoff                                       08
014300100121     C*
014400100121     C* Variabili per gestione videate
014500100121     C                   MOVE      'D1'          $GEST
014600100121     C                   MOVE      *OFF          $FINE
014700100121     C                   MOVE      *ON           $INZD1
014800100121     C                   MOVE      *BLANK        $LASTG
014900100121     C                   MOVE      *BLANK        $LASTV
015000100121     C                   Z-ADD     0             $ULKD1            3 0
015100100622     C                   MOVEL     'QEM'         tbe_COD           3
015200100622     C                   CLEAR                   dQEM
015300100121     C*
015400100121     C* Variabili appoggio
015500100121     C                   Z-ADD     0             CURR
015600100121     C                   Z-ADD     0             CURC
015700100618      *
015800100618     C* RECUPERO DATI DELL'UTENTE
015900100618     C                   Z-ADD     1             CODUT             1 0
016000100618     C                   CALL      'XPARUT'
016100100618     C                   PARM                    UTEDSE0F
016200100618     C                   MOVEL     RAGUT         RSUT             20
016300100618     C*
016400100618     C* RICERCA CAPOCONTI
016500100618     C                   DO        50            X
016600100618     C                   MOVE      TCU(X)        TCUDS
016700100618     C     F56           IFEQ      'CG'
016800100618     C     F34           ANDEQ     '01'
016900100618     C                   Z-ADD     KCU(X)        KCI               4 0
017000100618     C                   END
017100100618     C                   END
017200100618     C*
017300100121     C*
017400100121     C* Valorizzazione campi univoci testate
017500100121     C                   CLEAR                   T1
017600100121     C                   MOVEL     KNSIF         NOMSIF
017700100121     C******             MOVEL     XSCDSI        NOMDIT
017800100121     C                   MOVEL     DSPGM         NOMPGM
017900100121     C*
018000100121     C* Inizializzazione/Decodifica prima videata
018100100121     C                   EXSR      INZD1
018200100121     C                   MOVE      *OFF          $INZD1
018300100121     C*
018400100121     C                   ENDSR
018500100121     C************************************************************
018600100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
018700100121     C************************************************************
018800100121     C     DEFCAM        BEGSR
018900100121     C*
019000100121     C* klist
019100100618      *
019200100618     C     KACO          KLIST
019300100618     C                   KFLD                    KKUT
019400100618     C                   KFLD                    KKCC
019500100618     C                   KFLD                    KKSC
019600100618      *
019700100121     C     k_tbe01       kLIST
019800100121     C                   kfld                    tbe_cod
019900100121     C                   kfld                    tbe_key1         15
020000100121     C                   kfld                    tbe_key2         15
020100100409      *
020200100409     C     k_tbe011      kLIST
020300100409     C                   kfld                    tbe_cod
020400100409     C                   kfld                    xtakey1
020500100121     C*
020600100121     C* Variabili per gestione videate
020700100121     C*
020800100121     C                   MOVE      *BLANK        $GEST             2
020900100121     C                   MOVE      *BLANK        $FINE             1
021000100121     C                   MOVE      *BLANK        $INZD1            1
021100100121     C                   MOVE      *BLANK        $LASTG            2
021200100121     C                   MOVE      *BLANK        $LASTV            2
021300100121     C*
021400100121     C* Variabili appoggio
021500100121     C                   Z-ADD     0             CURR              2 0
021600100121     C                   Z-ADD     0             CURC              2 0
021700100121     C                   MOVE      *ZEROS        WIN              99
021800100121     C                   MOVE      *BLANK        WTBERR            1
021900100121     C* Indici
022000100121     C                   Z-ADD     0             X                 3 0
022100100121     C                   Z-ADD     0             Y                 3 0
022200100121     C*
022300100121     C                   ENDSR
022400100121     C************************************************************
022500940510     C* GESTIONE VIDEO RECORD D1
022600940131     C************************************************************
022700940127     C     GESD1         BEGSR
022800030113      *
022900940225     C* inizializzazione videata
023000940223     C     $INZD1        IFEQ      *ON
023100940127     C                   EXSR      INZD1
023200940223     C                   MOVE      *OFF          $INZD1
023300940117     C                   END
023400030113      *
023500940225     C* emissione piede videata se proveniente da altra
023600940225     C* salvataggio valore formato di provenienza
023700940207     C     $LASTV        IFNE      'D1'
023800940614     C                   WRITE     T1
023900940202     C                   MOVE      $LASTV        $LASTG
024000940202     C                   MOVE      'D1'          $LASTV
024100940202     C                   END
024200940131     C*
024300940203     C                   SELECT
024400100323     C* Inserimento e Copia
024500030114    >C     xtaopz        WHENEQ    '01'
024600100121    >C     xtaopz        OREQ      '03'
024700100323     c                   movel     Opz_ImmissioneD1Fun
024800100323     C* Visualizzazione
024900030114    >C     xtaopz        WHENEQ    '05'
025000100323     c                   movel     Opz_VisualizzaD1Fun
025100030114     C                   SETON                                        020306
025200100323     C* Annulla
025300030114    >C     xtaopz        WHENEQ    '04'
025400100323     c                   movel     Opz_Annulla   D1Fun
025500030114     C                   SETON                                        0203
025600030114     C                   SETOFF                                         06
025700100323     C* Ripristina
025800100323    >C     xtaopz        WHENEQ    '08'
025900100323     c                   movel     OPz_Riattiva  D1Fun
026000100323     C                   SETON                                        02  03
026100100323     C                   SETON                                        06  18
026200940620     C* Manutenzione
026300030113    >C     xtaopz        WHENEQ    '02'
026400100323     c                   movel     Opz_VariazioneD1Fun
026500030114     C                   SETON                                        02
026600030114     C                   SETOFF                                       0306
026700940620     C*
026800940620     C                   OTHER
026900090226     C*
027000030114     C                   SETOFF                                       020306
027100940203     C                   ENDSL
027200030114     C*
027300030114     c                   SETOFF                                         99
027400940620     C* Immissione/Modifica/Copia
027500030113     C*               *----------------*
027600940620     C                   EXFMT     D1
027700030113     C*               *----------------*
027800030114     C*
027900030114     C     *IN99         IFEQ      *OFF
028000030114     C*
0281000301141    C                   SELECT
028200030114     C* F3=Fine
028300030114     C     $TASTO        WHENEQ    F03
028400030114     C                   EXSR      F03D1
028500030114     C* F12=Ritorno
028600030114     C     $TASTO        WHENEQ    F12
028700030114     C                   EXSR      F12D1
028800030114     C*
0289000301141O   C                   OTHER
029000030114     C*
029100030114     C                   EXSR      CTRD1
029200030113     C*
029300940616     C     *IN99         IFEQ      *OFF
029400940616     C* F6
0295000302052    C     $TASTO        ifeq      F06
0296001003232    C     $TASTO        oreq      F18
0297000301152    C     $TASTO        oreq      F23
029800940224     C* eseguo aggiornamento
029900940315    >C                   EXSR      AGGANA
030000940224     C* eseguo operazioni del dopo-aggiornamento
030100030114     C  N99              EXSR      GESAGG
030200030205     c                   end
030300030114     C*
030400030114     C                   ENDIF
030500030114     C*
0306000301142-   C                   ENDSL
030700030114     C*
0308000301142-   C                   ENDIF
030900030114     C*
031000940117     C                   ENDSR
031100940117      ************************************************************
031200940207      * INIZIALIZZAZIONE VIDEATA DATI
031300940117      ************************************************************
031400940127     C     INZD1         BEGSR
031500940131     C*
031600940615     C                   CLEAR                   D1
031700100121     C*
031800100618     C                   move      *zeros        a1COD             7
031900100618     C                   eval      a1cod = %Subst(xtaKEY1:1:7)
032000100618     C                   move      a1COD         d1cod
032100100618      *
032200100618      * Decodifico partner
032300100618     C                   Z-ADD     1             KKUT
032400100618     C                   Z-ADD     KCI           KKCC
032500100618     C                   MOVE      *ZEROS        KKSC
032600100618     C                   MOVE      d1COD         KKSC
032700100618     C     KACO          CHAIN     CNACO00F                           21
032800100618     c                   if        %Found(CNACO00F)
032900100618     C                   eval      d1DES  = acoRAG
033000100618     c                   end
033100100121     C*
033200100121     C*
033300940224     C* valorizzo la chiave univoca per reperire i dati
033400940224     C* se non immissione
0335000301141   >C     xtaopz        IFNE      '01'
033600940315    >C*-------------------------------------------------------------
033700940315    >C* impostare i campi chiave ricevuti
033800940315    >C*-------------------------------------------------------------
033900100121     C                   exsr      Imposta_KEY
034000100121     C     K_tbe01       chain(N)  tntbe01L
034100100121     C                   IF        %Found(tntbe01L)
034200940224     C* riempio la videata
034300940224     C                   EXSR      RIED1
034400030113      *
034500940224     C* richiamo routine dei ctrl per decodificarla
034600940614    >C                   EXSR      CTRD1
034700030113      *
034800940224     C                   MOVE      *ALL'0'       IN5098
034900940224     C                   MOVEA     IN5098        *IN(50)
035000030113      *
035100940224     C* definizione indicatori su video
035200940224     C                   SETOFF                                       02
0353009402243    C                   SELECT
035400030114    >C     xtaopz        WHENEQ    '02'
035500940224     C                   SETON                                        02
035600030114    >C     xtaopz        WHENEQ    '05'
0357009402243-   C                   ENDSL
035800940224     C*
0359009402242-   C                   ENDIF
036000030114     C*
0361000301141   >C                   Else
036200030114     C* Se in Immissione nuovo record
036300030114     c                   exsr      NewD1
0364009402241-   C                   ENDIF
036500940224     C*
036600940117     C                   ENDSR
036700940207     C/EJECT
036800940207     C************************************************************
036900940510     C* RIEMPIMENTO VIDEATA  D1
037000940207     C************************************************************
037100030114     C     NEWD1         BEGSR
037200940207     C*
037300030114     c                   clear                   d1
037400100322     C*
037500030114     C                   ENDSR
037600030114     C************************************************************
037700030114     C* RIEMPIMENTO VIDEATA  D1
037800030114     C************************************************************
037900030114     C     RIED1         BEGSR
038000030114     C*
038100100622     c                   eval      dQEM = tbeUNI
038200100622     C                   eval      D1TRS     = §QEMDTB
038300100622     C                   eval      D1DET     = §QEMSGD
038400100622     C                   eval      D1ECC     = §QEMECC
038500100622     C                   eval      D1RIF     = §QEMRIF
038600100322     C*
038700100121     c                   setoff                                       08
038800100121     c                   if        tbeATB <> *blank
038900100121     c                   eval      d1ann = ' A N N U L L A T O '
039000100121     c                   seton                                        08
039100100121     c                   end
039200030204     C*
039300940207     C                   ENDSR
039400940309     C************************************************************
039500940309     C* GESTIONE F03 VIDEO D1
039600940309     C************************************************************
039700940309     C     F03D1         BEGSR
039800940309     C*
039900940309     C                   MOVE      *ON           $FINE
040000030113    >C                   MOVE      '1'           xtaret
040100940325     C*
040200940325     C                   ENDSR
040300940309     C************************************************************
040400940309     C* GESTIONE F12 VIDEO D1
040500940309     C************************************************************
040600940309     C     F12D1         BEGSR
040700940309     C*
040800940309     C                   MOVE      *ON           $FINE
040900100325    >C     xtaopz        ifeq      '01'
041000100325    >C                   MOVE      '2'           xtaret
041100100325     c                   end
041200940309     C*
041300940309     C                   ENDSR
041400940131     C************************************************************
041500940207     C* CONTROLLO VIDEATA
041600940131     C************************************************************
041700940127     C     CTRD1         BEGSR
041800940131     C*
041900940127     C                   SETOFF                                       99
042000030123      *
042100940127     C* non imposto pos. del cursore
042200940127     C                   Z-ADD     0             H1RIGA
042300940127     C                   Z-ADD     0             H1COLO
042400940621     C*
042500940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0426000301141   >C     xtaopz        IFNE      '04'
042700971110     C*
042800971110     C* controllo codice vuoto
042900100618$017 C     D1cod         Ifeq      *ALL'0'
043000100618     C                   SETON                                          5199
043100090226     C                   Else
043200090226     C*** se già presente
043300090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
043400090226    >C     xtaopz        IFEQ      '01'
043500090226    >C     xtaopz        oreq      '03'
043600100121     C                   exsr      Imposta_KEY
043700100121     C     K_tbe01       chain(N)  tntbe01L
043800100121     c                   if        %Found(tntbe01L)
043900100121     C                   SETON                                        5499
044000090226     C                   EndIF
044100090226     C                   ENDIF
044200100618     C*
044300090226     C                   ENDIF
044400100618      *
044500100618      * Decodifico partner
044600100618     C                   Z-ADD     1             KKUT
044700100618     C                   Z-ADD     KCI           KKCC
044800100618     C                   MOVE      *ZEROS        KKSC
044900100618     C                   MOVE      d1COD         KKSC
045000100618     C     KACO          CHAIN     CNACO00F                           21
045100100618     c                   if        not %Found(CNACO00F)
045200100618     C                   SETON                                        5599
045300100618     c                   else
045400100618     C                   eval      d1DES  = acoRAG
045500100618     c                   end
045600100121     C*
045700100121     C* controllo codice tabella
045800100121    >C     xtaopz        IFne      '01'
045900100121    >C     xtaopz        andNE     '03'
046000100121     C                   exsr      Imposta_KEY
046100100121     C     k_tbe01       chain     tntbe01l
046200100121     c                   if        not %Found(tntbe01l)
046300100121     C**** se sbagliato il codice
046400100121     C                   SETON                                        5599
046500090226     c                   end
046600100121     c                   endIF
046700100618      *
046800090226     C                   ENDIF
046900100121      *
047000100325      * -------------
047100100325      *   Controlli sui campi e gruppi di campi
047200100325      * -------------
047300100325      *  se non è stato immesso nulla
047400100325     c                   if        DSP_dsCAMPI = *blank
047500100618     c                   seton                                        6199
047600100325     C                   ENDIF
047700100325      *
047800100325      *    Altri:
047900100325      *
048000100325      * -------------
048100940302     C     *IN99         IFEQ      *ON
048200940510     C     $GEST         ANDEQ     'D1'
048300940117     C                   MOVEA     *IN           WIN
048400940128     C                   MOVE      *ALL'0'       IN5098           49
048500940128     C                   MOVEA     IN5098        *IN(50)
048600940614     C                   WRITE     D1
048700940117     C                   MOVEA     WIN           *IN
048800940302     C                   ENDIF
048900940127     C*
049000940117     C                   ENDSR
049100030204     C/EJECT
049200030204     C************************************************************
049300030204    >C* AGGIORNAMENTO ANAGRAFICA
049400030204     C************************************************************
049500100325    >C     FLDAGG        BEGSR
049600030204     C*
049700030204     C*  campi chiave da scrivere e aggiornare
049800100622     c                   eval      tbecod = 'QEM'
049900100121     c                   clear                   tbeke1
050000100121     c                   clear                   tbeke2
050100100618     C                   move      *zeros        a1COD             7
050200100618     C                   move      d1COD         a1cod
050300100618     c                   eval      tbeke1 = a1cod
050400100322     C*
050500100618     c                   exsr      RIEANA
050600100121     C*
050700100121     c                   clear                   tbeATB
050800100121     C*
050900030204     C                   ENDSR
051000940207     C/EJECT
051100940224     C************************************************************
051200940315    >C* AGGIORNAMENTO ANAGRAFICA
051300940224     C************************************************************
051400940315    >C     AGGANA        BEGSR
051500940207     C*
051600100408     c                   clear                   aggiorna_FLG      1
051700100408     C*
051800030204     C                   exsr      FLDAGG
051900030204     C*
052000940207     C                   SELECT
052100030204     C*
052200100323      *  Immette/Copia il record
052300030114    >C     xtaopz        WHENEQ    '01'
052400030114    >C     xtaopz        OREQ      '03'
052500030204     C*
052600100408     c                   eval      aggiorna_FLG = 'S'
052700100408     C*
052800090226    >C                   EXSR      RIEANA
052900100324     c                   exsr      wind_trasm
053000100121     c                   write     tntbe000                             59
053100940318     C* se rcd non scrivibile attivo errore
053200030114    >C   59              SETON                                          99
053300030204      *
053400100323      *  Modifica il record
053500030114    >C     xtaopz        WHENEQ    '02'
053600100408     c*
053700100408     c                   eval      aggiorna_FLG = 'S'
053800100408     c*
053900100121     C                   exsr      Imposta_KEY
054000100121     C     K_tbe01       chain     tntbe01L
054100030204     C                   exsr      FLDAGG
054200100324     c                   exsr      wind_trasm
054300090226    >C                   EXSR      RIEANA
054400100121     c                   update    tntbe000                             59
054500030114    >C   59              SETON                                          99
054600030204      *
054700100323      *  Cancella il record
054800030114    >C     xtaopz        WHENEQ    '04'
054900100408     c*
055000100408     c                   eval      aggiorna_FLG = 'S'
055100100408     c*
055200100121     C                   exsr      Imposta_KEY
055300100121     C     K_tbe01       chain     tntbe01L
055400100121     c                   move      'A'           tbeATB
055500100324     c                   exsr      wind_trasm
055600100121     c                   update    tntbe000                             59
055700030114    >C   59              SETON                                          99
055800100323      *
055900100323      *  Riattiva il record
056000100323    >C     xtaopz        WHENEQ    '08'
056100100408     c*
056200100408     c                   eval      aggiorna_FLG = 'S'
056300100408     c*
056400100323     C                   exsr      Imposta_KEY
056500100323     C     K_tbe01       chain     tntbe01L
056600100323     c                   move      ' '           tbeATB
056700100324     c                   exsr      wind_trasm
056800100323     c                   update    tntbe000                             59
056900100323    >C   59              SETON                                          99
057000940621     C*
057100940207     C                   ENDSL
057200100408     C*
057300940203     C                   ENDSR
057400940224     C************************************************************
057500940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
057600940224     C************************************************************
057700090226     C     RIEANA        BEGSR
057800030114     C*
057900100622     C                   eval       §QEMDTB = D1TRS
058000100622     C                   eval       §QEMSGD = D1DET
058100100622     C                   eval       §QEMECC = D1ECC
058200100622     C                   eval       §QEMRIF = D1RIF
058300100618     C*
058400100622     C                   MOVEl     DQEM          tbeUNI
058500100121     C*
058600100324     C                   ENDSR
058700100324     C************************************************************
058800100324     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
058900100324     C************************************************************
059000100324     C     Wind_trasm    BEGSR
059100100324     C*
059200100324     C                   mOVEl     tbeFTT        t1trs
059300100324     C                   z-add     tbeFLT        t1fil
059400100324     c                   exfmt     w1tras
059500100324     C                   MOVEl     t1trs         tbeFTT
059600100324     C                   z-add     t1fil         tbeFLT
059700100324     c                   clear                   tbeFTR
059800100324     c                   clear                   tbeDTR
059900030114     C*
060000940207     C                   ENDSR
060100940224     C************************************************************
060200940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
060300940224     C************************************************************
060400940224     C     GESAGG        BEGSR
060500940224     C*
060600940224     C* segnalo al pgm chiamante l'avvenuta conferma
060700030114    >C                   MOVE      *ON           xtaopr
060800940224     C* segnalo al pgm chiamante l'aver premuto l'F6
060900030114    >C                   MOVE      '0'           xtaret
061000940224     C*
0611009402241    C                   SELECT
061200940224     C* nel caso di immissione, ripristino la videata iniziale
061300030114    >C     xtaopz        WHENEQ    '01'
061400940224     C                   MOVE      *ON           $INZD1
061500030114     C                   MOVE      *ON           $FINE
061600940224     C* altrimenti ritorno al pgm chiamante
061700940224     C                   OTHER
061800940224     C                   MOVE      *ON           $FINE
0619009402241-   C                   ENDSL
062000940224     C*
062100940224     C                   ENDSR
062200940131     C************************************************************
062300100121     C*
062400100121     C************************************************************
062500100121     C     Imposta_KEY   BEGSR
062600100121     C*
062700100121     c                   clear                   tbe_Key1
062800100121     c                   clear                   tbe_Key2
062900100618     c                   movel(p)  d1Cod         tbe_Key1
063000100121     C*
063100100121     C                   ENDSR
