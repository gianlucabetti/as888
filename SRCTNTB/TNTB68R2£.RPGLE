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
002500080701$001 FTNTB68D2  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700080701     FTNTBE01L  UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900080701$003 D EXTDDSF       E DS                  EXTNAME(DDSF) INZ
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
004000030113     D  XTAKEY                 6     13
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
013600030114     C                   SETON                                        0203
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
017400030204     c                   Exsr      ACHI
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
019400080701     C                   move      'DSF'         TBEcod
019500030113     C                   MOVEl     xtakey        D1COD
019600080701     C                   MOVEl(p)  xtakey        TBEke1
019700940207     C*
019800940224     C* valorizzo la chiave univoca per reperire i dati
019900940224     C* se non immissione
0200000301141   >C     xtaopz        IFNE      '01'
020100030204      *
020200940315    >C*-------------------------------------------------------------
020300940315    >C* impostare i campi chiave ricevuti
020400940315    >C*-------------------------------------------------------------
020500080701     C                   move      'DSF'         TBEcod
020600080701     C     KTBL          chain(N)  TNTBE01L
020700940614     C*
020800080701     C                   IF        %Found(TNTBE01L)
020900080701     C                   MOVEL     TBEuni        EXTDDSF
021000080701     c                   if        TbeATB = '*'
021100030115     c                   seton                                        08
021200030114     C                   MOVEl     Annull        D1Ann
021300030114     C                   end
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
025600080701     c                   eval      d1DES = D§DSFDES
025700030204     C*
025800940207     C                   ENDSR
025900940309     C************************************************************
026000940309     C* GESTIONE F03 VIDEO D1
026100940309     C************************************************************
026200940309     C     F03D1         BEGSR
026300940309     C*
026400940309     C                   MOVE      *ON           $FINE
026500030113    >C                   MOVE      '1'           xtaret
026600940325     C*
026700940325     C                   ENDSR
026800940309     C************************************************************
026900940309     C* GESTIONE F12 VIDEO D1
027000940309     C************************************************************
027100940309     C     F12D1         BEGSR
027200940309     C*
027300940309     C                   MOVE      *ON           $FINE
027400940309     C*
027500940309     C                   ENDSR
027600940131     C************************************************************
027700940207     C* CONTROLLO VIDEATA
027800940131     C************************************************************
027900940127     C     CTRD1         BEGSR
028000940131     C*
028100940127     C                   SETOFF                                       99
028200030123      *
028300940127     C* non imposto pos. del cursore
028400940127     C                   Z-ADD     0             H1RIGA
028500940127     C                   Z-ADD     0             H1COLO
028600940621     C*
028700940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0288000301141   >C     xtaopz        IFNE      '04'
028900971110     C*
029000971110     C* controllo codice vuoto
029100971110$017 C     D1COD         IFEQ      *ALL' '
029200971110     C                   SETON                                        5099
029300971110     C                   ENDIF
029400030114     C*
029500030114     C* controllo codice vuoto
029600030114$017 C     D1DES         IFEQ      *ALL' '
029700030114     C                   SETON                                        5199
029800030114     C                   ENDIF
029900940207     C*
030000940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
030100030114    >C     xtaopz        IFEQ      '01'
030200030114    >C     xtaopz        oreq      '03'
030300080701     c                   movel(p)  d1cod         tbeke1
030400080701     C     KTBL          chain(N)  TNTBE01L
030500080701     c                   if        %Found(TNTBE01L)
030600030114     C                   SETON                                        5299
030700030114     C                   EndIF
030800030114     C                   ENDIF
030900030114      *
0310009402241-   C                   END
031100940131     C*
031200940302     C     *IN99         IFEQ      *ON
031300940510     C     $GEST         ANDEQ     'D1'
031400940117     C                   MOVEA     *IN           WIN
031500940128     C                   MOVE      *ALL'0'       IN5098           49
031600940128     C                   MOVEA     IN5098        *IN(50)
031700940614     C                   WRITE     D1
031800940117     C                   MOVEA     WIN           *IN
031900940302     C                   ENDIF
032000940127     C*
032100940117     C                   ENDSR
032200960305     C/EJECT
032300030204     C************************************************************
032400030204     C* A chi deve trasmettere
032500030204     C************************************************************
032600030204     C     ACHI          BEGSR
032700030204     C*
032800030204     c                   eval      S_Tasto = $Tasto
032900030204     C*
033000030204     c                   move      '1'           t1trs
033100030204     c                   clear                   t1fil
033200030204     c     su_win        tag
033300030204     C                   Exfmt     W1Tras
033400030204     c                   if        t1trs = '2' and
033500030204     c                             t1fil = 0
033600030204     c                   seton                                        77
033700030204     c                   goto      su_win
033800030204     c                   end
033900030204     C*
034000030204     c                   eval      $Tasto = S_Tasto
034100030204     C*
034200030204     C                   ENDSR
034300030204     C/EJECT
034400030204     C************************************************************
034500030204    >C* AGGIORNAMENTO ANAGRAFICA
034600030204     C************************************************************
034700030204    >C     FLDAGG        BEGSR
034800030204     C*
034900030204     C*  campi chiave da scrivere e aggiornare
035000080701     C                   movel     *blanks       tbeKE1
035100080701     C                   MOVEl     D1COD         tbeKE1
035200080701     c                   movel     T1trs         tbeFTT
035300080701     c                   move      T1fil         tbeFLT
035400080701     c                   move      *zeros        tbeDTR
035500080701     c                   move      *blanks       tbeFTR
035600080701     c                   move      ' '           TBeATB
035700030204     C*
035800030204     C                   ENDSR
035900940207     C/EJECT
036000940224     C************************************************************
036100940315    >C* AGGIORNAMENTO ANAGRAFICA
036200940224     C************************************************************
036300940315    >C     AGGANA        BEGSR
036400940207     C*
036500030204     C                   exsr      FLDAGG
036600030204     C*
036700940207     C                   SELECT
036800030204     C*
036900030114    >C     xtaopz        WHENEQ    '01'
037000030114    >C     xtaopz        OREQ      '03'
037100030204     C*
037200080701     C                   move      'DSF'         TBEcod
037300080701    >C                   EXSR      RIEANA_DSF
037400080701     c                   write     TNTBE000                             59
037500940318     C* se rcd non scrivibile attivo errore
037600030114    >C   59              SETON                                          99
037700030204      *
037800030114    >C     xtaopz        WHENEQ    '02'
037900030204      *
038000080701     C                   move      'DSF'         TBEcod
038100080701     C     KTBL          chain     TNTBE01L
038200030204     C                   exsr      FLDAGG
038300080701    >C                   EXSR      RIEANA_DSF
038400080701     c                   update    TNTBE000                             59
038500030114    >C   59              SETON                                          99
038600030204      *
038700030114    >C     xtaopz        WHENEQ    '04'
038800080701     C                   move      'DSF'         TBEcod
038900080701     C     KTBL          chain     TNTBE01L
039000080701     c                   move      'A'           TBEatb
039100030115     c                   if        $tasto = F23
039200080701     c                   delete    TNTBE000                             59
039300030115     c                   else
039400080701     c                   update    TNTBE000                             59
039500030115     c                   end
039600030114    >C   59              SETON                                          99
039700940621     C*
039800940207     C                   ENDSL
039900940203     C*
040000940203     C                   ENDSR
040100940224     C************************************************************
040200940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
040300940224     C************************************************************
040400080701     C     RIEANA_DSF    BEGSR
040500030114     C*
040600080701     C                   CLEAR                   tbeUNI
040700030114     C* descrizione
040800080701     c                   movel     d1DES         D§DSFDES
040900030114     C*
041000080701     C                   MOVEL     EXTDDSF       TBEUNI
041100030114     C*
041200940207     C                   ENDSR
041300940224     C************************************************************
041400940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
041500940224     C************************************************************
041600940224     C     GESAGG        BEGSR
041700940224     C*
041800940224     C* segnalo al pgm chiamante l'avvenuta conferma
041900030114    >C                   MOVE      *ON           xtaopr
042000940224     C* segnalo al pgm chiamante l'aver premuto l'F6
042100030114    >C                   MOVE      '0'           xtaret
042200940224     C*
0423009402241    C                   SELECT
042400940224     C* nel caso di immissione, ripristino la videata iniziale
042500030114    >C     xtaopz        WHENEQ    '01'
042600940224     C                   MOVE      *ON           $INZD1
042700030114     C                   MOVE      *ON           $FINE
042800940224     C* altrimenti ritorno al pgm chiamante
042900940224     C                   OTHER
043000940224     C                   MOVE      *ON           $FINE
0431009402241-   C                   ENDSL
043200940224     C*
043300940224     C                   ENDSR
043400940131     C************************************************************
043500940131     C* OPERAZIONI INIZIALI
043600940131     C************************************************************
043700940131     C     *INZSR        BEGSR
043800940131     C*
043900940127     C* Reperimento parametri
044000940127     C*
044100940117     C     *ENTRY        PLIST
044200940117     C                   PARM                    KPJBA
044300030113     C                   movel     kpjbu         tabds
044400030115     c                   setoff                                       08
044500030115     C*
044600940223     C* Variabili per gestione videate
044700940223     C                   MOVE      'D1'          $GEST
044800940223     C                   MOVE      *OFF          $FINE
044900940223     C                   MOVE      *ON           $INZD1
045000940224     C                   MOVE      *BLANK        $LASTG
045100940224     C                   MOVE      *BLANK        $LASTV
045200940506     C                   Z-ADD     0             $ULKD1            3 0
045300940127     C*
045400940223     C* Variabili appoggio
045500940224     C                   Z-ADD     0             CURR
045600940224     C                   Z-ADD     0             CURC
045700940207     C*
045800940207     C* Valorizzazione campi univoci testate
045900940614     C                   CLEAR                   T1
046000940614     C                   MOVEL     KNSIF         NOMSIF
046100030113     C******             MOVEL     XSCDSI        NOMDIT
046200941027     C                   MOVEL     DSPGM         NOMPGM
046300030113     C*
046400080701     C                   move      'DSF'         TBEcod
046500080701     C                   movel     xtakey        TBEke1
046600940224     C*
046700940224     C* Inizializzazione/Decodifica prima videata
046800940224     C                   EXSR      INZD1
046900940224     C                   MOVE      *OFF          $INZD1
047000940127     C*
047100940117     C                   ENDSR
047200940128     C************************************************************
047300940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
047400940223     C************************************************************
047500940223     C     DEFCAM        BEGSR
047600940223     C*
047700940223     C* klist
047800030113     C     KTBL          KLIST
047900080701     C                   KFLD                    TBeCOD
048000080701     C                   KFLD                    TBeKE1
048100940207     C*
048200940223     C* Variabili per gestione videate
048300940223     C*
048400940223     C                   MOVE      *BLANK        $GEST             2
048500940223     C                   MOVE      *BLANK        $FINE             1
048600940223     C                   MOVE      *BLANK        $INZD1            1
048700940223     C                   MOVE      *BLANK        $LASTG            2
048800940223     C                   MOVE      *BLANK        $LASTV            2
048900940223     C*
049000940223     C* Variabili appoggio
049100940223     C                   Z-ADD     0             CURR              2 0
049200940223     C                   Z-ADD     0             CURC              2 0
049300940223     C                   MOVE      *ZEROS        WIN              99
049400940224     C                   MOVE      *BLANK        WTBERR            1
049500940223     C*
049600940223     C* Indici
049700940223     C                   Z-ADD     0             X                 3 0
049800940223     C                   Z-ADD     0             Y                 3 0
049900940127     C*
050000940117     C                   ENDSR
050100940128     C************************************************************
