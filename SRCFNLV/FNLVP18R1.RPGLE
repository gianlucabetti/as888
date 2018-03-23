000100141127      *
000200970214     H DECEDIT('0,') DATEDIT(*DMY.)
000300030729      *
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
002400170705     Fazorg01L  IF   E           K DISK
002500170705$001 FFNLVP18D1 CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700170705     FTFNLVP5L  UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900030113      *-------------
003000940127     D* Passaggio Parametri
003100940127     D KPJBA         E DS
003200030113      *-------------
003300030113     D* Parametri in ricezione
003400030113     D  TABDS          DS
003500030729     D  XTAOPZ                 1      1
003600030729     D  XTARET                 2      2
003700030729     D  XTAOPR                 3      3
003800030729     D  XTAERR                 4      4
003900030729     D  XTAFIL                10     12  0
004000030729     D  XTADDA                13     20  0
004100030729     D  XTAADA                21     28  0
004200940201     D*-------------
004300030204     D  S_TASTO        S                   like($Tasto)
004400940201     D DSFMT           DS
004500940506     D  $TASTO               369    369
004600940201     D  NRG                  370    370
004700940201     D  NCL                  371    371
004800940201     D*-------------
004900940201     D* posizione cursore
005000940201     D CURSOR          DS
005100940223     D  RIRI                   1      2B 0 INZ
005200940201     D  R$$                    2      2
005300940223     D  COCO                   3      4B 0 INZ
005400940201     D  C$$                    4      4
005500030728      *
005600030728     Ddataiso          s               d   datfmt(*iso)
005700030728     Ddataeur          s               d   datfmt(*eur)
005800940207     D*-------------
005900940207     D* Reperimento nome PGM
006000940207     D STATUS         SDS           333
006100940207     D  DSPGM            *PROC
006200940225     D*-------------
006300940225     D* COSTANTI
006400940225     D*-------------
006500940506     D* Tasti di funzione
006600940506     D F01             C                   CONST(X'31')
006700940506     D F02             C                   CONST(X'32')
006800940506     D F03             C                   CONST(X'33')
006900940506     D F04             C                   CONST(X'34')
007000940506     D F05             C                   CONST(X'35')
007100940506     D F06             C                   CONST(X'36')
007200940506     D F07             C                   CONST(X'37')
007300940506     D F08             C                   CONST(X'38')
007400940506     D F09             C                   CONST(X'39')
007500940506     D F10             C                   CONST(X'3A')
007600940506     D F11             C                   CONST(X'3B')
007700940506     D F12             C                   CONST(X'3C')
007800940506     D F13             C                   CONST(X'B1')
007900940506     D F14             C                   CONST(X'B2')
008000940506     D F15             C                   CONST(X'B3')
008100940506     D F16             C                   CONST(X'B4')
008200940506     D F17             C                   CONST(X'B5')
008300940506     D F18             C                   CONST(X'B6')
008400940506     D F19             C                   CONST(X'B7')
008500940506     D F20             C                   CONST(X'B8')
008600940506     D F21             C                   CONST(X'B9')
008700940506     D F22             C                   CONST(X'BA')
008800940506     D F23             C                   CONST(X'BB')
008900940506     D F24             C                   CONST(X'BC')
009000940506     D ENTER           C                   CONST(X'F1')
009100940506     D ROLDWN          C                   CONST(X'F4')
009200940506     D ROLLUP          C                   CONST(X'F5')
009300940506     D*-------------
009400941108     D DATA            C                   CONST('0001-01-01')
009500030114     D Annull          C                   CONST('  ANNULLATO  ')
009600030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009700030114     D OPz_Var         C                   CONST('VARIAZIONE')
009800030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009900030114     D OPz_Vis         C                   CONST('VISUALIZZA')
010000940127     C*----------------------------------------------------*
010100940127     C*                MAIN LINE PROGRAM
010200940127     C*----------------------------------------------------*
010300940223     C*
010400940223     C     $FINE         DOWEQ     *OFF
010500940202     C     $GEST         CASEQ     'D1'          GESD1
010600940117     C                   END
010700940117     C                   END
010800940325     C* fine programma
010900030113     c                   movel     tabds         kpjbu
011000030113     C                   Seton                                        LR
011100940131     C************************************************************
011200940510     C* GESTIONE VIDEO RECORD D1
011300940131     C************************************************************
011400940127     C     GESD1         BEGSR
011500030113      *
011600940225     C* inizializzazione videata
011700940223     C     $INZD1        IFEQ      *ON
011800940127     C                   EXSR      INZD1
011900940223     C                   MOVE      *OFF          $INZD1
012000940117     C                   END
012100030113      *
012200940225     C* emissione piede videata se proveniente da altra
012300940225     C* salvataggio valore formato di provenienza
012400940207     C     $LASTV        IFNE      'D1'
012500940614     C                   WRITE     T1
012600940202     C                   MOVE      $LASTV        $LASTG
012700940202     C                   MOVE      'D1'          $LASTV
012800940202     C                   END
012900940131     C*
013000940203     C                   SELECT
013100940203     C* Annullamento
013200030114     C* Visualizzazione
013300030729    >C     xtaopz        WHENEQ    '1'
013400030114     c                   movel     Opz_Imm       D1Fun
013500030729    >C     xtaopz        WHENEQ    '5'
013600030114     c                   movel     Opz_Vis       D1Fun
013700030114     C                   SETON                                        020306
013800030729    >C     xtaopz        WHENEQ    '4'
013900030114     c                   movel     Opz_Del       D1Fun
014000030114     C                   SETON                                        0203
014100030114     C                   SETOFF                                         06
014200940620     C* Manutenzione
014300030729    >C     xtaopz        WHENEQ    '2'
014400030114     c                   movel     Opz_Var       D1Fun
014500030114     C                   SETON                                        02
014600030114     C                   SETOFF                                       0306
014700940620     C*
014800940620     C                   OTHER
014900940620     C*
015000030114     C                   SETOFF                                       020306
015100940203     C                   ENDSL
015200030113     C*
015300030728     c     video1        tag
015400030114     c                   SETOFF                                         99
015500940620     C* Immissione/Modifica/Copia
015600030113     C*               *----------------*
015700940620     C                   EXFMT     D1
015800030113     C*               *----------------*
015900030114     C*
016000030114     C     *IN99         IFEQ      *OFF
016100030114     C*
0162000301141    C                   SELECT
016300030114     C* F3=Fine
016400030114     C     $TASTO        WHENEQ    F03
016500030114     C                   EXSR      F03D1
016600030114     C* F12=Ritorno
016700030114     C     $TASTO        WHENEQ    F12
016800030114     C                   EXSR      F12D1
016900030114     C*
0170000301141O   C                   OTHER
017100030114     C*
017200030114     C                   EXSR      CTRD1
017300030728     c   99              goto      video1
017400030113     C*
017500940616     C     *IN99         IFEQ      *OFF
017600940616     C* F6
0177000302052    C     $TASTO        ifeq      F06
0178000301152    C     $TASTO        oreq      F23
017900940224     C* eseguo aggiornamento
018000940315    >C                   EXSR      AGGANA
018100940224     C* eseguo operazioni del dopo-aggiornamento
018200030114     C  N99              EXSR      GESAGG
018300030205     c                   end
018400030114     C*
018500030114     C                   ENDIF
018600030114     C*
0187000301142-   C                   ENDSL
018800030114     C*
0189000301142-   C                   ENDIF
019000940224     C*
019100940117     C                   ENDSR
019200940117      ************************************************************
019300940207      * INIZIALIZZAZIONE VIDEATA DATI
019400940117      ************************************************************
019500940127     C     INZD1         BEGSR
019600940131     C*
019700940615     C                   CLEAR                   D1
019800170705     C                   MOVEl     xtaFIL        D1FIL
019900940207     C*
020000940224     C* valorizzo la chiave univoca per reperire i dati
020100940224     C* se non immissione
0202000307291   >C     xtaopz        IFNE      '1'
020300030204      *
020400940315    >C*-------------------------------------------------------------
020500940315    >C* impostare i campi chiave ricevuti
020600940315    >C*-------------------------------------------------------------
020700170705     C     K05l          chain(N)  TFNLVP5L
020800940614     C*
020900170705     C                   IF        %Found(TFNLVP5L)
021000170705     c                   eval        d1fil =  VP5FGS
021100170705     c                   eval        h1dda =  VP5DIMM
021200170705     c                   movel     VP5DIMM       dataiso
021300030728     c                   movel     dataiso       dataeur
021400030729     c                   movel     dataeur       d1dda
021500170705     c     VP5FGS        chain     azorg01l
021600170705     c                   if        %found(azorg01l)
021700170705     c                   movel     orgDES        d1DES
021800170705     c                   end
021900030113      *
022000030729     C* richiamo routine dei ctrl per decodificarla
022100940614    >C                   EXSR      CTRD1
022200030113      *
022300960305    >C     *IN99         IFEQ      *ON
022400960305    >C                   MOVEL     *IN99         SAV99             1
022500960305    >C                   ENDIF
022600030113      *
022700940224     C                   MOVE      *ALL'0'       IN5098
022800940224     C                   MOVEA     IN5098        *IN(50)
022900030113      *
023000940224     C* definizione indicatori su video
023100940224     C                   SETOFF                                       02
0232009402243    C                   SELECT
023300030729    >C     xtaopz        WHENEQ    '2'
023400940224     C                   SETON                                        02
023500030729    >C     xtaopz        WHENEQ    '5'
0236009402243-   C                   ENDSL
023700940224     C*
0238009402242-   C                   ENDIF
023900030114     C*
0240000301141   >C                   Else
024100030114     C* Se in Immissione nuovo record
024200030728     c                   clear                   d1
024300940223     C*
0244009402241-   C                   ENDIF
024500940224     C*
024600940117     C                   ENDSR
024700940309     C************************************************************
024800940309     C* GESTIONE F03 VIDEO D1
024900940309     C************************************************************
025000940309     C     F03D1         BEGSR
025100940309     C*
025200940309     C                   MOVE      *ON           $FINE
025300030113    >C                   MOVE      '1'           xtaret
025400940325     C*
025500940325     C                   ENDSR
025600940309     C************************************************************
025700940309     C* GESTIONE F12 VIDEO D1
025800940309     C************************************************************
025900940309     C     F12D1         BEGSR
026000940309     C*
026100940309     C                   MOVE      *ON           $FINE
026200940309     C*
026300940309     C                   ENDSR
026400940131     C************************************************************
026500940207     C* CONTROLLO VIDEATA
026600940131     C************************************************************
026700940127     C     CTRD1         BEGSR
026800940131     C*
026900940127     C                   SETOFF                                       99
027000030123      *
027100940127     C* non imposto pos. del cursore
027200940127     C                   Z-ADD     0             H1RIGA
027300940127     C                   Z-ADD     0             H1COLO
027400940621     C*
027500940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0276000307291   >C     xtaopz        IFNE      '4'
027700030728     c* data decorrenza
027800030729     c                   if        d1dda  <> 0
027900030729     c     *eur          test(d)                 d1dda                  50
028000030728     c                   if        *in50
028100030729     c     *dmy          test(d)                 d1dda                  50
028200030729     c  n50*dmy          move      d1dda         dataeur
028300030729     c  n50              move      dataeur       d1dda
028400030728     c  n50              move      dataeur       dataiso
028500030729     c  n50              move      dataiso       h1dda
028600030728     c                   else
028700030729     c                   move      d1dda         dataeur
028800030729     c                   move      dataeur       d1dda
028900030728     c                   move      dataeur       dataiso
029000030729     c                   move      dataiso       h1dda
029100030728     c                   end
029200030728     c                   else
029300030728     c                   seton                                        50
029400030728     c                   end
029500030728     c   50              seton                                        99
029600030728     c*
029700030728     c* Codice Voce vuoto
029800170705     c                   if        d1fil =  *zeros
029900030728     c                   seton                                        5499
030000030728     c                   end
030100030728     c*
030200940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
030300030729    >C     xtaopz        IFEQ      '1'
030400030729    >C     xtaopz        oreq      '3'
030500030728     C*
030600030728     c* controllo di non sovrapposizione periodi
030700170705     C     K05           setll     TFNLVP5l
030800170705     C     K05           reade     TFNLVP5l
030900170705     c                   dow       not %eof(TFNLVP5l)
031000030728     c                   seton                                        5599
031100030728     c                   leave
031200170705     C     K05           reade     TFNLVP5l
031300030728     c                   end
031400030114     C                   ENDIF
031500030114     C*
031600030114     C* controllo codice vuoto
031700170705$017 C     D1FIL         IFEQ      *zeros
031800030728     C                   SETON                                        5499
031900170718     C                   Else
032000170718$017 C     D1FIL         chain     azorg01l
032100170718     c                   if        %found(azorg01l)
032200170718     c                   movel     orgDES        d1DES
032300170718     c                   end
032400170718      *
032500170718     C                   End
032600030114      *
0327009402241-   C                   END
032800940131     C*
032900940302     C     *IN99         IFEQ      *ON
033000940510     C     $GEST         ANDEQ     'D1'
033100940117     C                   MOVEA     *IN           WIN
033200940128     C                   MOVE      *ALL'0'       IN5098           49
033300940128     C                   MOVEA     IN5098        *IN(50)
033400940614     C                   WRITE     D1
033500940117     C                   MOVEA     WIN           *IN
033600940302     C                   ENDIF
033700940127     C*
033800940117     C                   ENDSR
033900940207     C/EJECT
034000940224     C************************************************************
034100940315    >C* AGGIORNAMENTO ANAGRAFICA
034200940224     C************************************************************
034300940315    >C     AGGANA        BEGSR
034400940207     C*
034500940207     C                   SELECT
034600030204     C*
034700030729    >C     xtaopz        WHENEQ    '1'
034800030729    >C     xtaopz        OREQ      '3'
034900030204     C*
035000030728    >C                   EXSR      RIEANA
035100170705     c                   write     TFNLVP5                              59
035200940318     C* se rcd non scrivibile attivo errore
035300030114    >C   59              SETON                                          99
035400030204      *
035500030729    >C     xtaopz        WHENEQ    '2'
035600030204      *
035700170705     C     K05l          chain     TFNLVP5L
035800030728    >C                   EXSR      RIEANA
035900170705     c                   update    TFNLVP5                              59
036000030114    >C   59              SETON                                          99
036100030204      *
036200030729    >C     xtaopz        WHENEQ    '4'
036300170705     C     K05l          chain     TFNLVP5L
036400170705     c                   delete    TFNLVP5                              59
036500030114    >C   59              SETON                                          99
036600030204      *
036700940207     C                   ENDSL
036800940203     C*
036900940203     C                   ENDSR
037000940224     C************************************************************
037100940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
037200940224     C************************************************************
037300030728     C     RIEANA        BEGSR
037400030114     C*
037500170705     c                   movel     d1FIL         VP5FGS
037600030728     c                   movel     d1DDA         dataeur
037700030728     c                   movel     dataeur       dataiso
037800170705     c                   movel     dataiso       VP5DIMM
037900030114     C*
038000940207     C                   ENDSR
038100940224     C************************************************************
038200940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
038300940224     C************************************************************
038400940224     C     GESAGG        BEGSR
038500940224     C*
038600940224     C* segnalo al pgm chiamante l'avvenuta conferma
038700030114    >C                   MOVE      *ON           xtaopr
038800940224     C* segnalo al pgm chiamante l'aver premuto l'F6
038900030114    >C                   MOVE      '0'           xtaret
039000940224     C*
0391009402241    C                   SELECT
039200940224     C* nel caso di immissione, ripristino la videata iniziale
039300030729    >C     xtaopz        WHENEQ    '1'
039400940224     C                   MOVE      *ON           $INZD1
039500030114     C                   MOVE      *ON           $FINE
039600940224     C* altrimenti ritorno al pgm chiamante
039700940224     C                   OTHER
039800940224     C                   MOVE      *ON           $FINE
0399009402241-   C                   ENDSL
040000940224     C*
040100940224     C                   ENDSR
040200940131     C************************************************************
040300940131     C* OPERAZIONI INIZIALI
040400940131     C************************************************************
040500940131     C     *INZSR        BEGSR
040600940131     C*
040700940127     C* Reperimento parametri
040800940127     C*
040900940117     C     *ENTRY        PLIST
041000940117     C                   PARM                    KPJBA
041100030113     C                   movel     kpjbu         tabds
041200030115     c                   setoff                                       08
041300030115     C*
041400940223     C* Variabili per gestione videate
041500940223     C                   MOVE      'D1'          $GEST
041600940223     C                   MOVE      *OFF          $FINE
041700940223     C                   MOVE      *ON           $INZD1
041800940224     C                   MOVE      *BLANK        $LASTG
041900940224     C                   MOVE      *BLANK        $LASTV
042000940506     C                   Z-ADD     0             $ULKD1            3 0
042100940127     C*
042200940223     C* Variabili appoggio
042300940224     C                   Z-ADD     0             CURR
042400940224     C                   Z-ADD     0             CURC
042500940207     C*
042600940207     C* Valorizzazione campi univoci testate
042700940614     C                   CLEAR                   T1
042800940614     C                   MOVEL     KNSIF         NOMSIF
042900941027     C                   MOVEL     DSPGM         NOMPGM
043000030113     C*
043100170705     C                   z-add     XTAFIL        VP5FGS
043200170705     c                   z-add     XTADDA        VP5DIMM
043300940224     C*
043400940224     C* Inizializzazione/Decodifica prima videata
043500940224     C                   EXSR      INZD1
043600940224     C                   MOVE      *OFF          $INZD1
043700940127     C*
043800940117     C                   ENDSR
043900940128     C************************************************************
044000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
044100030729     c************************************************************
044200940223     C     DEFCAM        BEGSR
044300940223     C*
044400940223     C* klist
044500170705     C     K05l          KLIST
044600030728     C                   KFLD                    XTAFIL
044700030728      *
044800170705     C     K05           KLIST
044900030729     C                   KFLD                    D1FIL
045000940207     C*
045100940223     C* Variabili per gestione videate
045200940223     C*
045300940223     C                   MOVE      *BLANK        $GEST             2
045400940223     C                   MOVE      *BLANK        $FINE             1
045500940223     C                   MOVE      *BLANK        $INZD1            1
045600940223     C                   MOVE      *BLANK        $LASTG            2
045700940223     C                   MOVE      *BLANK        $LASTV            2
045800940223     C*
045900940223     C* Variabili appoggio
046000940223     C                   Z-ADD     0             CURR              2 0
046100940223     C                   Z-ADD     0             CURC              2 0
046200940223     C                   MOVE      *ZEROS        WIN              99
046300940224     C                   MOVE      *BLANK        WTBERR            1
046400940223     C*
046500940223     C* Indici
046600940223     C                   Z-ADD     0             X                 3 0
046700940223     C                   Z-ADD     0             Y                 3 0
046800940127     C*
046900940117     C                   ENDSR
047000940128     C************************************************************
