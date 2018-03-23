000100141127     H*
000200141127     H*
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400030729      *
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
002500050509$001 FTISE80D1  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700050511     FTise800F  UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900030113      *-------------
003000940127     D* Passaggio Parametri
003100940127     D KPJBA         E DS
003200050511     D*------------------
003300050511     D* DS REPERIMENTO DATI UTENTE
003400050511     D*-------------------
003500050511     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
003600050511     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
003700050511     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
003800050511     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
003900050511     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
004000030113      *-------------
004100030113     D* Parametri in ricezione
004200030113     D  TABDS          DS
004300030729     D  XTAOPZ                 1      1
004400030729     D  XTARET                 2      2
004500030729     D  XTAOPR                 3      3
004600030729     D  XTAERR                 4      4
004700050509     D  XTACOD                 5     11  0
004800940201     D*-------------
004900030204     D  S_TASTO        S                   like($Tasto)
005000940201     D DSFMT           DS
005100940506     D  $TASTO               369    369
005200940201     D  NRG                  370    370
005300940201     D  NCL                  371    371
005400940201     D*-------------
005500940201     D* posizione cursore
005600940201     D CURSOR          DS
005700940223     D  RIRI                   1      2B 0 INZ
005800940201     D  R$$                    2      2
005900940223     D  COCO                   3      4B 0 INZ
006000940201     D  C$$                    4      4
006100050511     D*------------------
006200050511     D* DS CONTROLLO DATA (8)
006300050511     D*------------------
006400050511     D WLBDA8          DS                  INZ
006500050511     D  G08DAT                 1      8  0
006600050511     D  G08INV                 9     16  0
006700050511     D  G08ERR                17     17
006800050511     D  G08TGI                18     22  0
006900050511     D*------------------
007000050511     D* DS "TIBS10R"
007100050511     D*------------------
007200050511     D BS10DS        E DS                  EXTNAME(TIBS10DS)
007300030728      *
007400030728     Ddataiso          s               d   datfmt(*iso)
007500030728     Ddataeur          s               d   datfmt(*eur)
007600940207     D*-------------
007700940207     D* Reperimento nome PGM
007800940207     D STATUS         SDS           333
007900940207     D  DSPGM            *PROC
008000940225     D*-------------
008100940225     D* COSTANTI
008200940225     D*-------------
008300940506     D* Tasti di funzione
008400940506     D F01             C                   CONST(X'31')
008500940506     D F02             C                   CONST(X'32')
008600940506     D F03             C                   CONST(X'33')
008700940506     D F04             C                   CONST(X'34')
008800940506     D F05             C                   CONST(X'35')
008900940506     D F06             C                   CONST(X'36')
009000940506     D F07             C                   CONST(X'37')
009100940506     D F08             C                   CONST(X'38')
009200940506     D F09             C                   CONST(X'39')
009300940506     D F10             C                   CONST(X'3A')
009400940506     D F11             C                   CONST(X'3B')
009500940506     D F12             C                   CONST(X'3C')
009600940506     D F13             C                   CONST(X'B1')
009700940506     D F14             C                   CONST(X'B2')
009800940506     D F15             C                   CONST(X'B3')
009900940506     D F16             C                   CONST(X'B4')
010000940506     D F17             C                   CONST(X'B5')
010100940506     D F18             C                   CONST(X'B6')
010200940506     D F19             C                   CONST(X'B7')
010300940506     D F20             C                   CONST(X'B8')
010400940506     D F21             C                   CONST(X'B9')
010500940506     D F22             C                   CONST(X'BA')
010600940506     D F23             C                   CONST(X'BB')
010700940506     D F24             C                   CONST(X'BC')
010800940506     D ENTER           C                   CONST(X'F1')
010900940506     D ROLDWN          C                   CONST(X'F4')
011000940506     D ROLLUP          C                   CONST(X'F5')
011100940506     D*-------------
011200941108     D DATA            C                   CONST('0001-01-01')
011300030114     D Annull          C                   CONST('  ANNULLATO  ')
011400030114     D OPz_Imm         C                   CONST('IMMISSIONE')
011500030114     D OPz_Var         C                   CONST('VARIAZIONE')
011600030114     D OPz_Del         C                   CONST(' ANNULLA  ')
011700030114     D OPz_Vis         C                   CONST('VISUALIZZA')
011800050511     D*-------------
011900050511     D MsgAlert1       C                   CONST('Non presente in Tab.-
012000050511     D                                     Unificanti')
012100940127     C*----------------------------------------------------*
012200940127     C*                MAIN LINE PROGRAM
012300940127     C*----------------------------------------------------*
012400940223     C     $FINE         DOWEQ     *OFF
012500940202     C     $GEST         CASEQ     'D1'          GESD1
012600940117     C                   END
012700940117     C                   END
012800940325     C* fine programma
012900030113     c                   movel     tabds         kpjbu
013000030113     C                   Seton                                        LR
013100940131     C************************************************************
013200940510     C* GESTIONE VIDEO RECORD D1
013300940131     C************************************************************
013400940127     C     GESD1         BEGSR
013500030113      *
013600940225     C* inizializzazione videata
013700940223     C     $INZD1        IFEQ      *ON
013800940127     C                   EXSR      INZD1
013900940223     C                   MOVE      *OFF          $INZD1
014000940117     C                   END
014100030113      *
014200940225     C* emissione piede videata se proveniente da altra
014300940225     C* salvataggio valore formato di provenienza
014400940207     C     $LASTV        IFNE      'D1'
014500940614     C                   WRITE     T1
014600940202     C                   MOVE      $LASTV        $LASTG
014700940202     C                   MOVE      'D1'          $LASTV
014800940202     C                   END
014900940131     C*
015000940203     C                   SELECT
015100940203     C* Annullamento
015200030114     C* Visualizzazione
015300030729    >C     xtaopz        WHENEQ    '1'
015400030114     c                   movel     Opz_Imm       D1Fun
015500030729    >C     xtaopz        WHENEQ    '5'
015600030114     c                   movel     Opz_Vis       D1Fun
015700030114     C                   SETON                                        020306
015800030729    >C     xtaopz        WHENEQ    '4'
015900030114     c                   movel     Opz_Del       D1Fun
016000030114     C                   SETON                                        0203
016100030114     C                   SETOFF                                         06
016200940620     C* Manutenzione
016300030729    >C     xtaopz        WHENEQ    '2'
016400030114     c                   movel     Opz_Var       D1Fun
016500030114     C                   SETON                                        02
016600030114     C                   SETOFF                                       0306
016700940620     C*
016800940620     C                   OTHER
016900940620     C*
017000030114     C                   SETOFF                                       020306
017100940203     C                   ENDSL
017200030113     C*
017300030728     c     video1        tag
017400030114     c                   SETOFF                                         99
017500940620     C* Immissione/Modifica/Copia
017600030113     C*               *----------------*
017700940620     C                   EXFMT     D1
017800030113     C*               *----------------*
017900030114     C*
018000030114     C     *IN99         IFEQ      *OFF
018100030114     C*
0182000301141    C                   SELECT
018300030114     C* F3=Fine
018400030114     C     $TASTO        WHENEQ    F03
018500030114     C                   EXSR      F03D1
018600030114     C* F12=Ritorno
018700030114     C     $TASTO        WHENEQ    F12
018800030114     C                   EXSR      F12D1
018900030114     C*
0190000301141O   C                   OTHER
019100030114     C*
019200030114     C                   EXSR      CTRD1
019300030728     c   99              goto      video1
019400030113     C*
019500940616     C     *IN99         IFEQ      *OFF
019600940616     C* F6
0197000302052    C     $TASTO        ifeq      F06
0198000301152    C     $TASTO        oreq      F23
019900940224     C* eseguo aggiornamento
020000940315    >C                   EXSR      AGGANA
020100940224     C* eseguo operazioni del dopo-aggiornamento
020200030114     C  N99              EXSR      GESAGG
020300030205     c                   end
020400030114     C*
020500030114     C                   ENDIF
020600030114     C*
0207000301142-   C                   ENDSL
020800030114     C*
0209000301142-   C                   ENDIF
021000940224     C*
021100940117     C                   ENDSR
021200940117      ************************************************************
021300940207      * INIZIALIZZAZIONE VIDEATA DATI
021400940117      ************************************************************
021500940127     C     INZD1         BEGSR
021600940131     C*
021700940615     C                   CLEAR                   D1
021800030729     C                   MOVEl     xtaCOD        D1COD
021900940207     C*
022000940224     C* valorizzo la chiave univoca per reperire i dati
022100940224     C* se non immissione
0222000307291   >C     xtaopz        IFNE      '1'
022300030204      *
022400940315    >C*-------------------------------------------------------------
022500940315    >C* impostare i campi chiave ricevuti
022600940315    >C*-------------------------------------------------------------
022700050511     C     Kse8          chain(N)  Tise800F
022800940614     C*
022900050511     C                   IF        %Found(Tise800f)
023000050509     c                   eval        d1cod =  se8COD
023100050509     c                   eval        d1des =  se8DES
023200050509     c                   eval        d1uni =  se8UNI
023300050511     c                   eval        d1clv =  se8CLV
023400050513     c                   if        xtaOPZ = '2'
023500030115     c                   seton                                        08
023600050513     C                   MOVEl     OPz_Var       D1Ann
023700030114     C                   end
023800030113      *
023900030729     C* richiamo routine dei ctrl per decodificarla
024000940614    >C                   EXSR      CTRD1
024100030113      *
024200960305    >C     *IN99         IFEQ      *ON
024300960305    >C                   MOVEL     *IN99         SAV99             1
024400960305    >C                   ENDIF
024500030113      *
024600940224     C                   MOVE      *ALL'0'       IN5098
024700940224     C                   MOVEA     IN5098        *IN(50)
024800030113      *
024900940224     C* definizione indicatori su video
025000940224     C                   SETOFF                                       02
0251009402243    C                   SELECT
025200030729    >C     xtaopz        WHENEQ    '2'
025300940224     C                   SETON                                        02
025400030729    >C     xtaopz        WHENEQ    '5'
0255009402243-   C                   ENDSL
025600940224     C*
0257009402242-   C                   ENDIF
025800030114     C*
0259000301141   >C                   Else
026000030114     C* Se in Immissione nuovo record
026100030728     c                   clear                   d1
026200940223     C*
0263009402241-   C                   ENDIF
026400940224     C*
026500940117     C                   ENDSR
026600940309     C************************************************************
026700940309     C* GESTIONE F03 VIDEO D1
026800940309     C************************************************************
026900940309     C     F03D1         BEGSR
027000940309     C*
027100940309     C                   MOVE      *ON           $FINE
027200030113    >C                   MOVE      '1'           xtaret
027300940325     C*
027400940325     C                   ENDSR
027500940309     C************************************************************
027600940309     C* GESTIONE F12 VIDEO D1
027700940309     C************************************************************
027800940309     C     F12D1         BEGSR
027900940309     C*
028000940309     C                   MOVE      *ON           $FINE
028100940309     C*
028200940309     C                   ENDSR
028300940131     C************************************************************
028400940207     C* CONTROLLO VIDEATA
028500940131     C************************************************************
028600940127     C     CTRD1         BEGSR
028700940131     C*
028800940127     C                   SETOFF                                       99
028900030123      *
029000940127     C* non imposto pos. del cursore
029100940127     C                   Z-ADD     0             H1RIGA
029200940127     C                   Z-ADD     0             H1COLO
029300940621     C*
029400940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0295000307291   >C     xtaopz        IFNE      '4'
029600030728     c*
029700030728     c* Codice Voce vuoto
029800050509     c                   if        d1cod =  *zeros
029900030728     c                   seton                                        5499
030000050511     c                   else
030100050511      **
030200050511     C                   CLEAR                   BS69DS
030300050511     C                   CLEAR                   ACODS
030400050511     C                   MOVEL     KNSIF         I69SIF
030500050511     C                   MOVEL     d1cod         I69KAC
030600050511     C                   MOVEL     d1cod         I69KCP
030700050511     C                   CALL      'TIBS69R'
030800050511     C                   PARM                    BS69DS
030900050511     C                   PARM                    ACODS
031000050511     C                   PARM                    INDDS
031100050511     C                   PARM                    CLPDS
031200050511     C                   PARM                    CLSDS
031300050511IF  2C     O69ERR        IFeq      '1'
031400050511     c                   seton                                        5499
031500050511E   2C                   ElsE
031600050511     c* Descrizione
031700050511     c                   movel     acorag        d1des
031800050511     c                   movel     clpclv        d1clv
031900050511     c*
032000050511     C                   CLEAR                   BS10DS
032100050511     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
032200050511     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
032300050511     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
032400050511     C                   MOVE      d1cod         D10COD
032500050511     C                   CALL      'TIBS10R'
032600050511     C                   PARM                    BS10DS
032700050511IF  3C     D10ERR        IFne      *BLANKS
032800050511     C                   CLEAR                   BS10DS
032900050511     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
033000050511     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
033100050511     C                   MOVEL     'F'           D10PAF                         *CERCA PADRE
033200050511     C                   MOVE      d1cod         D10COD
033300050511     C                   CALL      'TIBS10R'
033400050511     C                   PARM                    BS10DS
033500050511     c                   enD
033600050511IF  3C     D10ERR        IFne      *BLANKS
033700050511     c                   evalr     d1dpa = MsgAlert1
033800050511     c                   else
033900050511     c*
034000050511     C     D10COD        ifeq      D10COP
034100050511     c                   MOVE      'S'           d1Uni
034200050511E   3C                   end
034300050511     c                   z-add     d10cop        d1pad
034400050511      **
034500050511     C                   CLEAR                   BS69DS
034600050511     C                   CLEAR                   ACODS
034700050511     C                   MOVEL     KNSIF         I69SIF
034800050511     C                   MOVEL     d1pad         I69KAC
034900050511     C                   CALL      'TIBS69R'
035000050511     C                   PARM                    BS69DS
035100050511     C                   PARM                    ACODS
035200050511     C                   PARM                    INDDS
035300050511     C                   PARM                    CLPDS
035400050511     C                   PARM                    CLSDS
035500050511IF  2C     O69ERR        IFeq      '1'
035600050511     c                   seton                                        5499
035700050511E   2C                   ElsE
035800050511     c* Descrizione
035900050511     c                   z-add     0             lungo             5 0
036000050511     c                   eval      lungo = %scan('   ' : acorag)
036100050513     c                   if        lungo > 0
036200050511     c                   evalr     d1dpa = %Subst(acorag:1:(lungo-1))
036300050513     c                   endIF
036400050511     c                   endIF
036500050511     c*
036600050511E   3C                   ENDIF
036700050511E   3C                   end
036800050511     c                   endIF
036900030728     c*
037000940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
037100030729    >C     xtaopz        IFEQ      '1'
037200030729    >C     xtaopz        oreq      '3'
037300050509     C*
037400050509     C                   ENDIF
037500030114     C*
037600030114     C* controllo codice vuoto
037700050509$017 C     D1cod         IFEQ      *zeros
037800030728     C                   SETON                                        5499
037900030114     C                   End
038000030114      *
0381009402241-   C                   END
038200940131     C*
038300940302     C     *IN99         IFEQ      *ON
038400940510     C     $GEST         ANDEQ     'D1'
038500940117     C                   MOVEA     *IN           WIN
038600940128     C                   MOVE      *ALL'0'       IN5098           49
038700940128     C                   MOVEA     IN5098        *IN(50)
038800940614     C                   WRITE     D1
038900940117     C                   MOVEA     WIN           *IN
039000940302     C                   ENDIF
039100940127     C*
039200940117     C                   ENDSR
039300940207     C/EJECT
039400940224     C************************************************************
039500940315    >C* AGGIORNAMENTO ANAGRAFICA
039600940224     C************************************************************
039700940315    >C     AGGANA        BEGSR
039800940207     C*
039900940207     C                   SELECT
040000030204     C*
040100030729    >C     xtaopz        WHENEQ    '1'
040200030729    >C     xtaopz        OREQ      '3'
040300030204     C*
040400050511     c     d1cod         chain     Tise800F
040500030728    >C                   EXSR      RIEANA
040600050511     c                   if        %Found(Tise800F)
040700050511     c                   update    Tise8000                             59
040800050511     c                   else
040900050509     c                   write     Tise8000                             59
041000050511     c                   end
041100940318     C* se rcd non scrivibile attivo errore
041200030114    >C   59              SETON                                          99
041300030204      *
041400030729    >C     xtaopz        WHENEQ    '2'
041500030204      *
041600050511     C     Kse8          chain     Tise800F
041700030728    >C                   EXSR      RIEANA
041800050509     c                   update    Tise8000                             59
041900030114    >C   59              SETON                                          99
042000030204      *
042100030729    >C     xtaopz        WHENEQ    '4'
042200050511     C     Kse8          chain     Tise800F
042300050511     c****               move      'A'           se8ANN
042400050511     c****               if        $tasto = F23
042500050509     c                   delete    Tise8000                             59
042600050511     c****               else
042700050511     c****               update    Tise8000                             59
042800050511     c****               end
042900030114    >C   59              SETON                                          99
043000030204      *
043100940207     C                   ENDSL
043200940203     C*
043300940203     C                   ENDSR
043400940224     C************************************************************
043500940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
043600940224     C************************************************************
043700030728     C     RIEANA        BEGSR
043800030114     C*
043900050509     c                   movel     d1DES         se8DES
044000050509     c                   movel     d1cod         se8COD
044100050509     c                   movel     d1UNI         se8UNI
044200050511     c                   movel     d1CLV         se8CLV
044300030114     C*
044400940207     C                   ENDSR
044500940224     C************************************************************
044600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
044700940224     C************************************************************
044800940224     C     GESAGG        BEGSR
044900940224     C*
045000940224     C* segnalo al pgm chiamante l'avvenuta conferma
045100030114    >C                   MOVE      *ON           xtaopr
045200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
045300030114    >C                   MOVE      '0'           xtaret
045400940224     C*
0455009402241    C                   SELECT
045600940224     C* nel caso di immissione, ripristino la videata iniziale
045700030729    >C     xtaopz        WHENEQ    '1'
045800940224     C                   MOVE      *ON           $INZD1
045900050511     C                   MOVE      *Off          $FINE
046000940224     C* altrimenti ritorno al pgm chiamante
046100940224     C                   OTHER
046200940224     C                   MOVE      *ON           $FINE
0463009402241-   C                   ENDSL
046400940224     C*
046500940224     C                   ENDSR
046600940131     C************************************************************
046700940131     C* OPERAZIONI INIZIALI
046800940131     C************************************************************
046900940131     C     *INZSR        BEGSR
047000940131     C*
047100940127     C* Reperimento parametri
047200940127     C*
047300940117     C     *ENTRY        PLIST
047400940117     C                   PARM                    KPJBA
047500030113     C                   movel     kpjbu         tabds
047600030115     c                   setoff                                       08
047700030115     C*
047800940223     C* Variabili per gestione videate
047900940223     C                   MOVE      'D1'          $GEST
048000940223     C                   MOVE      *OFF          $FINE
048100940223     C                   MOVE      *ON           $INZD1
048200940224     C                   MOVE      *BLANK        $LASTG
048300940224     C                   MOVE      *BLANK        $LASTV
048400940506     C                   Z-ADD     0             $ULKD1            3 0
048500940127     C*
048600940223     C* Variabili appoggio
048700940224     C                   Z-ADD     0             CURR
048800940224     C                   Z-ADD     0             CURC
048900940207     C*
049000940207     C* Valorizzazione campi univoci testate
049100940614     C                   CLEAR                   T1
049200940614     C                   MOVEL     KNSIF         NOMSIF
049300941027     C                   MOVEL     DSPGM         NOMPGM
049400030113     C*
049500050509     C                   movel     XTACOD        se8COD
049600940224     C*
049700940224     C* Inizializzazione/Decodifica prima videata
049800940224     C                   EXSR      INZD1
049900940224     C                   MOVE      *OFF          $INZD1
050000940127     C*
050100940117     C                   ENDSR
050200940128     C************************************************************
050300940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
050400030729     c************************************************************
050500940223     C     DEFCAM        BEGSR
050600940223     C*
050700940223     C* klist
050800050509     C     Kse8          KLIST
050900030728     C                   KFLD                    XTACOD
051000030728      *
051100050509     C     Ks8           KLIST
051200030729     C                   KFLD                    D1COD
051300940207     C*
051400940223     C* Variabili per gestione videate
051500940223     C*
051600940223     C                   MOVE      *BLANK        $GEST             2
051700940223     C                   MOVE      *BLANK        $FINE             1
051800940223     C                   MOVE      *BLANK        $INZD1            1
051900940223     C                   MOVE      *BLANK        $LASTG            2
052000940223     C                   MOVE      *BLANK        $LASTV            2
052100940223     C*
052200940223     C* Variabili appoggio
052300940223     C                   Z-ADD     0             CURR              2 0
052400940223     C                   Z-ADD     0             CURC              2 0
052500940223     C                   MOVE      *ZEROS        WIN              99
052600940224     C                   MOVE      *BLANK        WTBERR            1
052700940223     C*
052800940223     C* Indici
052900940223     C                   Z-ADD     0             X                 3 0
053000940223     C                   Z-ADD     0             Y                 3 0
053100050511     C*
053200050511     C* IMNPOSTA DATA / ORA CORRENTE
053300050511     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
053400050511     C                   MOVE      N14           G08DAT                         *DATA (8) IN G/M/AA
053500050511     C                   Z-ADD     *ZEROS        G08INV
053600050511     C                   MOVEL     '0'           G08ERR
053700050511     C                   CALL      'XSRDA8'
053800050511     C                   PARM                    WLBDA8
053900050511     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
054000940127     C*
054100940117     C                   ENDSR
054200940128     C************************************************************
