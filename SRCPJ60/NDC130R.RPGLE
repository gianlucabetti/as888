000100981216     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200981216     H*PARMS BNDDIR(PJXBND PJCBND)
000300990423     H*PARMS CVTOPT(*NONE)
000400940211     H DECEDIT('0,') DATEDIT(*DMY.)
000500941115      *
000600981218      *                 GESTIONE PARAMETRI STAMPA MASTRINI
000700950123      *          --------------------------------------------------
000800940224      *
000900940224      *
001000940224      * DOCUMENTI MICROANALISI :
001100940224      *
001200940224      * NOTE TECNICHE :
001300940224      *
001400940224      *
001500940224      * PERSONALIZZAZIONI
001600940224      *
001700940224      *
001800940224      *
001900940314      *               MAPPA INDICATORI
002000940224      *
002100981216      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002200981216      *
002300981216      *  11  protezione D1CTB Codice contabilità
002400981216      *  12  protezione D1UNI Codice unità
002500981216      *
002600981216      *  21           GENERICO OPERAZIONI I/O
002700981216      *               -lettura  ANESC01L
002800981216      *               -lettura  ANUNI01L
002900981216      *  22           GENERICO ERRORE OPERAZIONI I/O
003000981216      *
003100981216      *  30           SFLDSP
003200981216      * N31           SFLCLR
003300981216      *  31           SFLDSPCTL
003400981216      *  32           SFLNXTCHG
003500981216      *  33           SFLEND
003600981216      *  39           OF PRTF
003700981216      *
003800981216    > *  Specificare l'uso dei singoli indicatori
003900981216      *  40 <---> 49  DSPATR ERRORI SU SFL
004000981216      *
004100981216    > *  Specificare l'uso dei singoli indicatori
004200981216      *  50 <---> 98  ERRORI SU VIDEO
004300981216      *  50 unità inesistente o non abilitata
004400981216      *  51 data errata
004500981216      *  52 data errata
004600981216      *  53 data iniziale maggiore data finale
004700981216      *  54 valore non valido DET
004800981216      *  55 valore non valido NMO
004900981216      *  56 valore non valido SLM
005000981216      *  57 valore non valido SLP
005100981216      *  58 valore non valido CAU
005200981216      *  59 valore non valido SEO
005300981216      *  65
005400981216      *  66 descrizione obbligatoria
005500981216      *  67 contabilità errata
005600981216      *  68 utente non abilitato a contabilità
005700981216      *  69 utente non abilitato a TPR definitive
005800981216      *  70 valore non valido in   TPR definitive
005900981216      *  71 utente non abilitato a TPR provvisorie
006000981216      *  72 valore non valido in   TPR provvisorie
006100981216      *  73 utente non abilitato a TPR gestionali
006200981216      *  74 valore non valido in   TPR gestionali
006300981216      *
006400981216      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
006500981216      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
006600981216      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
006700981216      *  99           INDIC. GENERALE DI ERRORE
006800940128     F*----------------------------------------------------*
006900981216     FANESC01L  IF   E           K DISK
007000981216     FANUNI01L  IF   E           K DISK
007100981216     FNDC130D   CF   E             WORKSTN
007200981216     F                                     RENAME(C130D1:FMTD1)
007300981216     F                                     RENAME(C130T1:FMTT1)
007400981216     F                                     RENAME(C130Z1:FMTZ1)
007500981216    >F* eventuali schermi supplementari
007600981216     F                                     RENAME(C130D2:FMTD2)
007700981216     F                                     INFDS(DSFMT)
007800981216     D*----------------------------------------------------*
007900981216     D $COS            S             27    DIM(6) CTDATA PERRCD(1)              Costanti pgm
008000981216     D* Schiere per la gestione dei tasti di funzione
008100981216     D $FM             S             27    DIM(24)
008200981216     D $FX             S             50    DIM(24)
008300981216     D $FC             S              1    DIM(24)
008400981216     D $FR             S              1    DIM(24)
008500981216     D* Video D1/D2
008600981216     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
008700981216     D $FX1            S             50    DIM(24) ALT($FM1)
008800981216     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008900981216     D $FR1            S              1    DIM(24) ALT($FC1)
009000981216     D*----------------------------------------------------*
009100981216     D*-------------
009200981216     D* Passaggio Parametri
009300981216     D KPJBA         E DS
009400981216     D*-------------
009500981216     D* DS Esterna gestione messaggi per COMIT
009600981216     D XMSGDS        E DS
009700981216     D  STS          E                     EXTFLD(MSGSTS)
009800981216     D                                     DIM(333)
009900981216     D  JBA          E                     EXTFLD(MSGJBA)
010000981216     D                                     DIM(502)
010100981216     D*-------------
010200981216     D* Reperimento Autorizzazioni
010300981216     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
010400981216     D*-------------
010500981216     D* Dati per pgm XNOAUT
010600981216     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
010700981216     D*-------------
010800981216     D* Passaggio Autorizzazioni
010900981216     D DSAUT           DS           512
011000981216     D*-------------
011100981216     D* Maschera dati autorizzazioni
011200981216    >DGCIVDS         E DS                  EXTNAME(CONCIVDS)
011300981216     D*-------------
011400981217     D* Tabella contabilità
011500981216     DTABG84         E DS                  EXTNAME(ANGG84DS)
011600981216     D*-------------
011700981216     D* Controllo autorizzazione unità
011800981216     DXAUTUNIDS      E DS
011900981216     D*-------------
012000981216     D* Controllo autorizzazione profilo/contabilità
012100981216     DXAUTCTBDS      E DS
012200981216     D*-------------
012300981216     D* Valori assunti
012400981216     D XMPEDS        E DS                  INZ
012500981216     D XMPEDS1       E DS                  INZ
012600981216     D DSMPE           DS                  INZ
012700981216     D*-------------
012800981216     D* Reperimento nome PGM
012900981216     D STATUS         SDS           333
013000981216     D  DSPGM            *PROC
013100981216     D  PARMS            *PARMS
013200981216     D*-------------
013300981216     D* Richiamo a XATB
013400981216     D XATBDS        E DS
013500981216     D*-------------
013600981216     D* Parametri per ricerca codice ANATB00F
013700981216     D XTABDS        E DS
013800981216     D*-------------
013900981216     D* Dati di ambiente ottenuti da XSOC
014000981216    >D SOC001        E DS                  EXTNAME(XSOC001DS)
014100981216     D*-------------
014200981216     D* DS Interna per dati di output di XSOC
014300981216     D XSOCDS          DS          1000
014400981216     D*-------------
014500990107    >D A009DS        E DS                  EXTNAME(ANA009DS)
014600990107    >D  RET009       E                     INZ('0')
014700990107    >D  OPR009       E                     INZ('0')
014800990107    >D  ERR009       E                     INZ('0')
014900990107     D*-------------
015000981216     D* Passaggio Parametri al PGM di selezione e stampa
015100981216    >D C131DS        E DS                  EXTNAME(NDC131DS)
015200981216     D*-------------
015300990423     D* Parametri per reperimento esercizio
015400990423    >D X10EseDs      E DS
015500990423     D*-------------
015600981216     D DSFMT           DS
015700981216     D  $TASTO               369    369
015800981216     D  NRG                  370    370
015900981216     D  NCL                  371    371
016000981216     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
016100981216     D*-------------
016200981216     D* posizione cursore
016300981216     D CURSOR          DS
016400981216     D  RIRI                   1      2B 0 INZ
016500981216     D  R$$                    2      2
016600981216     D  COCO                   3      4B 0 INZ
016700981216     D  C$$                    4      4
016800981216     D*-------------
016900981216     D* Tasti di funzione
017000981216     D F01             C                   CONST(X'31')
017100981216     D F02             C                   CONST(X'32')
017200981216     D F03             C                   CONST(X'33')
017300981216     D F04             C                   CONST(X'34')
017400981216     D F05             C                   CONST(X'35')
017500981216     D F06             C                   CONST(X'36')
017600981216     D F07             C                   CONST(X'37')
017700981216     D F08             C                   CONST(X'38')
017800981216     D F09             C                   CONST(X'39')
017900981216     D F10             C                   CONST(X'3A')
018000981216     D F11             C                   CONST(X'3B')
018100981216     D F12             C                   CONST(X'3C')
018200981216     D F13             C                   CONST(X'B1')
018300981216     D F14             C                   CONST(X'B2')
018400981216     D F15             C                   CONST(X'B3')
018500981216     D F16             C                   CONST(X'B4')
018600981216     D F17             C                   CONST(X'B5')
018700981216     D F18             C                   CONST(X'B6')
018800981216     D F19             C                   CONST(X'B7')
018900981216     D F20             C                   CONST(X'B8')
019000981216     D F21             C                   CONST(X'B9')
019100981216     D F22             C                   CONST(X'BA')
019200981216     D F23             C                   CONST(X'BB')
019300981216     D F24             C                   CONST(X'BC')
019400981216     D ENTER           C                   CONST(X'F1')
019500981216     D ROLDWN          C                   CONST(X'F4')
019600981216     D ROLLUP          C                   CONST(X'F5')
019700981216     D*-------------
019800981216     D* Sottotitolo di testata
019900981216    >D* se non si hanno sottotitoli, mettere tutta la costante = ' '
020000981216     D*SOTTIT          C                   CONST('PROMSG    *LIBL-
020100981216     D*                                    COSxxxx')
020200981216     D SOTTIT          C                   CONST('                            ')
020300990423     D*-------------
020400981216     D* Campi di lavoro x date
020500981216     D D1DTIR          S               D   inz
020600981216     D D1DTFR          S               D   inz
020700981216     D D1DTCR          S               D   inz(D'0001-01-01')
020800990423     D*-------------
020900000118C1309D ZDateISO        S               D
021000940127     C*----------------------------------------------------*
021100940127     C*                MAIN LINE PROGRAM
021200940127     C*----------------------------------------------------*
021300940127     C*
021400940223     C* inizializzazione variabili
021500940223     C                   EXSR      INZVAR
021600940223     C*
021700940223     C     $FINE         DOWEQ     *OFF
021800941122     C     $GEST         CASEQ     'D1'          GESD1
021900941027     C                   ENDCS
022000941027     C                   ENDDO
022100940325     C* fine programma
022200940325     C                   EXSR      ENDPGM
022300940225     C*
022400940325     C/EJECT
022500940325     C************************************************************
022600940325     C* FINE PROGRAMMA
022700940325     C************************************************************
022800940325     C     ENDPGM        BEGSR
022900940722     C*
023000950123     C                   SETON                                            RT
023100940616     C                   RETURN
023200940325     C*
023300940325     C                   ENDSR
023400940224     C/EJECT
023500940201     C************************************************************
023600940201     C* RICERCHE
023700940201     C************************************************************
023800940127     C     SEARCH        BEGSR
023900981216     C*
024000981216     C* determino Riga/Colonna del cursore
024100981216     C                   MOVE      NRG           R$$
024200981216     C                   MOVE      NCL           C$$
024300981216     C                   Z-ADD     RIRI          CURR
024400981216     C                   Z-ADD     COCO          CURC
024500981216     C*
024600981216     C* I campi che al momento dell'F4 risultano protetti non devono
024700981216     C* essere abilitati alla funzione di ricerca
024800981216     C*
024900981216     C                   SELECT
025000981216     C*
025100981216    >C*  FMTD1  - Contabilità
025200981216     C     H1NMFL        WHENEQ    'D1CTB'
025300981216     C     *IN11         ANDEQ     *OFF
025400981216     C                   RESET                   XTABDS
025500981216     C                   MOVE      *BLANK        WWCTB
025600981216     C                   MOVE      WWCTB         XTAKEY
025700981216     C                   MOVE      '01'          XTAOPZ
025800981216     C                   MOVE      XSCSOC        XTAAZI
025900981216     C                   MOVE      XSCLIN        XTALIN
026000981216     C                   Z-ADD     CURR          XTARIG
026100981216     C                   Z-ADD     CURC          XTACOL
026200981216     C                   CLEAR                   KPJBU
026300981216     C                   MOVEL     XTABDS        KPJBU
026400981216     C                   CALL      'ANGG84R'
026500981216     C                   PARM                    KPJBA
026600981216     C                   MOVEL     KPJBU         XTABDS
026700981216     C     XTAERR        IFNE      '1'
026800981216     C                   MOVE      XTAKEY        WWCTB
026900981216     C                   ELSE
027000981216     C                   MOVE      *BLANK        WWCTB
027100981216     C                   ENDIF
027200981216     C*
027300981216     C     WWCTB         IFNE      *BLANK
027400981216     C                   MOVEL     WWCTB         D1CTB
027500981216     C                   ENDIF
027600990107     C*
027700990107    >C*  FMTD1  - unità
027800990107    >C     H1NMFL        WHENEQ    'D1UNI'
027900990107     C     GestUni       ANDEQ     *on
028000990107    >C                   RESET                   A009DS
028100990107    >C                   MOVEL     '01'          OPZ009
028200990107    >C                   MOVEL     XscSoc        SOC009
028300990107    >C                   MOVEL     A009DS        KPJBU
028400990107    >C                   CALL      'ANA009R'
028500990107     C                   PARM                    KPJBA
028600990107    >C                   MOVEL     KPJBU         A009DS
0287009901072   >C     UNI009        IFNE      *BLANK
028800990107    >C                   MOVE      UNI009        D1UNI
0289009901072-  >C                   ENDIF
029000981216     C*
029100981216     C* ricerca non abilitata in questa posizione
029200981216     C                   OTHER
029300981216     C* imposto pos. del cursore
029400981216     C                   Z-ADD     CURR          H1RIGA
029500981216     C                   Z-ADD     CURC          H1COLO
029600981216     C                   SETON                                        9899
029700981216     C*
029800981216     C                   ENDSL
029900981216     C*
030000981216     C                   MOVE      *HIVAL        $LASTV
030100981216     C                   ENDSR
030200940506     C/EJECT
030300940506     C************************************************************
030400940506     C* RIEMPIMENTO TASTI FUNZIONE
030500940506     C************************************************************
030600940506     C     RIEKEY        BEGSR
030700940506     C*
030800940506     C* Input :
030900940506     C* - $FX = schiera con tutti i testi disponibili
031000940506     C* - $FC = schiere con i flag di validità associati ai testi
031100940506     C* - $FR = schiere con i flag di REVERSE IMAGE
031200940506     C* - $ULK = ultimo testo caricato
031300940721     C* - $L01 = Lunghezza prima riga
031400940721     C* - $L02 = Lunghezza seconda riga
031500940506     C* Output :
031600940506     C* - $ULK
031700940609     C* - $KEY1= 1° riga di tasti funzione
031800940609     C* - $KEY2= 2° riga di tasti funzione
031900940506     C*
032000950529     C                   CALLB     'XKEYMSG'
032100940506     C* input
032200940506     C                   PARM                    $FX
032300940506     C                   PARM                    $FC
032400940506     C                   PARM                    $FR
032500940506     C                   PARM                    $ULK              3 0
032600940506     C* output
032700940506     C                   PARM                    $KEY1            79
032800940506     C                   PARM                    $KEY2            79
032900941115     C* input
033000941115     C                   PARM                    $L01              3 0
033100941115     C                   PARM                    $L02              3 0
033200940506     C*
033300060224C1913C                   PARM                    $ALLFUNCT       480
033400060224C1913C*
033500940506     C                   ENDSR
033600940224     C/EJECT
033700940224     C************************************************************
033800940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
033900940224     C************************************************************
034000940224     C     GESPIE        BEGSR
034100940224     C*
034200940224     C                   SELECT
034300941122     C*
034400940224     C     $GEST         WHENEQ    'D1'
034500940506     C     $ULKD1        IFEQ      0
034600940506     C                   EXSR      F24D1
034700940506     C                   ELSE
034800940506     C                   WRITE     FMTZ1
034900940506     C                   ENDIF
035000940506     C*
035100941128     C                   ENDSL
035200941128     C*
035300940224     C                   ENDSR
035400940224     C/EJECT
035500940224     C************************************************************
035600941128     C* GESTIONE VIDEO DI LANCIO
035700940224     C************************************************************
035800940224     C     GESD1         BEGSR
035900940224     C* inizializzazione videata
036000940223     C     $INZD1        IFEQ      *ON
036100940127     C                   EXSR      INZD1
036200940223     C                   MOVE      *OFF          $INZD1
036300941027     C                   ENDIF
036400940223     C* emissione piede videata se proveniente da altra
036500940223     C* salvataggio valore formato di provenienza
036600940202     C     $LASTV        IFNE      'D1'
036700940223     C* salvo stato F17 per sapere cosa fare in caso di F12
036800940202     C                   WRITE     FMTT1
036900940224     C                   EXSR      GESPIE
037000940202     C                   MOVE      $LASTV        $LASTG
037100940202     C                   MOVE      'D1'          $LASTV
037200940707     C                   ENDIF
037300940707     C*
037400940707     C* se esistono errori sulla videata
037500940707     C* emetto la write del formato a indicatori spenti per vedere
037600940707     C* le eventuali decodifiche
037700940707     C     *IN99         IFEQ      *ON
037800940707     C                   MOVEA     *IN           WIN
037900940707     C                   MOVE      *ALL'0'       IN5098           49
038000940707     C                   MOVEA     IN5098        *IN(50)
038100940707     C                   WRITE     FMTD1
038200940707     C                   MOVEA     WIN           *IN
038300940707     C                   ENDIF
038400990107     C*
038500950123     C                   SELECT
038600950123      * Annullamento
038700981218    >C     OPZ130        WHENEQ    '04'
038800950123      * Visualizzazione
038900981218    >C     OPZ130        OREQ      '05'
039000950123     C                   WRITE     FMTD1
039100950123     C                   EXFMT     PROTECT
039200950123      * Immissione/Modifica/Copia
039300950123     C                   OTHER
039400950123     C                   EXFMT     FMTD1
039500950123     C                   ENDSL
039600940506     C* controllo abilitazione tasti funzione
039700940506     C*
039800940506     C                   EXSR      CTRKD1
039900940506     C*
040000940506     C     *IN99         IFEQ      *OFF
040100940131     C*
040200940117     C                   SELECT
040300940916     C* F2=Decodifica
040400940916     C     $TASTO        WHENEQ    F02
040500940916     C                   EXSR      F02D1
040600940131     C* F3=Fine
040700940506     C     $TASTO        WHENEQ    F03
040800940307     C                   EXSR      F03D1
040900940131     C* F4=Ricerca
041000940506     C     $TASTO        WHENEQ    F04
041100940127     C                   EXSR      SEARCH
041200940916     C* F5=Refresh
041300940916     C     $TASTO        WHENEQ    F05
041400940916     C                   EXSR      F05D1
041500950123     C* F8=Selezione seguente
041600950123     C     $TASTO        WHENEQ    F08
041700950123     C                   EXSR      F08D1
041800940131     C* F12=Ritorno
041900940506     C     $TASTO        WHENEQ    F12
042000940307     C                   EXSR      F12D1
042100940224     C* F24=Altri tasti
042200940506     C     $TASTO        WHENEQ    F24
042300940309     C                   EXSR      F24D1
042400940131     C*
042500940117     C                   OTHER
042600940127     C                   EXSR      CTRD1
042700940127     C     *IN99         IFEQ      *OFF
042800941128     C     $TASTO        ANDEQ     F06
042900950123     C                   EXSR      AGGPAR
043000940211     C                   ENDIF
043100940224     C*
043200940117     C                   ENDSL
043300940224     C*
043400940506     C                   ENDIF
043500940117     C                   ENDSR
043600940224     C/EJECT
043700940224     C************************************************************
043800940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
043900940224     C************************************************************
044000940127     C     INZD1         BEGSR
044100940318     C*
044200940318     C                   CLEAR                   FMTD1
044300950508     C*
044400981218    >C                   MOVEL     DES130        D1DESCRIZ
044500991112     C*
044600981216     C                   MOVE      XSCON         D1ON1
044700981216     C                   MOVE      XSCON         D1ON2
044800981216     C                   MOVE      XSCON         D1ON3
044900981216     C                   MOVE      XSCON         D1ON4
045000981216     C                   MOVE      XSCON         D1ON5
045100981216     C                   MOVE      XSCON         D1ON6
045200981216     C                   MOVE      XSCON         D1ON7
045300981216     C                   MOVE      XSCON         D1ON8
045400981216     C                   MOVE      XSCOFF        D1OF1
045500981216     C                   MOVE      XSCOFF        D1OF2
045600981216     C                   MOVE      XSCOFF        D1OF3
045700981216     C                   MOVE      XSCOFF        D1OF4
045800981216     C                   MOVE      XSCOFF        D1OF5
045900981216     C                   MOVE      XSCOFF        D1OF6
046000981216     C                   MOVE      XSCOFF        D1OF7
046100981216     C                   MOVE      XSCOFF        D1OF8
046200991112     C*
046300991112     C* contabilità (protetta)
046400991112     C                   MOVE      'CG'          D1CTB
046500981218     C                   MOVE      *ON           *IN11
046600991112     C*
046700981218     C* unità
046800981218     C                   movel     xscfgo        gestuni           1
046900981218     C     gestuni       ifeq      *on
047000981218     C                   move      *on           *in12
047100981218     C                   endif
047200981221     C*
047300981221???? C* tipo registrazioni
047400981221???? C                   MOVE      xscon         D1DEF
047500981221???? C                   MOVE      xscoff        D1PRO
047600981221???? C                   MOVE      xscoff        D1GES
047700991112     C*
047800991112     C* data ultima chiusura
047900991112     C                   z-add     0             d1dtc
048000991112     C                   move      *loval        d1dtcr
048100981221     C     k2esc1        setll     anesc01l
048200981221     C     *in20         doweq     *off
048300981221     C     k2esc1        reade     anesc01l                               20
048400981221     C     *in20         ifeq      *off
048500981221     C     escrettif     andeq     '1'
048600000118C1309C******dmy          move      escdtfies     d1dtc
048700000118C1309C     *JobRun       move      escdtfies     d1dtc
048800981221     C                   move      escdtfies     d1dtcr
048900981221     C                   endif
049000981221     C                   enddo
049100981221     C                   setOff                                       20
049200981218     C*
049300981221     C* Se immissione propongo i valori di default
049400981218     C     OPZ130        IFEQ      '01'
049500981216     C* unità
049600981216     C     gestuni       ifeq      *on
049700981216     C                   move      xscdun        d1uni
049800981216     C                   endif
049900981216     C*
050000981216     C                   MOVE      'R'           D1SEO
050100981216???? C                   MOVE      *ZEROS        D1DTI
050200981216???? C                   MOVE      *ZEROS        D1DTF
050300981216     C                   MOVE      XSCON         D1DET
050400981216     C                   MOVE      XSCON         D1NMO
050500981216     C                   MOVE      XSCON         D1SLM
050600981216     C                   MOVE      XSCON         D1SLP
050700981216     C                   MOVE      XSCON         D1CAU
050800090504>>>>>C                   MOVE      XSCOFF        D1YCD
050900950123     C*
051000981218     C                   ELSE
051100950413     C*
051200950315     C* imposto valori ricevuti nella ds dei parametri di lancio
051300981218    >C                   MOVEL     PAR130        C131DS
051400950315     C* e carico video
051500981218    >C                   MOVEL     Ctb131        D1Ctb
051600981218    >C                   MOVEL     Uni131        D1Uni
051700981218     C*
051800991112     C****               If        Dtc131 <> *LoVal
051900991112    >C**** *dmy          MOVE      Dtc131        D1Dtc
052000991112     C****               EndIf
052100981218     C*
052200981218    >C                   MOVEL     Seo131        D1Seo
052300981218     C*
052400981218     C                   If        Dti131 <> *LoVal
052500000118C1309C******dmy          MOVE      Dti131        D1Dti
052600000118C1309C     *JobRun       MOVE      Dti131        D1Dti
052700981218     C                   EndIf
052800981218     C*
052900981218     C                   If        Dtf131 <> *HiVal
053000000118C1309C******dmy          MOVE      Dtf131        D1Dtf
053100000118C1309C     *JobRun       MOVE      Dtf131        D1Dtf
053200981218     C                   EndIf
053300981218     C*
053400981218     C                   If        Def131 = *On
053500981218     C                   Move      XscOn         D1Def
053600981218     C                   Else
053700981218     C                   Move      XscOff        D1Def
053800981218     C                   EndIf
053900981218     C                   If        Pro131 = *On
054000981218     C                   Move      XscOn         D1Pro
054100981218     C                   Else
054200981218     C                   Move      XscOff        D1Pro
054300981218     C                   EndIf
054400981218     C                   If        Ges131 = *On
054500981218     C                   Move      XscOn         D1Ges
054600981218     C                   Else
054700981218     C                   Move      XscOff        D1Ges
054800981218     C                   EndIf
054900981218     C                   If        Det131 = *On
055000981218     C                   Move      XscOn         D1Det
055100981218     C                   Else
055200981218     C                   Move      XscOff        D1Det
055300981218     C                   EndIf
055400981218     C                   If        Nmo131 = *On
055500981218     C                   Move      XscOn         D1Nmo
055600981218     C                   Else
055700981218     C                   Move      XscOff        D1Nmo
055800981218     C                   EndIf
055900981218     C                   If        Slm131 = *On
056000981218     C                   Move      XscOn         D1Slm
056100981218     C                   Else
056200981218     C                   Move      XscOff        D1Slm
056300981218     C                   EndIf
056400981218     C                   If        Slp131 = *On
056500981218     C                   Move      XscOn         D1Slp
056600981218     C                   Else
056700981218     C                   Move      XscOff        D1Slp
056800981218     C                   EndIf
056900981218     C                   If        Cau131 = *On
057000981218     C                   Move      XscOn         D1Cau
057100981218     C                   Else
057200981218     C                   Move      XscOff        D1Cau
057300981218     C                   EndIf
057400090504>>>>>C*
057500090504>>>>>C                   IF        Ycd131 = *ON
057600090504>>>>>C                   MOVEL     XscOn         D1Ycd
057700090504>>>>>C                   ELSE
057800090504>>>>>C                   MOVEL     XscOff        D1Ycd
057900090504>>>>>C                   ENDIF
058000981218     C*
058100981218     C                   ENDIF
058200950414     C*
058300950414     C* Controllo per decodifiche D1
058400950414     C                   EXSR      CTRD1
058500950414     C                   MOVE      *ALL'0'       IN5098
058600950414     C                   MOVEA     IN5098        *IN(50)
058700940224     C*
058800940117     C                   ENDSR
058900940506     C/EJECT
059000940506     C************************************************************
059100940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
059200940609     C* INPUT
059300941116     C* $FC1 = Abilitazioni videata D1
059400940509     C* OUTPUT
059500940509     C* *IN97 *IN99 Errori
059600940506     C************************************************************
059700940506     C     CTRKD1        BEGSR
059800940506     C                   SETOFF                                       99
059900950123     C                   MOVEA     $FC1          $FC
060000940506     C                   EXSR      CTRKEY
060100940506     C     *IN99         IFEQ      *ON
060200940506     C                   SETON                                        97
060300940506     C                   ENDIF
060400940506     C                   ENDSR
060500940506     C/EJECT
060600940506     C************************************************************
060700940506     C* CONTROLLO ABILITAZIONE TASTI
060800940509     C* INPUT
060900940609     C* $TASTO =Tasto funzione premuto
061000940509     C* $FC = Tasti abilitati
061100940509     C* OUTPUT
061200940509     C* IN99 = ERrore
061300940506     C************************************************************
061400940506     C     CTRKEY        BEGSR
061500940506     C                   SELECT
061600940506     C     $TASTO        WHENEQ    F01
061700940720     C     $FC(1)        IFEQ      '0'
061800940506     C                   SETON                                        99
061900940506     C                   ENDIF
062000940506     C     $TASTO        WHENEQ    F02
062100940720     C     $FC(2)        IFEQ      '0'
062200940506     C                   SETON                                        99
062300940506     C                   ENDIF
062400940506     C     $TASTO        WHENEQ    F03
062500940720     C     $FC(3)        IFEQ      '0'
062600940506     C                   SETON                                        99
062700940506     C                   ENDIF
062800940506     C     $TASTO        WHENEQ    F04
062900940720     C     $FC(4)        IFEQ      '0'
063000940506     C                   SETON                                        99
063100940506     C                   ENDIF
063200940506     C     $TASTO        WHENEQ    F05
063300940720     C     $FC(5)        IFEQ      '0'
063400940506     C                   SETON                                        99
063500940506     C                   ENDIF
063600940506     C     $TASTO        WHENEQ    F06
063700940720     C     $FC(6)        IFEQ      '0'
063800940506     C                   SETON                                        99
063900940506     C                   ENDIF
064000940506     C     $TASTO        WHENEQ    F07
064100940720     C     $FC(7)        IFEQ      '0'
064200940506     C                   SETON                                        99
064300940506     C                   ENDIF
064400940506     C     $TASTO        WHENEQ    F08
064500940720     C     $FC(8)        IFEQ      '0'
064600940506     C                   SETON                                        99
064700940506     C                   ENDIF
064800940615     C     $TASTO        WHENEQ    F09
064900940720     C     $FC(09)       IFEQ      '0'
065000940506     C                   SETON                                        99
065100940506     C                   ENDIF
065200940506     C     $TASTO        WHENEQ    F10
065300940720     C     $FC(10)       IFEQ      '0'
065400940506     C                   SETON                                        99
065500940506     C                   ENDIF
065600940506     C     $TASTO        WHENEQ    F11
065700940720     C     $FC(11)       IFEQ      '0'
065800940506     C                   SETON                                        99
065900940506     C                   ENDIF
066000940506     C     $TASTO        WHENEQ    F12
066100940720     C     $FC(12)       IFEQ      '0'
066200940506     C                   SETON                                        99
066300940506     C                   ENDIF
066400940506     C     $TASTO        WHENEQ    F13
066500940720     C     $FC(13)       IFEQ      '0'
066600940506     C                   SETON                                        99
066700940506     C                   ENDIF
066800940506     C     $TASTO        WHENEQ    F14
066900940720     C     $FC(14)       IFEQ      '0'
067000940506     C                   SETON                                        99
067100940506     C                   ENDIF
067200940506     C     $TASTO        WHENEQ    F15
067300940720     C     $FC(15)       IFEQ      '0'
067400940506     C                   SETON                                        99
067500940506     C                   ENDIF
067600940506     C     $TASTO        WHENEQ    F16
067700940720     C     $FC(16)       IFEQ      '0'
067800940506     C                   SETON                                        99
067900940506     C                   ENDIF
068000940506     C     $TASTO        WHENEQ    F17
068100940720     C     $FC(17)       IFEQ      '0'
068200940506     C                   SETON                                        99
068300940506     C                   ENDIF
068400940506     C     $TASTO        WHENEQ    F18
068500940720     C     $FC(18)       IFEQ      '0'
068600940506     C                   SETON                                        99
068700940506     C                   ENDIF
068800940506     C     $TASTO        WHENEQ    F19
068900940720     C     $FC(19)       IFEQ      '0'
069000940506     C                   SETON                                        99
069100940506     C                   ENDIF
069200940506     C     $TASTO        WHENEQ    F20
069300940720     C     $FC(20)       IFEQ      '0'
069400940506     C                   SETON                                        99
069500940506     C                   ENDIF
069600940506     C     $TASTO        WHENEQ    F21
069700940720     C     $FC(21)       IFEQ      '0'
069800940506     C                   SETON                                        99
069900940506     C                   ENDIF
070000940506     C     $TASTO        WHENEQ    F22
070100940720     C     $FC(22)       IFEQ      '0'
070200940506     C                   SETON                                        99
070300940506     C                   ENDIF
070400940506     C     $TASTO        WHENEQ    F23
070500940720     C     $FC(23)       IFEQ      '0'
070600940506     C                   SETON                                        99
070700940506     C                   ENDIF
070800940506     C     $TASTO        WHENEQ    F24
070900940720     C     $FC(24)       IFEQ      '0'
071000940506     C                   SETON                                        99
071100940506     C                   ENDIF
071200940506     C                   ENDSL
071300940506     C                   ENDSR
071400940916     C/EJECT
071500940916     C************************************************************
071600940916     C* GESTIONE F02 VIDEO D1
071700940916     C************************************************************
071800940916     C     F02D1         BEGSR
071900940916     C*
072000940916     C* per decodificare la videata richiamo la routine dei ctrl
072100940916     C* e poi disattivo gli eventuali messaggi di errore
072200940916     C                   EXSR      CTRD1
072300940916     C*
072400940916     C                   MOVE      *ALL'0'       IN5098
072500940916     C                   MOVEA     IN5098        *IN(50)
072600940916     C*
072700940916     C                   ENDSR
072800940224     C/EJECT
072900940131     C************************************************************
073000940307     C* GESTIONE F03 VIDEO D1
073100940131     C************************************************************
073200940307     C     F03D1         BEGSR
073300940131     C*
073400940307     C                   MOVE      *ON           $FINE
073500940325     C* fine programma
073600940325     C                   EXSR      ENDPGM
073700940307     C*
073800940307     C                   ENDSR
073900940916     C/EJECT
074000940916     C************************************************************
074100940916     C* GESTIONE F05 VIDEO D1
074200940916     C************************************************************
074300940916     C     F05D1         BEGSR
074400940916     C*
074500940916     C                   MOVE      *ON           $INZD1
074600940916     C*
074700940916     C                   ENDSR
074800940307     C/EJECT
074900950123     C************************************************************
075000950123     C* GESTIONE F08 VIDEO D1
075100950123     C************************************************************
075200950123     C     F08D1         BEGSR
075300950123     C*
075400950123     C                   MOVE      *ON           $FINE
075500981218    >C                   MOVE      '0'           RET130
075600950123     C*
075700950123     C                   ENDSR
075800950123     C/EJECT
075900940307     C************************************************************
076000940307     C* GESTIONE F12 VIDEO D1
076100940307     C************************************************************
076200940307     C     F12D1         BEGSR
076300940307     C*
076400940307     C                   MOVE      *ON           $FINE
076500981218    >C                   MOVE      '2'           RET130
076600940307     C*
076700940307     C                   ENDSR
076800940307     C/EJECT
076900940307     C************************************************************
077000940307     C* GESTIONE F24 VIDEO D1
077100940609     C* input
077200940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
077300940509     C* $FX1 =Testi tasti funzione
077400940509     C* $FC1 = Abilitazioni
077500940509     C* $FR1 = Inversioni di fondo
077600940307     C************************************************************
077700940307     C     F24D1         BEGSR
077800940506     C*
077900940506     C                   MOVEA     $FX1          $FX
078000940506     C                   MOVEA     $FC1          $FC
078100940506     C                   MOVEA     $FR1          $FR
078200940506     C                   Z-ADD     $ULKD1        $ULK
078300940929     C                   Z-ADD     79            $L01
078400940929     C                   Z-ADD     79            $L02
078500940506     C                   EXSR      RIEKEY
078600940506     C                   Z-ADD     $ULK          $ULKD1            3 0
078700940720     C                   MOVEA     $FC           $FC1
078800940506     C                   MOVEL     $KEY1         Z1KE1
078900060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
079000940506     C                   MOVEL     $KEY2         Z1KE2
079100940506     C                   WRITE     FMTZ1
079200940506     C*
079300940307     C                   ENDSR
079400940307     C/EJECT
079500940307     C************************************************************
079600940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
079700940307     C************************************************************
079800940307     C     CTRD1         BEGSR
079900940307     C*
080000981216     C                   SETOFF                                       99
080100990107      * non imposto pos. del cursore
080200990107     C                   Z-ADD     0             H1RIGA
080300990107     C                   Z-ADD     0             H1COLO
080400990107     C*
080500990107     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
080600990107     C*
080700990107    >C     OPZ130        IFNE      '04'
080800990107     C*
080900981216     C* controllo obbligatorietà descrizione
081000981216     C                   If        D1Descriz = *Blank
081100981216     C                   SetOn                                        6699
081200981216     C                   EndIf
081300990423      *
081400981216      * Controllo/decodifica unità
081500990107     C                   clear                   D1UniD
081600981216     C                   if        gestuni=*on
081700981216     C                   if        d1uni<>*blank
081800981216     C                   movel     d1uni         uniunita
081900981216     C     k02uni01      chain     anuni01l                           21
082000981216   1 C                   if        *in21=*on
082100981216     C                   seton                                        7899
082200981216     C                   else
082300981216     C                   movel     unidesbrev    d1unid
082400981216     C                   endif
082500981216     C                   endif
082600981216      * Controllo autorizzazione unità
082700990107   1 C                   if        *in78=*off
082800981216     C                   move      knmus         xauprf
082900981216     C                   move      xscsoc        xausoc
083000981216     C                   move      d1uni         xauuni
083100981216     C                   move      *off          xauabl
083200990107     C                   Call      'XAUTUNI'
083300981216     C                   parm                    xautunids
083400981216   2 C                   if        xauabl<>*ON
083500981216     C                   seton                                        5099
083600981216   2-C                   endif
083700981216   1-C                   endif
083800981216   1-C                   endif
083900981216      *
084000981216      * Controllo autorizzazione contabilità
084100981216     C                   move      knmus         xacprf
084200981216     C                   move      xscsoc        xacsoc
084300981216     C                   move      d1ctb         xacctb
084400981216     C                   move      *off          xacabl
084500981216     C                   move      *off          xacdef
084600981216     C                   move      *off          xacpro
084700981216     C                   move      *off          xacges
084800990107     C                   Call      'XAUTCTB'
084900981216     C                   parm                    xautctbds
085000981216   1 C                   if        xacabl<>*ON
085100981216     C                   seton                                        6899
085200981216   1xC                   else
085300981216      * Codice contabilità
085400981216     C                   MOVE      *BLANKS       D1CTBD
085500981216     C                   MOVE      XCACUT        XTBUTE
085600981216     C                   MOVE      '1'           XTBRIC
085700981216     C                   MOVE      XSCSOC        XTBAZI
085800981216     C                   MOVE      *ZERO         XTBKEY
085900981216     C                   MOVE      'G84'         XTBCOD
086000981216     C                   MOVE      D1CTB         XTBKEY
086100981216     C                   CALLb     'XATB'
086200981216     C                   PARM                    XATBDS
0863009812161  2 C     XTBERR        IFEQ      *ON
086400981216     C                   SETON                                        6799
0865009812161x 2xC                   ELSE
086600981216     C                   MOVEL     XTBUNI        TABG84
086700981216     C                   MOVEL     DSBG84        D1CTBD
0868009812161- 2-C                   ENDIF
086900981216   1-C                   endif
087000990423     C* Autorizzazione movimenti def./provv./gest.
087100981216   1 C                   if        d1def=xscon and xacdef<>*ON
087200981216     C                   SETON                                        6999
087300981216   1-C                   endif
087400981216   1 C                   if        d1pro=xscon and xacpro<>*ON
087500981216     C                   SETON                                        7199
087600981216   1-C                   endif
087700981216   1 C                   if        d1ges=xscon and xacges<>*ON
087800981216     C                   SETON                                        7399
087900981216   1-C                   endif
088000981216   1 C                   if        d1def=xscoff and d1pro=xscoff and
088100981216     C                             d1ges=xscoff
088200981216     C                   SETON                                        707274
088300981216     C                   SETON                                          99
088400981216   1-C                   endif
088500981216      * Registrazioni definitive
0886009812161    C     d1def         IFNE      XSCON
088700981216     C     d1def         ANDNE     XSCOFF
088800981216     C                   SETON                                        7099
0889009812161-   C                   ENDIF
089000981216      * Registrazioni provvisorie
0891009812161    C     d1pro         IFNE      XSCON
089200981216     C     d1pro         ANDNE     XSCOFF
089300981216     C                   SETON                                        7299
0894009812161-   C                   ENDIF
089500981216      * Registrazioni gestionali
0896009812161    C     d1ges         IFNE      XSCON
089700981216     C     d1ges         ANDNE     XSCOFF
089800981216     C                   SETON                                        7499
0899009812161-   C                   ENDIF
090000990423      *
090100990423      * Data fine
0902009904231  1 C                   if        d1dtf=*zeros
090300000118C1309C*****              move      udate         d1dtf
090400000118C1309C                   move      ZDate         d1dtf
090500990423     C     *dmy          move      udate         d1dtfr
0906009904231x 1xC                   else
090700000118C1309C******dmy          test(d)                 d1dtf                  21
090800000118C1309C     *JobRun       test(d)                 d1dtf                  21
0909009904233  3 C     *in21         ifeq      *on
091000990423     C                   SETON                                        5299
0911009904233x 3xC                   else
091200000118C1309C******dmy          MOVEL     d1dtf         D1DTFR
091300000118C1309C     *JobRun       MOVEL     d1dtf         D1DTFR
0914009904233- 3-C                   endif
0915009904231- 1-C                   endif
091600990423      *
091700990423      * Data inizio
0918009904231  1 C                   if        d1dti=*zeros
091900990423     C                   move      *loval        d1dtir
092000990423      * se non immessa imposto quella di inizio esercizio rispetto data fine
092100990423     C                   if        *in52 = *off
092200990423     C                   move      xscsoc        x10societa
092300990423     C                   move      d1ctb         x10ctb
092400990423     C                   move      d1dtfr        x10data
092500990423     C                   call      'X10ESER'
092600990423     C                   parm                    x10eseds
092700990423     C                   if        x10dtines <> *loval
092800000118C1309C******dmy          move      x10dtines     d1dti
092900000118C1309C     *JobRun       move      x10dtines     d1dti
093000990423     C                   move      x10dtines     d1dtir
093100990423     C                   endif
093200990423     C                   endif
0933009904231x 1xC                   else
093400000118C1309C******dmy          test(d)                 d1dti                  21
093500000118C1309C     *JobRun       test(d)                 d1dti                  21
0936009904232  2 C     *in21         ifeq      *on
093700990423     C                   SETON                                        5199
0938009904232x 2xC                   else
093900000118C1309C******dmy          MOVEL     d1dti         D1DTIR
094000000118C1309C     *JobRun       MOVEL     d1dti         D1DTIR
0941009904232- 2-C                   endif
0942009904231- 1-C                   endif
094300990423      *
094400990423      * Congruenza date inizio/fine
0945009904231  1 C                   if        d1dtfr<d1dtir
094600990423     C                   seton                                        5399
0947009904231- 1 C                   endif
094800981216      *
094900981216      * Stampa dettaglio
0950009812161    C     d1det         IFNE      XSCON
095100981216     C     d1det         ANDNE     XSCOFF
095200981216     C                   SETON                                        5499
0953009812161-   C                   ENDIF
095400981216      * Stampa conti non movimentati
0955009812161    C     d1nmo         IFNE      XSCON
095600981216     C     d1nmo         ANDNE     XSCOFF
095700981216     C                   SETON                                        5599
0958009812161-   C                   ENDIF
095900981216      * Stampa saldi mensili
0960009812161    C     d1slm         IFNE      XSCON
096100981216     C     d1slm         ANDNE     XSCOFF
096200981216     C                   SETON                                        5699
0963009812161-   C                   ENDIF
096400981216      * Stampa saldi progressivi
0965009812161    C     d1slp         IFNE      XSCON
096600981216     C     d1slp         ANDNE     XSCOFF
096700981216     C                   SETON                                        5799
0968009812161-   C                   ENDIF
096900090504>>>>> * Se è stata chiesta la stampa per il CD
097000090504>>>>> * non si può stampare il saldo progressivo
097100090504>>>>> * altrimenti è generata una stampa non conforme
097200090504>>>>>C                   IF        D1Ycd = XscOn
097300090504>>>>>C                             AND
097400090504>>>>>C                             D1Slp = XscOn
097500090504>>>>>C                   SETON                                        5799
097600090504>>>>>C                   ENDIF
097700981216      * Stampa elenco causali
0978009812161    C     d1cau         IFNE      XSCON
097900981216     C     d1cau         ANDNE     XSCOFF
098000981216     C                   SETON                                        5899
0981009812161-   C                   ENDIF
098200981216      * Selezione/Ordinamento
0983009812161    C     d1seo         IFNE      'R'
098400981216     C     d1seo         ANDNE     'G'
098500981216     C                   SETON                                        5999
0986009812161-   C                   ENDIF
098700981216     C*
098800990107     C                   EndIf
098900950123     C*
099000940117     C                   ENDSR
099100941128     C/EJECT
099200941128     C************************************************************
099300950123     C* PASSAGGIO PARAMETRI AL PGM CHIAMANTE
099400941128     C************************************************************
099500950123     C     AGGPAR        BEGSR
099600941128     C*
099700981216     C                   clear                   C131DS
099800981216     C                   MOVE      XSCSOC        SOC131
099900981218     C                   MOVEL     D1DESCRIZ     DES130
100000981216     C                   MOVE      D1CTB         CTB131
100100981216     C                   MOVE      D1UNI         UNI131
100200981216     C                   MOVE      D1DTCR        DTC131
100300981216     C                   MOVE      D1SEO         SEO131
100400981216     C                   MOVE      D1DTIR        DTI131
100500981216     C                   MOVE      D1DTFR        DTF131
100600981216     c                   if        d1def = xscon
100700981216     C                   MOVE      *on           DEF131
100800981216     c                   else
100900981216     C                   MOVE      *off          DEF131
101000981216     c                   endif
101100981216     c                   if        d1pro = xscon
101200981216     C                   MOVE      *on           PRO131
101300981216     c                   else
101400981216     C                   MOVE      *off          pro131
101500981216     c                   endif
101600981216     c                   if        d1ges = xscon
101700981216     C                   MOVE      *on           GES131
101800981216     c                   else
101900981216     C                   MOVE      *off          ges131
102000981216     c                   endif
102100981216     C     D1DET         IFEQ      XSCON
102200981216     C                   MOVE      *on           DET131
102300981216     C                   ELSE
102400981216     C                   MOVE      *off          DET131
102500981216     C                   ENDIF
102600981216     C     D1NMO         IFEQ      XSCON
102700981216     C                   MOVE      *on           NMO131
102800981216     C                   ELSE
102900981216     C                   MOVE      *off          NMO131
103000981216     C                   ENDIF
103100981216     C     D1SLM         IFEQ      XSCON
103200981216     C                   MOVE      *on           SLM131
103300981216     C                   ELSE
103400981216     C                   MOVE      *off          SLM131
103500981216     C                   ENDIF
103600981216     C     D1SLP         IFEQ      XSCON
103700981216     C                   MOVE      *on           SLP131
103800981216     C                   ELSE
103900981216     C                   MOVE      *off          SLP131
104000981216     C                   ENDIF
104100981216     C     D1CAU         IFEQ      xscon
104200981216     C                   MOVE      *on           CAU131
104300981216     C                   ELSE
104400981216     C                   MOVE      *off          CAU131
104500981216     C                   ENDIF
104600090504>>>>>C*
104700090504>>>>>C     D1Ycd         IFEQ      XscOn
104800090504>>>>>C                   MOVEL     *ON           Ycd131
104900090504>>>>>C                   ELSE
105000090504>>>>>C                   MOVEL     *OFF          Ycd131
105100090504>>>>>C                   ENDIF
105200950315     C*
105300981218     C* Passare DS nel campo PAR130 per ripassarlo al pgm di gestione
105400950315     C*
105500981218    >C                   MOVEL     C131DS        PAR130
105600950123     C*
105700950123     C                   MOVE      *ON           $FINE
105800981218    >C                   MOVE      *ON           OPR130
105900950207     C*
106000941128     C                   ENDSR
106100940223     C/EJECT
106200940131     C************************************************************
106300940131     C* OPERAZIONI INIZIALI
106400940131     C************************************************************
106500940131     C     *INZSR        BEGSR
106600000118C1309C     *DMY          Move      Udate         ZDateISO
106700000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
106800941104      *
106900941104      * Reperimento dati società
107000941104      *
107100941115     C                   MOVEL     'SOC001'      TIPXSC
107200941104     C                   MOVEL     *BLANK        SOCXSC
107300941104     C                   EXSR      REPSOC
107400941104     C     RTNXSC        IFNE      '1'
107500941104     C                   MOVEL     XSOCDS        SOC001
107600941104     C                   ELSE
107700941111     C                   RETURN
107800941104     C                   ENDIF
107900940225     C*
108000940506     C* Reperimento tasti di funzione
108100940506     C*
108200940506     C                   EXSR      REPKEY
108300950124     C*
108400950124     C* reperisco costanti
108500950124     C*
108600950124     C                   EXSR      REPCOS
108700940127     C*
108800940117     C                   ENDSR
108900941104     C/EJECT
109000941104      ************************************************************
109100941104      * Reperimento dati società
109200941104      ************************************************************
109300941104     C     REPSOC        BEGSR
109400941104      *
109500950529     C                   CALLB     'XSOC'
109600941104     C                   PARM                    TIPXSC            6
109700941104     C                   PARM                    SOCXSC            3
109800941104     C                   PARM                    CDSXSC            9 0
109900941104     C                   PARM                    MODXSC            3
110000941104     C                   PARM      *BLANKS       RTNXSC            1
110100941104     C                   PARM                    XSOCDS
110200941104     C                   PARM                    KPJBA
110300941104      *
110400941104     C                   ENDSR
110500940223     C/EJECT
110600940223     C************************************************************
110700940223     C* INIZIALIZZAZIONE VARIABILI
110800940223     C************************************************************
110900940223     C     INZVAR        BEGSR
111000950123     C*
111100950123     C* Reperimento parametri
111200950123     C*
111300950123     C     *ENTRY        PLIST
111400981218    >C                   PARM                    OPZ130            2
111500981218    >C                   PARM                    RET130            1
111600981218    >C                   PARM                    OPR130            1
111700981218    >C                   PARM                    ERR130            1
111800981218    >C                   PARM                    DES130           50
111900981218    >C                   PARM                    PAR130          256
112000940506     C*
112100940506     C* Definizione delle opzioni utilizzabili a pgm
112200940506     C*
112300940506     C                   EXSR      DEFOPZ
112400941122     C*
112500940223     C* Variabili per gestione videate
112600941122     C*
112700940223     C                   MOVE      *OFF          $FINE                          fine pgm
112800940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
112900940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
113000940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
113100940223     C                   MOVE      *OFF          $EFILE                         memo fine file
113200940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
113300940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
113400941124     C                   MOVE      *OFF          $AGGOK                         confermatore OK
113500940509     C                   MOVE      *HIVAL        $LASTV
113600940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
113700940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
113800940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
113900981218    >C                   MOVE      *OFF          OPR130
114000981218    >C                   MOVE      *OFF          RET130
114100981218    >C                   MOVE      *OFF          ERR130
114200940223     C*
114300940318     C                   MOVE      *ON           $INZD1
114400940318     C                   MOVE      *ON           $INZS1
114500940831     C*
114600940223     C                   MOVE      'D1'          $GEST
114700940223     C*
114800940223     C* Valorizzazione campi univoci testate
114900940223     C*
115000940223     C                   CLEAR                   FMTT1
115100940223     C                   MOVEL     KNSIF         NOMSIF
115200941109     C                   MOVEL     XSCDSI        NOMDIT
115300941027     C                   MOVEL     DSPGM         NOMPGM
115400950124     C                   SELECT
115500981218    >C     OPZ130        WHENEQ    '01'
115600950315     C                   MOVEL     IMM           T1OPE
115700981218    >C     OPZ130        WHENEQ    '02'
115800950315     C                   MOVEL     MOD           T1OPE
115900981218    >C     OPZ130        WHENEQ    '03'
116000950315     C                   MOVEL     COP           T1OPE
116100981218    >C     OPZ130        WHENEQ    '04'
116200950315     C                   MOVEL     ANN           T1OPE
116300981218    >C     OPZ130        WHENEQ    '05'
116400950315     C                   MOVEL     VIS           T1OPE
116500981218    >C     OPZ130        WHENEQ    '06'
116600960522     C                   MOVEL     LAN           T1OPE
116700950315    >C                   ENDSL
116800940223     C*
116900940223     C                   ENDSR
117000940223     C/EJECT
117100940131     C************************************************************
117200940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
117300940131     C************************************************************
117400940225     C     DEFOPZ        BEGSR
117500940131     C*
117600941128     C* Questa routine deve valorizzare la schiera $FC1/$FR1
117700941018     C* con 1 o 0 a seconda della utilizzabilità o meno
117800940715     C* del tasto funzione correlato
117900940715     C*
118000940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
118100940715     C* di compilazione
118200940715     C                   RESET                   $FC1
118300940715     C                   RESET                   $FR1
118400940225     C*
118500940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
118600941122     C* invalido alcuni tasti
118700951006     C                   SELECT
118800951006     C* in caso di visualizzazione disabilito conferma
118900981218     C     OPZ130        wheneq    '05'
119000951006      * F6=Conferma
119100951006     C                   Z-ADD     6             X
119200960424     C                   MOVE      '0'           $FC1(X)
119300951006     C                   ENDSL
119400940225     C*
119500940225     C                   ENDSR
119600940506     C/EJECT
119700940506     C************************************************************
119800940506     C* REPERIMENTO TASTI DI FUNZIONE
119900940506     C************************************************************
120000940506     C     REPKEY        BEGSR
120100941123     C*
120200940506     C                   MOVEA     $FM1          $FM
120300950721     C                   MOVEA     $Fc1          $Fc
120400940506     C                   EXSR      REPKKK
120500940506     C                   MOVEA     $FX           $FX1
120600940919     C*
120700940919     C                   ENDSR
120800940506     C/EJECT
120900940506     C************************************************************
121000940506     C* REPERIMENTO TASTI DI FUNZIONE
121100940506     C************************************************************
121200940506     C     REPKKK        BEGSR
121300940506     C*
121400940506     C                   CLEAR                   $FX
121500940506     C     1             DO        24            X
121600940506     C*
121700940506     C     $FM(X)        IFNE      *BLANK
121800950721     C     $Fc(X)        andne     '0'
121900940506     C*
122000940506     C                   MOVEL     $FM(X)        IDMSG
122100950529     C                   CALLB     'XRTVM'
122200940506     C                   PARM                    IDMSG            27
122300940506     C                   PARM                    TXTMSG          100
122400940506     C                   PARM                    ERRMSG            1
122500940506     C*
122600940506     C     ERRMSG        IFNE      *ON
122700940506     C                   MOVEL     TXTMSG        $FX(X)
122800940506     C                   ELSE
122900940506     C                   MOVEL     *ALL' '       $FX(X)
123000941027     C                   ENDIF
123100940506     C*
123200941027     C                   ENDIF
123300940506     C*
123400941027     C                   ENDDO
123500940506     C*
123600940506     C                   ENDSR
123700940223     C/EJECT
123800940128     C************************************************************
123900940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
124000940128     C************************************************************
124100940223     C     DEFCAM        BEGSR
124200940223     C*
124300981216     C* klist
124400981216     C* ANESC01L
124500981216     C     k2esc1        klist
124600981216     C                   kfld                    xscsoc
124700981216     C                   kfld                    d1ctb
124800981216     C* ANUNI01L
124900981216     C     k02uni01      klist
125000981216     C                   kfld                    xscsoc
125100981216     C                   kfld                    uniunita
125200981216     C*
125300940223     C* Variabili per gestione videate
125400940223     C*
125500940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
125600940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
125700940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
125800940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
125900940223     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
126000940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
126100940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
126200940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
126300940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
126400940223     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
126500941124     C                   MOVE      *BLANK        $AGGOK            1            confermatore OK
126600940224     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
126700940224     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
126800940223     C*
126900940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
127000940223     C*
127100981216    >C     *LIKE         DEFINE    D1CTB         WWCTB
127200941128     C     *LIKE         DEFINE    $INZS1        WINZS1
127300940223     C                   Z-ADD     0             CURR              2 0
127400940223     C                   Z-ADD     0             CURC              2 0
127500940223     C                   MOVE      *BLANK        WIN              99
127600940314     C*
127700940314     C* Variabili appoggio specifiche della singola anagrafica
127800940314     C*
127900940223     C*
128000940223     C* Indici
128100940223     C*
128200940223     C                   Z-ADD     0             X                 3 0
128300940223     C                   Z-ADD     0             Y                 3 0
128400981216     C*
128500940117     C                   ENDSR
128600950124     C/EJECT
128700950124     C************************************************************
128800950124     C* REPERIMENTO COSTANTI
128900950124     C************************************************************
129000950124     C     REPCOS        BEGSR
129100950124     C*
129200960522     C                   DO        6             $X                4 0
129300950124     C                   MOVEL     $COS($X)      IDMSG
129400950529     C                   CALLB     'XRTVM'
129500950124     C                   PARM                    IDMSG            27
129600950124     C                   PARM                    TXTMSG          100
129700950124     C                   PARM                    ERRMSG            1
129800950413     C                   PARM      *ON           CTRMSG            1            centratura
129900950413     C                   PARM      30            LENMSG            3 0          lun output.
130000950124     C     ERRMSG        IFNE      *ON
130100950124     C                   SELECT
130200950124     C     $X            WHENEQ    1
130300950124     C                   MOVEL     TXTMSG        IMM              30
130400950124     C     $X            WHENEQ    2
130500950124     C                   MOVEL     TXTMSG        MOD              30
130600950124     C     $X            WHENEQ    3
130700950124     C                   MOVEL     TXTMSG        COP              30
130800950124     C     $X            WHENEQ    4
130900950124     C                   MOVEL     TXTMSG        ANN              30
131000950124     C     $X            WHENEQ    5
131100950124     C                   MOVEL     TXTMSG        VIS              30
131200960522     C     $X            WHENEQ    6
131300960522     C                   MOVEL     TXTMSG        LAN              30
131400950124     C                   ENDSL
131500950124     C                   ELSE
131600950124     C                   SELECT
131700950124     C     $X            WHENEQ    1
131800950124     C                   MOVEL     *ALL'?'       IMM
131900950124     C     $X            WHENEQ    2
132000950124     C                   MOVEL     *ALL'?'       MOD
132100950124     C     $X            WHENEQ    3
132200950124     C                   MOVEL     *ALL'?'       COP
132300950124     C     $X            WHENEQ    4
132400950124     C                   MOVEL     *ALL'?'       ANN
132500950124     C     $X            WHENEQ    5
132600950124     C                   MOVEL     *ALL'?'       VIS
132700960522     C     $X            WHENEQ    6
132800960522     C                   MOVEL     *ALL'?'       LAN
132900950124     C                   ENDSL
133000950124     C                   ENDIF
133100950124     C                   ENDDO
133200950124     C*
133300950124     C                   ENDSR
133400940223     C/EJECT
133500940720**  Costanti da file messaggi
133600950124PROMSG    *LIBL     COS0001   Immissione
133700950124PROMSG    *LIBL     COS0002   Modifica
133800950124PROMSG    *LIBL     COS0003   Copia
133900950124PROMSG    *LIBL     COS0004   Annullamento
134000950124PROMSG    *LIBL     COS0005   Visualizzazione
134100960522PROMSG    *LIBL     COS0232   Lancio
134200940506** TASTI DI FUNZIONE  D1
134300940506PROMSG    *LIBL     KEY0001  01
134400940506PROMSG    *LIBL     KEY0002  02
134500940506PROMSG    *LIBL     KEY0003  03
134600940506PROMSG    *LIBL     KEY0004  04
134700940506PROMSG    *LIBL     KEY0005  05
134800940506PROMSG    *LIBL     KEY0006  06
134900940506PROMSG    *LIBL     KEY0007  07
135000940506PROMSG    *LIBL     KEY0008  08
135100940506PROMSG    *LIBL     KEY0009  09
135200940506PROMSG    *LIBL     KEY0010  10
135300940506PROMSG    *LIBL     KEY0011  11
135400940506PROMSG    *LIBL     KEY0012  12
135500940506PROMSG    *LIBL     KEY0013  13
135600940506PROMSG    *LIBL     KEY0014  14
135700940506PROMSG    *LIBL     KEY0015  15
135800940506PROMSG    *LIBL     KEY0016  16
135900940506PROMSG    *LIBL     KEY0017  17
136000940506PROMSG    *LIBL     KEY0018  18
136100940506PROMSG    *LIBL     KEY0019  19
136200941128PROMSG    *LIBL     KEY0022  20
136300940506PROMSG    *LIBL     KEY0021  21
136400940506PROMSG    *LIBL     KEY0022  22
136500940506PROMSG    *LIBL     KEY0023  23
136600940506PROMSG    *LIBL     KEY0024  24
136700940506**  ABILITAZIONE D1
1368009407200N  01
1369009502100N  02
1370009501230N  03
1371009407201N  04
1372009407200N  05
1373009411281N  06
1374009407200N  07
1375009501231N  08
1376009407200N  09
1377009407200N  10
1378009407200N  11
1379009407201N  12
1380009407200N  13
1381009407200N  14
1382009407200N  15
1383009407200N  16
1384009407200N  17
1385009411150N  18
1386009407200N  19
1387009407200N  20
1388009411290N  21
1389009407200N  22
1390009509130N  23   Impostare a '1' se si usano le opzioni
1391009509131N  24   Lasciare sempre impostata a '1' la colonna $FC
