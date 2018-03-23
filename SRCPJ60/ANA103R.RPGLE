000100981202     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200941110     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000300960416     H*--------INIZIO SPECIFICHE --------------------------*
000400940211     H DECEDIT('0,') DATEDIT(*DMY.)
000500940224      *----------*
000600941018    > * ANA103R  *
000700940224      *----------*
000800941024    > *               ANAGRAFICA NUMERATORI
000900940224      *          ------------------------------
001000940224      *
001100940224      *
001200941018      * DOCUMENTI MICROANALISI :
001300940224      *
001400940224      * NOTE TECNICHE :
001500940224      *
001600940224      *
001700940224      * PERSONALIZZAZIONI
001800940224      *
001900940224      *
002000940224      *
002100940314      *               MAPPA INDICATORI
002200941011      *  01           CONDIZIONAMENTO CAMPO POSIZ. ALFANUM. (C1POS )
002300941011      *  02           CONDIZIONAMENTO CAMPO POSIZ. NUMERICO (C1POS2)
002400940224      *
002500940307      *  21           GENERICO OPERAZIONI I/O
002600940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002700940224      *  30           SFLDSP
002800940224      * N31           SFLCLR
002900940224      *  31           SFLDSPCTL
003000940224      *  32           SFLNXTCHG
003100940224      *  33           SFLEND
003200940224      *  39           OF PRTF
003300940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003400940317    > *  Specificare l'uso dei singoli indicatori
003500940224      *  50 <---> 98  ERRORI SU VIDEO
003600940909    > *  Specificare l'uso dei singoli indicatori
003700940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003800940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003900940224      *  99           INDIC. GENERALE DI ERRORE
004000941007    > *----------------------------------------------------------------
004100941007    > * Viste logiche utilizzate per i diversi ordinamenti
004200941007    > *----------------------------------------------------------------
004300941007      *------------
004400981229     FANNMR02L  IF   E           K DISK
004500981229E0045F                                     rename(annmr000:annmr02)
004600981229  "  F*--Vl su Soc/uni/ctb/key/ser/dat
004700981229  "  FANNMR01L  IF   E           K DISK
004800981229E0045F                                     rename(annmr000:annmr01)
004900981229      *------------
005000940211     F*------------
005100940201     FANMPE02L  UF A E           K DISK
005200940211     F*------------
005300941018    >FANA103D   CF   E             WORKSTN
005400941018     F                                     RENAME(A103D1:FMTD1)
005500941018     F                                     RENAME(A103D2:FMTD2)
005600941018     F                                     RENAME(A103D3:FMTD3)
005700941018     F                                     RENAME(A103S1:FMTS1)
005800941018     F                                     RENAME(A103C1:FMTC1)
005900941018     F                                     RENAME(A103T1:FMTT1)
006000941018     F                                     RENAME(A103Z1:FMTZ1)
006100941018     F                                     RENAME(A103Z2:FMTZ2)
006200941018     F                                     RENAME(A103Z3:FMTZ3)
006300940131     F                                     SFILE(FMTS1:S1NRR)
006400940201     F                                     INFDS(DSFMT)
006500940128     D*----------------------------------------------------*
006600940302     D* Schiere per la gestione delle opzioni
006700980717     D $DIMOP          C                   CONST(30)
006800940302     D*
006900980717    >D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
007000980717    >D $TX1            S             50    DIM($dimop) ALT($MS1)
007100980717    >D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
007200980717    >D $FL1            S              1    DIM($dimop) ALT($OP1)
007300950403     D $TX             S             50    DIM(50)
007400950403     D $FL             S              1    DIM(50)
007500980717     D* schiera contenente i nomi dei programma richiamati attraverso
007600980717     D* le opzioni personalizzate
007700980717     D $PGO1           S             10    DIM($dimop)
007800980717     D*
007900980717     D* Indice della schiera per le opzioni personalizzate
008000980717     D PosOpz          S              2  0
008100980717     D* numero max opzioni
008200980717     D NumOpz          S              2  0 Inz($DIMOP)
008300980717     D* numero effettive opzioni
008400980717     D OpzLen          S              2  0
008500980717     D*-------------
008600940909     D* Schiere generali per la gestione dei tasti di funzione
008700940506     D $FM             S             27    DIM(24)
008800940506     D $FX             S             50    DIM(24)
008900940506     D $FC             S              1    DIM(24)
009000940506     D $FR             S              1    DIM(24)
009100980717     D* Indice della schiera tasti funzione
009200980717     D PosFun          S              2  0
009300040929     D*-------------
009400941018     D* Schiere per la gestione dei tasti di funzione
009500941018     D* Video D1
009600941018     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
009700941018     D $FX1            S             50    DIM(24) ALT($FM1)
009800941018     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
009900941018     D $FR1            S              1    DIM(24) ALT($FC1)
010000940506     D* Schiere per la gestione dei tasti di funzione
010100940506     D* Video S1
010200940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
010300940506     D $FX2            S             50    DIM(24) ALT($FM2)
010400940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
010500940506     D $FR2            S              1    DIM(24) ALT($FC2)
010600980717     D* Schiera contente i nomi dei programmi da richiamare con i tasti
010700980717     D* funzione personalizzata S1
010800980717     D $PGF2           S             10    DIM(25)
010900940916     D* Schiere per la gestione dei tasti di funzione
011000940916     D* Video D2
011100940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
011200940916     D $FX3            S             50    DIM(24) ALT($FM3)
011300940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
011400940916     D $FR3            S              1    DIM(24) ALT($FC3)
011500940211     D*-------------
011600940207     D* Valori assunti
011700940207     D DSMPE           DS
011800940207     D  $LOP                   1      1  0
011900940831     D  $ORD                   2      3  0
012000940207     D*-------------
012100940211     D* Passaggio Parametri
012200940211     D KPJBA         E DS
012300941103     D*-------------
012400941103     D* Dati per pgm XSOC
012500941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
012600941103     D*-------------
012700941103      * DS Interna per dati utente
012800941103     D XSOCDS          DS          1000
012900941103     D*-------------
013000941010      * Passaggio Parametri al PGM XATB
013100941010     D XATBDS        E DS                  INZ
013200941103     D*-------------
013300980717     D* campo di salvataggio per la DS d'ingresso durante la chiamata
013400980717     D* alla funzione personalizzata
013500980717    >D $SaveDS         S                   like(A103DS)
013600980717     D*-------------
013700940325     D* Parametri in ricezione
013800941018    >D A103DS        E DS                  EXTNAME(ANA103DS)
013900941103     D*-------------
014000941025      * DS Tabella serie
014100960416    >D TABG13        E DS                  EXTNAME(ANGG13DS)    inz
014200961015     D*-------------
014300961015     D* Ricerca legende
014400961015     D Ana964Ds      E DS                  INZ
014500961015     D*-------------
014600961015     D* Controlli validità legende
014700961015     D X07ValDS      E DS                  INZ
014800940325     D*-------------
014900941018     D* Parametri del PGM di manutenzione ana104R
015000941018    >D A104DS        E DS                  EXTNAME(ANA104DS) INZ
015100941018    >D  RET104       E                     INZ('0')
015200941018    >D  OPR104       E                     INZ('0')
015300941018    >D  ERR104       E                     INZ('0')
015400030225A0994D*----------
015500030225  "  D* Reperimento Autorizzazioni
015600030225  "  D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
015700030225  "  D*-------------
015800030225  "  D* Autorizzazioni numeratori
015900030225  "  D AngNumds      E DS
016000030225  "  D*----------
016100030225A0994D Zeri            S             15
016200940211     D*-------------
016300940211     D DSFMT           DS           512
016400940506     D  $TASTO               369    369
016500940211     D  NRG                  370    370
016600940211     D  NCL                  371    371
016700940211     D  SFLNRR               378    379B 0
016800940211     D*-------------
016900941025     D*Ds per tempificazione
017000941025     D* giornaliero/mensile/anno solare
017100941025     D DSUNO           DS                  INZ
017200941025     D  DSGG                   1      2
017300941025     D  DSBAR                  3      3    INZ('/')
017400941025     D  DSMM                   4      5
017500941025     D  DSBAR1                 6      6    INZ('/')
017600941025     D  DSAA                   7     10
017700941025     D*
017800941025     D* esercizio
017900941025     D DSDUE           DS                  INZ
018000941025     D  DSAAE                  3      6
018100941025     D  DSBAR2                 7      7    INZ('/')
018200941025     D  DSPRE                  8      8
018300940211     D* posizione cursore
018400940211     D CURSOR          DS
018500940211     D  RIRI                   1      2B 0 INZ
018600940211     D  R$$                    2      2
018700940211     D  COCO                   3      4B 0 INZ
018800940211     D  C$$                    4      4
018900940127     D*-------------
019000940127     D* Reperimento nome PGM
019100940127     D STATUS         SDS           333
019200940127     D  DSPGM            *PROC
019300940207     D*-------------
019400940616     D* Costanti
019500940616     D* nr.di righe del sfl (SFLPAG)
019600960416     D SFLPAG          C                   CONST(6)
019700940616     D*-------------
019800940506     D* Tasti di funzione
019900940506     D F01             C                   CONST(X'31')
020000940506     D F02             C                   CONST(X'32')
020100940506     D F03             C                   CONST(X'33')
020200940506     D F04             C                   CONST(X'34')
020300940506     D F05             C                   CONST(X'35')
020400940506     D F06             C                   CONST(X'36')
020500940506     D F07             C                   CONST(X'37')
020600940506     D F08             C                   CONST(X'38')
020700940506     D F09             C                   CONST(X'39')
020800940506     D F10             C                   CONST(X'3A')
020900940506     D F11             C                   CONST(X'3B')
021000940506     D F12             C                   CONST(X'3C')
021100940506     D F13             C                   CONST(X'B1')
021200940506     D F14             C                   CONST(X'B2')
021300940506     D F15             C                   CONST(X'B3')
021400940506     D F16             C                   CONST(X'B4')
021500940506     D F17             C                   CONST(X'B5')
021600940506     D F18             C                   CONST(X'B6')
021700940506     D F19             C                   CONST(X'B7')
021800940506     D F20             C                   CONST(X'B8')
021900940506     D F21             C                   CONST(X'B9')
022000940506     D F22             C                   CONST(X'BA')
022100940506     D F23             C                   CONST(X'BB')
022200940506     D F24             C                   CONST(X'BC')
022300940506     D ENTER           C                   CONST(X'F1')
022400940506     D ROLDWN          C                   CONST(X'F4')
022500940506     D ROLLUP          C                   CONST(X'F5')
022600940207     D*-------------
022700940127     D*
022800941010     D* Parametri per ricerca codice ANATB00F
022900941010     D XTABDS          DS                  INZ
023000941010     D* '  ' = Gestione
023100941010     D* '01' = Ricerca
023200941010     D* '02' = Gestione
023300941010     D* '05' = Visualizzazione
023400941010     D* '11' = Abilitazione
023500941010     D  OPZTAB                 1      2
023600941010     D* 0 = ritorno normale
023700941010     D* 1 = F3
023800941010     D* 2 = F12
023900941010     D  RETTAB                 3      3
024000941010     D* operazione eseguite dal pgm chiamato :
024100941010     D* 0 = nessuna operazione
024200941010     D* 1 = eseguito aggiornamento
024300941010     D  OPRTAB                 4      4
024400941010     D* funzione non eseguibile per errore :
024500941010     D* 0 = nessun errore
024600941010     D* 1 = funzione richiamata chiusa in errore
024700941010     D* Aggiungere eventuali altri codici di errore
024800941010     D  ERRTAB                 5      5
024900941010     D* Azienda
025000941010     D  AZITAB                 6      8
025100941010     D* Lingua
025200941010     D  LINTAB                 9      9
025300941010     D* Codice scelto
025400941010     D  KEYTAB                10     24
025500941010     D* riga cursore chiamante
025600941010     D  RIGTAB                25     26  0
025700941010     D* colonna cursore chiamante
025800941010     D  COLTAB                27     28  0
025900941010     D ERRORE          C                   CONST('Codice errato')
026000941010     D DATA            C                   CONST('0001-01-01')
026100960416     D*
026200960416     D SerieNm999      S                   like(D1SerieNm)
026300940211     I/EJECT
026400940128     I*----------------------------------------------------*
026500981229E0045I*ANNMR000
026600981229  "  I*             NMRSOCIETA                  NMRSOC
026700981229  "  I*             NMRUNITA                    NMRUNI
026800981229  "  I*             NMRCONTAB                   NMRCTB
026900981229  "  I*             NMRGRUPPO                   NMRGRU
027000981229  "  I*             NMRCHIAVE                   NMRKEY
027100981229  "  I*             NMRSERIENM                  NMRSER
027200981229  "  I*             NMRTEMPO                    NMRTMP
027300981229  "  I*             NMRNUMERO                   NMRNUM
027400981229  "  I*             NMRDATA                     NMRDAT
027500981229  "  IANNMR01
027600981229  "  I              NMRSOCIETA                  NMRSOC
027700981229  "  I              NMRUNITA                    NMRUNI
027800981229  "  I              NMRCONTAB                   NMRCTB
027900981229  "  I              NMRGRUPPO                   NMRGRU
028000981229  "  I              NMRCHIAVE                   NMRKEY
028100981229  "  I              NMRSERIENM                  NMRSER
028200981229  "  I              NMRTEMPO                    NMRTMP
028300981229  "  I              NMRNUMERO                   NMRNUM
028400981229  "  I              NMRDATA                     NMRDAT
028500981229  "  IANNMR02
028600981229  "  I              NMRSOCIETA                  NMRSOC
028700981229  "  I              NMRUNITA                    NMRUNI
028800981229  "  I              NMRCONTAB                   NMRCTB
028900981229  "  I              NMRGRUPPO                   NMRGRU
029000981229  "  I              NMRCHIAVE                   NMRKEY
029100981229  "  I              NMRSERIENM                  NMRSER
029200981229  "  I              NMRTEMPO                    NMRTMP
029300981229  "  I              NMRNUMERO                   NMRNUM
029400981229E0045I              NMRDATA                     NMRDAT
029500940127     C*----------------------------------------------------*
029600940127     C*                MAIN LINE PROGRAM
029700940127     C*----------------------------------------------------*
029800940127     C*
029900940223     C* inizializzazione variabili
030000940223     C                   EXSR      INZVAR
030100940223     C*
030200940223     C     $FINE         DOWEQ     *OFF
030300940131     C     $GEST         CASEQ     'D1'          GESD1
030400940131     C     $GEST         CASEQ     'S1'          GESS1
030500940131     C     $GEST         CASEQ     'D2'          GESD2
030600940117     C                   END
030700940117     C                   END
030800940325     C* fine programma
030900940325     C                   EXSR      ENDPGM
031000940225     C*
031100940325     C/EJECT
031200940325     C************************************************************
031300940325     C* FINE PROGRAMMA
031400940325     C************************************************************
031500940325     C     ENDPGM        BEGSR
031600940722     C*
031700940722     C* aggiorno valori assunti
031800940722     C                   EXSR      AGGMPE
031900941018     C   KC              MOVE      '1'           RET103
032000941018     C   KL              MOVE      '2'           RET103
032100940325     C*
032200940325     C* passaggio dati al pgm chiamante
032300941018    >C                   MOVEL     A103DS        KPJBU
032400940325     C*
032500940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
032600941018    >C     OPZ103        IFEQ      *BLANK
032700940325     C                   SETON                                            LR
032800940325     C* altrimenti chiudo in RT
032900940325     C                   ELSE
033000940325     C                   SETON                                            RT
033100940325     C                   ENDIF
033200940616     C                   RETURN
033300940325     C*
033400940325     C                   ENDSR
033500940224     C/EJECT
033600940201     C************************************************************
033700940201     C* RICERCHE
033800940201     C************************************************************
033900940127     C     SEARCH        BEGSR
034000940127     C*
034100940127     C* determino Riga/Colonna del cursore
034200940127     C                   MOVE      NRG           R$$
034300940127     C                   MOVE      NCL           C$$
034400940127     C                   Z-ADD     RIRI          CURR
034500940127     C                   Z-ADD     COCO          CURC
034600940127     C*
0347009410071    C                   SELECT
034800960416     C* FMTD1  - SERIE
034900960416     C* imposto pos. del cursore
035000960416      *
035100960416    >C     H1NMFL        WHENEQ    'D1SERIENM'
035200960416     C                   CLEAR                   XTABDS
035300960416     C                   MOVE      *BLANKS       KEYTAB
035400960416     C                   MOVE      '01'          OPZTAB
035500960416     C                   MOVE      'G13'         XTBCOD
035600960416     C                   MOVE      XSCSOC        AZITAB
035700960416     C                   MOVE      XSCLIN        LINTAB
035800960416     C                   CLEAR                   KPJBU
035900960416     C                   MOVEL     XTABDS        KPJBU
036000960416     C                   CALL      'ANGG13R'
036100960416     C                   PARM                    KPJBA
036200960416     C                   MOVEL     KPJBU         XTABDS
036300960416     C*
0364009604162   >C     KEYTAB        IFNE      *BLANK
036500961126    >C                   MOVE      KEYTAB        D1SerieNm
0366009604162-  >C                   ENDIF
036700940223     C*
036800961015    >C** FMTD1  - Tempificatore
036900961015     C     H1NMFL        WHENEQ    'D1TEMPIF'
037000961015     C                   RESET                   Ana964DS
037100961015     C                   MOVEL     '01'          OPZ964
037200961015     C                   MOVEL     'ANDIZ000'    TRC964
037300961015     C                   MOVEL     'DSPTEMPNM'   CAM964
037400961015     C                   MOVEL     ANA964DS      KPJBU
037500961015     C                   CALL      'ANA964R'
037600961015     C                   PARM                    KPJBA
037700961015     C                   MOVEL     KPJBU         ANA964DS
037800961015     C*
037900961015     C     val964        IFNE      *BLANK
038000961015     C                   MOVEL     val964        D1Tempif
038100961015     C                   ENDIF
038200961015     C
038300940223     C* ricerca non abilitata in questa posizione
0384009410071O   C                   OTHER
038500941007     C                   SETON                                        9899
038600940127     C*
0387009410071-   C                   ENDSL
038800961015     C*
038900961015     C* imposto pos. del cursore
039000961015     C                   Z-ADD     CURR          H1RIGA
039100961015     C                   Z-ADD     CURC          H1COLO
039200940127     C*
039300940506     C                   MOVE      *HIVAL        $LASTV
039400940127     C                   ENDSR
039500940131     C/EJECT
039600940224     C************************************************************
039700940131     C* RIEMPIMENTO OPZIONI
039800940224     C************************************************************
039900940131     C     RIEOPZ        BEGSR
040000940223     C*
040100940223     C* Input :
040200940223     C* - $TX = schiera con tutti i testi disponibili
040300940223     C* - $FL = schiere con i flag di validità associati ai testi
040400940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
040500940223     C*          opzione usato
040600940223     C* - WMXO = variabile contenente il numero massimo di testi
040700940223     C*          opzione usabili
040800940721     C* - $L01 = Lunghezza prima riga
040900940721     C* - $L02 = Lunghezza seconda riga
041000940223     C* Output :
041100940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
041200940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
041300950403     C* - FF23  = flag che indica se c'è bisogno del tasto F23
041400940223     C*
041500961003     C                   CALLb     'X04OPMSG'
041600940223     C* input
041700940223     C                   PARM                    $TX
041800940223     C                   PARM                    $FL
041900940225     C                   PARM                    WCOZ              3 0
042000940223     C                   PARM                    WMXO              3 0
042100940223     C* output
042200940223     C                   PARM                    WOPZ1            76
042300940223     C                   PARM                    WOPZ2            76
042400950403     C                   PARM                    FF23              1
042500940721    >C* input
042600940721    >C                   PARM                    $L01              3 0
042700940721    >C                   PARM                    $L02              3 0
042800940223     C*
042900060224A1148C                   PARM                    $ALLOPT         960
043000060224A1148C*
043100940131     C                   ENDSR
043200940506     C/EJECT
043300940506     C************************************************************
043400940506     C* RIEMPIMENTO TASTI FUNZIONE
043500940506     C************************************************************
043600940506     C     RIEKEY        BEGSR
043700940506     C*
043800940506     C* Input :
043900940506     C* - $FX = schiera con tutti i testi disponibili
044000940506     C* - $FC = schiere con i flag di validità associati ai testi
044100940506     C* - $FR = schiere con i flag di REVERSE IMAGE
044200940506     C* - $ULK = ultimo testo caricato
044300940721     C* - $L01 = Lunghezza prima riga
044400940721     C* - $L02 = Lunghezza seconda riga
044500940506     C* Output :
044600940506     C* - $ULK
044700940609     C* - $KEY1= 1° riga di tasti funzione
044800940609     C* - $KEY2= 2° riga di tasti funzione
044900940506     C*
045000961003     C                   CALLb     'XKEYMSG'
045100940506     C* input
045200940506     C                   PARM                    $FX
045300940506     C                   PARM                    $FC
045400940506     C                   PARM                    $FR
045500940506     C                   PARM                    $ULK              3 0
045600940506     C* output
045700940506     C                   PARM                    $KEY1            79
045800940506     C                   PARM                    $KEY2            79
045900940721    >C* input
046000940721    >C                   PARM                    $L01              3 0
046100940721    >C                   PARM                    $L02              3 0
046200940506     C*
046300060224A1148C                   PARM                    $ALLFUNCT       480
046400060224A1148C*
046500940506     C                   ENDSR
046600940224     C/EJECT
046700940224     C************************************************************
046800940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
046900940224     C************************************************************
047000940224     C     GESPIE        BEGSR
047100940224     C*
047200940224     C                   SELECT
047300940224     C* Parzializzazioni
047400940224     C     $GEST         WHENEQ    'D1'
047500940506     C     $ULKD1        IFEQ      0
047600940506     C* riempimento opzioni
047700940506     C                   EXSR      F24D1
047800940506     C                   ELSE
047900940506     C                   WRITE     FMTZ1
048000940506     C                   ENDIF
048100940506     C* Lista
048200940506     C     $GEST         WHENEQ    'S1'
048300940506     C     $ULKS1        IFEQ      0
048400940506     C                   EXSR      F24S1
048500940506     C                   ELSE
048600940506     C                   WRITE     FMTZ2
048700940506     C                   ENDIF
048800940916     C* Modifica valori assunti
048900940916     C     $GEST         WHENEQ    'D2'
049000940916     C     $ULKD2        IFEQ      0
049100940916     C                   EXSR      F24D2
049200940916     C                   ELSE
049300940916     C                   WRITE     FMTZ3
049400940916     C                   ENDIF
049500940506     C                   ENDSL
049600940506     C*
049700940224     C                   ENDSR
049800940224     C/EJECT
049900940224     C************************************************************
050000940224     C* GESTIONE PARZIALIZZAZIONI
050100940224     C************************************************************
050200940224     C     GESD1         BEGSR
050300940224     C* inizializzazione videata
050400940223     C     $INZD1        IFEQ      *ON
050500940127     C                   EXSR      INZD1
050600940223     C                   MOVE      *OFF          $INZD1
050700940117     C                   END
050800940223     C* emissione piede videata se proveniente da altra
050900940223     C* salvataggio valore formato di provenienza
051000940202     C     $LASTV        IFNE      'D1'
051100940223     C* salvo stato F17 per sapere cosa fare in caso di F12
051200940223     C                   MOVE      *INKR         WINKR
051300940202     C                   WRITE     FMTT1
051400940224     C                   EXSR      GESPIE
051500940202     C                   MOVE      $LASTV        $LASTG
051600940202     C                   MOVE      'D1'          $LASTV
051700940707     C                   ENDIF
051800940707     C*
051900940707     C* se esistono errori sulla videata
052000940707     C* emetto la write del formato a indicatori spenti per vedere
052100940707     C* le eventuali decodifiche
052200940707     C     *IN99         IFEQ      *ON
052300940707     C                   MOVEA     *IN           WIN
052400940707     C                   MOVE      *ALL'0'       IN5098           49
052500940707     C                   MOVEA     IN5098        *IN(50)
052600940707     C                   WRITE     FMTD1
052700940707     C                   MOVEA     WIN           *IN
052800940707     C                   ENDIF
052900940131     C*
053000940131     C                   EXFMT     FMTD1
053100940506     C* controllo abilitazione tasti funzione
053200940506     C*
053300940506     C                   EXSR      CTRKD1
053400940506     C*
053500940506     C     *IN99         IFEQ      *OFF
053600940131     C*
053700940117     C                   SELECT
053800940916     C* F2=Decodifica
053900940916     C     $TASTO        WHENEQ    F02
054000940916     C                   EXSR      F02D1
054100940131     C* F3=Fine
054200940506     C     $TASTO        WHENEQ    F03
054300940307     C                   EXSR      F03D1
054400940131     C* F4=Ricerca
054500940506     C     $TASTO        WHENEQ    F04
054600940127     C                   EXSR      SEARCH
054700940916     C* F5=Refresh
054800940916     C     $TASTO        WHENEQ    F05
054900940916     C                   EXSR      F05D1
055000940131     C* F12=Ritorno
055100940506     C     $TASTO        WHENEQ    F12
055200940307     C                   EXSR      F12D1
055300940131     C* F18=Modifica valori assunti
055400940506     C     $TASTO        WHENEQ    F18
055500940307     C                   EXSR      F18D1
055600940224     C* F24=Altri tasti
055700940506     C     $TASTO        WHENEQ    F24
055800940309     C                   EXSR      F24D1
055900940131     C*
056000940117     C                   OTHER
056100940127     C                   EXSR      CTRD1
056200940127     C     *IN99         IFEQ      *OFF
056300940131     C* definisco l'ordinamento
056400941018    >C* leggo in ordine di ordinamento
056500981229E0045C* se nelle parzializzazioni è specificato un valore per D1DRP
056600981229  "  C* forzo $ORD = 1 (gruppo/serie) altrimenti lascio il valore
056700981229  "  C* reperito o quello utilizzato precedentemente
056800981229  "  C     d1grp         ifne      *zero
056900981229  "  C     d1serienm     orne      *blank
057000981229  "  C                   z-add     1             $ord
057100981229  "  C                   endif
057200981229E0045C***                Z-ADD     1             $ORD
057300960416     C* richiamo la lista
057400960416     C                   MOVE      'S1'          $GEST
057500960416     C                   MOVE      *ON           $INZS1
057600960416     C                   MOVE      *BLANK        W1POS
057700941024     C                   END
057800941024     C                   ENDSL
057900940224     C*
058000940506     C                   ENDIF
058100940117     C                   ENDSR
058200940224     C/EJECT
058300940224     C************************************************************
058400940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
058500940224     C************************************************************
058600940127     C     INZD1         BEGSR
058700940318     C*
058800940318     C                   CLEAR                   FMTD1
058900960416     C                   MOVE      XscOn         D1On1
059000960416     C                   MOVE      XscOn         D1On2
059100960416     C                   MOVE      XscOff        D1Off1
059200960416     C                   MOVE      XscOff        D1Off2
059201151118>>>>>C                   EVAL      d1Societa = '*'
059202151118>>>>>C                   EVAL      d1Unita = '*'
059203151118>>>>>C                   EVAL      d1Chiave = '*'
059300940224     C*
059400940117     C                   ENDSR
059500940506     C/EJECT
059600940506     C************************************************************
059700940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
059800940609     C* INPUT
059900940509     C* $FC1 = Abilitazioni videata D!
060000940509     C* OUTPUT
060100940509     C* *IN97 *IN99 Errori
060200940506     C************************************************************
060300940506     C     CTRKD1        BEGSR
060400940506     C                   SETOFF                                       99
060500940506     C                   MOVEA     $FC1          $FC
060600940506     C                   EXSR      CTRKEY
060700940506     C     *IN99         IFEQ      *ON
060800940506     C                   SETON                                        97
060900940506     C                   ENDIF
061000940506     C                   ENDSR
061100940506     C/EJECT
061200940506     C************************************************************
061300940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
061400940506     C************************************************************
061500940506     C     CTRKS1        BEGSR
061600940506     C                   SETOFF                                       99
061700940506     C                   MOVEA     $FC2          $FC
061800940506     C                   EXSR      CTRKEY
061900940506     C     *IN99         IFEQ      *ON
062000940506     C                   SETON                                        97
062100940506     C                   ENDIF
062200940506     C                   ENDSR
062300940506     C/EJECT
062400940506     C************************************************************
062500940506     C* CONTROLLO ABILITAZIONE TASTI
062600940509     C* INPUT
062700940609     C* $TASTO =Tasto funzione premuto
062800940509     C* $FC = Tasti abilitati
062900940509     C* OUTPUT
063000940509     C* IN99 = ERrore
063100940506     C************************************************************
063200940506     C     CTRKEY        BEGSR
063300940506     C                   SELECT
063400940506     C     $TASTO        WHENEQ    F01
063500980717     C                   z-add     01            posfun
063600940720     C     $FC(1)        IFEQ      '0'
063700940506     C                   SETON                                        99
063800940506     C                   ENDIF
063900940506     C     $TASTO        WHENEQ    F02
064000980717     C                   z-add     02            posfun
064100940720     C     $FC(2)        IFEQ      '0'
064200940506     C                   SETON                                        99
064300940506     C                   ENDIF
064400940506     C     $TASTO        WHENEQ    F03
064500980717     C                   z-add     03            posfun
064600940720     C     $FC(3)        IFEQ      '0'
064700940506     C                   SETON                                        99
064800940506     C                   ENDIF
064900940506     C     $TASTO        WHENEQ    F04
065000980717     C                   z-add     04            posfun
065100940720     C     $FC(4)        IFEQ      '0'
065200940506     C                   SETON                                        99
065300940506     C                   ENDIF
065400940506     C     $TASTO        WHENEQ    F05
065500980717     C                   z-add     05            posfun
065600940720     C     $FC(5)        IFEQ      '0'
065700940506     C                   SETON                                        99
065800940506     C                   ENDIF
065900940506     C     $TASTO        WHENEQ    F06
066000980717     C                   z-add     06            posfun
066100940720     C     $FC(6)        IFEQ      '0'
066200940506     C                   SETON                                        99
066300940506     C                   ENDIF
066400940506     C     $TASTO        WHENEQ    F07
066500980717     C                   z-add     07            posfun
066600940720     C     $FC(7)        IFEQ      '0'
066700940506     C                   SETON                                        99
066800940506     C                   ENDIF
066900940506     C     $TASTO        WHENEQ    F08
067000980717     C                   z-add     08            posfun
067100940720     C     $FC(8)        IFEQ      '0'
067200940506     C                   SETON                                        99
067300940506     C                   ENDIF
067400940615     C     $TASTO        WHENEQ    F09
067500980717     C                   z-add     09            posfun
067600940720     C     $FC(09)       IFEQ      '0'
067700940506     C                   SETON                                        99
067800940506     C                   ENDIF
067900940506     C     $TASTO        WHENEQ    F10
068000980717     C                   z-add     10            posfun
068100940720     C     $FC(10)       IFEQ      '0'
068200940506     C                   SETON                                        99
068300940506     C                   ENDIF
068400940506     C     $TASTO        WHENEQ    F11
068500980717     C                   z-add     11            posfun
068600940720     C     $FC(11)       IFEQ      '0'
068700940506     C                   SETON                                        99
068800940506     C                   ENDIF
068900940506     C     $TASTO        WHENEQ    F12
069000980717     C                   z-add     12            posfun
069100940720     C     $FC(12)       IFEQ      '0'
069200940506     C                   SETON                                        99
069300940506     C                   ENDIF
069400940506     C     $TASTO        WHENEQ    F13
069500980717     C                   z-add     13            posfun
069600940720     C     $FC(13)       IFEQ      '0'
069700940506     C                   SETON                                        99
069800940506     C                   ENDIF
069900940506     C     $TASTO        WHENEQ    F14
070000980717     C                   z-add     14            posfun
070100940720     C     $FC(14)       IFEQ      '0'
070200940506     C                   SETON                                        99
070300940506     C                   ENDIF
070400940506     C     $TASTO        WHENEQ    F15
070500980717     C                   z-add     15            posfun
070600940720     C     $FC(15)       IFEQ      '0'
070700940506     C                   SETON                                        99
070800940506     C                   ENDIF
070900940506     C     $TASTO        WHENEQ    F16
071000980717     C                   z-add     16            posfun
071100940720     C     $FC(16)       IFEQ      '0'
071200940506     C                   SETON                                        99
071300940506     C                   ENDIF
071400940506     C     $TASTO        WHENEQ    F17
071500980717     C                   z-add     17            posfun
071600940720     C     $FC(17)       IFEQ      '0'
071700940506     C                   SETON                                        99
071800940506     C                   ENDIF
071900940506     C     $TASTO        WHENEQ    F18
072000980717     C                   z-add     18            posfun
072100940720     C     $FC(18)       IFEQ      '0'
072200940506     C                   SETON                                        99
072300940506     C                   ENDIF
072400940506     C     $TASTO        WHENEQ    F19
072500980717     C                   z-add     19            posfun
072600940720     C     $FC(19)       IFEQ      '0'
072700940506     C                   SETON                                        99
072800940506     C                   ENDIF
072900940506     C     $TASTO        WHENEQ    F20
073000980717     C                   z-add     20            posfun
073100940720     C     $FC(20)       IFEQ      '0'
073200940506     C                   SETON                                        99
073300940506     C                   ENDIF
073400940506     C     $TASTO        WHENEQ    F21
073500980717     C                   z-add     21            posfun
073600940720     C     $FC(21)       IFEQ      '0'
073700940506     C                   SETON                                        99
073800940506     C                   ENDIF
073900940506     C     $TASTO        WHENEQ    F22
074000980717     C                   z-add     22            posfun
074100940720     C     $FC(22)       IFEQ      '0'
074200940506     C                   SETON                                        99
074300940506     C                   ENDIF
074400940506     C     $TASTO        WHENEQ    F23
074500980717     C                   z-add     23            posfun
074600940720     C     $FC(23)       IFEQ      '0'
074700940506     C                   SETON                                        99
074800940506     C                   ENDIF
074900940506     C     $TASTO        WHENEQ    F24
075000980717     C                   z-add     24            posfun
075100940720     C     $FC(24)       IFEQ      '0'
075200940506     C                   SETON                                        99
075300940506     C                   ENDIF
075400940506     C                   ENDSL
075500940506     C                   ENDSR
075600940916     C/EJECT
075700940916     C************************************************************
075800940916     C* GESTIONE F02 VIDEO D1
075900940916     C************************************************************
076000940916     C     F02D1         BEGSR
076100940916     C*
076200940916     C* per decodificare la videata richiamo la routine dei ctrl
076300940916     C* e poi disattivo gli eventuali messaggi di errore
076400940916     C                   EXSR      CTRD1
076500940916     C*
076600940916     C                   MOVE      *ALL'0'       IN5098
076700940916     C                   MOVEA     IN5098        *IN(50)
076800940916     C*
076900940916     C                   ENDSR
077000940224     C/EJECT
077100940131     C************************************************************
077200940307     C* GESTIONE F03 VIDEO D1
077300940131     C************************************************************
077400940307     C     F03D1         BEGSR
077500940131     C*
077600940307     C                   MOVE      *ON           $FINE
077700940325     C* fine programma
077800940325     C                   EXSR      ENDPGM
077900940307     C*
078000940307     C                   ENDSR
078100940916     C/EJECT
078200940916     C************************************************************
078300940916     C* GESTIONE F05 VIDEO D1
078400940916     C************************************************************
078500940916     C     F05D1         BEGSR
078600940916     C*
078700940916     C                   MOVE      *ON           $INZD1
078800940916     C*
078900940916     C                   ENDSR
079000940307     C/EJECT
079100940307     C************************************************************
079200940307     C* GESTIONE F12 VIDEO D1
079300940307     C************************************************************
079400940307     C     F12D1         BEGSR
079500940307     C*
079600940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
079700940307     C*  per F12 torno alla lista
079800940307     C     $LOP          IFEQ      2
079900940307     C     WINKR         OREQ      *ON
080000940307     C                   MOVE      'S1'          $GEST
080100940307     C*  altrimenti, per F12 torno a menu
080200940307     C                   ELSE
080300940307     C                   MOVE      *ON           $FINE
080400940307     C                   END
080500940307     C*
080600940307     C                   ENDSR
080700940307     C/EJECT
080800940307     C************************************************************
080900940307     C* GESTIONE F18 VIDEO D1
081000940307     C************************************************************
081100940307     C     F18D1         BEGSR
081200940307     C*
081300940307     C                   MOVE      'D2'          $GEST
081400940307     C                   MOVEL     *ON           $INZD2
081500940307     C*
081600940307     C                   ENDSR
081700940307     C/EJECT
081800940307     C************************************************************
081900940307     C* GESTIONE F24 VIDEO D1
082000940609     C* input
082100940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
082200940509     C* $FX1 =Testi tasti funzione
082300940509     C* $FC1 = Abilitazioni
082400940509     C* $FR1 = Inversioni di fondo
082500940307     C************************************************************
082600940307     C     F24D1         BEGSR
082700940506     C*
082800940506     C                   MOVEA     $FX1          $FX
082900940506     C                   MOVEA     $FC1          $FC
083000940506     C                   MOVEA     $FR1          $FR
083100940506     C                   Z-ADD     $ULKD1        $ULK
083200940929     C                   Z-ADD     79            $L01
083300940929     C                   Z-ADD     79            $L02
083400940506     C                   EXSR      RIEKEY
083500940506     C                   Z-ADD     $ULK          $ULKD1            3 0
083600940720     C                   MOVEA     $FC           $FC1
083700940506     C                   MOVEL     $KEY1         Z1KE1
083800060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
083900940506     C                   MOVEL     $KEY2         Z1KE2
084000940506     C                   WRITE     FMTZ1
084100940506     C*
084200940307     C*
084300940307     C                   ENDSR
084400940307     C/EJECT
084500940307     C************************************************************
084600940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
084700940307     C************************************************************
084800961015     C     CTRD1         BEGSR
084900940307     C*
085000940127     C                   SETOFF                                       99
085100940127     C* non imposto pos. del cursore
085200940127     C                   Z-ADD     0             H1RIGA
085300940127     C                   Z-ADD     0             H1COLO
085400040929A1108C* se presente tempificatore ci deve essere anche il tipo
085500040929  "  C                   IF        D1Tempo <> *blanks and d1tempif = *blanks
085600040929  "  C                   SETON                                        6499
085700040929A1108C                   ENDIF
085800940131     C*
085900960416     C* flag abilitazione a iva valido
086000960416     C                   IF        D1IVA <> XscOn and D1IVA <> XscOff and
086100960416     C                             D1IVA <> *blank
086200960416     C                   SETON                                        6099
086300960416     C                   ENDIF
086400960416     C*
086500960416     C* flag abilitazione a registrazioni valido
086600960416     C                   IF        D1Reg <> XscOn and D1Reg <> XscOff and
086700960416     C                             D1Reg <> *blank
086800960416     C                   SETON                                        6199
086900960416     C                   ENDIF
087000940131     C*
087100961015     C* Tempificazione numeratore
087200961015     C                   Clear                   D1TempifDE
087300961015     C                   IF        D1Tempif<>*blank
087400961015     C                   RESET                   X07ValDS
087500961015     C                   MOVE      '1'           X07RIC
087600961015     C                   MOVEL     'ANDIZ000'    X07TRC
087700961015     C                   MOVEL     'DSPTEMPNM'   X07CAM
087800961015     C                   MOVEL     D1Tempif      X07VAL
087900961015     C                   CALLb     'X07VALR'
088000961015     C                   PARM                    X07ValDS
088100961015   B3C     X07ERR        IFEQ      '0'
088200961015     C                   MOVEL     X07DES        D1TempifDe
088300961015   X3C                   ELSE
088400961015     C                   Seton                                        6399
088500961015   E3C                   ENDIF
088600961015     C*
088700961015     C                   Endif
088800961015     C*
088900940117     C                   ENDSR
089000940224     C/EJECT
089100940127     C************************************************************
089200940131     C* GESTIONE LISTA
089300940127     C************************************************************
089400940127     C     GESS1         BEGSR
089500940223     C* inizializzazione videata
089600940223     C     $INZS1        IFEQ      *ON
089700940127     C                   EXSR      INZS1
089800940223     C                   MOVE      *OFF          $INZS1
089900940127     C                   ENDIF
090000940223     C* emissione piede videata se proveniente da altra
090100940223     C* salvataggio valore formato di provenienza
090200940202     C     $LASTV        IFNE      'S1'
090300940202     C                   WRITE     FMTT1
090400940224     C                   EXSR      GESPIE
090500940202     C                   MOVE      $LASTV        $LASTG
090600940202     C                   MOVE      'S1'          $LASTV
090700940202     C                   END
090800940223     C*
090900940223     C     WMAX          IFEQ      0
091000940211     C                   WRITE     FMTD3
091100940211     C                   ENDIF
091200980717     C*
091300980717     C* Inizializzazione indice tasti funzione personalizzati
091400980717     C                   Eval      PosFun = 25
091500980717     C*
091600940831     C*Serve per emissione finestra pgm invio ang727r (per open/close
091700940831     C*window)
091800940831     C                   MOVE      '1'           WIN727            1
091900940127     C*
092000940131     C                   EXFMT     FMTC1
092100940204     C     C1NRR         IFNE      0
092200940204     C                   Z-ADD     C1NRR         WSFL
092300940204     C                   ENDIF
092400940127     C                   Z-ADD     SFLNRR        C1RCD
092500940506     C*
092600940506     C* controllo abilitazione tasti funzione
092700940506     C*
092800940506     C                   EXSR      CTRKS1
092900940506     C     *IN99         IFEQ      *OFF
093000940506     C*
093100940127     C*
0932009401271    C                   SELECT
093300940127     C* F3=Fine
093400940506     C     $TASTO        WHENEQ    F03
093500940309     C                   EXSR      F03S1
093600940131     C* F10=Immissione
093700940506     C     $TASTO        WHENEQ    F10
093800940309     C                   EXSR      F10S1
093900940131     C* F12=Ritorno
094000940506     C     $TASTO        WHENEQ    F12
094100940309     C                   EXSR      F12S1
094200940131     C* F17=Parzializzazioni
094300940506     C     $TASTO        WHENEQ    F17
094400940309     C                   EXSR      F17S1
094500940131     C* F18=Modifica valori assunti
094600940506     C     $TASTO        WHENEQ    F18
094700940309     C                   EXSR      F18S1
094800940224     C* F23=Altre opzioni
094900940506     C     $TASTO        WHENEQ    F23
095000940309     C                   EXSR      F23S1
095100940224     C* F24=Altri tasti
095200940506     C     $TASTO        WHENEQ    F24
095300940309     C                   EXSR      F24S1
095400940127     C* ROLL-UP
095500940506     C     $TASTO        WHENEQ    ROLLUP
095600940127     C                   EXSR      ROLS1
095700980717     C*
095800980717     C* Personalizzata
095900980717     C     $PGF2(PosFun) Whenne    *BLANK
096000980717     C                   EXSR      FperS1
096100980717     C*
0962009401271O   C                   OTHER
096300940127     C* CONTROLLO DATI
096400940131     C                   EXSR      CTRC1
096500940201     C     *IN99         IFEQ      *OFF
096600940131     C                   EXSR      CTRS1
096700940131     C                   END
0968009401271-   C                   ENDSL
096900940127     C*
097000940506     C                   ENDIF
097100940127     C                   ENDSR
097200940224     C/EJECT
097300980717     C************************************************************
097400980717     C* Chiamata programma con tasto personalizzato
097500980717     C************************************************************
097600980717     C     FPerS1        BEGSR
097700980717     C*
097800980717     C* salvo la DS di ingresso
097900980717    >C                   MOVEL     A103DS        $SaveDS
098000980717     C*
098100980717    >C                   MOVEL     A103DS        KPJBU
098200980717    >C                   CALL      $PGF2(PosFun)                        22
098300980717     C                   PARM                    KPJBA
098400980717     C                   PARM                    NOMPGM
098500980717     C*
098600980717     C* ripristino la DS di ingresso
098700980717    >C                   MOVEL     $SaveDS       A103DS
098800980717     C*
098900980717     C                   ENDSR
099000980717     C/EJECT
099100940127     C************************************************************
099200940131     C* INIZIALIZZAZIONE LISTA
099300940127     C************************************************************
099400940127     C     INZS1         BEGSR
099500940302     C* pulizia SFL
099600940128     C                   SETOFF                                         3031
099700940131     C                   WRITE     FMTC1
099800940128     C                   SETON                                          31
099900940127     C*
100000940128     C* riempimento opzioni
100100940223     C                   MOVEA     $TX1          $TX
100200940223     C                   MOVEA     $FL1          $FL
100300940309     C                   Z-ADD     0             WCOZ
100400940225     C*  equivale al num.di elementi della schiera $TX1
100500980717     C                   Z-ADD     OPZLEN        WMXO
100600950403     C                   Z-ADD     76            $L01
100700950403     C                   Z-ADD     76            $L02
100800940128     C                   EXSR      RIEOPZ
100900950403     C                   Z-ADD     23            X
101000950403     C                   MOVE      FF23          $FC2(X)
101100940608     C                   MOVEL     WOPZ1         C1OPZ1
101200060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
101300940608     C                   MOVEL     WOPZ2         C1OPZ2
101400940225     C                   Z-ADD     WCOZ          C1COZ
101500940128     C*
101600940128     C* CARICAMENTO SFL A PAGINE
101700940128     C*
101800940201     C                   Z-ADD     0             S1NRR
101900940128     C                   Z-ADD     0             WMAX
102000940224     C*
102100940224     C* Posizionamento su file pilota
102200940302     C                   CLEAR                   C1POS
102300941006     C*
102400941005    >C                   EXSR      SETUTN
102500940302     C* ripristino i valori del posizionamento
102600941006     C                   MOVE      W1POS         C1POS
102700940131     C*
102800940201     C* Lettura file pilota
102900941024    >C                   EXSR      REDNMR
103000940131     C*
103100940131     C* Carico 1a pagina
103200940127     C                   EXSR      ROLS1
103300940201     C                   Z-ADD     SFLPAG        WPAG
103400940127     C*
103500940127     C                   ENDSR
103600940127     C/EJECT
103700940127     C************************************************************
103800940131     C* CARICAMENTO PAGINA LISTA
103900940127     C************************************************************
104000940127     C     ROLS1         BEGSR
104100940127     C*
104200940128     C                   SETOFF                                       32
104300940223     C                   Z-ADD     0             Y
104400940127     C                   Z-ADD     WMAX          S1NRR
104500940127     C*
104600940127     C* Leggo dal file anagrafico per caricare la lista
104700940127     C*
1048009401311    C     $EFILE        DOWEQ     *OFF
104900940201     C     Y             ANDLT     WPAG
105000940127     C*
105100940131     C                   EXSR      RIES1
105200940127     C*
105300940127     C                   ADD       1             S1NRR
105400940127     C                   ADD       1             Y
105500940127     C*
105600940131     C                   WRITE     FMTS1
105700940131     C*
105800941024    >C                   EXSR      REDNMR
105900940128     C*
1060009401271-   C                   ENDDO
106100940127     C*
106200940223     C                   Z-ADD     S1NRR         WMAX                 30
106300940127     C*
106400940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
106500940127     C*
106600940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
106700940127     C                   MVR                     RESTO             3 0
106800940131     C     PAGINE        MULT      SFLPAG        C1RCD
1069009401271    C     RESTO         IFGT      0
107000940127     C                   ADD       1             C1RCD
1071009401271E   C                   ELSE
107200940407     C                   SUB       SFLPAG        C1RCD
107300940407     C                   ADD       1             C1RCD
1074009401271-   C                   ENDIF
107500940128     C*
107600940127     C                   ENDSR
107700940224     C/EJECT
107800940224     C************************************************************
107900940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
108000940224     C************************************************************
108100941005    >C     SETUTN        BEGSR
108200940224     C*
108300940224     C                   SELECT
108400941005    >C* Ord.1 = Profilo
108500940224     C     $ORD          WHENEQ    1
108600940314    >C*-----------------------------------------------------------
108700940314    >C* Gestione posizionamento
108800940314    >C* Se il campo W1POS è > *blank imposto il posizionamento con
108900940314    >C* W1POS altrimenti lo imposto con il valore di parametrizzazione
109000940314    >C*-----------------------------------------------------------
109100981229E0045C***  *LOVAL        SETLL     ANNMR02L
109200981229  "  C                   z-add     d1grp         nmrgru
109300981229  "  C                   movel     d1serienm     nmrser
109400981229  "  C     K2nmr2        setll     annmr02
109500981229  "  C*
109600981229  "  C     $ord          wheneq    2
109700981229  "  C                   movel     xscsoc        nmrsoc
109800981229  "  C                   movel     *blank        nmruni
109900981229  "  C                   movel     *blank        nmrctb
110000981229  "  C                   movel     *blank        nmrkey
110100981229  "  C                   movel     d1serienm     nmrser
110200981229  "  C                   movel     *blank        nmrtmp
110300981229  "  C     K6nmr1        setll     annmr01
110400981229E0045C*
110500941018     C                   ENDSL
110600940224     C*
110700940224     C                   ENDSR
110800940128     C/EJECT
110900940128     C************************************************************
111000940131     C* LETTURA RCD ARCHIVIO PILOTA
111100940128     C************************************************************
111200941024    >C     REDNMR        BEGSR
111300940128     C*
111400940131     C                   MOVEL     *OFF          $EFILE
111500940131     C                   MOVEL     *OFF          $RCDOK
111600940131     C*
1117009401311    C     $EFILE        DOUEQ     *ON
111800940131     C     $RCDOK        OREQ      *ON
111900940131     C*
1120009401312    C                   SELECT
112100940131     C     $ORD          WHENEQ    1
112200981229E0045C***                READ      ANNMR02L                               21
112300981229  "  C                   read      annmr02                                21
112400981229  "  C*
112500981229  "  C     $ord          wheneq    2
112600981229  "  C                   read      annmr01                                21
112700981229E0045C*
1128009401312-   C                   ENDSL
112900940131     C*
113000940131     C                   MOVE      *IN21         $EFILE
113100940131     C*
1132009401312    C     $EFILE        IFEQ      *OFF
113300960416     C* reperisco i dati della serie
113400960416     C                   MOVE      '1'           XTBRIC
113500960416     C                   MOVE      XSCSOC        XTBAZI
113600960416     C                   MOVE      'G13'         XTBCOD
113700961015     C                   MOVE      *ZERO         XTBKEY
113800960416     C                   MOVE      NMRSer        XTBKEY
113900961003     C                   CALLb     'XATB'
114000960416     C                   PARM                    XATBDS
114100960416     C     XTBERR        IFEQ      '0'
114200960416     C                   MOVEL     XTBUNI        TABG13
114300960416     C                   ELSE
114400960416     C                   RESET                   TABG13
114500960416     C                   ENDIF
114600960416     C*
114700941024     C                   EXSR      SELNMR
1148009401312-   C                   ENDIF
114900940131     C*
1150009401311-   C                   ENDDO
115100940131     C*
115200940131     C                   MOVE      *IN21         *IN33
115300940131     C*
115400940131     C                   ENDSR
115500940224     C/EJECT
115600940224     C************************************************************
115700940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
115800940224     C************************************************************
115900940224     C     CLCPAG        BEGSR
116000940224     C* Input :
116100940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
116200940224     C*          cursore
116300940224     C* - SFLPAG = numero rcd per pagina sfl
116400940224     C* Output :
116500940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
116600940224     C*
116700940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
116800940224     C                   MVR                     RESTO             3 0
116900940224     C     RESTO         IFGT      0
117000940224     C                   ADD       1             PAGINE
117100940224     C                   ENDIF
117200940224     C     PAGINE        MULT      SFLPAG        WPAG
117300940224     C*
117400940224     C                   ENDSR
117500940131     C/EJECT
117600940131     C************************************************************
117700940131     C* SELEZIONI SULLA RIGA LETTA
117800940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
117900940314     C* non rientra nei parametri di parzializzazione
118000940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
118100940314     C* superiore stabilito nei parametri
118200940314     C*
118300940131     C************************************************************
118400941024    >C     SELNMR        BEGSR
118500940131     C*
118600940202     C                   MOVEL     *ON           $RCDOK
118700940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
118800940131A1211C                   MOVEL     *hival        ALL9             15
118900940314     C* creo limite massimo parzializzazioni
119000960416    >C     D1SerieNm     CAT       ALL9:0        SerieNm999
119100960416     C*
119200940314     C                   SELECT
119300940314     C*
119400940314     C* SELEZIONI RISPETTO AI VALORI RCD
119500940314     C*
119600940314    >C* << elaborare eventuali selezioni che non riguardano le
119700940316    >C*    parzializzazioni ma più propriamente i dati del rcd ;
119800940314    >C*    esempio : esclusione degli annullati. >>
119900941005     C*
120000940314     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
120100941024     C     NMRSOC        WHENNE    *BLANKS
120200941103     C     NMRSOC        ANDNE     XSCSOC
120300950308     C                   MOVEL     *OFF          $RCDOK
120400940314     C*
120500960416     C     NMRSer        WHENLT    D1SerieNm
120600960416     C     NMRSer        ORGT      SerieNm999
120700960416     C                   MOVEL     *OFF          $RCDOK
120800960416     C*
120900960416     C     D1IVA         WHENNE    IVAG13
121000960416     C     D1IVA         ANDNE     *blank
121100960416     C                   MOVEL     *OFF          $RCDOK
121200960416     C*
121300960416     C     D1Reg         WHENNE    RegG13
121400960416     C     D1Reg         ANDNE     *blank
121500960416     C                   MOVEL     *OFF          $RCDOK
121600960416     C*
121700961015     C     D1Tempif      WHENNE    TpmG13
121800961015     C     D1Tempif      ANDNE     *blank
121900961015     C                   MOVEL     *OFF          $RCDOK
122000981229E0045C*
122100981229  "  C     D1grp         WHENNE    *zero
122200981229  "  C     nmrgru        ANDNE     d1grp
122300981229E0045C                   MOVEL     *OFF          $RCDOK
122400961015     C*
122401151118>>>>>C                   WHEN      d1Societa <> '*' AND d1Societa <> nmrSoc
122402151118>>>>>C                   EVAL      $rcdOk = *OFF
122403151118>>>>>C*
122404151118>>>>>C                   WHEN      d1Unita <> '*' AND d1Unita <> nmrUni
122405151118>>>>>C                   EVAL      $rcdOk = *OFF
122407151118>>>>>C*
122408151118>>>>>C                   WHEN      d1Chiave <> '*' AND d1Chiave <> nmrKey
122409151118>>>>>C                   EVAL      $rcdOk = *OFF
122411151118>>>>>C*
122500940202     C                   ENDSL
122600040929A1108C*
122700040929  "  C     D1tempo       ifne      *blanks
122800040929  "  C     ' '           CHECK     D1tempo       NR                3 0
122900040929  "  C                   Z-ADD     NR            J                 2 0
123000040929  "  C                   if        %subst(d1tempo:J)<> %subst(nmrtmp:J)
123100040929  "  C                   MOVEL     *OFF          $RCDOK
123200040929  "  C                   ENDIF
123300040929A1108C                   ENDIF
123400940202     C*
123500940202     C                   ENDSR
123600940131     C/EJECT
123700940131     C************************************************************
123800940131     C* RIEMPIMENTO RIGA LISTA
123900940131     C************************************************************
124000940131     C     RIES1         BEGSR
124100940131     C*
124200940131     C                   CLEAR                   FMTS1
124300940314    >C* << riempio i campi del sfl, compresi gli hidden con i valori
124400940314    >C*    del rcd letto >>
124500940131     C*
124600941018    >C                   MOVEL     NMRSOC        S1SOC
124700941018    >C                   MOVEL     NMRUNI        S1UNI
124800981228E0045C                   z-add     nmrgru        S1gruppo
124900941018    >C                   MOVEL     NMRCTB        S1CTB
125000941018    >C                   MOVEL     NMRKEY        S1KEY
125100941018    >C                   MOVEL     NMRSER        S1SER
125200941018    >C                   Z-ADD     NMRNUM        S1NUM
125300961003    XC                   CALLb     'XDT4000'
125400941026     C                   PARM      NMRDAT        XDTAMG           10
125500941026     C                   PARM      *ZEROS        XDTGMA            6 0
125600941026     C                   PARM      2             XDTSTA            1 0
125700941026     C                   MOVE      XDTGMA        S1DAT
125800941025     C*
125900960416     C*---------------------------------------------------------------------
126000941025     C*Aggancio tabella g13 per tempificazione
126100941025     C*
126200960416     C* non ce n'è più bisogno perchè è stata spostata nella routine REDNMR
126300960416     C*                  MOVE      '1'           XTBRIC
126400960416     C*                  MOVE      XSCSOC        XTBAZI
126500960416     C*                  MOVE      *ZERO         XTBKEY
126600960416     C*                  MOVE      'G13'         XTBCOD
126700960416     C*                  MOVE      S1SER         XTBKEY
126800961003     C*                  CALLb     'XATB'
126900960416     C*                  PARM                    XATBDS
127000960416B1   C*    XTBERR        IFEQ      '0'
127100960416     C*                  MOVEL     XTBUNI        TABG13
127200960416     C*---------------------------------------------------------------------
127300941025     C                   RESET                   DSUNO
127400941025     C                   RESET                   DSDUE
127500950104     C                   MOVE      NMRTMP        H1TMP
127600941025     C* Giornaliero, mensile, annuale
127700941025     C     TPMG13        IFEQ      '1'
127800941025     C     TPMG13        OREQ      '3'
127900941025     C     TPMG13        OREQ      '4'
128000941025    >C                   MOVEL     NMRTMP        DSGG
128100941025     C                   MOVEL     NMRTMP        WRK4              4
128200941025     C                   MOVE      WRK4          DSMM
128300941025    >C                   MOVE      NMRTMP        DSAA
128400941025     C     TPMG13        IFEQ      '3'
128500941025     C                   MOVE      *BLANKS       DSBAR
128600941025     C                   END
128700941025     C     TPMG13        IFEQ      '4'
128800941025     C                   MOVE      *BLANKS       DSBAR
128900941025     C                   MOVE      *BLANKS       DSBAR1
129000941025     C                   END
129100941025     C                   MOVE      DSUNO         S1TMP
129200941025     C                   END
129300941025     C* esercizio
129400941025     C     TPMG13        IFEQ      '2'
129500941025     C                   MOVE      NMRTMP        WRK5              5
129600941025     C                   MOVEL     WRK5          DSAAE
129700941025     C                   MOVE      WRK5          DSPRE
129800941025     C                   MOVE      DSDUE         S1TMP
129900941025     C                   END
130000941025     C* progressivo
130100941025     C     TPMG13        IFEQ      '5'
130200941025     C                   MOVE      NMRTMP        S1TMP
130300941025     C                   END
130400940131     C*
130500960416     C                   MOVE      DESG13        S1SerieNmD
130600960416E1   C******             END
130700960416     C*
130800940131     C                   ENDSR
130900940309     C/EJECT
131000940309     C************************************************************
131100940309     C* GESTIONE F03 VIDEO S1
131200940309     C************************************************************
131300940309     C     F03S1         BEGSR
131400940309     C*
131500940309     C                   MOVE      *ON           $FINE
131600940325     C* fine programma
131700940325     C                   EXSR      ENDPGM
131800940309     C*
131900940309     C                   ENDSR
132000940309     C/EJECT
132100940309     C************************************************************
132200940309     C* GESTIONE F10 VIDEO S1
132300940609     C* AGGIUNTA RECORD
132400940309     C************************************************************
132500940309     C     F10S1         BEGSR
132600940309     C*
132700941018    >C                   RESET                   A104DS
132800941018    >C                   MOVEL     '01'          OPZ104
132900941018    >C                   MOVEL     A104DS        KPJBU
133000941018    >C                   CALL      'ANA104R'
133100940309     C                   PARM                    KPJBA
133200941018    >C                   MOVEL     KPJBU         A104DS
133300940309     C* ritorno da PGM gestione
133400941018    >C                   EXSR      RTN104
133500940309     C     WINZS1        IFEQ      *ON
133600940309     C                   MOVE      *ON           $INZS1
133700940309     C* se esistevano già righe sul sfl
133800940309     C* calcolo pagina a cui deve posizionarsi
133900940309     C     WSFL          IFGT      0
134000940309     C                   EXSR      CLCPAG
134100940309     C* altrimenti carico solo la 1a pagina
134200940309     C                   ELSE
134300940309     C                   Z-ADD     SFLPAG        WPAG
134400940309     C                   ENDIF
134500940309     C                   ENDIF
134600940309     C*
134700940627     C                   MOVE      *HIVAL        $LASTV
134800940627     C*
134900940309     C                   ENDSR
135000940309     C/EJECT
135100940309     C************************************************************
135200940309     C* GESTIONE F12 VIDEO S1
135300940309     C************************************************************
135400940309     C     F12S1         BEGSR
135500940309     C*
135600940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
135700940309     C     $LOP          IFEQ      1
135800940309     C                   MOVE      'D1'          $GEST
135900940309     C*  se la 1a videata è questa, per F12 torno a menu
136000940309     C                   ELSE
136100940309     C                   MOVE      *ON           $FINE
136200940309     C                   END
136300940309     C*
136400940309     C                   ENDSR
136500940309     C/EJECT
136600940309     C************************************************************
136700940309     C* GESTIONE F17 VIDEO S1
136800940309     C************************************************************
136900940309     C     F17S1         BEGSR
137000940309     C*
137100940309     C                   MOVE      'D1'          $GEST
137200940831     C* forza certi valori nel caso il pgm sia stato richiamato con
137300940831     C* chiave specifica
137400940309     C*
137500940309     C                   ENDSR
137600940309     C/EJECT
137700940309     C************************************************************
137800940309     C* GESTIONE F18 VIDEO S1
137900940309     C************************************************************
138000940309     C     F18S1         BEGSR
138100940309     C*
138200940309     C                   MOVE      'D2'          $GEST
138300940309     C                   MOVEL     *ON           $INZD2
138400940309     C*
138500940309     C                   ENDSR
138600940309     C/EJECT
138700940309     C************************************************************
138800940309     C* GESTIONE F23 VIDEO S1
138900940309     C************************************************************
139000940309     C     F23S1         BEGSR
139100940309     C*
139200950403     C                   Z-ADD     76            $L01
139300950403     C                   Z-ADD     76            $L02
139400940309     C                   EXSR      RIEOPZ
139500950403     C                   Z-ADD     23            X
139600950403     C                   MOVE      FF23          $FC2(X)
139700940608     C                   MOVEL     WOPZ1         C1OPZ1
139800060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
139900940608     C                   MOVEL     WOPZ2         C1OPZ2
140000940309     C                   Z-ADD     WCOZ          C1COZ
140100940309     C*
140200940309     C                   ENDSR
140300940309     C/EJECT
140400940309     C************************************************************
140500940309     C* GESTIONE F24 VIDEO S1
140600940309     C************************************************************
140700940309     C     F24S1         BEGSR
140800940309     C*
140900940506     C*
141000940506     C                   MOVEA     $FX2          $FX
141100940506     C                   MOVEA     $FC2          $FC
141200940506     C                   MOVEA     $FR2          $FR
141300940506     C                   Z-ADD     $ULKS1        $ULK
141400940929     C                   Z-ADD     79            $L01
141500940929     C                   Z-ADD     79            $L02
141600940506     C                   EXSR      RIEKEY
141700940506     C                   Z-ADD     $ULK          $ULKS1            3 0
141800940720     C                   MOVEA     $FC           $FC2
141900940506     C                   MOVEL     $KEY1         Z2KE1
142000060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
142100940506     C                   MOVEL     $KEY2         Z2KE2
142200940506     C                   WRITE     FMTZ2
142300940506     C*
142400940309     C                   ENDSR
142500940128     C/EJECT
142600940128     C************************************************************
142700940131     C* CONTROLLO TESTATA LISTA
142800940128     C************************************************************
142900940131     C     CTRC1         BEGSR
143000940128     C*
143100940201     C                   MOVE      *OFF          *IN99
143200940131     C*
143300940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
143400940202     C* L'INIZIALIZZAZIONE LISTA
143500940202     C*
143600941006     C                   SELECT
143700941006      *
143800941006      *
143900941018    >C     C1POS         WHENNE    W1POS
144000940202     C                   EXSR      POSS1
144100941006     C                   ENDSL
144200940202     C*
144300940202     C                   ENDSR
144400940202     C/EJECT
144500940202     C************************************************************
144600940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
144700940202     C************************************************************
144800940202     C     POSS1         BEGSR
144900940202     C*
145000940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
145100940202     C*
1452009402021    C     WMAX          IFGT      0
145300940202     C                   READC     FMTS1                                  21
1454009402022    C     *IN21         DOWEQ     *OFF
145500940202     C     *IN99         ANDEQ     *OFF
145600940202     C*
1457009402023    C     S1OPZ         IFNE      0
145800940202     C                   SETON                                        51  99
1459009402023-   C                   ENDIF
146000940202     C* se rilevata opzione attiva, attivo sflnxtchg
1461009402023    C     *IN99         IFEQ      *ON
146200940202     C                   MOVE      *ON           *IN32
146300940202     C                   UPDATE    FMTS1
1464009402023E   C                   ELSE
146500940202     C                   READC     FMTS1                                  21
1466009402023-   C                   ENDIF
146700940202     C*
1468009402022-   C                   ENDDO
146900940202     C*
1470009402021-   C                   ENDIF
147100940202     C*
147200940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
147300940221     C* AL VALORE DI POSIZIONAMENTO :
147400940221     C* se questo è maggiore del valore del rispettivo campo di
147500940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
147600940221     C* con i valori di parzializzazione.
147700940221     C*
1478009402021    C     *IN99         IFEQ      *OFF
147900940721    >C                   MOVE      *BLANK        W1POS
148000940202     C*
1481009402022    C                   SELECT
148200940202     C*
148300940202     C     $ORD          WHENEQ    1
148400940131     C*
148500940202     C                   MOVE      *ON           *IN99
148600940223     C                   MOVE      *ON           $INZS1
148700940831     C*
148800940831     C     $ORD          WHENEQ    99
148900940831     C*
149000940831     C                   MOVE      *ON           *IN99
149100940831     C                   MOVE      *ON           $INZS1
149200940202     C*
1493009402022-   C                   ENDSL
149400940202     C*
1495009402021-   C                   ENDIF
149600940202     C*
149700940131     C                   ENDSR
149800940131     C/EJECT
149900940131     C************************************************************
150000940131     C* CONTROLLO OPZIONI LISTA
150100940131     C************************************************************
150200940131     C     CTRS1         BEGSR
150300940131     C*
150400940202     C                   MOVEL     *OFF          $ESCI
150500940201     C                   SETOFF                                       99
150600940131     C                   Z-ADD     0             S1OPZ
150700940131     C*
150800940127     C* Leggo il sfl solo se ci sono rcd
1509009401311    C     WMAX          IFGT      0
151000940131     C                   READC     FMTS1                                  21
151100940127     C*
151200940131     C* esce se fine sfl o errore che richiede l'uscita
1513009401312    C     *IN21         DOWEQ     *OFF
151400940131     C     $ESCI         ANDEQ     *OFF
151500940201     C                   Z-ADD     S1NRR         C1RCD
151600940131     C* ctrl su riga
151700940131     C                   EXSR      RECS1
151800940131     C* gestione opzioni
1519009401313    C     S1OPZ         IFNE      0
152000940201     C     *IN99         ANDEQ     *OFF
152100940131     C                   EXSR      OPZS1
1522009401273-   C                   ENDIF
152300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
1524009402013    C     *IN99         IFEQ      *ON
152500940131     C                   MOVE      *ON           *IN32
1526009402233-   C                   ENDIF
152700940223     C*
152800940131     C                   Z-ADD     0             S1OPZ
152900940223     C*
153000940127     C                   UPDATE    FMTS1
153100941125     C                   MOVE      *OFF          *IN32
153200940223     C*
153300940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1534009401313    C     $ESCI         IFEQ      *OFF
153500940131     C                   READC     FMTS1                                  21
153600940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1537009402014    C     *IN21         IFEQ      *ON
153800940201     C                   MOVE      WINZS1        $INZS1
153900940204     C* calcolo pagina a cui deve posizionarsi
154000940728     C     WSFL          IFGT      0
154100940728     C                   EXSR      CLCPAG
154200940728     C                   ELSE
154300940728     C                   Z-ADD     SFLPAG        WPAG
154400940728     C                   ENDIF
1545009402014-   C                   ENDIF
1546009402013-   C                   ENDIF
154700940131     C*
1548009401272-   C                   ENDDO
154900940127     C*
1550009401311-   C                   ENDIF
155100940131     C*
155200940127     C                   ENDSR
155300940127     C/EJECT
155400940127     C************************************************************
155500940131     C* CONTROLLO CAMPI I/O RIGA LISTA
155600940127     C************************************************************
155700940131     C     RECS1         BEGSR
155800940131     C*
155900940201     C* reset indicatori DSPATR
156000940201     C                   MOVE      *ALL'0'       IN4049           10
156100940201     C                   MOVEA     IN4049        *IN(40)
156200940201     C*
156300940131     C* Controllo se Utente Autorizzato alla scelta
156400940202     C     S1OPZ         IFNE      0
156500940131     C                   Z-ADD     1             X
156600940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
156700980717     C* mi salvo l'indice dell' opzione
156800980717     C   21              Z-add     X             PosOpz            2 0
156900940211     C     *IN21         IFEQ      *OFF
157000940223     C     $FL1(X)       OREQ      'N'
157100940131     C                   MOVE      *ON           $ESCI
157200940201     C                   SETON                                        504099
157300940131     C                   ENDIF
157400940202     C                   ENDIF
157500030225     c*
157600030225A0994C* Controlli autorizzazioni
157700030225  "  c                   exsr      RepAut
157800030225  "  C                   if        *in99=*on
157900030225  "  C                   MOVE      *ON           $ESCI
158000030225  "  C                   SETON                                        5440
158100030225A0994c                   endif
158200940131     C*
158300940131     C                   ENDSR
158400940224     C/EJECT
158500940131     C************************************************************
158600940131     C* GESTIONE OPZIONI LISTA
158700940131     C************************************************************
158800940131     C     OPZS1         BEGSR
158900940201     C*
1590009402251    C                   SELECT
159100940225     C*
159200940225     C* se opz=Selezione passo la chiave al pgm chiamante
159300940225     C     S1OPZ         WHENEQ    1
159400941018    >C* << passare nei campi appositi della DS D103DS il valore
159500940314    >C*    della chiave della VL univoca (se c'è) >>
159600941018    >C                   MOVE      S1OPZ         OPZ103
159700941018    >C                   MOVE      S1SER         SER103
159800941025    >C                   MOVE      S1SOC         SOC103
159900941025    >C                   MOVE      S1UNI         UNI103
160000941025    >C                   MOVE      S1CTB         CTB103
160100941025    >C                   MOVE      S1KEY         KEY103
160200981229E0045C                   z-add     S1gruppo      GRP103
160300940225     C                   MOVE      *ON           $ESCI
160400940225     C                   MOVE      *ON           $FINE
160500940909     C*
160600940909     C                   MOVE      *HIVAL        $LASTV
160700980717     C*
160800980717     C* se selezionata opzione personalizzata, richiamo il programma
160900980717     C* corrispondente
161000980717     C     $PGO1(PosOpz) Whenne    *Blank
161100980717     C                   RESET                   A104DS
161200980717     C* << passare nei campi appositi della DS A043DS il valore
161300980717     C*    della chiave della VL univoca (se c'è) >>
161400980717     C                   MOVEL     S1OPZ         OPZ104
161500980717     C* passare nei campi appositi della DS D002DS il valore
161600980717     C* della chiave della VL univoca
161700980717    >C                   MOVEL     S1OPZ         OPZ104
161800980717    >C                   MOVEL     S1SER         SER104
161900980717    >C                   MOVE      S1SOC         SOC104
162000980717    >C                   MOVE      S1UNI         UNI104
162100980717    >C                   MOVE      S1CTB         CTB104
162200980717    >C                   MOVE      S1KEY         KEY104
162300980717    >C                   MOVE      H1TMP         TMP104
162400981229E0045C                   z-add     s1gruppo      GRP104
162500980717     C*
162600980717     C                   MOVEL     A104DS        KPJBU
162700980717     C*
162800980717     C                   CALL      $PGO1(PosOpz)                        22
162900980717     C                   PARM                    KPJBA
163000980717     C                   PARM                    NOMPGM
163100980717     C*
163200980717     C                   MOVEL     KPJBU         A104DS
163300980717     C*
163400980717     C* ritorno da PGM
163500980717     C                   EXSR      RTN104
163600980717     C*
163700940831     C*
1638009402251O   C                   OTHER
163900940225     C*
164000941018    >C                   RESET                   A104DS
164100941018    >C* << passare nei campi appositi della DS D104DS il valore
164200940314    >C*    della chiave della VL univoca (se c'è) >>
164300941018    >C                   MOVEL     S1OPZ         OPZ104
164400941018    >C                   MOVEL     S1SER         SER104
164500941025    >C                   MOVE      S1SOC         SOC104
164600941025    >C                   MOVE      S1UNI         UNI104
164700941025    >C                   MOVE      S1CTB         CTB104
164800941025    >C                   MOVE      S1KEY         KEY104
164900950104    >C                   MOVE      H1TMP         TMP104
165000940314     C*
165100941018    >C                   MOVEL     A104DS        KPJBU
165200941018    >C                   CALL      'ANA104R'
165300940314     C                   PARM                    KPJBA
165400941018    >C                   MOVEL     KPJBU         A104DS
165500940201     C*
165600940223     C* ritorno da PGM gestione
165700941018    >C                   EXSR      RTN104
165800940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1659009402252    C     *IN99         IFEQ      *ON
166000940223     C                   SETON                                        40
1661009402252-   C                   ENDIF
166200940131     C*
166300940506     C                   MOVE      *HIVAL        $LASTV
1664009402251-   C                   ENDSL
166500940225     C*
166600940131     C                   ENDSR
166700940223     C/EJECT
166800940223     C************************************************************
166900940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
167000940223     C************************************************************
167100941018    >C     RTN104        BEGSR
167200940223     C*
167300940223     C* modo di ritorno
167400940223     C*
1675009402231    C                   SELECT
167600940314    >C* << questi modi di utilizzo dei valori di ritorno dal
167700940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
167800940314    >C*    proposte, normalmente sempre valide, ma modificabili
167900940314    >C*    per situazioni particolari >>
168000940223     C* 1 = F3
168100941018    >C     RET104        WHENEQ    '1'
168200940224     C                   MOVE      *ON           $ESCI
168300940223     C                   MOVE      *ON           $FINE
168400940223     C* 2 = F12
168500941018    >C     RET104        WHENEQ    '2'
168600940223     C                   MOVE      *ON           $ESCI
168700940223     C*
1688009402231-   C                   ENDSL
168900940223     C*
169000940223     C* operazione eseguite dal pgm chiamato
169100940223     C*
1692009402231    C                   SELECT
169300940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
169400941018    >C     OPR104        WHENEQ    '1'
169500940223     C                   MOVE      *ON           WINZS1
169600940223     C*
1697009402231-   C                   ENDSL
169800940223     C*
169900940223     C* funzione non eseguibile per errore :
170000940223     C*
1701009402231    C                   SELECT
170200940223     C* 1 = funzione richiamata chiusa in errore
170300940316    >C*  eventualmente gestire altri codici di errore
170400941018    >C     ERR104        WHENEQ    '1'
170500940223     C                   MOVE      *ON           $ESCI
170600940223     C                   SETON                                        5299
170700940223     C*
1708009402231-   C                   ENDSL
170900940223     C*
171000940223     C                   ENDSR
171100940223     C/EJECT
171200940131     C************************************************************
171300940131     C* GESTIONE VALORI ASSUNTI
171400940131     C************************************************************
171500940131     C     GESD2         BEGSR
171600940131     C*
171700940223     C     $INZD2        IFEQ      *ON
171800940131     C                   EXSR      INZD2
171900940223     C                   MOVE      *OFF          $INZD2
172000940131     C                   END
172100940202     C*
172200940202     C     $LASTV        IFNE      'D2'
172300940923     C                   EXSR      GESPIE
172400940202     C                   MOVE      $LASTV        $LASTG
172500940202     C                   MOVE      'D2'          $LASTV
172600940707     C                   ENDIF
172700940131     C*
172800940707     C* se esistono errori sulla videata
172900940707     C* emetto la write del formato a indicatori spenti per vedere
173000940707     C* le eventuali decodifiche
173100940707     C     *IN99         IFEQ      *ON
173200940707     C                   MOVEA     *IN           WIN
173300940707     C                   MOVE      *ALL'0'       IN5098           49
173400940707     C                   MOVEA     IN5098        *IN(50)
173500940707     C                   WRITE     FMTD2
173600940707     C                   MOVEA     WIN           *IN
173700940707     C                   ENDIF
173800940707     C*
173900940131     C                   EXFMT     FMTD2
174000940131     C*
174100940131     C                   SELECT
174200940131     C* F3=Fine
174300940506     C     $TASTO        WHENEQ    F03
174400940309     C                   EXSR      F03D2
174500940131     C* F12=Ritorno
174600940506     C     $TASTO        WHENEQ    F12
174700940309     C                   EXSR      F12D2
174800940916     C* F24=Altri tasti
174900940916     C     $TASTO        WHENEQ    F24
175000940916     C                   EXSR      F24D2
175100940131     C*
175200940131     C                   OTHER
175300940131     C                   EXSR      CTRD2
1754009401312    C     *IN99         IFEQ      *OFF
175500940211     C                   MOVE      $LASTG        $GEST
1756009401313    C                   SELECT
175700940202     C     $LASTG        WHENEQ    'S1'
175800940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
175900940314     C     $ORD          IFNE      D2ORD
176000940223     C                   MOVE      *ON           $INZS1
176100940302     C                   MOVE      *BLANK        W1POS
176200940131     C                   END
176300940202     C     $LASTG        WHENEQ    'D1'
176400940318     C* per ora niente
1765009401313-   C                   ENDSL
176600940131     C* definisco i nuovi valori di default
176700940201     C                   Z-ADD     D2LOP         $LOP
176800940131     C                   Z-ADD     D2ORD         $ORD
176900941006     C     $ORD          IFEQ      2
177000941006     C                   SETON                                            02
177100941006     C                   ENDIF
177200940609    >C* << elenco altri dati default >>
1773009401312-   C                   END
177400940131     C*
177500940131     C                   ENDSL
177600940131     C*
177700940131     C                   ENDSR
177800940223     C/EJECT
177900940131     C************************************************************
178000940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
178100940131     C************************************************************
178200940201     C     INZD2         BEGSR
178300940131     C*
178400940131     C                   CLEAR                   FMTD2
178500940201     C                   MOVEL     $LOP          D2LOP
178600940831     C                   MOVE      $ORD          D2ORD
178700940314    >C* << elenco altri dati default >>
178800940131     C*
178900940131     C                   ENDSR
179000940309     C/EJECT
179100940309     C************************************************************
179200940309     C* GESTIONE F03 VIDEO D2
179300940309     C************************************************************
179400940309     C     F03D2         BEGSR
179500940309     C*
179600940309     C                   MOVE      *ON           $FINE
179700940325     C* fine programma
179800940325     C                   EXSR      ENDPGM
179900940309     C*
180000940309     C                   ENDSR
180100940309     C/EJECT
180200940309     C************************************************************
180300940309     C* GESTIONE F12 VIDEO D2
180400940309     C************************************************************
180500940309     C     F12D2         BEGSR
180600940309     C*
180700940309     C                   MOVE      $LASTG        $GEST
180800940309     C*
180900940309     C                   ENDSR
181000940916     C/EJECT
181100940916     C************************************************************
181200940916     C* GESTIONE F24 VIDEO D2
181300940916     C************************************************************
181400940916     C     F24D2         BEGSR
181500940916     C*
181600940916     C                   MOVEA     $FX3          $FX
181700940916     C                   MOVEA     $FC3          $FC
181800940916     C                   MOVEA     $FR3          $FR
181900940916     C                   Z-ADD     $ULKD2        $ULK
182000940929     C                   Z-ADD     56            $L01
182100940929     C                   Z-ADD     56            $L02
182200940916     C                   EXSR      RIEKEY
182300940916     C                   Z-ADD     $ULK          $ULKD2            3 0
182400940916     C                   MOVEA     $FC           $FC3
182500940916     C                   MOVEL     $KEY1         Z3KE1
182600060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
182700940916     C                   MOVEL     $KEY2         Z3KE2
182800940916     C                   WRITE     FMTZ3
182900940916     C*
183000940916     C                   ENDSR
183100940223     C/EJECT
183200940131     C************************************************************
183300940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
183400940131     C************************************************************
183500940131     C     CTRD2         BEGSR
183600940131     C*
183700940131     C                   SETOFF                                       99
183800940131     C* non imposto pos. del cursore
183900940131     C                   Z-ADD     0             H1RIGA
184000940131     C                   Z-ADD     0             H1COLO
184100940131     C*
184200940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
184300940201     C     D2LOP         IFEQ      0
184400940131     C                   SETON                                          5099
184500940131     C                   END
184600940131     C*
184700940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
184800940131     C     D2ORD         IFEQ      0
184900940131     C                   SETON                                          5199
185000940131     C                   END
185100940314    >C* << elenco controlli altri dati default >>
185200940131     C*
185300940131     C                   ENDSR
185400940223     C/EJECT
185500940201     C************************************************************
185600940201     C* AGGIORNAMENTO VALORI ASSUNTI
185700940201     C************************************************************
185800940201     C     AGGMPE        BEGSR
185900940201     C*
186000940201     C                   CLEAR                   ANMPE000
186100940201     C                   MOVEL     XSCSOC        MPESOC
186200940201     C                   MOVEL     DSPGM         MPEPGM
186300940201     C                   MOVEL     KNMUS         MPEPRF
186400941013     C                   SELECT
186500941018     C     OPZ103        WHENEQ    '01'
186600941013     C                   MOVEL     'PX'          MPETIP
186700941013     C                   OTHER
186800941013     C                   MOVEL     'PV'          MPETIP
186900941013     C                   ENDSL
187000940201     C     K4MPE2        CHAIN     ANMPE02L                           21
187100940201     C                   MOVEL     DSMPE         MPEPAR
187200940201     C     *IN21         IFEQ      *ON
187300940201     C                   WRITE     ANMPE000
187400940201     C                   ELSE
187500940201     C                   UPDATE    ANMPE000
187600940201     C                   END
187700940201     C*
187800940201     C                   ENDSR
187900940223     C/EJECT
188000940131     C************************************************************
188100940131     C* OPERAZIONI INIZIALI
188200940131     C************************************************************
188300940131     C     *INZSR        BEGSR
188400940131     C*
188500940127     C* Reperimento parametri
188600940127     C*
188700940117     C     *ENTRY        PLIST
188800940117     C                   PARM                    KPJBA
188900940314     C* Reperimento autorizzazioni
189000940314     C*
189100940314    >C* << nel caso l'anagrafica non gestisse le autorizzazioni
189200940909    >C*    asteriscare la chiamata alla subroutine REPAUT >>
189300941005<<<<<C*                    EXSR REPAUT
189400940225     C*
189500940223     C* Reperimento opzioni
189600940127     C*
189700940128     C                   EXSR      REPOPZ
189800940506     C*
189900940506     C* Reperimento tasti di funzione
190000940506     C*
190100940506     C                   EXSR      REPKEY
190200941103     C*
190300941103     C* Reperimento azienda
190400941103     C*
190500941103     C                   MOVEL     'SOC001'      TIPXSC
190600941103     C                   EXSR      REPSOC
190700941103     C     RTNXSC        IFNE      '1'
190800941103     C                   MOVEL     XSOCDS        SOC001
190900941103     C                   ELSE
191000941103     C                   MOVE      *ON           $FINE
191100941103     C* fine programma
191200941103     C                   EXSR      ENDPGM
191300941103     C                   ENDIF
191400940127     C*
191500980722     C* Chiamata a routine per il reperimento delle opzioni e funzioni
191600980722     C* personalizzate per S1
191700980722     C                   CAllb     'X46USREXT'
191800980722     C                   PARM                    $OP1
191900980722     C                   PARM                    $TX1
192000980722     C                   PARM                    $FL1
192100980722     C                   PARM                    $PGO1
192200980722     C                   PARM      $dimOp        NumOpz
192300980722     C                   PARM                    $FX2
192400980722     C                   PARM                    $FC2
192500980722     C                   PARM                    $PGF2
192600980722     C                   PARM                    DsPgm
192700980722     C                   PARM                    xsccut
192800980722     C                   PARM                    xscgrp
192900980722     C*
193000980722     C                   Z-add     1             OPZLEN
193100980722     C     *blank        Lookup    $TX1(opzlen)                           21
193200980722     C                   if        *in21 = *on
193300980722     C                   sub       1             OPZLEN
193400980722     C                   else
193500980722     C                   Z-add     $DimOp        OPZLEN
193600980722     C                   endif
193700980722     C*
193800940117     C                   ENDSR
193900940225     C/EJECT
194000941103      ************************************************************
194100941103      * Reperimento dati società
194200941103      ************************************************************
194300941103     C     REPSOC        BEGSR
194400941103      *
194500961003     C                   CALLb     'XSOC'
194600941103     C                   PARM                    TIPXSC            6
194700941103     C                   PARM                    SOCXSC            3
194800941103     C                   PARM                    CDSXSC            9 0
194900941103     C                   PARM                    MODXSC            3
195000941103     C                   PARM      *BLANKS       RTNXSC            1
195100941103     C                   PARM                    XSOCDS
195200941103     C                   PARM                    KPJBA
195300941103      *
195400941103     C                   ENDSR
195500941103     C/EJECT
195600940225     C************************************************************
195700940225     C* REPERIMENTO AUTORIZZAZIONI UTENTE
195800940225     C************************************************************
195900941005     C*          REPAUT    BEGSR
196000940225     C*
196100941005     C*                    CLEARXAUT
196200940225     C*
196300941005     C*                    Z-ADD0         XCAGRP           GRUPPO
196400941005     C*                    MOVELKNMUS     XCAPRF           PROFILO
196500941005    >C*                    MOVEL'OBJALL'  XCAFNC           FUNZIONE
196600941005     C*                    MOVEL*ZERO     XCAVFU           VARIABILE
196700941005     C*                    MOVE 'CK'      XCATCK           TIPO CTRL
196800941005     C*
196900941005     C*                    CALL 'XCHKAUT'
197000941005     C*                    PARM           XAUT
197100940225     C* utente non abilitato
197200941005     C*          XCARTN    IFEQ 0
197300941005    >C*<                   MOVELXCADDS    AUTDS
197400941005     C*                    ELSE
197500941005    >C*<                   CLEARAUTDS
197600941005     C*                    END
197700940225     C*
197800941005     C*                    ENDSR
197900940223     C/EJECT
198000940223     C************************************************************
198100940223     C* INIZIALIZZAZIONE VARIABILI
198200940223     C************************************************************
198300940223     C     INZVAR        BEGSR
198400940223     C*
198500940729     C*
198600940729     C* Dati da PGM chiamante
198700940729     C*
198800941018    >C                   MOVEL     KPJBU         A103DS
198900941026     C*
199000941026     C* Reperimento valori default utente
199100941026     C*
199200941026     C                   EXSR      REPDFT
199300940506     C*
199400940506     C* Definizione delle opzioni utilizzabili a pgm
199500940506     C*
199600940506     C                   EXSR      DEFOPZ
199700940223     C* Variabili per gestione videate
199800940223     C*
199900940223     C                   MOVE      *OFF          $INZD1
200000940223     C                   MOVE      *OFF          $INZS1
200100941103     C                   MOVE      *OFF          $FINE             1
200200940223     C                   MOVE      *OFF          $INZD2
200300940223     C                   MOVE      *OFF          $EFILE
200400940223     C                   MOVE      *OFF          $ESCI
200500940223     C                   MOVE      *OFF          $RCDOK
200600940509     C                   MOVE      *HIVAL        $LASTV
200700940506     C                   Z-ADD     0             $ULKD1            3 0
200800940506     C                   Z-ADD     0             $ULKS1            3 0
200900940916     C                   Z-ADD     0             $ULKD2            3 0
201000940223     C*
201100940318     C                   MOVE      *ON           $INZD1
201200940318     C                   MOVE      *ON           $INZS1
201300940831     C*
201400940831     C* se pgm richiamato con chiave univoca impostata,
201500940831     C* forzo la emissione della lista e imposto l'ordinamento
201600941013     C*
201700941013     C*
201800941013     C*
201900941013     C*
202000940223     C                   SELECT
202100940223     C     $LOP          WHENEQ    1
202200940223     C                   MOVE      'D1'          $GEST
202300940223     C     $LOP          WHENEQ    2
202400940223     C                   MOVE      'S1'          $GEST
202500940223     C                   ENDSL
202600940223     C*
202700940223     C* Variabili appoggio
202800940223     C*
202900940223     C                   Z-ADD     SFLPAG        WPAG
203000940223     C*
203100940223     C* Valorizzazione campi univoci testate
203200940223     C*
203300940223     C                   CLEAR                   FMTT1
203400940223     C                   MOVEL     KNSIF         NOMSIF
203500941110     C                   MOVEL     XSCDSI        NOMDIT
203600941027     C                   MOVEL     DSPGM         NOMPGM
203700940223     C*
203800941011     C* Accendo indicatori per condizioni su campo posizionamento C1POS
203900941011     C                   SELECT
204000941011     C     $ORD          WHENEQ    1
204100941011     C                   SETON                                            01
204200941011     C     $ORD          WHENEQ    2
204300941011     C                   SETON                                            02
204400941011     C                   ENDSL
204500941011      *
204600941011      *
204700940223     C                   ENDSR
204800940223     C/EJECT
204900940127     C************************************************************
205000940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
205100940127     C************************************************************
205200940131     C     REPDFT        BEGSR
205300940127     C*
205400940201     C                   MOVEL     XSCSOC        MPESOC
205500940201     C                   MOVEL     DSPGM         MPEPGM
205600940131     C                   MOVEL     KNMUS         MPEPRF
205700941013     C                   SELECT
205800941018     C     OPZ103        WHENEQ    '01'
205900941013     C                   MOVEL     'PX'          MPETIP
206000941013     C                   OTHER
206100941013     C                   MOVEL     'PV'          MPETIP
206200941013     C                   ENDSL
206300940202     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
206400940131     C     *IN21         IFEQ      *OFF
206500940131     C                   MOVEL     MPEPAR        DSMPE
206600940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
206700940131     C* i suoi valori di default vengono decisi a pgm
206800940131     C                   ELSE
206900940131     C                   MOVEL     *BLANK        DSMPE
207000941024     C                   Z-ADD     2             $LOP
207100940131     C                   Z-ADD     1             $ORD
207200941011     C                   SETON                                        01
207300941005    >C* << elenco altri dati default >>
207400940131     C                   END
207500940131     C*
207600940131     C                   ENDSR
207700030225A0994C************************************************************
207800030225     C* REPERIMENTO AUTORIZZAZIONI numeratori
207900030225A0994C************************************************************
208000030225     C     REPAUT        BEGSR
208100030225     C*
208200030225     C                   CLEAR                   AUTDS
208300030225     C*
208400030225     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
208500030225     C                   Z-ADD     0             XCAGRP                         GRUPPO
208600030225     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
208700030225    >C                   MOVEL     'ANGNUM'      XCAFNC                         FUNZIONE
208800030225     c* cerco lunghezza effettiva serie
208900030225     c     ' '           checkr    S1Ser         length            1 0
209000030225     c                   eval      Zeri  = *all'0'
209100030225     c                   eval      xcavfu  = %subst(Zeri:1:15-length) +
209200030225     c                                       %trim(S1Ser)
209300030225    >C                   MOVE      'CC'          XCATCK                         TIPO CTRL
209400030225    >C                   MOVE      ' '           XCANAU                         RICH.XNOAUT
209500030225     C*
209600030225     C                   CALLB     'XCHKAUT'
209700030225     C                   PARM                    AUTDS
209800030225     C*
209900030225     C* errori/non abilitazione
210000030225     C* se non esiste, NON è errore
210100030225b1   C     XCARTN        IFne      0
210200030225     c                   else
210300030225    >C                   MOVEL     XCADDS        AngNumds
210400030225     c* non abilitato a manutenzione
210500030225     c                   if        s1opz  = 2    and
210600030225b2   c                             ManNUM = *off
210700030225     c                   seton                                        5499
210800030225e2   C                   endif
210900030225     c* non abilitato a cancellare
211000030225b2   c                   if        s1opz  = 4    and AnnNUM = *off
211100030225     c                   seton                                        5499
211200030225e2   C                   ENDIF
211300030225     c* non abilitato a visualizzare
211400030225b2   c                   if        s1opz  = 5    and visNUM = *off
211500030225     c                   seton                                        5499
211600030225e2   C                   ENDIF
211700030225     C*
211800030225e1   C                   ENDIF
211900030225     C*
212000030225A0994C                   ENDSR
212100940223     C/EJECT
212200940131     C************************************************************
212300940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
212400940131     C************************************************************
212500940225     C     DEFOPZ        BEGSR
212600940131     C*
212700940225     C* Questa routine deve valorizzare la schiera $FL1
212800940225     C* con S o N a seconda della utilizzabilità o meno
212900940225     C* dell'opzione correlata
213000940225     C*
213100940225     C* Inizialmente si considerano tutte le opzioni come valide
213200940317     C     1             DO        $DIMOP        X
213300940225     C                   MOVE      'S'           $FL1(X)
213400940225     C                   END
213500940715     C*
213600940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
213700940715     C* con S o N a seconda della utilizzabilità o meno
213800940715     C* del tasto funzione correlato
213900940715     C*
214000940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
214100940715     C* di compilazione
214200940715     C                   RESET                   $FC1
214300940715     C                   RESET                   $FR1
214400940715     C                   RESET                   $FC2
214500940715     C                   RESET                   $FR2
214600940919     C                   RESET                   $FC3
214700940919     C                   RESET                   $FR3
214800940225     C*
214900940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
215000940225     C* invalido alcune opzioni
215100940225     C                   SELECT
215200940225     C* Se pgm richiamato in ricerca disattivo :
215300941018    >C     OPZ103        WHENEQ    '01'
215400940307     C* F10=Immissione
215500940506     C                   Z-ADD     10            X
215600940720     C                   MOVE      '0'           $FC2(X)
215700940603     C* F3=Fine
215800940603     C                   Z-ADD     3             X
215900940720     C                   MOVE      '0'           $FC2(X)
216000940307     C* 2=Modifica
216100940307     C                   Z-ADD     1             X
216200940225     C     02            LOOKUP    $OP1(X)                                21
216300940225     C     *IN21         IFEQ      *ON
216400940225     C                   MOVE      'N'           $FL1(X)
216500940225     C                   ENDIF
216600940225     C* 3=Copia
216700940225     C                   Z-ADD     1             X
216800940225     C     03            LOOKUP    $OP1(X)                                21
216900940225     C     *IN21         IFEQ      *ON
217000940225     C                   MOVE      'N'           $FL1(X)
217100940225     C                   ENDIF
217200940225     C* 4=Annullamento
217300940225     C                   Z-ADD     1             X
217400940225     C     04            LOOKUP    $OP1(X)                                21
217500940225     C     *IN21         IFEQ      *ON
217600940225     C                   MOVE      'N'           $FL1(X)
217700940225     C                   ENDIF
217800940314    >C* << aggiungere nollo stesso modo le altre opzioni da
217900940314    >C*    disattivare (quelle specifiche per ogni pgm) >>
218000940225     C* Se pgm richiamato in interrogazione disattivo :
218100941018    >C     OPZ103        WHENEQ    '05'
218200940307     C* F10=Immissione
218300940615     C                   Z-ADD     10            X
218400940720     C                   MOVE      '0'           $FC2(X)
218500940225     C* 1=Selezione
218600940225     C                   Z-ADD     1             X
218700940225     C     01            LOOKUP    $OP1(X)                                21
218800940225     C     *IN21         IFEQ      *ON
218900940225     C                   MOVE      'N'           $FL1(X)
219000940225     C                   ENDIF
219100940225     C* 2=Modifica
219200940225     C                   Z-ADD     1             X
219300940225     C     02            LOOKUP    $OP1(X)                                21
219400940225     C     *IN21         IFEQ      *ON
219500940225     C                   MOVE      'N'           $FL1(X)
219600940225     C                   ENDIF
219700940225     C* 3=Copia
219800940225     C                   Z-ADD     1             X
219900940225     C     03            LOOKUP    $OP1(X)                                21
220000940225     C     *IN21         IFEQ      *ON
220100940225     C                   MOVE      'N'           $FL1(X)
220200940225     C                   ENDIF
220300941005     C* 4=Annullamento
220400940225     C                   Z-ADD     1             X
220500940225     C     04            LOOKUP    $OP1(X)                                21
220600940225     C     *IN21         IFEQ      *ON
220700940225     C                   MOVE      'N'           $FL1(X)
220800940225     C                   ENDIF
220900940225     C*
221000940225     C                   OTHER
221100940225     C* Se pgm non richiamato, disattivo subito :
221200940225     C* 1=Selezione
221300940225     C                   Z-ADD     1             X
221400940225     C     01            LOOKUP    $OP1(X)                                21
221500940225     C     *IN21         IFEQ      *ON
221600940225     C                   MOVE      'N'           $FL1(X)
221700940225     C                   ENDIF
221800940225     C* poi, disattivo a seconda delle autorizzazioni :
221900940314    >C*********  AUTXXX    IFEQ 'N'
222000940314    >C*********            Z-ADD1         X
222100940314    >C*********  'XX'      LOKUP$OP1,X                   21
222200940314    >C*********  *IN21     IFEQ *ON
222300940314    >C*********            MOVE 'N'       $FL1,X
222400940314    >C*********            ENDIF
222500940314    >C*********            ENDIF
222600940225     C*
222700940225     C                   ENDSL
222800940225     C*
222900940225     C                   ENDSR
223000940225     C/EJECT
223100940225     C************************************************************
223200940225     C* REPERIMENTO OPZIONI
223300940225     C************************************************************
223400940225     C     REPOPZ        BEGSR
223500940225     C*
223600940314     C     1             DO        $DIMOP        X
223700940128     C*
223800940223     C     $MS1(X)       IFNE      *BLANK
223900940128     C*
224000940223     C                   MOVEL     $MS1(X)       IDMSG
224100961003     C                   CALLb     'XRTVM'
224200940201     C                   PARM                    IDMSG            27
224300940201     C                   PARM                    TXTMSG          100
224400940201     C                   PARM                    ERRMSG            1
224500940128     C*
224600940128     C     ERRMSG        IFNE      *ON
224700940223     C                   MOVEL     TXTMSG        $TX1(X)
224800940128     C                   ELSE
224900940223     C                   MOVEL     *ALL'?'       $TX1(X)
225000940128     C                   END
225100940128     C*
225200940128     C                   END
225300940128     C*
225400940128     C                   END
225500940128     C*
225600940128     C                   ENDSR
225700940506     C/EJECT
225800940506     C************************************************************
225900940506     C* REPERIMENTO TASTI DI FUNZIONE
226000940506     C************************************************************
226100940506     C     REPKEY        BEGSR
226200940506     C                   MOVEA     $FM1          $FM
226300950906     C                   MOVEA     $FC1          $FC
226400940506     C                   EXSR      REPKKK
226500940506     C                   MOVEA     $FX           $FX1
226600940506     C*
226700940506     C                   MOVEA     $FM2          $FM
226800950906     C                   MOVEA     $FC2          $FC
226900940506     C                   EXSR      REPKKK
227000940506     C                   MOVEA     $FX           $FX2
227100940919     C*
227200940919     C                   MOVEA     $FM3          $FM
227300950906     C                   MOVEA     $FC3          $FC
227400940919     C                   EXSR      REPKKK
227500940919     C                   MOVEA     $FX           $FX3
227600940919     C                   ENDSR
227700940506     C/EJECT
227800940506     C************************************************************
227900940506     C* REPERIMENTO TASTI DI FUNZIONE
228000940506     C************************************************************
228100940506     C     REPKKK        BEGSR
228200940506     C*
228300940506     C                   CLEAR                   $FX
228400940506     C     1             DO        24            X
228500940506     C*
228600940506     C     $FM(X)        IFNE      *BLANK
228700950906     C     $FC(X)        ANDNE     '0'
228800940506     C*
228900940506     C                   MOVEL     $FM(X)        IDMSG
229000961003     C                   CALLb     'XRTVM'
229100940506     C                   PARM                    IDMSG            27
229200940506     C                   PARM                    TXTMSG          100
229300940506     C                   PARM                    ERRMSG            1
229400940506     C*
229500940506     C     ERRMSG        IFNE      *ON
229600940506     C                   MOVEL     TXTMSG        $FX(X)
229700940506     C                   ELSE
229800940506     C                   MOVEL     *ALL' '       $FX(X)
229900940506     C                   END
230000940506     C*
230100940506     C                   END
230200940506     C*
230300940506     C                   END
230400940506     C*
230500940506     C                   ENDSR
230600940223     C/EJECT
230700940128     C************************************************************
230800940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
230900940128     C************************************************************
231000940223     C     DEFCAM        BEGSR
231100940128     C*
231200940223     C* klist
231300981229E0045C*
231400981229  "  C* ANNMR01L
231500981229  "  C     K6NMR1        KLIST
231600981229  "  C                   KFLD                    NMRsoc
231700981229  "  C                   KFLD                    NMRuni
231800981229  "  C                   KFLD                    NMRctb
231900981229  "  C                   KFLD                    NMRkey
232000981229  "  C                   KFLD                    NMRser
232100981229  "  C                   KFLD                    NMRtmp
232200981229E0045C*
232300941024     C*
232400941028     C* ANNMR02L
232500981229E0045C***  K2NMR1        KLIST
232600981229E0045C     K2NMR2        KLIST
232700941024     C                   KFLD                    NMRGRU
232800941024     C                   KFLD                    NMRSER
232900940223     C*
233000941005     C* ANMPE02L
233100941005     C     K4MPE2        KLIST
233200941005     C                   KFLD                    MPESOC
233300941005     C                   KFLD                    MPEPGM
233400941005     C                   KFLD                    MPETIP
233500941005     C                   KFLD                    MPEPRF
233600940128     C*
233700940223     C*
233800940223     C* Variabili per gestione videate
233900940223     C*
234000940223     C                   MOVE      *BLANK        $GEST             2
234100940223     C                   MOVE      *BLANK        $INZD1            1
234200940223     C                   MOVE      *BLANK        $INZS1            1
234300940223     C                   MOVE      *BLANK        $INZD2            1
234400940223     C                   MOVE      *BLANK        $LASTG            2
234500940223     C                   MOVE      *BLANK        $LASTV            2
234600940223     C                   MOVE      *BLANK        $EFILE            1
234700940223     C                   MOVE      *BLANK        $ESCI             1
234800940223     C                   MOVE      *BLANK        $RCDOK            1
234900940224     C                   Z-ADD     0             $PIED1            3 0
235000940224     C                   Z-ADD     0             $PIES1            3 0
235100940223     C*
235200940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
235300940223     C*
235400940223     C     *LIKE         DEFINE    C1RCD         S1NRR
235500940223     C     *LIKE         DEFINE    C1NRR         WSFL
235600940223     C     *LIKE         DEFINE    C1RCD         WMAX
235700940223     C     *LIKE         DEFINE    C1RCD         WPAG
235800940223     C     *LIKE         DEFINE    $INZS1        WINZS1
235900960416     C                   MOVE      *off          WINKR             1
236000940223     C                   Z-ADD     0             CURR              2 0
236100940223     C                   Z-ADD     0             CURC              2 0
236200940223     C                   MOVE      *BLANK        WIN              99
236300940314     C     *LIKE         DEFINE    C1POS         W1POS
236400940314     C*
236500940314     C* Variabili appoggio specifiche della singola anagrafica
236600940314     C*
236700940223     C* campi per parzializzazioni su rcd (solo alfanumerici)
236800940223     C*
236900940223     C* Indici
237000940223     C*
237100940223     C                   Z-ADD     0             X                 3 0
237200940223     C                   Z-ADD     0             Y                 3 0
237300940127     C*
237400940117     C                   ENDSR
237500941010     C/EJECT
237600941010    > ************************************************************
237700941010    > * ACCESSO A TABELLE ANTAB00F
237800941010    > ************************************************************
237900941010    >C     LETTAB        BEGSR
238000941010    > *
238100941010    > * Input :
238200941010    > * - TABGRU/TABCOD = chiave univoca ANTAB00F
238300941010    > * Output :
238400941010    > * - WTBERR = flag segnalazione errore reperimento tabella
238500941010    > *
238600961003    >C                   CALLb     'XATB'
238700941010    >C                   PARM                    XATBDS
238800941010    > *
238900941010    >C                   MOVE      *OFF          WTBERR
239000941010    >C     XTBERR        IFEQ      *ON
239100941010    >C                   MOVE      *BLANK        XTBUNI
239200941010    >C                   MOVE      *ON           WTBERR            1
239300941010    >C                   ENDIF
239400941010    > *
239500941010    >C                   ENDSR
239600941010     C/EJECT
239700950906**
239800940718PROMSG    *LIBL     OPZ0001
239900940225PROMSG    *LIBL     OPZ0002
240000940131PROMSG    *LIBL     OPZ0003
240100940131PROMSG    *LIBL     OPZ0004
240200940131PROMSG    *LIBL     OPZ0005
240300940201**
24040094022501S      <Selezione
24050094022502S      <Modifica
24060094022503S      <Copia
24070094022504S      <Annullamento
24080094022505S      <Visualizzazione
240900940506** TASTI DI FUNZIONE  D1
241000940506PROMSG    *LIBL     KEY0001  01
241100940506PROMSG    *LIBL     KEY0002  02
241200940506PROMSG    *LIBL     KEY0003  03
241300940506PROMSG    *LIBL     KEY0004  04
241400940506PROMSG    *LIBL     KEY0005  05
241500940506PROMSG    *LIBL     KEY0006  06
241600940506PROMSG    *LIBL     KEY0007  07
241700940506PROMSG    *LIBL     KEY0008  08
241800940506PROMSG    *LIBL     KEY0009  09
241900940506PROMSG    *LIBL     KEY0010  10
242000940506PROMSG    *LIBL     KEY0011  11
242100940506PROMSG    *LIBL     KEY0012  12
242200940506PROMSG    *LIBL     KEY0013  13
242300940506PROMSG    *LIBL     KEY0014  14
242400940506PROMSG    *LIBL     KEY0015  15
242500940506PROMSG    *LIBL     KEY0016  16
242600940506PROMSG    *LIBL     KEY0017  17
242700940506PROMSG    *LIBL     KEY0018  18
242800940506PROMSG    *LIBL     KEY0019  19
242900940506PROMSG    *LIBL     KEY0020  20
243000940506PROMSG    *LIBL     KEY0021  21
243100940506PROMSG    *LIBL     KEY0022  22
243200940506PROMSG    *LIBL     KEY0023  23
243300940506PROMSG    *LIBL     KEY0024  24
243400940506**  ABILITAZIONE D1
2435009407200N  01
2436009604161N  02
2437009604161N  03
2438009604161N  04
2439009604161N  05
2440009407200N  06
2441009407200N  07
2442009407200N  08
2443009407200N  09
2444009407200N  10
2445009407200N  11
2446009604161N  12
2447009407200N  13
2448009407200N  14
2449009407200N  15
2450009407200N  16
2451009410180N  17
2452009604161N  18
2453009407200N  19
2454009407200N  20
2455009407200N  21
2456009407200N  22
2457009509060N  23   Impostare a '1' se si utilizzano opzioni
2458009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
245900940916** TASTI DI FUNZIONE  S1
246000940916PROMSG    *LIBL     KEY0001  01
246100940916PROMSG    *LIBL     KEY0002  02
246200940916PROMSG    *LIBL     KEY0003  03
246300940916PROMSG    *LIBL     KEY0004  04
246400940916PROMSG    *LIBL     KEY0005  05
246500940916PROMSG    *LIBL     KEY0006  06
246600940916PROMSG    *LIBL     KEY0007  07
246700940916PROMSG    *LIBL     KEY0008  08
246800940916PROMSG    *LIBL     KEY0009  09
246900940916PROMSG    *LIBL     KEY0010  10
247000940916PROMSG    *LIBL     KEY0011  11
247100940916PROMSG    *LIBL     KEY0012  12
247200940916PROMSG    *LIBL     KEY0013  13
247300940916PROMSG    *LIBL     KEY0014  14
247400940916PROMSG    *LIBL     KEY0015  15
247500940916PROMSG    *LIBL     KEY0016  16
247600940916PROMSG    *LIBL     KEY0017  17
247700940916PROMSG    *LIBL     KEY0018  18
247800940916PROMSG    *LIBL     KEY0019  19
247900940916PROMSG    *LIBL     KEY0020  20
248000940916PROMSG    *LIBL     KEY0021  21
248100940916PROMSG    *LIBL     KEY0022  22
248200940916PROMSG    *LIBL     KEY0023  23
248300940916PROMSG    *LIBL     KEY0024  24
248400940916**  ABILITAZIONE S1
2485009409160N  01
2486009409160N  02
2487009409161N  03
2488009409160N  04
2489009409160N  05
2490009409160N  06
2491009409160N  07
2492009409160N  08
2493009409160N  09
2494009409161N  10
2495009409160N  11
2496009409161N  12
2497009409160N  13
2498009409160N  14
2499009409160N  15
2500009409160N  16
2501009604161N  17
2502009604161N  18
2503009409160N  19
2504009409160N  20
2505009409160N  21
2506009409160N  22
2507009509061N  23   Impostare a '1' se si utilizzano opzioni
2508009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
250900941018** TASTI DI FUNZIONE  D3
251000941018PROMSG    *LIBL     KEY0001  01
251100941018PROMSG    *LIBL     KEY0002  02
251200941018PROMSG    *LIBL     KEY0003  03
251300941018PROMSG    *LIBL     KEY0004  04
251400941018PROMSG    *LIBL     KEY0005  05
251500941018PROMSG    *LIBL     KEY0006  06
251600941018PROMSG    *LIBL     KEY0007  07
251700941018PROMSG    *LIBL     KEY0008  08
251800941018PROMSG    *LIBL     KEY0009  09
251900941018PROMSG    *LIBL     KEY0010  10
252000941018PROMSG    *LIBL     KEY0011  11
252100941018PROMSG    *LIBL     KEY0012  12
252200941018PROMSG    *LIBL     KEY0013  13
252300941018PROMSG    *LIBL     KEY0014  14
252400941018PROMSG    *LIBL     KEY0015  15
252500941018PROMSG    *LIBL     KEY0016  16
252600941018PROMSG    *LIBL     KEY0017  17
252700941018PROMSG    *LIBL     KEY0018  18
252800941018PROMSG    *LIBL     KEY0019  19
252900941018PROMSG    *LIBL     KEY0020  20
253000941018PROMSG    *LIBL     KEY0021  21
253100941018PROMSG    *LIBL     KEY0022  22
253200941018PROMSG    *LIBL     KEY0023  23
253300941018PROMSG    *LIBL     KEY0024  24
253400941018**  ABILITAZIONE D3
2535009410180N  01
2536009410180N  02
2537009410181N  03
2538009410180N  04
2539009410180N  05
2540009410180N  06
2541009410180N  07
2542009410180N  08
2543009410180N  09
2544009410180N  10
2545009410180N  11
2546009410181N  12
2547009410180N  13
2548009410180N  14
2549009410180N  15
2550009410180N  16
2551009410180N  17
2552009410180N  18
2553009410180N  19
2554009410180N  20
2555009410180N  21
2556009410180N  22
2557009509060N  23   Impostare a '1' se si utilizzano opzioni
2558009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
