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
000000060224A1148C                   PARM                    $ALLOPT         960
000000060224A1148C*
042900940131     C                   ENDSR
043000940506     C/EJECT
043100940506     C************************************************************
043200940506     C* RIEMPIMENTO TASTI FUNZIONE
043300940506     C************************************************************
043400940506     C     RIEKEY        BEGSR
043500940506     C*
043600940506     C* Input :
043700940506     C* - $FX = schiera con tutti i testi disponibili
043800940506     C* - $FC = schiere con i flag di validità associati ai testi
043900940506     C* - $FR = schiere con i flag di REVERSE IMAGE
044000940506     C* - $ULK = ultimo testo caricato
044100940721     C* - $L01 = Lunghezza prima riga
044200940721     C* - $L02 = Lunghezza seconda riga
044300940506     C* Output :
044400940506     C* - $ULK
044500940609     C* - $KEY1= 1° riga di tasti funzione
044600940609     C* - $KEY2= 2° riga di tasti funzione
044700940506     C*
044800961003     C                   CALLb     'XKEYMSG'
044900940506     C* input
045000940506     C                   PARM                    $FX
045100940506     C                   PARM                    $FC
045200940506     C                   PARM                    $FR
045300940506     C                   PARM                    $ULK              3 0
045400940506     C* output
045500940506     C                   PARM                    $KEY1            79
045600940506     C                   PARM                    $KEY2            79
045700940721    >C* input
045800940721    >C                   PARM                    $L01              3 0
045900940721    >C                   PARM                    $L02              3 0
046000940506     C*
000000060224A1148C                   PARM                    $ALLFUNCT       480
000000060224A1148C*
046100940506     C                   ENDSR
046200940224     C/EJECT
046300940224     C************************************************************
046400940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
046500940224     C************************************************************
046600940224     C     GESPIE        BEGSR
046700940224     C*
046800940224     C                   SELECT
046900940224     C* Parzializzazioni
047000940224     C     $GEST         WHENEQ    'D1'
047100940506     C     $ULKD1        IFEQ      0
047200940506     C* riempimento opzioni
047300940506     C                   EXSR      F24D1
047400940506     C                   ELSE
047500940506     C                   WRITE     FMTZ1
047600940506     C                   ENDIF
047700940506     C* Lista
047800940506     C     $GEST         WHENEQ    'S1'
047900940506     C     $ULKS1        IFEQ      0
048000940506     C                   EXSR      F24S1
048100940506     C                   ELSE
048200940506     C                   WRITE     FMTZ2
048300940506     C                   ENDIF
048400940916     C* Modifica valori assunti
048500940916     C     $GEST         WHENEQ    'D2'
048600940916     C     $ULKD2        IFEQ      0
048700940916     C                   EXSR      F24D2
048800940916     C                   ELSE
048900940916     C                   WRITE     FMTZ3
049000940916     C                   ENDIF
049100940506     C                   ENDSL
049200940506     C*
049300940224     C                   ENDSR
049400940224     C/EJECT
049500940224     C************************************************************
049600940224     C* GESTIONE PARZIALIZZAZIONI
049700940224     C************************************************************
049800940224     C     GESD1         BEGSR
049900940224     C* inizializzazione videata
050000940223     C     $INZD1        IFEQ      *ON
050100940127     C                   EXSR      INZD1
050200940223     C                   MOVE      *OFF          $INZD1
050300940117     C                   END
050400940223     C* emissione piede videata se proveniente da altra
050500940223     C* salvataggio valore formato di provenienza
050600940202     C     $LASTV        IFNE      'D1'
050700940223     C* salvo stato F17 per sapere cosa fare in caso di F12
050800940223     C                   MOVE      *INKR         WINKR
050900940202     C                   WRITE     FMTT1
051000940224     C                   EXSR      GESPIE
051100940202     C                   MOVE      $LASTV        $LASTG
051200940202     C                   MOVE      'D1'          $LASTV
051300940707     C                   ENDIF
051400940707     C*
051500940707     C* se esistono errori sulla videata
051600940707     C* emetto la write del formato a indicatori spenti per vedere
051700940707     C* le eventuali decodifiche
051800940707     C     *IN99         IFEQ      *ON
051900940707     C                   MOVEA     *IN           WIN
052000940707     C                   MOVE      *ALL'0'       IN5098           49
052100940707     C                   MOVEA     IN5098        *IN(50)
052200940707     C                   WRITE     FMTD1
052300940707     C                   MOVEA     WIN           *IN
052400940707     C                   ENDIF
052500940131     C*
052600940131     C                   EXFMT     FMTD1
052700940506     C* controllo abilitazione tasti funzione
052800940506     C*
052900940506     C                   EXSR      CTRKD1
053000940506     C*
053100940506     C     *IN99         IFEQ      *OFF
053200940131     C*
053300940117     C                   SELECT
053400940916     C* F2=Decodifica
053500940916     C     $TASTO        WHENEQ    F02
053600940916     C                   EXSR      F02D1
053700940131     C* F3=Fine
053800940506     C     $TASTO        WHENEQ    F03
053900940307     C                   EXSR      F03D1
054000940131     C* F4=Ricerca
054100940506     C     $TASTO        WHENEQ    F04
054200940127     C                   EXSR      SEARCH
054300940916     C* F5=Refresh
054400940916     C     $TASTO        WHENEQ    F05
054500940916     C                   EXSR      F05D1
054600940131     C* F12=Ritorno
054700940506     C     $TASTO        WHENEQ    F12
054800940307     C                   EXSR      F12D1
054900940131     C* F18=Modifica valori assunti
055000940506     C     $TASTO        WHENEQ    F18
055100940307     C                   EXSR      F18D1
055200940224     C* F24=Altri tasti
055300940506     C     $TASTO        WHENEQ    F24
055400940309     C                   EXSR      F24D1
055500940131     C*
055600940117     C                   OTHER
055700940127     C                   EXSR      CTRD1
055800940127     C     *IN99         IFEQ      *OFF
055900940131     C* definisco l'ordinamento
056000941018    >C* leggo in ordine di ordinamento
056100981229E0045C* se nelle parzializzazioni è specificato un valore per D1DRP
056200981229  "  C* forzo $ORD = 1 (gruppo/serie) altrimenti lascio il valore
056300981229  "  C* reperito o quello utilizzato precedentemente
056400981229  "  C     d1grp         ifne      *zero
056500981229  "  C     d1serienm     orne      *blank
056600981229  "  C                   z-add     1             $ord
056700981229  "  C                   endif
056800981229E0045C***                Z-ADD     1             $ORD
056900960416     C* richiamo la lista
057000960416     C                   MOVE      'S1'          $GEST
057100960416     C                   MOVE      *ON           $INZS1
057200960416     C                   MOVE      *BLANK        W1POS
057300941024     C                   END
057400941024     C                   ENDSL
057500940224     C*
057600940506     C                   ENDIF
057700940117     C                   ENDSR
057800940224     C/EJECT
057900940224     C************************************************************
058000940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
058100940224     C************************************************************
058200940127     C     INZD1         BEGSR
058300940318     C*
058400940318     C                   CLEAR                   FMTD1
058500960416     C                   MOVE      XscOn         D1On1
058600960416     C                   MOVE      XscOn         D1On2
058700960416     C                   MOVE      XscOff        D1Off1
058800960416     C                   MOVE      XscOff        D1Off2
058900940224     C*
059000940117     C                   ENDSR
059100940506     C/EJECT
059200940506     C************************************************************
059300940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
059400940609     C* INPUT
059500940509     C* $FC1 = Abilitazioni videata D!
059600940509     C* OUTPUT
059700940509     C* *IN97 *IN99 Errori
059800940506     C************************************************************
059900940506     C     CTRKD1        BEGSR
060000940506     C                   SETOFF                                       99
060100940506     C                   MOVEA     $FC1          $FC
060200940506     C                   EXSR      CTRKEY
060300940506     C     *IN99         IFEQ      *ON
060400940506     C                   SETON                                        97
060500940506     C                   ENDIF
060600940506     C                   ENDSR
060700940506     C/EJECT
060800940506     C************************************************************
060900940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
061000940506     C************************************************************
061100940506     C     CTRKS1        BEGSR
061200940506     C                   SETOFF                                       99
061300940506     C                   MOVEA     $FC2          $FC
061400940506     C                   EXSR      CTRKEY
061500940506     C     *IN99         IFEQ      *ON
061600940506     C                   SETON                                        97
061700940506     C                   ENDIF
061800940506     C                   ENDSR
061900940506     C/EJECT
062000940506     C************************************************************
062100940506     C* CONTROLLO ABILITAZIONE TASTI
062200940509     C* INPUT
062300940609     C* $TASTO =Tasto funzione premuto
062400940509     C* $FC = Tasti abilitati
062500940509     C* OUTPUT
062600940509     C* IN99 = ERrore
062700940506     C************************************************************
062800940506     C     CTRKEY        BEGSR
062900940506     C                   SELECT
063000940506     C     $TASTO        WHENEQ    F01
063100980717     C                   z-add     01            posfun
063200940720     C     $FC(1)        IFEQ      '0'
063300940506     C                   SETON                                        99
063400940506     C                   ENDIF
063500940506     C     $TASTO        WHENEQ    F02
063600980717     C                   z-add     02            posfun
063700940720     C     $FC(2)        IFEQ      '0'
063800940506     C                   SETON                                        99
063900940506     C                   ENDIF
064000940506     C     $TASTO        WHENEQ    F03
064100980717     C                   z-add     03            posfun
064200940720     C     $FC(3)        IFEQ      '0'
064300940506     C                   SETON                                        99
064400940506     C                   ENDIF
064500940506     C     $TASTO        WHENEQ    F04
064600980717     C                   z-add     04            posfun
064700940720     C     $FC(4)        IFEQ      '0'
064800940506     C                   SETON                                        99
064900940506     C                   ENDIF
065000940506     C     $TASTO        WHENEQ    F05
065100980717     C                   z-add     05            posfun
065200940720     C     $FC(5)        IFEQ      '0'
065300940506     C                   SETON                                        99
065400940506     C                   ENDIF
065500940506     C     $TASTO        WHENEQ    F06
065600980717     C                   z-add     06            posfun
065700940720     C     $FC(6)        IFEQ      '0'
065800940506     C                   SETON                                        99
065900940506     C                   ENDIF
066000940506     C     $TASTO        WHENEQ    F07
066100980717     C                   z-add     07            posfun
066200940720     C     $FC(7)        IFEQ      '0'
066300940506     C                   SETON                                        99
066400940506     C                   ENDIF
066500940506     C     $TASTO        WHENEQ    F08
066600980717     C                   z-add     08            posfun
066700940720     C     $FC(8)        IFEQ      '0'
066800940506     C                   SETON                                        99
066900940506     C                   ENDIF
067000940615     C     $TASTO        WHENEQ    F09
067100980717     C                   z-add     09            posfun
067200940720     C     $FC(09)       IFEQ      '0'
067300940506     C                   SETON                                        99
067400940506     C                   ENDIF
067500940506     C     $TASTO        WHENEQ    F10
067600980717     C                   z-add     10            posfun
067700940720     C     $FC(10)       IFEQ      '0'
067800940506     C                   SETON                                        99
067900940506     C                   ENDIF
068000940506     C     $TASTO        WHENEQ    F11
068100980717     C                   z-add     11            posfun
068200940720     C     $FC(11)       IFEQ      '0'
068300940506     C                   SETON                                        99
068400940506     C                   ENDIF
068500940506     C     $TASTO        WHENEQ    F12
068600980717     C                   z-add     12            posfun
068700940720     C     $FC(12)       IFEQ      '0'
068800940506     C                   SETON                                        99
068900940506     C                   ENDIF
069000940506     C     $TASTO        WHENEQ    F13
069100980717     C                   z-add     13            posfun
069200940720     C     $FC(13)       IFEQ      '0'
069300940506     C                   SETON                                        99
069400940506     C                   ENDIF
069500940506     C     $TASTO        WHENEQ    F14
069600980717     C                   z-add     14            posfun
069700940720     C     $FC(14)       IFEQ      '0'
069800940506     C                   SETON                                        99
069900940506     C                   ENDIF
070000940506     C     $TASTO        WHENEQ    F15
070100980717     C                   z-add     15            posfun
070200940720     C     $FC(15)       IFEQ      '0'
070300940506     C                   SETON                                        99
070400940506     C                   ENDIF
070500940506     C     $TASTO        WHENEQ    F16
070600980717     C                   z-add     16            posfun
070700940720     C     $FC(16)       IFEQ      '0'
070800940506     C                   SETON                                        99
070900940506     C                   ENDIF
071000940506     C     $TASTO        WHENEQ    F17
071100980717     C                   z-add     17            posfun
071200940720     C     $FC(17)       IFEQ      '0'
071300940506     C                   SETON                                        99
071400940506     C                   ENDIF
071500940506     C     $TASTO        WHENEQ    F18
071600980717     C                   z-add     18            posfun
071700940720     C     $FC(18)       IFEQ      '0'
071800940506     C                   SETON                                        99
071900940506     C                   ENDIF
072000940506     C     $TASTO        WHENEQ    F19
072100980717     C                   z-add     19            posfun
072200940720     C     $FC(19)       IFEQ      '0'
072300940506     C                   SETON                                        99
072400940506     C                   ENDIF
072500940506     C     $TASTO        WHENEQ    F20
072600980717     C                   z-add     20            posfun
072700940720     C     $FC(20)       IFEQ      '0'
072800940506     C                   SETON                                        99
072900940506     C                   ENDIF
073000940506     C     $TASTO        WHENEQ    F21
073100980717     C                   z-add     21            posfun
073200940720     C     $FC(21)       IFEQ      '0'
073300940506     C                   SETON                                        99
073400940506     C                   ENDIF
073500940506     C     $TASTO        WHENEQ    F22
073600980717     C                   z-add     22            posfun
073700940720     C     $FC(22)       IFEQ      '0'
073800940506     C                   SETON                                        99
073900940506     C                   ENDIF
074000940506     C     $TASTO        WHENEQ    F23
074100980717     C                   z-add     23            posfun
074200940720     C     $FC(23)       IFEQ      '0'
074300940506     C                   SETON                                        99
074400940506     C                   ENDIF
074500940506     C     $TASTO        WHENEQ    F24
074600980717     C                   z-add     24            posfun
074700940720     C     $FC(24)       IFEQ      '0'
074800940506     C                   SETON                                        99
074900940506     C                   ENDIF
075000940506     C                   ENDSL
075100940506     C                   ENDSR
075200940916     C/EJECT
075300940916     C************************************************************
075400940916     C* GESTIONE F02 VIDEO D1
075500940916     C************************************************************
075600940916     C     F02D1         BEGSR
075700940916     C*
075800940916     C* per decodificare la videata richiamo la routine dei ctrl
075900940916     C* e poi disattivo gli eventuali messaggi di errore
076000940916     C                   EXSR      CTRD1
076100940916     C*
076200940916     C                   MOVE      *ALL'0'       IN5098
076300940916     C                   MOVEA     IN5098        *IN(50)
076400940916     C*
076500940916     C                   ENDSR
076600940224     C/EJECT
076700940131     C************************************************************
076800940307     C* GESTIONE F03 VIDEO D1
076900940131     C************************************************************
077000940307     C     F03D1         BEGSR
077100940131     C*
077200940307     C                   MOVE      *ON           $FINE
077300940325     C* fine programma
077400940325     C                   EXSR      ENDPGM
077500940307     C*
077600940307     C                   ENDSR
077700940916     C/EJECT
077800940916     C************************************************************
077900940916     C* GESTIONE F05 VIDEO D1
078000940916     C************************************************************
078100940916     C     F05D1         BEGSR
078200940916     C*
078300940916     C                   MOVE      *ON           $INZD1
078400940916     C*
078500940916     C                   ENDSR
078600940307     C/EJECT
078700940307     C************************************************************
078800940307     C* GESTIONE F12 VIDEO D1
078900940307     C************************************************************
079000940307     C     F12D1         BEGSR
079100940307     C*
079200940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
079300940307     C*  per F12 torno alla lista
079400940307     C     $LOP          IFEQ      2
079500940307     C     WINKR         OREQ      *ON
079600940307     C                   MOVE      'S1'          $GEST
079700940307     C*  altrimenti, per F12 torno a menu
079800940307     C                   ELSE
079900940307     C                   MOVE      *ON           $FINE
080000940307     C                   END
080100940307     C*
080200940307     C                   ENDSR
080300940307     C/EJECT
080400940307     C************************************************************
080500940307     C* GESTIONE F18 VIDEO D1
080600940307     C************************************************************
080700940307     C     F18D1         BEGSR
080800940307     C*
080900940307     C                   MOVE      'D2'          $GEST
081000940307     C                   MOVEL     *ON           $INZD2
081100940307     C*
081200940307     C                   ENDSR
081300940307     C/EJECT
081400940307     C************************************************************
081500940307     C* GESTIONE F24 VIDEO D1
081600940609     C* input
081700940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
081800940509     C* $FX1 =Testi tasti funzione
081900940509     C* $FC1 = Abilitazioni
082000940509     C* $FR1 = Inversioni di fondo
082100940307     C************************************************************
082200940307     C     F24D1         BEGSR
082300940506     C*
082400940506     C                   MOVEA     $FX1          $FX
082500940506     C                   MOVEA     $FC1          $FC
082600940506     C                   MOVEA     $FR1          $FR
082700940506     C                   Z-ADD     $ULKD1        $ULK
082800940929     C                   Z-ADD     79            $L01
082900940929     C                   Z-ADD     79            $L02
083000940506     C                   EXSR      RIEKEY
083100940506     C                   Z-ADD     $ULK          $ULKD1            3 0
083200940720     C                   MOVEA     $FC           $FC1
083300940506     C                   MOVEL     $KEY1         Z1KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
083400940506     C                   MOVEL     $KEY2         Z1KE2
083500940506     C                   WRITE     FMTZ1
083600940506     C*
083700940307     C*
083800940307     C                   ENDSR
083900940307     C/EJECT
084000940307     C************************************************************
084100940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
084200940307     C************************************************************
084300961015     C     CTRD1         BEGSR
084400940307     C*
084500940127     C                   SETOFF                                       99
084600940127     C* non imposto pos. del cursore
084700940127     C                   Z-ADD     0             H1RIGA
084800940127     C                   Z-ADD     0             H1COLO
084900040929A1108C* se presente tempificatore ci deve essere anche il tipo
085000040929  "  C                   IF        D1Tempo <> *blanks and d1tempif = *blanks
085100040929  "  C                   SETON                                        6499
085200040929A1108C                   ENDIF
085300940131     C*
085400960416     C* flag abilitazione a iva valido
085500960416     C                   IF        D1IVA <> XscOn and D1IVA <> XscOff and
085600960416     C                             D1IVA <> *blank
085700960416     C                   SETON                                        6099
085800960416     C                   ENDIF
085900960416     C*
086000960416     C* flag abilitazione a registrazioni valido
086100960416     C                   IF        D1Reg <> XscOn and D1Reg <> XscOff and
086200960416     C                             D1Reg <> *blank
086300960416     C                   SETON                                        6199
086400960416     C                   ENDIF
086500940131     C*
086600961015     C* Tempificazione numeratore
086700961015     C                   Clear                   D1TempifDE
086800961015     C                   IF        D1Tempif<>*blank
086900961015     C                   RESET                   X07ValDS
087000961015     C                   MOVE      '1'           X07RIC
087100961015     C                   MOVEL     'ANDIZ000'    X07TRC
087200961015     C                   MOVEL     'DSPTEMPNM'   X07CAM
087300961015     C                   MOVEL     D1Tempif      X07VAL
087400961015     C                   CALLb     'X07VALR'
087500961015     C                   PARM                    X07ValDS
087600961015   B3C     X07ERR        IFEQ      '0'
087700961015     C                   MOVEL     X07DES        D1TempifDe
087800961015   X3C                   ELSE
087900961015     C                   Seton                                        6399
088000961015   E3C                   ENDIF
088100961015     C*
088200961015     C                   Endif
088300961015     C*
088400940117     C                   ENDSR
088500940224     C/EJECT
088600940127     C************************************************************
088700940131     C* GESTIONE LISTA
088800940127     C************************************************************
088900940127     C     GESS1         BEGSR
089000940223     C* inizializzazione videata
089100940223     C     $INZS1        IFEQ      *ON
089200940127     C                   EXSR      INZS1
089300940223     C                   MOVE      *OFF          $INZS1
089400940127     C                   ENDIF
089500940223     C* emissione piede videata se proveniente da altra
089600940223     C* salvataggio valore formato di provenienza
089700940202     C     $LASTV        IFNE      'S1'
089800940202     C                   WRITE     FMTT1
089900940224     C                   EXSR      GESPIE
090000940202     C                   MOVE      $LASTV        $LASTG
090100940202     C                   MOVE      'S1'          $LASTV
090200940202     C                   END
090300940223     C*
090400940223     C     WMAX          IFEQ      0
090500940211     C                   WRITE     FMTD3
090600940211     C                   ENDIF
090700980717     C*
090800980717     C* Inizializzazione indice tasti funzione personalizzati
090900980717     C                   Eval      PosFun = 25
091000980717     C*
091100940831     C*Serve per emissione finestra pgm invio ang727r (per open/close
091200940831     C*window)
091300940831     C                   MOVE      '1'           WIN727            1
091400940127     C*
091500940131     C                   EXFMT     FMTC1
091600940204     C     C1NRR         IFNE      0
091700940204     C                   Z-ADD     C1NRR         WSFL
091800940204     C                   ENDIF
091900940127     C                   Z-ADD     SFLNRR        C1RCD
092000940506     C*
092100940506     C* controllo abilitazione tasti funzione
092200940506     C*
092300940506     C                   EXSR      CTRKS1
092400940506     C     *IN99         IFEQ      *OFF
092500940506     C*
092600940127     C*
0927009401271    C                   SELECT
092800940127     C* F3=Fine
092900940506     C     $TASTO        WHENEQ    F03
093000940309     C                   EXSR      F03S1
093100940131     C* F10=Immissione
093200940506     C     $TASTO        WHENEQ    F10
093300940309     C                   EXSR      F10S1
093400940131     C* F12=Ritorno
093500940506     C     $TASTO        WHENEQ    F12
093600940309     C                   EXSR      F12S1
093700940131     C* F17=Parzializzazioni
093800940506     C     $TASTO        WHENEQ    F17
093900940309     C                   EXSR      F17S1
094000940131     C* F18=Modifica valori assunti
094100940506     C     $TASTO        WHENEQ    F18
094200940309     C                   EXSR      F18S1
094300940224     C* F23=Altre opzioni
094400940506     C     $TASTO        WHENEQ    F23
094500940309     C                   EXSR      F23S1
094600940224     C* F24=Altri tasti
094700940506     C     $TASTO        WHENEQ    F24
094800940309     C                   EXSR      F24S1
094900940127     C* ROLL-UP
095000940506     C     $TASTO        WHENEQ    ROLLUP
095100940127     C                   EXSR      ROLS1
095200980717     C*
095300980717     C* Personalizzata
095400980717     C     $PGF2(PosFun) Whenne    *BLANK
095500980717     C                   EXSR      FperS1
095600980717     C*
0957009401271O   C                   OTHER
095800940127     C* CONTROLLO DATI
095900940131     C                   EXSR      CTRC1
096000940201     C     *IN99         IFEQ      *OFF
096100940131     C                   EXSR      CTRS1
096200940131     C                   END
0963009401271-   C                   ENDSL
096400940127     C*
096500940506     C                   ENDIF
096600940127     C                   ENDSR
096700940224     C/EJECT
096800980717     C************************************************************
096900980717     C* Chiamata programma con tasto personalizzato
097000980717     C************************************************************
097100980717     C     FPerS1        BEGSR
097200980717     C*
097300980717     C* salvo la DS di ingresso
097400980717    >C                   MOVEL     A103DS        $SaveDS
097500980717     C*
097600980717    >C                   MOVEL     A103DS        KPJBU
097700980717    >C                   CALL      $PGF2(PosFun)                        22
097800980717     C                   PARM                    KPJBA
097900980717     C                   PARM                    NOMPGM
098000980717     C*
098100980717     C* ripristino la DS di ingresso
098200980717    >C                   MOVEL     $SaveDS       A103DS
098300980717     C*
098400980717     C                   ENDSR
098500980717     C/EJECT
098600940127     C************************************************************
098700940131     C* INIZIALIZZAZIONE LISTA
098800940127     C************************************************************
098900940127     C     INZS1         BEGSR
099000940302     C* pulizia SFL
099100940128     C                   SETOFF                                         3031
099200940131     C                   WRITE     FMTC1
099300940128     C                   SETON                                          31
099400940127     C*
099500940128     C* riempimento opzioni
099600940223     C                   MOVEA     $TX1          $TX
099700940223     C                   MOVEA     $FL1          $FL
099800940309     C                   Z-ADD     0             WCOZ
099900940225     C*  equivale al num.di elementi della schiera $TX1
100000980717     C                   Z-ADD     OPZLEN        WMXO
100100950403     C                   Z-ADD     76            $L01
100200950403     C                   Z-ADD     76            $L02
100300940128     C                   EXSR      RIEOPZ
100400950403     C                   Z-ADD     23            X
100500950403     C                   MOVE      FF23          $FC2(X)
100600940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
100700940608     C                   MOVEL     WOPZ2         C1OPZ2
100800940225     C                   Z-ADD     WCOZ          C1COZ
100900940128     C*
101000940128     C* CARICAMENTO SFL A PAGINE
101100940128     C*
101200940201     C                   Z-ADD     0             S1NRR
101300940128     C                   Z-ADD     0             WMAX
101400940224     C*
101500940224     C* Posizionamento su file pilota
101600940302     C                   CLEAR                   C1POS
101700941006     C*
101800941005    >C                   EXSR      SETUTN
101900940302     C* ripristino i valori del posizionamento
102000941006     C                   MOVE      W1POS         C1POS
102100940131     C*
102200940201     C* Lettura file pilota
102300941024    >C                   EXSR      REDNMR
102400940131     C*
102500940131     C* Carico 1a pagina
102600940127     C                   EXSR      ROLS1
102700940201     C                   Z-ADD     SFLPAG        WPAG
102800940127     C*
102900940127     C                   ENDSR
103000940127     C/EJECT
103100940127     C************************************************************
103200940131     C* CARICAMENTO PAGINA LISTA
103300940127     C************************************************************
103400940127     C     ROLS1         BEGSR
103500940127     C*
103600940128     C                   SETOFF                                       32
103700940223     C                   Z-ADD     0             Y
103800940127     C                   Z-ADD     WMAX          S1NRR
103900940127     C*
104000940127     C* Leggo dal file anagrafico per caricare la lista
104100940127     C*
1042009401311    C     $EFILE        DOWEQ     *OFF
104300940201     C     Y             ANDLT     WPAG
104400940127     C*
104500940131     C                   EXSR      RIES1
104600940127     C*
104700940127     C                   ADD       1             S1NRR
104800940127     C                   ADD       1             Y
104900940127     C*
105000940131     C                   WRITE     FMTS1
105100940131     C*
105200941024    >C                   EXSR      REDNMR
105300940128     C*
1054009401271-   C                   ENDDO
105500940127     C*
105600940223     C                   Z-ADD     S1NRR         WMAX                 30
105700940127     C*
105800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
105900940127     C*
106000940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
106100940127     C                   MVR                     RESTO             3 0
106200940131     C     PAGINE        MULT      SFLPAG        C1RCD
1063009401271    C     RESTO         IFGT      0
106400940127     C                   ADD       1             C1RCD
1065009401271E   C                   ELSE
106600940407     C                   SUB       SFLPAG        C1RCD
106700940407     C                   ADD       1             C1RCD
1068009401271-   C                   ENDIF
106900940128     C*
107000940127     C                   ENDSR
107100940224     C/EJECT
107200940224     C************************************************************
107300940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
107400940224     C************************************************************
107500941005    >C     SETUTN        BEGSR
107600940224     C*
107700940224     C                   SELECT
107800941005    >C* Ord.1 = Profilo
107900940224     C     $ORD          WHENEQ    1
108000940314    >C*-----------------------------------------------------------
108100940314    >C* Gestione posizionamento
108200940314    >C* Se il campo W1POS è > *blank imposto il posizionamento con
108300940314    >C* W1POS altrimenti lo imposto con il valore di parametrizzazione
108400940314    >C*-----------------------------------------------------------
108500981229E0045C***  *LOVAL        SETLL     ANNMR02L
108600981229  "  C                   z-add     d1grp         nmrgru
108700981229  "  C                   movel     d1serienm     nmrser
108800981229  "  C     K2nmr2        setll     annmr02
108900981229  "  C*
109000981229  "  C     $ord          wheneq    2
109100981229  "  C                   movel     xscsoc        nmrsoc
109200981229  "  C                   movel     *blank        nmruni
109300981229  "  C                   movel     *blank        nmrctb
109400981229  "  C                   movel     *blank        nmrkey
109500981229  "  C                   movel     d1serienm     nmrser
109600981229  "  C                   movel     *blank        nmrtmp
109700981229  "  C     K6nmr1        setll     annmr01
109800981229E0045C*
109900941018     C                   ENDSL
110000940224     C*
110100940224     C                   ENDSR
110200940128     C/EJECT
110300940128     C************************************************************
110400940131     C* LETTURA RCD ARCHIVIO PILOTA
110500940128     C************************************************************
110600941024    >C     REDNMR        BEGSR
110700940128     C*
110800940131     C                   MOVEL     *OFF          $EFILE
110900940131     C                   MOVEL     *OFF          $RCDOK
111000940131     C*
1111009401311    C     $EFILE        DOUEQ     *ON
111200940131     C     $RCDOK        OREQ      *ON
111300940131     C*
1114009401312    C                   SELECT
111500940131     C     $ORD          WHENEQ    1
111600981229E0045C***                READ      ANNMR02L                               21
111700981229  "  C                   read      annmr02                                21
111800981229  "  C*
111900981229  "  C     $ord          wheneq    2
112000981229  "  C                   read      annmr01                                21
112100981229E0045C*
1122009401312-   C                   ENDSL
112300940131     C*
112400940131     C                   MOVE      *IN21         $EFILE
112500940131     C*
1126009401312    C     $EFILE        IFEQ      *OFF
112700960416     C* reperisco i dati della serie
112800960416     C                   MOVE      '1'           XTBRIC
112900960416     C                   MOVE      XSCSOC        XTBAZI
113000960416     C                   MOVE      'G13'         XTBCOD
113100961015     C                   MOVE      *ZERO         XTBKEY
113200960416     C                   MOVE      NMRSer        XTBKEY
113300961003     C                   CALLb     'XATB'
113400960416     C                   PARM                    XATBDS
113500960416     C     XTBERR        IFEQ      '0'
113600960416     C                   MOVEL     XTBUNI        TABG13
113700960416     C                   ELSE
113800960416     C                   RESET                   TABG13
113900960416     C                   ENDIF
114000960416     C*
114100941024     C                   EXSR      SELNMR
1142009401312-   C                   ENDIF
114300940131     C*
1144009401311-   C                   ENDDO
114500940131     C*
114600940131     C                   MOVE      *IN21         *IN33
114700940131     C*
114800940131     C                   ENDSR
114900940224     C/EJECT
115000940224     C************************************************************
115100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
115200940224     C************************************************************
115300940224     C     CLCPAG        BEGSR
115400940224     C* Input :
115500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
115600940224     C*          cursore
115700940224     C* - SFLPAG = numero rcd per pagina sfl
115800940224     C* Output :
115900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
116000940224     C*
116100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
116200940224     C                   MVR                     RESTO             3 0
116300940224     C     RESTO         IFGT      0
116400940224     C                   ADD       1             PAGINE
116500940224     C                   ENDIF
116600940224     C     PAGINE        MULT      SFLPAG        WPAG
116700940224     C*
116800940224     C                   ENDSR
116900940131     C/EJECT
117000940131     C************************************************************
117100940131     C* SELEZIONI SULLA RIGA LETTA
117200940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
117300940314     C* non rientra nei parametri di parzializzazione
117400940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
117500940314     C* superiore stabilito nei parametri
117600940314     C*
117700940131     C************************************************************
117800941024    >C     SELNMR        BEGSR
117900940131     C*
118000940202     C                   MOVEL     *ON           $RCDOK
118100940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
118100940131A1211C                   MOVEL     *hival        ALL9             15
118200940314     C* creo limite massimo parzializzazioni
118300960416    >C     D1SerieNm     CAT       ALL9:0        SerieNm999
118400960416     C*
118500940314     C                   SELECT
118600940314     C*
118700940314     C* SELEZIONI RISPETTO AI VALORI RCD
118800940314     C*
118900940314    >C* << elaborare eventuali selezioni che non riguardano le
119000940316    >C*    parzializzazioni ma più propriamente i dati del rcd ;
119100940314    >C*    esempio : esclusione degli annullati. >>
119200941005     C*
119300940314     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
119400941024     C     NMRSOC        WHENNE    *BLANKS
119500941103     C     NMRSOC        ANDNE     XSCSOC
119600950308     C                   MOVEL     *OFF          $RCDOK
119700940314     C*
119800960416     C     NMRSer        WHENLT    D1SerieNm
119900960416     C     NMRSer        ORGT      SerieNm999
120000960416     C                   MOVEL     *OFF          $RCDOK
120100960416     C*
120200960416     C     D1IVA         WHENNE    IVAG13
120300960416     C     D1IVA         ANDNE     *blank
120400960416     C                   MOVEL     *OFF          $RCDOK
120500960416     C*
120600960416     C     D1Reg         WHENNE    RegG13
120700960416     C     D1Reg         ANDNE     *blank
120800960416     C                   MOVEL     *OFF          $RCDOK
120900960416     C*
121000961015     C     D1Tempif      WHENNE    TpmG13
121100961015     C     D1Tempif      ANDNE     *blank
121200961015     C                   MOVEL     *OFF          $RCDOK
121300981229E0045C*
121400981229  "  C     D1grp         WHENNE    *zero
121500981229  "  C     nmrgru        ANDNE     d1grp
121600981229E0045C                   MOVEL     *OFF          $RCDOK
121700961015     C*
121800940202     C                   ENDSL
121900040929A1108C*
122000040929  "  C     D1tempo       ifne      *blanks
122100040929  "  C     ' '           CHECK     D1tempo       NR                3 0
122200040929  "  C                   Z-ADD     NR            J                 2 0
122300040929  "  C                   if        %subst(d1tempo:J)<> %subst(nmrtmp:J)
122400040929  "  C                   MOVEL     *OFF          $RCDOK
122500040929  "  C                   ENDIF
122600040929A1108C                   ENDIF
122700940202     C*
122800940202     C                   ENDSR
122900940131     C/EJECT
123000940131     C************************************************************
123100940131     C* RIEMPIMENTO RIGA LISTA
123200940131     C************************************************************
123300940131     C     RIES1         BEGSR
123400940131     C*
123500940131     C                   CLEAR                   FMTS1
123600940314    >C* << riempio i campi del sfl, compresi gli hidden con i valori
123700940314    >C*    del rcd letto >>
123800940131     C*
123900941018    >C                   MOVEL     NMRSOC        S1SOC
124000941018    >C                   MOVEL     NMRUNI        S1UNI
124100981228E0045C                   z-add     nmrgru        S1gruppo
124200941018    >C                   MOVEL     NMRCTB        S1CTB
124300941018    >C                   MOVEL     NMRKEY        S1KEY
124400941018    >C                   MOVEL     NMRSER        S1SER
124500941018    >C                   Z-ADD     NMRNUM        S1NUM
124600961003    XC                   CALLb     'XDT4000'
124700941026     C                   PARM      NMRDAT        XDTAMG           10
124800941026     C                   PARM      *ZEROS        XDTGMA            6 0
124900941026     C                   PARM      2             XDTSTA            1 0
125000941026     C                   MOVE      XDTGMA        S1DAT
125100941025     C*
125200960416     C*---------------------------------------------------------------------
125300941025     C*Aggancio tabella g13 per tempificazione
125400941025     C*
125500960416     C* non ce n'è più bisogno perchè è stata spostata nella routine REDNMR
125600960416     C*                  MOVE      '1'           XTBRIC
125700960416     C*                  MOVE      XSCSOC        XTBAZI
125800960416     C*                  MOVE      *ZERO         XTBKEY
125900960416     C*                  MOVE      'G13'         XTBCOD
126000960416     C*                  MOVE      S1SER         XTBKEY
126100961003     C*                  CALLb     'XATB'
126200960416     C*                  PARM                    XATBDS
126300960416B1   C*    XTBERR        IFEQ      '0'
126400960416     C*                  MOVEL     XTBUNI        TABG13
126500960416     C*---------------------------------------------------------------------
126600941025     C                   RESET                   DSUNO
126700941025     C                   RESET                   DSDUE
126800950104     C                   MOVE      NMRTMP        H1TMP
126900941025     C* Giornaliero, mensile, annuale
127000941025     C     TPMG13        IFEQ      '1'
127100941025     C     TPMG13        OREQ      '3'
127200941025     C     TPMG13        OREQ      '4'
127300941025    >C                   MOVEL     NMRTMP        DSGG
127400941025     C                   MOVEL     NMRTMP        WRK4              4
127500941025     C                   MOVE      WRK4          DSMM
127600941025    >C                   MOVE      NMRTMP        DSAA
127700941025     C     TPMG13        IFEQ      '3'
127800941025     C                   MOVE      *BLANKS       DSBAR
127900941025     C                   END
128000941025     C     TPMG13        IFEQ      '4'
128100941025     C                   MOVE      *BLANKS       DSBAR
128200941025     C                   MOVE      *BLANKS       DSBAR1
128300941025     C                   END
128400941025     C                   MOVE      DSUNO         S1TMP
128500941025     C                   END
128600941025     C* esercizio
128700941025     C     TPMG13        IFEQ      '2'
128800941025     C                   MOVE      NMRTMP        WRK5              5
128900941025     C                   MOVEL     WRK5          DSAAE
129000941025     C                   MOVE      WRK5          DSPRE
129100941025     C                   MOVE      DSDUE         S1TMP
129200941025     C                   END
129300941025     C* progressivo
129400941025     C     TPMG13        IFEQ      '5'
129500941025     C                   MOVE      NMRTMP        S1TMP
129600941025     C                   END
129700940131     C*
129800960416     C                   MOVE      DESG13        S1SerieNmD
129900960416E1   C******             END
130000960416     C*
130100940131     C                   ENDSR
130200940309     C/EJECT
130300940309     C************************************************************
130400940309     C* GESTIONE F03 VIDEO S1
130500940309     C************************************************************
130600940309     C     F03S1         BEGSR
130700940309     C*
130800940309     C                   MOVE      *ON           $FINE
130900940325     C* fine programma
131000940325     C                   EXSR      ENDPGM
131100940309     C*
131200940309     C                   ENDSR
131300940309     C/EJECT
131400940309     C************************************************************
131500940309     C* GESTIONE F10 VIDEO S1
131600940609     C* AGGIUNTA RECORD
131700940309     C************************************************************
131800940309     C     F10S1         BEGSR
131900940309     C*
132000941018    >C                   RESET                   A104DS
132100941018    >C                   MOVEL     '01'          OPZ104
132200941018    >C                   MOVEL     A104DS        KPJBU
132300941018    >C                   CALL      'ANA104R'
132400940309     C                   PARM                    KPJBA
132500941018    >C                   MOVEL     KPJBU         A104DS
132600940309     C* ritorno da PGM gestione
132700941018    >C                   EXSR      RTN104
132800940309     C     WINZS1        IFEQ      *ON
132900940309     C                   MOVE      *ON           $INZS1
133000940309     C* se esistevano già righe sul sfl
133100940309     C* calcolo pagina a cui deve posizionarsi
133200940309     C     WSFL          IFGT      0
133300940309     C                   EXSR      CLCPAG
133400940309     C* altrimenti carico solo la 1a pagina
133500940309     C                   ELSE
133600940309     C                   Z-ADD     SFLPAG        WPAG
133700940309     C                   ENDIF
133800940309     C                   ENDIF
133900940309     C*
134000940627     C                   MOVE      *HIVAL        $LASTV
134100940627     C*
134200940309     C                   ENDSR
134300940309     C/EJECT
134400940309     C************************************************************
134500940309     C* GESTIONE F12 VIDEO S1
134600940309     C************************************************************
134700940309     C     F12S1         BEGSR
134800940309     C*
134900940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
135000940309     C     $LOP          IFEQ      1
135100940309     C                   MOVE      'D1'          $GEST
135200940309     C*  se la 1a videata è questa, per F12 torno a menu
135300940309     C                   ELSE
135400940309     C                   MOVE      *ON           $FINE
135500940309     C                   END
135600940309     C*
135700940309     C                   ENDSR
135800940309     C/EJECT
135900940309     C************************************************************
136000940309     C* GESTIONE F17 VIDEO S1
136100940309     C************************************************************
136200940309     C     F17S1         BEGSR
136300940309     C*
136400940309     C                   MOVE      'D1'          $GEST
136500940831     C* forza certi valori nel caso il pgm sia stato richiamato con
136600940831     C* chiave specifica
136700940309     C*
136800940309     C                   ENDSR
136900940309     C/EJECT
137000940309     C************************************************************
137100940309     C* GESTIONE F18 VIDEO S1
137200940309     C************************************************************
137300940309     C     F18S1         BEGSR
137400940309     C*
137500940309     C                   MOVE      'D2'          $GEST
137600940309     C                   MOVEL     *ON           $INZD2
137700940309     C*
137800940309     C                   ENDSR
137900940309     C/EJECT
138000940309     C************************************************************
138100940309     C* GESTIONE F23 VIDEO S1
138200940309     C************************************************************
138300940309     C     F23S1         BEGSR
138400940309     C*
138500950403     C                   Z-ADD     76            $L01
138600950403     C                   Z-ADD     76            $L02
138700940309     C                   EXSR      RIEOPZ
138800950403     C                   Z-ADD     23            X
138900950403     C                   MOVE      FF23          $FC2(X)
139000940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
139100940608     C                   MOVEL     WOPZ2         C1OPZ2
139200940309     C                   Z-ADD     WCOZ          C1COZ
139300940309     C*
139400940309     C                   ENDSR
139500940309     C/EJECT
139600940309     C************************************************************
139700940309     C* GESTIONE F24 VIDEO S1
139800940309     C************************************************************
139900940309     C     F24S1         BEGSR
140000940309     C*
140100940506     C*
140200940506     C                   MOVEA     $FX2          $FX
140300940506     C                   MOVEA     $FC2          $FC
140400940506     C                   MOVEA     $FR2          $FR
140500940506     C                   Z-ADD     $ULKS1        $ULK
140600940929     C                   Z-ADD     79            $L01
140700940929     C                   Z-ADD     79            $L02
140800940506     C                   EXSR      RIEKEY
140900940506     C                   Z-ADD     $ULK          $ULKS1            3 0
141000940720     C                   MOVEA     $FC           $FC2
141100940506     C                   MOVEL     $KEY1         Z2KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
141200940506     C                   MOVEL     $KEY2         Z2KE2
141300940506     C                   WRITE     FMTZ2
141400940506     C*
141500940309     C                   ENDSR
141600940128     C/EJECT
141700940128     C************************************************************
141800940131     C* CONTROLLO TESTATA LISTA
141900940128     C************************************************************
142000940131     C     CTRC1         BEGSR
142100940128     C*
142200940201     C                   MOVE      *OFF          *IN99
142300940131     C*
142400940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
142500940202     C* L'INIZIALIZZAZIONE LISTA
142600940202     C*
142700941006     C                   SELECT
142800941006      *
142900941006      *
143000941018    >C     C1POS         WHENNE    W1POS
143100940202     C                   EXSR      POSS1
143200941006     C                   ENDSL
143300940202     C*
143400940202     C                   ENDSR
143500940202     C/EJECT
143600940202     C************************************************************
143700940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
143800940202     C************************************************************
143900940202     C     POSS1         BEGSR
144000940202     C*
144100940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
144200940202     C*
1443009402021    C     WMAX          IFGT      0
144400940202     C                   READC     FMTS1                                  21
1445009402022    C     *IN21         DOWEQ     *OFF
144600940202     C     *IN99         ANDEQ     *OFF
144700940202     C*
1448009402023    C     S1OPZ         IFNE      0
144900940202     C                   SETON                                        51  99
1450009402023-   C                   ENDIF
145100940202     C* se rilevata opzione attiva, attivo sflnxtchg
1452009402023    C     *IN99         IFEQ      *ON
145300940202     C                   MOVE      *ON           *IN32
145400940202     C                   UPDATE    FMTS1
1455009402023E   C                   ELSE
145600940202     C                   READC     FMTS1                                  21
1457009402023-   C                   ENDIF
145800940202     C*
1459009402022-   C                   ENDDO
146000940202     C*
1461009402021-   C                   ENDIF
146200940202     C*
146300940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
146400940221     C* AL VALORE DI POSIZIONAMENTO :
146500940221     C* se questo è maggiore del valore del rispettivo campo di
146600940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
146700940221     C* con i valori di parzializzazione.
146800940221     C*
1469009402021    C     *IN99         IFEQ      *OFF
147000940721    >C                   MOVE      *BLANK        W1POS
147100940202     C*
1472009402022    C                   SELECT
147300940202     C*
147400940202     C     $ORD          WHENEQ    1
147500940131     C*
147600940202     C                   MOVE      *ON           *IN99
147700940223     C                   MOVE      *ON           $INZS1
147800940831     C*
147900940831     C     $ORD          WHENEQ    99
148000940831     C*
148100940831     C                   MOVE      *ON           *IN99
148200940831     C                   MOVE      *ON           $INZS1
148300940202     C*
1484009402022-   C                   ENDSL
148500940202     C*
1486009402021-   C                   ENDIF
148700940202     C*
148800940131     C                   ENDSR
148900940131     C/EJECT
149000940131     C************************************************************
149100940131     C* CONTROLLO OPZIONI LISTA
149200940131     C************************************************************
149300940131     C     CTRS1         BEGSR
149400940131     C*
149500940202     C                   MOVEL     *OFF          $ESCI
149600940201     C                   SETOFF                                       99
149700940131     C                   Z-ADD     0             S1OPZ
149800940131     C*
149900940127     C* Leggo il sfl solo se ci sono rcd
1500009401311    C     WMAX          IFGT      0
150100940131     C                   READC     FMTS1                                  21
150200940127     C*
150300940131     C* esce se fine sfl o errore che richiede l'uscita
1504009401312    C     *IN21         DOWEQ     *OFF
150500940131     C     $ESCI         ANDEQ     *OFF
150600940201     C                   Z-ADD     S1NRR         C1RCD
150700940131     C* ctrl su riga
150800940131     C                   EXSR      RECS1
150900940131     C* gestione opzioni
1510009401313    C     S1OPZ         IFNE      0
151100940201     C     *IN99         ANDEQ     *OFF
151200940131     C                   EXSR      OPZS1
1513009401273-   C                   ENDIF
151400940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
1515009402013    C     *IN99         IFEQ      *ON
151600940131     C                   MOVE      *ON           *IN32
1517009402233-   C                   ENDIF
151800940223     C*
151900940131     C                   Z-ADD     0             S1OPZ
152000940223     C*
152100940127     C                   UPDATE    FMTS1
152200941125     C                   MOVE      *OFF          *IN32
152300940223     C*
152400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1525009401313    C     $ESCI         IFEQ      *OFF
152600940131     C                   READC     FMTS1                                  21
152700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1528009402014    C     *IN21         IFEQ      *ON
152900940201     C                   MOVE      WINZS1        $INZS1
153000940204     C* calcolo pagina a cui deve posizionarsi
153100940728     C     WSFL          IFGT      0
153200940728     C                   EXSR      CLCPAG
153300940728     C                   ELSE
153400940728     C                   Z-ADD     SFLPAG        WPAG
153500940728     C                   ENDIF
1536009402014-   C                   ENDIF
1537009402013-   C                   ENDIF
153800940131     C*
1539009401272-   C                   ENDDO
154000940127     C*
1541009401311-   C                   ENDIF
154200940131     C*
154300940127     C                   ENDSR
154400940127     C/EJECT
154500940127     C************************************************************
154600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
154700940127     C************************************************************
154800940131     C     RECS1         BEGSR
154900940131     C*
155000940201     C* reset indicatori DSPATR
155100940201     C                   MOVE      *ALL'0'       IN4049           10
155200940201     C                   MOVEA     IN4049        *IN(40)
155300940201     C*
155400940131     C* Controllo se Utente Autorizzato alla scelta
155500940202     C     S1OPZ         IFNE      0
155600940131     C                   Z-ADD     1             X
155700940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
155800980717     C* mi salvo l'indice dell' opzione
155900980717     C   21              Z-add     X             PosOpz            2 0
156000940211     C     *IN21         IFEQ      *OFF
156100940223     C     $FL1(X)       OREQ      'N'
156200940131     C                   MOVE      *ON           $ESCI
156300940201     C                   SETON                                        504099
156400940131     C                   ENDIF
156500940202     C                   ENDIF
156600030225     c*
156700030225A0994C* Controlli autorizzazioni
156800030225  "  c                   exsr      RepAut
156900030225  "  C                   if        *in99=*on
157000030225  "  C                   MOVE      *ON           $ESCI
157100030225  "  C                   SETON                                        5440
157200030225A0994c                   endif
157300940131     C*
157400940131     C                   ENDSR
157500940224     C/EJECT
157600940131     C************************************************************
157700940131     C* GESTIONE OPZIONI LISTA
157800940131     C************************************************************
157900940131     C     OPZS1         BEGSR
158000940201     C*
1581009402251    C                   SELECT
158200940225     C*
158300940225     C* se opz=Selezione passo la chiave al pgm chiamante
158400940225     C     S1OPZ         WHENEQ    1
158500941018    >C* << passare nei campi appositi della DS D103DS il valore
158600940314    >C*    della chiave della VL univoca (se c'è) >>
158700941018    >C                   MOVE      S1OPZ         OPZ103
158800941018    >C                   MOVE      S1SER         SER103
158900941025    >C                   MOVE      S1SOC         SOC103
159000941025    >C                   MOVE      S1UNI         UNI103
159100941025    >C                   MOVE      S1CTB         CTB103
159200941025    >C                   MOVE      S1KEY         KEY103
159300981229E0045C                   z-add     S1gruppo      GRP103
159400940225     C                   MOVE      *ON           $ESCI
159500940225     C                   MOVE      *ON           $FINE
159600940909     C*
159700940909     C                   MOVE      *HIVAL        $LASTV
159800980717     C*
159900980717     C* se selezionata opzione personalizzata, richiamo il programma
160000980717     C* corrispondente
160100980717     C     $PGO1(PosOpz) Whenne    *Blank
160200980717     C                   RESET                   A104DS
160300980717     C* << passare nei campi appositi della DS A043DS il valore
160400980717     C*    della chiave della VL univoca (se c'è) >>
160500980717     C                   MOVEL     S1OPZ         OPZ104
160600980717     C* passare nei campi appositi della DS D002DS il valore
160700980717     C* della chiave della VL univoca
160800980717    >C                   MOVEL     S1OPZ         OPZ104
160900980717    >C                   MOVEL     S1SER         SER104
161000980717    >C                   MOVE      S1SOC         SOC104
161100980717    >C                   MOVE      S1UNI         UNI104
161200980717    >C                   MOVE      S1CTB         CTB104
161300980717    >C                   MOVE      S1KEY         KEY104
161400980717    >C                   MOVE      H1TMP         TMP104
161500981229E0045C                   z-add     s1gruppo      GRP104
161600980717     C*
161700980717     C                   MOVEL     A104DS        KPJBU
161800980717     C*
161900980717     C                   CALL      $PGO1(PosOpz)                        22
162000980717     C                   PARM                    KPJBA
162100980717     C                   PARM                    NOMPGM
162200980717     C*
162300980717     C                   MOVEL     KPJBU         A104DS
162400980717     C*
162500980717     C* ritorno da PGM
162600980717     C                   EXSR      RTN104
162700980717     C*
162800940831     C*
1629009402251O   C                   OTHER
163000940225     C*
163100941018    >C                   RESET                   A104DS
163200941018    >C* << passare nei campi appositi della DS D104DS il valore
163300940314    >C*    della chiave della VL univoca (se c'è) >>
163400941018    >C                   MOVEL     S1OPZ         OPZ104
163500941018    >C                   MOVEL     S1SER         SER104
163600941025    >C                   MOVE      S1SOC         SOC104
163700941025    >C                   MOVE      S1UNI         UNI104
163800941025    >C                   MOVE      S1CTB         CTB104
163900941025    >C                   MOVE      S1KEY         KEY104
164000950104    >C                   MOVE      H1TMP         TMP104
164100940314     C*
164200941018    >C                   MOVEL     A104DS        KPJBU
164300941018    >C                   CALL      'ANA104R'
164400940314     C                   PARM                    KPJBA
164500941018    >C                   MOVEL     KPJBU         A104DS
164600940201     C*
164700940223     C* ritorno da PGM gestione
164800941018    >C                   EXSR      RTN104
164900940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1650009402252    C     *IN99         IFEQ      *ON
165100940223     C                   SETON                                        40
1652009402252-   C                   ENDIF
165300940131     C*
165400940506     C                   MOVE      *HIVAL        $LASTV
1655009402251-   C                   ENDSL
165600940225     C*
165700940131     C                   ENDSR
165800940223     C/EJECT
165900940223     C************************************************************
166000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
166100940223     C************************************************************
166200941018    >C     RTN104        BEGSR
166300940223     C*
166400940223     C* modo di ritorno
166500940223     C*
1666009402231    C                   SELECT
166700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
166800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
166900940314    >C*    proposte, normalmente sempre valide, ma modificabili
167000940314    >C*    per situazioni particolari >>
167100940223     C* 1 = F3
167200941018    >C     RET104        WHENEQ    '1'
167300940224     C                   MOVE      *ON           $ESCI
167400940223     C                   MOVE      *ON           $FINE
167500940223     C* 2 = F12
167600941018    >C     RET104        WHENEQ    '2'
167700940223     C                   MOVE      *ON           $ESCI
167800940223     C*
1679009402231-   C                   ENDSL
168000940223     C*
168100940223     C* operazione eseguite dal pgm chiamato
168200940223     C*
1683009402231    C                   SELECT
168400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
168500941018    >C     OPR104        WHENEQ    '1'
168600940223     C                   MOVE      *ON           WINZS1
168700940223     C*
1688009402231-   C                   ENDSL
168900940223     C*
169000940223     C* funzione non eseguibile per errore :
169100940223     C*
1692009402231    C                   SELECT
169300940223     C* 1 = funzione richiamata chiusa in errore
169400940316    >C*  eventualmente gestire altri codici di errore
169500941018    >C     ERR104        WHENEQ    '1'
169600940223     C                   MOVE      *ON           $ESCI
169700940223     C                   SETON                                        5299
169800940223     C*
1699009402231-   C                   ENDSL
170000940223     C*
170100940223     C                   ENDSR
170200940223     C/EJECT
170300940131     C************************************************************
170400940131     C* GESTIONE VALORI ASSUNTI
170500940131     C************************************************************
170600940131     C     GESD2         BEGSR
170700940131     C*
170800940223     C     $INZD2        IFEQ      *ON
170900940131     C                   EXSR      INZD2
171000940223     C                   MOVE      *OFF          $INZD2
171100940131     C                   END
171200940202     C*
171300940202     C     $LASTV        IFNE      'D2'
171400940923     C                   EXSR      GESPIE
171500940202     C                   MOVE      $LASTV        $LASTG
171600940202     C                   MOVE      'D2'          $LASTV
171700940707     C                   ENDIF
171800940131     C*
171900940707     C* se esistono errori sulla videata
172000940707     C* emetto la write del formato a indicatori spenti per vedere
172100940707     C* le eventuali decodifiche
172200940707     C     *IN99         IFEQ      *ON
172300940707     C                   MOVEA     *IN           WIN
172400940707     C                   MOVE      *ALL'0'       IN5098           49
172500940707     C                   MOVEA     IN5098        *IN(50)
172600940707     C                   WRITE     FMTD2
172700940707     C                   MOVEA     WIN           *IN
172800940707     C                   ENDIF
172900940707     C*
173000940131     C                   EXFMT     FMTD2
173100940131     C*
173200940131     C                   SELECT
173300940131     C* F3=Fine
173400940506     C     $TASTO        WHENEQ    F03
173500940309     C                   EXSR      F03D2
173600940131     C* F12=Ritorno
173700940506     C     $TASTO        WHENEQ    F12
173800940309     C                   EXSR      F12D2
173900940916     C* F24=Altri tasti
174000940916     C     $TASTO        WHENEQ    F24
174100940916     C                   EXSR      F24D2
174200940131     C*
174300940131     C                   OTHER
174400940131     C                   EXSR      CTRD2
1745009401312    C     *IN99         IFEQ      *OFF
174600940211     C                   MOVE      $LASTG        $GEST
1747009401313    C                   SELECT
174800940202     C     $LASTG        WHENEQ    'S1'
174900940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
175000940314     C     $ORD          IFNE      D2ORD
175100940223     C                   MOVE      *ON           $INZS1
175200940302     C                   MOVE      *BLANK        W1POS
175300940131     C                   END
175400940202     C     $LASTG        WHENEQ    'D1'
175500940318     C* per ora niente
1756009401313-   C                   ENDSL
175700940131     C* definisco i nuovi valori di default
175800940201     C                   Z-ADD     D2LOP         $LOP
175900940131     C                   Z-ADD     D2ORD         $ORD
176000941006     C     $ORD          IFEQ      2
176100941006     C                   SETON                                            02
176200941006     C                   ENDIF
176300940609    >C* << elenco altri dati default >>
1764009401312-   C                   END
176500940131     C*
176600940131     C                   ENDSL
176700940131     C*
176800940131     C                   ENDSR
176900940223     C/EJECT
177000940131     C************************************************************
177100940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
177200940131     C************************************************************
177300940201     C     INZD2         BEGSR
177400940131     C*
177500940131     C                   CLEAR                   FMTD2
177600940201     C                   MOVEL     $LOP          D2LOP
177700940831     C                   MOVE      $ORD          D2ORD
177800940314    >C* << elenco altri dati default >>
177900940131     C*
178000940131     C                   ENDSR
178100940309     C/EJECT
178200940309     C************************************************************
178300940309     C* GESTIONE F03 VIDEO D2
178400940309     C************************************************************
178500940309     C     F03D2         BEGSR
178600940309     C*
178700940309     C                   MOVE      *ON           $FINE
178800940325     C* fine programma
178900940325     C                   EXSR      ENDPGM
179000940309     C*
179100940309     C                   ENDSR
179200940309     C/EJECT
179300940309     C************************************************************
179400940309     C* GESTIONE F12 VIDEO D2
179500940309     C************************************************************
179600940309     C     F12D2         BEGSR
179700940309     C*
179800940309     C                   MOVE      $LASTG        $GEST
179900940309     C*
180000940309     C                   ENDSR
180100940916     C/EJECT
180200940916     C************************************************************
180300940916     C* GESTIONE F24 VIDEO D2
180400940916     C************************************************************
180500940916     C     F24D2         BEGSR
180600940916     C*
180700940916     C                   MOVEA     $FX3          $FX
180800940916     C                   MOVEA     $FC3          $FC
180900940916     C                   MOVEA     $FR3          $FR
181000940916     C                   Z-ADD     $ULKD2        $ULK
181100940929     C                   Z-ADD     56            $L01
181200940929     C                   Z-ADD     56            $L02
181300940916     C                   EXSR      RIEKEY
181400940916     C                   Z-ADD     $ULK          $ULKD2            3 0
181500940916     C                   MOVEA     $FC           $FC3
181600940916     C                   MOVEL     $KEY1         Z3KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
181700940916     C                   MOVEL     $KEY2         Z3KE2
181800940916     C                   WRITE     FMTZ3
181900940916     C*
182000940916     C                   ENDSR
182100940223     C/EJECT
182200940131     C************************************************************
182300940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
182400940131     C************************************************************
182500940131     C     CTRD2         BEGSR
182600940131     C*
182700940131     C                   SETOFF                                       99
182800940131     C* non imposto pos. del cursore
182900940131     C                   Z-ADD     0             H1RIGA
183000940131     C                   Z-ADD     0             H1COLO
183100940131     C*
183200940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
183300940201     C     D2LOP         IFEQ      0
183400940131     C                   SETON                                          5099
183500940131     C                   END
183600940131     C*
183700940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
183800940131     C     D2ORD         IFEQ      0
183900940131     C                   SETON                                          5199
184000940131     C                   END
184100940314    >C* << elenco controlli altri dati default >>
184200940131     C*
184300940131     C                   ENDSR
184400940223     C/EJECT
184500940201     C************************************************************
184600940201     C* AGGIORNAMENTO VALORI ASSUNTI
184700940201     C************************************************************
184800940201     C     AGGMPE        BEGSR
184900940201     C*
185000940201     C                   CLEAR                   ANMPE000
185100940201     C                   MOVEL     XSCSOC        MPESOC
185200940201     C                   MOVEL     DSPGM         MPEPGM
185300940201     C                   MOVEL     KNMUS         MPEPRF
185400941013     C                   SELECT
185500941018     C     OPZ103        WHENEQ    '01'
185600941013     C                   MOVEL     'PX'          MPETIP
185700941013     C                   OTHER
185800941013     C                   MOVEL     'PV'          MPETIP
185900941013     C                   ENDSL
186000940201     C     K4MPE2        CHAIN     ANMPE02L                           21
186100940201     C                   MOVEL     DSMPE         MPEPAR
186200940201     C     *IN21         IFEQ      *ON
186300940201     C                   WRITE     ANMPE000
186400940201     C                   ELSE
186500940201     C                   UPDATE    ANMPE000
186600940201     C                   END
186700940201     C*
186800940201     C                   ENDSR
186900940223     C/EJECT
187000940131     C************************************************************
187100940131     C* OPERAZIONI INIZIALI
187200940131     C************************************************************
187300940131     C     *INZSR        BEGSR
187400940131     C*
187500940127     C* Reperimento parametri
187600940127     C*
187700940117     C     *ENTRY        PLIST
187800940117     C                   PARM                    KPJBA
187900940314     C* Reperimento autorizzazioni
188000940314     C*
188100940314    >C* << nel caso l'anagrafica non gestisse le autorizzazioni
188200940909    >C*    asteriscare la chiamata alla subroutine REPAUT >>
188300941005<<<<<C*                    EXSR REPAUT
188400940225     C*
188500940223     C* Reperimento opzioni
188600940127     C*
188700940128     C                   EXSR      REPOPZ
188800940506     C*
188900940506     C* Reperimento tasti di funzione
189000940506     C*
189100940506     C                   EXSR      REPKEY
189200941103     C*
189300941103     C* Reperimento azienda
189400941103     C*
189500941103     C                   MOVEL     'SOC001'      TIPXSC
189600941103     C                   EXSR      REPSOC
189700941103     C     RTNXSC        IFNE      '1'
189800941103     C                   MOVEL     XSOCDS        SOC001
189900941103     C                   ELSE
190000941103     C                   MOVE      *ON           $FINE
190100941103     C* fine programma
190200941103     C                   EXSR      ENDPGM
190300941103     C                   ENDIF
190400940127     C*
190500980722     C* Chiamata a routine per il reperimento delle opzioni e funzioni
190600980722     C* personalizzate per S1
190700980722     C                   CAllb     'X46USREXT'
190800980722     C                   PARM                    $OP1
190900980722     C                   PARM                    $TX1
191000980722     C                   PARM                    $FL1
191100980722     C                   PARM                    $PGO1
191200980722     C                   PARM      $dimOp        NumOpz
191300980722     C                   PARM                    $FX2
191400980722     C                   PARM                    $FC2
191500980722     C                   PARM                    $PGF2
191600980722     C                   PARM                    DsPgm
191700980722     C                   PARM                    xsccut
191800980722     C                   PARM                    xscgrp
191900980722     C*
192000980722     C                   Z-add     1             OPZLEN
192100980722     C     *blank        Lookup    $TX1(opzlen)                           21
192200980722     C                   if        *in21 = *on
192300980722     C                   sub       1             OPZLEN
192400980722     C                   else
192500980722     C                   Z-add     $DimOp        OPZLEN
192600980722     C                   endif
192700980722     C*
192800940117     C                   ENDSR
192900940225     C/EJECT
193000941103      ************************************************************
193100941103      * Reperimento dati società
193200941103      ************************************************************
193300941103     C     REPSOC        BEGSR
193400941103      *
193500961003     C                   CALLb     'XSOC'
193600941103     C                   PARM                    TIPXSC            6
193700941103     C                   PARM                    SOCXSC            3
193800941103     C                   PARM                    CDSXSC            9 0
193900941103     C                   PARM                    MODXSC            3
194000941103     C                   PARM      *BLANKS       RTNXSC            1
194100941103     C                   PARM                    XSOCDS
194200941103     C                   PARM                    KPJBA
194300941103      *
194400941103     C                   ENDSR
194500941103     C/EJECT
194600940225     C************************************************************
194700940225     C* REPERIMENTO AUTORIZZAZIONI UTENTE
194800940225     C************************************************************
194900941005     C*          REPAUT    BEGSR
195000940225     C*
195100941005     C*                    CLEARXAUT
195200940225     C*
195300941005     C*                    Z-ADD0         XCAGRP           GRUPPO
195400941005     C*                    MOVELKNMUS     XCAPRF           PROFILO
195500941005    >C*                    MOVEL'OBJALL'  XCAFNC           FUNZIONE
195600941005     C*                    MOVEL*ZERO     XCAVFU           VARIABILE
195700941005     C*                    MOVE 'CK'      XCATCK           TIPO CTRL
195800941005     C*
195900941005     C*                    CALL 'XCHKAUT'
196000941005     C*                    PARM           XAUT
196100940225     C* utente non abilitato
196200941005     C*          XCARTN    IFEQ 0
196300941005    >C*<                   MOVELXCADDS    AUTDS
196400941005     C*                    ELSE
196500941005    >C*<                   CLEARAUTDS
196600941005     C*                    END
196700940225     C*
196800941005     C*                    ENDSR
196900940223     C/EJECT
197000940223     C************************************************************
197100940223     C* INIZIALIZZAZIONE VARIABILI
197200940223     C************************************************************
197300940223     C     INZVAR        BEGSR
197400940223     C*
197500940729     C*
197600940729     C* Dati da PGM chiamante
197700940729     C*
197800941018    >C                   MOVEL     KPJBU         A103DS
197900941026     C*
198000941026     C* Reperimento valori default utente
198100941026     C*
198200941026     C                   EXSR      REPDFT
198300940506     C*
198400940506     C* Definizione delle opzioni utilizzabili a pgm
198500940506     C*
198600940506     C                   EXSR      DEFOPZ
198700940223     C* Variabili per gestione videate
198800940223     C*
198900940223     C                   MOVE      *OFF          $INZD1
199000940223     C                   MOVE      *OFF          $INZS1
199100941103     C                   MOVE      *OFF          $FINE             1
199200940223     C                   MOVE      *OFF          $INZD2
199300940223     C                   MOVE      *OFF          $EFILE
199400940223     C                   MOVE      *OFF          $ESCI
199500940223     C                   MOVE      *OFF          $RCDOK
199600940509     C                   MOVE      *HIVAL        $LASTV
199700940506     C                   Z-ADD     0             $ULKD1            3 0
199800940506     C                   Z-ADD     0             $ULKS1            3 0
199900940916     C                   Z-ADD     0             $ULKD2            3 0
200000940223     C*
200100940318     C                   MOVE      *ON           $INZD1
200200940318     C                   MOVE      *ON           $INZS1
200300940831     C*
200400940831     C* se pgm richiamato con chiave univoca impostata,
200500940831     C* forzo la emissione della lista e imposto l'ordinamento
200600941013     C*
200700941013     C*
200800941013     C*
200900941013     C*
201000940223     C                   SELECT
201100940223     C     $LOP          WHENEQ    1
201200940223     C                   MOVE      'D1'          $GEST
201300940223     C     $LOP          WHENEQ    2
201400940223     C                   MOVE      'S1'          $GEST
201500940223     C                   ENDSL
201600940223     C*
201700940223     C* Variabili appoggio
201800940223     C*
201900940223     C                   Z-ADD     SFLPAG        WPAG
202000940223     C*
202100940223     C* Valorizzazione campi univoci testate
202200940223     C*
202300940223     C                   CLEAR                   FMTT1
202400940223     C                   MOVEL     KNSIF         NOMSIF
202500941110     C                   MOVEL     XSCDSI        NOMDIT
202600941027     C                   MOVEL     DSPGM         NOMPGM
202700940223     C*
202800941011     C* Accendo indicatori per condizioni su campo posizionamento C1POS
202900941011     C                   SELECT
203000941011     C     $ORD          WHENEQ    1
203100941011     C                   SETON                                            01
203200941011     C     $ORD          WHENEQ    2
203300941011     C                   SETON                                            02
203400941011     C                   ENDSL
203500941011      *
203600941011      *
203700940223     C                   ENDSR
203800940223     C/EJECT
203900940127     C************************************************************
204000940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
204100940127     C************************************************************
204200940131     C     REPDFT        BEGSR
204300940127     C*
204400940201     C                   MOVEL     XSCSOC        MPESOC
204500940201     C                   MOVEL     DSPGM         MPEPGM
204600940131     C                   MOVEL     KNMUS         MPEPRF
204700941013     C                   SELECT
204800941018     C     OPZ103        WHENEQ    '01'
204900941013     C                   MOVEL     'PX'          MPETIP
205000941013     C                   OTHER
205100941013     C                   MOVEL     'PV'          MPETIP
205200941013     C                   ENDSL
205300940202     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
205400940131     C     *IN21         IFEQ      *OFF
205500940131     C                   MOVEL     MPEPAR        DSMPE
205600940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
205700940131     C* i suoi valori di default vengono decisi a pgm
205800940131     C                   ELSE
205900940131     C                   MOVEL     *BLANK        DSMPE
206000941024     C                   Z-ADD     2             $LOP
206100940131     C                   Z-ADD     1             $ORD
206200941011     C                   SETON                                        01
206300941005    >C* << elenco altri dati default >>
206400940131     C                   END
206500940131     C*
206600940131     C                   ENDSR
206700030225A0994C************************************************************
206800030225     C* REPERIMENTO AUTORIZZAZIONI numeratori
206900030225A0994C************************************************************
207000030225     C     REPAUT        BEGSR
207100030225     C*
207200030225     C                   CLEAR                   AUTDS
207300030225     C*
207400030225     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
207500030225     C                   Z-ADD     0             XCAGRP                         GRUPPO
207600030225     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
207700030225    >C                   MOVEL     'ANGNUM'      XCAFNC                         FUNZIONE
207800030225     c* cerco lunghezza effettiva serie
207900030225     c     ' '           checkr    S1Ser         length            1 0
208000030225     c                   eval      Zeri  = *all'0'
208100030225     c                   eval      xcavfu  = %subst(Zeri:1:15-length) +
208200030225     c                                       %trim(S1Ser)
208300030225    >C                   MOVE      'CC'          XCATCK                         TIPO CTRL
208400030225    >C                   MOVE      ' '           XCANAU                         RICH.XNOAUT
208500030225     C*
208600030225     C                   CALLB     'XCHKAUT'
208700030225     C                   PARM                    AUTDS
208800030225     C*
208900030225     C* errori/non abilitazione
209000030225     C* se non esiste, NON è errore
209100030225b1   C     XCARTN        IFne      0
209200030225     c                   else
209300030225    >C                   MOVEL     XCADDS        AngNumds
209400030225     c* non abilitato a manutenzione
209500030225     c                   if        s1opz  = 2    and
209600030225b2   c                             ManNUM = *off
209700030225     c                   seton                                        5499
209800030225e2   C                   endif
209900030225     c* non abilitato a cancellare
210000030225b2   c                   if        s1opz  = 4    and AnnNUM = *off
210100030225     c                   seton                                        5499
210200030225e2   C                   ENDIF
210300030225     c* non abilitato a visualizzare
210400030225b2   c                   if        s1opz  = 5    and visNUM = *off
210500030225     c                   seton                                        5499
210600030225e2   C                   ENDIF
210700030225     C*
210800030225e1   C                   ENDIF
210900030225     C*
211000030225A0994C                   ENDSR
211100940223     C/EJECT
211200940131     C************************************************************
211300940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
211400940131     C************************************************************
211500940225     C     DEFOPZ        BEGSR
211600940131     C*
211700940225     C* Questa routine deve valorizzare la schiera $FL1
211800940225     C* con S o N a seconda della utilizzabilità o meno
211900940225     C* dell'opzione correlata
212000940225     C*
212100940225     C* Inizialmente si considerano tutte le opzioni come valide
212200940317     C     1             DO        $DIMOP        X
212300940225     C                   MOVE      'S'           $FL1(X)
212400940225     C                   END
212500940715     C*
212600940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
212700940715     C* con S o N a seconda della utilizzabilità o meno
212800940715     C* del tasto funzione correlato
212900940715     C*
213000940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
213100940715     C* di compilazione
213200940715     C                   RESET                   $FC1
213300940715     C                   RESET                   $FR1
213400940715     C                   RESET                   $FC2
213500940715     C                   RESET                   $FR2
213600940919     C                   RESET                   $FC3
213700940919     C                   RESET                   $FR3
213800940225     C*
213900940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
214000940225     C* invalido alcune opzioni
214100940225     C                   SELECT
214200940225     C* Se pgm richiamato in ricerca disattivo :
214300941018    >C     OPZ103        WHENEQ    '01'
214400940307     C* F10=Immissione
214500940506     C                   Z-ADD     10            X
214600940720     C                   MOVE      '0'           $FC2(X)
214700940603     C* F3=Fine
214800940603     C                   Z-ADD     3             X
214900940720     C                   MOVE      '0'           $FC2(X)
215000940307     C* 2=Modifica
215100940307     C                   Z-ADD     1             X
215200940225     C     02            LOOKUP    $OP1(X)                                21
215300940225     C     *IN21         IFEQ      *ON
215400940225     C                   MOVE      'N'           $FL1(X)
215500940225     C                   ENDIF
215600940225     C* 3=Copia
215700940225     C                   Z-ADD     1             X
215800940225     C     03            LOOKUP    $OP1(X)                                21
215900940225     C     *IN21         IFEQ      *ON
216000940225     C                   MOVE      'N'           $FL1(X)
216100940225     C                   ENDIF
216200940225     C* 4=Annullamento
216300940225     C                   Z-ADD     1             X
216400940225     C     04            LOOKUP    $OP1(X)                                21
216500940225     C     *IN21         IFEQ      *ON
216600940225     C                   MOVE      'N'           $FL1(X)
216700940225     C                   ENDIF
216800940314    >C* << aggiungere nollo stesso modo le altre opzioni da
216900940314    >C*    disattivare (quelle specifiche per ogni pgm) >>
217000940225     C* Se pgm richiamato in interrogazione disattivo :
217100941018    >C     OPZ103        WHENEQ    '05'
217200940307     C* F10=Immissione
217300940615     C                   Z-ADD     10            X
217400940720     C                   MOVE      '0'           $FC2(X)
217500940225     C* 1=Selezione
217600940225     C                   Z-ADD     1             X
217700940225     C     01            LOOKUP    $OP1(X)                                21
217800940225     C     *IN21         IFEQ      *ON
217900940225     C                   MOVE      'N'           $FL1(X)
218000940225     C                   ENDIF
218100940225     C* 2=Modifica
218200940225     C                   Z-ADD     1             X
218300940225     C     02            LOOKUP    $OP1(X)                                21
218400940225     C     *IN21         IFEQ      *ON
218500940225     C                   MOVE      'N'           $FL1(X)
218600940225     C                   ENDIF
218700940225     C* 3=Copia
218800940225     C                   Z-ADD     1             X
218900940225     C     03            LOOKUP    $OP1(X)                                21
219000940225     C     *IN21         IFEQ      *ON
219100940225     C                   MOVE      'N'           $FL1(X)
219200940225     C                   ENDIF
219300941005     C* 4=Annullamento
219400940225     C                   Z-ADD     1             X
219500940225     C     04            LOOKUP    $OP1(X)                                21
219600940225     C     *IN21         IFEQ      *ON
219700940225     C                   MOVE      'N'           $FL1(X)
219800940225     C                   ENDIF
219900940225     C*
220000940225     C                   OTHER
220100940225     C* Se pgm non richiamato, disattivo subito :
220200940225     C* 1=Selezione
220300940225     C                   Z-ADD     1             X
220400940225     C     01            LOOKUP    $OP1(X)                                21
220500940225     C     *IN21         IFEQ      *ON
220600940225     C                   MOVE      'N'           $FL1(X)
220700940225     C                   ENDIF
220800940225     C* poi, disattivo a seconda delle autorizzazioni :
220900940314    >C*********  AUTXXX    IFEQ 'N'
221000940314    >C*********            Z-ADD1         X
221100940314    >C*********  'XX'      LOKUP$OP1,X                   21
221200940314    >C*********  *IN21     IFEQ *ON
221300940314    >C*********            MOVE 'N'       $FL1,X
221400940314    >C*********            ENDIF
221500940314    >C*********            ENDIF
221600940225     C*
221700940225     C                   ENDSL
221800940225     C*
221900940225     C                   ENDSR
222000940225     C/EJECT
222100940225     C************************************************************
222200940225     C* REPERIMENTO OPZIONI
222300940225     C************************************************************
222400940225     C     REPOPZ        BEGSR
222500940225     C*
222600940314     C     1             DO        $DIMOP        X
222700940128     C*
222800940223     C     $MS1(X)       IFNE      *BLANK
222900940128     C*
223000940223     C                   MOVEL     $MS1(X)       IDMSG
223100961003     C                   CALLb     'XRTVM'
223200940201     C                   PARM                    IDMSG            27
223300940201     C                   PARM                    TXTMSG          100
223400940201     C                   PARM                    ERRMSG            1
223500940128     C*
223600940128     C     ERRMSG        IFNE      *ON
223700940223     C                   MOVEL     TXTMSG        $TX1(X)
223800940128     C                   ELSE
223900940223     C                   MOVEL     *ALL'?'       $TX1(X)
224000940128     C                   END
224100940128     C*
224200940128     C                   END
224300940128     C*
224400940128     C                   END
224500940128     C*
224600940128     C                   ENDSR
224700940506     C/EJECT
224800940506     C************************************************************
224900940506     C* REPERIMENTO TASTI DI FUNZIONE
225000940506     C************************************************************
225100940506     C     REPKEY        BEGSR
225200940506     C                   MOVEA     $FM1          $FM
225300950906     C                   MOVEA     $FC1          $FC
225400940506     C                   EXSR      REPKKK
225500940506     C                   MOVEA     $FX           $FX1
225600940506     C*
225700940506     C                   MOVEA     $FM2          $FM
225800950906     C                   MOVEA     $FC2          $FC
225900940506     C                   EXSR      REPKKK
226000940506     C                   MOVEA     $FX           $FX2
226100940919     C*
226200940919     C                   MOVEA     $FM3          $FM
226300950906     C                   MOVEA     $FC3          $FC
226400940919     C                   EXSR      REPKKK
226500940919     C                   MOVEA     $FX           $FX3
226600940919     C                   ENDSR
226700940506     C/EJECT
226800940506     C************************************************************
226900940506     C* REPERIMENTO TASTI DI FUNZIONE
227000940506     C************************************************************
227100940506     C     REPKKK        BEGSR
227200940506     C*
227300940506     C                   CLEAR                   $FX
227400940506     C     1             DO        24            X
227500940506     C*
227600940506     C     $FM(X)        IFNE      *BLANK
227700950906     C     $FC(X)        ANDNE     '0'
227800940506     C*
227900940506     C                   MOVEL     $FM(X)        IDMSG
228000961003     C                   CALLb     'XRTVM'
228100940506     C                   PARM                    IDMSG            27
228200940506     C                   PARM                    TXTMSG          100
228300940506     C                   PARM                    ERRMSG            1
228400940506     C*
228500940506     C     ERRMSG        IFNE      *ON
228600940506     C                   MOVEL     TXTMSG        $FX(X)
228700940506     C                   ELSE
228800940506     C                   MOVEL     *ALL' '       $FX(X)
228900940506     C                   END
229000940506     C*
229100940506     C                   END
229200940506     C*
229300940506     C                   END
229400940506     C*
229500940506     C                   ENDSR
229600940223     C/EJECT
229700940128     C************************************************************
229800940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
229900940128     C************************************************************
230000940223     C     DEFCAM        BEGSR
230100940128     C*
230200940223     C* klist
230300981229E0045C*
230400981229  "  C* ANNMR01L
230500981229  "  C     K6NMR1        KLIST
230600981229  "  C                   KFLD                    NMRsoc
230700981229  "  C                   KFLD                    NMRuni
230800981229  "  C                   KFLD                    NMRctb
230900981229  "  C                   KFLD                    NMRkey
231000981229  "  C                   KFLD                    NMRser
231100981229  "  C                   KFLD                    NMRtmp
231200981229E0045C*
231300941024     C*
231400941028     C* ANNMR02L
231500981229E0045C***  K2NMR1        KLIST
231600981229E0045C     K2NMR2        KLIST
231700941024     C                   KFLD                    NMRGRU
231800941024     C                   KFLD                    NMRSER
231900940223     C*
232000941005     C* ANMPE02L
232100941005     C     K4MPE2        KLIST
232200941005     C                   KFLD                    MPESOC
232300941005     C                   KFLD                    MPEPGM
232400941005     C                   KFLD                    MPETIP
232500941005     C                   KFLD                    MPEPRF
232600940128     C*
232700940223     C*
232800940223     C* Variabili per gestione videate
232900940223     C*
233000940223     C                   MOVE      *BLANK        $GEST             2
233100940223     C                   MOVE      *BLANK        $INZD1            1
233200940223     C                   MOVE      *BLANK        $INZS1            1
233300940223     C                   MOVE      *BLANK        $INZD2            1
233400940223     C                   MOVE      *BLANK        $LASTG            2
233500940223     C                   MOVE      *BLANK        $LASTV            2
233600940223     C                   MOVE      *BLANK        $EFILE            1
233700940223     C                   MOVE      *BLANK        $ESCI             1
233800940223     C                   MOVE      *BLANK        $RCDOK            1
233900940224     C                   Z-ADD     0             $PIED1            3 0
234000940224     C                   Z-ADD     0             $PIES1            3 0
234100940223     C*
234200940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
234300940223     C*
234400940223     C     *LIKE         DEFINE    C1RCD         S1NRR
234500940223     C     *LIKE         DEFINE    C1NRR         WSFL
234600940223     C     *LIKE         DEFINE    C1RCD         WMAX
234700940223     C     *LIKE         DEFINE    C1RCD         WPAG
234800940223     C     *LIKE         DEFINE    $INZS1        WINZS1
234900960416     C                   MOVE      *off          WINKR             1
235000940223     C                   Z-ADD     0             CURR              2 0
235100940223     C                   Z-ADD     0             CURC              2 0
235200940223     C                   MOVE      *BLANK        WIN              99
235300940314     C     *LIKE         DEFINE    C1POS         W1POS
235400940314     C*
235500940314     C* Variabili appoggio specifiche della singola anagrafica
235600940314     C*
235700940223     C* campi per parzializzazioni su rcd (solo alfanumerici)
235800940223     C*
235900940223     C* Indici
236000940223     C*
236100940223     C                   Z-ADD     0             X                 3 0
236200940223     C                   Z-ADD     0             Y                 3 0
236300940127     C*
236400940117     C                   ENDSR
236500941010     C/EJECT
236600941010    > ************************************************************
236700941010    > * ACCESSO A TABELLE ANTAB00F
236800941010    > ************************************************************
236900941010    >C     LETTAB        BEGSR
237000941010    > *
237100941010    > * Input :
237200941010    > * - TABGRU/TABCOD = chiave univoca ANTAB00F
237300941010    > * Output :
237400941010    > * - WTBERR = flag segnalazione errore reperimento tabella
237500941010    > *
237600961003    >C                   CALLb     'XATB'
237700941010    >C                   PARM                    XATBDS
237800941010    > *
237900941010    >C                   MOVE      *OFF          WTBERR
238000941010    >C     XTBERR        IFEQ      *ON
238100941010    >C                   MOVE      *BLANK        XTBUNI
238200941010    >C                   MOVE      *ON           WTBERR            1
238300941010    >C                   ENDIF
238400941010    > *
238500941010    >C                   ENDSR
238600941010     C/EJECT
238700950906**
238800940718PROMSG    *LIBL     OPZ0001
238900940225PROMSG    *LIBL     OPZ0002
239000940131PROMSG    *LIBL     OPZ0003
239100940131PROMSG    *LIBL     OPZ0004
239200940131PROMSG    *LIBL     OPZ0005
239300940201**
23940094022501S      <Selezione
23950094022502S      <Modifica
23960094022503S      <Copia
23970094022504S      <Annullamento
23980094022505S      <Visualizzazione
239900940506** TASTI DI FUNZIONE  D1
240000940506PROMSG    *LIBL     KEY0001  01
240100940506PROMSG    *LIBL     KEY0002  02
240200940506PROMSG    *LIBL     KEY0003  03
240300940506PROMSG    *LIBL     KEY0004  04
240400940506PROMSG    *LIBL     KEY0005  05
240500940506PROMSG    *LIBL     KEY0006  06
240600940506PROMSG    *LIBL     KEY0007  07
240700940506PROMSG    *LIBL     KEY0008  08
240800940506PROMSG    *LIBL     KEY0009  09
240900940506PROMSG    *LIBL     KEY0010  10
241000940506PROMSG    *LIBL     KEY0011  11
241100940506PROMSG    *LIBL     KEY0012  12
241200940506PROMSG    *LIBL     KEY0013  13
241300940506PROMSG    *LIBL     KEY0014  14
241400940506PROMSG    *LIBL     KEY0015  15
241500940506PROMSG    *LIBL     KEY0016  16
241600940506PROMSG    *LIBL     KEY0017  17
241700940506PROMSG    *LIBL     KEY0018  18
241800940506PROMSG    *LIBL     KEY0019  19
241900940506PROMSG    *LIBL     KEY0020  20
242000940506PROMSG    *LIBL     KEY0021  21
242100940506PROMSG    *LIBL     KEY0022  22
242200940506PROMSG    *LIBL     KEY0023  23
242300940506PROMSG    *LIBL     KEY0024  24
242400940506**  ABILITAZIONE D1
2425009407200N  01
2426009604161N  02
2427009604161N  03
2428009604161N  04
2429009604161N  05
2430009407200N  06
2431009407200N  07
2432009407200N  08
2433009407200N  09
2434009407200N  10
2435009407200N  11
2436009604161N  12
2437009407200N  13
2438009407200N  14
2439009407200N  15
2440009407200N  16
2441009410180N  17
2442009604161N  18
2443009407200N  19
2444009407200N  20
2445009407200N  21
2446009407200N  22
2447009509060N  23   Impostare a '1' se si utilizzano opzioni
2448009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
244900940916** TASTI DI FUNZIONE  S1
245000940916PROMSG    *LIBL     KEY0001  01
245100940916PROMSG    *LIBL     KEY0002  02
245200940916PROMSG    *LIBL     KEY0003  03
245300940916PROMSG    *LIBL     KEY0004  04
245400940916PROMSG    *LIBL     KEY0005  05
245500940916PROMSG    *LIBL     KEY0006  06
245600940916PROMSG    *LIBL     KEY0007  07
245700940916PROMSG    *LIBL     KEY0008  08
245800940916PROMSG    *LIBL     KEY0009  09
245900940916PROMSG    *LIBL     KEY0010  10
246000940916PROMSG    *LIBL     KEY0011  11
246100940916PROMSG    *LIBL     KEY0012  12
246200940916PROMSG    *LIBL     KEY0013  13
246300940916PROMSG    *LIBL     KEY0014  14
246400940916PROMSG    *LIBL     KEY0015  15
246500940916PROMSG    *LIBL     KEY0016  16
246600940916PROMSG    *LIBL     KEY0017  17
246700940916PROMSG    *LIBL     KEY0018  18
246800940916PROMSG    *LIBL     KEY0019  19
246900940916PROMSG    *LIBL     KEY0020  20
247000940916PROMSG    *LIBL     KEY0021  21
247100940916PROMSG    *LIBL     KEY0022  22
247200940916PROMSG    *LIBL     KEY0023  23
247300940916PROMSG    *LIBL     KEY0024  24
247400940916**  ABILITAZIONE S1
2475009409160N  01
2476009409160N  02
2477009409161N  03
2478009409160N  04
2479009409160N  05
2480009409160N  06
2481009409160N  07
2482009409160N  08
2483009409160N  09
2484009409161N  10
2485009409160N  11
2486009409161N  12
2487009409160N  13
2488009409160N  14
2489009409160N  15
2490009409160N  16
2491009604161N  17
2492009604161N  18
2493009409160N  19
2494009409160N  20
2495009409160N  21
2496009409160N  22
2497009509061N  23   Impostare a '1' se si utilizzano opzioni
2498009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
249900941018** TASTI DI FUNZIONE  D3
250000941018PROMSG    *LIBL     KEY0001  01
250100941018PROMSG    *LIBL     KEY0002  02
250200941018PROMSG    *LIBL     KEY0003  03
250300941018PROMSG    *LIBL     KEY0004  04
250400941018PROMSG    *LIBL     KEY0005  05
250500941018PROMSG    *LIBL     KEY0006  06
250600941018PROMSG    *LIBL     KEY0007  07
250700941018PROMSG    *LIBL     KEY0008  08
250800941018PROMSG    *LIBL     KEY0009  09
250900941018PROMSG    *LIBL     KEY0010  10
251000941018PROMSG    *LIBL     KEY0011  11
251100941018PROMSG    *LIBL     KEY0012  12
251200941018PROMSG    *LIBL     KEY0013  13
251300941018PROMSG    *LIBL     KEY0014  14
251400941018PROMSG    *LIBL     KEY0015  15
251500941018PROMSG    *LIBL     KEY0016  16
251600941018PROMSG    *LIBL     KEY0017  17
251700941018PROMSG    *LIBL     KEY0018  18
251800941018PROMSG    *LIBL     KEY0019  19
251900941018PROMSG    *LIBL     KEY0020  20
252000941018PROMSG    *LIBL     KEY0021  21
252100941018PROMSG    *LIBL     KEY0022  22
252200941018PROMSG    *LIBL     KEY0023  23
252300941018PROMSG    *LIBL     KEY0024  24
252400941018**  ABILITAZIONE D3
2525009410180N  01
2526009410180N  02
2527009410181N  03
2528009410180N  04
2529009410180N  05
2530009410180N  06
2531009410180N  07
2532009410180N  08
2533009410180N  09
2534009410180N  10
2535009410180N  11
2536009410181N  12
2537009410180N  13
2538009410180N  14
2539009410180N  15
2540009410180N  16
2541009410180N  17
2542009410180N  18
2543009410180N  19
2544009410180N  20
2545009410180N  21
2546009410180N  22
2547009509060N  23   Impostare a '1' se si utilizzano opzioni
2548009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
