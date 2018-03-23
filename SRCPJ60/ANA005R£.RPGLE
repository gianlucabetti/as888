000100990803 1212H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200940211     H DECEDIT('0,') DATEDIT(*DMY.)
000300940224      *----------*
000400941214    > * ANA005R  *
000500940224      *----------*
000600941010    > *               ABILITAZIONE UNITA'
000700940224      *          ------------------------------
000800940224      *
000900940224      *
001000941013      * DOCUMENTI MICROANALISI :  NSDAMUS/ANALISI/PROF 001.DOC
001100940224      *
001200940224      * NOTE TECNICHE :
001300940224      *
001400940224      *
001500940224      * PERSONALIZZAZIONI
001600940224      *
001700990812      *----------------------------------------------------------------
001800990812      * C1212 Trasformato il pgm da RPG a RPGLE.
001900990812      *       Aggiunto l'abilitazione ai tipi registrazione
002000990812      *       e alla riconciliazione partite con altre unità,
002100990812      *       quindi gestiti anche nel sfl
002200990812      *----------------------------------------------------------------
002300940224      *
002400940224      *
002500940314      *               MAPPA INDICATORI
002600940224      *
002700940307      *  21           GENERICO OPERAZIONI I/O
002800940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002900940224      *  30           SFLDSP
003000940224      * N31           SFLCLR
003100940224      *  31           SFLDSPCTL
003200940224      *  32           SFLNXTCHG
003300940224      *  33           SFLEND
003400940224      *  39           OF PRTF
003500940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003600940317    > *  Specificare l'uso dei singoli indicatori
003700940224      *  50 <---> 98  ERRORI SU VIDEO
003800940909    > *  Specificare l'uso dei singoli indicatori
003900940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
004000940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
004100940224      *  99           INDIC. GENERALE DI ERRORE
004200940128     F*----------------------------------------------------*
004300940314    >F*----------------------------------------------------------------
004400940314    >F* Viste logiche utilizzate per i diversi ordinamenti
004500940314    >F*----------------------------------------------------------------
004600940909     F*------------
004700941011<<<<>F**ANA99LIF  E           K        DISK
004800941011     F********    xxANA000                          KRENAMExxANA99
004900940909     F*------------
005000941010     FANABU01L  IF   E           K DISK
005100941010     F                                     RENAME(ANABU000:ANABU01)
005200940211     F*------------
005300941010    >FANABU02L  IF   E           K DISK
005400941010     F                                     RENAME(ANABU000:ANABU02)
005500940211     F*------------
005600940201     FANMPE02L  UF A E           K DISK
005700940211     F*------------
005800941010    >FANA005D   CF   E             WORKSTN
005900941010     F                                     RENAME(A005D1:FMTD1)
006000941010     F                                     RENAME(A005D2:FMTD2)
006100941010     F                                     RENAME(A005D3:FMTD3)
006200941010     F                                     RENAME(A005S1:FMTS1)
006300941010     F                                     RENAME(A005C1:FMTC1)
006400941010     F                                     RENAME(A005T1:FMTT1)
006500941010     F                                     RENAME(A005Z1:FMTZ1)
006600941010     F                                     RENAME(A005Z2:FMTZ2)
006700941010     F                                     RENAME(A005Z3:FMTZ3)
006800940131     F                                     SFILE(FMTS1:S1NRR)
006900940201     F                                     INFDS(DSFMT)
007000940128     D*----------------------------------------------------*
007100940302     D* Schiere per la gestione delle opzioni
007200940302     D*
007300990804C1212 *
007400990804  "  D*$MS1            S             27    DIM(5) CTDATA PERRCD(1)
007500990804  "  D*$TX1            S             50    DIM(5) ALT($MS1)
007600990804  "  D*$OP1            S              2  0 DIM(5) CTDATA PERRCD(1)
007700990804  "  D*$FL1            S              1    DIM(5) ALT($OP1)
007800990804  "   * dimensione della schiera $MS1, da non cambiare
007900990804  "  D $DIMOP          C                   CONST(30)
008000990804  "  D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
008100990804  "  D $TX1            S             50    DIM($dimop) ALT($MS1)
008200990804  "  D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
008300990804  "  D $FL1            S              1    DIM($dimop) ALT($OP1)
008400990804C1212 *
008500950403     D $TX             S             50    DIM(50)
008600950403     D $FL             S              1    DIM(50)
008700940909     D* Schiere generali per la gestione dei tasti di funzione
008800940506     D $FM             S             27    DIM(24)
008900940506     D $FX             S             50    DIM(24)
009000940506     D $FC             S              1    DIM(24)
009100940506     D $FR             S              1    DIM(24)
009200940909     D* Schiere per la gestione dei tasti di funzione
009300940506     D* Video D1
009400940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
009500940506     D $FX1            S             50    DIM(24) ALT($FM1)
009600940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
009700940506     D $FR1            S              1    DIM(24) ALT($FC1)
009800940506     D* Schiere per la gestione dei tasti di funzione
009900940506     D* Video S1
010000940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
010100940506     D $FX2            S             50    DIM(24) ALT($FM2)
010200940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
010300940506     D $FR2            S              1    DIM(24) ALT($FC2)
010400940916     D* Schiere per la gestione dei tasti di funzione
010500940916     D* Video D2
010600940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
010700940916     D $FX3            S             50    DIM(24) ALT($FM3)
010800940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
010900940916     D $FR3            S              1    DIM(24) ALT($FC3)
011000940302     D*
011100940211     D/EJECT
011200940128     D*----------------------------------------------------*
011300940211     D*-------------
011400940207     D* Valori assunti
011500941115     D DSMPE           DS                  INZ
011600940207     D  $LOP                   1      1  0
011700940831     D  $ORD                   2      3  0
011800940909     D*-------------
011900940831    >D*Definire campi chiave per la vista logica 99L
012000940831    >D* DS per chiave aggancio
012100940831    >D DSKEY           DS
012200940831    >D*                                       1   90PRGKEY
012300940831    >D*                                      10  140RIGKEY
012400940207     D*-------------
012500940211     D* Passaggio Parametri
012600940211     D KPJBA         E DS
012700940225     D*-------------
012800940225     D* Reperimento Autorizzazioni
012900941011     D**XAUT      E DSXCHKAUTDS
013000940325     D*-------------
013100940325     D* Parametri in ricezione
013200941010    >D A005DS        E DS                  EXTNAME(ANA005DS)
013300940325     D*-------------
013400940722     D* Parametri del PGM di manutenzione STD002R
013500941010    >D A006DS        E DS                  EXTNAME(ANA006DS) INZ
013600941010    >D  RET006       E                     INZ('0')
013700941010    >D  OPR006       E                     INZ('0')
013800941010    >D  ERR006       E                     INZ('0')
013900941013     D* Passaggio Parametri al PGM ANA001R
014000941013    >D A001DS        E DS                  EXTNAME(ANA001DS)
014100941013    >D  RET001       E                     INZ('0')
014200941013    >D  OPR001       E                     INZ('0')
014300941013    >D  ERR001       E                     INZ('0')
014400941111     D* Passaggio Parametri PGM ANA007R
014500941111     D A007DS        E DS                  EXTNAME(ANA007DS)
014600941111    >D  RET007       E                     INZ('0')
014700941111    >D  OPR007       E                     INZ('0')
014800941111    >D  ERR007       E                     INZ('0')
014900941111     D* Passaggio Parametri al PGM ANA009R
015000941111    >D A009DS        E DS                  EXTNAME(ANA009DS)
015100941111    >D  RET009       E                     INZ('0')
015200941111    >D  OPR009       E                     INZ('0')
015300941111    >D  ERR009       E                     INZ('0')
015400940211     D*-------------
015500940211     D DSFMT           DS           512
015600940506     D  $TASTO               369    369
015700940211     D  NRG                  370    370
015800940211     D  NCL                  371    371
015900940211     D  SFLNRR               378    379B 0
016000940211     D*-------------
016100940211     D* posizione cursore
016200940211     D CURSOR          DS
016300940211     D  RIRI                   1      2B 0 INZ
016400940211     D  R$$                    2      2
016500940211     D  COCO                   3      4B 0 INZ
016600940211     D  C$$                    4      4
016700940127     D*-------------
016800940127     D* Reperimento nome PGM
016900940127     D STATUS         SDS           333
017000940127     D  DSPGM            *PROC
017100940207     D*-------------
017200940616     D* Costanti
017300940616     D* nr.di righe del sfl (SFLPAG)
017400940314     D SFLPAG          C                   CONST(10)
017500990804     D* dimensione della schiera $MS1
017600990804C1212D*$DIMOP          C                   CONST(1)
017700940616     D*-------------
017800940506     D* Tasti di funzione
017900940506     D F01             C                   CONST(X'31')
018000940506     D F02             C                   CONST(X'32')
018100940506     D F03             C                   CONST(X'33')
018200940506     D F04             C                   CONST(X'34')
018300940506     D F05             C                   CONST(X'35')
018400940506     D F06             C                   CONST(X'36')
018500940506     D F07             C                   CONST(X'37')
018600940506     D F08             C                   CONST(X'38')
018700940506     D F09             C                   CONST(X'39')
018800940506     D F10             C                   CONST(X'3A')
018900940506     D F11             C                   CONST(X'3B')
019000940506     D F12             C                   CONST(X'3C')
019100940506     D F13             C                   CONST(X'B1')
019200940506     D F14             C                   CONST(X'B2')
019300940506     D F15             C                   CONST(X'B3')
019400940506     D F16             C                   CONST(X'B4')
019500940506     D F17             C                   CONST(X'B5')
019600940506     D F18             C                   CONST(X'B6')
019700940506     D F19             C                   CONST(X'B7')
019800940506     D F20             C                   CONST(X'B8')
019900940506     D F21             C                   CONST(X'B9')
020000940506     D F22             C                   CONST(X'BA')
020100940506     D F23             C                   CONST(X'BB')
020200940506     D F24             C                   CONST(X'BC')
020300940506     D ENTER           C                   CONST(X'F1')
020400940506     D ROLDWN          C                   CONST(X'F4')
020500940506     D ROLLUP          C                   CONST(X'F5')
020600940207     D*-------------
020700941011     D* Parametri per ricerca codice ANATB00F
020800941011     D XTABDS          DS                  INZ
020900941011     D* '  ' = Gestione
021000941011     D* '01' = Ricerca
021100941011     D* '02' = Gestione
021200941011     D* '05' = Visualizzazione
021300941011     D* '11' = Abilitazione
021400941011     D  OPZTAB                 1      2
021500941011     D* 0 = ritorno normale
021600941011     D* 1 = F3
021700941011     D* 2 = F12
021800941011     D  RETTAB                 3      3
021900941011     D* operazione eseguite dal pgm chiamato :
022000941011     D* 0 = nessuna operazione
022100941011     D* 1 = eseguito aggiornamento
022200941011     D  OPRTAB                 4      4
022300941011     D* funzione non eseguibile per errore :
022400941011     D* 0 = nessun errore
022500941011     D* 1 = funzione richiamata chiusa in errore
022600941011     D* Aggiungere eventuali altri codici di errore
022700941011     D  ERRTAB                 5      5
022800941011     D* Azienda
022900941011     D  AZITAB                 6      8
023000941011     D* Lingua
023100941011     D  LINTAB                 9      9
023200941011     D* Codice scelto
023300941011     D  KEYTAB                10     24
023400941011     D* riga cursore chiamante
023500941011     D  RIGTAB                25     26  0
023600941011     D* colonna cursore chiamante
023700941011     D  COLTAB                27     28  0
023800941011     D ERRORE          C                   CONST('Codice errato')
023900990803C1212D*DATA            C                   CONST('0001-01-01')
024000941011      * Passaggio Parametri al PGM XATB
024100941011     D XATBDS        E DS                  INZ
024200941103      *
024300941103      * Dati di ambiente ottenuti da XSOC
024400941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
024500941103      * DS Interna per dati di output di XSOC
024600941103     D XSOCDS          DS          1000
024700940127     C*----------------------------------------------------*
024800940127     C*                MAIN LINE PROGRAM
024900940127     C*----------------------------------------------------*
025000940127     C*
025100940223     C* inizializzazione variabili
025200940223     C                   EXSR      INZVAR
025300940223     C*
025400940223     C     $FINE         DOWEQ     *OFF
025500940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
025600940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
025700941011     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
025800940117     C                   END
025900940117     C                   END
026000940325     C* fine programma
026100940325     C                   EXSR      ENDPGM
026200940225     C*
026300940325     C/EJECT
026400940325     C************************************************************
026500940325     C* FINE PROGRAMMA
026600940325     C************************************************************
026700940325     C     ENDPGM        BEGSR
026800940722     C*
026900940722     C* aggiorno valori assunti
027000940722     C                   EXSR      AGGMPE
027100941011     C   KC              MOVE      '1'           RET005
027200941011     C   KL              MOVE      '2'           RET005
027300940325     C*
027400940325     C* passaggio dati al pgm chiamante
027500941010    >C                   MOVEL     A005DS        KPJBU
027600940325     C*
027700940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
027800941011    >C     OPZ005        IFEQ      *BLANK
027900940325     C                   SETON                                            LR
028000940325     C* altrimenti chiudo in RT
028100940325     C                   ELSE
028200940325     C                   SETON                                            RT
028300940325     C                   ENDIF
028400940616     C                   RETURN
028500940325     C*
028600940325     C                   ENDSR
028700940224     C/EJECT
028800940201     C************************************************************
028900940201     C* RICERCHE
029000940201     C************************************************************
029100940127     C     SEARCH        BEGSR
029200940127     C*
029300940127     C* determino Riga/Colonna del cursore
029400940127     C                   MOVE      NRG           R$$
029500940127     C                   MOVE      NCL           C$$
029600940127     C                   Z-ADD     RIRI          CURR
029700940127     C                   Z-ADD     COCO          CURC
029800940127     C*
0299009401271    C                   SELECT
030000940127     C*
030100940314    >C*--------------------------------------------------------------
030200940314    >C* inserire le routine di ricerca
030300940314    >C*--------------------------------------------------------------
030400940207     C* FMTD1  - MODULO
030500941010    >C     H1NMFL        WHENEQ    'D1GRP'
030600941010     C                   MOVE      *ZERO         WWGRP
030700941010     C                   MOVE      WWGRP         KEYTAB
030800941010     C                   MOVE      '01'          OPZTAB
030900941103     C                   MOVE      XSCSOC        AZITAB
031000941103     C                   MOVE      XSCLIN        LINTAB
031100941010     C                   CLEAR                   KPJBU
031200941010     C                   MOVEL     XTABDS        KPJBU
031300941010     C                   CALL      'ANGG22R'
031400941010     C                   PARM                    KPJBA
031500941010     C                   MOVEL     KPJBU         XTABDS
031600941010     C                   MOVE      KEYTAB        WWGRP
031700941010     C*
0318009410102   >C     WWGRP         IFNE      *ZEROS
031900941010    >C                   MOVEL     WWGRP         D1GRP
0320009410102-  >C                   ENDIF
032100940127     C*
032200940127     C* imposto pos. del cursore
032300940127     C                   Z-ADD     CURR          H1RIGA
032400940127     C                   Z-ADD     CURC          H1COLO
032500941013    >C     H1NMFL        WHENEQ    'D1PRF'
032600941013    >C                   RESET                   A001DS
032700941013    >C                   MOVEL     '01'          OPZ001
032800941013    >C                   MOVEL     A001DS        KPJBU
032900941013    >C                   CALL      'ANA001R'
033000941013     C                   PARM                    KPJBA
033100941013    >C                   MOVEL     KPJBU         A001DS
0332009410132   >C     PRF001        IFNE      *BLANK
033300941013    >C                   MOVE      PRF001        D1PRF
0334009410132-  >C                   ENDIF
033500950112     C* imposto pos. del cursore
033600950112     C                   Z-ADD     CURR          H1RIGA
033700950112     C                   Z-ADD     CURC          H1COLO
033800941111      * Società
033900941111    >C     H1NMFL        WHENEQ    'D1SOC'
034000941111    >C                   RESET                   A007DS
034100941111    >C                   MOVEL     '01'          OPZ007
034200941111    >C                   MOVEL     A007DS        KPJBU
034300941111    >C                   CALL      'ANA007R'
034400941111     C                   PARM                    KPJBA
034500941111     C                   MOVEL     KPJBU         A007DS
034600941111     C     SOC007        IFNE      *BLANK
034700941111     C                   MOVEL     SOC007        D1SOC
034800941111     C                   ENDIF
034900941111      * imposto pos. del cursore
035000941111     C                   Z-ADD     CURR          H1RIGA
035100941111     C                   Z-ADD     CURC          H1COLO
035200941111      *
035300941111    >C     H1NMFL        WHENEQ    'D1UNI'
035400941111    >C                   RESET                   A009DS
035500941111    >C                   MOVEL     '01'          OPZ009
035600941111    >C                   MOVEL     D1SOC         SOC009
035700941111    >C                   MOVEL     A009DS        KPJBU
035800941111    >C                   CALL      'ANA009R'
035900941111     C                   PARM                    KPJBA
036000941111    >C                   MOVEL     KPJBU         A009DS
0361009411112   >C     UNI009        IFNE      *BLANK
036200941111    >C                   MOVE      UNI009        D1UNI
0363009411112-  >C                   ENDIF
036400941111      * imposto pos. del cursore
036500941111     C                   Z-ADD     CURR          H1RIGA
036600941111     C                   Z-ADD     CURC          H1COLO
036700941111      *
036800940223     C* ricerca non abilitata in questa posizione
0369009402231O   C                   OTHER
037000940929     C* imposto pos. del cursore
037100940929     C                   Z-ADD     CURR          H1RIGA
037200940929     C                   Z-ADD     CURC          H1COLO
037300940929     C                   SETON                                        9899
037400940127     C*
0375009401271-   C                   ENDSL
037600940127     C*
037700940506     C                   MOVE      *HIVAL        $LASTV
037800940127     C                   ENDSR
037900940131     C/EJECT
038000940224     C************************************************************
038100940131     C* RIEMPIMENTO OPZIONI
038200940224     C************************************************************
038300940131     C     RIEOPZ        BEGSR
038400940223     C*
038500940223     C* Input :
038600940223     C* - $TX = schiera con tutti i testi disponibili
038700940223     C* - $FL = schiere con i flag di validità associati ai testi
038800940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
038900940223     C*          opzione usato
039000940223     C* - WMXO = variabile contenente il numero massimo di testi
039100940223     C*          opzione usabili
039200940721     C* - $L01 = Lunghezza prima riga
039300940721     C* - $L02 = Lunghezza seconda riga
039400940223     C* Output :
039500940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
039600940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
039700950403     C* - FF23  = flag che indica se c'è bisogno del tasto F23
039800940223     C*
039900950403     C                   CALL      'X04OPMSG'
040000940223     C* input
040100940223     C                   PARM                    $TX
040200940223     C                   PARM                    $FL
040300940225     C                   PARM                    WCOZ              3 0
040400940223     C                   PARM                    WMXO              3 0
040500940223     C* output
040600940223     C                   PARM                    WOPZ1            76
040700940223     C                   PARM                    WOPZ2            76
040800950403     C                   PARM                    FF23              1
040900940721    >C* input
041000941010    >C                   PARM                    $L01              3 0
041100941010    >C                   PARM                    $L02              3 0
041200940223     C*
000000060224A1148C                   PARM                    $ALLOPT         960
000000060224A1148C*
041300940131     C                   ENDSR
041400940506     C/EJECT
041500940506     C************************************************************
041600940506     C* RIEMPIMENTO TASTI FUNZIONE
041700940506     C************************************************************
041800940506     C     RIEKEY        BEGSR
041900940506     C*
042000940506     C* Input :
042100940506     C* - $FX = schiera con tutti i testi disponibili
042200940506     C* - $FC = schiere con i flag di validità associati ai testi
042300940506     C* - $FR = schiere con i flag di REVERSE IMAGE
042400940506     C* - $ULK = ultimo testo caricato
042500940721     C* - $L01 = Lunghezza prima riga
042600940721     C* - $L02 = Lunghezza seconda riga
042700940506     C* Output :
042800940506     C* - $ULK
042900940609     C* - $KEY1= 1° riga di tasti funzione
043000940609     C* - $KEY2= 2° riga di tasti funzione
043100940506     C*
043200940506     C                   CALL      'XKEYMSG'
043300940506     C* input
043400940506     C                   PARM                    $FX
043500940506     C                   PARM                    $FC
043600940506     C                   PARM                    $FR
043700940506     C                   PARM                    $ULK              3 0
043800940506     C* output
043900940506     C                   PARM                    $KEY1            79
044000940506     C                   PARM                    $KEY2            79
044100940721    >C* input
044200941010    >C****************     PARM           $L01    30
044300941010    >C**************+      PARM           $L02    30
044400940506     C*
000000060224A1148C                   PARM      79            $L01              3 0
000000060224A1148C                   PARM      79            $L02              3 0
000000060224A1148C*
000000060224A1148C                   PARM                    $ALLFUNCT       480
000000060224A1148C*
044500940506     C                   ENDSR
044600940224     C/EJECT
044700940224     C************************************************************
044800940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
044900940224     C************************************************************
045000940224     C     GESPIE        BEGSR
045100940224     C*
045200940224     C                   SELECT
045300940224     C* Parzializzazioni
045400940224     C     $GEST         WHENEQ    'D1'
045500940506     C     $ULKD1        IFEQ      0
045600940506     C* riempimento opzioni
045700940506     C                   EXSR      F24D1
045800940506     C                   ELSE
045900940506     C                   WRITE     FMTZ1
046000940506     C                   ENDIF
046100940506     C* Lista
046200940506     C     $GEST         WHENEQ    'S1'
046300940506     C     $ULKS1        IFEQ      0
046400940506     C                   EXSR      F24S1
046500940506     C                   ELSE
046600940506     C                   WRITE     FMTZ2
046700940506     C                   ENDIF
046800941124     C* Modifica valori assunti
046900941124     C     $GEST         WHENEQ    'D2'
047000941124     C     $ULKD2        IFEQ      0
047100941124     C                   EXSR      F24D2
047200941124     C                   ELSE
047300941124     C                   WRITE     FMTZ3
047400941124     C                   ENDIF
047500940506     C                   ENDSL
047600940506     C*
047700940224     C                   ENDSR
047800940224     C/EJECT
047900940224     C************************************************************
048000940224     C* GESTIONE PARZIALIZZAZIONI
048100940224     C************************************************************
048200940224     C     GESD1         BEGSR
048300940224     C* inizializzazione videata
048400940223     C     $INZD1        IFEQ      *ON
048500940127     C                   EXSR      INZD1
048600940223     C                   MOVE      *OFF          $INZD1
048700940117     C                   END
048800940223     C* emissione piede videata se proveniente da altra
048900940223     C* salvataggio valore formato di provenienza
049000940202     C     $LASTV        IFNE      'D1'
049100940223     C* salvo stato F17 per sapere cosa fare in caso di F12
049200940223     C                   MOVE      *INKR         WINKR
049300940202     C                   WRITE     FMTT1
049400940224     C                   EXSR      GESPIE
049500940202     C                   MOVE      $LASTV        $LASTG
049600940202     C                   MOVE      'D1'          $LASTV
049700940707     C                   ENDIF
049800940707     C*
049900940707     C* se esistono errori sulla videata
050000940707     C* emetto la write del formato a indicatori spenti per vedere
050100940707     C* le eventuali decodifiche
050200940707     C     *IN99         IFEQ      *ON
050300940707     C                   MOVEA     *IN           WIN
050400940707     C                   MOVE      *ALL'0'       IN5098           49
050500940707     C                   MOVEA     IN5098        *IN(50)
050600940707     C                   WRITE     FMTD1
050700940707     C                   MOVEA     WIN           *IN
050800940707     C                   ENDIF
050900940131     C*
051000940131     C                   EXFMT     FMTD1
051100940506     C* controllo abilitazione tasti funzione
051200940506     C*
051300940506     C                   EXSR      CTRKD1
051400940506     C*
051500940506     C     *IN99         IFEQ      *OFF
051600940131     C*
051700940117     C                   SELECT
051800940916     C* F2=Decodifica
051900940916     C     $TASTO        WHENEQ    F02
052000940916     C                   EXSR      F02D1
052100940131     C* F3=Fine
052200940506     C     $TASTO        WHENEQ    F03
052300940307     C                   EXSR      F03D1
052400940131     C* F4=Ricerca
052500940506     C     $TASTO        WHENEQ    F04
052600940127     C                   EXSR      SEARCH
052700940916     C* F5=Refresh
052800940916     C     $TASTO        WHENEQ    F05
052900940916     C                   EXSR      F05D1
053000001024B0284C* F8=Successivo
053100001024  "  C     $TASTO        WHENEQ    F08
053200001024  "  C                   EXSR      F08D1
053300001024  "  C* F10=Immissione
053400001024  "  C     $TASTO        WHENEQ    F10
053500001024B0284C                   EXSR      F10D1
053600940131     C* F12=Ritorno
053700940506     C     $TASTO        WHENEQ    F12
053800940307     C                   EXSR      F12D1
053900940131     C* F18=Modifica valori assunti
054000940506     C     $TASTO        WHENEQ    F18
054100940307     C                   EXSR      F18D1
054200940224     C* F24=Altri tasti
054300940506     C     $TASTO        WHENEQ    F24
054400940309     C                   EXSR      F24D1
054500940131     C*
054600940117     C                   OTHER
054700940127     C                   EXSR      CTRD1
054800940127     C     *IN99         IFEQ      *OFF
054900940131     C* definisco l'ordinamento
055000940131     C                   SELECT
055100940314    >C* << nel caso di più viste logiche e quindi più campi di
055200940314    >C*    parzializzazione, qui vanno definiti i criteri per
055300940314    >C*    determinare quale VL va letta, a seconda di quali campi
055400940314    >C*    sono stati valorizzati >>
055500941010    >C     D1PRF         WHENNE    *BLANK
055600940131     C                   Z-ADD     1             $ORD
055700941011     C                   SETON                                        01
055800941011     C                   SETOFF                                       02
055900941010    >C     D1GRP         WHENNE    *ZERO
056000940131     C                   Z-ADD     2             $ORD
056100941011     C                   SETON                                        02
056200941011     C                   SETOFF                                       01
056300941011<<<<<C******     D1PRG     WHNE 0
056400941011     C********             Z-ADD99        $ORD
056500940131     C* nel caso non sia stato digitato nulla uso l'ordinamento
056600940131     C* dei valori assunti
056700940131     C                   ENDSL
056800940127     C                   MOVE      'S1'          $GEST
056900940223     C                   MOVE      *ON           $INZS1
057000940302     C                   MOVE      *BLANK        W1POS
057100941215     C                   MOVE      *ZERO         W1POS2
057200940211     C                   ENDIF
057300940224     C*
057400940117     C                   ENDSL
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
058500940831     C* Se pgm non richiamato
058600941011     C     OPZ005        IFEQ      *BLANK
058700941011    >C*********            Z-ADDUDATE     D1DRG
058800940831     C                   ELSE
058900940831     C* Se pgm richiamato
059000941013     C                   SELECT
059100941013    <C     GRP005        WHENNE    0
059200941013    <C     PRF005        ORNE      *BLANK
059300941013     C                   MOVEL     PRF005        D1PRF
059400941013     C                   MOVEL     GRP005        D1GRP
059500941013     C                   MOVEL     SOC005        D1SOC
059600941013     C                   ENDSL
059700940831     C                   END
059800940224     C*
059900940117     C                   ENDSR
060000940506     C/EJECT
060100940506     C************************************************************
060200940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
060300940609     C* INPUT
060400940509     C* $FC1 = Abilitazioni videata D!
060500940509     C* OUTPUT
060600940509     C* *IN97 *IN99 Errori
060700940506     C************************************************************
060800940506     C     CTRKD1        BEGSR
060900940506     C                   SETOFF                                       99
061000940506     C                   MOVEA     $FC1          $FC
061100940506     C                   EXSR      CTRKEY
061200940506     C     *IN99         IFEQ      *ON
061300940506     C                   SETON                                        97
061400940506     C                   ENDIF
061500940506     C                   ENDSR
061600940506     C/EJECT
061700940506     C************************************************************
061800940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
061900940506     C************************************************************
062000940506     C     CTRKS1        BEGSR
062100940506     C                   SETOFF                                       99
062200940506     C                   MOVEA     $FC2          $FC
062300940506     C                   EXSR      CTRKEY
062400940506     C     *IN99         IFEQ      *ON
062500940506     C                   SETON                                        97
062600940506     C                   ENDIF
062700940506     C                   ENDSR
062800940506     C/EJECT
062900940506     C************************************************************
063000940506     C* CONTROLLO ABILITAZIONE TASTI
063100940509     C* INPUT
063200940609     C* $TASTO =Tasto funzione premuto
063300940509     C* $FC = Tasti abilitati
063400940509     C* OUTPUT
063500940509     C* IN99 = ERrore
063600940506     C************************************************************
063700940506     C     CTRKEY        BEGSR
063800940506     C                   SELECT
063900940506     C     $TASTO        WHENEQ    F01
064000940720     C     $FC(1)        IFEQ      '0'
064100940506     C                   SETON                                        99
064200940506     C                   ENDIF
064300940506     C     $TASTO        WHENEQ    F02
064400940720     C     $FC(2)        IFEQ      '0'
064500940506     C                   SETON                                        99
064600940506     C                   ENDIF
064700940506     C     $TASTO        WHENEQ    F03
064800940720     C     $FC(3)        IFEQ      '0'
064900940506     C                   SETON                                        99
065000940506     C                   ENDIF
065100940506     C     $TASTO        WHENEQ    F04
065200940720     C     $FC(4)        IFEQ      '0'
065300940506     C                   SETON                                        99
065400940506     C                   ENDIF
065500940506     C     $TASTO        WHENEQ    F05
065600940720     C     $FC(5)        IFEQ      '0'
065700940506     C                   SETON                                        99
065800940506     C                   ENDIF
065900940506     C     $TASTO        WHENEQ    F06
066000940720     C     $FC(6)        IFEQ      '0'
066100940506     C                   SETON                                        99
066200940506     C                   ENDIF
066300940506     C     $TASTO        WHENEQ    F07
066400940720     C     $FC(7)        IFEQ      '0'
066500940506     C                   SETON                                        99
066600940506     C                   ENDIF
066700940506     C     $TASTO        WHENEQ    F08
066800940720     C     $FC(8)        IFEQ      '0'
066900940506     C                   SETON                                        99
067000940506     C                   ENDIF
067100940615     C     $TASTO        WHENEQ    F09
067200940720     C     $FC(09)       IFEQ      '0'
067300940506     C                   SETON                                        99
067400940506     C                   ENDIF
067500940506     C     $TASTO        WHENEQ    F10
067600940720     C     $FC(10)       IFEQ      '0'
067700940506     C                   SETON                                        99
067800940506     C                   ENDIF
067900940506     C     $TASTO        WHENEQ    F11
068000940720     C     $FC(11)       IFEQ      '0'
068100940506     C                   SETON                                        99
068200940506     C                   ENDIF
068300940506     C     $TASTO        WHENEQ    F12
068400940720     C     $FC(12)       IFEQ      '0'
068500940506     C                   SETON                                        99
068600940506     C                   ENDIF
068700940506     C     $TASTO        WHENEQ    F13
068800940720     C     $FC(13)       IFEQ      '0'
068900940506     C                   SETON                                        99
069000940506     C                   ENDIF
069100940506     C     $TASTO        WHENEQ    F14
069200940720     C     $FC(14)       IFEQ      '0'
069300940506     C                   SETON                                        99
069400940506     C                   ENDIF
069500940506     C     $TASTO        WHENEQ    F15
069600940720     C     $FC(15)       IFEQ      '0'
069700940506     C                   SETON                                        99
069800940506     C                   ENDIF
069900940506     C     $TASTO        WHENEQ    F16
070000940720     C     $FC(16)       IFEQ      '0'
070100940506     C                   SETON                                        99
070200940506     C                   ENDIF
070300940506     C     $TASTO        WHENEQ    F17
070400940720     C     $FC(17)       IFEQ      '0'
070500940506     C                   SETON                                        99
070600940506     C                   ENDIF
070700940506     C     $TASTO        WHENEQ    F18
070800940720     C     $FC(18)       IFEQ      '0'
070900940506     C                   SETON                                        99
071000940506     C                   ENDIF
071100940506     C     $TASTO        WHENEQ    F19
071200940720     C     $FC(19)       IFEQ      '0'
071300940506     C                   SETON                                        99
071400940506     C                   ENDIF
071500940506     C     $TASTO        WHENEQ    F20
071600940720     C     $FC(20)       IFEQ      '0'
071700940506     C                   SETON                                        99
071800940506     C                   ENDIF
071900940506     C     $TASTO        WHENEQ    F21
072000940720     C     $FC(21)       IFEQ      '0'
072100940506     C                   SETON                                        99
072200940506     C                   ENDIF
072300940506     C     $TASTO        WHENEQ    F22
072400940720     C     $FC(22)       IFEQ      '0'
072500940506     C                   SETON                                        99
072600940506     C                   ENDIF
072700940506     C     $TASTO        WHENEQ    F23
072800940720     C     $FC(23)       IFEQ      '0'
072900940506     C                   SETON                                        99
073000940506     C                   ENDIF
073100940506     C     $TASTO        WHENEQ    F24
073200940720     C     $FC(24)       IFEQ      '0'
073300940506     C                   SETON                                        99
073400940506     C                   ENDIF
073500940506     C                   ENDSL
073600940506     C                   ENDSR
073700940916     C/EJECT
073800940916     C************************************************************
073900940916     C* GESTIONE F02 VIDEO D1
074000940916     C************************************************************
074100940916     C     F02D1         BEGSR
074200940916     C*
074300940916     C* per decodificare la videata richiamo la routine dei ctrl
074400940916     C* e poi disattivo gli eventuali messaggi di errore
074500940916     C                   EXSR      CTRD1
074600940916     C*
074700940916     C                   MOVE      *ALL'0'       IN5098
074800940916     C                   MOVEA     IN5098        *IN(50)
074900940916     C*
075000940916     C                   ENDSR
075100940224     C/EJECT
075200940131     C************************************************************
075300940307     C* GESTIONE F03 VIDEO D1
075400940131     C************************************************************
075500940307     C     F03D1         BEGSR
075600940131     C*
075700940307     C                   MOVE      *ON           $FINE
075800940325     C* fine programma
075900940325     C                   EXSR      ENDPGM
076000940307     C*
076100940307     C                   ENDSR
076200940916     C/EJECT
076300940916     C************************************************************
076400940916     C* GESTIONE F05 VIDEO D1
076500940916     C************************************************************
076600940916     C     F05D1         BEGSR
076700940916     C*
076800940916     C                   MOVE      *ON           $INZD1
076900940916     C*
077000940916     C                   ENDSR
077100940307     C/EJECT
077200001024B0284C************************************************************
077300001024  "  C* GESTIONE F08 VIDEO D1
077400001024  "  C************************************************************
077500001024B0284C     F08D1         BEGSR
077600001024     C*
077700001024     C                   MOVE      *ON           $FINE
077800001024    >C                   MOVE      '0'           RET005
077900001024     C*
078000001024B0284C                   ENDSR
078100001024B0284C************************************************************
078200001024  "  C* GESTIONE F10 VIDEO D1
078300001024  "  C************************************************************
078400001024B0284C     F10D1         BEGSR
078500001024     C*
078600001024    >C                   RESET                   A006DS
078700001024    >C                   MOVEL     '01'          OPZ006
078800001024    >C                   MOVEL     A006DS        KPJBU
078900001024    >C                   CALL      'ANA006R'
079000001024     C                   PARM                    KPJBA
079100001024    >C                   MOVEL     KPJBU         A006DS
079200001024     C* ritorno da PGM gestione
079300001024    >C                   EXSR      RTN006
079400001024     C*
079500001024     C                   MOVE      *HIVAL        $LASTV
079600001024     C*
079700001024B0284C                   ENDSR
079800940307     C************************************************************
079900940307     C* GESTIONE F12 VIDEO D1
080000940307     C************************************************************
080100940307     C     F12D1         BEGSR
080200940307     C*
080300940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
080400940307     C*  per F12 torno alla lista
080500940307     C     $LOP          IFEQ      2
080600940307     C     WINKR         OREQ      *ON
080700940307     C                   MOVE      'S1'          $GEST
080800940307     C*  altrimenti, per F12 torno a menu
080900940307     C                   ELSE
081000940307     C                   MOVE      *ON           $FINE
081100940307     C                   END
081200940307     C*
081300940307     C                   ENDSR
081400940307     C/EJECT
081500940307     C************************************************************
081600940307     C* GESTIONE F18 VIDEO D1
081700940307     C************************************************************
081800940307     C     F18D1         BEGSR
081900940307     C*
082000940307     C                   MOVE      'D2'          $GEST
082100940307     C                   MOVEL     *ON           $INZD2
082200940307     C*
082300940307     C                   ENDSR
082400940307     C/EJECT
082500940307     C************************************************************
082600940307     C* GESTIONE F24 VIDEO D1
082700940609     C* input
082800940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
082900940509     C* $FX1 =Testi tasti funzione
083000940509     C* $FC1 = Abilitazioni
083100940509     C* $FR1 = Inversioni di fondo
083200940307     C************************************************************
083300940307     C     F24D1         BEGSR
083400940506     C*
083500940506     C                   MOVEA     $FX1          $FX
083600940506     C                   MOVEA     $FC1          $FC
083700940506     C                   MOVEA     $FR1          $FR
083800940506     C                   Z-ADD     $ULKD1        $ULK
083900940929     C                   Z-ADD     79            $L01
084000940929     C                   Z-ADD     79            $L02
084100940506     C                   EXSR      RIEKEY
084200940506     C                   Z-ADD     $ULK          $ULKD1            3 0
084300940720     C                   MOVEA     $FC           $FC1
084400940506     C                   MOVEL     $KEY1         Z1KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
084500940506     C                   MOVEL     $KEY2         Z1KE2
084600940506     C                   WRITE     FMTZ1
084700940506     C*
084800940307     C*
084900940307     C                   ENDSR
085000940307     C/EJECT
085100940307     C************************************************************
085200940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
085300940307     C************************************************************
085400940307     C     CTRD1         BEGSR
085500940307     C*
085600940127     C                   SETOFF                                       99
085700940127     C* non imposto pos. del cursore
085800940127     C                   Z-ADD     0             H1RIGA
085900940127     C                   Z-ADD     0             H1COLO
086000940131     C*
086100940314    >C* << elencare gli eventuali controlli sui campi di
086200940314    >C*    parzializzazione, ricordando che quasi sempre essi
086300940314    >C*    non ne avranno perchè determinano solo il range di
086400940314    >C*    scrittura della lista e quindi (nel caso di campi
086500940314    >C*    alfanumerici) saranno trattati come chiavi parziali >>
086600940131     C*
086700940117     C                   ENDSR
086800940224     C/EJECT
086900940127     C************************************************************
087000940131     C* GESTIONE LISTA
087100940127     C************************************************************
087200940127     C     GESS1         BEGSR
087300940223     C* inizializzazione videata
087400940223     C     $INZS1        IFEQ      *ON
087500940127     C                   EXSR      INZS1
087600940223     C                   MOVE      *OFF          $INZS1
087700940127     C                   ENDIF
087800940223     C* emissione piede videata se proveniente da altra
087900940223     C* salvataggio valore formato di provenienza
088000940202     C     $LASTV        IFNE      'S1'
088100940202     C                   WRITE     FMTT1
088200940224     C                   EXSR      GESPIE
088300940202     C                   MOVE      $LASTV        $LASTG
088400940202     C                   MOVE      'S1'          $LASTV
088500940202     C                   END
088600940223     C*
088700940223     C     WMAX          IFEQ      0
088800940211     C                   WRITE     FMTD3                                        NO RCD
088900940211     C                   ENDIF
089000940831     C*Serve per emissione finestra pgm invio ang727r (per open/close
089100940831     C*window)
089200940831     C                   MOVE      '1'           WIN727            1
089300940127     C*
089400940131     C                   EXFMT     FMTC1
089500940204     C     C1NRR         IFNE      0
089600940204     C                   Z-ADD     C1NRR         WSFL
089700940204     C                   ENDIF
089800940127     C                   Z-ADD     SFLNRR        C1RCD
089900940506     C*
090000940506     C* controllo abilitazione tasti funzione
090100940506     C*
090200940506     C                   EXSR      CTRKS1
090300940506     C     *IN99         IFEQ      *OFF
090400940506     C*
090500940127     C*
0906009401271    C                   SELECT
090700940127     C* F3=Fine
090800940506     C     $TASTO        WHENEQ    F03
090900940309     C                   EXSR      F03S1
091000001024B0284C* F8=Successivo
091100001024  "  C     $TASTO        WHENEQ    F08
091200001024B0284C                   EXSR      F08S1
091300940131     C* F10=Immissione
091400940506     C     $TASTO        WHENEQ    F10
091500940309     C                   EXSR      F10S1
091600940131     C* F12=Ritorno
091700940506     C     $TASTO        WHENEQ    F12
091800940309     C                   EXSR      F12S1
091900940131     C* F17=Parzializzazioni
092000940506     C     $TASTO        WHENEQ    F17
092100940309     C                   EXSR      F17S1
092200940131     C* F18=Modifica valori assunti
092300940506     C     $TASTO        WHENEQ    F18
092400940309     C                   EXSR      F18S1
092500940224     C* F23=Altre opzioni
092600940506     C     $TASTO        WHENEQ    F23
092700940309     C                   EXSR      F23S1
092800940224     C* F24=Altri tasti
092900940506     C     $TASTO        WHENEQ    F24
093000940309     C                   EXSR      F24S1
093100940127     C* ROLL-UP
093200940506     C     $TASTO        WHENEQ    ROLLUP
093300940127     C                   EXSR      ROLS1
093400940127     C*
0935009401271O   C                   OTHER
093600940127     C* CONTROLLO DATI
093700940131     C                   EXSR      CTRC1
093800940201     C     *IN99         IFEQ      *OFF
093900940131     C                   EXSR      CTRS1
094000940131     C                   END
0941009401271-   C                   ENDSL
094200940127     C*
094300940506     C                   ENDIF
094400940127     C                   ENDSR
094500940224     C/EJECT
094600940127     C************************************************************
094700940131     C* INIZIALIZZAZIONE LISTA
094800940127     C************************************************************
094900940127     C     INZS1         BEGSR
095000940302     C* pulizia SFL
095100940128     C                   SETOFF                                         3031
095200940131     C                   WRITE     FMTC1
095300940128     C                   SETON                                          31
095400940127     C*
095500940128     C* riempimento opzioni
095600940223     C                   MOVEA     $TX1          $TX
095700940223     C                   MOVEA     $FL1          $FL
095800940309     C                   Z-ADD     0             WCOZ
095900940225     C*  equivale al num.di elementi della schiera $TX1
096000940509     C                   Z-ADD     $DIMOP        WMXO
096100950403     C                   Z-ADD     76            $L01
096200950403     C                   Z-ADD     76            $L02
096300940128     C                   EXSR      RIEOPZ
096400950403     C                   Z-ADD     23            X
096500950403     C                   MOVE      FF23          $FC2(X)
096600940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
096700940608     C                   MOVEL     WOPZ2         C1OPZ2
096800940225     C                   Z-ADD     WCOZ          C1COZ
096900940128     C*
097000940128     C* CARICAMENTO SFL A PAGINE
097100940128     C*
097200940201     C                   Z-ADD     0             S1NRR
097300940128     C                   Z-ADD     0             WMAX
097400940224     C*
097500940224     C* Posizionamento su file pilota
097600940302     C                   CLEAR                   C1POS
097700941010    >C                   EXSR      SETABU
097800940302     C* ripristino i valori del posizionamento
097900940302     C                   MOVE      W1POS         C1POS
098000941215     C                   MOVE      W1POS2        C1POS2
098100940131     C*
098200940201     C* Lettura file pilota
098300941010    >C                   EXSR      REDABU
098400940131     C*
098500940131     C* Carico 1a pagina
098600940127     C                   EXSR      ROLS1
098700940201     C                   Z-ADD     SFLPAG        WPAG
098800940127     C*
098900940127     C                   ENDSR
099000940127     C/EJECT
099100940127     C************************************************************
099200940131     C* CARICAMENTO PAGINA LISTA
099300940127     C************************************************************
099400940127     C     ROLS1         BEGSR
099500940127     C*
099600940128     C                   SETOFF                                       32
099700940223     C                   Z-ADD     0             Y
099800940127     C                   Z-ADD     WMAX          S1NRR
099900940127     C*
100000940127     C* Leggo dal file anagrafico per caricare la lista
100100940127     C*
1002009401311    C     $EFILE        DOWEQ     *OFF
100300940201     C     Y             ANDLT     WPAG
100400940127     C*
100500940131     C                   EXSR      RIES1
100600940127     C*
100700940127     C                   ADD       1             S1NRR
100800940127     C                   ADD       1             Y
100900940127     C*
101000940131     C                   WRITE     FMTS1
101100940131     C*
101200941010    >C                   EXSR      REDABU
101300940128     C*
1014009401271-   C                   ENDDO
101500940127     C*
101600940223     C                   Z-ADD     S1NRR         WMAX                 30
101700940127     C*
101800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
101900940127     C*
102000940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
102100940127     C                   MVR                     RESTO             3 0
102200940131     C     PAGINE        MULT      SFLPAG        C1RCD
1023009401271    C     RESTO         IFGT      0
102400940127     C                   ADD       1             C1RCD
1025009401271E   C                   ELSE
102600940407     C                   SUB       SFLPAG        C1RCD
102700940407     C                   ADD       1             C1RCD
1028009401271-   C                   ENDIF
102900940128     C*
103000940127     C                   ENDSR
103100940224     C/EJECT
103200940224     C************************************************************
103300940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
103400940224     C************************************************************
103500941010    >C     SETABU        BEGSR
103600940224     C*
103700940224     C                   SELECT
103800940314    >C* Ord.1 = chiave ordinamento 1
103900940224     C     $ORD          WHENEQ    1
104000940314    >C*-----------------------------------------------------------
104100940314    >C* Gestione posizionamento
104200940314    >C* Se il campo W1POS è > *blank imposto il posizionamento con
104300940314    >C* W1POS altrimenti lo imposto con il valore di parametrizzazione
104400940314    >C*-----------------------------------------------------------
104500940302     C     W1POS         IFNE      *BLANK
104600941010    >C                   MOVEL     W1POS         ABUPRF
104700940302     C                   ELSE
104800941010    >C                   MOVEL     D1PRF         ABUPRF
104900940302     C                   END
105000941010    >C                   MOVEL     D1SOC         ABUSOC
105100941010    >C                   MOVEL     D1UNI         ABUUNI
105200941011    >C     K3ABU1        SETLL     ANABU01
105300940224     C* Ord.2 = Modulo
105400940224     C     $ORD          WHENEQ    2
105500941011     C     W1POS2        IFNE      *ZERO
105600941010    >C                   MOVEL     W1POS2        ABUGRP
105700940302     C                   ELSE
105800941010    >C                   MOVEL     D1GRP         ABUGRP
105900940302     C                   END
106000941010    >C                   MOVEL     D1PRF         ABUPRF
106100941010    >C                   MOVEL     D1SOC         ABUSOC
106200941010    >C                   MOVEL     D1UNI         ABUUNI
106300941010    >C     K4ABU2        SETLL     ANABU02
106400940831     C* Ord.99= Chiave univoca
106500941011<<<< C******     $ORD      WHEQ 99
106600941011    >C*****      W1POS     IFNE *BLANK
106700941011    >C*++++                MOVELW1POS     MOVPRG
106800941011    >C******               ELSE
106900941011    >C****+++              MOVELD1PRG     MOVPRG
107000941011    >C*******              END
107100941011    >C********   MOVPRG    SETLLxxANA99
107200940224     C                   ENDSL
107300940224     C*
107400940224     C                   ENDSR
107500940128     C/EJECT
107600940128     C************************************************************
107700940131     C* LETTURA RCD ARCHIVIO PILOTA
107800940128     C************************************************************
107900941010    >C     REDABU        BEGSR
108000940128     C*
108100940131     C                   MOVEL     *OFF          $EFILE
108200940131     C                   MOVEL     *OFF          $RCDOK
108300940131     C*
1084009401311    C     $EFILE        DOUEQ     *ON
108500940131     C     $RCDOK        OREQ      *ON
108600940131     C*
1087009401312    C                   SELECT
108800940131     C     $ORD          WHENEQ    1
108900941010    >C                   READ      ANABU01                                21
109000940131     C     $ORD          WHENEQ    2
109100941010    >C                   READ      ANABU02                                21
109200941011<<<< C********   $ORD      WHEQ 99
109300941011    >C*********            READ xxANA99                  21
1094009401312-   C                   ENDSL
109500940131     C*
109600940131     C                   MOVE      *IN21         $EFILE
109700940131     C*
1098009401312    C     $EFILE        IFEQ      *OFF
109900941010    >C                   EXSR      SELABU
1100009401312-   C                   ENDIF
110100940131     C*
1102009401311-   C                   ENDDO
110300940131     C*
110400940131     C                   MOVE      *IN21         *IN33                          SflEnd
110500940131     C*
110600940131     C                   ENDSR
110700940224     C/EJECT
110800940224     C************************************************************
110900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
111000940224     C************************************************************
111100940224     C     CLCPAG        BEGSR
111200940224     C* Input :
111300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
111400940224     C*          cursore
111500940224     C* - SFLPAG = numero rcd per pagina sfl
111600940224     C* Output :
111700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
111800940224     C*
111900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
112000940224     C                   MVR                     RESTO             3 0
112100940224     C     RESTO         IFGT      0
112200940224     C                   ADD       1             PAGINE
112300940224     C                   ENDIF
112400940224     C     PAGINE        MULT      SFLPAG        WPAG
112500940224     C*
112600940224     C                   ENDSR
112700940131     C/EJECT
112800940131     C************************************************************
112900940131     C* SELEZIONI SULLA RIGA LETTA
113000940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
113100940314     C* non rientra nei parametri di parzializzazione
113200940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
113300940314     C* superiore stabilito nei parametri
113400940314     C*
113500940131     C************************************************************
113600941010    >C     SELABU        BEGSR
113700940131     C*
113800940202     C                   MOVEL     *ON           $RCDOK
113900940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
113900940131A1211C                   MOVEL     *hival        ALL9             15
114000940314     C* creo limite massimo parzializzazioni
114100940609    >C* << per ogni valore alfanumerico creo il limite superiore
114200940314    >C*    per cui caricare, creando una variabile che contiene
114300940314    >C*    il valore digitato nel campo a video riempito di "9" >>
114400941010    >C     D1PRF         CAT       ALL9:0        PRF999
114500941214    >C     D1SOC         CAT       ALL9:0        SOC999
114600941214    >C     D1UNI         CAT       ALL9:0        UNI999
114700940314     C*
114800940314     C                   SELECT
114900940314     C*
115000940314     C* SELEZIONI RISPETTO AI VALORI RCD
115100940314     C*
115200940314    >C* << elaborare eventuali selezioni che non riguardano le
115300940316    >C*    parzializzazioni ma più propriamente i dati del rcd ;
115400940314    >C*    esempio : esclusione degli annullati. >>
115500940314     C*
115600940314     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
115700940314     C*
115800941010    >C* PROFILO non compreso nella scelta
115900941010    >C     ABUPRF        WHENLT    D1PRF
116000941010    >C     ABUPRF        ORGT      PRF999
116100941010    >C                   MOVEL     *OFF          $RCDOK
116200941010    >C     ABUPRF        IFGT      PRF999
116300941010    >C     $ORD          ANDEQ     1
116400941010    >C     *HIVAL        SETLL     ANABU01
116500941010    >C                   ENDIF
116600941010    >C* GRUPPO non compresa nella scelta
116700941010    >C     ABUGRP        WHENNE    D1GRP
116800941010    >C     D1GRP         ANDNE     *ZERO
116900941010    >C                   MOVEL     *OFF          $RCDOK
117000941214    >C*
117100941214    >C* SOCIETA' non compresa nella scelta
117200941214    >C     ABUSOC        WHENLT    D1SOC
117300941214    >C     ABUSOC        ORGT      SOC999
117400941214    >C                   MOVEL     *OFF          $RCDOK
117500940202     C*
117600941214    >C* SOCIETA' non compresa nella scelta
117700941214    >C     ABUUNI        WHENLT    D1UNI
117800941214    >C     ABUUNI        ORGT      UNI999
117900941214    >C                   MOVEL     *OFF          $RCDOK
118000940202     C                   ENDSL
118100940202     C*
118200940202     C                   ENDSR
118300940131     C/EJECT
118400940131     C************************************************************
118500940131     C* RIEMPIMENTO RIGA LISTA
118600940131     C************************************************************
118700940131     C     RIES1         BEGSR
118800940131     C*
118900940131     C                   CLEAR                   FMTS1
119000940314    >C* << riempio i campi del sfl, compresi gli hidden con i valori
119100940314    >C*    del rcd letto >>
119200940131     C*
119300941010    >C                   MOVEL     ABUPRF        S1PRF
119400941010    >C                   MOVEL     ABUGRP        S1GRP
119500941010    >C                   MOVEL     ABUSOC        S1SOC
119600941010    >C                   MOVEL     ABUUNI        S1UNI
119700940131     C*
119800990803C1212 *
119900990803  "  C                   Eval      H1Def       = ABUDef
120000990803  "  C                   Eval      H1Prov      = ABUProv
120100990803  "  C                   Eval      H1Gest      = ABUGest
120200990803  "  C                   Eval      H1Rico      = ABURico
120300990804  "   *
120400990804  "  C* imposto gli attributi del sfl
120500990804  "  C                   EXSR      ATRS1
120600990804C1212 *
120700990803     C*
120800940131     C                   ENDSR
120900940309     C/EJECT
121000990804C1212 *
121100990804  "  C************************************************************
121200990804  "  C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
121300990804  "  C************************************************************
121400990804  "  C     ATRS1         BEGSR
121500990804  "   *
121600990804  "   * Abilitazione a Registrazione definitiva
121700990804  "  C                   IF        H1Def       = *On
121800990804  "  C                   Seton                                        41
121900990804  "  C                   Else
122000990804  "  C                   Setoff                                       41
122100990804  "  C                   EndIf
122200990804  "   *
122300990804  "   * Abilitazione a Registrazione provvisoria
122400990804  "  C                   IF        H1Prov      = *On
122500990804  "  C                   Seton                                        42
122600990804  "  C                   Else
122700990804  "  C                   Setoff                                       42
122800990804  "  C                   EndIf
122900990804  "   *
123000990804  "   * Abilitazione a Registrazione gestionale
123100990804  "  C                   IF        H1Gest      = *On
123200990804  "  C                   Seton                                        43
123300990804  "  C                   Else
123400990804  "  C                   Setoff                                       43
123500990804  "  C                   EndIf
123600990804  "   *
123700990804  "   * Abilitazione a Riconciliazione con altre partite
123800990804  "  C                   IF        H1Rico      = *On
123900990804  "  C                   Seton                                        44
124000990804  "  C                   Else
124100990804  "  C                   Setoff                                       44
124200990804  "  C                   EndIf
124300990804  "   *
124400990804  "  C     XAtrs1        ENDSR
124500990804C1212 *
124600940309     C************************************************************
124700940309     C* GESTIONE F03 VIDEO S1
124800940309     C************************************************************
124900940309     C     F03S1         BEGSR
125000940309     C*
125100940309     C                   MOVE      *ON           $FINE
125200940325     C* fine programma
125300940325     C                   EXSR      ENDPGM
125400940309     C*
125500940309     C                   ENDSR
125600940309     C/EJECT
125700001024B0284C************************************************************
125800001024  "  C* GESTIONE F08 VIDEO S1
125900001024  "  C************************************************************
126000001024B0284C     F08S1         BEGSR
126100001024     C*
126200001024     C                   MOVE      *ON           $FINE
126300001024    >C                   MOVE      '0'           RET005
126400001024     C*
126500001024B0284C                   ENDSR
126600940309     C************************************************************
126700940309     C* GESTIONE F10 VIDEO S1
126800940609     C* AGGIUNTA RECORD
126900940309     C************************************************************
127000940309     C     F10S1         BEGSR
127100940309     C*
127200941010    >C                   RESET                   A006DS
127300941010    >C                   MOVEL     '01'          OPZ006
127400941010    >C                   MOVEL     A006DS        KPJBU
127500941010    >C                   CALL      'ANA006R'
127600940309     C                   PARM                    KPJBA
127700941010    >C                   MOVEL     KPJBU         A006DS
127800940309     C* ritorno da PGM gestione
127900941010    >C                   EXSR      RTN006
128000940309     C     WINZS1        IFEQ      *ON
128100940309     C                   MOVE      *ON           $INZS1
128200940309     C* se esistevano già righe sul sfl
128300940309     C* calcolo pagina a cui deve posizionarsi
128400940309     C     WSFL          IFGT      0
128500940309     C                   EXSR      CLCPAG
128600940309     C* altrimenti carico solo la 1a pagina
128700940309     C                   ELSE
128800940309     C                   Z-ADD     SFLPAG        WPAG
128900940309     C                   ENDIF
129000940309     C                   ENDIF
129100940309     C*
129200940627     C                   MOVE      *HIVAL        $LASTV
129300940627     C*
129400940309     C                   ENDSR
129500940309     C/EJECT
129600940309     C************************************************************
129700940309     C* GESTIONE F12 VIDEO S1
129800940309     C************************************************************
129900940309     C     F12S1         BEGSR
130000940309     C*
130100940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
130200940309     C     $LOP          IFEQ      1
130300940309     C                   MOVE      'D1'          $GEST
130400940309     C*  se la 1a videata è questa, per F12 torno a menu
130500940309     C                   ELSE
130600940309     C                   MOVE      *ON           $FINE
130700940309     C                   END
130800940309     C*
130900940309     C                   ENDSR
131000940309     C/EJECT
131100940309     C************************************************************
131200940309     C* GESTIONE F17 VIDEO S1
131300940309     C************************************************************
131400940309     C     F17S1         BEGSR
131500940309     C*
131600940309     C                   MOVE      'D1'          $GEST
131700940831     C* forza certi valori nel caso il pgm sia stato richiamato con
131800940831     C* chiave specifica
131900941011<<<<<C*+         PRG005    IFNE 0
132000941011    >C**                   Z-ADD0         PRG005
132100941011     C**                   END
132200940831     C     $ORD          IFEQ      99
132300940831     C                   Z-ADD     1             $ORD
132400941215     C                   SETON                                        01
132500941215     C                   SETOFF                                       02
132600941011    >C*************        Z-ADDUDATE     D1DRG
132700940831     C                   END
132800940309     C*
132900940309     C                   ENDSR
133000940309     C/EJECT
133100940309     C************************************************************
133200940309     C* GESTIONE F18 VIDEO S1
133300940309     C************************************************************
133400940309     C     F18S1         BEGSR
133500940309     C*
133600940309     C                   MOVE      'D2'          $GEST
133700940309     C                   MOVEL     *ON           $INZD2
133800940831     C     $ORD          IFEQ      99
133900940831     C                   Z-ADD     1             $ORD
134000940831     C                   END
134100940309     C*
134200940309     C                   ENDSR
134300940309     C/EJECT
134400940309     C************************************************************
134500940309     C* GESTIONE F23 VIDEO S1
134600940309     C************************************************************
134700940309     C     F23S1         BEGSR
134800940309     C*
134900950403     C                   Z-ADD     76            $L01
135000950403     C                   Z-ADD     76            $L02
135100940309     C                   EXSR      RIEOPZ
135200950403     C                   Z-ADD     23            X
135300950403     C                   MOVE      FF23          $FC2(X)
135400940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
135500940608     C                   MOVEL     WOPZ2         C1OPZ2
135600940309     C                   Z-ADD     WCOZ          C1COZ
135700940309     C*
135800940309     C                   ENDSR
135900940309     C/EJECT
136000940309     C************************************************************
136100940309     C* GESTIONE F24 VIDEO S1
136200940309     C************************************************************
136300940309     C     F24S1         BEGSR
136400940309     C*
136500940506     C*
136600940506     C                   MOVEA     $FX2          $FX
136700940506     C                   MOVEA     $FC2          $FC
136800940506     C                   MOVEA     $FR2          $FR
136900940506     C                   Z-ADD     $ULKS1        $ULK
137000940929     C                   Z-ADD     79            $L01
137100940929     C                   Z-ADD     79            $L02
137200940506     C                   EXSR      RIEKEY
137300940506     C                   Z-ADD     $ULK          $ULKS1            3 0
137400940720     C                   MOVEA     $FC           $FC2
137500940506     C                   MOVEL     $KEY1         Z2KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
137600940506     C                   MOVEL     $KEY2         Z2KE2
137700940506     C                   WRITE     FMTZ2
137800940506     C*
137900940309     C                   ENDSR
138000940128     C/EJECT
138100940128     C************************************************************
138200940131     C* CONTROLLO TESTATA LISTA
138300940128     C************************************************************
138400940131     C     CTRC1         BEGSR
138500940128     C*
138600940201     C                   MOVE      *OFF          *IN99
138700940131     C*
138800940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
138900940202     C* L'INIZIALIZZAZIONE LISTA
139000940202     C*
139100941215     C                   SELECT
139200941215     C     $ORD          WHENEQ    1
139300941215    >C     C1POS         IFNE      W1POS
139400941215     C                   EXSR      POSS1
139500941215     C                   END
139600941215     C     $ORD          WHENEQ    2
139700941215    >C     C1POS2        IFNE      W1POS2
139800941215     C                   EXSR      POSS1
139900941215     C                   END
140000941215      *
140100941215     C                   ENDSL
140200940202     C*
140300940202     C                   ENDSR
140400940202     C/EJECT
140500940202     C************************************************************
140600940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
140700940202     C************************************************************
140800940202     C     POSS1         BEGSR
140900940202     C*
141000940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
141100940202     C*
1412009402021    C     WMAX          IFGT      0
141300940202     C                   READC     FMTS1                                  21
1414009402022    C     *IN21         DOWEQ     *OFF
141500940202     C     *IN99         ANDEQ     *OFF
141600940202     C*
1417009402023    C     S1OPZ         IFNE      0
141800940202     C                   SETON                                        51  99
1419009402023-   C                   ENDIF
142000940202     C* se rilevata opzione attiva, attivo sflnxtchg
1421009402023    C     *IN99         IFEQ      *ON
142200940202     C                   MOVE      *ON           *IN32
142300990804C1212 *
142400990804  "  C* imposto gli attributi del sfl
142500990804  "  C                   EXSR      ATRS1
142600990804C1212 *
142700940202     C                   UPDATE    FMTS1
1428009402023E   C                   ELSE
142900940202     C                   READC     FMTS1                                  21
1430009402023-   C                   ENDIF
143100940202     C*
1432009402022-   C                   ENDDO
143300940202     C*
1434009402021-   C                   ENDIF
143500940202     C*
143600940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
143700940221     C* AL VALORE DI POSIZIONAMENTO :
143800940221     C* se questo è maggiore del valore del rispettivo campo di
143900940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
144000940221     C* con i valori di parzializzazione.
144100940221     C*
1442009402021    C     *IN99         IFEQ      *OFF
144300940721    >C                   MOVE      *BLANK        W1POS
144400941215    >C                   MOVE      *ZERO         W1POS2
144500940202     C*
1446009402022    C                   SELECT
144700940202     C*
144800940202     C     $ORD          WHENEQ    1
144900940202     C*
145000941010    >C     C1POS         IFGT      D1PRF
145100941010    >C                   MOVEL     C1POS         W1POS
145200940131     C                   END
145300940131     C*
145400940202     C                   MOVE      *ON           *IN99
145500940223     C                   MOVE      *ON           $INZS1
145600940831     C*
145700940831     C     $ORD          WHENEQ    99
145800940831     C*
145900940831     C**         C1POS     IFGT D1PRG
146000940831     C**                   MOVE C1POS     W1POS
146100940831     C**                   END
146200940831     C                   MOVE      *ON           *IN99
146300940831     C                   MOVE      *ON           $INZS1
146400940831     C*
146500940202     C*
146600940202     C     $ORD          WHENEQ    2
146700940202     C*
146800941010    >C     C1POS2        IFGT      D1GRP
146900941010    >C                   MOVE      C1POS2        W1POS2
147000940202     C                   END
147100940202     C*
147200940202     C                   MOVE      *ON           *IN99
147300940223     C                   MOVE      *ON           $INZS1
147400940202     C*
1475009402022-   C                   ENDSL
147600940202     C*
1477009402021-   C                   ENDIF
147800940202     C*
147900940131     C                   ENDSR
148000940131     C/EJECT
148100940131     C************************************************************
148200940131     C* CONTROLLO OPZIONI LISTA
148300940131     C************************************************************
148400940131     C     CTRS1         BEGSR
148500940131     C*
148600940202     C                   MOVEL     *OFF          $ESCI
148700940201     C                   SETOFF                                       99
148800940131     C                   Z-ADD     0             S1OPZ
148900940131     C*
149000940127     C* Leggo il sfl solo se ci sono rcd
1491009401311    C     WMAX          IFGT      0
149200940131     C                   READC     FMTS1                                  21
149300940127     C*
149400940131     C* esce se fine sfl o errore che richiede l'uscita
1495009401312    C     *IN21         DOWEQ     *OFF
149600940131     C     $ESCI         ANDEQ     *OFF
149700940201     C                   Z-ADD     S1NRR         C1RCD
149800940131     C* ctrl su riga
149900940131     C                   EXSR      RECS1
150000940131     C* gestione opzioni
1501009401313    C     S1OPZ         IFNE      0
150200940201     C     *IN99         ANDEQ     *OFF
150300940131     C                   EXSR      OPZS1
1504009401273-   C                   ENDIF
150500941125     C* se rilevato errore, attivo sflnxtchg
1506009402013    C     *IN99         IFEQ      *ON
150700940131     C                   MOVE      *ON           *IN32
1508009402233-   C                   ENDIF
150900990804C1212 *
151000990804  "   * imposto gli attributi del sfl
151100990804  "  C                   EXSR      ATRS1
151200990804C1212 *
151300940131     C                   Z-ADD     0             S1OPZ
151400940223     C*
151500940127     C                   UPDATE    FMTS1
151600941125     C                   MOVE      *OFF          *IN32
151700940223     C*
151800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1519009401313    C     $ESCI         IFEQ      *OFF
152000940131     C                   READC     FMTS1                                  21
152100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1522009402014    C     *IN21         IFEQ      *ON
152300940201     C                   MOVE      WINZS1        $INZS1
152400940204     C* calcolo pagina a cui deve posizionarsi
152500940728     C     WSFL          IFGT      0
152600940728     C                   EXSR      CLCPAG
152700940728     C                   ELSE
152800940728     C                   Z-ADD     SFLPAG        WPAG
152900940728     C                   ENDIF
1530009402014-   C                   ENDIF
1531009402013-   C                   ENDIF
153200940131     C*
1533009401272-   C                   ENDDO
153400940127     C*
1535009401311-   C                   ENDIF
153600940131     C*
153700940127     C                   ENDSR
153800940127     C/EJECT
153900940127     C************************************************************
154000940131     C* CONTROLLO CAMPI I/O RIGA LISTA
154100940127     C************************************************************
154200940131     C     RECS1         BEGSR
154300940131     C*
154400940201     C* reset indicatori DSPATR
154500940201     C                   MOVE      *ALL'0'       IN4049           10
154600940201     C                   MOVEA     IN4049        *IN(40)
154700940201     C*
154800940131     C* Controllo se Utente Autorizzato alla scelta
154900940202     C     S1OPZ         IFNE      0
155000940131     C                   Z-ADD     1             X
155100940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
155200940211     C     *IN21         IFEQ      *OFF
155300940223     C     $FL1(X)       OREQ      'N'
155400940131     C                   MOVE      *ON           $ESCI
155500940201     C                   SETON                                        504099
155600940131     C                   ENDIF
155700940202     C                   ENDIF
155800940131     C*
155900940131     C                   ENDSR
156000940224     C/EJECT
156100940131     C************************************************************
156200940131     C* GESTIONE OPZIONI LISTA
156300940131     C************************************************************
156400940131     C     OPZS1         BEGSR
156500940201     C*
1566009402251    C                   SELECT
156700940225     C*
156800940225     C* se opz=Selezione passo la chiave al pgm chiamante
156900940225     C     S1OPZ         WHENEQ    1
157000941011    >C* << passare nei campi appositi della DS A001DS il valore
157100940314    >C*    della chiave della VL univoca (se c'è) >>
157200941011    >C                   MOVE      S1OPZ         OPZ005
157300941011    >C                   MOVE      S1PRF         PRF005
157400941011    >C                   MOVE      S1GRP         GRP005
157500941011    >C                   MOVE      S1SOC         SOC005
157600941011    >C                   MOVE      S1UNI         UNI005
157700940225     C                   MOVE      *ON           $ESCI
157800940225     C                   MOVE      *ON           $FINE
157900940909     C*
158000940831     C                   MOVE      *HIVAL        $LASTV
158100940225     C*
158200940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1583009402251O   C                   OTHER
158400940225     C*
158500941010    >C                   RESET                   A006DS
158600941010    >C* << passare nei campi appositi della DS A006DS il valore
158700940314    >C*    della chiave della VL univoca (se c'è) >>
158800941010    >C                   MOVEL     S1OPZ         OPZ006
158900941010    >C                   MOVEL     S1PRF         PRF006
159000941010    >C                   MOVEL     S1GRP         GRP006
159100941010    >C                   MOVEL     S1SOC         SOC006
159200941010    >C                   MOVEL     S1UNI         UNI006
159300940314     C*
159400941010    >C                   MOVEL     A006DS        KPJBU
159500941010    >C                   CALL      'ANA006R'
159600940314     C                   PARM                    KPJBA
159700941010    >C                   MOVEL     KPJBU         A006DS
159800940201     C*
159900940223     C* ritorno da PGM gestione
160000941010    >C                   EXSR      RTN006
160100940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1602009402252    C     *IN99         IFEQ      *ON
160300940223     C                   SETON                                        40
1604009402252-   C                   ENDIF
160500940131     C*
160600940506     C                   MOVE      *HIVAL        $LASTV
1607009402251-   C                   ENDSL
160800940225     C*
160900940131     C                   ENDSR
161000940223     C/EJECT
161100940223     C************************************************************
161200940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
161300940223     C************************************************************
161400941010    >C     RTN006        BEGSR
161500940223     C*
161600940223     C* modo di ritorno
161700940223     C*
1618009402231    C                   SELECT
161900940314    >C* << questi modi di utilizzo dei valori di ritorno dal
162000940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
162100940314    >C*    proposte, normalmente sempre valide, ma modificabili
162200940314    >C*    per situazioni particolari >>
162300940223     C* 1 = F3
162400941010    >C     RET006        WHENEQ    '1'
162500940224     C                   MOVE      *ON           $ESCI                          sospese READC
162600940223     C                   MOVE      *ON           $FINE                          fine pgm
162700940223     C* 2 = F12
162800941010    >C     RET006        WHENEQ    '2'
162900940223     C                   MOVE      *ON           $ESCI                          sospese READC
163000940223     C*
1631009402231-   C                   ENDSL
163200940223     C*
163300940223     C* operazione eseguite dal pgm chiamato
163400940223     C*
1635009402231    C                   SELECT
163600940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
163700941010    >C     OPR006        WHENEQ    '1'
163800940223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
163900940223     C*
1640009402231-   C                   ENDSL
164100940223     C*
164200940223     C* funzione non eseguibile per errore :
164300940223     C*
1644009402231    C                   SELECT
164500940223     C* 1 = funzione richiamata chiusa in errore
164600940316    >C*  eventualmente gestire altri codici di errore
164700941010    >C     ERR006        WHENEQ    '1'
164800940223     C                   MOVE      *ON           $ESCI                          sospese READC
164900940223     C                   SETON                                        5299      emiss.SFLMSGID
165000940223     C*
1651009402231-   C                   ENDSL
165200940223     C*
165300940223     C                   ENDSR
165400941011     C/EJECT
165500941011     C************************************************************
165600941011     C* GESTIONE VALORI ASSUNTI
165700941011     C************************************************************
165800941011     C     GESD2         BEGSR
165900941215     C**
166000941011     C     $INZD2        IFEQ      *ON
166100941011     C                   EXSR      INZD2
166200941011     C                   MOVE      *OFF          $INZD2
166300941011     C                   END
166400941215     C**
166500941011     C     $LASTV        IFNE      'D2'
166600941011     C                   EXSR      GESPIE
166700941011     C                   MOVE      $LASTV        $LASTG
166800941011     C                   MOVE      'D2'          $LASTV
166900941011     C                   ENDIF
167000941215     C**
167100941011     C* se esistono errori sulla videata
167200941011     C* emetto la write del formato a indicatori spenti per vedere
167300941011     C* le eventuali decodifiche
167400941011     C     *IN99         IFEQ      *ON
167500941011     C                   MOVEA     *IN           WIN
167600941011     C                   MOVE      *ALL'0'       IN5098           49
167700941011     C                   MOVEA     IN5098        *IN(50)
167800941011     C                   WRITE     FMTD2
167900941011     C                   MOVEA     WIN           *IN
168000941011     C                   ENDIF
168100941215     C**
168200941011     C                   EXFMT     FMTD2
168300941215     C**
168400941011     C                   SELECT
168500941011     C* F3=Fine
168600941011     C     $TASTO        WHENEQ    F03
168700941011     C                   EXSR      F03D2
168800941011     C* F12=Ritorno
168900941011     C     $TASTO        WHENEQ    F12
169000941011     C                   EXSR      F12D2
169100941011     C* F24=Altri tasti
169200941011     C     $TASTO        WHENEQ    F24
169300941011     C                   EXSR      F24D2
169400941215     C**
169500941011     C                   OTHER
169600941011     C                   EXSR      CTRD2
1697009410112    C     *IN99         IFEQ      *OFF
169800941011     C                   MOVE      $LASTG        $GEST
1699009410113    C                   SELECT
170000941011     C     $LASTG        WHENEQ    'S1'
170100941215     C**se è stato modificato l'ordinamento di default, ricarico la lista
170200941011     C     $ORD          IFNE      D2ORD
170300941011     C                   MOVE      *ON           $INZS1
170400941011     C                   MOVE      *BLANK        W1POS
170500941215     C                   MOVE      *ZERO         W1POS2
170600941011     C                   END
170700941011     C     $LASTG        WHENEQ    'D1'
170800941215     C**per ora niente
1709009410113-   C                   ENDSL
171000941011     C* definisco i nuovi valori di default
171100941011     C                   Z-ADD     D2LOP         $LOP
171200941011     C                   Z-ADD     D2ORD         $ORD
171300941215     C                   SELECT
171400941215     C     $ORD          WHENEQ    1
171500941215     C                   SETON                                            01
171600941215     C                   SETOFF                                           02
171700941215     C     $ORD          WHENEQ    2
171800941215     C                   SETON                                            02
171900941215     C                   SETOFF                                           01
172000941215     C                   ENDSL
172100941011    >C* << elenco altri dati default >>
1722009410112-   C                   END
172300941215     C**
172400941011     C                   ENDSL
172500941215     C**
172600941011     C                   ENDSR
172700941011     C/EJECT
172800941011     C************************************************************
172900941011     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
173000941011     C************************************************************
173100941011     C     INZD2         BEGSR
173200941011     C*
173300941011     C                   CLEAR                   FMTD2
173400941011     C                   MOVEL     $LOP          D2LOP
173500941011     C                   MOVE      $ORD          D2ORD
173600941011    >C* << elenco altri dati default >>
173700941011     C*
173800941011     C                   ENDSR
173900941011     C/EJECT
174000941011     C************************************************************
174100941011     C* GESTIONE F03 VIDEO D2
174200941011     C************************************************************
174300941011     C     F03D2         BEGSR
174400941011     C*
174500941011     C                   MOVE      *ON           $FINE
174600941011     C* fine programma
174700941011     C                   EXSR      ENDPGM
174800941011     C*
174900941011     C                   ENDSR
175000941011     C/EJECT
175100941011     C************************************************************
175200941011     C* GESTIONE F12 VIDEO D2
175300941011     C************************************************************
175400941011     C     F12D2         BEGSR
175500941011     C*
175600941011     C                   MOVE      $LASTG        $GEST
175700941011     C*
175800941011     C                   ENDSR
175900941011     C/EJECT
176000941011     C************************************************************
176100941011     C* GESTIONE F24 VIDEO D2
176200941011     C************************************************************
176300941011     C     F24D2         BEGSR
176400941011     C*
176500941011     C                   MOVEA     $FX3          $FX
176600941011     C                   MOVEA     $FC3          $FC
176700941011     C                   MOVEA     $FR3          $FR
176800941011     C                   Z-ADD     $ULKD2        $ULK
176900941011     C                   Z-ADD     56            $L01
177000941011     C                   Z-ADD     56            $L02
177100941011     C                   EXSR      RIEKEY
177200941011     C                   Z-ADD     $ULK          $ULKD2            3 0
177300941011     C                   MOVEA     $FC           $FC3
177400941011     C                   MOVEL     $KEY1         Z3KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
177500941011     C                   MOVEL     $KEY2         Z3KE2
177600941011     C                   WRITE     FMTZ3
177700941011     C*
177800941011     C                   ENDSR
177900941011     C/EJECT
178000941011     C************************************************************
178100941011     C* CONTROLLO VIDEATA VALORI ASSUNTI
178200941011     C************************************************************
178300941011     C     CTRD2         BEGSR
178400941011     C*
178500941011     C                   SETOFF                                       99
178600941011     C* non imposto pos. del cursore
178700941011     C                   Z-ADD     0             H1RIGA
178800941011     C                   Z-ADD     0             H1COLO
178900941011     C*
179000941011     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
179100941011     C     D2LOP         IFEQ      0
179200941011     C                   SETON                                          5099
179300941011     C                   END
179400941011     C*
179500941011     C* OBBLIGATORIO VALORE PER ORDINAMENTO
179600941011     C     D2ORD         IFEQ      0
179700941011     C                   SETON                                          5199
179800941011     C                   END
179900941011    >C* << elenco controlli altri dati default >>
180000941011     C*
180100941011     C                   ENDSR
180200940223     C/EJECT
180300940201     C************************************************************
180400940201     C* AGGIORNAMENTO VALORI ASSUNTI
180500940201     C************************************************************
180600940201     C     AGGMPE        BEGSR
180700940201     C*
180800940201     C                   CLEAR                   ANMPE000
180900940201     C                   MOVEL     XSCSOC        MPESOC
181000940201     C                   MOVEL     DSPGM         MPEPGM
181100940201     C                   MOVEL     KNMUS         MPEPRF
181200941013     C                   SELECT
181300941013     C     OPZ005        WHENEQ    '01'
181400941013     C     OPZ005        OREQ      '11'
181500941013     C                   MOVEL     'PX'          MPETIP
181600941013     C                   OTHER
181700941013     C                   MOVEL     'PV'          MPETIP
181800941013     C                   ENDSL
181900940201     C     K4MPE2        CHAIN     ANMPE02L                           21
182000940201     C                   MOVEL     DSMPE         MPEPAR
182100940201     C     *IN21         IFEQ      *ON
182200940201     C                   WRITE     ANMPE000
182300940201     C                   ELSE
182400940201     C                   UPDATE    ANMPE000
182500940201     C                   END
182600940201     C*
182700940201     C                   ENDSR
182800940223     C/EJECT
182900940131     C************************************************************
183000940131     C* OPERAZIONI INIZIALI
183100940131     C************************************************************
183200940131     C     *INZSR        BEGSR
183300940131     C*
183400940127     C* Reperimento parametri
183500940127     C*
183600940117     C     *ENTRY        PLIST
183700940117     C                   PARM                    KPJBA
183800940314     C* Reperimento autorizzazioni
183900940314     C*
184000940314    >C* << nel caso l'anagrafica non gestisse le autorizzazioni
184100940909    >C*    asteriscare la chiamata alla subroutine REPAUT >>
184200941011    >C*********            EXSR REPAUT
184300940225     C*
184400940223     C* Reperimento opzioni
184500940127     C*
184600940128     C                   EXSR      REPOPZ
184700940506     C*
184800940506     C* Reperimento tasti di funzione
184900940506     C*
185000940506     C                   EXSR      REPKEY
185100940506     C*
185200940127     C*
185300940117     C                   ENDSR
185400940225     C/EJECT
185500941011     C************************************************************
185600941011     C* REPERIMENTO AUTORIZZAZIONI UTENTE
185700941011     C************************************************************
185800941011     C*          REPAUT    BEGSR
185900941011     C*
186000941011     C*                    CLEARXAUT
186100941011     C*
186200941011     C*                    Z-ADD0         XCAGRP           GRUPPO
186300941011     C*                    MOVELKNMUS     XCAPRF           PROFILO
186400941011    >C*                    MOVEL'OBJALL'  XCAFNC           FUNZIONE
186500941011     C*                    MOVEL*ZERO     XCAVFU           VARIABILE
186600941011     C*                    MOVE 'CK'      XCATCK           TIPO CTRL
186700941011     C*
186800941011     C*                    CALL 'XCHKAUT'
186900941011     C*                    PARM           XAUT
187000941011     C* utente non abilitato
187100941011     C*          XCARTN    IFEQ 0
187200941011    >C*                    MOVELXCADDS    AUTDS
187300941011     C*                    ELSE
187400941011    >C*                    CLEARAUTDS
187500941011     C*                    END
187600941011     C*
187700941011     C*                    ENDSR
187800940223     C/EJECT
187900940223     C************************************************************
188000940223     C* INIZIALIZZAZIONE VARIABILI
188100940223     C************************************************************
188200940223     C     INZVAR        BEGSR
188300941215     C*
188400941215     C* Dati da PGM chiamante
188500941215     C*
188600941215    >C                   MOVEL     KPJBU         A005DS
188700941215      *
188800941215      * Reperimento dati società
188900941215      *
189000941215     C                   MOVEL     'SOC001'      TIPXSC
189100941215     C                   MOVEL     *BLANK        SOCXSC
189200941215     C                   EXSR      REPSOC
189300941215     C     RTNXSC        IFNE      '1'
189400941215     C                   MOVEL     XSOCDS        SOC001
189500941215     C                   ELSE
189600941215     C                   MOVE      *ON           $FINE
189700941215     C                   EXSR      ENDPGM
189800941215     C                   ENDIF
189900941026     C*
190000941026     C* Reperimento valori default utente
190100941026     C*
190200941026     C                   EXSR      REPDFT
190300940506     C*
190400940506     C* Definizione delle opzioni utilizzabili a pgm
190500940506     C*
190600940506     C                   EXSR      DEFOPZ
190700940223     C* Variabili per gestione videate
190800940223     C*
190900940223     C                   MOVE      *OFF          $FINE                          fine pgm
191000940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
191100940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
191200940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
191300940223     C                   MOVE      *OFF          $EFILE                         memo fine file
191400940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
191500940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
191600940509     C                   MOVE      *HIVAL        $LASTV
191700940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
191800940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
191900940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
192000940223     C*
192100940318     C                   MOVE      *ON           $INZD1
192200940318     C                   MOVE      *ON           $INZS1
192300941011      *
192400941011     C                   SELECT
192500941011      * 1 RICHIAMATO PER ABILITAZIONE A LIVELLO DI GRUPPO
192600941011<<<< C     OPZ005        WHENEQ    '11'
192700941011     C     GRP005        ANDNE     0
192800941011     C                   Z-ADD     2             $ORD
192900941013     C                   MOVEL     PRF005        D1PRF
193000941013     C                   MOVEL     GRP005        D1GRP
193100941013     C                   MOVEL     SOC005        D1SOC
193200941011      *
193300941011      * 1 RICHIAMATO PER ABILITAZIONE A LIVELLO DI PROFILO
193400941011<<<< C     OPZ005        WHENEQ    '11'
193500941215     C     PRF005        ANDNE     *BLANK
193600941011     C                   Z-ADD     1             $ORD
193700941013     C                   MOVEL     PRF005        D1PRF
193800941013     C                   MOVEL     GRP005        D1GRP
193900941013     C                   MOVEL     SOC005        D1SOC
194000941011      *
194100941013     C     OPZ005        WHENNE    *BLANK
194200941011     C     PRF005        ANDNE     *BLANK
194300941011     C                   Z-ADD     2             $LOP
194400941011     C                   Z-ADD     1             $ORD
194500941011      *
194600941013     C     OPZ005        WHENNE    *BLANK
194700941215     C     GRP005        ANDNE     *ZERO
194800941011     C                   Z-ADD     2             $LOP
194900941011     C                   Z-ADD     2             $ORD
195000941011     C                   ENDSL
195100941011      *
195200940223     C                   SELECT
195300940223     C     $LOP          WHENEQ    1
195400940223     C                   MOVE      'D1'          $GEST
195500940223     C     $LOP          WHENEQ    2
195600940223     C                   MOVE      'S1'          $GEST
195700941104     C                   EXSR      INZD1
195800941104     C                   MOVE      *OFF          $INZD1
195900940223     C                   ENDSL
196000940223     C*
196100940223     C* Variabili appoggio
196200940223     C*
196300940223     C                   Z-ADD     SFLPAG        WPAG
196400940223     C*
196500940223     C* Valorizzazione campi univoci testate
196600940223     C*
196700940223     C                   CLEAR                   FMTT1
196800940223     C                   MOVEL     KNSIF         NOMSIF
196900941110     C                   MOVEL     XSCDSI        NOMDIT
197000941027     C                   MOVEL     DSPGM         NOMPGM
197100941104      *
197200941011     C                   SELECT
197300941011     C     $ORD          WHENEQ    1
197400941011     C                   SETON                                            01
197500941011     C     $ORD          WHENEQ    2
197600941011     C                   SETON                                            02
197700941011     C                   ENDSL
197800941011      *
197900940223     C                   ENDSR
198000940223     C/EJECT
198100940127     C************************************************************
198200940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
198300940127     C************************************************************
198400940131     C     REPDFT        BEGSR
198500940127     C*
198600940201     C                   MOVEL     XSCSOC        MPESOC
198700940201     C                   MOVEL     DSPGM         MPEPGM
198800940131     C                   MOVEL     KNMUS         MPEPRF
198900941013     C                   SELECT
199000941013     C     OPZ005        WHENEQ    '01'
199100941013     C     OPZ005        OREQ      '11'
199200941013     C                   MOVEL     'PX'          MPETIP
199300941013     C                   OTHER
199400941013     C                   MOVEL     'PV'          MPETIP
199500941013     C                   ENDSL
199600940202     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
199700940131     C     *IN21         IFEQ      *OFF
199800940131     C                   MOVEL     MPEPAR        DSMPE
199900940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
200000940131     C* i suoi valori di default vengono decisi a pgm
200100940131     C                   ELSE
200200940201     C                   Z-ADD     1             $LOP
200300940131     C                   Z-ADD     1             $ORD
200400941011     C                   SETON                                        01
200500941011    >C**<< elenco altri dati default >>
200600940131     C                   END
200700940131     C*
200800940131     C                   ENDSR
200900940223     C/EJECT
201000940131     C************************************************************
201100940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
201200940131     C************************************************************
201300940225     C     DEFOPZ        BEGSR
201400940131     C*
201500940225     C* Questa routine deve valorizzare la schiera $FL1
201600940225     C* con S o N a seconda della utilizzabilità o meno
201700940225     C* dell'opzione correlata
201800940225     C*
201900940225     C* Inizialmente si considerano tutte le opzioni come valide
202000940317     C     1             DO        $DIMOP        X
202100940225     C                   MOVE      'S'           $FL1(X)
202200940225     C                   END
202300940715     C*
202400940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
202500940715     C* con S o N a seconda della utilizzabilità o meno
202600940715     C* del tasto funzione correlato
202700940715     C*
202800940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
202900940715     C* di compilazione
203000940715     C                   RESET                   $FC1
203100940715     C                   RESET                   $FR1
203200940715     C                   RESET                   $FC2
203300940715     C                   RESET                   $FR2
203400940919     C                   RESET                   $FC3
203500940919     C                   RESET                   $FR3
203600940225     C*
203700940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
203800940225     C* invalido alcune opzioni
203900940225     C                   SELECT
204000940225     C* Se pgm richiamato in ricerca disattivo :
204100941010    >C     OPZ005        WHENEQ    '01'
204200940307     C* F10=Immissione
204300940506     C                   Z-ADD     10            X
204400001024B0284C                   MOVE      '0'           $FC1(X)
204500940720     C                   MOVE      '0'           $FC2(X)
204600940603     C* F3=Fine
204700940603     C                   Z-ADD     3             X
204800001024B0284C                   MOVE      '0'           $FC1(X)
204900940720     C                   MOVE      '0'           $FC2(X)
205000001024B0284C                   MOVE      '0'           $FC3(X)
205100940307     C* 2=Modifica
205200940307     C                   Z-ADD     1             X
205300940225     C     02            LOOKUP    $OP1(X)                                21
205400940225     C     *IN21         IFEQ      *ON
205500940225     C                   MOVE      'N'           $FL1(X)
205600940225     C                   ENDIF
205700940225     C* 3=Copia
205800940225     C                   Z-ADD     1             X
205900940225     C     03            LOOKUP    $OP1(X)                                21
206000940225     C     *IN21         IFEQ      *ON
206100940225     C                   MOVE      'N'           $FL1(X)
206200940225     C                   ENDIF
206300940225     C* 4=Annullamento
206400940225     C                   Z-ADD     1             X
206500940225     C     04            LOOKUP    $OP1(X)                                21
206600940225     C     *IN21         IFEQ      *ON
206700940225     C                   MOVE      'N'           $FL1(X)
206800940225     C                   ENDIF
206900940314    >C* << aggiungere nollo stesso modo le altre opzioni da
207000940314    >C*    disattivare (quelle specifiche per ogni pgm) >>
207100940225     C* Se pgm richiamato in interrogazione disattivo :
207200941010    >C     OPZ005        WHENEQ    '05'
207300940307     C* F10=Immissione
207400940615     C                   Z-ADD     10            X
207500001024B0284C                   MOVE      '0'           $FC1(X)
207600940720     C                   MOVE      '0'           $FC2(X)
207700940225     C* 1=Selezione
207800940225     C                   Z-ADD     1             X
207900940225     C     01            LOOKUP    $OP1(X)                                21
208000940225     C     *IN21         IFEQ      *ON
208100940225     C                   MOVE      'N'           $FL1(X)
208200940225     C                   ENDIF
208300940225     C* 2=Modifica
208400940225     C                   Z-ADD     1             X
208500940225     C     02            LOOKUP    $OP1(X)                                21
208600940225     C     *IN21         IFEQ      *ON
208700940225     C                   MOVE      'N'           $FL1(X)
208800940225     C                   ENDIF
208900940225     C* 3=Copia
209000940225     C                   Z-ADD     1             X
209100940225     C     03            LOOKUP    $OP1(X)                                21
209200940225     C     *IN21         IFEQ      *ON
209300940225     C                   MOVE      'N'           $FL1(X)
209400940225     C                   ENDIF
209500940225     C* 4=Annullamento
209600940225     C                   Z-ADD     1             X
209700940225     C     04            LOOKUP    $OP1(X)                                21
209800940225     C     *IN21         IFEQ      *ON
209900940225     C                   MOVE      'N'           $FL1(X)
210000940225     C                   ENDIF
210100940225     C*
210200941011    >C     OPZ005        WHENEQ    '11'
210300941011     C* 1=Selezione
210400941011     C                   Z-ADD     1             X
210500941011     C     01            LOOKUP    $OP1(X)                                21
210600941011     C     *IN21         IFEQ      *ON
210700941011     C                   MOVE      'N'           $FL1(X)
210800941011     C                   ENDIF
210900940225     C                   OTHER
211000940225     C* Se pgm non richiamato, disattivo subito :
211100001024B0284C* F8=Selezione seguente
211200001024  "  C                   Z-ADD     8             X
211300001024  "  C                   MOVE      '0'           $FC1(X)
211400001024B0284C                   MOVE      '0'           $FC2(X)
211500940225     C* 1=Selezione
211600940225     C                   Z-ADD     1             X
211700940225     C     01            LOOKUP    $OP1(X)                                21
211800940225     C     *IN21         IFEQ      *ON
211900940225     C                   MOVE      'N'           $FL1(X)
212000940225     C                   ENDIF
212100940225     C* poi, disattivo a seconda delle autorizzazioni :
212200940314    >C*********  AUTXXX    IFEQ 'N'
212300940314    >C*********            Z-ADD1         X
212400940314    >C*********  'XX'      LOKUP$OP1,X                   21
212500940314    >C*********  *IN21     IFEQ *ON
212600940314    >C*********            MOVE 'N'       $FL1,X
212700940314    >C*********            ENDIF
212800940314    >C*********            ENDIF
212900940225     C*
213000940225     C                   ENDSL
213100940225     C*
213200940225     C                   ENDSR
213300940225     C/EJECT
213400940225     C************************************************************
213500940225     C* REPERIMENTO OPZIONI
213600940225     C************************************************************
213700940225     C     REPOPZ        BEGSR
213800940225     C*
213900940314     C     1             DO        $DIMOP        X
214000940128     C*
214100940223     C     $MS1(X)       IFNE      *BLANK
214200940128     C*
214300940223     C                   MOVEL     $MS1(X)       IDMSG
214400940128     C                   CALL      'XRTVM'
214500940201     C                   PARM                    IDMSG            27
214600940201     C                   PARM                    TXTMSG          100
214700940201     C                   PARM                    ERRMSG            1
214800940128     C*
214900940128     C     ERRMSG        IFNE      *ON
215000940223     C                   MOVEL     TXTMSG        $TX1(X)
215100940128     C                   ELSE
215200940223     C                   MOVEL     *ALL'?'       $TX1(X)
215300940128     C                   END
215400940128     C*
215500940128     C                   END
215600940128     C*
215700940128     C                   END
215800940128     C*
215900940128     C                   ENDSR
216000940506     C/EJECT
216100940506     C************************************************************
216200940506     C* REPERIMENTO TASTI DI FUNZIONE
216300940506     C************************************************************
216400940506     C     REPKEY        BEGSR
216500940506     C                   MOVEA     $FM1          $FM
216600950906     C                   MOVEA     $FC1          $FM
216700940506     C                   EXSR      REPKKK
216800940506     C                   MOVEA     $FX           $FX1
216900940506     C*
217000940506     C                   MOVEA     $FM2          $FM
217100950906     C                   MOVEA     $FC2          $FM
217200940506     C                   EXSR      REPKKK
217300940506     C                   MOVEA     $FX           $FX2
217400940919     C*
217500940919     C                   MOVEA     $FM3          $FM
217600950906     C                   MOVEA     $FC3          $FM
217700940919     C                   EXSR      REPKKK
217800940919     C                   MOVEA     $FX           $FX3
217900940919     C                   ENDSR
218000940506     C/EJECT
218100940506     C************************************************************
218200940506     C* REPERIMENTO TASTI DI FUNZIONE
218300940506     C************************************************************
218400940506     C     REPKKK        BEGSR
218500940506     C*
218600940506     C                   CLEAR                   $FX
218700940506     C     1             DO        24            X
218800940506     C*
218900940506     C     $FM(X)        IFNE      *BLANK
219000950906     C     $FC(X)        ANDNE     '0'
219100940506     C*
219200940506     C                   MOVEL     $FM(X)        IDMSG
219300940506     C                   CALL      'XRTVM'
219400940506     C                   PARM                    IDMSG            27
219500940506     C                   PARM                    TXTMSG          100
219600940506     C                   PARM                    ERRMSG            1
219700940506     C*
219800940506     C     ERRMSG        IFNE      *ON
219900940506     C                   MOVEL     TXTMSG        $FX(X)
220000940506     C                   ELSE
220100940506     C                   MOVEL     *ALL' '       $FX(X)
220200940506     C                   END
220300940506     C*
220400940506     C                   END
220500940506     C*
220600940506     C                   END
220700940506     C*
220800940506     C                   ENDSR
220900940223     C/EJECT
221000940128     C************************************************************
221100940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
221200940128     C************************************************************
221300940223     C     DEFCAM        BEGSR
221400940128     C*
221500940223     C* klist
221600940223     C*
221700941010    >C* ANABU01L
221800941010    >C     K3ABU1        KLIST
221900941010    >C                   KFLD                    ABUPRF
222000941010    >C                   KFLD                    ABUSOC
222100941010    >C                   KFLD                    ABUUNI
222200940127     C*
222300941010    >C* ANABU02L
222400941010    >C     K4ABU2        KLIST
222500941011    >C                   KFLD                    ABUGRP
222600941010    >C                   KFLD                    ABUPRF
222700941010    >C                   KFLD                    ABUSOC
222800941010    >C                   KFLD                    ABUUNI
222900940128     C*
223000940128     C* ANMPE02L
223100940128     C     K4MPE2        KLIST
223200940128     C                   KFLD                    MPESOC
223300940201     C                   KFLD                    MPEPGM
223400940128     C                   KFLD                    MPETIP
223500940128     C                   KFLD                    MPEPRF
223600940223     C*
223700940223     C* Variabili per gestione videate
223800940223     C*
223900940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
224000940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
224100940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
224200940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
224300940223     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
224400940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
224500940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
224600940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
224700940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
224800940223     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
224900940224     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
225000940224     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
225100940223     C*
225200940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
225300940223     C*
225400940223     C     *LIKE         DEFINE    C1RCD         S1NRR
225500940223     C     *LIKE         DEFINE    C1NRR         WSFL
225600940223     C     *LIKE         DEFINE    C1RCD         WMAX
225700940223     C     *LIKE         DEFINE    C1RCD         WPAG
225800940223     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
225900940608     C     *LIKE         DEFINE    *INKR         WINKR                                      ne S1
226000940223     C                   Z-ADD     0             CURR              2 0
226100940223     C                   Z-ADD     0             CURC              2 0
226200940223     C                   MOVE      *BLANK        WIN              99
226300940314     C     *LIKE         DEFINE    C1POS         W1POS
226400941011     C     *LIKE         DEFINE    C1POS2        W1POS2
226500940314     C*
226600940314     C* Variabili appoggio specifiche della singola anagrafica
226700940314     C*
226800940314     C* campi per gestione F4=Ricerca
226900941011    >C     *LIKE         DEFINE    ABUGRP        WWGRP
227000940223     C* campi per parzializzazioni su rcd (solo alfanumerici)
227100941011    >C     *LIKE         DEFINE    ABUPRF        PRF999
227200941214    >C     *LIKE         DEFINE    ABUSOC        SOC999
227300941214    >C     *LIKE         DEFINE    ABUUNI        UNI999
227400940223     C*
227500940223     C* Indici
227600940223     C*
227700940223     C                   Z-ADD     0             X                 3 0
227800940223     C                   Z-ADD     0             Y                 3 0
227900940127     C*
228000940117     C                   ENDSR
228100941011     C/EJECT
228200941011    > ************************************************************
228300941011    > * ACCESSO A TABELLE ANTAB00F
228400941011    > ************************************************************
228500941011    >C     LETTAB        BEGSR
228600941011    > *
228700941011    > * Input :
228800941011    > * - TABGRU/TABCOD = chiave univoca ANTAB00F
228900941011    > * Output :
229000941011    > * - WTBERR = flag segnalazione errore reperimento tabella
229100941011    > *
229200941011    >C                   CALL      'XATB'
229300941011    >C                   PARM                    XATBDS
229400941011    > *
229500941011    >C                   MOVE      *OFF          WTBERR
229600941011    >C     XTBERR        IFEQ      *ON
229700941011    >C                   MOVE      *BLANK        XTBUNI
229800941011    >C                   MOVE      *ON           WTBERR            1
229900941011    >C                   ENDIF
230000941011    > *
230100941011    >C                   ENDSR
230200941103     C/EJECT
230300941103      ************************************************************
230400941103      * Reperimento dati società
230500941103      ************************************************************
230600941103     C     REPSOC        BEGSR
230700941103      *
230800941103     C                   CALL      'XSOC'
230900941103     C                   PARM                    TIPXSC            6
231000941103     C                   PARM                    SOCXSC            3
231100941103     C                   PARM                    CDSXSC            9 0
231200941103     C                   PARM                    MODXSC            3
231300941103     C                   PARM      *BLANKS       RTNXSC            1
231400941103     C                   PARM                    XSOCDS
231500941103     C                   PARM                    KPJBA
231600941103      *
231700941103     C                   ENDSR
231800941103     C/EJECT
231900950906**
232000990804PROMSG    *LIBL     OPZ0002                                                                **C1212**
232100990804PROMSG    *LIBL     OPZ0003                                                                **C1212**
232200990804PROMSG    *LIBL     OPZ0004
232300990804PROMSG    *LIBL     OPZ0005                                                                **C1212**
232400990804**
23250099080402S      <Modifica                                                                         **C1212**
23260099080403S      <Copia                                                                            **C1212**
23270099080404S      <Annullamento
23280099080405S      <Visualizzazione                                                                  **C1212**
232900940506** TASTI DI FUNZIONE  D1
233000940506PROMSG    *LIBL     KEY0001  01
233100940506PROMSG    *LIBL     KEY0002  02
233200940506PROMSG    *LIBL     KEY0003  03
233300940506PROMSG    *LIBL     KEY0004  04
233400940506PROMSG    *LIBL     KEY0005  05
233500940506PROMSG    *LIBL     KEY0006  06
233600940506PROMSG    *LIBL     KEY0007  07
233700940506PROMSG    *LIBL     KEY0008  08
233800940506PROMSG    *LIBL     KEY0009  09
233900940506PROMSG    *LIBL     KEY0010  10
234000940506PROMSG    *LIBL     KEY0011  11
234100940506PROMSG    *LIBL     KEY0012  12
234200940506PROMSG    *LIBL     KEY0013  13
234300940506PROMSG    *LIBL     KEY0014  14
234400940506PROMSG    *LIBL     KEY0015  15
234500940506PROMSG    *LIBL     KEY0016  16
234600940506PROMSG    *LIBL     KEY0017  17
234700940506PROMSG    *LIBL     KEY0018  18
234800940506PROMSG    *LIBL     KEY0019  19
234900940506PROMSG    *LIBL     KEY0020  20
235000940506PROMSG    *LIBL     KEY0021  21
235100940506PROMSG    *LIBL     KEY0022  22
235200940506PROMSG    *LIBL     KEY0023  23
235300940506PROMSG    *LIBL     KEY0024  24
235400940506**  ABILITAZIONE D1
2355009407200N  01
2356009407200N  02
2357009407201N  03
2358009407201N  04
2359009407200N  05
2360009407200N  06
2361009407200N  07
2362009908041N  08                                                                                     **C1212**
2363009908040N  09
2364009908041N  10                                                                                     **C1212**
2365009407200N  11
2366009407201N  12
2367009407200N  13
2368009407200N  14
2369009407200N  15
2370009407200N  16
2371009407200N  17
2372009407201N  18
2373009407200N  19
2374009407200N  20
2375009407200N  21
2376009407200N  22
2377009509060N  23   Impostare a '1' se si utilizzano opzioni
2378009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
237900940916** TASTI DI FUNZIONE  S1
238000940916PROMSG    *LIBL     KEY0001  01
238100940916PROMSG    *LIBL     KEY0002  02
238200940916PROMSG    *LIBL     KEY0003  03
238300940916PROMSG    *LIBL     KEY0004  04
238400940916PROMSG    *LIBL     KEY0005  05
238500940916PROMSG    *LIBL     KEY0006  06
238600940916PROMSG    *LIBL     KEY0007  07
238700940916PROMSG    *LIBL     KEY0008  08
238800940916PROMSG    *LIBL     KEY0009  09
238900940916PROMSG    *LIBL     KEY0010  10
239000940916PROMSG    *LIBL     KEY0011  11
239100940916PROMSG    *LIBL     KEY0012  12
239200940916PROMSG    *LIBL     KEY0013  13
239300940916PROMSG    *LIBL     KEY0014  14
239400940916PROMSG    *LIBL     KEY0015  15
239500940916PROMSG    *LIBL     KEY0016  16
239600940916PROMSG    *LIBL     KEY0017  17
239700940916PROMSG    *LIBL     KEY0018  18
239800940916PROMSG    *LIBL     KEY0019  19
239900940916PROMSG    *LIBL     KEY0020  20
240000940916PROMSG    *LIBL     KEY0021  21
240100940916PROMSG    *LIBL     KEY0022  22
240200940916PROMSG    *LIBL     KEY0023  23
240300940916PROMSG    *LIBL     KEY0024  24
240400940916**  ABILITAZIONE S1
2405009409160N  01
2406009409160N  02
2407009409161N  03
2408009409160N  04
2409009409160N  05
2410009409160N  06
2411009409160N  07
2412009908041N  08                                                                                     **C1212**
2413009908040N  09
2414009908041N  10
2415009409160N  11
2416009409161N  12
2417009409160N  13
2418009409160N  14
2419009409160N  15
2420009409160N  16
2421009409161N  17
2422009409161N  18
2423009409160N  19
2424009409160N  20
2425009409160N  21
2426009409160N  22
2427009509061N  23   Impostare a '1' se si utilizzano opzioni
2428009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
242900940916** TASTI DI FUNZIONE  D3
243000940506PROMSG    *LIBL     KEY0001  01
243100940506PROMSG    *LIBL     KEY0002  02
243200940506PROMSG    *LIBL     KEY0003  03
243300940506PROMSG    *LIBL     KEY0004  04
243400940506PROMSG    *LIBL     KEY0005  05
243500940506PROMSG    *LIBL     KEY0006  06
243600940506PROMSG    *LIBL     KEY0007  07
243700940506PROMSG    *LIBL     KEY0008  08
243800940506PROMSG    *LIBL     KEY0009  09
243900940506PROMSG    *LIBL     KEY0010  10
244000940506PROMSG    *LIBL     KEY0011  11
244100940506PROMSG    *LIBL     KEY0012  12
244200940506PROMSG    *LIBL     KEY0013  13
244300940506PROMSG    *LIBL     KEY0014  14
244400940506PROMSG    *LIBL     KEY0015  15
244500940506PROMSG    *LIBL     KEY0016  16
244600940506PROMSG    *LIBL     KEY0017  17
244700940506PROMSG    *LIBL     KEY0018  18
244800940506PROMSG    *LIBL     KEY0019  19
244900940506PROMSG    *LIBL     KEY0020  20
245000940506PROMSG    *LIBL     KEY0021  21
245100940506PROMSG    *LIBL     KEY0022  22
245200940506PROMSG    *LIBL     KEY0023  23
245300940506PROMSG    *LIBL     KEY0024  24
245400940916**  ABILITAZIONE D3
2455009407200N  01
2456009407200N  02
2457009407201N  03
2458009407200N  04
2459009407200N  05
2460009407200N  06
2461009407200N  07
2462009407200N  08
2463009407200N  09
2464009409160N  10
2465009407200N  11
2466009407201N  12
2467009407200N  13
2468009407200N  14
2469009407200N  15
2470009407200N  16
2471009409160N  17
2472009409160N  18
2473009407200N  19
2474009407200N  20
2475009407200N  21
2476009407200N  22
2477009509060N  23   Impostare a '1' se si utilizzano opzioni
2478009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
