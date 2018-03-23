000100000105     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200000105     H*PARMS CVTOPT(*NONE)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400000105     H*--------INIZIO SPECIFICHE --------------------------*
000500940224      *----------*
000600941007    > * ANA003R  *
000700940224      *----------*
000800940317    > *               TITOLO PROGRAMMA
000900940224      *          ------------------------------
001000940224      *
001100940224      *
001200941013      * DOCUMENTI MICROANALISI :  NSDAMUS/ANALISI/PROF 001.DOC
001300940224      *
001400940224      * NOTE TECNICHE :
001500940224      *
001600940224      *
001700940224      * PERSONALIZZAZIONI
001800940224      *
001900940224      *
002000940224      *
002100940314      *               MAPPA INDICATORI
002200940224      *
002300940307      *  21           GENERICO OPERAZIONI I/O
002400940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002500940224      *  30           SFLDSP
002600940224      * N31           SFLCLR
002700940224      *  31           SFLDSPCTL
002800940224      *  32           SFLNXTCHG
002900940224      *  33           SFLEND
003000940224      *  39           OF PRTF
003100940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003200940317    > *  Specificare l'uso dei singoli indicatori
003300940224      *  50 <---> 98  ERRORI SU VIDEO
003400940909    > *  Specificare l'uso dei singoli indicatori
003500940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003600940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003700940224      *  99           INDIC. GENERALE DI ERRORE
003800940128     F*----------------------------------------------------*
003900940314    >F*----------------------------------------------------------------
004000940314    >F* Viste logiche utilizzate per i diversi ordinamenti
004100940314    >F*----------------------------------------------------------------
004200941007     FANABS01L  IF   E           K DISK
004300941007     F                                     RENAME(ANABS000:ANABS01)
004400940211     F*------------
004500941010     FANABS02L  IF   E           K DISK
004600941010     F                                     RENAME(ANABS000:ANABS02)
004700941010     F*------------
004800940201     FANMPE02L  UF A E           K DISK
004900940211     F*------------
005000941007    >FANA003D   CF   E             WORKSTN
005100941007     F                                     RENAME(A003D1:FMTD1)
005200941007     F                                     RENAME(A003D2:FMTD2)
005300941007     F                                     RENAME(A003D3:FMTD3)
005400941007     F                                     RENAME(A003S1:FMTS1)
005500941007     F                                     RENAME(A003C1:FMTC1)
005600941007     F                                     RENAME(A003T1:FMTT1)
005700941007     F                                     RENAME(A003Z1:FMTZ1)
005800941007     F                                     RENAME(A003Z2:FMTZ2)
005900941007     F                                     RENAME(A003Z3:FMTZ3)
006000940131     F                                     SFILE(FMTS1:S1NRR)
006100940201     F                                     INFDS(DSFMT)
006200940128     D*----------------------------------------------------*
006300940302     D* Schiere per la gestione delle opzioni
006400940302     D*
006500950403    >D $MS1            S             27    DIM(2) CTDATA PERRCD(1)
006600950403    >D $TX1            S             50    DIM(2) ALT($MS1)
006700941011    >D $OP1            S              2  0 DIM(2) CTDATA PERRCD(1)
006800941011    >D $FL1            S              1    DIM(2) ALT($OP1)
006900950403     D $TX             S             50    DIM(50)
007000950403     D $FL             S              1    DIM(50)
007100940909     D* Schiere generali per la gestione dei tasti di funzione
007200940506     D $FM             S             27    DIM(24)
007300940506     D $FX             S             50    DIM(24)
007400940506     D $FC             S              1    DIM(24)
007500940506     D $FR             S              1    DIM(24)
007600940909     D* Schiere per la gestione dei tasti di funzione
007700940506     D* Video D1
007800940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
007900940506     D $FX1            S             50    DIM(24) ALT($FM1)
008000940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008100940506     D $FR1            S              1    DIM(24) ALT($FC1)
008200940506     D* Schiere per la gestione dei tasti di funzione
008300940506     D* Video S1
008400940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
008500940506     D $FX2            S             50    DIM(24) ALT($FM2)
008600940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
008700940506     D $FR2            S              1    DIM(24) ALT($FC2)
008800940916     D* Schiere per la gestione dei tasti di funzione
008900940916     D* Video D2
009000940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
009100940916     D $FX3            S             50    DIM(24) ALT($FM3)
009200940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
009300940916     D $FR3            S              1    DIM(24) ALT($FC3)
009400940302     D*
009500940211     D/EJECT
009600940128     D*----------------------------------------------------*
009700940211     D*-------------
009800940207     D* Valori assunti
009900941111     D DSMPE           DS                  INZ
010000940207     D  $LOP                   1      1  0
010100940831     D  $ORD                   2      3  0
010200940909     D*-------------
010300940831    >D*Definire campi chiave per la vista logica 99L
010400940831    >D* DS per chiave aggancio
010500941007    >D*DSKEY       DS
010600940831    >D*                                       1   90PRGKEY
010700940831    >D*                                      10  140RIGKEY
010800940909     D*-------------
010900940211     D* Passaggio Parametri
011000940211     D KPJBA         E DS
011100940225     D*-------------
011200940225     D* Reperimento Autorizzazioni
011300940225     D XAUT          E DS                  EXTNAME(XCHKAUTDS)
011400940325     D*-------------
011500941012     D* Parametri del PGM di manutenzione ANA004R
011600941012    >D A001DS        E DS                  EXTNAME(ANA001DS) INZ
011700941012    >D  RET001       E                     INZ('0')
011800941012    >D  OPR001       E                     INZ('0')
011900941012    >D  ERR001       E                     INZ('0')
012000940325     D* Parametri in ricezione
012100941007    >D A003DS        E DS                  EXTNAME(ANA003DS)
012200940325     D*-------------
012300941007     D* Parametri del PGM di manutenzione ANA004R
012400941010    >D A004DS        E DS                  EXTNAME(ANA004DS) INZ
012500941007    >D  RET004       E                     INZ('0')
012600941007    >D  OPR004       E                     INZ('0')
012700941007    >D  ERR004       E                     INZ('0')
012800941011     D*-------------
012900941011     D* Parametri del PGM  ANA005R
013000941011    >D A005DS        E DS                  EXTNAME(ANA005DS) INZ
013100941011    >D  RET005       E                     INZ('0')
013200941011    >D  OPR005       E                     INZ('0')
013300941011    >D  ERR005       E                     INZ('0')
013400940211     D*-------------
013500940211     D DSFMT           DS           512
013600940506     D  $TASTO               369    369
013700940211     D  NRG                  370    370
013800940211     D  NCL                  371    371
013900940211     D  SFLNRR               378    379B 0
014000940211     D*-------------
014100940211     D* posizione cursore
014200940211     D CURSOR          DS
014300940211     D  RIRI                   1      2B 0 INZ
014400940211     D  R$$                    2      2
014500940211     D  COCO                   3      4B 0 INZ
014600940211     D  C$$                    4      4
014700940127     D*-------------
014800940127     D* Reperimento nome PGM
014900940127     D STATUS         SDS           333
015000940127     D  DSPGM            *PROC
015100940207     D*-------------
015200940616     D* Costanti
015300940616     D* nr.di righe del sfl (SFLPAG)
015400940314     D SFLPAG          C                   CONST(10)
015500940609     D* dimensione della schiera $MS1
015600941011     D $DIMOP          C                   CONST(2)
015700940616     D*-------------
015800940506     D* Tasti di funzione
015900940506     D F01             C                   CONST(X'31')
016000940506     D F02             C                   CONST(X'32')
016100940506     D F03             C                   CONST(X'33')
016200940506     D F04             C                   CONST(X'34')
016300940506     D F05             C                   CONST(X'35')
016400940506     D F06             C                   CONST(X'36')
016500940506     D F07             C                   CONST(X'37')
016600940506     D F08             C                   CONST(X'38')
016700940506     D F09             C                   CONST(X'39')
016800940506     D F10             C                   CONST(X'3A')
016900940506     D F11             C                   CONST(X'3B')
017000940506     D F12             C                   CONST(X'3C')
017100940506     D F13             C                   CONST(X'B1')
017200940506     D F14             C                   CONST(X'B2')
017300940506     D F15             C                   CONST(X'B3')
017400940506     D F16             C                   CONST(X'B4')
017500940506     D F17             C                   CONST(X'B5')
017600940506     D F18             C                   CONST(X'B6')
017700940506     D F19             C                   CONST(X'B7')
017800940506     D F20             C                   CONST(X'B8')
017900940506     D F21             C                   CONST(X'B9')
018000940506     D F22             C                   CONST(X'BA')
018100940506     D F23             C                   CONST(X'BB')
018200940506     D F24             C                   CONST(X'BC')
018300940506     D ENTER           C                   CONST(X'F1')
018400940506     D ROLDWN          C                   CONST(X'F4')
018500940506     D ROLLUP          C                   CONST(X'F5')
018600941011      *-------------
018700941012     D* Parametri per ricerca codice ANATB00F
018800941012     D XTABDS          DS                  INZ
018900941012     D* '  ' = Gestione
019000941012     D* '01' = Ricerca
019100941012     D* '02' = Gestione
019200941012     D* '05' = Visualizzazione
019300941012     D* '11' = Abilitazione
019400941012     D  OPZTAB                 1      2
019500941012     D* 0 = ritorno normale
019600941012     D* 1 = F3
019700941012     D* 2 = F12
019800941012     D  RETTAB                 3      3
019900941012     D* operazione eseguite dal pgm chiamato :
020000941012     D* 0 = nessuna operazione
020100941012     D* 1 = eseguito aggiornamento
020200941012     D  OPRTAB                 4      4
020300941012     D* funzione non eseguibile per errore :
020400941012     D* 0 = nessun errore
020500941012     D* 1 = funzione richiamata chiusa in errore
020600941012     D* Aggiungere eventuali altri codici di errore
020700941012     D  ERRTAB                 5      5
020800941012     D* Azienda
020900941012     D  AZITAB                 6      8
021000941012     D* Lingua
021100941012     D  LINTAB                 9      9
021200941012     D* Codice scelto
021300941012     D  KEYTAB                10     24
021400941012     D* riga cursore chiamante
021500941012     D  RIGTAB                25     26  0
021600941012     D* colonna cursore chiamante
021700941012     D  COLTAB                27     28  0
021800941012     D ERRORE          C                   CONST('Codice errato')
021900941012     D DATA            C                   CONST('0001-01-01')
022000941011      *
022100941103     D* Dati di ambiente ottenuti da XSOC
022200941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
022300941103      * DS Interna per dati di output di XSOC
022400941103     D XSOCDS          DS          1000
022500941011      *----------------------------------------------------*
022600941011      *                MAIN LINE PROGRAM
022700941011      *----------------------------------------------------*
022800941011      *
022900941011      * inizializzazione variabili
023000940223     C                   EXSR      INZVAR
023100941011      *
023200940223     C     $FINE         DOWEQ     *OFF
023300940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
023400940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
023500940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
023600940117     C                   END
023700940117     C                   END
023800940325     C* fine programma
023900940325     C                   EXSR      ENDPGM
024000940225     C*
024100940325     C/EJECT
024200940325     C************************************************************
024300940325     C* FINE PROGRAMMA
024400940325     C************************************************************
024500940325     C     ENDPGM        BEGSR
024600940722     C*
024700940722     C* aggiorno valori assunti
024800940722     C                   EXSR      AGGMPE
024900941007     C   KC              MOVE      '1'           RET003
025000941007     C   KL              MOVE      '2'           RET003
025100940325     C*
025200940325     C* passaggio dati al pgm chiamante
025300941007    >C                   MOVEL     A003DS        KPJBU
025400940325     C*
025500940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
025600941007    >C     OPZ003        IFEQ      *BLANK
025700940325     C                   SETON                                            LR
025800940325     C* altrimenti chiudo in RT
025900940325     C                   ELSE
026000940325     C                   SETON                                            RT
026100940325     C                   ENDIF
026200940616     C                   RETURN
026300940325     C*
026400940325     C                   ENDSR
026500940224     C/EJECT
026600940201     C************************************************************
026700940201     C* RICERCHE
026800940201     C************************************************************
026900940127     C     SEARCH        BEGSR
027000940127     C*
027100940127     C* determino Riga/Colonna del cursore
027200940127     C                   MOVE      NRG           R$$
027300940127     C                   MOVE      NCL           C$$
027400940127     C                   Z-ADD     RIRI          CURR
027500940127     C                   Z-ADD     COCO          CURC
027600940127     C*
0277009401271    C                   SELECT
027800940127     C*
027900940314    >C*--------------------------------------------------------------
028000940314    >C* inserire le routine di ricerca
028100940314    >C*--------------------------------------------------------------
028200940207     C* FMTD1  - MODULO
028300941012    >C     H1NMFL        WHENEQ    'D1GRP'
028400940127     C*
028500941013     C                   MOVE      *ZERO         WWGRP
028600941012     C                   MOVE      WWGRP         KEYTAB
028700941012     C                   MOVE      '01'          OPZTAB
028800941103     C                   MOVE      XSCSOC        AZITAB
028900941103     C                   MOVE      XSCLIN        LINTAB
029000941012     C                   CLEAR                   KPJBU
029100941012     C                   MOVEL     XTABDS        KPJBU
029200941012     C                   CALL      'ANGG22R'
029300941012     C                   PARM                    KPJBA
029400941012     C                   MOVEL     KPJBU         XTABDS
029500941012     C                   MOVE      KEYTAB        WWGRP
029600941012      *
0297009410122   >C     WWGRP         IFNE      *ZEROS
029800941012    >C                   MOVEL     WWGRP         D1GRP
0299009410122-  >C                   ENDIF
030000941012      *
030100941012      *
030200941201     C* imposto pos. del cursore
030300941201     C                   Z-ADD     CURR          H1RIGA
030400941201     C                   Z-ADD     CURC          H1COLO
030500941201      *
030600941012    >C     H1NMFL        WHENEQ    'D1PRF'
030700941012    >C                   RESET                   A001DS
030800941012    >C                   MOVEL     '01'          OPZ001
030900941012    >C                   MOVEL     A001DS        KPJBU
031000941012    >C                   CALL      'ANA001R'
031100941012     C                   PARM                    KPJBA
031200941012    >C                   MOVEL     KPJBU         A001DS
0313009410132   >C     PRF001        IFNE      *BLANK
031400941013    >C                   MOVE      PRF001        D1PRF
0315009410122-  >C                   ENDIF
031600940223     C*
031700940223     C* ricerca non abilitata in questa posizione
0318009402231O   C                   OTHER
031900940929     C* imposto pos. del cursore
032000940929     C                   Z-ADD     CURR          H1RIGA
032100940929     C                   Z-ADD     CURC          H1COLO
032200940929     C                   SETON                                        9899
032300940127     C*
0324009401271-   C                   ENDSL
032500940127     C*
032600940506     C                   MOVE      *HIVAL        $LASTV
032700940127     C                   ENDSR
032800940131     C/EJECT
032900940224     C************************************************************
033000940131     C* RIEMPIMENTO OPZIONI
033100940224     C************************************************************
033200940131     C     RIEOPZ        BEGSR
033300940223     C*
033400940223     C* Input :
033500940223     C* - $TX = schiera con tutti i testi disponibili
033600940223     C* - $FL = schiere con i flag di validità associati ai testi
033700940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
033800940223     C*          opzione usato
033900940223     C* - WMXO = variabile contenente il numero massimo di testi
034000940223     C*          opzione usabili
034100940721     C* - $L01 = Lunghezza prima riga
034200940721     C* - $L02 = Lunghezza seconda riga
034300940223     C* Output :
034400940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
034500940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
034600950403     C* - FF23  = flag che indica se c'è bisogno del tasto F23
034700940223     C*
034800950403     C                   CALL      'X04OPMSG'
034900940223     C* input
035000940223     C                   PARM                    $TX
035100940223     C                   PARM                    $FL
035200940225     C                   PARM                    WCOZ              3 0
035300940223     C                   PARM                    WMXO              3 0
035400940223     C* output
035500940223     C                   PARM                    WOPZ1            76
035600940223     C                   PARM                    WOPZ2            76
035700950403     C                   PARM                    FF23              1
035800940721    >C* input
035900940721    >C                   PARM                    $L01              3 0
036000940721    >C                   PARM                    $L02              3 0
036100940223     C*
036200060224A1148C                   PARM                    $ALLOPT         960
036300060224A1148C*
036400940131     C                   ENDSR
036500940506     C/EJECT
036600940506     C************************************************************
036700940506     C* RIEMPIMENTO TASTI FUNZIONE
036800940506     C************************************************************
036900940506     C     RIEKEY        BEGSR
037000940506     C*
037100940506     C* Input :
037200940506     C* - $FX = schiera con tutti i testi disponibili
037300940506     C* - $FC = schiere con i flag di validità associati ai testi
037400940506     C* - $FR = schiere con i flag di REVERSE IMAGE
037500940506     C* - $ULK = ultimo testo caricato
037600940721     C* - $L01 = Lunghezza prima riga
037700940721     C* - $L02 = Lunghezza seconda riga
037800940506     C* Output :
037900940506     C* - $ULK
038000940609     C* - $KEY1= 1° riga di tasti funzione
038100940609     C* - $KEY2= 2° riga di tasti funzione
038200940506     C*
038300940506     C                   CALL      'XKEYMSG'
038400940506     C* input
038500940506     C                   PARM                    $FX
038600940506     C                   PARM                    $FC
038700940506     C                   PARM                    $FR
038800940506     C                   PARM                    $ULK              3 0
038900940506     C* output
039000940506     C                   PARM                    $KEY1            79
039100940506     C                   PARM                    $KEY2            79
039200940721    >C* input
039300940721    >C                   PARM                    $L01              3 0
039400940721    >C                   PARM                    $L02              3 0
039500940506     C*
039600060224A1148C                   PARM                    $ALLFUNCT       480
039700060224A1148C*
039800940506     C                   ENDSR
039900940224     C/EJECT
040000940224     C************************************************************
040100940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
040200940224     C************************************************************
040300940224     C     GESPIE        BEGSR
040400940224     C*
040500940224     C                   SELECT
040600940224     C* Parzializzazioni
040700940224     C     $GEST         WHENEQ    'D1'
040800940506     C     $ULKD1        IFEQ      0
040900940506     C* riempimento opzioni
041000940506     C                   EXSR      F24D1
041100940506     C                   ELSE
041200940506     C                   WRITE     FMTZ1
041300940506     C                   ENDIF
041400940506     C* Lista
041500940506     C     $GEST         WHENEQ    'S1'
041600940506     C     $ULKS1        IFEQ      0
041700940506     C                   EXSR      F24S1
041800940506     C                   ELSE
041900940506     C                   WRITE     FMTZ2
042000940506     C                   ENDIF
042100940916     C* Modifica valori assunti
042200940916     C     $GEST         WHENEQ    'D2'
042300940916     C     $ULKD2        IFEQ      0
042400940916     C                   EXSR      F24D2
042500940916     C                   ELSE
042600940916     C                   WRITE     FMTZ3
042700940916     C                   ENDIF
042800940506     C                   ENDSL
042900940506     C*
043000940224     C                   ENDSR
043100940224     C/EJECT
043200940224     C************************************************************
043300940224     C* GESTIONE PARZIALIZZAZIONI
043400940224     C************************************************************
043500940224     C     GESD1         BEGSR
043600940224     C* inizializzazione videata
043700940223     C     $INZD1        IFEQ      *ON
043800940127     C                   EXSR      INZD1
043900940223     C                   MOVE      *OFF          $INZD1
044000940117     C                   END
044100940223     C* emissione piede videata se proveniente da altra
044200940223     C* salvataggio valore formato di provenienza
044300940202     C     $LASTV        IFNE      'D1'
044400940223     C* salvo stato F17 per sapere cosa fare in caso di F12
044500940223     C                   MOVE      *INKR         WINKR
044600940202     C                   WRITE     FMTT1
044700940224     C                   EXSR      GESPIE
044800940202     C                   MOVE      $LASTV        $LASTG
044900940202     C                   MOVE      'D1'          $LASTV
045000940707     C                   ENDIF
045100940707     C*
045200940707     C* se esistono errori sulla videata
045300940707     C* emetto la write del formato a indicatori spenti per vedere
045400940707     C* le eventuali decodifiche
045500940707     C     *IN99         IFEQ      *ON
045600940707     C                   MOVEA     *IN           WIN
045700940707     C                   MOVE      *ALL'0'       IN5098           49
045800940707     C                   MOVEA     IN5098        *IN(50)
045900940707     C                   WRITE     FMTD1
046000940707     C                   MOVEA     WIN           *IN
046100940707     C                   ENDIF
046200940131     C*
046300940131     C                   EXFMT     FMTD1
046400940506     C* controllo abilitazione tasti funzione
046500940506     C*
046600940506     C                   EXSR      CTRKD1
046700940506     C*
046800940506     C     *IN99         IFEQ      *OFF
046900940131     C*
047000940117     C                   SELECT
047100940916     C* F2=Decodifica
047200940916     C     $TASTO        WHENEQ    F02
047300940916     C                   EXSR      F02D1
047400940131     C* F3=Fine
047500940506     C     $TASTO        WHENEQ    F03
047600940307     C                   EXSR      F03D1
047700940131     C* F4=Ricerca
047800940506     C     $TASTO        WHENEQ    F04
047900940127     C                   EXSR      SEARCH
048000940916     C* F5=Refresh
048100940916     C     $TASTO        WHENEQ    F05
048200940916     C                   EXSR      F05D1
048300940131     C* F12=Ritorno
048400940506     C     $TASTO        WHENEQ    F12
048500940307     C                   EXSR      F12D1
048600940131     C* F18=Modifica valori assunti
048700940506     C     $TASTO        WHENEQ    F18
048800940307     C                   EXSR      F18D1
048900940224     C* F24=Altri tasti
049000940506     C     $TASTO        WHENEQ    F24
049100940309     C                   EXSR      F24D1
049200940131     C*
049300940117     C                   OTHER
049400940127     C                   EXSR      CTRD1
049500941010     C     *IN99         IFEQ      *OFF
049600940131     C* definisco l'ordinamento
049700940131     C                   SELECT
049800940314    >C* << nel caso di più viste logiche e quindi più campi di
049900940314    >C*    parzializzazione, qui vanno definiti i criteri per
050000940314    >C*    determinare quale VL va letta, a seconda di quali campi
050100940314    >C*    sono stati valorizzati >>
050200941007    >C     D1PRF         WHENNE    *BLANK
050300940131     C                   Z-ADD     1             $ORD
050400941007     C                   SETON                                            01
050500941124     C                   SETOFF                                           02
050600941010    >C     D1GRP         WHENNE    0
050700940131     C                   Z-ADD     2             $ORD
050800941007     C                   SETON                                            02
050900941124     C                   SETOFF                                           01
051000940131     C* nel caso non sia stato digitato nulla uso l'ordinamento
051100940131     C* dei valori assunti
051200940131     C                   ENDSL
051300941010     C                   MOVE      'S1'          $GEST
051400940223     C                   MOVE      *ON           $INZS1
051500940302     C                   MOVE      *BLANK        W1POS
051600940211     C                   ENDIF
051700940224     C*
051800940117     C                   ENDSL
051900940224     C*
052000940506     C                   ENDIF
052100940117     C                   ENDSR
052200940224     C/EJECT
052300940224     C************************************************************
052400940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
052500940224     C************************************************************
052600940127     C     INZD1         BEGSR
052700940318     C*
052800940318     C                   CLEAR                   FMTD1
052801151123>>>>>C                   EVAL      d1societa = '*'
052900940831     C* Se pgm non richiamato
053000941007     C     OPZ003        IFEQ      *BLANK
053100941010    >C*******              Z-ADDUDATE     D1DRG
053200940831     C                   ELSE
053300940831     C* Se pgm richiamato
053400940831     C                   SELECT
053500941010     C     PRF003        WHENNE    *BLANK
053600941010     C     SOC003        ANDNE     *BLANK
053700941010     C                   MOVEL     PRF003        D1PRF
053800941010     C                   OTHER
053900941010     C*********            Z-ADDUDATE     D1DRG
054000940831     C                   ENDSL
054100940831     C                   END
054200940224     C*
054300940117     C                   ENDSR
054400940506     C/EJECT
054500940506     C************************************************************
054600940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
054700940609     C* INPUT
054800940509     C* $FC1 = Abilitazioni videata D!
054900940509     C* OUTPUT
055000940509     C* *IN97 *IN99 Errori
055100940506     C************************************************************
055200940506     C     CTRKD1        BEGSR
055300940506     C                   SETOFF                                       99
055400940506     C                   MOVEA     $FC1          $FC
055500940506     C                   EXSR      CTRKEY
055600940506     C     *IN99         IFEQ      *ON
055700940506     C                   SETON                                        97
055800940506     C                   ENDIF
055900940506     C                   ENDSR
056000940506     C/EJECT
056100940506     C************************************************************
056200940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
056300940506     C************************************************************
056400940506     C     CTRKS1        BEGSR
056500940506     C                   SETOFF                                       99
056600940506     C                   MOVEA     $FC2          $FC
056700940506     C                   EXSR      CTRKEY
056800940506     C     *IN99         IFEQ      *ON
056900940506     C                   SETON                                        97
057000940506     C                   ENDIF
057100940506     C                   ENDSR
057200940506     C/EJECT
057300940506     C************************************************************
057400940506     C* CONTROLLO ABILITAZIONE TASTI
057500940509     C* INPUT
057600940609     C* $TASTO =Tasto funzione premuto
057700940509     C* $FC = Tasti abilitati
057800940509     C* OUTPUT
057900940509     C* IN99 = ERrore
058000940506     C************************************************************
058100940506     C     CTRKEY        BEGSR
058200940506     C                   SELECT
058300940506     C     $TASTO        WHENEQ    F01
058400940720     C     $FC(1)        IFEQ      '0'
058500940506     C                   SETON                                        99
058600940506     C                   ENDIF
058700940506     C     $TASTO        WHENEQ    F02
058800940720     C     $FC(2)        IFEQ      '0'
058900940506     C                   SETON                                        99
059000940506     C                   ENDIF
059100940506     C     $TASTO        WHENEQ    F03
059200940720     C     $FC(3)        IFEQ      '0'
059300940506     C                   SETON                                        99
059400940506     C                   ENDIF
059500940506     C     $TASTO        WHENEQ    F04
059600940720     C     $FC(4)        IFEQ      '0'
059700940506     C                   SETON                                        99
059800940506     C                   ENDIF
059900940506     C     $TASTO        WHENEQ    F05
060000940720     C     $FC(5)        IFEQ      '0'
060100940506     C                   SETON                                        99
060200940506     C                   ENDIF
060300940506     C     $TASTO        WHENEQ    F06
060400940720     C     $FC(6)        IFEQ      '0'
060500940506     C                   SETON                                        99
060600940506     C                   ENDIF
060700940506     C     $TASTO        WHENEQ    F07
060800940720     C     $FC(7)        IFEQ      '0'
060900940506     C                   SETON                                        99
061000940506     C                   ENDIF
061100940506     C     $TASTO        WHENEQ    F08
061200940720     C     $FC(8)        IFEQ      '0'
061300940506     C                   SETON                                        99
061400940506     C                   ENDIF
061500940615     C     $TASTO        WHENEQ    F09
061600940720     C     $FC(09)       IFEQ      '0'
061700940506     C                   SETON                                        99
061800940506     C                   ENDIF
061900940506     C     $TASTO        WHENEQ    F10
062000940720     C     $FC(10)       IFEQ      '0'
062100940506     C                   SETON                                        99
062200940506     C                   ENDIF
062300940506     C     $TASTO        WHENEQ    F11
062400940720     C     $FC(11)       IFEQ      '0'
062500940506     C                   SETON                                        99
062600940506     C                   ENDIF
062700940506     C     $TASTO        WHENEQ    F12
062800940720     C     $FC(12)       IFEQ      '0'
062900940506     C                   SETON                                        99
063000940506     C                   ENDIF
063100940506     C     $TASTO        WHENEQ    F13
063200940720     C     $FC(13)       IFEQ      '0'
063300940506     C                   SETON                                        99
063400940506     C                   ENDIF
063500940506     C     $TASTO        WHENEQ    F14
063600940720     C     $FC(14)       IFEQ      '0'
063700940506     C                   SETON                                        99
063800940506     C                   ENDIF
063900940506     C     $TASTO        WHENEQ    F15
064000940720     C     $FC(15)       IFEQ      '0'
064100940506     C                   SETON                                        99
064200940506     C                   ENDIF
064300940506     C     $TASTO        WHENEQ    F16
064400940720     C     $FC(16)       IFEQ      '0'
064500940506     C                   SETON                                        99
064600940506     C                   ENDIF
064700940506     C     $TASTO        WHENEQ    F17
064800940720     C     $FC(17)       IFEQ      '0'
064900940506     C                   SETON                                        99
065000940506     C                   ENDIF
065100940506     C     $TASTO        WHENEQ    F18
065200940720     C     $FC(18)       IFEQ      '0'
065300940506     C                   SETON                                        99
065400940506     C                   ENDIF
065500940506     C     $TASTO        WHENEQ    F19
065600940720     C     $FC(19)       IFEQ      '0'
065700940506     C                   SETON                                        99
065800940506     C                   ENDIF
065900940506     C     $TASTO        WHENEQ    F20
066000940720     C     $FC(20)       IFEQ      '0'
066100940506     C                   SETON                                        99
066200940506     C                   ENDIF
066300940506     C     $TASTO        WHENEQ    F21
066400940720     C     $FC(21)       IFEQ      '0'
066500940506     C                   SETON                                        99
066600940506     C                   ENDIF
066700940506     C     $TASTO        WHENEQ    F22
066800940720     C     $FC(22)       IFEQ      '0'
066900940506     C                   SETON                                        99
067000940506     C                   ENDIF
067100940506     C     $TASTO        WHENEQ    F23
067200940720     C     $FC(23)       IFEQ      '0'
067300940506     C                   SETON                                        99
067400940506     C                   ENDIF
067500940506     C     $TASTO        WHENEQ    F24
067600940720     C     $FC(24)       IFEQ      '0'
067700940506     C                   SETON                                        99
067800940506     C                   ENDIF
067900940506     C                   ENDSL
068000940506     C                   ENDSR
068100940916     C/EJECT
068200940916     C************************************************************
068300940916     C* GESTIONE F02 VIDEO D1
068400940916     C************************************************************
068500940916     C     F02D1         BEGSR
068600940916     C*
068700940916     C* per decodificare la videata richiamo la routine dei ctrl
068800940916     C* e poi disattivo gli eventuali messaggi di errore
068900940916     C                   EXSR      CTRD1
069000940916     C*
069100940916     C                   MOVE      *ALL'0'       IN5098
069200940916     C                   MOVEA     IN5098        *IN(50)
069300940916     C*
069400940916     C                   ENDSR
069500940224     C/EJECT
069600940131     C************************************************************
069700940307     C* GESTIONE F03 VIDEO D1
069800940131     C************************************************************
069900940307     C     F03D1         BEGSR
070000940131     C*
070100940307     C                   MOVE      *ON           $FINE
070200940325     C* fine programma
070300940325     C                   EXSR      ENDPGM
070400940307     C*
070500940307     C                   ENDSR
070600940916     C/EJECT
070700940916     C************************************************************
070800940916     C* GESTIONE F05 VIDEO D1
070900940916     C************************************************************
071000940916     C     F05D1         BEGSR
071100940916     C*
071200940916     C                   MOVE      *ON           $INZD1
071300940916     C*
071400940916     C                   ENDSR
071500940307     C/EJECT
071600940307     C************************************************************
071700940307     C* GESTIONE F12 VIDEO D1
071800940307     C************************************************************
071900940307     C     F12D1         BEGSR
072000940307     C*
072100940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
072200940307     C*  per F12 torno alla lista
072300940307     C     $LOP          IFEQ      2
072400940307     C     WINKR         OREQ      *ON
072500940307     C                   MOVE      'S1'          $GEST
072600940307     C*  altrimenti, per F12 torno a menu
072700940307     C                   ELSE
072800940307     C                   MOVE      *ON           $FINE
072900940307     C                   END
073000940307     C*
073100940307     C                   ENDSR
073200940307     C/EJECT
073300940307     C************************************************************
073400940307     C* GESTIONE F18 VIDEO D1
073500940307     C************************************************************
073600940307     C     F18D1         BEGSR
073700940307     C*
073800940307     C                   MOVE      'D2'          $GEST
073900940307     C                   MOVEL     *ON           $INZD2
074000940307     C*
074100940307     C                   ENDSR
074200940307     C/EJECT
074300940307     C************************************************************
074400940307     C* GESTIONE F24 VIDEO D1
074500940609     C* input
074600940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
074700940509     C* $FX1 =Testi tasti funzione
074800940509     C* $FC1 = Abilitazioni
074900940509     C* $FR1 = Inversioni di fondo
075000940307     C************************************************************
075100940307     C     F24D1         BEGSR
075200940506     C*
075300940506     C                   MOVEA     $FX1          $FX
075400940506     C                   MOVEA     $FC1          $FC
075500940506     C                   MOVEA     $FR1          $FR
075600940506     C                   Z-ADD     $ULKD1        $ULK
075700940929     C                   Z-ADD     79            $L01
075800940929     C                   Z-ADD     79            $L02
075900940506     C                   EXSR      RIEKEY
076000940506     C                   Z-ADD     $ULK          $ULKD1            3 0
076100940720     C                   MOVEA     $FC           $FC1
076200940506     C                   MOVEL     $KEY1         Z1KE1
076300060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
076400940506     C                   MOVEL     $KEY2         Z1KE2
076500940506     C                   WRITE     FMTZ1
076600940506     C*
076700940307     C*
076800940307     C                   ENDSR
076900940307     C/EJECT
077000940307     C************************************************************
077100940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
077200940307     C************************************************************
077300940307     C     CTRD1         BEGSR
077400940307     C*
077500940127     C                   SETOFF                                       99
077600940127     C* non imposto pos. del cursore
077700940127     C                   Z-ADD     0             H1RIGA
077800940127     C                   Z-ADD     0             H1COLO
077900940131     C*
078000940314    >C* << elencare gli eventuali controlli sui campi di
078100940314    >C*    parzializzazione, ricordando che quasi sempre essi
078200940314    >C*    non ne avranno perchè determinano solo il range di
078300940314    >C*    scrittura della lista e quindi (nel caso di campi
078400940314    >C*    alfanumerici) saranno trattati come chiavi parziali >>
078500940131     C*
078600940117     C                   ENDSR
078700940224     C/EJECT
078800940127     C************************************************************
078900940131     C* GESTIONE LISTA
079000940127     C************************************************************
079100940127     C     GESS1         BEGSR
079200940223     C* inizializzazione videata
079300940223     C     $INZS1        IFEQ      *ON
079400940127     C                   EXSR      INZS1
079500940223     C                   MOVE      *OFF          $INZS1
079600940127     C                   ENDIF
079700940223     C* emissione piede videata se proveniente da altra
079800940223     C* salvataggio valore formato di provenienza
079900940202     C     $LASTV        IFNE      'S1'
080000940202     C                   WRITE     FMTT1
080100940224     C                   EXSR      GESPIE
080200940202     C                   MOVE      $LASTV        $LASTG
080300940202     C                   MOVE      'S1'          $LASTV
080400940202     C                   END
080500940223     C*
080600940223     C     WMAX          IFEQ      0
080700940211     C                   WRITE     FMTD3                                        NO RCD
080800940211     C                   ENDIF
080900940831     C*Serve per emissione finestra pgm invio ang727r (per open/close
081000940831     C*window)
081100940831     C                   MOVE      '1'           WIN727            1
081200940127     C*
081300940131     C                   EXFMT     FMTC1
081400940204     C     C1NRR         IFNE      0
081500940204     C                   Z-ADD     C1NRR         WSFL
081600940204     C                   ENDIF
081700940127     C                   Z-ADD     SFLNRR        C1RCD
081800940506     C*
081900940506     C* controllo abilitazione tasti funzione
082000940506     C*
082100940506     C                   EXSR      CTRKS1
082200940506     C     *IN99         IFEQ      *OFF
082300940506     C*
082400940127     C*
0825009401271    C                   SELECT
082600940127     C* F3=Fine
082700940506     C     $TASTO        WHENEQ    F03
082800940309     C                   EXSR      F03S1
082900940131     C* F10=Immissione
083000940506     C     $TASTO        WHENEQ    F10
083100940309     C                   EXSR      F10S1
083200940131     C* F12=Ritorno
083300940506     C     $TASTO        WHENEQ    F12
083400940309     C                   EXSR      F12S1
083500940131     C* F17=Parzializzazioni
083600940506     C     $TASTO        WHENEQ    F17
083700940309     C                   EXSR      F17S1
083800940131     C* F18=Modifica valori assunti
083900940506     C     $TASTO        WHENEQ    F18
084000940309     C                   EXSR      F18S1
084100940224     C* F23=Altre opzioni
084200940506     C     $TASTO        WHENEQ    F23
084300940309     C                   EXSR      F23S1
084400940224     C* F24=Altri tasti
084500940506     C     $TASTO        WHENEQ    F24
084600940309     C                   EXSR      F24S1
084700940127     C* ROLL-UP
084800940506     C     $TASTO        WHENEQ    ROLLUP
084900940127     C                   EXSR      ROLS1
085000940127     C*
0851009401271O   C                   OTHER
085200940127     C* CONTROLLO DATI
085300940131     C                   EXSR      CTRC1
085400940201     C     *IN99         IFEQ      *OFF
085500940131     C                   EXSR      CTRS1
085600940131     C                   END
0857009401271-   C                   ENDSL
085800940127     C*
085900940506     C                   ENDIF
086000940127     C                   ENDSR
086100940224     C/EJECT
086200940127     C************************************************************
086300940131     C* INIZIALIZZAZIONE LISTA
086400940127     C************************************************************
086500940127     C     INZS1         BEGSR
086600940302     C* pulizia SFL
086700940128     C                   SETOFF                                         3031
086800940131     C                   WRITE     FMTC1
086900940128     C                   SETON                                          31
087000940127     C*
087100940128     C* riempimento opzioni
087200940223     C                   MOVEA     $TX1          $TX
087300940223     C                   MOVEA     $FL1          $FL
087400940309     C                   Z-ADD     0             WCOZ
087500940225     C*  equivale al num.di elementi della schiera $TX1
087600940509     C                   Z-ADD     $DIMOP        WMXO
087700950403     C                   Z-ADD     76            $L01
087800950403     C                   Z-ADD     76            $L02
087900940128     C                   EXSR      RIEOPZ
088000950403     C                   Z-ADD     23            X
088100950403     C                   MOVE      FF23          $FC2(X)
088200940608     C                   MOVEL     WOPZ1         C1OPZ1
088300060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
088400940608     C                   MOVEL     WOPZ2         C1OPZ2
088500940225     C                   Z-ADD     WCOZ          C1COZ
088600940128     C*
088700940128     C* CARICAMENTO SFL A PAGINE
088800940128     C*
088900940201     C                   Z-ADD     0             S1NRR
089000940128     C                   Z-ADD     0             WMAX
089100940224     C*
089200940224     C* Posizionamento su file pilota
089300940302     C                   CLEAR                   C1POS
089400941124     C                   CLEAR                   C1POS2
089500941007    >C                   EXSR      SETABS
089600940302     C* ripristino i valori del posizionamento
089700941124     C     $ORD          IFEQ      2
089800941124     C                   Z-ADD     W1POS2        C1POS2
089900941124     C                   ELSE
090000941124     C                   MOVE      W1POS         C1POS
090100941124     C                   ENDIF
090200940131     C*
090300940201     C* Lettura file pilota
090400941007    >C                   EXSR      REDABS
090500940131     C*
090600940131     C* Carico 1a pagina
090700940127     C                   EXSR      ROLS1
090800940201     C                   Z-ADD     SFLPAG        WPAG
090900940127     C*
091000940127     C                   ENDSR
091100940127     C/EJECT
091200940127     C************************************************************
091300940131     C* CARICAMENTO PAGINA LISTA
091400940127     C************************************************************
091500940127     C     ROLS1         BEGSR
091600940127     C*
091700940128     C                   SETOFF                                       32
091800940223     C                   Z-ADD     0             Y
091900940127     C                   Z-ADD     WMAX          S1NRR
092000940127     C*
092100940127     C* Leggo dal file anagrafico per caricare la lista
092200940127     C*
0923009401311    C     $EFILE        DOWEQ     *OFF
092400940201     C     Y             ANDLT     WPAG
092500940127     C*
092600940131     C                   EXSR      RIES1
092700940127     C*
092800940127     C                   ADD       1             S1NRR
092900940127     C                   ADD       1             Y
093000940127     C*
093100940131     C                   WRITE     FMTS1
093200940131     C*
093300941007    >C                   EXSR      REDABS
093400940128     C*
0935009401271-   C                   ENDDO
093600940127     C*
093700940223     C                   Z-ADD     S1NRR         WMAX                 30
093800940127     C*
093900940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
094000940127     C*
094100940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
094200940127     C                   MVR                     RESTO             3 0
094300940131     C     PAGINE        MULT      SFLPAG        C1RCD
0944009401271    C     RESTO         IFGT      0
094500940127     C                   ADD       1             C1RCD
0946009401271E   C                   ELSE
094700940407     C                   SUB       SFLPAG        C1RCD
094800940407     C                   ADD       1             C1RCD
0949009401271-   C                   ENDIF
095000940128     C*
095100940127     C                   ENDSR
095200940224     C/EJECT
095300940224     C************************************************************
095400940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
095500940224     C************************************************************
095600941007    >C     SETABS        BEGSR
095700940224     C*
095800940224     C                   SELECT
095900940314    >C* Ord.1 = chiave ordinamento 1
096000940224     C     $ORD          WHENEQ    1
096100940314    >C*-----------------------------------------------------------
096200940314    >C* Gestione posizionamento
096300940314    >C* Se il campo W1POS è > *blank imposto il posizionamento con
096400940314    >C* W1POS altrimenti lo imposto con il valore di parametrizzazione
096500940314    >C*-----------------------------------------------------------
096600940302     C     W1POS         IFNE      *BLANK
096700941124    >C                   MOVEL     W1POS         ABSPRF
096800940302     C                   ELSE
096900941007    >C                   MOVEL     D1PRF         ABSPRF
097000940302     C                   END
097100950302    >C     ABSPRF        SETLL     ANABS01
097200940224     C* Ord.2 = Modulo
097300940224     C     $ORD          WHENEQ    2
097400941124     C     W1POS2        IFNE      0
097500941124    >C                   Z-ADD     W1POS2        ABSGRP
097600941124    >C                   MOVEL     *BLANK        ABSPRF
097700940302     C                   ELSE
097800941010    >C                   MOVEL     D1GRP         ABSGRP
097900941010    >C                   MOVEL     *BLANK        ABSPRF
098000940302     C                   END
098100950302    >C     K2ABS2        SETLL     ANABS02
098200940831     C* Ord.99= Chiave univoca
098300941010     C*          $ORD      WHEQ 99
098400941010    >C*          W1POS     IFNE *BLANK
098500941010    >C*                    MOVELW1POS     MOVPRG
098600941010    >C*                    ELSE
098700941010    >C*                    MOVELD1PRG     MOVPRG
098800941010    >C*                    END
098900941010    >C*          MOVPRG    SETLLxxANA99
099000940224     C                   ENDSL
099100940224     C*
099200940224     C                   ENDSR
099300940128     C/EJECT
099400940128     C************************************************************
099500940131     C* LETTURA RCD ARCHIVIO PILOTA
099600940128     C************************************************************
099700941010    >C     REDABS        BEGSR
099800940128     C*
099900940131     C                   MOVEL     *OFF          $EFILE
100000940131     C                   MOVEL     *OFF          $RCDOK
100100940131     C*
1002009401311    C     $EFILE        DOUEQ     *ON
100300940131     C     $RCDOK        OREQ      *ON
100400940131     C*
1005009401312    C                   SELECT
100600940131     C     $ORD          WHENEQ    1
100700941010    >C                   READ      ANABS01                                21
100800940131     C     $ORD          WHENEQ    2
100900941010    >C                   READ      ANABS02                                21
101000941010     C*          $ORD      WHEQ 99
101100941010    >C*                    READ xxANA99                  21
1012009401312-   C                   ENDSL
101300940131     C*
101400940131     C                   MOVE      *IN21         $EFILE
101500940131     C*
1016009401312    C     $EFILE        IFEQ      *OFF
101700941010    >C                   EXSR      SELABS
1018009401312-   C                   ENDIF
101900940131     C*
1020009401311-   C                   ENDDO
102100940131     C*
102200940131     C                   MOVE      *IN21         *IN33                          SflEnd
102300940131     C*
102400940131     C                   ENDSR
102500940224     C/EJECT
102600940224     C************************************************************
102700940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
102800940224     C************************************************************
102900940224     C     CLCPAG        BEGSR
103000940224     C* Input :
103100940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
103200940224     C*          cursore
103300940224     C* - SFLPAG = numero rcd per pagina sfl
103400940224     C* Output :
103500940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
103600940224     C*
103700940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
103800940224     C                   MVR                     RESTO             3 0
103900940224     C     RESTO         IFGT      0
104000940224     C                   ADD       1             PAGINE
104100940224     C                   ENDIF
104200940224     C     PAGINE        MULT      SFLPAG        WPAG
104300940224     C*
104400940224     C                   ENDSR
104500940131     C/EJECT
104600940131     C************************************************************
104700940131     C* SELEZIONI SULLA RIGA LETTA
104800940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
104900940314     C* non rientra nei parametri di parzializzazione
105000940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
105100940314     C* superiore stabilito nei parametri
105200940314     C*
105300940131     C************************************************************
105400941010    >C     SELABS        BEGSR
105500940131     C*
105600940202     C                   MOVEL     *ON           $RCDOK
105700940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
105800940131A1211C                   MOVEL     *hival        ALL9             15
105900940314     C* creo limite massimo parzializzazioni
106000940609    >C* << per ogni valore alfanumerico creo il limite superiore
106100940314    >C*    per cui caricare, creando una variabile che contiene
106200940314    >C*    il valore digitato nel campo a video riempito di "9" >>
106300941010    >C     D1PRF         CAT       ALL9:0        PRF999
106400940314     C*
106500940314     C                   SELECT
106600940314     C*
106700940314     C* SELEZIONI RISPETTO AI VALORI RCD
106800940314     C*
106900940314    >C* << elaborare eventuali selezioni che non riguardano le
107000940316    >C*    parzializzazioni ma più propriamente i dati del rcd ;
107100940314    >C*    esempio : esclusione degli annullati. >>
107200940314     C*
107300940314     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
107400940314     C*
107500940316    >C* oggetto non compreso nella scelta
107600941010    >C     ABSPRF        WHENLT    D1PRF
107700941010    >C     ABSPRF        ORGT      PRF999
107800941010    >C                   MOVEL     *OFF          $RCDOK
107900941010    >C     ABSPRF        IFGT      PRF999
108000941010    >C     $ORD          ANDEQ     1
108100941010    >C     *HIVAL        SETLL     ANABS01
108200941010    >C                   ENDIF
108300940316    >C* modulo non compresa nella scelta
108400941010    >C     ABSGRP        WHENNE    D1GRP
108500941010    >C     D1GRP         ANDNE     0
108600941010      *
108700941010    >C                   MOVEL     *OFF          $RCDOK
108800941010      *
108900940831     C*
109000940831    >C* chiave uguale a quella passata al pgm
109100941010    >C**         MOVPRG    WHLT D1PRG
109200941010    >C**         D1PRG     ANDNE0
109300941010    >C*          MOVPRG    ORGT D1PRG
109400941010    >C*          D1PRG     ANDNE0
109500941010    >C*                    MOVEL*OFF      $RCDOK
109600941010    >C*          MOVPRG    IFGT D1PRG
109700941010    >C*          $ORD      ANDEQ99
109800941010    >C*          *HIVAL    SETLLxxANA99
109900941010     C*                    ENDIF
110000941010    >C*******
110100940316    >C* questo è lo schema generico da usare
110200941010    >C*          xxxxxx    WHLT D1xxx
110300941010    >C*          xxxxxx    ORGT xxx999
110400941010     C*                    MOVEL*OFF      $RCDOK
110500940314     C* nel caso che il dato letto sia > del limite superiore e
110600940314     C* questo dato è la chiave della VL in lettura, allora per
110700940314     C* evitare altri accessi inutili salto a fine file
110800941010    >C*          xxxxxx    IFGT xxx999
110900941010     C*          $ORD      ANDEQ1
111000941010    >C*          *HIVAL    SETLLxxxxx01
111100941010     C*                    ENDIF
111200940202     C*
111201151123>>>>>C                   WHEN      d1societa <> '*' AND d1societa <> absSoc
111202151123>>>>>C                   EVAL      $rcdOk = *OFF
111300940202     C                   ENDSL
111400940202     C*
111500940202     C                   ENDSR
111600940131     C/EJECT
111700940131     C************************************************************
111800940131     C* RIEMPIMENTO RIGA LISTA
111900940131     C************************************************************
112000940131     C     RIES1         BEGSR
112100940131     C*
112200940131     C                   CLEAR                   FMTS1
112300940314    >C* << riempio i campi del sfl, compresi gli hidden con i valori
112400940314    >C*    del rcd letto >>
112500940131     C*
112600941010    >C                   MOVEL     ABSGRP        S1GRP
112700941010    >C                   MOVEL     ABSPRF        S1PRF
112800941010    >C                   MOVEL     ABSSOC        S1SOC
112900940131     C*
113000940131     C                   ENDSR
113100940309     C/EJECT
113200940309     C************************************************************
113300940309     C* GESTIONE F03 VIDEO S1
113400940309     C************************************************************
113500940309     C     F03S1         BEGSR
113600940309     C*
113700940309     C                   MOVE      *ON           $FINE
113800940325     C* fine programma
113900940325     C                   EXSR      ENDPGM
114000940309     C*
114100940309     C                   ENDSR
114200940309     C/EJECT
114300940309     C************************************************************
114400940309     C* GESTIONE F10 VIDEO S1
114500940609     C* AGGIUNTA RECORD
114600940309     C************************************************************
114700940309     C     F10S1         BEGSR
114800940309     C*
114900941010    >C                   RESET                   A004DS
115000941010    >C                   MOVEL     '01'          OPZ004
115100941010    >C                   MOVEL     A004DS        KPJBU
115200941010    >C                   CALL      'ANA004R'
115300940309     C                   PARM                    KPJBA
115400941010    >C                   MOVEL     KPJBU         A004DS
115500940309     C* ritorno da PGM gestione
115600941010    >C                   EXSR      RTN004
115700940309     C     WINZS1        IFEQ      *ON
115800940309     C                   MOVE      *ON           $INZS1
115900940309     C* se esistevano già righe sul sfl
116000940309     C* calcolo pagina a cui deve posizionarsi
116100940309     C     WSFL          IFGT      0
116200940309     C                   EXSR      CLCPAG
116300940309     C* altrimenti carico solo la 1a pagina
116400940309     C                   ELSE
116500940309     C                   Z-ADD     SFLPAG        WPAG
116600940309     C                   ENDIF
116700940309     C                   ENDIF
116800940309     C*
116900940627     C                   MOVE      *HIVAL        $LASTV
117000940627     C*
117100940309     C                   ENDSR
117200940309     C/EJECT
117300940309     C************************************************************
117400940309     C* GESTIONE F12 VIDEO S1
117500940309     C************************************************************
117600940309     C     F12S1         BEGSR
117700940309     C*
117800940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
117900940309     C     $LOP          IFEQ      1
118000940309     C                   MOVE      'D1'          $GEST
118100940309     C*  se la 1a videata è questa, per F12 torno a menu
118200940309     C                   ELSE
118300940309     C                   MOVE      *ON           $FINE
118400940309     C                   END
118500940309     C*
118600940309     C                   ENDSR
118700940309     C/EJECT
118800940309     C************************************************************
118900940309     C* GESTIONE F17 VIDEO S1
119000940309     C************************************************************
119100940309     C     F17S1         BEGSR
119200940309     C*
119300940309     C                   MOVE      'D1'          $GEST
119400940831     C* forza certi valori nel caso il pgm sia stato richiamato con
119500940831     C* chiave specifica
119600941010<<<<>C     PRF003        IFNE      *BLANK
119700941010    >C                   MOVEL     *BLANK        PRF003
119800940831     C                   END
119900940831     C     $ORD          IFEQ      99
120000940831     C                   Z-ADD     1             $ORD
120100941124     C                   SETON                                        01
120200941124     C                   SETOFF                                       02
120300941010    >C*************        Z-ADDUDATE     D1DRG
120400940831     C                   END
120500940309     C*
120600940309     C                   ENDSR
120700940309     C/EJECT
120800940309     C************************************************************
120900940309     C* GESTIONE F18 VIDEO S1
121000940309     C************************************************************
121100940309     C     F18S1         BEGSR
121200940309     C*
121300940309     C                   MOVE      'D2'          $GEST
121400940309     C                   MOVEL     *ON           $INZD2
121500940831     C     $ORD          IFEQ      99
121600940831     C                   Z-ADD     1             $ORD
121700941124     C                   SETON                                        01
121800941124     C                   SETOFF                                       02
121900940831     C                   END
122000940309     C*
122100940309     C                   ENDSR
122200940309     C/EJECT
122300940309     C************************************************************
122400940309     C* GESTIONE F23 VIDEO S1
122500940309     C************************************************************
122600940309     C     F23S1         BEGSR
122700940309     C*
122800950403     C                   Z-ADD     76            $L01
122900950403     C                   Z-ADD     76            $L02
123000940309     C                   EXSR      RIEOPZ
123100950403     C                   Z-ADD     23            X
123200950403     C                   MOVE      FF23          $FC2(X)
123300940608     C                   MOVEL     WOPZ1         C1OPZ1
123400060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
123500940608     C                   MOVEL     WOPZ2         C1OPZ2
123600940309     C                   Z-ADD     WCOZ          C1COZ
123700940309     C*
123800940309     C                   ENDSR
123900940309     C/EJECT
124000940309     C************************************************************
124100940309     C* GESTIONE F24 VIDEO S1
124200940309     C************************************************************
124300940309     C     F24S1         BEGSR
124400940309     C*
124500940506     C*
124600940506     C                   MOVEA     $FX2          $FX
124700940506     C                   MOVEA     $FC2          $FC
124800940506     C                   MOVEA     $FR2          $FR
124900940506     C                   Z-ADD     $ULKS1        $ULK
125000940929     C                   Z-ADD     79            $L01
125100940929     C                   Z-ADD     79            $L02
125200940506     C                   EXSR      RIEKEY
125300940506     C                   Z-ADD     $ULK          $ULKS1            3 0
125400940720     C                   MOVEA     $FC           $FC2
125500940506     C                   MOVEL     $KEY1         Z2KE1
125600060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
125700940506     C                   MOVEL     $KEY2         Z2KE2
125800940506     C                   WRITE     FMTZ2
125900940506     C*
126000940309     C                   ENDSR
126100940128     C/EJECT
126200940128     C************************************************************
126300940131     C* CONTROLLO TESTATA LISTA
126400940128     C************************************************************
126500940131     C     CTRC1         BEGSR
126600940128     C*
126700940201     C                   MOVE      *OFF          *IN99
126800940131     C*
126900940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
127000940202     C* L'INIZIALIZZAZIONE LISTA
127100940202     C*
127200941010     C                   SELECT
127300941010     C     $ORD          WHENEQ    2
127400941010    >C     C1POS2        IFNE      W1POS2
127500941010     C                   EXSR      POSS1
127600941010     C                   END
127700941010      *
127800941010     C                   OTHER
127900941010      *
128000941010    >C     C1POS         IFNE      W1POS
128100941010     C                   EXSR      POSS1
128200941010     C                   END
128300941010     C                   ENDSL
128400941010     C*
128500940202     C*
128600940202     C                   ENDSR
128700940202     C/EJECT
128800940202     C************************************************************
128900940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
129000940202     C************************************************************
129100940202     C     POSS1         BEGSR
129200940202     C*
129300940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
129400940202     C*
1295009402021    C     WMAX          IFGT      0
129600940202     C                   READC     FMTS1                                  21
1297009402022    C     *IN21         DOWEQ     *OFF
129800940202     C     *IN99         ANDEQ     *OFF
129900940202     C*
1300009402023    C     S1OPZ         IFNE      0
130100940202     C                   SETON                                        51  99
1302009402023-   C                   ENDIF
130300940202     C* se rilevata opzione attiva, attivo sflnxtchg
1304009402023    C     *IN99         IFEQ      *ON
130500940202     C                   MOVE      *ON           *IN32
130600940202     C                   UPDATE    FMTS1
1307009402023E   C                   ELSE
130800940202     C                   READC     FMTS1                                  21
1309009402023-   C                   ENDIF
131000940202     C*
1311009402022-   C                   ENDDO
131200940202     C*
1313009402021-   C                   ENDIF
131400940202     C*
131500940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
131600940221     C* AL VALORE DI POSIZIONAMENTO :
131700940221     C* se questo è maggiore del valore del rispettivo campo di
131800940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
131900940221     C* con i valori di parzializzazione.
132000940221     C*
1321009402021    C     *IN99         IFEQ      *OFF
132200940721    >C                   MOVE      *BLANK        W1POS
132300941124    >C                   Z-ADD     0             W1POS2
132400940202     C*
1325009402022    C                   SELECT
132600940202     C*
132700940202     C     $ORD          WHENEQ    1
132800940202     C*
132900941010    >C     C1POS         IFGT      D1PRF
133000941124    >C                   MOVEL     C1POS         W1POS
133100940131     C                   END
133200940131     C*
133300940202     C                   MOVE      *ON           *IN99
133400940223     C                   MOVE      *ON           $INZS1
133500940831     C*
133600940831     C     $ORD          WHENEQ    99
133700940831     C*
133800940831     C**         C1POS     IFGT D1PRG
133900940831     C**                   MOVE C1POS     W1POS
134000940831     C**                   END
134100940831     C                   MOVE      *ON           *IN99
134200940831     C                   MOVE      *ON           $INZS1
134300940831     C*
134400940202     C*
134500940202     C     $ORD          WHENEQ    2
134600940202     C*
134700941010    >C     C1POS2        IFGT      D1GRP
134800941124    >C                   MOVE      C1POS2        W1POS2
134900940202     C                   END
135000940202     C*
135100940202     C                   MOVE      *ON           *IN99
135200940223     C                   MOVE      *ON           $INZS1
135300940202     C*
1354009402022-   C                   ENDSL
135500940202     C*
1356009402021-   C                   ENDIF
135700940202     C*
135800940131     C                   ENDSR
135900940131     C/EJECT
136000940131     C************************************************************
136100940131     C* CONTROLLO OPZIONI LISTA
136200940131     C************************************************************
136300940131     C     CTRS1         BEGSR
136400940131     C*
136500940202     C                   MOVEL     *OFF          $ESCI
136600940201     C                   SETOFF                                       99
136700940131     C                   Z-ADD     0             S1OPZ
136800940131     C*
136900940127     C* Leggo il sfl solo se ci sono rcd
1370009401311    C     WMAX          IFGT      0
137100940131     C                   READC     FMTS1                                  21
137200940127     C*
137300940131     C* esce se fine sfl o errore che richiede l'uscita
1374009401312    C     *IN21         DOWEQ     *OFF
137500940131     C     $ESCI         ANDEQ     *OFF
137600940201     C                   Z-ADD     S1NRR         C1RCD
137700940131     C* ctrl su riga
137800940131     C                   EXSR      RECS1
137900940131     C* gestione opzioni
1380009401313    C     S1OPZ         IFNE      0
138100940201     C     *IN99         ANDEQ     *OFF
138200940131     C                   EXSR      OPZS1
1383009401273-   C                   ENDIF
138400941125     C* se rilevato errore, attivo sflnxtchg
1385009402013    C     *IN99         IFEQ      *ON
138600940131     C                   MOVE      *ON           *IN32
1387009402233-   C                   ENDIF
138800940223     C*
138900940131     C                   Z-ADD     0             S1OPZ
139000940223     C*
139100940127     C                   UPDATE    FMTS1
139200941125     C                   MOVE      *OFF          *IN32
139300940223     C*
139400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1395009401313    C     $ESCI         IFEQ      *OFF
139600940131     C                   READC     FMTS1                                  21
139700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1398009402014    C     *IN21         IFEQ      *ON
139900940201     C                   MOVE      WINZS1        $INZS1
140000940204     C* calcolo pagina a cui deve posizionarsi
140100940728     C     WSFL          IFGT      0
140200940728     C                   EXSR      CLCPAG
140300940728     C                   ELSE
140400940728     C                   Z-ADD     SFLPAG        WPAG
140500940728     C                   ENDIF
1406009402014-   C                   ENDIF
1407009402013-   C                   ENDIF
140800940131     C*
1409009401272-   C                   ENDDO
141000940127     C*
1411009401311-   C                   ENDIF
141200940131     C*
141300940127     C                   ENDSR
141400940127     C/EJECT
141500940127     C************************************************************
141600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
141700940127     C************************************************************
141800940131     C     RECS1         BEGSR
141900940131     C*
142000940201     C* reset indicatori DSPATR
142100940201     C                   MOVE      *ALL'0'       IN4049           10
142200940201     C                   MOVEA     IN4049        *IN(40)
142300940201     C*
142400940131     C* Controllo se Utente Autorizzato alla scelta
142500940202     C     S1OPZ         IFNE      0
142600940131     C                   Z-ADD     1             X
142700940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
142800940211     C     *IN21         IFEQ      *OFF
142900940223     C     $FL1(X)       OREQ      'N'
143000940131     C                   MOVE      *ON           $ESCI
143100940201     C                   SETON                                        504099
143200940131     C                   ENDIF
143300940202     C                   ENDIF
143400940131     C*
143500940131     C                   ENDSR
143600940224     C/EJECT
143700940131     C************************************************************
143800940131     C* GESTIONE OPZIONI LISTA
143900940131     C************************************************************
144000940131     C     OPZS1         BEGSR
144100940201     C*
1442009402251    C                   SELECT
144300940225     C*
144400940225     C* se opz=Selezione passo la chiave al pgm chiamante
144500940225     C     S1OPZ         WHENEQ    1
144600941007    >C* << passare nei campi appositi della DS A003DS il valore
144700940314    >C*    della chiave della VL univoca (se c'è) >>
144800941010    >C                   MOVE      S1PRF         PRF003
144900941010    >C                   MOVE      S1SOC         SOC003
145000940225     C                   MOVE      *ON           $ESCI
145100940225     C                   MOVE      *ON           $FINE
145200940909     C*
145300940909     C                   MOVE      *HIVAL        $LASTV
145400940225     C*
145500941011     C     S1OPZ         WHENEQ    11
145600941011    >C                   RESET                   A005DS
145700941011    >C* << passare nei campi appositi della DS D004DS il valore
145800941011    >C*    della chiave della VL univoca (se c'è) >>
145900941011    >C                   MOVEL     S1OPZ         OPZ005
146000941011    >C                   MOVEL     S1SOC         SOC005
146100941011    >C                   MOVEL     S1PRF         PRF005
146200941011    >C                   MOVEL     S1GRP         GRP005
146300941011     C*
146400941011    >C                   MOVEL     A005DS        KPJBU
146500941011    >C                   CALL      'ANA005R'
146600941011     C                   PARM                    KPJBA
146700941011    >C                   MOVEL     KPJBU         A005DS
146800941011     C*
146900940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1470009402251O   C                   OTHER
147100940225     C*
147200941007    >C                   RESET                   A004DS
147300941010    >C* << passare nei campi appositi della DS D004DS il valore
147400940314    >C*    della chiave della VL univoca (se c'è) >>
147500941010    >C                   MOVEL     S1OPZ         OPZ004
147600941010    >C                   MOVEL     S1SOC         SOC004
147700941010    >C                   MOVEL     S1PRF         PRF004
147800941010    >C                   MOVEL     S1GRP         GRP004
147900940314     C*
148000941007    >C                   MOVEL     A004DS        KPJBU
148100941007    >C                   CALL      'ANA004R'
148200940314     C                   PARM                    KPJBA
148300941007    >C                   MOVEL     KPJBU         A004DS
148400940201     C*
148500940223     C* ritorno da PGM gestione
148600941007    >C                   EXSR      RTN004
148700940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1488009402252    C     *IN99         IFEQ      *ON
148900940223     C                   SETON                                        40
1490009402252-   C                   ENDIF
149100940131     C*
149200940506     C                   MOVE      *HIVAL        $LASTV
1493009402251-   C                   ENDSL
149400940225     C*
149500940131     C                   ENDSR
149600940223     C/EJECT
149700940223     C************************************************************
149800940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
149900940223     C************************************************************
150000941007    >C     RTN004        BEGSR
150100940223     C*
150200940223     C* modo di ritorno
150300940223     C*
1504009402231    C                   SELECT
150500940314    >C* << questi modi di utilizzo dei valori di ritorno dal
150600940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
150700940314    >C*    proposte, normalmente sempre valide, ma modificabili
150800940314    >C*    per situazioni particolari >>
150900940223     C* 1 = F3
151000941007    >C     RET004        WHENEQ    '1'
151100940224     C                   MOVE      *ON           $ESCI                          sospese READC
151200940223     C                   MOVE      *ON           $FINE                          fine pgm
151300940223     C* 2 = F12
151400941007    >C     RET004        WHENEQ    '2'
151500940223     C                   MOVE      *ON           $ESCI                          sospese READC
151600940223     C*
1517009402231-   C                   ENDSL
151800940223     C*
151900940223     C* operazione eseguite dal pgm chiamato
152000940223     C*
1521009402231    C                   SELECT
152200940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
152300941007    >C     OPR004        WHENEQ    '1'
152400940223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
152500940223     C*
1526009402231-   C                   ENDSL
152700940223     C*
152800940223     C* funzione non eseguibile per errore :
152900940223     C*
1530009402231    C                   SELECT
153100940223     C* 1 = funzione richiamata chiusa in errore
153200940316    >C*  eventualmente gestire altri codici di errore
153300941007    >C     ERR004        WHENEQ    '1'
153400940223     C                   MOVE      *ON           $ESCI                          sospese READC
153500940223     C                   SETON                                        5299      emiss.SFLMSGID
153600940223     C*
1537009402231-   C                   ENDSL
153800940223     C*
153900940223     C                   ENDSR
154000940223     C/EJECT
154100940131     C************************************************************
154200940131     C* GESTIONE VALORI ASSUNTI
154300940131     C************************************************************
154400940131     C     GESD2         BEGSR
154500940131     C*
154600940223     C     $INZD2        IFEQ      *ON
154700940131     C                   EXSR      INZD2
154800940223     C                   MOVE      *OFF          $INZD2
154900940131     C                   END
155000940202     C*
155100940202     C     $LASTV        IFNE      'D2'
155200940923     C                   EXSR      GESPIE
155300940202     C                   MOVE      $LASTV        $LASTG
155400940202     C                   MOVE      'D2'          $LASTV
155500940707     C                   ENDIF
155600940131     C*
155700940707     C* se esistono errori sulla videata
155800940707     C* emetto la write del formato a indicatori spenti per vedere
155900940707     C* le eventuali decodifiche
156000940707     C     *IN99         IFEQ      *ON
156100940707     C                   MOVEA     *IN           WIN
156200940707     C                   MOVE      *ALL'0'       IN5098           49
156300940707     C                   MOVEA     IN5098        *IN(50)
156400940707     C                   WRITE     FMTD2
156500940707     C                   MOVEA     WIN           *IN
156600940707     C                   ENDIF
156700940707     C*
156800940131     C                   EXFMT     FMTD2
156900940131     C*
157000940131     C                   SELECT
157100940131     C* F3=Fine
157200940506     C     $TASTO        WHENEQ    F03
157300940309     C                   EXSR      F03D2
157400940131     C* F12=Ritorno
157500940506     C     $TASTO        WHENEQ    F12
157600940309     C                   EXSR      F12D2
157700940916     C* F24=Altri tasti
157800940916     C     $TASTO        WHENEQ    F24
157900940916     C                   EXSR      F24D2
158000940131     C*
158100940131     C                   OTHER
158200940131     C                   EXSR      CTRD2
1583009401312    C     *IN99         IFEQ      *OFF
158400940211     C                   MOVE      $LASTG        $GEST
1585009401313    C                   SELECT
158600940202     C     $LASTG        WHENEQ    'S1'
158700940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
158800940314     C     $ORD          IFNE      D2ORD
158900940223     C                   MOVE      *ON           $INZS1
159000940302     C                   MOVE      *BLANK        W1POS
159100941124     C                   Z-ADD     0             W1POS2
159200940131     C                   END
159300940202     C     $LASTG        WHENEQ    'D1'
159400940318     C* per ora niente
1595009401313-   C                   ENDSL
159600940131     C* definisco i nuovi valori di default
159700940201     C                   Z-ADD     D2LOP         $LOP
159800940131     C                   Z-ADD     D2ORD         $ORD
159900941124     C                   SELECT
160000941124     C     $ORD          WHENEQ    1
160100941124     C                   SETON                                            01
160200941124     C                   SETOFF                                           02
160300941124     C     $ORD          WHENEQ    2
160400941124     C                   SETON                                            02
160500941124     C                   SETOFF                                           01
160600941124     C                   ENDSL
160700940609    >C* << elenco altri dati default >>
1608009401312-   C                   END
160900940131     C*
161000940131     C                   ENDSL
161100940131     C*
161200940131     C                   ENDSR
161300940223     C/EJECT
161400940131     C************************************************************
161500940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
161600940131     C************************************************************
161700940201     C     INZD2         BEGSR
161800940131     C*
161900940131     C                   CLEAR                   FMTD2
162000940201     C                   MOVEL     $LOP          D2LOP
162100940831     C                   MOVE      $ORD          D2ORD
162200940314    >C* << elenco altri dati default >>
162300940131     C*
162400940131     C                   ENDSR
162500940309     C/EJECT
162600940309     C************************************************************
162700940309     C* GESTIONE F03 VIDEO D2
162800940309     C************************************************************
162900940309     C     F03D2         BEGSR
163000940309     C*
163100940309     C                   MOVE      *ON           $FINE
163200940325     C* fine programma
163300940325     C                   EXSR      ENDPGM
163400940309     C*
163500940309     C                   ENDSR
163600940309     C/EJECT
163700940309     C************************************************************
163800940309     C* GESTIONE F12 VIDEO D2
163900940309     C************************************************************
164000940309     C     F12D2         BEGSR
164100940309     C*
164200940309     C                   MOVE      $LASTG        $GEST
164300940309     C*
164400940309     C                   ENDSR
164500940916     C/EJECT
164600940916     C************************************************************
164700940916     C* GESTIONE F24 VIDEO D2
164800940916     C************************************************************
164900940916     C     F24D2         BEGSR
165000940916     C*
165100940916     C                   MOVEA     $FX3          $FX
165200940916     C                   MOVEA     $FC3          $FC
165300940916     C                   MOVEA     $FR3          $FR
165400940916     C                   Z-ADD     $ULKD2        $ULK
165500940929     C                   Z-ADD     56            $L01
165600940929     C                   Z-ADD     56            $L02
165700940916     C                   EXSR      RIEKEY
165800940916     C                   Z-ADD     $ULK          $ULKD2            3 0
165900940916     C                   MOVEA     $FC           $FC3
166000940916     C                   MOVEL     $KEY1         Z3KE1
166100060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
166200940916     C                   MOVEL     $KEY2         Z3KE2
166300940916     C                   WRITE     FMTZ3
166400940916     C*
166500940916     C                   ENDSR
166600940223     C/EJECT
166700940131     C************************************************************
166800940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
166900940131     C************************************************************
167000940131     C     CTRD2         BEGSR
167100940131     C*
167200940131     C                   SETOFF                                       99
167300940131     C* non imposto pos. del cursore
167400940131     C                   Z-ADD     0             H1RIGA
167500940131     C                   Z-ADD     0             H1COLO
167600940131     C*
167700940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
167800940201     C     D2LOP         IFEQ      0
167900940131     C                   SETON                                          5099
168000940131     C                   END
168100940131     C*
168200940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
168300940131     C     D2ORD         IFEQ      0
168400940131     C                   SETON                                          5199
168500940131     C                   END
168600940314    >C* << elenco controlli altri dati default >>
168700940131     C*
168800940131     C                   ENDSR
168900940223     C/EJECT
169000940201     C************************************************************
169100940201     C* AGGIORNAMENTO VALORI ASSUNTI
169200940201     C************************************************************
169300940201     C     AGGMPE        BEGSR
169400940201     C*
169500940201     C                   CLEAR                   ANMPE000
169600940201     C                   MOVEL     XSCSOC        MPESOC
169700940201     C                   MOVEL     DSPGM         MPEPGM
169800940201     C                   MOVEL     KNMUS         MPEPRF
169900941013     C*
170000941013     C                   SELECT
170100941013     C     OPZ003        WHENEQ    '01'
170200941013     C                   MOVEL     'PX'          MPETIP
170300941013     C                   OTHER
170400941013     C                   MOVEL     'PV'          MPETIP
170500941013     C                   ENDSL
170600940201     C     K4MPE2        CHAIN     ANMPE02L                           21
170700940201     C                   MOVEL     DSMPE         MPEPAR
170800940201     C     *IN21         IFEQ      *ON
170900940201     C                   WRITE     ANMPE000
171000940201     C                   ELSE
171100940201     C                   UPDATE    ANMPE000
171200940201     C                   END
171300940201     C*
171400940201     C                   ENDSR
171500940223     C/EJECT
171600940131     C************************************************************
171700940131     C* OPERAZIONI INIZIALI
171800940131     C************************************************************
171900940131     C     *INZSR        BEGSR
172000940131     C*
172100940127     C* Reperimento parametri
172200940127     C*
172300940117     C     *ENTRY        PLIST
172400940117     C                   PARM                    KPJBA
172500941103      *
172600941103      * Reperimento dati società
172700941103      *
172800941103     C                   MOVEL     'SOC001'      TIPXSC
172900941103     C                   MOVEL     *BLANK        SOCXSC
173000941103     C                   EXSR      REPSOC
173100941103     C     RTNXSC        IFNE      '1'
173200941103     C                   MOVEL     XSOCDS        SOC001
173300941103     C                   ELSE
173400941111     C                   RETURN
173500941103     C                   ENDIF
173600940314     C* Reperimento autorizzazioni
173700940314     C*
173800940314    >C* << nel caso l'anagrafica non gestisse le autorizzazioni
173900940909    >C*    asteriscare la chiamata alla subroutine REPAUT >>
174000941007<<<<>C*                    EXSR REPAUT
174100940225     C*
174200940223     C* Reperimento opzioni
174300940127     C*
174400940128     C                   EXSR      REPOPZ
174500940506     C*
174600940506     C* Reperimento tasti di funzione
174700940506     C*
174800940506     C                   EXSR      REPKEY
174900940506     C*
175000940127     C*
175100940117     C                   ENDSR
175200940225     C/EJECT
175300940225     C************************************************************
175400940225     C* REPERIMENTO AUTORIZZAZIONI UTENTE
175500940225     C************************************************************
175600941007     C*          REPAUT    BEGSR
175700940225     C*
175800941007     C*                    CLEARXAUT
175900941007     C*
176000941007     C*                    Z-ADD0         XCAGRP           GRUPPO
176100941007     C*                    MOVELKNMUS     XCAPRF           PROFILO
176200941007    >C*                    MOVEL'OBJALL'  XCAFNC           FUNZIONE
176300941007     C*                    MOVEL*ZERO     XCAVFU           VARIABILE
176400941007     C*                    MOVE 'CK'      XCATCK           TIPO CTRL
176500941007     C*
176600941007     C*                    CALL 'XCHKAUT'
176700941007     C*                    PARM           XAUT
176800941007     C* utente non abilitato
176900941007     C*          XCARTN    IFEQ 0
177000941007    >C*                    MOVELXCADDS    AUTDS
177100941007     C*                    ELSE
177200941007    >C*                    CLEARAUTDS
177300941007     C*                    END
177400941007     C*
177500941007     C*                    ENDSR
177600940223     C/EJECT
177700940223     C************************************************************
177800940223     C* INIZIALIZZAZIONE VARIABILI
177900940223     C************************************************************
178000940223     C     INZVAR        BEGSR
178100940223     C*
178200940729     C*
178300940729     C* Dati da PGM chiamante
178400940729     C*
178500941007    >C                   MOVEL     KPJBU         A003DS
178600941026     C*
178700941026     C* Reperimento valori default utente
178800941026     C*
178900941026     C                   EXSR      REPDFT
179000940506     C*
179100940506     C* Definizione delle opzioni utilizzabili a pgm
179200940506     C*
179300940506     C                   EXSR      DEFOPZ
179400940223     C* Variabili per gestione videate
179500940223     C*
179600940223     C                   MOVE      *OFF          $FINE                          fine pgm
179700940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
179800940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
179900940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
180000940223     C                   MOVE      *OFF          $EFILE                         memo fine file
180100940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
180200940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
180300940509     C                   MOVE      *HIVAL        $LASTV
180400940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
180500940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
180600940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
180700940223     C*
180800940318     C                   MOVE      *ON           $INZD1
180900940318     C                   MOVE      *ON           $INZS1
181000940831     C*
181100940223     C                   SELECT
181200940223     C     $LOP          WHENEQ    1
181300940223     C                   MOVE      'D1'          $GEST
181400940223     C     $LOP          WHENEQ    2
181500940223     C                   MOVE      'S1'          $GEST
181600941104     C                   EXSR      INZD1
181700941104     C                   MOVE      *OFF          $INZD1
181800940223     C                   ENDSL
181900940223     C*
182000940223     C* Variabili appoggio
182100940223     C*
182200940223     C                   Z-ADD     SFLPAG        WPAG
182300940223     C*
182400940223     C* Valorizzazione campi univoci testate
182500940223     C*
182600940223     C                   CLEAR                   FMTT1
182700940223     C                   MOVEL     KNSIF         NOMSIF
182800941110     C                   MOVEL     XSCDSI        NOMDIT
182900941027     C                   MOVEL     DSPGM         NOMPGM
183000941011     C* Accendo indicatori per condizioni su campo posizionamento C1POS
183100941011     C                   SELECT
183200941011     C     $ORD          WHENEQ    1
183300941011     C                   SETON                                            01
183400941124     C                   SETOFF                                           02
183500941011     C     $ORD          WHENEQ    2
183600941011     C                   SETON                                            02
183700941124     C                   SETOFF                                           01
183800941011     C                   ENDSL
183900940223     C*
184000940223     C                   ENDSR
184100940223     C/EJECT
184200940127     C************************************************************
184300940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
184400940127     C************************************************************
184500940131     C     REPDFT        BEGSR
184600940127     C*
184700940201     C                   MOVEL     XSCSOC        MPESOC
184800940201     C                   MOVEL     DSPGM         MPEPGM
184900940131     C                   MOVEL     KNMUS         MPEPRF
185000941013     C*
185100941013     C                   SELECT
185200941013     C     OPZ003        WHENEQ    '01'
185300941013     C                   MOVEL     'PX'          MPETIP
185400941013     C                   OTHER
185500941013     C                   MOVEL     'PV'          MPETIP
185600941013     C                   ENDSL
185700940202     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
185800940131     C     *IN21         IFEQ      *OFF
185900940131     C                   MOVEL     MPEPAR        DSMPE
186000940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
186100940131     C* i suoi valori di default vengono decisi a pgm
186200940131     C                   ELSE
186300940201     C                   Z-ADD     1             $LOP
186400940131     C                   Z-ADD     1             $ORD
186500941010    >C* << elenco altri dati default >>
186600940131     C                   END
186700940131     C*
186800940131     C                   ENDSR
186900940223     C/EJECT
187000940131     C************************************************************
187100940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
187200940131     C************************************************************
187300940225     C     DEFOPZ        BEGSR
187400940131     C*
187500940225     C* Questa routine deve valorizzare la schiera $FL1
187600940225     C* con S o N a seconda della utilizzabilità o meno
187700940225     C* dell'opzione correlata
187800940225     C*
187900940225     C* Inizialmente si considerano tutte le opzioni come valide
188000940317     C     1             DO        $DIMOP        X
188100940225     C                   MOVE      'S'           $FL1(X)
188200940225     C                   END
188300940715     C*
188400940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
188500940715     C* con S o N a seconda della utilizzabilità o meno
188600940715     C* del tasto funzione correlato
188700940715     C*
188800940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
188900940715     C* di compilazione
189000940715     C                   RESET                   $FC1
189100940715     C                   RESET                   $FR1
189200940715     C                   RESET                   $FC2
189300940715     C                   RESET                   $FR2
189400940919     C                   RESET                   $FC3
189500940919     C                   RESET                   $FR3
189600940225     C*
189700940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
189800940225     C* invalido alcune opzioni
189900940225     C                   SELECT
190000940225     C* Se pgm richiamato in ricerca disattivo :
190100941007    >C     OPZ003        WHENEQ    '01'
190200940307     C* F10=Immissione
190300940506     C                   Z-ADD     10            X
190400940720     C                   MOVE      '0'           $FC2(X)
190500940603     C* F3=Fine
190600940603     C                   Z-ADD     3             X
190700940720     C                   MOVE      '0'           $FC2(X)
190800940307     C* 2=Modifica
190900940307     C                   Z-ADD     1             X
191000940225     C     02            LOOKUP    $OP1(X)                                21
191100940225     C     *IN21         IFEQ      *ON
191200940225     C                   MOVE      'N'           $FL1(X)
191300940225     C                   ENDIF
191400940225     C* 3=Copia
191500940225     C                   Z-ADD     1             X
191600940225     C     03            LOOKUP    $OP1(X)                                21
191700940225     C     *IN21         IFEQ      *ON
191800940225     C                   MOVE      'N'           $FL1(X)
191900940225     C                   ENDIF
192000940225     C* 4=Annullamento
192100940225     C                   Z-ADD     1             X
192200940225     C     04            LOOKUP    $OP1(X)                                21
192300940225     C     *IN21         IFEQ      *ON
192400940225     C                   MOVE      'N'           $FL1(X)
192500940225     C                   ENDIF
192600940314    >C* << aggiungere nollo stesso modo le altre opzioni da
192700940314    >C*    disattivare (quelle specifiche per ogni pgm) >>
192800940225     C* Se pgm richiamato in interrogazione disattivo :
192900941007    >C     OPZ003        WHENEQ    '05'
193000940307     C* F10=Immissione
193100940615     C                   Z-ADD     10            X
193200940720     C                   MOVE      '0'           $FC2(X)
193300940225     C* 1=Selezione
193400940225     C                   Z-ADD     1             X
193500940225     C     01            LOOKUP    $OP1(X)                                21
193600940225     C     *IN21         IFEQ      *ON
193700940225     C                   MOVE      'N'           $FL1(X)
193800940225     C                   ENDIF
193900940225     C* 2=Modifica
194000940225     C                   Z-ADD     1             X
194100940225     C     02            LOOKUP    $OP1(X)                                21
194200940225     C     *IN21         IFEQ      *ON
194300940225     C                   MOVE      'N'           $FL1(X)
194400940225     C                   ENDIF
194500940225     C* 3=Copia
194600940225     C                   Z-ADD     1             X
194700940225     C     03            LOOKUP    $OP1(X)                                21
194800940225     C     *IN21         IFEQ      *ON
194900940225     C                   MOVE      'N'           $FL1(X)
195000940225     C                   ENDIF
195100940225     C* 4=Annullamento
195200940225     C                   Z-ADD     1             X
195300940225     C     04            LOOKUP    $OP1(X)                                21
195400940225     C     *IN21         IFEQ      *ON
195500940225     C                   MOVE      'N'           $FL1(X)
195600940225     C                   ENDIF
195700940225     C*
195800940225     C                   OTHER
195900940225     C* Se pgm non richiamato, disattivo subito :
196000940225     C* 1=Selezione
196100940225     C                   Z-ADD     1             X
196200940225     C     01            LOOKUP    $OP1(X)                                21
196300940225     C     *IN21         IFEQ      *ON
196400940225     C                   MOVE      'N'           $FL1(X)
196500940225     C                   ENDIF
196600940225     C* poi, disattivo a seconda delle autorizzazioni :
196700940314    >C*********  AUTXXX    IFEQ 'N'
196800940314    >C*********            Z-ADD1         X
196900940314    >C*********  'XX'      LOKUP$OP1,X                   21
197000940314    >C*********  *IN21     IFEQ *ON
197100940314    >C*********            MOVE 'N'       $FL1,X
197200940314    >C*********            ENDIF
197300940314    >C*********            ENDIF
197400940225     C*
197500940225     C                   ENDSL
197600940225     C*
197700940225     C                   ENDSR
197800940225     C/EJECT
197900940225     C************************************************************
198000940225     C* REPERIMENTO OPZIONI
198100940225     C************************************************************
198200940225     C     REPOPZ        BEGSR
198300940225     C*
198400940314     C     1             DO        $DIMOP        X
198500940128     C*
198600940223     C     $MS1(X)       IFNE      *BLANK
198700940128     C*
198800940223     C                   MOVEL     $MS1(X)       IDMSG
198900940128     C                   CALL      'XRTVM'
199000940201     C                   PARM                    IDMSG            27
199100940201     C                   PARM                    TXTMSG          100
199200940201     C                   PARM                    ERRMSG            1
199300940128     C*
199400940128     C     ERRMSG        IFNE      *ON
199500940223     C                   MOVEL     TXTMSG        $TX1(X)
199600940128     C                   ELSE
199700940223     C                   MOVEL     *ALL'?'       $TX1(X)
199800940128     C                   END
199900940128     C*
200000940128     C                   END
200100940128     C*
200200940128     C                   END
200300940128     C*
200400940128     C                   ENDSR
200500940506     C/EJECT
200600940506     C************************************************************
200700940506     C* REPERIMENTO TASTI DI FUNZIONE
200800940506     C************************************************************
200900940506     C     REPKEY        BEGSR
201000940506     C                   MOVEA     $FM1          $FM
201100950906     C                   MOVEA     $FC1          $FC
201200940506     C                   EXSR      REPKKK
201300940506     C                   MOVEA     $FX           $FX1
201400940506     C*
201500940506     C                   MOVEA     $FM2          $FM
201600950906     C                   MOVEA     $FC2          $FC
201700940506     C                   EXSR      REPKKK
201800940506     C                   MOVEA     $FX           $FX2
201900940919     C*
202000940919     C                   MOVEA     $FM3          $FM
202100950906     C                   MOVEA     $FC3          $FC
202200940919     C                   EXSR      REPKKK
202300940919     C                   MOVEA     $FX           $FX3
202400940919     C                   ENDSR
202500940506     C/EJECT
202600940506     C************************************************************
202700940506     C* REPERIMENTO TASTI DI FUNZIONE
202800940506     C************************************************************
202900940506     C     REPKKK        BEGSR
203000940506     C*
203100940506     C                   CLEAR                   $FX
203200940506     C     1             DO        24            X
203300940506     C*
203400940506     C     $FM(X)        IFNE      *BLANK
203500950906     C     $FC(X)        ANDNE     '0'
203600940506     C*
203700940506     C                   MOVEL     $FM(X)        IDMSG
203800940506     C                   CALL      'XRTVM'
203900940506     C                   PARM                    IDMSG            27
204000940506     C                   PARM                    TXTMSG          100
204100940506     C                   PARM                    ERRMSG            1
204200940506     C*
204300940506     C     ERRMSG        IFNE      *ON
204400940506     C                   MOVEL     TXTMSG        $FX(X)
204500940506     C                   ELSE
204600940506     C                   MOVEL     *ALL' '       $FX(X)
204700940506     C                   END
204800940506     C*
204900940506     C                   END
205000940506     C*
205100940506     C                   END
205200940506     C*
205300940506     C                   ENDSR
205400940223     C/EJECT
205500940128     C************************************************************
205600940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
205700940128     C************************************************************
205800940223     C     DEFCAM        BEGSR
205900940128     C*
206000940223     C* klist
206100940127     C*
206200941007    >C* ANABS02L
206300950302    >C     K2ABS2        KLIST
206400941007    >C                   KFLD                    ABSGRP
206500941007    >C                   KFLD                    ABSPRF
206600940128     C*
206700940128     C* ANMPE02L
206800940128     C     K4MPE2        KLIST
206900940128     C                   KFLD                    MPESOC
207000940201     C                   KFLD                    MPEPGM
207100940128     C                   KFLD                    MPETIP
207200940128     C                   KFLD                    MPEPRF
207300940223     C*
207400940223     C* Variabili per gestione videate
207500940223     C*
207600940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
207700940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
207800940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
207900940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
208000940223     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
208100940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
208200940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
208300940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
208400940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
208500940223     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
208600940224     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
208700940224     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
208800940223     C*
208900940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
209000940223     C*
209100940223     C     *LIKE         DEFINE    C1RCD         S1NRR
209200940223     C     *LIKE         DEFINE    C1NRR         WSFL
209300940223     C     *LIKE         DEFINE    C1RCD         WMAX
209400940223     C     *LIKE         DEFINE    C1RCD         WPAG
209500940223     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
209600940608     C     *LIKE         DEFINE    *INKR         WINKR                                      ne S1
209700940223     C                   MOVE      *BLANK        WWGRP             3
209800940223     C                   Z-ADD     0             CURR              2 0
209900940223     C                   Z-ADD     0             CURC              2 0
210000940223     C                   MOVE      *BLANK        WIN              99
210100940314     C     *LIKE         DEFINE    C1POS         W1POS
210200941010     C     *LIKE         DEFINE    C1POS2        W1POS2
210300940314     C*
210400940314     C* Variabili appoggio specifiche della singola anagrafica
210500940314     C*
210600940314     C* campi per gestione F4=Ricerca
210700941010    >C**         *LIKE     DEFN ANAxxx    WWxxx
210800941010    >C***        *LIKE     DEFN ANAMOD    WWMOD
210900940223     C* campi per parzializzazioni su rcd (solo alfanumerici)
211000941010    >C     *LIKE         DEFINE    ABSPRF        PRF999
211100941010    >C*****      *LIKE     DEFN ANAOBJ    OBJ999
211200940223     C*
211300940223     C* Indici
211400940223     C*
211500940223     C                   Z-ADD     0             X                 3 0
211600940223     C                   Z-ADD     0             Y                 3 0
211700940127     C*
211800940117     C                   ENDSR
211900941103     C/EJECT
212000941103      ************************************************************
212100941103      * Reperimento dati società
212200941103      ************************************************************
212300941103     C     REPSOC        BEGSR
212400941103      *
212500941103     C                   CALL      'XSOC'
212600941103     C                   PARM                    TIPXSC            6
212700941103     C                   PARM                    SOCXSC            3
212800941103     C                   PARM                    CDSXSC            9 0
212900941103     C                   PARM                    MODXSC            3
213000941103     C                   PARM      *BLANKS       RTNXSC            1
213100941103     C                   PARM                    XSOCDS
213200941103     C                   PARM                    KPJBA
213300941103      *
213400941103     C                   ENDSR
213500950906**
213600940131PROMSG    *LIBL     OPZ0004
213700941011PROMSG    *LIBL     OPZ0027
213800940201**
21390094022504S      <Annullamento
21400094101111S      <Abilitazioni Unità
214100940506** TASTI DI FUNZIONE  D1
214200940506PROMSG    *LIBL     KEY0001  01
214300940506PROMSG    *LIBL     KEY0002  02
214400940506PROMSG    *LIBL     KEY0003  03
214500940506PROMSG    *LIBL     KEY0004  04
214600940506PROMSG    *LIBL     KEY0005  05
214700940506PROMSG    *LIBL     KEY0006  06
214800940506PROMSG    *LIBL     KEY0007  07
214900940506PROMSG    *LIBL     KEY0008  08
215000940506PROMSG    *LIBL     KEY0009  09
215100940506PROMSG    *LIBL     KEY0010  10
215200940506PROMSG    *LIBL     KEY0011  11
215300940506PROMSG    *LIBL     KEY0012  12
215400940506PROMSG    *LIBL     KEY0013  13
215500940506PROMSG    *LIBL     KEY0014  14
215600940506PROMSG    *LIBL     KEY0015  15
215700940506PROMSG    *LIBL     KEY0016  16
215800940506PROMSG    *LIBL     KEY0017  17
215900940506PROMSG    *LIBL     KEY0018  18
216000940506PROMSG    *LIBL     KEY0019  19
216100940506PROMSG    *LIBL     KEY0020  20
216200940506PROMSG    *LIBL     KEY0021  21
216300940506PROMSG    *LIBL     KEY0022  22
216400940506PROMSG    *LIBL     KEY0023  23
216500940506PROMSG    *LIBL     KEY0024  24
216600940506**  ABILITAZIONE D1
2167009407200N  01
2168009407200N  02
2169009407201N  03
2170009407201N  04
2171009407200N  05
2172009407200N  06
2173009407200N  07
2174009407200N  08
2175009407200N  09
2176009407200N  10
2177009407200N  11
2178009407201N  12
2179009407200N  13
2180009407200N  14
2181009407200N  15
2182009407200N  16
2183009407200N  17
2184009407201N  18
2185009407200N  19
2186009407200N  20
2187009407200N  21
2188009407200N  22
2189009509060N  23   Impostare a '1' se si utilizzano opzioni
2190009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
219100940916** TASTI DI FUNZIONE  S1
219200940916PROMSG    *LIBL     KEY0001  01
219300940916PROMSG    *LIBL     KEY0002  02
219400940916PROMSG    *LIBL     KEY0003  03
219500940916PROMSG    *LIBL     KEY0004  04
219600940916PROMSG    *LIBL     KEY0005  05
219700940916PROMSG    *LIBL     KEY0006  06
219800940916PROMSG    *LIBL     KEY0007  07
219900940916PROMSG    *LIBL     KEY0008  08
220000940916PROMSG    *LIBL     KEY0009  09
220100940916PROMSG    *LIBL     KEY0010  10
220200940916PROMSG    *LIBL     KEY0011  11
220300940916PROMSG    *LIBL     KEY0012  12
220400940916PROMSG    *LIBL     KEY0013  13
220500940916PROMSG    *LIBL     KEY0014  14
220600940916PROMSG    *LIBL     KEY0015  15
220700940916PROMSG    *LIBL     KEY0016  16
220800940916PROMSG    *LIBL     KEY0017  17
220900940916PROMSG    *LIBL     KEY0018  18
221000940916PROMSG    *LIBL     KEY0019  19
221100940916PROMSG    *LIBL     KEY0020  20
221200940916PROMSG    *LIBL     KEY0021  21
221300940916PROMSG    *LIBL     KEY0022  22
221400940916PROMSG    *LIBL     KEY0023  23
221500940916PROMSG    *LIBL     KEY0024  24
221600940916**  ABILITAZIONE S1
2217009409160N  01
2218009409160N  02
2219009409161N  03
2220009409160N  04
2221009409160N  05
2222009409160N  06
2223009409160N  07
2224009409160N  08
2225009409160N  09
2226009409161N  10
2227009409160N  11
2228009409161N  12
2229009409160N  13
2230009409160N  14
2231009409160N  15
2232009409160N  16
2233009409161N  17
2234009409161N  18
2235009409160N  19
2236009409160N  20
2237009409160N  21
2238009409160N  22
2239009509061N  23   Impostare a '1' se si utilizzano opzioni
2240009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
224100940916** TASTI DI FUNZIONE  D3
224200940506PROMSG    *LIBL     KEY0001  01
224300940506PROMSG    *LIBL     KEY0002  02
224400940506PROMSG    *LIBL     KEY0003  03
224500940506PROMSG    *LIBL     KEY0004  04
224600940506PROMSG    *LIBL     KEY0005  05
224700940506PROMSG    *LIBL     KEY0006  06
224800940506PROMSG    *LIBL     KEY0007  07
224900940506PROMSG    *LIBL     KEY0008  08
225000940506PROMSG    *LIBL     KEY0009  09
225100940506PROMSG    *LIBL     KEY0010  10
225200940506PROMSG    *LIBL     KEY0011  11
225300940506PROMSG    *LIBL     KEY0012  12
225400940506PROMSG    *LIBL     KEY0013  13
225500940506PROMSG    *LIBL     KEY0014  14
225600940506PROMSG    *LIBL     KEY0015  15
225700940506PROMSG    *LIBL     KEY0016  16
225800940506PROMSG    *LIBL     KEY0017  17
225900940506PROMSG    *LIBL     KEY0018  18
226000940506PROMSG    *LIBL     KEY0019  19
226100940506PROMSG    *LIBL     KEY0020  20
226200940506PROMSG    *LIBL     KEY0021  21
226300940506PROMSG    *LIBL     KEY0022  22
226400940506PROMSG    *LIBL     KEY0023  23
226500940506PROMSG    *LIBL     KEY0024  24
226600940916**  ABILITAZIONE D3
2267009407200N  01
2268009407200N  02
2269009407201N  03
2270009407200N  04
2271009407200N  05
2272009407200N  06
2273009407200N  07
2274009407200N  08
2275009407200N  09
2276009409160N  10
2277009407200N  11
2278009407201N  12
2279009407200N  13
2280009407200N  14
2281009407200N  15
2282009407200N  16
2283009409160N  17
2284009409160N  18
2285009407200N  19
2286009407200N  20
2287009407200N  21
2288009407200N  22
2289009509060N  23   Impostare a '1' se si utilizzano opzioni
2290009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
