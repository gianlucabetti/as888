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
000000060224A1148C                   PARM                    $ALLOPT         960
000000060224A1148C*
036200940131     C                   ENDSR
036300940506     C/EJECT
036400940506     C************************************************************
036500940506     C* RIEMPIMENTO TASTI FUNZIONE
036600940506     C************************************************************
036700940506     C     RIEKEY        BEGSR
036800940506     C*
036900940506     C* Input :
037000940506     C* - $FX = schiera con tutti i testi disponibili
037100940506     C* - $FC = schiere con i flag di validità associati ai testi
037200940506     C* - $FR = schiere con i flag di REVERSE IMAGE
037300940506     C* - $ULK = ultimo testo caricato
037400940721     C* - $L01 = Lunghezza prima riga
037500940721     C* - $L02 = Lunghezza seconda riga
037600940506     C* Output :
037700940506     C* - $ULK
037800940609     C* - $KEY1= 1° riga di tasti funzione
037900940609     C* - $KEY2= 2° riga di tasti funzione
038000940506     C*
038100940506     C                   CALL      'XKEYMSG'
038200940506     C* input
038300940506     C                   PARM                    $FX
038400940506     C                   PARM                    $FC
038500940506     C                   PARM                    $FR
038600940506     C                   PARM                    $ULK              3 0
038700940506     C* output
038800940506     C                   PARM                    $KEY1            79
038900940506     C                   PARM                    $KEY2            79
039000940721    >C* input
039100940721    >C                   PARM                    $L01              3 0
039200940721    >C                   PARM                    $L02              3 0
039300940506     C*
000000060224A1148C                   PARM                    $ALLFUNCT       480
000000060224A1148C*
039400940506     C                   ENDSR
039500940224     C/EJECT
039600940224     C************************************************************
039700940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
039800940224     C************************************************************
039900940224     C     GESPIE        BEGSR
040000940224     C*
040100940224     C                   SELECT
040200940224     C* Parzializzazioni
040300940224     C     $GEST         WHENEQ    'D1'
040400940506     C     $ULKD1        IFEQ      0
040500940506     C* riempimento opzioni
040600940506     C                   EXSR      F24D1
040700940506     C                   ELSE
040800940506     C                   WRITE     FMTZ1
040900940506     C                   ENDIF
041000940506     C* Lista
041100940506     C     $GEST         WHENEQ    'S1'
041200940506     C     $ULKS1        IFEQ      0
041300940506     C                   EXSR      F24S1
041400940506     C                   ELSE
041500940506     C                   WRITE     FMTZ2
041600940506     C                   ENDIF
041700940916     C* Modifica valori assunti
041800940916     C     $GEST         WHENEQ    'D2'
041900940916     C     $ULKD2        IFEQ      0
042000940916     C                   EXSR      F24D2
042100940916     C                   ELSE
042200940916     C                   WRITE     FMTZ3
042300940916     C                   ENDIF
042400940506     C                   ENDSL
042500940506     C*
042600940224     C                   ENDSR
042700940224     C/EJECT
042800940224     C************************************************************
042900940224     C* GESTIONE PARZIALIZZAZIONI
043000940224     C************************************************************
043100940224     C     GESD1         BEGSR
043200940224     C* inizializzazione videata
043300940223     C     $INZD1        IFEQ      *ON
043400940127     C                   EXSR      INZD1
043500940223     C                   MOVE      *OFF          $INZD1
043600940117     C                   END
043700940223     C* emissione piede videata se proveniente da altra
043800940223     C* salvataggio valore formato di provenienza
043900940202     C     $LASTV        IFNE      'D1'
044000940223     C* salvo stato F17 per sapere cosa fare in caso di F12
044100940223     C                   MOVE      *INKR         WINKR
044200940202     C                   WRITE     FMTT1
044300940224     C                   EXSR      GESPIE
044400940202     C                   MOVE      $LASTV        $LASTG
044500940202     C                   MOVE      'D1'          $LASTV
044600940707     C                   ENDIF
044700940707     C*
044800940707     C* se esistono errori sulla videata
044900940707     C* emetto la write del formato a indicatori spenti per vedere
045000940707     C* le eventuali decodifiche
045100940707     C     *IN99         IFEQ      *ON
045200940707     C                   MOVEA     *IN           WIN
045300940707     C                   MOVE      *ALL'0'       IN5098           49
045400940707     C                   MOVEA     IN5098        *IN(50)
045500940707     C                   WRITE     FMTD1
045600940707     C                   MOVEA     WIN           *IN
045700940707     C                   ENDIF
045800940131     C*
045900940131     C                   EXFMT     FMTD1
046000940506     C* controllo abilitazione tasti funzione
046100940506     C*
046200940506     C                   EXSR      CTRKD1
046300940506     C*
046400940506     C     *IN99         IFEQ      *OFF
046500940131     C*
046600940117     C                   SELECT
046700940916     C* F2=Decodifica
046800940916     C     $TASTO        WHENEQ    F02
046900940916     C                   EXSR      F02D1
047000940131     C* F3=Fine
047100940506     C     $TASTO        WHENEQ    F03
047200940307     C                   EXSR      F03D1
047300940131     C* F4=Ricerca
047400940506     C     $TASTO        WHENEQ    F04
047500940127     C                   EXSR      SEARCH
047600940916     C* F5=Refresh
047700940916     C     $TASTO        WHENEQ    F05
047800940916     C                   EXSR      F05D1
047900940131     C* F12=Ritorno
048000940506     C     $TASTO        WHENEQ    F12
048100940307     C                   EXSR      F12D1
048200940131     C* F18=Modifica valori assunti
048300940506     C     $TASTO        WHENEQ    F18
048400940307     C                   EXSR      F18D1
048500940224     C* F24=Altri tasti
048600940506     C     $TASTO        WHENEQ    F24
048700940309     C                   EXSR      F24D1
048800940131     C*
048900940117     C                   OTHER
049000940127     C                   EXSR      CTRD1
049100941010     C     *IN99         IFEQ      *OFF
049200940131     C* definisco l'ordinamento
049300940131     C                   SELECT
049400940314    >C* << nel caso di più viste logiche e quindi più campi di
049500940314    >C*    parzializzazione, qui vanno definiti i criteri per
049600940314    >C*    determinare quale VL va letta, a seconda di quali campi
049700940314    >C*    sono stati valorizzati >>
049800941007    >C     D1PRF         WHENNE    *BLANK
049900940131     C                   Z-ADD     1             $ORD
050000941007     C                   SETON                                            01
050100941124     C                   SETOFF                                           02
050200941010    >C     D1GRP         WHENNE    0
050300940131     C                   Z-ADD     2             $ORD
050400941007     C                   SETON                                            02
050500941124     C                   SETOFF                                           01
050600940131     C* nel caso non sia stato digitato nulla uso l'ordinamento
050700940131     C* dei valori assunti
050800940131     C                   ENDSL
050900941010     C                   MOVE      'S1'          $GEST
051000940223     C                   MOVE      *ON           $INZS1
051100940302     C                   MOVE      *BLANK        W1POS
051200940211     C                   ENDIF
051300940224     C*
051400940117     C                   ENDSL
051500940224     C*
051600940506     C                   ENDIF
051700940117     C                   ENDSR
051800940224     C/EJECT
051900940224     C************************************************************
052000940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
052100940224     C************************************************************
052200940127     C     INZD1         BEGSR
052300940318     C*
052400940318     C                   CLEAR                   FMTD1
052500940831     C* Se pgm non richiamato
052600941007     C     OPZ003        IFEQ      *BLANK
052700941010    >C*******              Z-ADDUDATE     D1DRG
052800940831     C                   ELSE
052900940831     C* Se pgm richiamato
053000940831     C                   SELECT
053100941010     C     PRF003        WHENNE    *BLANK
053200941010     C     SOC003        ANDNE     *BLANK
053300941010     C                   MOVEL     PRF003        D1PRF
053400941010     C                   OTHER
053500941010     C*********            Z-ADDUDATE     D1DRG
053600940831     C                   ENDSL
053700940831     C                   END
053800940224     C*
053900940117     C                   ENDSR
054000940506     C/EJECT
054100940506     C************************************************************
054200940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
054300940609     C* INPUT
054400940509     C* $FC1 = Abilitazioni videata D!
054500940509     C* OUTPUT
054600940509     C* *IN97 *IN99 Errori
054700940506     C************************************************************
054800940506     C     CTRKD1        BEGSR
054900940506     C                   SETOFF                                       99
055000940506     C                   MOVEA     $FC1          $FC
055100940506     C                   EXSR      CTRKEY
055200940506     C     *IN99         IFEQ      *ON
055300940506     C                   SETON                                        97
055400940506     C                   ENDIF
055500940506     C                   ENDSR
055600940506     C/EJECT
055700940506     C************************************************************
055800940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
055900940506     C************************************************************
056000940506     C     CTRKS1        BEGSR
056100940506     C                   SETOFF                                       99
056200940506     C                   MOVEA     $FC2          $FC
056300940506     C                   EXSR      CTRKEY
056400940506     C     *IN99         IFEQ      *ON
056500940506     C                   SETON                                        97
056600940506     C                   ENDIF
056700940506     C                   ENDSR
056800940506     C/EJECT
056900940506     C************************************************************
057000940506     C* CONTROLLO ABILITAZIONE TASTI
057100940509     C* INPUT
057200940609     C* $TASTO =Tasto funzione premuto
057300940509     C* $FC = Tasti abilitati
057400940509     C* OUTPUT
057500940509     C* IN99 = ERrore
057600940506     C************************************************************
057700940506     C     CTRKEY        BEGSR
057800940506     C                   SELECT
057900940506     C     $TASTO        WHENEQ    F01
058000940720     C     $FC(1)        IFEQ      '0'
058100940506     C                   SETON                                        99
058200940506     C                   ENDIF
058300940506     C     $TASTO        WHENEQ    F02
058400940720     C     $FC(2)        IFEQ      '0'
058500940506     C                   SETON                                        99
058600940506     C                   ENDIF
058700940506     C     $TASTO        WHENEQ    F03
058800940720     C     $FC(3)        IFEQ      '0'
058900940506     C                   SETON                                        99
059000940506     C                   ENDIF
059100940506     C     $TASTO        WHENEQ    F04
059200940720     C     $FC(4)        IFEQ      '0'
059300940506     C                   SETON                                        99
059400940506     C                   ENDIF
059500940506     C     $TASTO        WHENEQ    F05
059600940720     C     $FC(5)        IFEQ      '0'
059700940506     C                   SETON                                        99
059800940506     C                   ENDIF
059900940506     C     $TASTO        WHENEQ    F06
060000940720     C     $FC(6)        IFEQ      '0'
060100940506     C                   SETON                                        99
060200940506     C                   ENDIF
060300940506     C     $TASTO        WHENEQ    F07
060400940720     C     $FC(7)        IFEQ      '0'
060500940506     C                   SETON                                        99
060600940506     C                   ENDIF
060700940506     C     $TASTO        WHENEQ    F08
060800940720     C     $FC(8)        IFEQ      '0'
060900940506     C                   SETON                                        99
061000940506     C                   ENDIF
061100940615     C     $TASTO        WHENEQ    F09
061200940720     C     $FC(09)       IFEQ      '0'
061300940506     C                   SETON                                        99
061400940506     C                   ENDIF
061500940506     C     $TASTO        WHENEQ    F10
061600940720     C     $FC(10)       IFEQ      '0'
061700940506     C                   SETON                                        99
061800940506     C                   ENDIF
061900940506     C     $TASTO        WHENEQ    F11
062000940720     C     $FC(11)       IFEQ      '0'
062100940506     C                   SETON                                        99
062200940506     C                   ENDIF
062300940506     C     $TASTO        WHENEQ    F12
062400940720     C     $FC(12)       IFEQ      '0'
062500940506     C                   SETON                                        99
062600940506     C                   ENDIF
062700940506     C     $TASTO        WHENEQ    F13
062800940720     C     $FC(13)       IFEQ      '0'
062900940506     C                   SETON                                        99
063000940506     C                   ENDIF
063100940506     C     $TASTO        WHENEQ    F14
063200940720     C     $FC(14)       IFEQ      '0'
063300940506     C                   SETON                                        99
063400940506     C                   ENDIF
063500940506     C     $TASTO        WHENEQ    F15
063600940720     C     $FC(15)       IFEQ      '0'
063700940506     C                   SETON                                        99
063800940506     C                   ENDIF
063900940506     C     $TASTO        WHENEQ    F16
064000940720     C     $FC(16)       IFEQ      '0'
064100940506     C                   SETON                                        99
064200940506     C                   ENDIF
064300940506     C     $TASTO        WHENEQ    F17
064400940720     C     $FC(17)       IFEQ      '0'
064500940506     C                   SETON                                        99
064600940506     C                   ENDIF
064700940506     C     $TASTO        WHENEQ    F18
064800940720     C     $FC(18)       IFEQ      '0'
064900940506     C                   SETON                                        99
065000940506     C                   ENDIF
065100940506     C     $TASTO        WHENEQ    F19
065200940720     C     $FC(19)       IFEQ      '0'
065300940506     C                   SETON                                        99
065400940506     C                   ENDIF
065500940506     C     $TASTO        WHENEQ    F20
065600940720     C     $FC(20)       IFEQ      '0'
065700940506     C                   SETON                                        99
065800940506     C                   ENDIF
065900940506     C     $TASTO        WHENEQ    F21
066000940720     C     $FC(21)       IFEQ      '0'
066100940506     C                   SETON                                        99
066200940506     C                   ENDIF
066300940506     C     $TASTO        WHENEQ    F22
066400940720     C     $FC(22)       IFEQ      '0'
066500940506     C                   SETON                                        99
066600940506     C                   ENDIF
066700940506     C     $TASTO        WHENEQ    F23
066800940720     C     $FC(23)       IFEQ      '0'
066900940506     C                   SETON                                        99
067000940506     C                   ENDIF
067100940506     C     $TASTO        WHENEQ    F24
067200940720     C     $FC(24)       IFEQ      '0'
067300940506     C                   SETON                                        99
067400940506     C                   ENDIF
067500940506     C                   ENDSL
067600940506     C                   ENDSR
067700940916     C/EJECT
067800940916     C************************************************************
067900940916     C* GESTIONE F02 VIDEO D1
068000940916     C************************************************************
068100940916     C     F02D1         BEGSR
068200940916     C*
068300940916     C* per decodificare la videata richiamo la routine dei ctrl
068400940916     C* e poi disattivo gli eventuali messaggi di errore
068500940916     C                   EXSR      CTRD1
068600940916     C*
068700940916     C                   MOVE      *ALL'0'       IN5098
068800940916     C                   MOVEA     IN5098        *IN(50)
068900940916     C*
069000940916     C                   ENDSR
069100940224     C/EJECT
069200940131     C************************************************************
069300940307     C* GESTIONE F03 VIDEO D1
069400940131     C************************************************************
069500940307     C     F03D1         BEGSR
069600940131     C*
069700940307     C                   MOVE      *ON           $FINE
069800940325     C* fine programma
069900940325     C                   EXSR      ENDPGM
070000940307     C*
070100940307     C                   ENDSR
070200940916     C/EJECT
070300940916     C************************************************************
070400940916     C* GESTIONE F05 VIDEO D1
070500940916     C************************************************************
070600940916     C     F05D1         BEGSR
070700940916     C*
070800940916     C                   MOVE      *ON           $INZD1
070900940916     C*
071000940916     C                   ENDSR
071100940307     C/EJECT
071200940307     C************************************************************
071300940307     C* GESTIONE F12 VIDEO D1
071400940307     C************************************************************
071500940307     C     F12D1         BEGSR
071600940307     C*
071700940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
071800940307     C*  per F12 torno alla lista
071900940307     C     $LOP          IFEQ      2
072000940307     C     WINKR         OREQ      *ON
072100940307     C                   MOVE      'S1'          $GEST
072200940307     C*  altrimenti, per F12 torno a menu
072300940307     C                   ELSE
072400940307     C                   MOVE      *ON           $FINE
072500940307     C                   END
072600940307     C*
072700940307     C                   ENDSR
072800940307     C/EJECT
072900940307     C************************************************************
073000940307     C* GESTIONE F18 VIDEO D1
073100940307     C************************************************************
073200940307     C     F18D1         BEGSR
073300940307     C*
073400940307     C                   MOVE      'D2'          $GEST
073500940307     C                   MOVEL     *ON           $INZD2
073600940307     C*
073700940307     C                   ENDSR
073800940307     C/EJECT
073900940307     C************************************************************
074000940307     C* GESTIONE F24 VIDEO D1
074100940609     C* input
074200940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
074300940509     C* $FX1 =Testi tasti funzione
074400940509     C* $FC1 = Abilitazioni
074500940509     C* $FR1 = Inversioni di fondo
074600940307     C************************************************************
074700940307     C     F24D1         BEGSR
074800940506     C*
074900940506     C                   MOVEA     $FX1          $FX
075000940506     C                   MOVEA     $FC1          $FC
075100940506     C                   MOVEA     $FR1          $FR
075200940506     C                   Z-ADD     $ULKD1        $ULK
075300940929     C                   Z-ADD     79            $L01
075400940929     C                   Z-ADD     79            $L02
075500940506     C                   EXSR      RIEKEY
075600940506     C                   Z-ADD     $ULK          $ULKD1            3 0
075700940720     C                   MOVEA     $FC           $FC1
075800940506     C                   MOVEL     $KEY1         Z1KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
075900940506     C                   MOVEL     $KEY2         Z1KE2
076000940506     C                   WRITE     FMTZ1
076100940506     C*
076200940307     C*
076300940307     C                   ENDSR
076400940307     C/EJECT
076500940307     C************************************************************
076600940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
076700940307     C************************************************************
076800940307     C     CTRD1         BEGSR
076900940307     C*
077000940127     C                   SETOFF                                       99
077100940127     C* non imposto pos. del cursore
077200940127     C                   Z-ADD     0             H1RIGA
077300940127     C                   Z-ADD     0             H1COLO
077400940131     C*
077500940314    >C* << elencare gli eventuali controlli sui campi di
077600940314    >C*    parzializzazione, ricordando che quasi sempre essi
077700940314    >C*    non ne avranno perchè determinano solo il range di
077800940314    >C*    scrittura della lista e quindi (nel caso di campi
077900940314    >C*    alfanumerici) saranno trattati come chiavi parziali >>
078000940131     C*
078100940117     C                   ENDSR
078200940224     C/EJECT
078300940127     C************************************************************
078400940131     C* GESTIONE LISTA
078500940127     C************************************************************
078600940127     C     GESS1         BEGSR
078700940223     C* inizializzazione videata
078800940223     C     $INZS1        IFEQ      *ON
078900940127     C                   EXSR      INZS1
079000940223     C                   MOVE      *OFF          $INZS1
079100940127     C                   ENDIF
079200940223     C* emissione piede videata se proveniente da altra
079300940223     C* salvataggio valore formato di provenienza
079400940202     C     $LASTV        IFNE      'S1'
079500940202     C                   WRITE     FMTT1
079600940224     C                   EXSR      GESPIE
079700940202     C                   MOVE      $LASTV        $LASTG
079800940202     C                   MOVE      'S1'          $LASTV
079900940202     C                   END
080000940223     C*
080100940223     C     WMAX          IFEQ      0
080200940211     C                   WRITE     FMTD3                                        NO RCD
080300940211     C                   ENDIF
080400940831     C*Serve per emissione finestra pgm invio ang727r (per open/close
080500940831     C*window)
080600940831     C                   MOVE      '1'           WIN727            1
080700940127     C*
080800940131     C                   EXFMT     FMTC1
080900940204     C     C1NRR         IFNE      0
081000940204     C                   Z-ADD     C1NRR         WSFL
081100940204     C                   ENDIF
081200940127     C                   Z-ADD     SFLNRR        C1RCD
081300940506     C*
081400940506     C* controllo abilitazione tasti funzione
081500940506     C*
081600940506     C                   EXSR      CTRKS1
081700940506     C     *IN99         IFEQ      *OFF
081800940506     C*
081900940127     C*
0820009401271    C                   SELECT
082100940127     C* F3=Fine
082200940506     C     $TASTO        WHENEQ    F03
082300940309     C                   EXSR      F03S1
082400940131     C* F10=Immissione
082500940506     C     $TASTO        WHENEQ    F10
082600940309     C                   EXSR      F10S1
082700940131     C* F12=Ritorno
082800940506     C     $TASTO        WHENEQ    F12
082900940309     C                   EXSR      F12S1
083000940131     C* F17=Parzializzazioni
083100940506     C     $TASTO        WHENEQ    F17
083200940309     C                   EXSR      F17S1
083300940131     C* F18=Modifica valori assunti
083400940506     C     $TASTO        WHENEQ    F18
083500940309     C                   EXSR      F18S1
083600940224     C* F23=Altre opzioni
083700940506     C     $TASTO        WHENEQ    F23
083800940309     C                   EXSR      F23S1
083900940224     C* F24=Altri tasti
084000940506     C     $TASTO        WHENEQ    F24
084100940309     C                   EXSR      F24S1
084200940127     C* ROLL-UP
084300940506     C     $TASTO        WHENEQ    ROLLUP
084400940127     C                   EXSR      ROLS1
084500940127     C*
0846009401271O   C                   OTHER
084700940127     C* CONTROLLO DATI
084800940131     C                   EXSR      CTRC1
084900940201     C     *IN99         IFEQ      *OFF
085000940131     C                   EXSR      CTRS1
085100940131     C                   END
0852009401271-   C                   ENDSL
085300940127     C*
085400940506     C                   ENDIF
085500940127     C                   ENDSR
085600940224     C/EJECT
085700940127     C************************************************************
085800940131     C* INIZIALIZZAZIONE LISTA
085900940127     C************************************************************
086000940127     C     INZS1         BEGSR
086100940302     C* pulizia SFL
086200940128     C                   SETOFF                                         3031
086300940131     C                   WRITE     FMTC1
086400940128     C                   SETON                                          31
086500940127     C*
086600940128     C* riempimento opzioni
086700940223     C                   MOVEA     $TX1          $TX
086800940223     C                   MOVEA     $FL1          $FL
086900940309     C                   Z-ADD     0             WCOZ
087000940225     C*  equivale al num.di elementi della schiera $TX1
087100940509     C                   Z-ADD     $DIMOP        WMXO
087200950403     C                   Z-ADD     76            $L01
087300950403     C                   Z-ADD     76            $L02
087400940128     C                   EXSR      RIEOPZ
087500950403     C                   Z-ADD     23            X
087600950403     C                   MOVE      FF23          $FC2(X)
087700940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
087800940608     C                   MOVEL     WOPZ2         C1OPZ2
087900940225     C                   Z-ADD     WCOZ          C1COZ
088000940128     C*
088100940128     C* CARICAMENTO SFL A PAGINE
088200940128     C*
088300940201     C                   Z-ADD     0             S1NRR
088400940128     C                   Z-ADD     0             WMAX
088500940224     C*
088600940224     C* Posizionamento su file pilota
088700940302     C                   CLEAR                   C1POS
088800941124     C                   CLEAR                   C1POS2
088900941007    >C                   EXSR      SETABS
089000940302     C* ripristino i valori del posizionamento
089100941124     C     $ORD          IFEQ      2
089200941124     C                   Z-ADD     W1POS2        C1POS2
089300941124     C                   ELSE
089400941124     C                   MOVE      W1POS         C1POS
089500941124     C                   ENDIF
089600940131     C*
089700940201     C* Lettura file pilota
089800941007    >C                   EXSR      REDABS
089900940131     C*
090000940131     C* Carico 1a pagina
090100940127     C                   EXSR      ROLS1
090200940201     C                   Z-ADD     SFLPAG        WPAG
090300940127     C*
090400940127     C                   ENDSR
090500940127     C/EJECT
090600940127     C************************************************************
090700940131     C* CARICAMENTO PAGINA LISTA
090800940127     C************************************************************
090900940127     C     ROLS1         BEGSR
091000940127     C*
091100940128     C                   SETOFF                                       32
091200940223     C                   Z-ADD     0             Y
091300940127     C                   Z-ADD     WMAX          S1NRR
091400940127     C*
091500940127     C* Leggo dal file anagrafico per caricare la lista
091600940127     C*
0917009401311    C     $EFILE        DOWEQ     *OFF
091800940201     C     Y             ANDLT     WPAG
091900940127     C*
092000940131     C                   EXSR      RIES1
092100940127     C*
092200940127     C                   ADD       1             S1NRR
092300940127     C                   ADD       1             Y
092400940127     C*
092500940131     C                   WRITE     FMTS1
092600940131     C*
092700941007    >C                   EXSR      REDABS
092800940128     C*
0929009401271-   C                   ENDDO
093000940127     C*
093100940223     C                   Z-ADD     S1NRR         WMAX                 30
093200940127     C*
093300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
093400940127     C*
093500940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
093600940127     C                   MVR                     RESTO             3 0
093700940131     C     PAGINE        MULT      SFLPAG        C1RCD
0938009401271    C     RESTO         IFGT      0
093900940127     C                   ADD       1             C1RCD
0940009401271E   C                   ELSE
094100940407     C                   SUB       SFLPAG        C1RCD
094200940407     C                   ADD       1             C1RCD
0943009401271-   C                   ENDIF
094400940128     C*
094500940127     C                   ENDSR
094600940224     C/EJECT
094700940224     C************************************************************
094800940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
094900940224     C************************************************************
095000941007    >C     SETABS        BEGSR
095100940224     C*
095200940224     C                   SELECT
095300940314    >C* Ord.1 = chiave ordinamento 1
095400940224     C     $ORD          WHENEQ    1
095500940314    >C*-----------------------------------------------------------
095600940314    >C* Gestione posizionamento
095700940314    >C* Se il campo W1POS è > *blank imposto il posizionamento con
095800940314    >C* W1POS altrimenti lo imposto con il valore di parametrizzazione
095900940314    >C*-----------------------------------------------------------
096000940302     C     W1POS         IFNE      *BLANK
096100941124    >C                   MOVEL     W1POS         ABSPRF
096200940302     C                   ELSE
096300941007    >C                   MOVEL     D1PRF         ABSPRF
096400940302     C                   END
096500950302    >C     ABSPRF        SETLL     ANABS01
096600940224     C* Ord.2 = Modulo
096700940224     C     $ORD          WHENEQ    2
096800941124     C     W1POS2        IFNE      0
096900941124    >C                   Z-ADD     W1POS2        ABSGRP
097000941124    >C                   MOVEL     *BLANK        ABSPRF
097100940302     C                   ELSE
097200941010    >C                   MOVEL     D1GRP         ABSGRP
097300941010    >C                   MOVEL     *BLANK        ABSPRF
097400940302     C                   END
097500950302    >C     K2ABS2        SETLL     ANABS02
097600940831     C* Ord.99= Chiave univoca
097700941010     C*          $ORD      WHEQ 99
097800941010    >C*          W1POS     IFNE *BLANK
097900941010    >C*                    MOVELW1POS     MOVPRG
098000941010    >C*                    ELSE
098100941010    >C*                    MOVELD1PRG     MOVPRG
098200941010    >C*                    END
098300941010    >C*          MOVPRG    SETLLxxANA99
098400940224     C                   ENDSL
098500940224     C*
098600940224     C                   ENDSR
098700940128     C/EJECT
098800940128     C************************************************************
098900940131     C* LETTURA RCD ARCHIVIO PILOTA
099000940128     C************************************************************
099100941010    >C     REDABS        BEGSR
099200940128     C*
099300940131     C                   MOVEL     *OFF          $EFILE
099400940131     C                   MOVEL     *OFF          $RCDOK
099500940131     C*
0996009401311    C     $EFILE        DOUEQ     *ON
099700940131     C     $RCDOK        OREQ      *ON
099800940131     C*
0999009401312    C                   SELECT
100000940131     C     $ORD          WHENEQ    1
100100941010    >C                   READ      ANABS01                                21
100200940131     C     $ORD          WHENEQ    2
100300941010    >C                   READ      ANABS02                                21
100400941010     C*          $ORD      WHEQ 99
100500941010    >C*                    READ xxANA99                  21
1006009401312-   C                   ENDSL
100700940131     C*
100800940131     C                   MOVE      *IN21         $EFILE
100900940131     C*
1010009401312    C     $EFILE        IFEQ      *OFF
101100941010    >C                   EXSR      SELABS
1012009401312-   C                   ENDIF
101300940131     C*
1014009401311-   C                   ENDDO
101500940131     C*
101600940131     C                   MOVE      *IN21         *IN33                          SflEnd
101700940131     C*
101800940131     C                   ENDSR
101900940224     C/EJECT
102000940224     C************************************************************
102100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
102200940224     C************************************************************
102300940224     C     CLCPAG        BEGSR
102400940224     C* Input :
102500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
102600940224     C*          cursore
102700940224     C* - SFLPAG = numero rcd per pagina sfl
102800940224     C* Output :
102900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
103000940224     C*
103100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
103200940224     C                   MVR                     RESTO             3 0
103300940224     C     RESTO         IFGT      0
103400940224     C                   ADD       1             PAGINE
103500940224     C                   ENDIF
103600940224     C     PAGINE        MULT      SFLPAG        WPAG
103700940224     C*
103800940224     C                   ENDSR
103900940131     C/EJECT
104000940131     C************************************************************
104100940131     C* SELEZIONI SULLA RIGA LETTA
104200940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
104300940314     C* non rientra nei parametri di parzializzazione
104400940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
104500940314     C* superiore stabilito nei parametri
104600940314     C*
104700940131     C************************************************************
104800941010    >C     SELABS        BEGSR
104900940131     C*
105000940202     C                   MOVEL     *ON           $RCDOK
105100940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
105100940131A1211C                   MOVEL     *hival        ALL9             15
105200940314     C* creo limite massimo parzializzazioni
105300940609    >C* << per ogni valore alfanumerico creo il limite superiore
105400940314    >C*    per cui caricare, creando una variabile che contiene
105500940314    >C*    il valore digitato nel campo a video riempito di "9" >>
105600941010    >C     D1PRF         CAT       ALL9:0        PRF999
105700940314     C*
105800940314     C                   SELECT
105900940314     C*
106000940314     C* SELEZIONI RISPETTO AI VALORI RCD
106100940314     C*
106200940314    >C* << elaborare eventuali selezioni che non riguardano le
106300940316    >C*    parzializzazioni ma più propriamente i dati del rcd ;
106400940314    >C*    esempio : esclusione degli annullati. >>
106500940314     C*
106600940314     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
106700940314     C*
106800940316    >C* oggetto non compreso nella scelta
106900941010    >C     ABSPRF        WHENLT    D1PRF
107000941010    >C     ABSPRF        ORGT      PRF999
107100941010    >C                   MOVEL     *OFF          $RCDOK
107200941010    >C     ABSPRF        IFGT      PRF999
107300941010    >C     $ORD          ANDEQ     1
107400941010    >C     *HIVAL        SETLL     ANABS01
107500941010    >C                   ENDIF
107600940316    >C* modulo non compresa nella scelta
107700941010    >C     ABSGRP        WHENNE    D1GRP
107800941010    >C     D1GRP         ANDNE     0
107900941010      *
108000941010    >C                   MOVEL     *OFF          $RCDOK
108100941010      *
108200940831     C*
108300940831    >C* chiave uguale a quella passata al pgm
108400941010    >C**         MOVPRG    WHLT D1PRG
108500941010    >C**         D1PRG     ANDNE0
108600941010    >C*          MOVPRG    ORGT D1PRG
108700941010    >C*          D1PRG     ANDNE0
108800941010    >C*                    MOVEL*OFF      $RCDOK
108900941010    >C*          MOVPRG    IFGT D1PRG
109000941010    >C*          $ORD      ANDEQ99
109100941010    >C*          *HIVAL    SETLLxxANA99
109200941010     C*                    ENDIF
109300941010    >C*******
109400940316    >C* questo è lo schema generico da usare
109500941010    >C*          xxxxxx    WHLT D1xxx
109600941010    >C*          xxxxxx    ORGT xxx999
109700941010     C*                    MOVEL*OFF      $RCDOK
109800940314     C* nel caso che il dato letto sia > del limite superiore e
109900940314     C* questo dato è la chiave della VL in lettura, allora per
110000940314     C* evitare altri accessi inutili salto a fine file
110100941010    >C*          xxxxxx    IFGT xxx999
110200941010     C*          $ORD      ANDEQ1
110300941010    >C*          *HIVAL    SETLLxxxxx01
110400941010     C*                    ENDIF
110500940202     C*
110600940202     C                   ENDSL
110700940202     C*
110800940202     C                   ENDSR
110900940131     C/EJECT
111000940131     C************************************************************
111100940131     C* RIEMPIMENTO RIGA LISTA
111200940131     C************************************************************
111300940131     C     RIES1         BEGSR
111400940131     C*
111500940131     C                   CLEAR                   FMTS1
111600940314    >C* << riempio i campi del sfl, compresi gli hidden con i valori
111700940314    >C*    del rcd letto >>
111800940131     C*
111900941010    >C                   MOVEL     ABSGRP        S1GRP
112000941010    >C                   MOVEL     ABSPRF        S1PRF
112100941010    >C                   MOVEL     ABSSOC        S1SOC
112200940131     C*
112300940131     C                   ENDSR
112400940309     C/EJECT
112500940309     C************************************************************
112600940309     C* GESTIONE F03 VIDEO S1
112700940309     C************************************************************
112800940309     C     F03S1         BEGSR
112900940309     C*
113000940309     C                   MOVE      *ON           $FINE
113100940325     C* fine programma
113200940325     C                   EXSR      ENDPGM
113300940309     C*
113400940309     C                   ENDSR
113500940309     C/EJECT
113600940309     C************************************************************
113700940309     C* GESTIONE F10 VIDEO S1
113800940609     C* AGGIUNTA RECORD
113900940309     C************************************************************
114000940309     C     F10S1         BEGSR
114100940309     C*
114200941010    >C                   RESET                   A004DS
114300941010    >C                   MOVEL     '01'          OPZ004
114400941010    >C                   MOVEL     A004DS        KPJBU
114500941010    >C                   CALL      'ANA004R'
114600940309     C                   PARM                    KPJBA
114700941010    >C                   MOVEL     KPJBU         A004DS
114800940309     C* ritorno da PGM gestione
114900941010    >C                   EXSR      RTN004
115000940309     C     WINZS1        IFEQ      *ON
115100940309     C                   MOVE      *ON           $INZS1
115200940309     C* se esistevano già righe sul sfl
115300940309     C* calcolo pagina a cui deve posizionarsi
115400940309     C     WSFL          IFGT      0
115500940309     C                   EXSR      CLCPAG
115600940309     C* altrimenti carico solo la 1a pagina
115700940309     C                   ELSE
115800940309     C                   Z-ADD     SFLPAG        WPAG
115900940309     C                   ENDIF
116000940309     C                   ENDIF
116100940309     C*
116200940627     C                   MOVE      *HIVAL        $LASTV
116300940627     C*
116400940309     C                   ENDSR
116500940309     C/EJECT
116600940309     C************************************************************
116700940309     C* GESTIONE F12 VIDEO S1
116800940309     C************************************************************
116900940309     C     F12S1         BEGSR
117000940309     C*
117100940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
117200940309     C     $LOP          IFEQ      1
117300940309     C                   MOVE      'D1'          $GEST
117400940309     C*  se la 1a videata è questa, per F12 torno a menu
117500940309     C                   ELSE
117600940309     C                   MOVE      *ON           $FINE
117700940309     C                   END
117800940309     C*
117900940309     C                   ENDSR
118000940309     C/EJECT
118100940309     C************************************************************
118200940309     C* GESTIONE F17 VIDEO S1
118300940309     C************************************************************
118400940309     C     F17S1         BEGSR
118500940309     C*
118600940309     C                   MOVE      'D1'          $GEST
118700940831     C* forza certi valori nel caso il pgm sia stato richiamato con
118800940831     C* chiave specifica
118900941010<<<<>C     PRF003        IFNE      *BLANK
119000941010    >C                   MOVEL     *BLANK        PRF003
119100940831     C                   END
119200940831     C     $ORD          IFEQ      99
119300940831     C                   Z-ADD     1             $ORD
119400941124     C                   SETON                                        01
119500941124     C                   SETOFF                                       02
119600941010    >C*************        Z-ADDUDATE     D1DRG
119700940831     C                   END
119800940309     C*
119900940309     C                   ENDSR
120000940309     C/EJECT
120100940309     C************************************************************
120200940309     C* GESTIONE F18 VIDEO S1
120300940309     C************************************************************
120400940309     C     F18S1         BEGSR
120500940309     C*
120600940309     C                   MOVE      'D2'          $GEST
120700940309     C                   MOVEL     *ON           $INZD2
120800940831     C     $ORD          IFEQ      99
120900940831     C                   Z-ADD     1             $ORD
121000941124     C                   SETON                                        01
121100941124     C                   SETOFF                                       02
121200940831     C                   END
121300940309     C*
121400940309     C                   ENDSR
121500940309     C/EJECT
121600940309     C************************************************************
121700940309     C* GESTIONE F23 VIDEO S1
121800940309     C************************************************************
121900940309     C     F23S1         BEGSR
122000940309     C*
122100950403     C                   Z-ADD     76            $L01
122200950403     C                   Z-ADD     76            $L02
122300940309     C                   EXSR      RIEOPZ
122400950403     C                   Z-ADD     23            X
122500950403     C                   MOVE      FF23          $FC2(X)
122600940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
122700940608     C                   MOVEL     WOPZ2         C1OPZ2
122800940309     C                   Z-ADD     WCOZ          C1COZ
122900940309     C*
123000940309     C                   ENDSR
123100940309     C/EJECT
123200940309     C************************************************************
123300940309     C* GESTIONE F24 VIDEO S1
123400940309     C************************************************************
123500940309     C     F24S1         BEGSR
123600940309     C*
123700940506     C*
123800940506     C                   MOVEA     $FX2          $FX
123900940506     C                   MOVEA     $FC2          $FC
124000940506     C                   MOVEA     $FR2          $FR
124100940506     C                   Z-ADD     $ULKS1        $ULK
124200940929     C                   Z-ADD     79            $L01
124300940929     C                   Z-ADD     79            $L02
124400940506     C                   EXSR      RIEKEY
124500940506     C                   Z-ADD     $ULK          $ULKS1            3 0
124600940720     C                   MOVEA     $FC           $FC2
124700940506     C                   MOVEL     $KEY1         Z2KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
124800940506     C                   MOVEL     $KEY2         Z2KE2
124900940506     C                   WRITE     FMTZ2
125000940506     C*
125100940309     C                   ENDSR
125200940128     C/EJECT
125300940128     C************************************************************
125400940131     C* CONTROLLO TESTATA LISTA
125500940128     C************************************************************
125600940131     C     CTRC1         BEGSR
125700940128     C*
125800940201     C                   MOVE      *OFF          *IN99
125900940131     C*
126000940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
126100940202     C* L'INIZIALIZZAZIONE LISTA
126200940202     C*
126300941010     C                   SELECT
126400941010     C     $ORD          WHENEQ    2
126500941010    >C     C1POS2        IFNE      W1POS2
126600941010     C                   EXSR      POSS1
126700941010     C                   END
126800941010      *
126900941010     C                   OTHER
127000941010      *
127100941010    >C     C1POS         IFNE      W1POS
127200941010     C                   EXSR      POSS1
127300941010     C                   END
127400941010     C                   ENDSL
127500941010     C*
127600940202     C*
127700940202     C                   ENDSR
127800940202     C/EJECT
127900940202     C************************************************************
128000940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
128100940202     C************************************************************
128200940202     C     POSS1         BEGSR
128300940202     C*
128400940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
128500940202     C*
1286009402021    C     WMAX          IFGT      0
128700940202     C                   READC     FMTS1                                  21
1288009402022    C     *IN21         DOWEQ     *OFF
128900940202     C     *IN99         ANDEQ     *OFF
129000940202     C*
1291009402023    C     S1OPZ         IFNE      0
129200940202     C                   SETON                                        51  99
1293009402023-   C                   ENDIF
129400940202     C* se rilevata opzione attiva, attivo sflnxtchg
1295009402023    C     *IN99         IFEQ      *ON
129600940202     C                   MOVE      *ON           *IN32
129700940202     C                   UPDATE    FMTS1
1298009402023E   C                   ELSE
129900940202     C                   READC     FMTS1                                  21
1300009402023-   C                   ENDIF
130100940202     C*
1302009402022-   C                   ENDDO
130300940202     C*
1304009402021-   C                   ENDIF
130500940202     C*
130600940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
130700940221     C* AL VALORE DI POSIZIONAMENTO :
130800940221     C* se questo è maggiore del valore del rispettivo campo di
130900940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
131000940221     C* con i valori di parzializzazione.
131100940221     C*
1312009402021    C     *IN99         IFEQ      *OFF
131300940721    >C                   MOVE      *BLANK        W1POS
131400941124    >C                   Z-ADD     0             W1POS2
131500940202     C*
1316009402022    C                   SELECT
131700940202     C*
131800940202     C     $ORD          WHENEQ    1
131900940202     C*
132000941010    >C     C1POS         IFGT      D1PRF
132100941124    >C                   MOVEL     C1POS         W1POS
132200940131     C                   END
132300940131     C*
132400940202     C                   MOVE      *ON           *IN99
132500940223     C                   MOVE      *ON           $INZS1
132600940831     C*
132700940831     C     $ORD          WHENEQ    99
132800940831     C*
132900940831     C**         C1POS     IFGT D1PRG
133000940831     C**                   MOVE C1POS     W1POS
133100940831     C**                   END
133200940831     C                   MOVE      *ON           *IN99
133300940831     C                   MOVE      *ON           $INZS1
133400940831     C*
133500940202     C*
133600940202     C     $ORD          WHENEQ    2
133700940202     C*
133800941010    >C     C1POS2        IFGT      D1GRP
133900941124    >C                   MOVE      C1POS2        W1POS2
134000940202     C                   END
134100940202     C*
134200940202     C                   MOVE      *ON           *IN99
134300940223     C                   MOVE      *ON           $INZS1
134400940202     C*
1345009402022-   C                   ENDSL
134600940202     C*
1347009402021-   C                   ENDIF
134800940202     C*
134900940131     C                   ENDSR
135000940131     C/EJECT
135100940131     C************************************************************
135200940131     C* CONTROLLO OPZIONI LISTA
135300940131     C************************************************************
135400940131     C     CTRS1         BEGSR
135500940131     C*
135600940202     C                   MOVEL     *OFF          $ESCI
135700940201     C                   SETOFF                                       99
135800940131     C                   Z-ADD     0             S1OPZ
135900940131     C*
136000940127     C* Leggo il sfl solo se ci sono rcd
1361009401311    C     WMAX          IFGT      0
136200940131     C                   READC     FMTS1                                  21
136300940127     C*
136400940131     C* esce se fine sfl o errore che richiede l'uscita
1365009401312    C     *IN21         DOWEQ     *OFF
136600940131     C     $ESCI         ANDEQ     *OFF
136700940201     C                   Z-ADD     S1NRR         C1RCD
136800940131     C* ctrl su riga
136900940131     C                   EXSR      RECS1
137000940131     C* gestione opzioni
1371009401313    C     S1OPZ         IFNE      0
137200940201     C     *IN99         ANDEQ     *OFF
137300940131     C                   EXSR      OPZS1
1374009401273-   C                   ENDIF
137500941125     C* se rilevato errore, attivo sflnxtchg
1376009402013    C     *IN99         IFEQ      *ON
137700940131     C                   MOVE      *ON           *IN32
1378009402233-   C                   ENDIF
137900940223     C*
138000940131     C                   Z-ADD     0             S1OPZ
138100940223     C*
138200940127     C                   UPDATE    FMTS1
138300941125     C                   MOVE      *OFF          *IN32
138400940223     C*
138500940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1386009401313    C     $ESCI         IFEQ      *OFF
138700940131     C                   READC     FMTS1                                  21
138800940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1389009402014    C     *IN21         IFEQ      *ON
139000940201     C                   MOVE      WINZS1        $INZS1
139100940204     C* calcolo pagina a cui deve posizionarsi
139200940728     C     WSFL          IFGT      0
139300940728     C                   EXSR      CLCPAG
139400940728     C                   ELSE
139500940728     C                   Z-ADD     SFLPAG        WPAG
139600940728     C                   ENDIF
1397009402014-   C                   ENDIF
1398009402013-   C                   ENDIF
139900940131     C*
1400009401272-   C                   ENDDO
140100940127     C*
1402009401311-   C                   ENDIF
140300940131     C*
140400940127     C                   ENDSR
140500940127     C/EJECT
140600940127     C************************************************************
140700940131     C* CONTROLLO CAMPI I/O RIGA LISTA
140800940127     C************************************************************
140900940131     C     RECS1         BEGSR
141000940131     C*
141100940201     C* reset indicatori DSPATR
141200940201     C                   MOVE      *ALL'0'       IN4049           10
141300940201     C                   MOVEA     IN4049        *IN(40)
141400940201     C*
141500940131     C* Controllo se Utente Autorizzato alla scelta
141600940202     C     S1OPZ         IFNE      0
141700940131     C                   Z-ADD     1             X
141800940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
141900940211     C     *IN21         IFEQ      *OFF
142000940223     C     $FL1(X)       OREQ      'N'
142100940131     C                   MOVE      *ON           $ESCI
142200940201     C                   SETON                                        504099
142300940131     C                   ENDIF
142400940202     C                   ENDIF
142500940131     C*
142600940131     C                   ENDSR
142700940224     C/EJECT
142800940131     C************************************************************
142900940131     C* GESTIONE OPZIONI LISTA
143000940131     C************************************************************
143100940131     C     OPZS1         BEGSR
143200940201     C*
1433009402251    C                   SELECT
143400940225     C*
143500940225     C* se opz=Selezione passo la chiave al pgm chiamante
143600940225     C     S1OPZ         WHENEQ    1
143700941007    >C* << passare nei campi appositi della DS A003DS il valore
143800940314    >C*    della chiave della VL univoca (se c'è) >>
143900941010    >C                   MOVE      S1PRF         PRF003
144000941010    >C                   MOVE      S1SOC         SOC003
144100940225     C                   MOVE      *ON           $ESCI
144200940225     C                   MOVE      *ON           $FINE
144300940909     C*
144400940909     C                   MOVE      *HIVAL        $LASTV
144500940225     C*
144600941011     C     S1OPZ         WHENEQ    11
144700941011    >C                   RESET                   A005DS
144800941011    >C* << passare nei campi appositi della DS D004DS il valore
144900941011    >C*    della chiave della VL univoca (se c'è) >>
145000941011    >C                   MOVEL     S1OPZ         OPZ005
145100941011    >C                   MOVEL     S1SOC         SOC005
145200941011    >C                   MOVEL     S1PRF         PRF005
145300941011    >C                   MOVEL     S1GRP         GRP005
145400941011     C*
145500941011    >C                   MOVEL     A005DS        KPJBU
145600941011    >C                   CALL      'ANA005R'
145700941011     C                   PARM                    KPJBA
145800941011    >C                   MOVEL     KPJBU         A005DS
145900941011     C*
146000940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1461009402251O   C                   OTHER
146200940225     C*
146300941007    >C                   RESET                   A004DS
146400941010    >C* << passare nei campi appositi della DS D004DS il valore
146500940314    >C*    della chiave della VL univoca (se c'è) >>
146600941010    >C                   MOVEL     S1OPZ         OPZ004
146700941010    >C                   MOVEL     S1SOC         SOC004
146800941010    >C                   MOVEL     S1PRF         PRF004
146900941010    >C                   MOVEL     S1GRP         GRP004
147000940314     C*
147100941007    >C                   MOVEL     A004DS        KPJBU
147200941007    >C                   CALL      'ANA004R'
147300940314     C                   PARM                    KPJBA
147400941007    >C                   MOVEL     KPJBU         A004DS
147500940201     C*
147600940223     C* ritorno da PGM gestione
147700941007    >C                   EXSR      RTN004
147800940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1479009402252    C     *IN99         IFEQ      *ON
148000940223     C                   SETON                                        40
1481009402252-   C                   ENDIF
148200940131     C*
148300940506     C                   MOVE      *HIVAL        $LASTV
1484009402251-   C                   ENDSL
148500940225     C*
148600940131     C                   ENDSR
148700940223     C/EJECT
148800940223     C************************************************************
148900940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
149000940223     C************************************************************
149100941007    >C     RTN004        BEGSR
149200940223     C*
149300940223     C* modo di ritorno
149400940223     C*
1495009402231    C                   SELECT
149600940314    >C* << questi modi di utilizzo dei valori di ritorno dal
149700940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
149800940314    >C*    proposte, normalmente sempre valide, ma modificabili
149900940314    >C*    per situazioni particolari >>
150000940223     C* 1 = F3
150100941007    >C     RET004        WHENEQ    '1'
150200940224     C                   MOVE      *ON           $ESCI                          sospese READC
150300940223     C                   MOVE      *ON           $FINE                          fine pgm
150400940223     C* 2 = F12
150500941007    >C     RET004        WHENEQ    '2'
150600940223     C                   MOVE      *ON           $ESCI                          sospese READC
150700940223     C*
1508009402231-   C                   ENDSL
150900940223     C*
151000940223     C* operazione eseguite dal pgm chiamato
151100940223     C*
1512009402231    C                   SELECT
151300940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
151400941007    >C     OPR004        WHENEQ    '1'
151500940223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
151600940223     C*
1517009402231-   C                   ENDSL
151800940223     C*
151900940223     C* funzione non eseguibile per errore :
152000940223     C*
1521009402231    C                   SELECT
152200940223     C* 1 = funzione richiamata chiusa in errore
152300940316    >C*  eventualmente gestire altri codici di errore
152400941007    >C     ERR004        WHENEQ    '1'
152500940223     C                   MOVE      *ON           $ESCI                          sospese READC
152600940223     C                   SETON                                        5299      emiss.SFLMSGID
152700940223     C*
1528009402231-   C                   ENDSL
152900940223     C*
153000940223     C                   ENDSR
153100940223     C/EJECT
153200940131     C************************************************************
153300940131     C* GESTIONE VALORI ASSUNTI
153400940131     C************************************************************
153500940131     C     GESD2         BEGSR
153600940131     C*
153700940223     C     $INZD2        IFEQ      *ON
153800940131     C                   EXSR      INZD2
153900940223     C                   MOVE      *OFF          $INZD2
154000940131     C                   END
154100940202     C*
154200940202     C     $LASTV        IFNE      'D2'
154300940923     C                   EXSR      GESPIE
154400940202     C                   MOVE      $LASTV        $LASTG
154500940202     C                   MOVE      'D2'          $LASTV
154600940707     C                   ENDIF
154700940131     C*
154800940707     C* se esistono errori sulla videata
154900940707     C* emetto la write del formato a indicatori spenti per vedere
155000940707     C* le eventuali decodifiche
155100940707     C     *IN99         IFEQ      *ON
155200940707     C                   MOVEA     *IN           WIN
155300940707     C                   MOVE      *ALL'0'       IN5098           49
155400940707     C                   MOVEA     IN5098        *IN(50)
155500940707     C                   WRITE     FMTD2
155600940707     C                   MOVEA     WIN           *IN
155700940707     C                   ENDIF
155800940707     C*
155900940131     C                   EXFMT     FMTD2
156000940131     C*
156100940131     C                   SELECT
156200940131     C* F3=Fine
156300940506     C     $TASTO        WHENEQ    F03
156400940309     C                   EXSR      F03D2
156500940131     C* F12=Ritorno
156600940506     C     $TASTO        WHENEQ    F12
156700940309     C                   EXSR      F12D2
156800940916     C* F24=Altri tasti
156900940916     C     $TASTO        WHENEQ    F24
157000940916     C                   EXSR      F24D2
157100940131     C*
157200940131     C                   OTHER
157300940131     C                   EXSR      CTRD2
1574009401312    C     *IN99         IFEQ      *OFF
157500940211     C                   MOVE      $LASTG        $GEST
1576009401313    C                   SELECT
157700940202     C     $LASTG        WHENEQ    'S1'
157800940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
157900940314     C     $ORD          IFNE      D2ORD
158000940223     C                   MOVE      *ON           $INZS1
158100940302     C                   MOVE      *BLANK        W1POS
158200941124     C                   Z-ADD     0             W1POS2
158300940131     C                   END
158400940202     C     $LASTG        WHENEQ    'D1'
158500940318     C* per ora niente
1586009401313-   C                   ENDSL
158700940131     C* definisco i nuovi valori di default
158800940201     C                   Z-ADD     D2LOP         $LOP
158900940131     C                   Z-ADD     D2ORD         $ORD
159000941124     C                   SELECT
159100941124     C     $ORD          WHENEQ    1
159200941124     C                   SETON                                            01
159300941124     C                   SETOFF                                           02
159400941124     C     $ORD          WHENEQ    2
159500941124     C                   SETON                                            02
159600941124     C                   SETOFF                                           01
159700941124     C                   ENDSL
159800940609    >C* << elenco altri dati default >>
1599009401312-   C                   END
160000940131     C*
160100940131     C                   ENDSL
160200940131     C*
160300940131     C                   ENDSR
160400940223     C/EJECT
160500940131     C************************************************************
160600940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
160700940131     C************************************************************
160800940201     C     INZD2         BEGSR
160900940131     C*
161000940131     C                   CLEAR                   FMTD2
161100940201     C                   MOVEL     $LOP          D2LOP
161200940831     C                   MOVE      $ORD          D2ORD
161300940314    >C* << elenco altri dati default >>
161400940131     C*
161500940131     C                   ENDSR
161600940309     C/EJECT
161700940309     C************************************************************
161800940309     C* GESTIONE F03 VIDEO D2
161900940309     C************************************************************
162000940309     C     F03D2         BEGSR
162100940309     C*
162200940309     C                   MOVE      *ON           $FINE
162300940325     C* fine programma
162400940325     C                   EXSR      ENDPGM
162500940309     C*
162600940309     C                   ENDSR
162700940309     C/EJECT
162800940309     C************************************************************
162900940309     C* GESTIONE F12 VIDEO D2
163000940309     C************************************************************
163100940309     C     F12D2         BEGSR
163200940309     C*
163300940309     C                   MOVE      $LASTG        $GEST
163400940309     C*
163500940309     C                   ENDSR
163600940916     C/EJECT
163700940916     C************************************************************
163800940916     C* GESTIONE F24 VIDEO D2
163900940916     C************************************************************
164000940916     C     F24D2         BEGSR
164100940916     C*
164200940916     C                   MOVEA     $FX3          $FX
164300940916     C                   MOVEA     $FC3          $FC
164400940916     C                   MOVEA     $FR3          $FR
164500940916     C                   Z-ADD     $ULKD2        $ULK
164600940929     C                   Z-ADD     56            $L01
164700940929     C                   Z-ADD     56            $L02
164800940916     C                   EXSR      RIEKEY
164900940916     C                   Z-ADD     $ULK          $ULKD2            3 0
165000940916     C                   MOVEA     $FC           $FC3
165100940916     C                   MOVEL     $KEY1         Z3KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
165200940916     C                   MOVEL     $KEY2         Z3KE2
165300940916     C                   WRITE     FMTZ3
165400940916     C*
165500940916     C                   ENDSR
165600940223     C/EJECT
165700940131     C************************************************************
165800940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
165900940131     C************************************************************
166000940131     C     CTRD2         BEGSR
166100940131     C*
166200940131     C                   SETOFF                                       99
166300940131     C* non imposto pos. del cursore
166400940131     C                   Z-ADD     0             H1RIGA
166500940131     C                   Z-ADD     0             H1COLO
166600940131     C*
166700940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
166800940201     C     D2LOP         IFEQ      0
166900940131     C                   SETON                                          5099
167000940131     C                   END
167100940131     C*
167200940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
167300940131     C     D2ORD         IFEQ      0
167400940131     C                   SETON                                          5199
167500940131     C                   END
167600940314    >C* << elenco controlli altri dati default >>
167700940131     C*
167800940131     C                   ENDSR
167900940223     C/EJECT
168000940201     C************************************************************
168100940201     C* AGGIORNAMENTO VALORI ASSUNTI
168200940201     C************************************************************
168300940201     C     AGGMPE        BEGSR
168400940201     C*
168500940201     C                   CLEAR                   ANMPE000
168600940201     C                   MOVEL     XSCSOC        MPESOC
168700940201     C                   MOVEL     DSPGM         MPEPGM
168800940201     C                   MOVEL     KNMUS         MPEPRF
168900941013     C*
169000941013     C                   SELECT
169100941013     C     OPZ003        WHENEQ    '01'
169200941013     C                   MOVEL     'PX'          MPETIP
169300941013     C                   OTHER
169400941013     C                   MOVEL     'PV'          MPETIP
169500941013     C                   ENDSL
169600940201     C     K4MPE2        CHAIN     ANMPE02L                           21
169700940201     C                   MOVEL     DSMPE         MPEPAR
169800940201     C     *IN21         IFEQ      *ON
169900940201     C                   WRITE     ANMPE000
170000940201     C                   ELSE
170100940201     C                   UPDATE    ANMPE000
170200940201     C                   END
170300940201     C*
170400940201     C                   ENDSR
170500940223     C/EJECT
170600940131     C************************************************************
170700940131     C* OPERAZIONI INIZIALI
170800940131     C************************************************************
170900940131     C     *INZSR        BEGSR
171000940131     C*
171100940127     C* Reperimento parametri
171200940127     C*
171300940117     C     *ENTRY        PLIST
171400940117     C                   PARM                    KPJBA
171500941103      *
171600941103      * Reperimento dati società
171700941103      *
171800941103     C                   MOVEL     'SOC001'      TIPXSC
171900941103     C                   MOVEL     *BLANK        SOCXSC
172000941103     C                   EXSR      REPSOC
172100941103     C     RTNXSC        IFNE      '1'
172200941103     C                   MOVEL     XSOCDS        SOC001
172300941103     C                   ELSE
172400941111     C                   RETURN
172500941103     C                   ENDIF
172600940314     C* Reperimento autorizzazioni
172700940314     C*
172800940314    >C* << nel caso l'anagrafica non gestisse le autorizzazioni
172900940909    >C*    asteriscare la chiamata alla subroutine REPAUT >>
173000941007<<<<>C*                    EXSR REPAUT
173100940225     C*
173200940223     C* Reperimento opzioni
173300940127     C*
173400940128     C                   EXSR      REPOPZ
173500940506     C*
173600940506     C* Reperimento tasti di funzione
173700940506     C*
173800940506     C                   EXSR      REPKEY
173900940506     C*
174000940127     C*
174100940117     C                   ENDSR
174200940225     C/EJECT
174300940225     C************************************************************
174400940225     C* REPERIMENTO AUTORIZZAZIONI UTENTE
174500940225     C************************************************************
174600941007     C*          REPAUT    BEGSR
174700940225     C*
174800941007     C*                    CLEARXAUT
174900941007     C*
175000941007     C*                    Z-ADD0         XCAGRP           GRUPPO
175100941007     C*                    MOVELKNMUS     XCAPRF           PROFILO
175200941007    >C*                    MOVEL'OBJALL'  XCAFNC           FUNZIONE
175300941007     C*                    MOVEL*ZERO     XCAVFU           VARIABILE
175400941007     C*                    MOVE 'CK'      XCATCK           TIPO CTRL
175500941007     C*
175600941007     C*                    CALL 'XCHKAUT'
175700941007     C*                    PARM           XAUT
175800941007     C* utente non abilitato
175900941007     C*          XCARTN    IFEQ 0
176000941007    >C*                    MOVELXCADDS    AUTDS
176100941007     C*                    ELSE
176200941007    >C*                    CLEARAUTDS
176300941007     C*                    END
176400941007     C*
176500941007     C*                    ENDSR
176600940223     C/EJECT
176700940223     C************************************************************
176800940223     C* INIZIALIZZAZIONE VARIABILI
176900940223     C************************************************************
177000940223     C     INZVAR        BEGSR
177100940223     C*
177200940729     C*
177300940729     C* Dati da PGM chiamante
177400940729     C*
177500941007    >C                   MOVEL     KPJBU         A003DS
177600941026     C*
177700941026     C* Reperimento valori default utente
177800941026     C*
177900941026     C                   EXSR      REPDFT
178000940506     C*
178100940506     C* Definizione delle opzioni utilizzabili a pgm
178200940506     C*
178300940506     C                   EXSR      DEFOPZ
178400940223     C* Variabili per gestione videate
178500940223     C*
178600940223     C                   MOVE      *OFF          $FINE                          fine pgm
178700940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
178800940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
178900940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
179000940223     C                   MOVE      *OFF          $EFILE                         memo fine file
179100940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
179200940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
179300940509     C                   MOVE      *HIVAL        $LASTV
179400940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
179500940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
179600940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
179700940223     C*
179800940318     C                   MOVE      *ON           $INZD1
179900940318     C                   MOVE      *ON           $INZS1
180000940831     C*
180100940223     C                   SELECT
180200940223     C     $LOP          WHENEQ    1
180300940223     C                   MOVE      'D1'          $GEST
180400940223     C     $LOP          WHENEQ    2
180500940223     C                   MOVE      'S1'          $GEST
180600941104     C                   EXSR      INZD1
180700941104     C                   MOVE      *OFF          $INZD1
180800940223     C                   ENDSL
180900940223     C*
181000940223     C* Variabili appoggio
181100940223     C*
181200940223     C                   Z-ADD     SFLPAG        WPAG
181300940223     C*
181400940223     C* Valorizzazione campi univoci testate
181500940223     C*
181600940223     C                   CLEAR                   FMTT1
181700940223     C                   MOVEL     KNSIF         NOMSIF
181800941110     C                   MOVEL     XSCDSI        NOMDIT
181900941027     C                   MOVEL     DSPGM         NOMPGM
182000941011     C* Accendo indicatori per condizioni su campo posizionamento C1POS
182100941011     C                   SELECT
182200941011     C     $ORD          WHENEQ    1
182300941011     C                   SETON                                            01
182400941124     C                   SETOFF                                           02
182500941011     C     $ORD          WHENEQ    2
182600941011     C                   SETON                                            02
182700941124     C                   SETOFF                                           01
182800941011     C                   ENDSL
182900940223     C*
183000940223     C                   ENDSR
183100940223     C/EJECT
183200940127     C************************************************************
183300940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
183400940127     C************************************************************
183500940131     C     REPDFT        BEGSR
183600940127     C*
183700940201     C                   MOVEL     XSCSOC        MPESOC
183800940201     C                   MOVEL     DSPGM         MPEPGM
183900940131     C                   MOVEL     KNMUS         MPEPRF
184000941013     C*
184100941013     C                   SELECT
184200941013     C     OPZ003        WHENEQ    '01'
184300941013     C                   MOVEL     'PX'          MPETIP
184400941013     C                   OTHER
184500941013     C                   MOVEL     'PV'          MPETIP
184600941013     C                   ENDSL
184700940202     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
184800940131     C     *IN21         IFEQ      *OFF
184900940131     C                   MOVEL     MPEPAR        DSMPE
185000940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
185100940131     C* i suoi valori di default vengono decisi a pgm
185200940131     C                   ELSE
185300940201     C                   Z-ADD     1             $LOP
185400940131     C                   Z-ADD     1             $ORD
185500941010    >C* << elenco altri dati default >>
185600940131     C                   END
185700940131     C*
185800940131     C                   ENDSR
185900940223     C/EJECT
186000940131     C************************************************************
186100940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
186200940131     C************************************************************
186300940225     C     DEFOPZ        BEGSR
186400940131     C*
186500940225     C* Questa routine deve valorizzare la schiera $FL1
186600940225     C* con S o N a seconda della utilizzabilità o meno
186700940225     C* dell'opzione correlata
186800940225     C*
186900940225     C* Inizialmente si considerano tutte le opzioni come valide
187000940317     C     1             DO        $DIMOP        X
187100940225     C                   MOVE      'S'           $FL1(X)
187200940225     C                   END
187300940715     C*
187400940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
187500940715     C* con S o N a seconda della utilizzabilità o meno
187600940715     C* del tasto funzione correlato
187700940715     C*
187800940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
187900940715     C* di compilazione
188000940715     C                   RESET                   $FC1
188100940715     C                   RESET                   $FR1
188200940715     C                   RESET                   $FC2
188300940715     C                   RESET                   $FR2
188400940919     C                   RESET                   $FC3
188500940919     C                   RESET                   $FR3
188600940225     C*
188700940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
188800940225     C* invalido alcune opzioni
188900940225     C                   SELECT
189000940225     C* Se pgm richiamato in ricerca disattivo :
189100941007    >C     OPZ003        WHENEQ    '01'
189200940307     C* F10=Immissione
189300940506     C                   Z-ADD     10            X
189400940720     C                   MOVE      '0'           $FC2(X)
189500940603     C* F3=Fine
189600940603     C                   Z-ADD     3             X
189700940720     C                   MOVE      '0'           $FC2(X)
189800940307     C* 2=Modifica
189900940307     C                   Z-ADD     1             X
190000940225     C     02            LOOKUP    $OP1(X)                                21
190100940225     C     *IN21         IFEQ      *ON
190200940225     C                   MOVE      'N'           $FL1(X)
190300940225     C                   ENDIF
190400940225     C* 3=Copia
190500940225     C                   Z-ADD     1             X
190600940225     C     03            LOOKUP    $OP1(X)                                21
190700940225     C     *IN21         IFEQ      *ON
190800940225     C                   MOVE      'N'           $FL1(X)
190900940225     C                   ENDIF
191000940225     C* 4=Annullamento
191100940225     C                   Z-ADD     1             X
191200940225     C     04            LOOKUP    $OP1(X)                                21
191300940225     C     *IN21         IFEQ      *ON
191400940225     C                   MOVE      'N'           $FL1(X)
191500940225     C                   ENDIF
191600940314    >C* << aggiungere nollo stesso modo le altre opzioni da
191700940314    >C*    disattivare (quelle specifiche per ogni pgm) >>
191800940225     C* Se pgm richiamato in interrogazione disattivo :
191900941007    >C     OPZ003        WHENEQ    '05'
192000940307     C* F10=Immissione
192100940615     C                   Z-ADD     10            X
192200940720     C                   MOVE      '0'           $FC2(X)
192300940225     C* 1=Selezione
192400940225     C                   Z-ADD     1             X
192500940225     C     01            LOOKUP    $OP1(X)                                21
192600940225     C     *IN21         IFEQ      *ON
192700940225     C                   MOVE      'N'           $FL1(X)
192800940225     C                   ENDIF
192900940225     C* 2=Modifica
193000940225     C                   Z-ADD     1             X
193100940225     C     02            LOOKUP    $OP1(X)                                21
193200940225     C     *IN21         IFEQ      *ON
193300940225     C                   MOVE      'N'           $FL1(X)
193400940225     C                   ENDIF
193500940225     C* 3=Copia
193600940225     C                   Z-ADD     1             X
193700940225     C     03            LOOKUP    $OP1(X)                                21
193800940225     C     *IN21         IFEQ      *ON
193900940225     C                   MOVE      'N'           $FL1(X)
194000940225     C                   ENDIF
194100940225     C* 4=Annullamento
194200940225     C                   Z-ADD     1             X
194300940225     C     04            LOOKUP    $OP1(X)                                21
194400940225     C     *IN21         IFEQ      *ON
194500940225     C                   MOVE      'N'           $FL1(X)
194600940225     C                   ENDIF
194700940225     C*
194800940225     C                   OTHER
194900940225     C* Se pgm non richiamato, disattivo subito :
195000940225     C* 1=Selezione
195100940225     C                   Z-ADD     1             X
195200940225     C     01            LOOKUP    $OP1(X)                                21
195300940225     C     *IN21         IFEQ      *ON
195400940225     C                   MOVE      'N'           $FL1(X)
195500940225     C                   ENDIF
195600940225     C* poi, disattivo a seconda delle autorizzazioni :
195700940314    >C*********  AUTXXX    IFEQ 'N'
195800940314    >C*********            Z-ADD1         X
195900940314    >C*********  'XX'      LOKUP$OP1,X                   21
196000940314    >C*********  *IN21     IFEQ *ON
196100940314    >C*********            MOVE 'N'       $FL1,X
196200940314    >C*********            ENDIF
196300940314    >C*********            ENDIF
196400940225     C*
196500940225     C                   ENDSL
196600940225     C*
196700940225     C                   ENDSR
196800940225     C/EJECT
196900940225     C************************************************************
197000940225     C* REPERIMENTO OPZIONI
197100940225     C************************************************************
197200940225     C     REPOPZ        BEGSR
197300940225     C*
197400940314     C     1             DO        $DIMOP        X
197500940128     C*
197600940223     C     $MS1(X)       IFNE      *BLANK
197700940128     C*
197800940223     C                   MOVEL     $MS1(X)       IDMSG
197900940128     C                   CALL      'XRTVM'
198000940201     C                   PARM                    IDMSG            27
198100940201     C                   PARM                    TXTMSG          100
198200940201     C                   PARM                    ERRMSG            1
198300940128     C*
198400940128     C     ERRMSG        IFNE      *ON
198500940223     C                   MOVEL     TXTMSG        $TX1(X)
198600940128     C                   ELSE
198700940223     C                   MOVEL     *ALL'?'       $TX1(X)
198800940128     C                   END
198900940128     C*
199000940128     C                   END
199100940128     C*
199200940128     C                   END
199300940128     C*
199400940128     C                   ENDSR
199500940506     C/EJECT
199600940506     C************************************************************
199700940506     C* REPERIMENTO TASTI DI FUNZIONE
199800940506     C************************************************************
199900940506     C     REPKEY        BEGSR
200000940506     C                   MOVEA     $FM1          $FM
200100950906     C                   MOVEA     $FC1          $FC
200200940506     C                   EXSR      REPKKK
200300940506     C                   MOVEA     $FX           $FX1
200400940506     C*
200500940506     C                   MOVEA     $FM2          $FM
200600950906     C                   MOVEA     $FC2          $FC
200700940506     C                   EXSR      REPKKK
200800940506     C                   MOVEA     $FX           $FX2
200900940919     C*
201000940919     C                   MOVEA     $FM3          $FM
201100950906     C                   MOVEA     $FC3          $FC
201200940919     C                   EXSR      REPKKK
201300940919     C                   MOVEA     $FX           $FX3
201400940919     C                   ENDSR
201500940506     C/EJECT
201600940506     C************************************************************
201700940506     C* REPERIMENTO TASTI DI FUNZIONE
201800940506     C************************************************************
201900940506     C     REPKKK        BEGSR
202000940506     C*
202100940506     C                   CLEAR                   $FX
202200940506     C     1             DO        24            X
202300940506     C*
202400940506     C     $FM(X)        IFNE      *BLANK
202500950906     C     $FC(X)        ANDNE     '0'
202600940506     C*
202700940506     C                   MOVEL     $FM(X)        IDMSG
202800940506     C                   CALL      'XRTVM'
202900940506     C                   PARM                    IDMSG            27
203000940506     C                   PARM                    TXTMSG          100
203100940506     C                   PARM                    ERRMSG            1
203200940506     C*
203300940506     C     ERRMSG        IFNE      *ON
203400940506     C                   MOVEL     TXTMSG        $FX(X)
203500940506     C                   ELSE
203600940506     C                   MOVEL     *ALL' '       $FX(X)
203700940506     C                   END
203800940506     C*
203900940506     C                   END
204000940506     C*
204100940506     C                   END
204200940506     C*
204300940506     C                   ENDSR
204400940223     C/EJECT
204500940128     C************************************************************
204600940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
204700940128     C************************************************************
204800940223     C     DEFCAM        BEGSR
204900940128     C*
205000940223     C* klist
205100940127     C*
205200941007    >C* ANABS02L
205300950302    >C     K2ABS2        KLIST
205400941007    >C                   KFLD                    ABSGRP
205500941007    >C                   KFLD                    ABSPRF
205600940128     C*
205700940128     C* ANMPE02L
205800940128     C     K4MPE2        KLIST
205900940128     C                   KFLD                    MPESOC
206000940201     C                   KFLD                    MPEPGM
206100940128     C                   KFLD                    MPETIP
206200940128     C                   KFLD                    MPEPRF
206300940223     C*
206400940223     C* Variabili per gestione videate
206500940223     C*
206600940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
206700940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
206800940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
206900940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
207000940223     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
207100940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
207200940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
207300940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
207400940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
207500940223     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
207600940224     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
207700940224     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
207800940223     C*
207900940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
208000940223     C*
208100940223     C     *LIKE         DEFINE    C1RCD         S1NRR
208200940223     C     *LIKE         DEFINE    C1NRR         WSFL
208300940223     C     *LIKE         DEFINE    C1RCD         WMAX
208400940223     C     *LIKE         DEFINE    C1RCD         WPAG
208500940223     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
208600940608     C     *LIKE         DEFINE    *INKR         WINKR                                      ne S1
208700940223     C                   MOVE      *BLANK        WWGRP             3
208800940223     C                   Z-ADD     0             CURR              2 0
208900940223     C                   Z-ADD     0             CURC              2 0
209000940223     C                   MOVE      *BLANK        WIN              99
209100940314     C     *LIKE         DEFINE    C1POS         W1POS
209200941010     C     *LIKE         DEFINE    C1POS2        W1POS2
209300940314     C*
209400940314     C* Variabili appoggio specifiche della singola anagrafica
209500940314     C*
209600940314     C* campi per gestione F4=Ricerca
209700941010    >C**         *LIKE     DEFN ANAxxx    WWxxx
209800941010    >C***        *LIKE     DEFN ANAMOD    WWMOD
209900940223     C* campi per parzializzazioni su rcd (solo alfanumerici)
210000941010    >C     *LIKE         DEFINE    ABSPRF        PRF999
210100941010    >C*****      *LIKE     DEFN ANAOBJ    OBJ999
210200940223     C*
210300940223     C* Indici
210400940223     C*
210500940223     C                   Z-ADD     0             X                 3 0
210600940223     C                   Z-ADD     0             Y                 3 0
210700940127     C*
210800940117     C                   ENDSR
210900941103     C/EJECT
211000941103      ************************************************************
211100941103      * Reperimento dati società
211200941103      ************************************************************
211300941103     C     REPSOC        BEGSR
211400941103      *
211500941103     C                   CALL      'XSOC'
211600941103     C                   PARM                    TIPXSC            6
211700941103     C                   PARM                    SOCXSC            3
211800941103     C                   PARM                    CDSXSC            9 0
211900941103     C                   PARM                    MODXSC            3
212000941103     C                   PARM      *BLANKS       RTNXSC            1
212100941103     C                   PARM                    XSOCDS
212200941103     C                   PARM                    KPJBA
212300941103      *
212400941103     C                   ENDSR
212500950906**
212600940131PROMSG    *LIBL     OPZ0004
212700941011PROMSG    *LIBL     OPZ0027
212800940201**
21290094022504S      <Annullamento
21300094101111S      <Abilitazioni Unità
213100940506** TASTI DI FUNZIONE  D1
213200940506PROMSG    *LIBL     KEY0001  01
213300940506PROMSG    *LIBL     KEY0002  02
213400940506PROMSG    *LIBL     KEY0003  03
213500940506PROMSG    *LIBL     KEY0004  04
213600940506PROMSG    *LIBL     KEY0005  05
213700940506PROMSG    *LIBL     KEY0006  06
213800940506PROMSG    *LIBL     KEY0007  07
213900940506PROMSG    *LIBL     KEY0008  08
214000940506PROMSG    *LIBL     KEY0009  09
214100940506PROMSG    *LIBL     KEY0010  10
214200940506PROMSG    *LIBL     KEY0011  11
214300940506PROMSG    *LIBL     KEY0012  12
214400940506PROMSG    *LIBL     KEY0013  13
214500940506PROMSG    *LIBL     KEY0014  14
214600940506PROMSG    *LIBL     KEY0015  15
214700940506PROMSG    *LIBL     KEY0016  16
214800940506PROMSG    *LIBL     KEY0017  17
214900940506PROMSG    *LIBL     KEY0018  18
215000940506PROMSG    *LIBL     KEY0019  19
215100940506PROMSG    *LIBL     KEY0020  20
215200940506PROMSG    *LIBL     KEY0021  21
215300940506PROMSG    *LIBL     KEY0022  22
215400940506PROMSG    *LIBL     KEY0023  23
215500940506PROMSG    *LIBL     KEY0024  24
215600940506**  ABILITAZIONE D1
2157009407200N  01
2158009407200N  02
2159009407201N  03
2160009407201N  04
2161009407200N  05
2162009407200N  06
2163009407200N  07
2164009407200N  08
2165009407200N  09
2166009407200N  10
2167009407200N  11
2168009407201N  12
2169009407200N  13
2170009407200N  14
2171009407200N  15
2172009407200N  16
2173009407200N  17
2174009407201N  18
2175009407200N  19
2176009407200N  20
2177009407200N  21
2178009407200N  22
2179009509060N  23   Impostare a '1' se si utilizzano opzioni
2180009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
218100940916** TASTI DI FUNZIONE  S1
218200940916PROMSG    *LIBL     KEY0001  01
218300940916PROMSG    *LIBL     KEY0002  02
218400940916PROMSG    *LIBL     KEY0003  03
218500940916PROMSG    *LIBL     KEY0004  04
218600940916PROMSG    *LIBL     KEY0005  05
218700940916PROMSG    *LIBL     KEY0006  06
218800940916PROMSG    *LIBL     KEY0007  07
218900940916PROMSG    *LIBL     KEY0008  08
219000940916PROMSG    *LIBL     KEY0009  09
219100940916PROMSG    *LIBL     KEY0010  10
219200940916PROMSG    *LIBL     KEY0011  11
219300940916PROMSG    *LIBL     KEY0012  12
219400940916PROMSG    *LIBL     KEY0013  13
219500940916PROMSG    *LIBL     KEY0014  14
219600940916PROMSG    *LIBL     KEY0015  15
219700940916PROMSG    *LIBL     KEY0016  16
219800940916PROMSG    *LIBL     KEY0017  17
219900940916PROMSG    *LIBL     KEY0018  18
220000940916PROMSG    *LIBL     KEY0019  19
220100940916PROMSG    *LIBL     KEY0020  20
220200940916PROMSG    *LIBL     KEY0021  21
220300940916PROMSG    *LIBL     KEY0022  22
220400940916PROMSG    *LIBL     KEY0023  23
220500940916PROMSG    *LIBL     KEY0024  24
220600940916**  ABILITAZIONE S1
2207009409160N  01
2208009409160N  02
2209009409161N  03
2210009409160N  04
2211009409160N  05
2212009409160N  06
2213009409160N  07
2214009409160N  08
2215009409160N  09
2216009409161N  10
2217009409160N  11
2218009409161N  12
2219009409160N  13
2220009409160N  14
2221009409160N  15
2222009409160N  16
2223009409161N  17
2224009409161N  18
2225009409160N  19
2226009409160N  20
2227009409160N  21
2228009409160N  22
2229009509061N  23   Impostare a '1' se si utilizzano opzioni
2230009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
223100940916** TASTI DI FUNZIONE  D3
223200940506PROMSG    *LIBL     KEY0001  01
223300940506PROMSG    *LIBL     KEY0002  02
223400940506PROMSG    *LIBL     KEY0003  03
223500940506PROMSG    *LIBL     KEY0004  04
223600940506PROMSG    *LIBL     KEY0005  05
223700940506PROMSG    *LIBL     KEY0006  06
223800940506PROMSG    *LIBL     KEY0007  07
223900940506PROMSG    *LIBL     KEY0008  08
224000940506PROMSG    *LIBL     KEY0009  09
224100940506PROMSG    *LIBL     KEY0010  10
224200940506PROMSG    *LIBL     KEY0011  11
224300940506PROMSG    *LIBL     KEY0012  12
224400940506PROMSG    *LIBL     KEY0013  13
224500940506PROMSG    *LIBL     KEY0014  14
224600940506PROMSG    *LIBL     KEY0015  15
224700940506PROMSG    *LIBL     KEY0016  16
224800940506PROMSG    *LIBL     KEY0017  17
224900940506PROMSG    *LIBL     KEY0018  18
225000940506PROMSG    *LIBL     KEY0019  19
225100940506PROMSG    *LIBL     KEY0020  20
225200940506PROMSG    *LIBL     KEY0021  21
225300940506PROMSG    *LIBL     KEY0022  22
225400940506PROMSG    *LIBL     KEY0023  23
225500940506PROMSG    *LIBL     KEY0024  24
225600940916**  ABILITAZIONE D3
2257009407200N  01
2258009407200N  02
2259009407201N  03
2260009407200N  04
2261009407200N  05
2262009407200N  06
2263009407200N  07
2264009407200N  08
2265009407200N  09
2266009409160N  10
2267009407200N  11
2268009407201N  12
2269009407200N  13
2270009407200N  14
2271009407200N  15
2272009407200N  16
2273009409160N  17
2274009409160N  18
2275009407200N  19
2276009407200N  20
2277009407200N  21
2278009407200N  22
2279009509060N  23   Impostare a '1' se si utilizzano opzioni
2280009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
