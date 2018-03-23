000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200980708     H*PARMS CVTOPT(*DATETIME)
000300950403     H DATEDIT(*DMY)
000400941027      *----------*
000500941017      * ANA011R  *
000600940127      *----------*
000700940117      *
000800940127      *            GESTIONE AUTORIZZAZIONI APPLICATIVE
000900940117      *
001000940117      *
001100940128      *               MAPPA INDICATORI STD
001200940117      *
001300940223      * N09           CAMPO POSIZIONAMENTO RELATIVO A GRUPPO/PROFILO
001400940223      *  09           CAMPO POSIZIONAMENTO RELATIVO A FUNZIONE
001500940128      *  21           GENERICO OPERAZIONI I/O
001600940128      *  22           GENERICO ERRORE OPERAZIONI I/O
001700940128      *  30           SFLDSP
001800940128      * N31           SFLCLR
001900940128      *  31           SFLDSPCTL
002000940128      *  32           SFLNXTCHG
002100940128      *  33           SFLEND
002200940128      *  35           ROLLUP
002300940128      *  36           ROLLDOWN
002400940128      *  39           OF PRTF
002500940128      *  40 <---> 49  DSPATR ERRORI SU SFL
002600940128      *  50 <---> 98  ERRORI SU VIDEO
002700940209      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002800940209      *  99           INDIC. GENERALE DI ERRORE
002900940128     F*----------------------------------------------------*
003000940127     FANAUT01L  IF   E           K DISK
003100940131     F                                     RENAME(ANAUT000:ANAUT01)
003200940117     FANAUT02L  IF   E           K DISK
003300940117     F                                     RENAME(ANAUT000:ANAUT02)
003400940201     FANMPE02L  UF A E           K DISK
003500941017     FANA011D   CF   E             WORKSTN
003600941017     F                                     RENAME(A011D1:FMTD1)
003700941017     F                                     RENAME(A011D2:FMTD2)
003800941017     F                                     RENAME(A011D3:FMTD3)
003900941017     F                                     RENAME(A011S1:FMTS1)
004000941017     F                                     RENAME(A011C1:FMTC1)
004100941017     F                                     RENAME(A011T1:FMTT1)
004200941017     F                                     RENAME(A011Z1:FMTZ1)
004300941017     F                                     RENAME(A011Z2:FMTZ2)
004400940131     F                                     SFILE(FMTS1:S1NRR)
004500940201     F                                     INFDS(DSFMT)
004600940128     D*----------------------------------------------------*
004700950403     D $MS1            S             27    DIM(4) CTDATA PERRCD(1)
004800950403     D $TX1            S             50    DIM(4) ALT($MS1)
004900940223     D $OP1            S              2  0 DIM(4) CTDATA PERRCD(1)
005000940223     D $FL1            S              1    DIM(4) ALT($OP1)
005100950403     D $TX             S             50    DIM(50)
005200950403     D $FL             S              1    DIM(50)
005300940201     D OPZ             S             19    DIM(4)
005400940128     D*----------------------------------------------------*
005500940127     D* Passaggio Parametri
005600940127     D KPJBA         E DS
005700940201     D*-------------
005800941027     D* Passaggio Parametri PGM ANA001R
005900941027     D A001DS        E DS                  EXTNAME(ANA001DS)
006000941027    >D  RET001       E                     INZ('0')
006100941027    >D  OPR001       E                     INZ('0')
006200941027    >D  ERR001       E                     INZ('0')
006300941027     D* Passaggio Parametri PGM ANA007R
006400941027     D A007DS        E DS                  EXTNAME(ANA007DS)
006500941027    >D  RET007       E                     INZ('0')
006600941027    >D  OPR007       E                     INZ('0')
006700941027    >D  ERR007       E                     INZ('0')
006800940128     D*-------------
006900940209     D* Passaggio Parametri al PGM di gestione
007000941017     D A012DS          DS
007100940201     D* opzione richiesta
007200941017     D  OPZ012                 1      2
007300940209     D* modo di ritorno :
007400940209     D* 0 = F6 o altri tasti che non inibiscono il chiamante
007500940209     D* 1 = F3
007600940209     D* 2 = F12
007700941017     D  RET012                 3      3    INZ('0')
007800940209     D* operazione eseguite dal pgm chiamato :
007900940209     D* 0 = nessuna operazione
008000940209     D* 1 = eseguito aggiornamento
008100941017     D  OPR012                 4      4    INZ('0')
008200940209     D* funzione non eseguibile per errore :
008300940209     D* 0 = nessun errore
008400940209     D* 1 = funzione richiamata chiusa in errore
008500941017     D  ERR012                 5      5    INZ('0')
008600940201     D* chiavi del rcd da elaborare
008700941017     D  GRP012                 6      8  0 INZ(0)
008800941017     D  PRF012                 9     18
008900941017     D  FNC012                19     24
009000941017     D  VFU012                25     39
009100941017     D  UNI012                40     47
009200941017     D  SOC012                48     50
009300940201     D*-------------
009400940128     D* Valori assunti
009500941115     D DSMPE           DS                  INZ
009600940201     D  $LOP                   1      1  0
009700940131     D  $ORD                   2      2  0
009800940201     D*-------------
009900940201     D DSFMT           DS
010000940201     D  NRG                  370    370
010100940201     D  NCL                  371    371
010200940201     D  SFLNRR               378    379B 0
010300940201     D*-------------
010400940201     D* posizione cursore
010500940201     D CURSOR          DS
010600940209     D  RIRI                   1      2B 0 INZ
010700940201     D  R$$                    2      2
010800940209     D  COCO                   3      4B 0 INZ
010900940201     D  C$$                    4      4
011000940128     D*-------------
011100940131     D SFLPAG          C                   CONST(10)
011200940127     D*-------------
011300940127     D* Reperimento nome PGM
011400940127     D STATUS         SDS           333
011500940127     D  DSPGM            *PROC
011600941017     D* Passaggio Parametri al PGM ANA001R
011700941017    >D A009DS        E DS                  EXTNAME(ANA009DS)
011800941017    >D  RET009       E                     INZ('0')
011900941017    >D  OPR009       E                     INZ('0')
012000941017    >D  ERR009       E                     INZ('0')
012100941027      *
012200941027      * Parametri per ricerca codice ANATB00F
012300941027     D XTABDS          DS                  INZ
012400941027      * '  ' = Gestione
012500941027      * '01' = Ricerca
012600941027      * '02' = Gestione
012700941027      * '05' = Visualizzazione
012800941027      * '11' = Abilitazione
012900941027     D  OPZTAB                 1      2
013000941027      * 0 = ritorno normale
013100941027      * 1 = F3
013200941027      * 2 = F12
013300941027     D  RETTAB                 3      3
013400941027      * operazione eseguite dal pgm chiamato :
013500941027      * 0 = nessuna operazione
013600941027      * 1 = eseguito aggiornamento
013700941027     D  OPRTAB                 4      4
013800941027      * funzione non eseguibile per errore :
013900941027      * 0 = nessun errore
014000941027      * 1 = funzione richiamata chiusa in errore
014100941027      * Aggiungere eventuali altri codici di errore
014200941027     D  ERRTAB                 5      5
014300941027      * Azienda
014400941027     D  AZITAB                 6      8
014500941027      * Lingua
014600941027     D  LINTAB                 9      9
014700941027      * Codice scelto
014800941027     D  KEYTAB                10     24
014900941027      * riga cursore chiamante
015000941027     D  RIGTAB                25     26  0
015100941027      * colonna cursore chiamante
015200941027     D  COLTAB                27     28  0
015300941027     D ERRORE          C                   CONST('Codice errato')
015400941027     D DATA            C                   CONST('0001-01-01')
015500941103     D*-------------
015600941103     D* Dati di ambiente ottenuti da XSOC
015700941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
015800941103      * DS Interna per dati di output di XSOC
015900941103     D XSOCDS          DS          1000
016000941027      *----------------------------------------------------*
016100941027      *                MAIN LINE PROGRAM
016200941027      *----------------------------------------------------*
016300941027      *
016400941027      * inizializzazione variabili
016500940210     C                   EXSR      INZVAR
016600941027      *
016700940209     C     $FINE         DOWEQ     *OFF
016800940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
016900940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
017000940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
017100940117     C                   END
017200940117     C                   END
017300941103      *
017400941103     C                   EXSR      ENDPGM
017500941027      *
017600941103     C/EJECT
017700941103      ************************************************************
017800941103      * FINE PROGRAMMA
017900941103      ************************************************************
018000941103     C     ENDPGM        BEGSR
018100941103      *
018200941103      * aggiorno valori assunti
018300941103      *
018400941103     C                   EXSR      AGGMPE
018500941103      *
018600941103     C                   SETON                                            LR
018700941103      *
018800941103     C                   RETURN
018900941103      *
019000941103     C                   ENDSR
019100941027      ************************************************************
019200941103      * RICERCHE                                                 *
019300941027      ************************************************************
019400940127     C     SEARCH        BEGSR
019500941027      *
019600941027      * determino Riga/Colonna del cursore
019700940127     C                   MOVE      NRG           R$$
019800940127     C                   MOVE      NCL           C$$
019900940127     C                   Z-ADD     RIRI          CURR
020000940127     C                   Z-ADD     COCO          CURC
020100941027      *
0202009401271    C                   SELECT
020300941027      * Società
020400941027    >C     H1NMFL        WHENEQ    'D1SOC'
020500941027    >C                   RESET                   A007DS
020600941027    >C                   MOVEL     '01'          OPZ007
020700941027    >C                   MOVEL     A007DS        KPJBU
020800941027    >C                   CALL      'ANA007R'
020900941027     C                   PARM                    KPJBA
021000941027     C                   MOVEL     KPJBU         A007DS
021100941027     C     SOC007        IFNE      *BLANK
021200941027     C                   MOVEL     SOC007        D1SOC
021300941027     C                   ENDIF
021400941028      * imposto pos. del cursore
021500941028     C                   Z-ADD     CURR          H1RIGA
021600941028     C                   Z-ADD     CURC          H1COLO
021700941027      *
021800941027      *
021900941027      * FMTD1  - GRUPPO
022000940127     C     H1NMFL        WHENEQ    'D1GRP'
022100941027     C                   MOVE      *ZERO         WWGRP
022200941027     C                   MOVE      WWGRP         KEYTAB
022300941027     C                   MOVE      '01'          OPZTAB
022400941103     C                   MOVE      XSCSOC        AZITAB
022500941103     C                   MOVE      XSCLIN        LINTAB
022600941027     C                   CLEAR                   KPJBU
022700941027     C                   MOVEL     XTABDS        KPJBU
022800941027     C                   CALL      'ANGG22R'
022900941027     C                   PARM                    KPJBA
023000941027     C                   MOVEL     KPJBU         XTABDS
023100941027     C                   MOVE      KEYTAB        WWGRP
023200941028      * imposto pos. del cursore
023300941028     C                   Z-ADD     CURR          H1RIGA
023400941028     C                   Z-ADD     CURC          H1COLO
023500941027      *
0236009410272   >C     WWGRP         IFNE      *ZEROS
023700941027    >C                   MOVEL     WWGRP         D1GRP
0238009410272-  >C                   ENDIF
023900941027      * imposto pos. del cursore
024000940127     C                   Z-ADD     CURR          H1RIGA
024100940127     C                   Z-ADD     CURC          H1COLO
024200941027      *
024300941027      * FMTD1  - PROFILO
024400940127     C     H1NMFL        WHENEQ    'D1PRF'
024500941027    >C                   RESET                   A001DS
024600941027    >C                   MOVEL     '01'          OPZ001
024700941027    >C                   MOVEL     A001DS        KPJBU
024800941027    >C                   CALL      'ANA001R'
024900941027     C                   PARM                    KPJBA
025000941027    >C                   MOVEL     KPJBU         A001DS
0251009410272   >C     PRF001        IFNE      *BLANK
025200941027    >C                   MOVE      PRF001        D1PRF
0253009410272-  >C                   ENDIF
025400941027      *
025500941027      * imposto pos. del cursore
025600940127     C                   Z-ADD     CURR          H1RIGA
025700940127     C                   Z-ADD     CURC          H1COLO
025800941027      *
025900941027      * FMTD1  - FUNZIONE
026000940127     C     H1NMFL        WHENEQ    'D1FNC'
026100941102     C                   MOVE      *BLANK        WWFNC
026200941102     C                   MOVE      WWFNC         KEYTAB
026300941027     C                   MOVE      '01'          OPZTAB
026400941103     C                   MOVE      XSCSOC        AZITAB
026500941103     C                   MOVE      XSCLIN        LINTAB
026600941027     C                   CLEAR                   KPJBU
026700941027     C                   MOVEL     XTABDS        KPJBU
026800941027     C                   CALL      'ANGG23R'
026900941027     C                   PARM                    KPJBA
027000941027     C                   MOVEL     KPJBU         XTABDS
027100941027     C                   MOVE      KEYTAB        WWFNC
027200941027      *
0273009401272    C     WWFNC         IFNE      *BLANK
027400940127     C                   MOVEL     WWFNC         D1FNC
0275009401272-   C                   ENDIF
027600941027      * imposto pos. del cursore
027700940127     C                   Z-ADD     CURR          H1RIGA
027800940127     C                   Z-ADD     CURC          H1COLO
027900941017      *
028000941017    >C     H1NMFL        WHENEQ    'D1UNI'
028100941017    >C                   RESET                   A009DS
028200941017    >C                   MOVEL     '01'          OPZ009
028300941111    >C                   MOVEL     D1SOC         SOC009
028400941017    >C                   MOVEL     A009DS        KPJBU
028500941027    >C                   CALL      'ANA009R'
028600941017     C                   PARM                    KPJBA
028700941017    >C                   MOVEL     KPJBU         A009DS
0288009410172   >C     UNI009        IFNE      *BLANK
028900941017    >C                   MOVE      UNI009        D1UNI
0290009410172-  >C                   ENDIF
029100941102      * imposto pos. del cursore
029200941102     C                   Z-ADD     CURR          H1RIGA
029300941102     C                   Z-ADD     CURC          H1COLO
029400941027      *
029500941027      *
029600941027      * ricerca non abilitata in questa posizione
0297009402091O   C                   OTHER
029800940209     C                   SETON                                        98
029900941201      * imposto pos. del cursore
030000941201     C                   Z-ADD     CURR          H1RIGA
030100941201     C                   Z-ADD     CURC          H1COLO
030200941027      *
0303009410281-   C                   ENDSL
030400941027      *
030500940127     C                   ENDSR
030600940131     C/EJECT
030700941027      ************************************************************
030800941027      * RIEMPIMENTO OPZIONI
030900941027      ************************************************************
031000940131     C     RIEOPZ        BEGSR
031100941027      *
031200941027      * Input :
031300941027      * - $TX = schiera con tutti i testi disponibili
031400941027      * - $FL = schiere con i flag di validità associati ai testi
031500941027      * - WCNT = variabile contenente l'indice dell'ultimo testo
031600941027      *          opzione usato
031700941027      * - WMXO = variabile contenente il numero massimo di testi
031800941027      *          opzione usabili
031900941027      * Output :
032000941027      * - WOPZ1 = variabile contenente la prima riga opzioni
032100941027      * - WOPZ2 = variabile contenente la seconda riga opzioni
032200950403     C* - FF23  = flag che indica se c'è bisogno del tasto F23
032300941027      *
032400950403     C                   CALL      'X04OPMSG'
032500941027      * input
032600940223     C                   PARM                    $TX
032700940223     C                   PARM                    $FL
032800940223     C                   PARM                    WCNT              3 0
032900940223     C                   PARM                    WMXO              3 0
033000941027      * output
033100940223     C                   PARM                    WOPZ1            76
033200940223     C                   PARM                    WOPZ2            76
033300950403     C                   PARM                    FF23              1
033400950403    > * input
033500950403    >C                   PARM                    $L01              3 0
033600950403    >C                   PARM                    $L02              3 0
033700941027      *
033800060228A1148C                   PARM                    $ALLOPT         960
033900060228A1148C*
034000940131     C                   ENDSR
034100940131     C/EJECT
034200941027      ************************************************************
034300941027      * GESTIONE PARZIALIZZAZIONI
034400941027      ************************************************************
034500940127     C     GESD1         BEGSR
034600941027      * inizializzazione videata
034700940209     C     $INZD1        IFEQ      *ON
034800940127     C                   EXSR      INZD1
034900940209     C                   MOVE      *OFF          $INZD1
035000940117     C                   END
035100941027      * emissione piede videata se proveniente da altra
035200941027      * salvataggio valore formato di provenienza
035300940202     C     $LASTV        IFNE      'D1'
035400941027      * salvo stato F17 per sapere cosa fare in caso di F12
035500940223     C                   MOVE      *INKR         WINKR
035600940202     C                   WRITE     FMTT1
035700940202     C                   WRITE     FMTZ1
035800940202     C                   MOVE      $LASTV        $LASTG
035900940202     C                   MOVE      'D1'          $LASTV
036000940202     C                   END
036100941027      *
036200940131     C                   EXFMT     FMTD1
036300941027      *
036400940117     C                   SELECT
036500941027      * F3=Fine
036600940117     C     *INKC         WHENEQ    *ON
036700940209     C                   MOVE      *ON           $FINE
036800941027      * F4=Ricerca
036900940127     C     *INKD         WHENEQ    *ON
037000940127     C                   EXSR      SEARCH
037100941027      * F12=Ritorno
037200940127     C     *INKL         WHENEQ    *ON
037300941027      *  se la 1a videata è la lista oppure sono arrivato qui con F17,
037400941027      *  per F12 torno alla lista
037500940201     C     $LOP          IFEQ      2
037600940223     C     WINKR         OREQ      *ON
037700940131     C                   MOVE      'S1'          $GEST
037800940209     C                   MOVEL     *OFF          $INZS1
037900941027      *  altrimenti, per F12 torno a menu
038000940131     C                   ELSE
038100940209     C                   MOVE      *ON           $FINE
038200940131     C                   END
038300941027      * F18=Modifica valori assunti
038400940127     C     *INKS         WHENEQ    *ON
038500940131     C                   MOVE      'D2'          $GEST
038600940209     C                   MOVEL     *ON           $INZD2
038700941027      *
038800940117     C                   OTHER
038900940127     C                   EXSR      CTRD1
039000940127     C     *IN99         IFEQ      *OFF
039100941027      * definisco l'ordinamento
039200940131     C                   SELECT
039300940201     C     D1GRP         WHENNE    0
039400940131     C     D1PRF         ORNE      *BLANK
039500940131     C                   Z-ADD     1             $ORD
039600940131     C     D1FNC         WHENNE    *BLANK
039700940201     C     D1GRP         ANDEQ     0
039800940131     C     D1PRF         ANDEQ     *BLANK
039900940131     C                   Z-ADD     2             $ORD
040000941027      * nel caso non sia stato digitato nulla uso l'ordinamento
040100941027      * dei valori assunti
040200940131     C                   ENDSL
040300940127     C                   MOVE      'S1'          $GEST
040400940209     C                   MOVE      *ON           $INZS1
040500940202     C                   MOVE      '1'           $POSS1
040600940202     C                   Z-ADD     0             W1GRP
040700940202     C                   MOVE      *BLANK        W1PRF
040800940202     C                   MOVE      *BLANK        W1FNC
040900940117     C                   END
041000940117      *
041100940117     C                   ENDSL
041200940117      *
041300940117     C                   ENDSR
041400940117      ************************************************************
041500940127      * INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
041600940117      ************************************************************
041700940127     C     INZD1         BEGSR
041800941027      *
041900940131     C                   CLEAR                   FMTD1
042000940128     C                   MOVEL     KNMUS         D1PRF
042001151118>>>>>C                   EVAL      d1Vfu = '*ALL'
042100941027      *
042200940117     C                   ENDSR
042300941027      ************************************************************
042400941027      * CONTROLLO VIDEATA PARZIALIZZAZIONI
042500941027      ************************************************************
042600940127     C     CTRD1         BEGSR
042700941027      *
042800940127     C                   SETOFF                                       99
042900941027      * non imposto pos. del cursore
043000940127     C                   Z-ADD     0             H1RIGA
043100940127     C                   Z-ADD     0             H1COLO
043200941027      *
043300941027      * GRUPPO E PROFILO sono in alternativa
043400940201     C     D1GRP         IFNE      0
043500940131     C     D1PRF         ANDNE     *BLANK
043600940131     C                   SETON                                          5099
043700940131     C                   END
043800941027      *
043900940117     C                   MOVEA     *IN           WIN
044000940128     C                   MOVE      *ALL'0'       IN5098           49
044100940128     C                   MOVEA     IN5098        *IN(50)
044200940131     C                   WRITE     FMTD1
044300940117     C                   MOVEA     WIN           *IN
044400941027      *
044500940117     C                   ENDSR
044600941027      ************************************************************
044700941027      * GESTIONE LISTA
044800941027      ************************************************************
044900940127     C     GESS1         BEGSR
045000941027      * inizializzazione videata
045100940209     C     $INZS1        IFEQ      *ON
045200940127     C                   EXSR      INZS1
045300940209     C                   MOVE      *OFF          $INZS1
045400940127     C                   ENDIF
045500941027      * emissione piede videata se proveniente da altra
045600941027      * salvataggio valore formato di provenienza
045700940202     C     $LASTV        IFNE      'S1'
045800940202     C                   WRITE     FMTT1
045900940202     C                   WRITE     FMTZ2
046000940202     C                   MOVE      $LASTV        $LASTG
046100940202     C                   MOVE      'S1'          $LASTV
046200940202     C                   END
046300941027      *
046400940223     C     WMAX          IFEQ      0
046500940223     C                   WRITE     FMTD3                                        NO RCD
046600940223     C                   ENDIF
046700941027      *
046800940131     C                   EXFMT     FMTC1
046900940204     C     C1NRR         IFNE      0
047000940204     C                   Z-ADD     C1NRR         WSFL
047100940204     C                   ENDIF
047200940127     C                   Z-ADD     SFLNRR        C1RCD
047300941027      *
0474009401271    C                   SELECT
047500941027      * F3=Fine
047600940127     C     *INKC         WHENEQ    *ON
047700940209     C                   MOVE      *ON           $FINE
047800941027      * F10=Immissione
047900940131     C     *INKJ         WHENEQ    *ON
048000941017     C                   RESET                   A012DS
048100941017     C                   MOVEL     '01'          OPZ012
048200941017     C                   MOVE      KNMUS         PRF012
048300941017     C                   MOVEL     A012DS        KPJBU
048400941017     C                   CALL      'ANA012R'
048500940131     C                   PARM                    KPJBA
048600941017     C                   MOVEL     KPJBU         A012DS
048700941027      * Ritorno da PGM gestione
048800940209     C                   EXSR      GESRET
048900940209     C     WINZS1        IFEQ      *ON
049000940209     C                   MOVE      *ON           $INZS1
049100940202     C                   MOVE      '1'           $POSS1
049200941027      * se esistevano già righe sul sfl
049300941027      * calcolo pagina a cui deve posizionarsi
049400940221     C     WSFL          IFGT      0
049500940223     C                   EXSR      CLCPAG
049600941027      * altrimenti carico solo la 1a pagina
049700940223     C                   ELSE
049800940223     C                   Z-ADD     SFLPAG        WPAG
049900940223     C                   ENDIF
050000940221     C                   ENDIF
050100941027      * F12=Ritorno
050200940131     C     *INKL         WHENEQ    *ON
050300941027      *  se la 1a videata è le parzializzazioni, per F12 torno a questa
050400940201     C     $LOP          IFEQ      1
050500940131     C                   MOVE      'D1'          $GEST
050600940209     C                   MOVE      *OFF          $INZD1
050700941027      *  se la 1a videata è questa, per F12 torno a menu
050800940131     C                   ELSE
050900940209     C                   MOVE      *ON           $FINE
051000940131     C                   END
051100941027      * F17=Parzializzazioni
051200940131     C     *INKR         WHENEQ    *ON
051300940131     C                   MOVE      'D1'          $GEST
051400940209     C                   MOVEL     *OFF          $INZD1
051500941027      * F18=Modifica valori assunti
051600940131     C     *INKS         WHENEQ    *ON
051700940131     C                   MOVE      'D2'          $GEST
051800940209     C                   MOVEL     *ON           $INZD2
051900941027      * ROLL-UP
052000940202     C     *IN35         WHENEQ    *ON
052100940127     C                   EXSR      ROLS1
052200941027      *
0523009401271O   C                   OTHER
052400941027      * Controllo dati
052500940131     C                   EXSR      CTRC1
052600940201     C     *IN99         IFEQ      *OFF
052700940131     C                   EXSR      CTRS1
052800940131     C                   END
0529009401271-   C                   ENDSL
053000941027      *
053100940127     C                   ENDSR
053200941027      ************************************************************
053300941027      * INIZIALIZZAZIONE LISTA
053400941027      ************************************************************
053500940127     C     INZS1         BEGSR
053600941027      *
053700940128     C                   SETOFF                                         3031
053800940131     C                   WRITE     FMTC1
053900940128     C                   SETON                                          31
054000110322B1153c                   clear                   Fmts1
054100941027      *
054200941027      * abilito un posizionamento a seconda  dell'ordinamento definito
054300940131     C     $ORD          COMP      2                                      09
054400941027      *
054500941027      * riempimento opzioni
054600940223     C                   MOVEA     $TX1          $TX
054700940223     C                   MOVEA     $FL1          $FL
054800940223     C                   Z-ADD     0             WCNT
054900940223     C                   Z-ADD     4             WMXO
055000950403     C                   Z-ADD     76            $L01
055100950403     C                   Z-ADD     76            $L02
055200940128     C                   EXSR      RIEOPZ
055300950403     C                   Z-ADD     23            X
055400950403     C                   MOVE      FF23          $FL(X)
055500940223     C                   MOVE      WOPZ1         C1OPZ1
055600060228A1148C                   MOVEL     $ALLOPT       H1ALLOPT
055700940223     C                   MOVE      WOPZ2         C1OPZ2
055800941027      * ripristino i valori del posizionamento
055900940202     C                   MOVE      W1GRP         C1GRP
056000940202     C                   MOVE      W1PRF         C1PRF
056100940202     C                   MOVE      W1FNC         C1FNC
056200941027      *
056300941027      * Caricamento SFL a pagine
056400941027      *
056500940201     C                   Z-ADD     0             S1NRR
056600940128     C                   Z-ADD     0             WMAX
056700941027      *
056800941027      * Posizionamento su file pilota
056900940223     C                   EXSR      SETAUT
057000941027      *
057100941027      * Lettura file pilota
057200940131     C                   EXSR      REDAUT
057300941027      *
057400941027      * Carico 1a pagina
057500940127     C                   EXSR      ROLS1
057600940201     C                   Z-ADD     SFLPAG        WPAG
057700941027      *
057800940127     C                   ENDSR
057900941027      *
058000940127     C/EJECT
058100941027      ************************************************************
058200941027      * CARICAMENTO PAGINA LISTA
058300941027      ************************************************************
058400940127     C     ROLS1         BEGSR
058500941027      *
058600940128     C                   SETOFF                                       32
058700940210     C                   Z-ADD     0             Y
058800940127     C                   Z-ADD     WMAX          S1NRR
058900941027      *
059000941027      * Leggo dal file anagrafico per caricare la lista
059100941027      *
0592009401311    C     $EFILE        DOWEQ     *OFF
059300940201     C     Y             ANDLT     WPAG
059400110316B1153c* potrebbe capitare ad esempio nel caso di ANGATB di avere +
059500110316  "  c* record di anaut, per cui ne emetto solo 1
059600110316B1153C                   if        AUTGRP     <>  S1GRP
059700110316     C                             or  AUTPRF <>  S1PRF
059800110316     C                             or  AUTFNC <>  S1FNC
059900110316     C                             or  AUTVFU <>  S1VFU
060000110316GL   C                             or  AUTSOC <>  S1SOC
060100110316GL   C                             or  AUTUNI <>  S1UNI
060200941027      *
060300940131     C                   EXSR      RIES1
060400941027      *
060500940127     C                   ADD       1             S1NRR
060600940127     C                   ADD       1             Y
060700941027      *
060800940131     C                   WRITE     FMTS1
060900110316B1153c                   endif
061000941027      *
061100940131     C                   EXSR      REDAUT
061200941027      *
0613009401271-   C                   ENDDO
061400941027      *
061500940223     C                   Z-ADD     S1NRR         WMAX                 30
061600941027      *
061700941027      * POSIZIONAMENTO AL 1° RCD DELLA PAGINA
061800941027      *
061900940223     C     S1NRR         DIV       SFLPAG        PAGINE
062000940223     C                   MVR                     RESTO
062100940131     C     PAGINE        MULT      SFLPAG        C1RCD
0622009401271    C     RESTO         IFGT      0
062300940127     C                   ADD       1             C1RCD
0624009401271E   C                   ELSE
062500940127     C                   SUB       9             C1RCD
0626009401271-   C                   ENDIF
062700941027      *
062800940127     C                   ENDSR
062900940128     C/EJECT
063000941027      ************************************************************
063100941027      * POSIZIONAMENTO RCD ARCHIVIO PILOTA
063200941027      ************************************************************
063300940223     C     SETAUT        BEGSR
063400941117      *
063500941117     C                   MOVEL     D1SOC         AUTSOC
063600941117     C                   MOVEL     D1UNI         AUTUNI
063700941027      *
063800940223     C                   SELECT
063900940223     C     $ORD          WHENEQ    1
064000940223     C     $POSS1        IFEQ      '1'
064100940223     C                   MOVE      D1GRP         AUTGRP
064200940223     C                   MOVE      D1PRF         AUTPRF
064300940223     C                   ELSE
064400940223     C                   MOVE      W1GRP         AUTGRP
064500940223     C                   MOVE      W1PRF         AUTPRF
064600940223     C                   END
064700941020     C     K4AUT1        SETLL     ANAUT01
064800940223     C     $ORD          WHENEQ    2
064900940223     C     $POSS1        IFEQ      '1'
065000940223     C                   MOVE      D1FNC         AUTFNC
065100940223     C                   ELSE
065200940223     C                   MOVE      W1FNC         AUTFNC
065300940223     C                   END
065400941020     C     K3AUT2        SETLL     ANAUT02
065500940223     C                   ENDSL
065600941027      *
065700940223     C                   ENDSR
065800940223     C/EJECT
065900941027      ************************************************************
066000941027      * LETTURA RCD ARCHIVIO PILOTA
066100941027      ************************************************************
066200940223     C     REDAUT        BEGSR
066300941027      *
066400940131     C                   MOVEL     *OFF          $EFILE
066500940131     C                   MOVEL     *OFF          $RCDOK
066600941027      *
0667009401311    C     $EFILE        DOUEQ     *ON
066800940131     C     $RCDOK        OREQ      *ON
066900941027      *
0670009401312    C                   SELECT
067100940131     C     $ORD          WHENEQ    1
067200940131     C                   READ      ANAUT01                                21
067300940131     C     $ORD          WHENEQ    2
067400940131     C                   READ      ANAUT02                                21
0675009401312-   C                   ENDSL
067600941027      *
067700940131     C                   MOVE      *IN21         $EFILE
067800941027      *
0679009401312    C     $EFILE        IFEQ      *OFF
068000940131     C                   EXSR      SELAUT
0681009401312-   C                   ENDIF
068200941027      *
0683009401311-   C                   ENDDO
068400941027      *
068500940131     C                   MOVE      *IN21         *IN33                          SflEnd
068600941027      *
068700940131     C                   ENDSR
068800940131     C/EJECT
068900941027      ************************************************************
069000941027      * CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
069100941027      ************************************************************
069200940223     C     CLCPAG        BEGSR
069300941027      * Input :
069400941027      * - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
069500941027      *          cursore
069600941027      * - SFLPAG = numero rcd per pagina sfl
069700941027      * Output :
069800941027      * - WPAG = pagina fino a cui deve essere ricaricato il sfl
069900941027      *
070000940223     C     WSFL          DIV       SFLPAG        PAGINE            4 0
070100940223     C                   MVR                     RESTO             3 0
070200940223     C     RESTO         IFGT      0
070300940223     C                   ADD       1             PAGINE
070400940223     C                   ENDIF
070500940223     C     PAGINE        MULT      SFLPAG        WPAG
070600941027      *
070700940223     C                   ENDSR
070800940223     C/EJECT
070900941027      ************************************************************
071000941027      * SELEZIONI SULLA RIGA LETTA
071100941027      ************************************************************
071200940223     C     SELAUT        BEGSR
071300941027      *
071400940202     C                   MOVEL     *ON           $RCDOK
071500940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
071600940131A1211C                   MOVEL     *hival        ALL9             15
071700941027      * creo limite massimo parzializzazioni
071800940202     C     D1PRF         CAT       ALL9:0        PRF999           10
071900940202     C     D1FNC         CAT       ALL9:0        FNC999            6
072000941017     C     D1SOC         CAT       ALL9:0        SOC999            3
072100941017     C     D1UNI         CAT       ALL9:0        UNI999            8
072200941027      *
072300940202     C                   SELECT
072400941027      *
072500941027      * SELEZIONI RISPETTO AI VALORI RCD
072600941027      *
072700941027      * rcd annullato
072800940202     C     AUTANN        WHENEQ    'A'
072900940202     C                   MOVEL     *OFF          $RCDOK
073000941027      *
073100941027      * SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
073200941027      *
073300941027      * gruppo non compreso nella scelta
073400940202     C     D1GRP         WHENNE    0
073500940202     C     AUTGRP        ANDNE     D1GRP
073600940202     C                   MOVEL     *OFF          $RCDOK
073700941027      * gruppo non valido rispetto alla scelta del profilo
073800941117     C     D1GRP         WHENEQ    0
073900940204     C     D1PRF         ANDNE     *BLANK
074000940204     C     AUTGRP        ANDNE     0
074100940204     C                   MOVEL     *OFF          $RCDOK
074200941027      * profilo non compreso nella scelta
074300940202     C     AUTPRF        WHENLT    D1PRF
074400940202     C     AUTPRF        ORGT      PRF999
074500940202     C                   MOVEL     *OFF          $RCDOK
074600941027      * funzione non compresa nella scelta
074700940202     C     AUTFNC        WHENLT    D1FNC
074800940202     C     AUTFNC        ORGT      FNC999
074900940202     C                   MOVEL     *OFF          $RCDOK
075000941027      *
075100941027      * società non compresa nella scelta
075200941017     C     AUTSOC        WHENLT    D1SOC
075300941017     C     AUTSOC        ORGT      SOC999
075400941017     C                   MOVEL     *OFF          $RCDOK
075500941017     C     AUTSOC        IFGT      SOC999
075600941017     C     *HIVAL        SETLL     ANAUT02
075700941017     C                   ENDIF
075800941027      * unità non compresa nella scelta
075900941017     C     AUTUNI        WHENLT    D1UNI
076000941017     C     AUTUNI        ORGT      UNI999
076100941017     C                   MOVEL     *OFF          $RCDOK
076101151118>>>>>** Variabile non compresa nella scelta.
076102151118>>>>>C                   WHEN      d1Vfu <> '*ALL' AND d1Vfu <> autVfu
076103151118>>>>>C                   EVAL      $rcdOk = *OFF
076200940202     C                   ENDSL
076300941027      *
076400940202     C                   ENDSR
076500940131     C/EJECT
076600941027      ************************************************************
076700941027      * RIEMPIMENTO RIGA LISTA
076800941027      ************************************************************
076900940131     C     RIES1         BEGSR
077000941027      *
077100940131     C                   CLEAR                   FMTS1
077200941027      *
077300940131     C                   MOVEL     AUTGRP        S1GRP
077400940131     C                   MOVEL     AUTPRF        S1PRF
077500940131     C                   MOVEL     AUTFNC        S1FNC
077600940131     C                   MOVEL     AUTVFU        S1VFU
077700941017GL   C                   MOVEL     AUTSOC        S1SOC
077800941017GL   C                   MOVEL     AUTUNI        S1UNI
077900941027      *
078000940131     C                   ENDSR
078100940128     C/EJECT
078200941027      ************************************************************
078300941027      * CONTROLLO TESTATA LISTA
078400941027      ************************************************************
078500940131     C     CTRC1         BEGSR
078600941027      *
078700940201     C                   MOVE      *OFF          *IN99
078800941027      *
078900941027      * Se richiesto un inizio elenco particolare attivo
079000941027      * L'inizializzazione lista
079100941027      *
079200940202     C     C1GRP         IFNE      W1GRP
079300940202     C     C1PRF         ORNE      W1PRF
079400940202     C     C1FNC         ORNE      W1FNC
079500940202     C                   EXSR      POSS1
079600940202     C                   END
079700941027      *
079800940202     C                   ENDSR
079900940202     C/EJECT
080000941027      ************************************************************
080100941027      * CONTROLLI PER POSIZIONAMENTO SU LISTA
080200941027      ************************************************************
080300940202     C     POSS1         BEGSR
080400941027      *
080500941027      * Controllo se esistono opzioni attive
080600941027      *
0807009402021    C     WMAX          IFGT      0
080800940202     C                   READC     FMTS1                                  21
0809009402022    C     *IN21         DOWEQ     *OFF
081000940202     C     *IN99         ANDEQ     *OFF
081100941027      *
0812009402023    C     S1OPZ         IFNE      0
081300940202     C                   SETON                                        51  99
0814009402023-   C                   ENDIF
081500941027      * se rilevata opzione attiva, attivo sflnxtchg
0816009402023    C     *IN99         IFEQ      *ON
081700940202     C                   MOVE      *ON           *IN32
081800940202     C                   UPDATE    FMTS1
0819009402023E   C                   ELSE
082000940202     C                   READC     FMTS1                                  21
0821009402023-   C                   ENDIF
082200941027      *
0823009402022-   C                   ENDDO
082400941027      *
0825009402021-   C                   ENDIF
082600941027      *
082700941027      * SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
082800941027      * AL VALORE DI POSIZIONAMENTO :
082900941027      * se questo è maggiore del valore del rispettivo campo di
083000941027      * parzializzazione lo uso, altrimenti eseguo un ricaricamento
083100941027      * con i valori di parzializzazione.
083200941027      *
0833009402021    C     *IN99         IFEQ      *OFF
083400940217     C                   MOVE      '1'           $POSS1
083500941027      *
0836009402022    C                   SELECT
083700941027      *
083800940202     C     $ORD          WHENEQ    1
083900941027      *
084000940203     C     C1GRP         IFGT      D1GRP
084100940202     C     C1PRF         ORGT      D1PRF
084200940202     C     D1GRP         ANDEQ     0
084300940202     C                   MOVE      '2'           $POSS1
084400940217     C                   END
084500941027      *
084600940202     C                   MOVE      C1GRP         W1GRP
084700940202     C                   MOVE      C1PRF         W1PRF
084800940202     C                   MOVE      *ON           *IN99
084900940209     C                   MOVE      *ON           $INZS1
085000941027      *
085100940202     C     $ORD          WHENEQ    2
085200941027      *
085300940202     C     C1FNC         IFGT      D1FNC
085400940202     C                   MOVE      '2'           $POSS1
085500940202     C                   END
085600941027      *
085700940202     C                   MOVE      C1FNC         W1FNC
085800940202     C                   MOVE      *ON           *IN99
085900940209     C                   MOVE      *ON           $INZS1
086000941027      *
0861009402022-   C                   ENDSL
086200941027      *
0863009402021-   C                   ENDIF
086400941027      *
086500940131     C                   ENDSR
086600940131     C/EJECT
086700941027      ************************************************************
086800941027      * CONTROLLO OPZIONI LISTA
086900941027      ************************************************************
087000940131     C     CTRS1         BEGSR
087100941027      *
087200940202     C                   MOVEL     *OFF          $ESCI
087300940201     C                   SETOFF                                       99
087400940131     C                   Z-ADD     0             S1OPZ
087500941027      *
087600941027      * Leggo il sfl solo se ci sono rcd
0877009401311    C     WMAX          IFGT      0
087800940131     C                   READC     FMTS1                                  21
087900941027      *
088000941027      * esce se fine sfl o errore che richiede l'uscita
0881009401312    C     *IN21         DOWEQ     *OFF
088200940131     C     $ESCI         ANDEQ     *OFF
088300940201     C                   Z-ADD     S1NRR         C1RCD
088400941027      * ctrl su riga
088500940131     C                   EXSR      RECS1
088600941027      * gestione opzioni
0887009401313    C     S1OPZ         IFNE      0
088800940201     C     *IN99         ANDEQ     *OFF
0889009807134    C                   SELECT
089000980713     C     S1OPZ         WHENGE    2
089100980713     C     S1OPZ         ANDLE     5
089200940131     C                   EXSR      OPZS1
0893009807134-   C                   ENDSL
0894009402093-   C                   ENDIF
089500941125      * se rilevato errore, attivo sflnxtchg
0896009402013    C     *IN99         IFEQ      *ON
089700940131     C                   MOVE      *ON           *IN32
0898009402103-   C                   ENDIF
089900941027      *
090000940131     C                   Z-ADD     0             S1OPZ
090100941027      *
090200940127     C                   UPDATE    FMTS1
090300941125     C                   MOVE      *OFF          *IN32
090400941027      *
090500941027      * leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0906009401313    C     $ESCI         IFEQ      *OFF
090700940131     C                   READC     FMTS1                                  21
090800941027      * a fine ciclo ripristino il flag richiesta iniz.sfl
0909009402014    C     *IN21         IFEQ      *ON
091000940201     C                   MOVE      WINZS1        $INZS1
091100940204     C                   MOVE      '1'           $POSS1
091200050608D1830C                   MOVE      *off          WINZS1
091300050608  "  C* se è avvenuta almeno una modifica
091400050608  "  C                   IF        $INZS1 = *on
091500050608  "  C* calcolo pagina a cui deve posizionarsi
091600050608D1830C     WSFL          IFGT      0
091700941027      * calcolo pagina a cui deve posizionarsi
091800940223     C                   EXSR      CLCPAG
091900050608D1830C                   ELSE
092000050608D1830C                   Z-ADD     SFLPAG        WPAG
0921009402014-   C                   ENDIF
0922009402013-   C                   ENDIF
092300050608D1830C                   ENDIF
092400050608D1830C                   ENDIF
092500941027      *
0926009401272-   C                   ENDDO
092700941027      *
0928009401311-   C                   ENDIF
092900941027      *
093000940127     C                   ENDSR
093100940127     C/EJECT
093200941027      ************************************************************
093300941027      * CONTROLLO CAMPI I/O RIGA LISTA
093400941027      ************************************************************
093500940131     C     RECS1         BEGSR
093600941027      *
093700941027      * reset indicatori DSPATR
093800940201     C                   MOVE      *ALL'0'       IN4049           10
093900940201     C                   MOVEA     IN4049        *IN(40)
094000941027      *
094100941027      * Controllo se Utente Autorizzato alla scelta
094200940202     C     S1OPZ         IFNE      0
094300940131     C                   Z-ADD     1             X
094400940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
094500940131     C     *IN21         IFEQ      *ON
094600940223     C     $FL1(X)       IFEQ      'N'
094700940131     C                   MOVE      *ON           $ESCI
094800940201     C                   SETON                                        504099
094900940131     C                   ENDIF
095000940131     C                   ELSE
095100940131     C                   MOVE      *ON           $ESCI
095200940201     C                   SETON                                        504099
095300940131     C                   ENDIF
095400940202     C                   ENDIF
095500941027      *
095600940131     C                   ENDSR
095700940209     C/EJECT
095800941027      ************************************************************
095900941027      * GESTIONE OPZIONI LISTA
096000941027      ************************************************************
096100940131     C     OPZS1         BEGSR
096200941027      *
096300941017     C                   RESET                   A012DS
096400941017     C                   MOVE      S1OPZ         OPZ012
096500941017     C                   MOVE      S1GRP         GRP012
096600941017     C                   MOVE      S1PRF         PRF012
096700941017     C                   MOVE      S1FNC         FNC012
096800941017     C                   MOVE      S1VFU         VFU012
096900941017     C                   MOVE      S1SOC         SOC012
097000941017     C                   MOVE      S1UNI         UNI012
097100941027      *
097200941017     C                   MOVEL     A012DS        KPJBU
097300941017     C                   CALL      'ANA012R'
097400940201     C                   PARM                    KPJBA
097500941017     C                   MOVEL     KPJBU         A012DS
097600941027      * serve per evitare la riemissione della finestra senza sfondo
097700940204     C                   WRITE     OVRLAY
097800941027      * ritorno da PGM gestione
097900940209     C                   EXSR      GESRET
098000941027      * se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
098100940209     C     *IN99         IFEQ      *ON
098200940209     C                   SETON                                        40
098300940209     C                   END
098400941027      *
098500940131     C                   ENDSR
098600940209     C/EJECT
098700941027      ************************************************************
098800941027      * GESTIONE RITORNO DA PGM DI GESTIONE
098900941027      ************************************************************
099000940209     C     GESRET        BEGSR
099100941027      *
099200941027      * modo di ritorno
099300941027      *
0994009402091    C                   SELECT
099500941027      * 1 = F3
099600941017     C     RET012        WHENEQ    '1'
099700940209     C                   MOVE      *ON           $FINE                          fine pgm
099800950908     C                   MOVE      *ON           $ESCI                          sospese READC
099900941027      * 2 = F12
100000941017     C     RET012        WHENEQ    '2'
100100940209     C                   MOVE      *ON           $ESCI                          sospese READC
100200941027      *
1003009402091-   C                   ENDSL
100400941027      *
100500941027      * operazione eseguite dal pgm chiamato
100600941027      *
1007009402091    C                   SELECT
100800941027      * 1 = eseguito aggiornamento
100900941017     C     OPR012        WHENEQ    '1'
101000940209     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
101100941027      *
1012009402091-   C                   ENDSL
101300941027      *
101400941027      * funzione non eseguibile per errore :
101500941027      *
1016009402091    C                   SELECT
101700941027      * 1 = funzione richiamata chiusa in errore
101800941017     C     ERR012        WHENEQ    '1'
101900940209     C                   MOVE      *ON           $ESCI                          sospese READC
102000940209     C                   SETON                                        5299      emiss.SFLMSGID
102100941027      *
1022009402091-   C                   ENDSL
102300941027      *
102400940209     C                   ENDSR
102500940209     C/EJECT
102600941027      ************************************************************
102700941027      * GESTIONE VALORI ASSUNTI
102800941027      ************************************************************
102900940209     C     GESD2         BEGSR
103000941027      * inizializzazione videata
103100940209     C     $INZD2        IFEQ      *ON
103200940131     C                   EXSR      INZD2
103300940209     C                   MOVE      *OFF          $INZD2
103400940131     C                   END
103500941027      * salvataggio valore formato di provenienza
103600940202     C     $LASTV        IFNE      'D2'
103700940202     C                   MOVE      $LASTV        $LASTG
103800940202     C                   MOVE      'D2'          $LASTV
103900940202     C                   END
104000941027      *
104100940131     C                   EXFMT     FMTD2
104200941027      *
104300940131     C                   SELECT
104400941027      * F3=Fine
104500940131     C     *INKC         WHENEQ    *ON
104600940209     C                   MOVE      *ON           $FINE
104700941027      * F12=Ritorno
104800940131     C     *INKL         WHENEQ    *ON
104900940131     C                   SELECT
105000940202     C     $LASTG        WHENEQ    'S1'
105100940131     C                   MOVE      'S1'          $GEST
105200940209     C                   MOVEL     *OFF          $INZS1
105300940202     C     $LASTG        WHENEQ    'D1'
105400940131     C                   MOVE      'D1'          $GEST
105500940209     C                   MOVEL     *OFF          $INZD1
105600940131     C                   ENDSL
105700941027      *
105800940131     C                   OTHER
105900940131     C                   EXSR      CTRD2
1060009401312    C     *IN99         IFEQ      *OFF
1061009401313    C                   SELECT
106200940202     C     $LASTG        WHENEQ    'S1'
106300940131     C                   MOVE      'S1'          $GEST
106400941027      * se è stato modificato l'ordinamento di default, ricarico la lista
106500940131     C     $ORD          IFEQ      1
106600940131     C     D2ORD         ANDEQ     2
106700940131     C     $ORD          OREQ      2
106800940131     C     D2ORD         ANDEQ     1
106900940209     C                   MOVE      *ON           $INZS1
107000940202     C                   MOVE      '1'           $POSS1
107100940203     C                   Z-ADD     0             W1GRP
107200940203     C                   MOVE      *BLANK        W1PRF
107300940203     C                   MOVE      *BLANK        W1FNC
107400940131     C                   ELSE
107500940209     C                   MOVE      *OFF          $INZS1
107600940131     C                   END
107700940202     C     $LASTG        WHENEQ    'D1'
107800940131     C                   MOVE      'D1'          $GEST
107900940209     C                   MOVE      *OFF          $INZD1
1080009401313-   C                   ENDSL
108100941027      * definisco i nuovi valori di default
108200940201     C                   Z-ADD     D2LOP         $LOP
108300940131     C                   Z-ADD     D2ORD         $ORD
1084009401312-   C                   END
108500941027      *
108600940131     C                   ENDSL
108700941027      *
108800940131     C                   ENDSR
108900941027      ************************************************************
109000941027      * INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
109100941027      ************************************************************
109200940201     C     INZD2         BEGSR
109300941027      *
109400940131     C                   CLEAR                   FMTD2
109500940201     C                   MOVEL     $LOP          D2LOP
109600940201     C                   MOVEL     $ORD          D2ORD
109700941027      *
109800940131     C                   ENDSR
109900941027      ************************************************************
110000941027      * CONTROLLO VIDEATA VALORI ASSUNTI
110100941027      ************************************************************
110200940131     C     CTRD2         BEGSR
110300941027      *
110400940131     C                   SETOFF                                       99
110500941027      * non imposto pos. del cursore
110600940131     C                   Z-ADD     0             H1RIGA
110700940131     C                   Z-ADD     0             H1COLO
110800941027      *
110900941027      * OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
111000940201     C     D2LOP         IFEQ      0
111100940131     C                   SETON                                          5099
111200940131     C                   END
111300941027      *
111400941027      * OBBLIGATORIO VALORE PER ORDINAMENTO
111500940131     C     D2ORD         IFEQ      0
111600940131     C                   SETON                                          5199
111700940131     C                   END
111800941027      *
111900940131     C                   MOVEA     *IN           WIN
112000940131     C                   MOVE      *ALL'0'       IN5098           49
112100940131     C                   MOVEA     IN5098        *IN(50)
112200940131     C                   WRITE     FMTD1
112300940131     C                   MOVEA     WIN           *IN
112400941027      *
112500940131     C                   ENDSR
112600941027      ************************************************************
112700941027      * AGGIORNAMENTO VALORI ASSUNTI
112800941027      ************************************************************
112900940201     C     AGGMPE        BEGSR
113000941027      *
113100940201     C                   CLEAR                   ANMPE000
113200940201     C                   MOVEL     XSCSOC        MPESOC
113300940201     C                   MOVEL     DSPGM         MPEPGM
113400940201     C                   MOVEL     KNMUS         MPEPRF
113500940201     C                   MOVEL     'PV'          MPETIP
113600940201     C     K4MPE2        CHAIN     ANMPE02L                           21
113700940201     C                   MOVEL     DSMPE         MPEPAR
113800940201     C     *IN21         IFEQ      *ON
113900940201     C                   WRITE     ANMPE000
114000940201     C                   ELSE
114100940201     C                   UPDATE    ANMPE000
114200940201     C                   END
114300941027      *
114400940201     C                   ENDSR
114500941027      ************************************************************
114600941027      * OPERAZIONI INIZIALI
114700941027      ************************************************************
114800940131     C     *INZSR        BEGSR
114900941027      *
115000941027      * Reperimento parametri
115100941027      *
115200940117     C     *ENTRY        PLIST
115300940117     C                   PARM                    KPJBA
115400941103      *
115500941103      * Reperimento dati società
115600941103      *
115700941103     C                   MOVEL     'SOC001'      TIPXSC
115800941103     C                   MOVEL     *BLANK        SOCXSC
115900941103     C                   EXSR      REPSOC
116000941103     C     RTNXSC        IFNE      '1'
116100941103     C                   MOVEL     XSOCDS        SOC001
116200941103     C                   ELSE
116300941103     C                   MOVE      *ON           $FINE
116400941103     C                   EXSR      ENDPGM
116500941103     C                   ENDIF
116600941027      *
116700941027      * Reperimento valori default utente
116800941027      *
116900940131     C                   EXSR      REPDFT
117000941027      *
117100941027      * Reperimento opzioni
117200941027      *
117300940128     C                   EXSR      REPOPZ
117400941027      *
117500940117     C                   ENDSR
117600941027      ************************************************************
117700941027      * INIZIALIZZAZIONE VARIABILI
117800941027      ************************************************************
117900940210     C     INZVAR        BEGSR
118000941027      *
118100941027      * Variabili per gestione videate
118200941027      *
118300940210     C                   MOVE      *OFF          $FINE                          fine pgm
118400940210     C                   MOVE      *OFF          $INZD1                         inizializzione D1
118500940210     C                   MOVE      *OFF          $INZS1                         inizializzione S1
118600940210     C                   MOVE      '1'           $POSS1                         1=video 2=sflctl
118700940210     C                   MOVE      *OFF          $INZD2                         inizializzione D2
118800940210     C                   MOVE      *OFF          $EFILE                         memo fine file
118900940210     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
119000940210     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
119100941027      *
119200940210     C                   SELECT
119300940210     C     $LOP          WHENEQ    1
119400940210     C                   MOVE      'D1'          $GEST
119500940210     C     $LOP          WHENEQ    2
119600940210     C                   MOVE      'S1'          $GEST
119700940210     C                   MOVE      '1'           $POSS1
119800940210     C                   ENDSL
119900950615      *
120000950615     C                   MOVE      *ON           $INZD1
120100950615     C                   MOVE      *ON           $INZS1
120200941027      *
120300941027      * Variabili appoggio
120400941027      *
120500940210     C                   Z-ADD     SFLPAG        WPAG
120600941027      *
120700941027      * Valorizzazione campi univoci testate
120800941027      *
120900940210     C                   CLEAR                   FMTT1
121000940210     C                   MOVEL     KNSIF         NOMSIF
121100941110     C                   MOVEL     XSCDSI        NOMDIT
121200941028     C                   MOVEL     DSPGM         NOMPGM
121300941027      *
121400940210     C                   ENDSR
121500941027      ************************************************************
121600941027      * REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
121700941027      ************************************************************
121800940131     C     REPDFT        BEGSR
121900941027      *
122000940201     C                   MOVEL     XSCSOC        MPESOC
122100940201     C                   MOVEL     DSPGM         MPEPGM
122200940131     C                   MOVEL     KNMUS         MPEPRF
122300940131     C                   MOVEL     'PV'          MPETIP
122400940202     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
122500940131     C     *IN21         IFEQ      *OFF
122600940131     C                   MOVEL     MPEPAR        DSMPE
122700941027      * nel caso sia la prima volta che l'utente apre questa anagrafica
122800941027      * i suoi valori di default vengono decisi a pgm
122900940131     C                   ELSE
123000941115     C                   CLEAR                   DSMPE
123100940201     C                   Z-ADD     1             $LOP
123200940131     C                   Z-ADD     1             $ORD
123300940131     C                   END
123400941027      *
123500940131     C                   ENDSR
123600941027      ************************************************************
123700941027      * REPERIMENTO OPZIONI
123800941027      ************************************************************
123900940131     C     REPOPZ        BEGSR
124000941027      *
124100940210     C     1             DO        4             X
124200941027      *
124300940223     C     $MS1(X)       IFNE      *BLANK
124400940223     C     $FL1(X)       ANDEQ     'S'
124500941027      *
124600940223     C                   MOVEL     $MS1(X)       IDMSG
124700940128     C                   CALL      'XRTVM'
124800940201     C                   PARM                    IDMSG            27
124900940201     C                   PARM                    TXTMSG          100
125000940201     C                   PARM                    ERRMSG            1
125100941027      *
125200940128     C     ERRMSG        IFNE      *ON
125300940223     C                   MOVEL     TXTMSG        $TX1(X)
125400940128     C                   ELSE
125500940223     C                   MOVEL     *ALL'?'       $TX1(X)
125600940128     C                   END
125700941027      *
125800940128     C                   END
125900941027      *
126000940128     C                   END
126100941027      *
126200940128     C                   ENDSR
126300941027      ************************************************************
126400941027      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
126500941027      ************************************************************
126600940210     C     DEFCAM        BEGSR
126700941027      *
126800941027      * klist
126900941027      *
127000941027      * ANAUT01L
127100941020     C     K4AUT1        KLIST
127200941020     C                   KFLD                    AUTSOC
127300941020     C                   KFLD                    AUTUNI
127400941020     C                   KFLD                    AUTGRP
127500940117     C                   KFLD                    AUTPRF
127600941027      *
127700941027      * ANAUT02L
127800941020     C     K3AUT2        KLIST
127900941020     C                   KFLD                    AUTSOC
128000941020     C                   KFLD                    AUTUNI
128100941020     C                   KFLD                    AUTFNC
128200941027      *
128300941027      * ANMPE02L
128400940128     C     K4MPE2        KLIST
128500940128     C                   KFLD                    MPESOC
128600940201     C                   KFLD                    MPEPGM
128700940128     C                   KFLD                    MPETIP
128800940128     C                   KFLD                    MPEPRF
128900941027      *
129000941027      * Variabili per gestione videate
129100941027      *
129200940210     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
129300940210     C                   MOVE      *BLANK        $FINE             1            fine pgm
129400940210     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
129500940210     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
129600940210     C                   MOVE      *BLANK        $POSS1            1            1=video 2=sflctl
129700940210     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
129800940210     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
129900940210     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
130000940210     C                   MOVE      *BLANK        $EFILE            1            memo fine file
130100940210     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
130200940210     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
130300941027      *
130400941027      * Variabili appoggio
130500941027      *
130600940210     C     *LIKE         DEFINE    C1RCD         S1NRR
130700940210     C     *LIKE         DEFINE    C1NRR         WSFL
130800940210     C     *LIKE         DEFINE    C1RCD         WMAX
130900940210     C     *LIKE         DEFINE    C1RCD         WPAG
131000940210     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
131100940210     C     *LIKE         DEFINE    AUTPRF        WWPRF
131200940210     C     *LIKE         DEFINE    AUTFNC        WWFNC
131300941102     C     *LIKE         DEFINE    AUTGRP        WWGRP
131400940210     C     *LIKE         DEFINE    AUTGRP        W1GRP
131500940210     C     *LIKE         DEFINE    AUTPRF        W1PRF
131600940210     C     *LIKE         DEFINE    AUTFNC        W1FNC
131700940223     C     *LIKE         DEFINE    *INKR         WINKR
131800940210     C                   Z-ADD     0             CURR              2 0
131900940210     C                   Z-ADD     0             CURC              2 0
132000940210     C                   MOVE      *BLANK        WIN              99
132100941027      *
132200941027      * Indici
132300941027      *
132400940210     C                   Z-ADD     0             X                 3 0
132500940210     C                   Z-ADD     0             Y                 3 0
132600941027      *
132700940117     C                   ENDSR
132800941103     C/EJECT
132900941103      ************************************************************
133000941103      * Reperimento dati società
133100941103      ************************************************************
133200941103     C     REPSOC        BEGSR
133300941103      *
133400941103     C                   CALL      'XSOC'
133500941103     C                   PARM                    TIPXSC            6
133600941103     C                   PARM                    SOCXSC            3
133700941103     C                   PARM                    CDSXSC            9 0
133800941103     C                   PARM                    MODXSC            3
133900941103     C                   PARM      *BLANKS       RTNXSC            1
134000941103     C                   PARM                    XSOCDS
134100941103     C                   PARM                    KPJBA
134200941103      *
134300941103     C                   ENDSR
134400941027     C/EJECT
134500941027      ************************************************************
134600940223     C/EJECT
134700940131**
134800940131PROMSG    *LIBL     OPZ0002
134900940131PROMSG    *LIBL     OPZ0003
135000940131PROMSG    *LIBL     OPZ0004
135100940131PROMSG    *LIBL     OPZ0005
135200940201**
13530094013102S
13540094013103S
13550094013104S
13560094013105S
