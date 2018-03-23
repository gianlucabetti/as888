000100940211     H*--------INIZIO SPECIFICHE --------------------------*
000101981202     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200941110     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *----------*
000500980722    > * ANA009R  *
000600940224      *----------*
000700940317    > *               TITOLO PROGRAMMA
000800940224      *          ------------------------------
000900940224      *
001000940224      *
001100941013      * DOCUMENTI MICROANALISI :  NSDAMUS/ANALISI/PROF 001.DOC
001200940224      *
001300940224      * NOTE TECNICHE :
001400940224      *
001500940224      *
001600940224      * PERSONALIZZAZIONI
001700940224      *
001800940224      *
001900940224      *
002000940314      *               MAPPA INDICATORI
002100940224      *
002200940307      *  21           GENERICO OPERAZIONI I/O
002300940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002400940224      *  30           SFLDSP
002500940224      * N31           SFLCLR
002600940224      *  31           SFLDSPCTL
002700940224      *  32           SFLNXTCHG
002800940224      *  33           SFLEND
002900940224      *  39           OF PRTF
003000940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003100940317    > *  Specificare l'uso dei singoli indicatori
003200940224      *  50 <---> 98  ERRORI SU VIDEO
003300941013      *  50
003400941013      *  51           D1  SOCIETA' OBBLIGATORIA                 PRO0014
003500941013      *  52
003600941013      *  53           D1  SOCIETA' INESISTENTE                  PRO0027
003700940909    > *  Specificare l'uso dei singoli indicatori
003800940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003900940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
004000940224      *  99           INDIC. GENERALE DI ERRORE
004100940128     F*----------------------------------------------------*
004200940314    >F*----------------------------------------------------------------
004300940314    >F* Viste logiche utilizzate per i diversi ordinamenti
004400940314    >F*----------------------------------------------------------------
004500940909     F*------------
004600941012     FANUNI01L  IF   E           K DISK
004700941012     F                                     RENAME(ANUNI000:ANUNI01)
004800940211     F*------------
004900940201     FANMPE02L  UF A E           K DISK
005000941013     F*------------
005100941013    >FANSIF01L  IF   E           K DISK
005200940211     F*------------
005300941012    >FANA009D   CF   E             WORKSTN
005400941012     F                                     RENAME(A009D1:FMTD1)
005500941012     F                                     RENAME(A009D2:FMTD2)
005600941012     F                                     RENAME(A009D3:FMTD3)
005700941012     F                                     RENAME(A009S1:FMTS1)
005800941012     F                                     RENAME(A009C1:FMTC1)
005900941012     F                                     RENAME(A009T1:FMTT1)
006000941012     F                                     RENAME(A009Z1:FMTZ1)
006100941012     F                                     RENAME(A009Z2:FMTZ2)
006200941012     F                                     RENAME(A009Z3:FMTZ3)
006300940131     F                                     SFILE(FMTS1:S1NRR)
006400940201     F                                     INFDS(DSFMT)
006500940128     D*----------------------------------------------------*
006600940302     D* Schiere per la gestione delle opzioni
006601980716     D $DIMOP          C                   CONST(30)
006700940302     D*
006701980716     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
006702980716     D $TX1            S             50    DIM($dimop) ALT($MS1)
006703980716     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
006704980716     D $FL1            S              1    DIM($dimop) ALT($OP1)
007200950403     D $TX             S             50    DIM(50)
007300950403     D $FL             S              1    DIM(50)
007301980716     D* schiera contenente i nomi dei programma richiamati attraverso
007302980716     D* le opzioni personalizzate
007303980716     D $PGO1           S             10    DIM($dimop)
007304980716     D*
007305980716     D* Indice della schiera per le opzioni personalizzate
007306980716     D PosOpz          S              2  0
007307980716     D* numero max opzioni
007308980716     D NumOpz          S              2  0 Inz($DIMOP)
007309980716     D* numero effettive opzioni
007310980716     D OpzLen          S              2  0
007311980716     D*-------------
007400940909     D* Schiere generali per la gestione dei tasti di funzione
007500940506     D $FM             S             27    DIM(24)
007600940506     D $FX             S             50    DIM(24)
007700940506     D $FC             S              1    DIM(24)
007800940506     D $FR             S              1    DIM(24)
007801980716     D* Indice della schiera tasti funzione
007802980716     D PosFun          S              2  0
007803980716     D*-------------
007900940909     D* Schiere per la gestione dei tasti di funzione
008000940506     D* Video D1
008100940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
008200940506     D $FX1            S             50    DIM(24) ALT($FM1)
008300940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008400940506     D $FR1            S              1    DIM(24) ALT($FC1)
008500940506     D* Schiere per la gestione dei tasti di funzione
008600940506     D* Video S1
008700940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
008800940506     D $FX2            S             50    DIM(24) ALT($FM2)
008900940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
009000940506     D $FR2            S              1    DIM(24) ALT($FC2)
009001980716     D* Schiera contente i nomi dei programmi da richiamare con i tasti
009002980716     D* funzione personalizzata S1
009003980716     D $PGF2           S             10    DIM(25)
009100940916     D* Schiere per la gestione dei tasti di funzione
009200940916     D* Video D2
009300940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
009400940916     D $FX3            S             50    DIM(24) ALT($FM3)
009500940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
009600940916     D $FR3            S              1    DIM(24) ALT($FC3)
009700940211     D*-------------
009800940207     D* Valori assunti
009900941115     D DSMPE           DS                  INZ
010000940207     D  $LOP                   1      1  0
010100940831     D  $ORD                   2      3  0
010200940909     D*-------------
010300940831    >D*Definire campi chiave per la vista logica 99L
010400940831    >D* DS per chiave aggancio
010500940831    >D DSKEY           DS
010600940831    >D*                                       1   90PRGKEY
010700940831    >D*                                      10  140RIGKEY
010800940207     D*-------------
010900940211     D* Passaggio Parametri
011000940211     D KPJBA         E DS
011100940225     D*-------------
011200940225     D* Reperimento Autorizzazioni
011300941012     D*****XAUT      E DSXCHKAUTDS
011400940325     D*-------------
011500940325     D* Parametri in ricezione
011600941012    >D A009DS        E DS                  EXTNAME(ANA009DS)
011700940325     D*-------------
011701980716     D* campo di salvataggio per la DS d'ingresso durante la chiamata
011702980716     D* alla funzione personalizzata
011703980716    >D $SaveDS         S                   like(A009DS)
011704980716     D*-------------
011800941012     D* Parametri del PGM di manutenzione ANA010R
011900941012    >D A010DS        E DS                  EXTNAME(ANA010DS) INZ
012000941012    >D  RET010       E                     INZ('0')
012100941012    >D  OPR010       E                     INZ('0')
012200941012    >D  ERR010       E                     INZ('0')
012300940211     D*-------------
012400940211     D DSFMT           DS           512
012500940506     D  $TASTO               369    369
012600940211     D  NRG                  370    370
012700940211     D  NCL                  371    371
012800940211     D  SFLNRR               378    379B 0
012900940211     D*-------------
013000940211     D* posizione cursore
013100940211     D CURSOR          DS
013200940211     D  RIRI                   1      2B 0 INZ
013300940211     D  R$$                    2      2
013400940211     D  COCO                   3      4B 0 INZ
013500940211     D  C$$                    4      4
013600940127     D*-------------
013700940127     D* Reperimento nome PGM
013800940127     D STATUS         SDS           333
013900940127     D  DSPGM            *PROC
014000940207     D*-------------
014100940616     D* Costanti
014200940616     D* nr.di righe del sfl (SFLPAG)
014300940314     D SFLPAG          C                   CONST(10)
014400940609     D* dimensione della schiera $MS1
014600940616     D*-------------
014700940506     D* Tasti di funzione
014800940506     D F01             C                   CONST(X'31')
014900940506     D F02             C                   CONST(X'32')
015000940506     D F03             C                   CONST(X'33')
015100940506     D F04             C                   CONST(X'34')
015200940506     D F05             C                   CONST(X'35')
015300940506     D F06             C                   CONST(X'36')
015400940506     D F07             C                   CONST(X'37')
015500940506     D F08             C                   CONST(X'38')
015600940506     D F09             C                   CONST(X'39')
015700940506     D F10             C                   CONST(X'3A')
015800940506     D F11             C                   CONST(X'3B')
015900940506     D F12             C                   CONST(X'3C')
016000940506     D F13             C                   CONST(X'B1')
016100940506     D F14             C                   CONST(X'B2')
016200940506     D F15             C                   CONST(X'B3')
016300940506     D F16             C                   CONST(X'B4')
016400940506     D F17             C                   CONST(X'B5')
016500940506     D F18             C                   CONST(X'B6')
016600940506     D F19             C                   CONST(X'B7')
016700940506     D F20             C                   CONST(X'B8')
016800940506     D F21             C                   CONST(X'B9')
016900940506     D F22             C                   CONST(X'BA')
017000940506     D F23             C                   CONST(X'BB')
017100940506     D F24             C                   CONST(X'BC')
017200940506     D ENTER           C                   CONST(X'F1')
017300940506     D ROLDWN          C                   CONST(X'F4')
017400940506     D ROLLUP          C                   CONST(X'F5')
017500941028      *-------------
017600941028      * Passaggio Parametri PGM ANA007R
017700941028     D A007DS        E DS                  EXTNAME(ANA007DS)
017800941028    >D  RET007       E                     INZ('0')
017900941028    >D  OPR007       E                     INZ('0')
018000941028    >D  ERR007       E                     INZ('0')
018100941103      * Dati di ambiente ottenuti da XSOC
018200941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
018300960102     D  XSCSLC       E                     EXTFLD(XSCSALDOC)
018400960102     D  XSCSLA       E                     EXTFLD(XSCSALDOA)
018500941103      * DS Interna per dati di output di XSOC
018600941103     D XSOCDS          DS          1000
018700940302     I*
018800940211     I/EJECT
018900941013     I* RIDENOMINAZIONE CAMPI
019000941111     IANSIF000
019100941205     I              SIFSOCIETA                  SIFSOC
019200941205     I              SIFCDSIA                    SIFSIA
019300941205     I              SIFCDSIAGR                  SIFSI2
019400941205     I              SIFUBICAZ                   SIFMCH
019500941205     I              SIFSYSINF                   SIFNSI
019600941205     I              SIFLINGUA                   SIFLIN
019700941205     I              SIFFLGOPE                   SIFFLG
019800941205     I              SIFDESBREV                  SIFDSI
019900941205     I              SIFDIVISA                   SIFDIV
020000941205     I              SIFINPUT                    SIFINP
020100941205     I              SIFLOCALIT                  SIFCIT
020200941205     I              SIFCDFISC                   SIFCDF
020300941205     I              SIFPARTIVA                  SIFIVA
020400941205     I              SIFPROV                     SIFPRV
020500941205     I              SIFSTATO                    SIFSTA
020600941205     I              SIFTELEFON                  SIFTEL
020700941205     I              SIFTELEX                    SIFTLX
020800941205     I              SIFTELEFAX                  SIFFAX
020900941205     I              SIFDTISCC                   SIFDTI
021000941205     I              SIFNATGIUR                  SIFNCR
021100941205     I              SIFDTAPBIL                  SIFDAB
021200941205     I              SIFDTSCBIL                  SIFDSA
021300941205     I              SIFISCCIAA                  SIFCCT
021400941205     I              SIFATCCIAA                  SIFATT
021500941205     I              SIFCAPOFCT                  SIFCPC
021600941205     I              SIFCAPOFAN                  SIFCPA
021700941205     I              SIFSOCFITT                  SIFSCF
021800941205     I              SIFCAPOFSN                  SIFCPF
021900941205     I              SIFDTFIVL                   SIFDFV
022000941205     I              SIFCDCTL20                  SIFCDC
022100960102     I              SIFSALDOC                   SIFSLC
022200960102     I              SIFSALDOA                   SIFSLA
022300940128     I*----------------------------------------------------*
022400941111     IANUNI01
022500941111     I              UNISOCIETA                  UNISOC
022600941111     I              UNIUNITA                    UNIUNI
022700941111     I              UNIDESBREV                  UNIDES
022800941028      *
022900941028      *----------------------------------------------------*
023000941028      *                MAIN LINE PROGRAM
023100941028      *----------------------------------------------------*
023200941028      *
023300941028      * inizializzazione variabili
023400940223     C                   EXSR      INZVAR
023500941028      *
023600940223     C     $FINE         DOWEQ     *OFF
023700940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
023800940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
023900940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
024000940117     C                   END
024100940117     C                   END
024200941103      * fine programma
024300940325     C                   EXSR      ENDPGM
024400941103      *
024500940325     C/EJECT
024600941103      ************************************************************
024700941103      * FINE PROGRAMMA
024800941103      ************************************************************
024900940325     C     ENDPGM        BEGSR
025000941103      *
025100941103      * aggiorno valori assunti
025200940722     C                   EXSR      AGGMPE
025300941012     C   KC              MOVE      '1'           RET009
025400941012     C   KL              MOVE      '2'           RET009
025500941103      *
025600941103      * passaggio dati al pgm chiamante
025700941012    >C                   MOVEL     A009DS        KPJBU
025800941103      *
025900941103      * Se pgm Guida non è stato richiamato posso chiudere in LR
026000941012    >C     OPZ009        IFEQ      *BLANK
026100940325     C                   SETON                                            LR
026200941103      * altrimenti chiudo in RT
026300940325     C                   ELSE
026400940325     C                   SETON                                            RT
026500940325     C                   ENDIF
026600940616     C                   RETURN
026700941103      *
026800940325     C                   ENDSR
026900940224     C/EJECT
027000941103      ************************************************************
027100941103      * RICERCHE
027200941103      ************************************************************
027300940127     C     SEARCH        BEGSR
027400941103      *
027500941103      * determino Riga/Colonna del cursore
027600940127     C                   MOVE      NRG           R$$
027700940127     C                   MOVE      NCL           C$$
027800940127     C                   Z-ADD     RIRI          CURR
027900940127     C                   Z-ADD     COCO          CURC
028000941103      *
0281009401271    C                   SELECT
028200941103      *
028300941103    > *--------------------------------------------------------------
028400941103    > * inserire le routine di ricerca
028500941103    > *--------------------------------------------------------------
028600941103      * FMTD1  - MODULO
028700941028    >C     H1NMFL        WHENEQ    'D1SOC'
028800941028    >C                   RESET                   A007DS
028900941028    >C                   MOVEL     '01'          OPZ007
029000941028    >C                   MOVEL     A007DS        KPJBU
029100941028    >C                   CALL      'ANA007R'
029200941028     C                   PARM                    KPJBA
029300941028     C                   MOVEL     KPJBU         A007DS
029400941028     C     SOC007        IFNE      *BLANK
029500941028     C                   MOVEL     SOC007        D1SOC
029600941028     C                   ENDIF
029700941103      * imposto pos. del cursore
029800940127     C                   Z-ADD     CURR          H1RIGA
029900940127     C                   Z-ADD     CURC          H1COLO
030000941103      *
030100941103      * ricerca non abilitata in questa posizione
0302009402231O   C                   OTHER
030300941103      * imposto pos. del cursore
030400940929     C                   Z-ADD     CURR          H1RIGA
030500940929     C                   Z-ADD     CURC          H1COLO
030600940929     C                   SETON                                        9899
030700941103      *
0308009401271-   C                   ENDSL
030900941103      *
031000940506     C                   MOVE      *HIVAL        $LASTV
031100941104     C                   ENDSR
031200940131     C/EJECT
031300941103      ************************************************************
031400941103      * RIEMPIMENTO OPZIONI
031500941103      ************************************************************
031600940131     C     RIEOPZ        BEGSR
031700941103      *
031800941103      * Input :
031900941103      * - $TX = schiera con tutti i testi disponibili
032000941103      * - $FL = schiere con i flag di validità associati ai testi
032100941103      * - WCOZ = variabile contenente l'indice dell'ultimo testo
032200941103      *          opzione usato
032300941103      * - WMXO = variabile contenente il numero massimo di testi
032400941103      *          opzione usabili
032500941103      * - $L01 = Lunghezza prima riga
032600941103      * - $L02 = Lunghezza seconda riga
032700941103      * Output :
032800941103      * - WOPZ1 = variabile contenente la prima riga opzioni
032900941103      * - WOPZ2 = variabile contenente la seconda riga opzioni
033000950403     C* - FF23  = flag che indica se c'è bisogno del tasto F23
033100941103      *
033200950403     C                   CALL      'X04OPMSG'
033300941103      * input
033400940223     C                   PARM                    $TX
033500940223     C                   PARM                    $FL
033600940225     C                   PARM                    WCOZ              3 0
033700940223     C                   PARM                    WMXO              3 0
033800941103      * output
033900940223     C                   PARM                    WOPZ1            76
034000940223     C                   PARM                    WOPZ2            76
034100950403     C                   PARM                    FF23              1
034200941103    > * input
034300941012    >C                   PARM                    $L01              3 0
034400941012    >C                   PARM                    $L02              3 0
034500941103      *
000000060224A1148C                   PARM                    $ALLOPT         960
000000060224A1148C*
034600940131     C                   ENDSR
034700940506     C/EJECT
034800941103      ************************************************************
034900941103      * RIEMPIMENTO TASTI FUNZIONE
035000941103      ************************************************************
035100940506     C     RIEKEY        BEGSR
035200941103      *
035300941103      * Input :
035400941103      * - $FX = schiera con tutti i testi disponibili
035500941103      * - $FC = schiere con i flag di validità associati ai testi
035600941103      * - $FR = schiere con i flag di REVERSE IMAGE
035700941103      * - $ULK = ultimo testo caricato
035800941103      * - $L01 = Lunghezza prima riga
035900941103      * - $L02 = Lunghezza seconda riga
036000941103      * Output :
036100941103      * - $ULK
036200941103      * - $KEY1= 1° riga di tasti funzione
036300941103      * - $KEY2= 2° riga di tasti funzione
036400941103      *
036500940506     C                   CALL      'XKEYMSG'
036600941103      * input
036700940506     C                   PARM                    $FX
036800940506     C                   PARM                    $FC
036900940506     C                   PARM                    $FR
037000940506     C                   PARM                    $ULK              3 0
037100941103      * output
037200940506     C                   PARM                    $KEY1            79
037300940506     C                   PARM                    $KEY2            79
037400941103    > * input
037500941103    > ***********          PARM           $L01    30
037600941103    > **********           PARM           $L02    30
037700941103      *
000000060224A1148C                   PARM      79            $L01              3 0
000000060224A1148C                   PARM      79            $L02              3 0
000000060224A1148C*
000000060224A1148C                   PARM                    $ALLFUNCT       480
000000060224A1148C*
037800940506     C                   ENDSR
037900940224     C/EJECT
038000941103      ************************************************************
038100941103      * GESTIONE EMISSIONE TASTI FUNZIONE
038200941103      ************************************************************
038300940224     C     GESPIE        BEGSR
038400941103      *
038500940224     C                   SELECT
038600941103      * Parzializzazioni
038700940224     C     $GEST         WHENEQ    'D1'
038800940506     C     $ULKD1        IFEQ      0
038900941103      * riempimento opzioni
039000940506     C                   EXSR      F24D1
039100940506     C                   ELSE
039200940506     C                   WRITE     FMTZ1
039300940506     C                   ENDIF
039400941103      * Lista
039500940506     C     $GEST         WHENEQ    'S1'
039600940506     C     $ULKS1        IFEQ      0
039700940506     C                   EXSR      F24S1
039800940506     C                   ELSE
039900940506     C                   WRITE     FMTZ2
040000940506     C                   ENDIF
040100941103      * Modifica valori assunti
040200940916     C     $GEST         WHENEQ    'D2'
040300940916     C     $ULKD2        IFEQ      0
040400940916     C                   EXSR      F24D2
040500940916     C                   ELSE
040600940916     C                   WRITE     FMTZ3
040700940916     C                   ENDIF
040800940506     C                   ENDSL
040900941103      *
041000940224     C                   ENDSR
041100940224     C/EJECT
041200941103      ************************************************************
041300941103      * GESTIONE PARZIALIZZAZIONI
041400941103      ************************************************************
041500940224     C     GESD1         BEGSR
041600941103      * inizializzazione videata
041700940223     C     $INZD1        IFEQ      *ON
041800940127     C                   EXSR      INZD1
041900940223     C                   MOVE      *OFF          $INZD1
042000940117     C                   END
042100941103      * emissione piede videata se proveniente da altra
042200941103      * salvataggio valore formato di provenienza
042300940202     C     $LASTV        IFNE      'D1'
042400941103      * salvo stato F17 per sapere cosa fare in caso di F12
042500940223     C                   MOVE      *INKR         WINKR
042600940202     C                   WRITE     FMTT1
042700940224     C                   EXSR      GESPIE
042800940202     C                   MOVE      $LASTV        $LASTG
042900940202     C                   MOVE      'D1'          $LASTV
043000940707     C                   ENDIF
043100941103      *
043200941103      * se esistono errori sulla videata
043300941103      * emetto la write del formato a indicatori spenti per vedere
043400941103      * le eventuali decodifiche
043500940707     C     *IN99         IFEQ      *ON
043600940707     C                   MOVEA     *IN           WIN
043700940707     C                   MOVE      *ALL'0'       IN5098           49
043800940707     C                   MOVEA     IN5098        *IN(50)
043900940707     C                   WRITE     FMTD1
044000940707     C                   MOVEA     WIN           *IN
044100940707     C                   ENDIF
044200941103      *
044300940131     C                   EXFMT     FMTD1
044400941103      * controllo abilitazione tasti funzione
044500941103      *
044600940506     C                   EXSR      CTRKD1
044700941103      *
044800940506     C     *IN99         IFEQ      *OFF
044900941103      *
045000940117     C                   SELECT
045100941103      * F2=Decodifica
045200940916     C     $TASTO        WHENEQ    F02
045300940916     C                   EXSR      F02D1
045400941103      * F3=Fine
045500940506     C     $TASTO        WHENEQ    F03
045600940307     C                   EXSR      F03D1
045700941103      * F4=Ricerca
045800940506     C     $TASTO        WHENEQ    F04
045900940127     C                   EXSR      SEARCH
046000941103      * F5=Refresh
046100940916     C     $TASTO        WHENEQ    F05
046200940916     C                   EXSR      F05D1
046300941103      * F12=Ritorno
046400940506     C     $TASTO        WHENEQ    F12
046500940307     C                   EXSR      F12D1
046600941103      * F18=Modifica valori assunti
046700940506     C     $TASTO        WHENEQ    F18
046800940307     C                   EXSR      F18D1
046900941103      * F24=Altri tasti
047000940506     C     $TASTO        WHENEQ    F24
047100940309     C                   EXSR      F24D1
047200941103      *
047300940117     C                   OTHER
047400940127     C                   EXSR      CTRD1
047500940127     C     *IN99         IFEQ      *OFF
047600941103      * definisco l'ordinamento
047700940131     C                   SELECT
047800941103    > * << nel caso di più viste logiche e quindi più campi di
047900941103    > *    parzializzazione, qui vanno definiti i criteri per
048000941103    > *    determinare quale VL va letta, a seconda di quali campi
048100941103    > *    sono stati valorizzati >>
048200941012    >C     D1SOC         WHENNE    *BLANK
048300940131     C                   Z-ADD     1             $ORD
048400940131     C                   ENDSL
048500940127     C                   MOVE      'S1'          $GEST
048600940223     C                   MOVE      *ON           $INZS1
048700941103      ******               MOVE *BLANK    W1POS
048800940211     C                   ENDIF
048900941103      *
049000940117     C                   ENDSL
049100941103      *
049200940506     C                   ENDIF
049300940117     C                   ENDSR
049400940224     C/EJECT
049500941103      ************************************************************
049600941103      * INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
049700941103      ************************************************************
049800940127     C     INZD1         BEGSR
049900941103      *
050000940318     C                   CLEAR                   FMTD1
050100941103      * Se pgm non richiamato
050200941012     C     OPZ009        IFEQ      *BLANK
050300940831     C                   ELSE
050400941103      * Se pgm richiamato
050500950615     C                   MOVEL     SOC009        D1SOC
050600950615      *
050700950615     C                   END
050800941103      *
050900950615     C     D1SOC         IFEQ      *BLANK
051000950615     C     $LOP          ANDEQ     2
051100950615     C     $TASTO        ANDNE     F05
051200950615     C                   MOVEL     XSCDSO        D1SOC
051300950615     C                   ENDIF
051400940117     C                   ENDSR
051500940506     C/EJECT
051600941103      ************************************************************
051700941103      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
051800941103      * INPUT
051900941103      * $FC1 = Abilitazioni videata D!
052000941103      * OUTPUT
052100941103      * *IN97 *IN99 Errori
052200941103      ************************************************************
052300940506     C     CTRKD1        BEGSR
052400940506     C                   SETOFF                                       99
052500940506     C                   MOVEA     $FC1          $FC
052600940506     C                   EXSR      CTRKEY
052700940506     C     *IN99         IFEQ      *ON
052800940506     C                   SETON                                        97
052900940506     C                   ENDIF
053000940506     C                   ENDSR
053100940506     C/EJECT
053200941103      ************************************************************
053300941103      * CONTROLLO ABILITAZIONE TASTI VIDEATA S1
053400941103      ************************************************************
053500940506     C     CTRKS1        BEGSR
053600940506     C                   SETOFF                                       99
053700940506     C                   MOVEA     $FC2          $FC
053800940506     C                   EXSR      CTRKEY
053900940506     C     *IN99         IFEQ      *ON
054000940506     C                   SETON                                        97
054100940506     C                   ENDIF
054200940506     C                   ENDSR
054300940506     C/EJECT
054400941103      ************************************************************
054500941103      * CONTROLLO ABILITAZIONE TASTI
054600941103      * INPUT
054700941103      * $TASTO =Tasto funzione premuto
054800941103      * $FC = Tasti abilitati
054900941103      * OUTPUT
055000941103      * IN99 = ERrore
055100941103      ************************************************************
055200940506     C     CTRKEY        BEGSR
055300940506     C                   SELECT
055400940506     C     $TASTO        WHENEQ    F01
055401980716     C                   z-add     01            posfun
055500940720     C     $FC(1)        IFEQ      '0'
055600940506     C                   SETON                                        99
055700940506     C                   ENDIF
055800940506     C     $TASTO        WHENEQ    F02
055801980716     C                   z-add     02            posfun
055900940720     C     $FC(2)        IFEQ      '0'
056000940506     C                   SETON                                        99
056100940506     C                   ENDIF
056200940506     C     $TASTO        WHENEQ    F03
056201980716     C                   z-add     03            posfun
056300940720     C     $FC(3)        IFEQ      '0'
056400940506     C                   SETON                                        99
056500940506     C                   ENDIF
056600940506     C     $TASTO        WHENEQ    F04
056601980716     C                   z-add     04            posfun
056700940720     C     $FC(4)        IFEQ      '0'
056800940506     C                   SETON                                        99
056900940506     C                   ENDIF
057000940506     C     $TASTO        WHENEQ    F05
057001980716     C                   z-add     05            posfun
057100940720     C     $FC(5)        IFEQ      '0'
057200940506     C                   SETON                                        99
057300940506     C                   ENDIF
057400940506     C     $TASTO        WHENEQ    F06
057401980716     C                   z-add     06            posfun
057500940720     C     $FC(6)        IFEQ      '0'
057600940506     C                   SETON                                        99
057700940506     C                   ENDIF
057800940506     C     $TASTO        WHENEQ    F07
057801980716     C                   z-add     07            posfun
057900940720     C     $FC(7)        IFEQ      '0'
058000940506     C                   SETON                                        99
058100940506     C                   ENDIF
058200940506     C     $TASTO        WHENEQ    F08
058201980716     C                   z-add     08            posfun
058300940720     C     $FC(8)        IFEQ      '0'
058400940506     C                   SETON                                        99
058500940506     C                   ENDIF
058600940615     C     $TASTO        WHENEQ    F09
058601980716     C                   z-add     09            posfun
058700940720     C     $FC(09)       IFEQ      '0'
058800940506     C                   SETON                                        99
058900940506     C                   ENDIF
059000940506     C     $TASTO        WHENEQ    F10
059001980716     C                   z-add     10            posfun
059100940720     C     $FC(10)       IFEQ      '0'
059200940506     C                   SETON                                        99
059300940506     C                   ENDIF
059400940506     C     $TASTO        WHENEQ    F11
059401980716     C                   z-add     11            posfun
059500940720     C     $FC(11)       IFEQ      '0'
059600940506     C                   SETON                                        99
059700940506     C                   ENDIF
059800940506     C     $TASTO        WHENEQ    F12
059801980716     C                   z-add     12            posfun
059900940720     C     $FC(12)       IFEQ      '0'
060000940506     C                   SETON                                        99
060100940506     C                   ENDIF
060200940506     C     $TASTO        WHENEQ    F13
060201980716     C                   z-add     13            posfun
060300940720     C     $FC(13)       IFEQ      '0'
060400940506     C                   SETON                                        99
060500940506     C                   ENDIF
060600940506     C     $TASTO        WHENEQ    F14
060601980716     C                   z-add     14            posfun
060700940720     C     $FC(14)       IFEQ      '0'
060800940506     C                   SETON                                        99
060900940506     C                   ENDIF
061000940506     C     $TASTO        WHENEQ    F15
061001980716     C                   z-add     15            posfun
061100940720     C     $FC(15)       IFEQ      '0'
061200940506     C                   SETON                                        99
061300940506     C                   ENDIF
061400940506     C     $TASTO        WHENEQ    F16
061401980716     C                   z-add     16            posfun
061500940720     C     $FC(16)       IFEQ      '0'
061600940506     C                   SETON                                        99
061700940506     C                   ENDIF
061800940506     C     $TASTO        WHENEQ    F17
061801980716     C                   z-add     17            posfun
061900940720     C     $FC(17)       IFEQ      '0'
062000940506     C                   SETON                                        99
062100940506     C                   ENDIF
062200940506     C     $TASTO        WHENEQ    F18
062201980716     C                   z-add     01            posfun
062300940720     C     $FC(18)       IFEQ      '0'
062400940506     C                   SETON                                        99
062500940506     C                   ENDIF
062600940506     C     $TASTO        WHENEQ    F19
062601980716     C                   z-add     19            posfun
062700940720     C     $FC(19)       IFEQ      '0'
062800940506     C                   SETON                                        99
062900940506     C                   ENDIF
063000940506     C     $TASTO        WHENEQ    F20
063001980716     C                   z-add     20            posfun
063100940720     C     $FC(20)       IFEQ      '0'
063200940506     C                   SETON                                        99
063300940506     C                   ENDIF
063400940506     C     $TASTO        WHENEQ    F21
063401980716     C                   z-add     21            posfun
063500940720     C     $FC(21)       IFEQ      '0'
063600940506     C                   SETON                                        99
063700940506     C                   ENDIF
063800940506     C     $TASTO        WHENEQ    F22
063801980716     C                   z-add     22            posfun
063900940720     C     $FC(22)       IFEQ      '0'
064000940506     C                   SETON                                        99
064100940506     C                   ENDIF
064200940506     C     $TASTO        WHENEQ    F23
064201980716     C                   z-add     23            posfun
064300940720     C     $FC(23)       IFEQ      '0'
064400940506     C                   SETON                                        99
064500940506     C                   ENDIF
064600940506     C     $TASTO        WHENEQ    F24
064601980716     C                   z-add     24            posfun
064700940720     C     $FC(24)       IFEQ      '0'
064800940506     C                   SETON                                        99
064900940506     C                   ENDIF
065000940506     C                   ENDSL
065100940506     C                   ENDSR
065200940916     C/EJECT
065300941103      ************************************************************
065400941103      * GESTIONE F02 VIDEO D1
065500941103      ************************************************************
065600940916     C     F02D1         BEGSR
065700941103      *
065800941103      * per decodificare la videata richiamo la routine dei ctrl
065900941103      * e poi disattivo gli eventuali messaggi di errore
066000940916     C                   EXSR      CTRD1
066100941103      *
066200940916     C                   MOVE      *ALL'0'       IN5098
066300940916     C                   MOVEA     IN5098        *IN(50)
066400941103      *
066500940916     C                   ENDSR
066600940224     C/EJECT
066700941103      ************************************************************
066800941103      * GESTIONE F03 VIDEO D1
066900941103      ************************************************************
067000940307     C     F03D1         BEGSR
067100941103      *
067200940307     C                   MOVE      *ON           $FINE
067300941103      * fine programma
067400940325     C                   EXSR      ENDPGM
067500941103      *
067600940307     C                   ENDSR
067700940916     C/EJECT
067800941103      ************************************************************
067900941103      * GESTIONE F05 VIDEO D1
068000941103      ************************************************************
068100940916     C     F05D1         BEGSR
068200941103      *
068300940916     C                   MOVE      *ON           $INZD1
068400941103      *
068500940916     C                   ENDSR
068600940307     C/EJECT
068700941103      ************************************************************
068800941103      * GESTIONE F12 VIDEO D1
068900941103      ************************************************************
069000940307     C     F12D1         BEGSR
069100941103      *
069200941103      *  se la 1a videata è la lista oppure sono arrivato qui con F17,
069300941103      *  per F12 torno alla lista
069400940307     C     $LOP          IFEQ      2
069500940307     C     WINKR         OREQ      *ON
069600940307     C                   MOVE      'S1'          $GEST
069700941103      *  altrimenti, per F12 torno a menu
069800940307     C                   ELSE
069900940307     C                   MOVE      *ON           $FINE
070000940307     C                   END
070100941103      *
070200940307     C                   ENDSR
070300940307     C/EJECT
070400941103      ************************************************************
070500941103      * GESTIONE F18 VIDEO D1
070600941103      ************************************************************
070700940307     C     F18D1         BEGSR
070800941103      *
070900940307     C                   MOVE      'D2'          $GEST
071000940307     C                   MOVEL     *ON           $INZD2
071100941103      *
071200940307     C                   ENDSR
071300940307     C/EJECT
071400941103      ************************************************************
071500941103      * GESTIONE F24 VIDEO D1
071600941103      * input
071700941103      * $ULKD1 = Ultimo tasto funzione emesso per video D1
071800941103      * $FX1 =Testi tasti funzione
071900941103      * $FC1 = Abilitazioni
072000941103      * $FR1 = Inversioni di fondo
072100941103      ************************************************************
072200940307     C     F24D1         BEGSR
072300941103      *
072400940506     C                   MOVEA     $FX1          $FX
072500940506     C                   MOVEA     $FC1          $FC
072600940506     C                   MOVEA     $FR1          $FR
072700940506     C                   Z-ADD     $ULKD1        $ULK
072800940929     C                   Z-ADD     79            $L01
072900940929     C                   Z-ADD     79            $L02
073000940506     C                   EXSR      RIEKEY
073100940506     C                   Z-ADD     $ULK          $ULKD1            3 0
073200940720     C                   MOVEA     $FC           $FC1
073300940506     C                   MOVEL     $KEY1         Z1KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
073400940506     C                   MOVEL     $KEY2         Z1KE2
073500940506     C                   WRITE     FMTZ1
073600941103      *
073700941103      *
073800940307     C                   ENDSR
073900940307     C/EJECT
074000941103      ************************************************************
074100941103      * CONTROLLO VIDEATA PARZIALIZZAZIONI
074200941103      ************************************************************
074300940307     C     CTRD1         BEGSR
074400941103      *
074500940127     C                   SETOFF                                       99
074600941103      * non imposto pos. del cursore
074700941012     C                   Z-ADD     0             H1RIGA
074800940127     C                   Z-ADD     0             H1COLO
074900941103      *
0750009410132   >C     D1SOC         IFEQ      *BLANK
075100941013    >C                   SETON                                        5199
075200941103      * CTRL VALIDITA' SOCIETA'
075300941013    >C                   ELSE
075400941013    >C                   MOVE      D1SOC         SIFSOC
075500941013    >C     K1SIF1        SETLL     ANSIF01L                               21
075600941013    >C     *IN21         IFEQ      *OFF
075700941013    >C                   SETON                                        5399
075800941013    >C                   ENDIF
075900941013    >C                   ENDIF
076000941103      *
076100941103      *
076200940117     C                   ENDSR
076300940224     C/EJECT
076400941103      ************************************************************
076500941103      * GESTIONE LISTA
076600941103      ************************************************************
076700940127     C     GESS1         BEGSR
076800941103      * inizializzazione videata
076900940223     C     $INZS1        IFEQ      *ON
077000940127     C                   EXSR      INZS1
077100940223     C                   MOVE      *OFF          $INZS1
077200940127     C                   ENDIF
077300941103      * emissione piede videata se proveniente da altra
077400941103      * salvataggio valore formato di provenienza
077500940202     C     $LASTV        IFNE      'S1'
077600940202     C                   WRITE     FMTT1
077700940224     C                   EXSR      GESPIE
077800940202     C                   MOVE      $LASTV        $LASTG
077900940202     C                   MOVE      'S1'          $LASTV
078000940202     C                   END
078100941103      *
078200940223     C     WMAX          IFEQ      0
078300940211     C                   WRITE     FMTD3                                        NO RCD
078400940211     C                   ENDIF
078401980716     C* Inizializzazione indice tasti funzione personalizzati
078402980716     C                   Eval      PosFun = 25
078403980716     C*
078500941103      *Serve per emissione finestra pgm invio ang727r (per open/close
078600941103      *window)
078700940831     C                   MOVE      '1'           WIN727            1
078800941103      *
078900941103      *
079000940131     C                   EXFMT     FMTC1
079100940204     C     C1NRR         IFNE      0
079200940204     C                   Z-ADD     C1NRR         WSFL
079300940204     C                   ENDIF
079400940127     C                   Z-ADD     SFLNRR        C1RCD
079500941103      *
079600941103      * controllo abilitazione tasti funzione
079700941103      *
079800940506     C                   EXSR      CTRKS1
079900940506     C     *IN99         IFEQ      *OFF
080000941103      *
080100941103      *
0802009401271    C                   SELECT
080300941103      * F3=Fine
080400940506     C     $TASTO        WHENEQ    F03
080500940309     C                   EXSR      F03S1
080600941103      * F10=Immissione
080700940506     C     $TASTO        WHENEQ    F10
080800940309     C                   EXSR      F10S1
080900941103      * F12=Ritorno
081000940506     C     $TASTO        WHENEQ    F12
081100940309     C                   EXSR      F12S1
081200941103      * F17=Parzializzazioni
081300940506     C     $TASTO        WHENEQ    F17
081400940309     C                   EXSR      F17S1
081500941103      * F18=Modifica valori assunti
081600940506     C     $TASTO        WHENEQ    F18
081700940309     C                   EXSR      F18S1
081800941103      * F23=Altre opzioni
081900940506     C     $TASTO        WHENEQ    F23
082000940309     C                   EXSR      F23S1
082100941103      * F24=Altri tasti
082200940506     C     $TASTO        WHENEQ    F24
082300940309     C                   EXSR      F24S1
082400941103      * ROLL-UP
082500940506     C     $TASTO        WHENEQ    ROLLUP
082600940127     C                   EXSR      ROLS1
082700980716     C*
082701980716     C* Personalizzata
082702980716     C     $PGF2(PosFun) Whenne    *BLANK
082703980716     C                   EXSR      FperS1
082704980716     C*
0828009401271O   C                   OTHER
082900941103      * CONTROLLO DATI
083000940131     C                   EXSR      CTRC1
083100940201     C     *IN99         IFEQ      *OFF
083200940131     C                   EXSR      CTRS1
083300940131     C                   END
0834009401271-   C                   ENDSL
083500941103      *
083600940506     C                   ENDIF
083700940127     C                   ENDSR
083800940224     C/EJECT
083801980716     C************************************************************
083802980716     C* Chiamata programma con tasto personalizzato
083803980716     C************************************************************
083804980716     C     FPerS1        BEGSR
083805980716     C*
083806980716     C* salvo la DS di ingresso
083807980716    >C                   MOVEL     A009DS        $SaveDS
083808980716     C*
083809980716    >C                   MOVEL     A009DS        KPJBU
083810980716    >C                   CALL      $PGF2(PosFun)                        22
083811980716     C                   PARM                    KPJBA
083812980716     C                   PARM                    NOMPGM
083813980716     C*
083814980716     C* ripristino la DS di ingresso
083815980716    >C                   MOVEL     $SaveDS       A009DS
083816980716     C*
083817980716     C                   ENDSR
083818980716     C/EJECT
083900941103      ************************************************************
084000941103      * INIZIALIZZAZIONE LISTA
084100941103      ************************************************************
084200940127     C     INZS1         BEGSR
084300941103      * pulizia SFL
084400940128     C                   SETOFF                                         3031
084500940131     C                   WRITE     FMTC1
084600940128     C                   SETON                                          31
084700941103      *
084800941103      * riempimento opzioni
084900940223     C                   MOVEA     $TX1          $TX
085000940223     C                   MOVEA     $FL1          $FL
085100941012     C                   Z-ADD     0             WCOZ
085200941103      *  equivale al num.di elementi della schiera $TX1
085300980716     C                   Z-ADD     opzlen        WMXO
085400950403     C                   Z-ADD     76            $L01
085500950403     C                   Z-ADD     76            $L02
085600940128     C                   EXSR      RIEOPZ
085700950403     C                   Z-ADD     23            X
085800950403     C                   MOVE      FF23          $FC2(X)
085900940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
086000940608     C                   MOVEL     WOPZ2         C1OPZ2
086100940225     C                   Z-ADD     WCOZ          C1COZ
086200941103      *
086300941103      * CARICAMENTO SFL A PAGINE
086400941103      *
086500940201     C                   Z-ADD     0             S1NRR
086600940128     C                   Z-ADD     0             WMAX
086700941014      *CARICAMENTO CAMPI SFLCTL
086800941014    >C                   MOVEL     D1SOC         C1SOC
086900941014    >C                   MOVEL     D1DSI         C1DSI
087000941103      *
087100941103      * Posizionamento su file pilota
087200941103      *********            CLEARC1POS
087300941012    >C                   EXSR      SETUNI
087400941103      * ripristino i valori del posizionamento
087500941103      *********            MOVE W1POS     C1POS
087600941103      *
087700941103      * Lettura file pilota
087800941012    >C                   EXSR      REDUNI
087900941103      *
088000941103      * Carico 1a pagina
088100940127     C                   EXSR      ROLS1
088200940201     C                   Z-ADD     SFLPAG        WPAG
088300941103      *
088400940127     C                   ENDSR
088500940127     C/EJECT
088600941103      ************************************************************
088700941103      * CARICAMENTO PAGINA LISTA
088800941103      ************************************************************
088900940127     C     ROLS1         BEGSR
089000941103      *
089100940128     C                   SETOFF                                       32
089200940223     C                   Z-ADD     0             Y
089300940127     C                   Z-ADD     WMAX          S1NRR
089400941103      *
089500941103      * Leggo dal file anagrafico per caricare la lista
089600941103      *
0897009401311    C     $EFILE        DOWEQ     *OFF
089800940201     C     Y             ANDLT     WPAG
089900941103      *
090000940131     C                   EXSR      RIES1
090100941103      *
090200940127     C                   ADD       1             S1NRR
090300940127     C                   ADD       1             Y
090400941103      *
090500940131     C                   WRITE     FMTS1
090600941103      *
090700941012    >C                   EXSR      REDUNI
090800941103      *
0909009401271-   C                   ENDDO
091000941103      *
091100940223     C                   Z-ADD     S1NRR         WMAX                 30
091200941103      *
091300941103      * POSIZIONAMENTO AL 1° RCD DELLA PAGINA
091400941103      *
091500940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
091600940127     C                   MVR                     RESTO             3 0
091700940131     C     PAGINE        MULT      SFLPAG        C1RCD
0918009401271    C     RESTO         IFGT      0
091900940127     C                   ADD       1             C1RCD
0920009401271E   C                   ELSE
092100940407     C                   SUB       SFLPAG        C1RCD
092200940407     C                   ADD       1             C1RCD
0923009401271-   C                   ENDIF
092400941103      *
092500940127     C                   ENDSR
092600940224     C/EJECT
092700941103      ************************************************************
092800941103      * POSIZIONAMENTO RCD ARCHIVIO PILOTA
092900941103      ************************************************************
093000941012    >C     SETUNI        BEGSR
093100941103      *
093200940224     C                   SELECT
093300941103    > * Ord.1 = chiave ordinamento 1
093400940224     C     $ORD          WHENEQ    1
093500941103    > *-----------------------------------------------------------
093600941103    > * Gestione posizionamento
093700941103    > * Se il campo W1POS è > *blank imposto il posizionamento con
093800941103    > * W1POS altrimenti lo imposto con il valore di parametrizzazione
093900941103    > *-----------------------------------------------------------
094000941012    >C                   MOVEL     D1SOC         UNISOC
094100941013    >C                   MOVEL     *BLANK        UNIUNI
094200941012    >C     K2UNI1        SETLL     ANUNI01L
094300940224     C                   ENDSL
094400941103      *
094500940224     C                   ENDSR
094600940128     C/EJECT
094700941103      ************************************************************
094800941103      * LETTURA RCD ARCHIVIO PILOTA
094900941103      ************************************************************
095000941012    >C     REDUNI        BEGSR
095100941103      *
095200940131     C                   MOVEL     *OFF          $EFILE
095300940131     C                   MOVEL     *OFF          $RCDOK
095400941103      *
0955009401311    C     $EFILE        DOUEQ     *ON
095600940131     C     $RCDOK        OREQ      *ON
095700941103      *
0958009401312    C                   SELECT
095900940131     C     $ORD          WHENEQ    1
096000941012    >C                   READ      ANUNI01                                21
0961009401312-   C                   ENDSL
096200941103      *
096300940131     C                   MOVE      *IN21         $EFILE
096400941103      *
0965009401312    C     $EFILE        IFEQ      *OFF
096600941012    >C                   EXSR      SELUNI
0967009401312-   C                   ENDIF
096800941103      *
0969009401311-   C                   ENDDO
097000941103      *
097100940131     C                   MOVE      *IN21         *IN33                          SflEnd
097200941103      *
097300940131     C                   ENDSR
097400940224     C/EJECT
097500941103      ************************************************************
097600941103      * CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
097700941103      ************************************************************
097800940224     C     CLCPAG        BEGSR
097900941103      * Input :
098000941103      * - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
098100941103      *          cursore
098200941103      * - SFLPAG = numero rcd per pagina sfl
098300941103      * Output :
098400941103      * - WPAG = pagina fino a cui deve essere ricaricato il sfl
098500941103      *
098600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
098700940224     C                   MVR                     RESTO             3 0
098800940224     C     RESTO         IFGT      0
098900940224     C                   ADD       1             PAGINE
099000940224     C                   ENDIF
099100940224     C     PAGINE        MULT      SFLPAG        WPAG
099200941103      *
099300940224     C                   ENDSR
099400940131     C/EJECT
099500941103      ************************************************************
099600941103      * SELEZIONI SULLA RIGA LETTA
099700941103      * impostare a *OFF il campo $RCDOK per scartare un record che
099800941103      * non rientra nei parametri di parzializzazione
099900941103      * Eseguire una *HIVAL SETLL al raggiungimento di un limite
100000941103      * superiore stabilito nei parametri
100100941103      *
100200941103      ************************************************************
100300941012    >C     SELUNI        BEGSR
100400941103      *
100500940202     C                   MOVEL     *ON           $RCDOK
100600940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
100600940131A1211C                   MOVEL     *hival        ALL9             15
100700941103      * creo limite massimo parzializzazioni
100800941103    > * << per ogni valore alfanumerico creo il limite superiore
100900941103    > *    per cui caricare, creando una variabile che contiene
101000941103    > *    il valore digitato nel campo a video riempito di "9" >>
101100941103      *
101200940314     C                   SELECT
101300941103      *
101400941103      * SELEZIONI RISPETTO AI VALORI RCD
101500941103      *
101600941103    > * << elaborare eventuali selezioni che non riguardano le
101700941103    > *    parzializzazioni ma più propriamente i dati del rcd ;
101800941103    > *    esempio : esclusione degli annullati. >>
101900941103      *
102000941103      * SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
102100941103      *
102200941103    > * oggetto non compreso nella scelta
102300941012    >C     UNISOC        WHENNE    D1SOC
102400941012    >C                   MOVEL     *OFF          $RCDOK
102500941012    >C     UNISOC        IFGT      D1SOC
102600941012    >C     $ORD          ANDEQ     1
102700941012    >C     *HIVAL        SETLL     ANUNI01
102800941012    >C                   ENDIF
102900941103      *
103000940202     C                   ENDSL
103100941103      *
103200940202     C                   ENDSR
103300940131     C/EJECT
103400941103      ************************************************************
103500941103      * RIEMPIMENTO RIGA LISTA
103600941103      ************************************************************
103700940131     C     RIES1         BEGSR
103800941103      *
103900940131     C                   CLEAR                   FMTS1
104000941103    > * << riempio i campi del sfl, compresi gli hidden con i valori
104100941103    > *    del rcd letto >>
104200941103      *
104300941013    >C                   MOVEL     UNIUNI        S1UNI
104400941012    >C                   MOVEL     UNIDES        S1DES
104500941103      *
104600940131     C                   ENDSR
104700940309     C/EJECT
104800941103      ************************************************************
104900941103      * GESTIONE F03 VIDEO S1
105000941103      ************************************************************
105100940309     C     F03S1         BEGSR
105200941103      *
105300940309     C                   MOVE      *ON           $FINE
105400941103      * fine programma
105500940325     C                   EXSR      ENDPGM
105600941103      *
105700940309     C                   ENDSR
105800940309     C/EJECT
105900941103      ************************************************************
106000941103      * GESTIONE F10 VIDEO S1
106100941103      * AGGIUNTA RECORD
106200941103      ************************************************************
106300940309     C     F10S1         BEGSR
106400941103      *
106500941012    >C                   RESET                   A010DS
106600941012    >C                   MOVEL     '01'          OPZ010
106700941013    >C                   MOVEL     C1SOC         SOC010
106800941012    >C                   MOVEL     A010DS        KPJBU
106900941012    >C                   CALL      'ANA010R'
107000940309     C                   PARM                    KPJBA
107100941012    >C                   MOVEL     KPJBU         A010DS
107200941103      * ritorno da PGM gestione
107300941012    >C                   EXSR      RTN010
107400940309     C     WINZS1        IFEQ      *ON
107500940309     C                   MOVE      *ON           $INZS1
107600941103      * se esistevano già righe sul sfl
107700941103      * calcolo pagina a cui deve posizionarsi
107800940309     C     WSFL          IFGT      0
107900940309     C                   EXSR      CLCPAG
108000941103      * altrimenti carico solo la 1a pagina
108100940309     C                   ELSE
108200940309     C                   Z-ADD     SFLPAG        WPAG
108300940309     C                   ENDIF
108400940309     C                   ENDIF
108500941103      *
108600940627     C                   MOVE      *HIVAL        $LASTV
108700941103      *
108800940309     C                   ENDSR
108900940309     C/EJECT
109000941103      ************************************************************
109100941103      * GESTIONE F12 VIDEO S1
109200941103      ************************************************************
109300940309     C     F12S1         BEGSR
109400941103      *
109500941103      *  se la 1a videata sono le parzializzazioni, per F12 torno a queste
109600940309     C     $LOP          IFEQ      1
109700940309     C                   MOVE      'D1'          $GEST
109800941103      *  se la 1a videata è questa, per F12 torno a menu
109900940309     C                   ELSE
110000940309     C                   MOVE      *ON           $FINE
110100940309     C                   END
110200941103      *
110300940309     C                   ENDSR
110400940309     C/EJECT
110500941103      ************************************************************
110600941103      * GESTIONE F17 VIDEO S1
110700941103      ************************************************************
110800940309     C     F17S1         BEGSR
110900941103      *
111000940309     C                   MOVE      'D1'          $GEST
111100941103      * forza certi valori nel caso il pgm sia stato richiamato con
111200941103      * chiave specifica
111300941103    > ***        PRG009    IFNE 0
111400941103    > ****                 Z-ADD0         PRG009
111500941103      ****                 END
111600940831     C     $ORD          IFEQ      99
111700940831     C                   Z-ADD     1             $ORD
111800941103    > ******               Z-ADDUDATE     D1DRG
111900940831     C                   END
112000941103      *
112100940309     C                   ENDSR
112200940309     C/EJECT
112300941103      ************************************************************
112400941103      * GESTIONE F18 VIDEO S1
112500941103      ************************************************************
112600940309     C     F18S1         BEGSR
112700941103      *
112800940309     C                   MOVE      'D2'          $GEST
112900940309     C                   MOVEL     *ON           $INZD2
113000940831     C     $ORD          IFEQ      99
113100940831     C                   Z-ADD     1             $ORD
113200940831     C                   END
113300941103      *
113400940309     C                   ENDSR
113500940309     C/EJECT
113600941103      ************************************************************
113700941103      * GESTIONE F23 VIDEO S1
113800941103      ************************************************************
113900940309     C     F23S1         BEGSR
114000941103      *
114100950403     C                   Z-ADD     76            $L01
114200950403     C                   Z-ADD     76            $L02
114300940309     C                   EXSR      RIEOPZ
114400950403     C                   Z-ADD     23            X
114500950403     C                   MOVE      FF23          $FC2(X)
114600940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
114700940608     C                   MOVEL     WOPZ2         C1OPZ2
114800940309     C                   Z-ADD     WCOZ          C1COZ
114900941103      *
115000940309     C                   ENDSR
115100940309     C/EJECT
115200941103      ************************************************************
115300941103      * GESTIONE F24 VIDEO S1
115400941103      ************************************************************
115500940309     C     F24S1         BEGSR
115600941103      *
115700941103      *
115800940506     C                   MOVEA     $FX2          $FX
115900940506     C                   MOVEA     $FC2          $FC
116000940506     C                   MOVEA     $FR2          $FR
116100940506     C                   Z-ADD     $ULKS1        $ULK
116200940929     C                   Z-ADD     79            $L01
116300940929     C                   Z-ADD     79            $L02
116400940506     C                   EXSR      RIEKEY
116500940506     C                   Z-ADD     $ULK          $ULKS1            3 0
116600940720     C                   MOVEA     $FC           $FC2
116700940506     C                   MOVEL     $KEY1         Z2KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
116800940506     C                   MOVEL     $KEY2         Z2KE2
116900940506     C                   WRITE     FMTZ2
117000941103      *
117100940309     C                   ENDSR
117200940128     C/EJECT
117300941103      ************************************************************
117400941103      * CONTROLLO TESTATA LISTA
117500941103      ************************************************************
117600940131     C     CTRC1         BEGSR
117700941103      *
117800940201     C                   MOVE      *OFF          *IN99
117900941103      *
118000941103      * SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
118100941103      * L'INIZIALIZZAZIONE LISTA
118200941103      *
118300941103    > ******     C1POS     IFNE W1POS
118400941103      ******               EXSR POSS1
118500941103      *******              END
118600941103      *
118700940202     C                   ENDSR
118800940202     C/EJECT
118900941103      ************************************************************
119000941103      * CONTROLLI PER POSIZIONAMENTO SU LISTA
119100941103      ************************************************************
119200941103      *          POSS1     BEGSR
119300941103      *
119400941103      * CONTROLLO SE ESISTONO OPZIONI ATTIVE
119500941103      *
1196009411031     *          WMAX      IFGT 0
119700941103      *                    READCFMTS1                    21
1198009411032     *          *IN21     DOWEQ*OFF
119900941103      *          *IN99     ANDEQ*OFF
120000941103      *
1201009411033     *          S1OPZ     IFNE 0
120200941103      *                    SETON                     51  99
1203009411033-    *                    ENDIF
120400941103      * se rilevata opzione attiva, attivo sflnxtchg
1205009411033     *          *IN99     IFEQ *ON
120600941103      *                    MOVE *ON       *IN32
120700941103      *                    UPDATFMTS1
1208009411033E    *                    ELSE
120900941103      *                    READCFMTS1                    21
1210009411033-    *                    ENDIF
121100941103      *
1212009411032-    *                    ENDDO
121300941103      *
1214009411031-    *                    ENDIF
121500941103      *
121600941103      * SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
121700941103      * AL VALORE DI POSIZIONAMENTO :
121800941103      * se questo è maggiore del valore del rispettivo campo di
121900941103      * parzializzazione lo uso, altrimenti eseguo un ricaricamento
122000941103      * con i valori di parzializzazione.
122100941103      *
1222009411031     *          *IN99     IFEQ *OFF
122300941103    > *                    MOVE *BLANK    W1POS
122400941103      *
1225009411032     *                    SELEC
122600941103      *
122700941103      *          $ORD      WHEQ 1
122800941103      *
122900941103    > *          C1POS     IFGT D1OBJ
123000941103    > *                    MOVELC1POS     W1POS
123100941103      *                    END
123200941103      *
123300941103      *                    MOVE *ON       *IN99
123400941103      *                    MOVE *ON       $INZS1
123500941103      *
123600941103      *
1237009411032-    *                    ENDSL
123800941103      *
1239009411031-    *                    ENDIF
124000941103      *
124100941103      *                    ENDSR
124200940131     C/EJECT
124300941103      ************************************************************
124400941103      * CONTROLLO OPZIONI LISTA
124500941103      ************************************************************
124600940131     C     CTRS1         BEGSR
124700941103      *
124800940202     C                   MOVEL     *OFF          $ESCI
124900940201     C                   SETOFF                                       99
125000940131     C                   Z-ADD     0             S1OPZ
125100941103      *
125200941103      * Leggo il sfl solo se ci sono rcd
1253009401311    C     WMAX          IFGT      0
125400940131     C                   READC     FMTS1                                  21
125500941103      *
125600941103      * esce se fine sfl o errore che richiede l'uscita
1257009401312    C     *IN21         DOWEQ     *OFF
125800940131     C     $ESCI         ANDEQ     *OFF
125900940201     C                   Z-ADD     S1NRR         C1RCD
126000941103      * ctrl su riga
126100940131     C                   EXSR      RECS1
126200941103      * gestione opzioni
1263009401313    C     S1OPZ         IFNE      0
126400940201     C     *IN99         ANDEQ     *OFF
126500940131     C                   EXSR      OPZS1
1266009401273-   C                   ENDIF
126700941125      * se rilevato errore, attivo sflnxtchg
1268009402013    C     *IN99         IFEQ      *ON
126900940131     C                   MOVE      *ON           *IN32
1270009402233-   C                   ENDIF
127100941103      *
127200940131     C                   Z-ADD     0             S1OPZ
127300941103      *
127400940127     C                   UPDATE    FMTS1
127500941125     C                   MOVE      *OFF          *IN32
127600941103      *
127700941103      * leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1278009401313    C     $ESCI         IFEQ      *OFF
127900940131     C                   READC     FMTS1                                  21
128000941103      * a fine ciclo ripristino il flag richiesta iniz.sfl
1281009402014    C     *IN21         IFEQ      *ON
128200940201     C                   MOVE      WINZS1        $INZS1
128300941103      * calcolo pagina a cui deve posizionarsi
128400940728     C     WSFL          IFGT      0
128500940728     C                   EXSR      CLCPAG
128600940728     C                   ELSE
128700940728     C                   Z-ADD     SFLPAG        WPAG
128800940728     C                   ENDIF
1289009402014-   C                   ENDIF
1290009402013-   C                   ENDIF
129100941103      *
1292009401272-   C                   ENDDO
129300941103      *
1294009401311-   C                   ENDIF
129500941103      *
129600940127     C                   ENDSR
129700940127     C/EJECT
129800941103      ************************************************************
129900941103      * CONTROLLO CAMPI I/O RIGA LISTA
130000941103      ************************************************************
130100940131     C     RECS1         BEGSR
130200941103      *
130300941103      * reset indicatori DSPATR
130400940201     C                   MOVE      *ALL'0'       IN4049           10
130500940201     C                   MOVEA     IN4049        *IN(40)
130600941103      *
130700941103      * Controllo se Utente Autorizzato alla scelta
130800940202     C     S1OPZ         IFNE      0
130900940131     C                   Z-ADD     1             X
131000940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
131001980716     C* mi salvo l'indice dell' opzione
131002980716     C   21              Z-add     X             PosOpz            2 0
131100940211     C     *IN21         IFEQ      *OFF
131200940223     C     $FL1(X)       OREQ      'N'
131300940131     C                   MOVE      *ON           $ESCI
131400940201     C                   SETON                                        504099
131500940131     C                   ENDIF
131600940202     C                   ENDIF
131700941103      *
131800940131     C                   ENDSR
131900940224     C/EJECT
132000941103      ************************************************************
132100941103      * GESTIONE OPZIONI LISTA
132200941103      ************************************************************
132300940131     C     OPZS1         BEGSR
132400941103      *
1325009402251    C                   SELECT
132600941103      *
132700941103      * se opz=Selezione passo la chiave al pgm chiamante
132800940225     C     S1OPZ         WHENEQ    1
132900941103    > * << passare nei campi appositi della DS A009DS il valore
133000941103    > *    della chiave della VL univoca (se c'è) >>
133100941012    >C                   MOVE      S1OPZ         OPZ009
133200941012    >C                   MOVE      C1SOC         SOC009
133300941215    >C                   MOVE      S1UNI         UNI009
133400940225     C                   MOVE      *ON           $ESCI
133500940225     C                   MOVE      *ON           $FINE
133600941103      *
133700940909     C                   MOVE      *HIVAL        $LASTV
133701980716     C*
133702980716     C* se selezionata opzione personalizzata, richiamo il programma
133703980716     C* corrispondente
133704980716     C     $PGO1(PosOpz) Whenne    *Blank
133705980716     C                   RESET                   A010DS
133706980716     C* << passare nei campi appositi della DS A043DS il valore
133707980716     C*    della chiave della VL univoca (se c'è) >>
133708980716     C                   MOVEL     S1OPZ         OPZ010
133709980717     C* passare nei campi appositi della DS A010DS il valore
133710980716     C* della chiave della VL univoca
133711980716    >C                   MOVEL     C1SOC         SOC010
133712980716    >C                   MOVEL     S1UNI         UNI010
133716980716     C*
133717980716     C                   MOVEL     A010DS        KPJBU
133718980716     C*
133719980716     C                   CALL      $PGO1(PosOpz)                        22
133720980716     C                   PARM                    KPJBA
133721980716     C                   PARM                    NOMPGM
133722980716     C*
133723980716     C                   MOVEL     KPJBU         A010DS
133724980716     C*
133725980716     C* ritorno da PGM
133726980716     C                   EXSR      RTN010
133727980716     C*
133728980716     C*
133800941103      *
133900941103      * se altre opzioni (o nessuna opzione) richiamo pgm gestione
1340009402251O   C                   OTHER
134100941103      *
134200941012    >C                   RESET                   A010DS
134300980717    > * << passare nei campi appositi della DS D002DS il valore
134400941103    > *    della chiave della VL univoca (se c'è) >>
134500941012    >C                   MOVEL     S1OPZ         OPZ010
134600941012    >C                   MOVEL     C1SOC         SOC010
134700941013    >C                   MOVEL     S1UNI         UNI010
134800941103      *
134900941012    >C                   MOVEL     A010DS        KPJBU
135000941012    >C                   CALL      'ANA010R'
135100940314     C                   PARM                    KPJBA
135200941012    >C                   MOVEL     KPJBU         A010DS
135300941103      *
135400941103      * ritorno da PGM gestione
135500941012    >C                   EXSR      RTN010
135600941103      * se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1357009402252    C     *IN99         IFEQ      *ON
135800940223     C                   SETON                                        40
1359009402252-   C                   ENDIF
136000941103      *
136100940506     C                   MOVE      *HIVAL        $LASTV
1362009402251-   C                   ENDSL
136300941103      *
136400940131     C                   ENDSR
136500940223     C/EJECT
136600941103      ************************************************************
136700941103      * GESTIONE RITORNO DA PGM DI GESTIONE
136800941103      ************************************************************
136900941012    >C     RTN010        BEGSR
137000941103      *
137100941103      * modo di ritorno
137200941103      *
1373009402231    C                   SELECT
137400941103    > * << questi modi di utilizzo dei valori di ritorno dal
137500941103    > *    pgm di manutenzione rcd di anagrafica sono delle
137600941103    > *    proposte, normalmente sempre valide, ma modificabili
137700941103    > *    per situazioni particolari >>
137800941103      * 1 = F3
137900941012    >C     RET010        WHENEQ    '1'
138000940224     C                   MOVE      *ON           $ESCI                          sospese READC
138100940223     C                   MOVE      *ON           $FINE                          fine pgm
138200941103      * 2 = F12
138300941012    >C     RET010        WHENEQ    '2'
138400940223     C                   MOVE      *ON           $ESCI                          sospese READC
138500941103      *
1386009402231-   C                   ENDSL
138700941103      *
138800941103      * operazione eseguite dal pgm chiamato
138900941103      *
1390009402231    C                   SELECT
139100941103      * 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
139200941012    >C     OPR010        WHENEQ    '1'
139300940223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
139400941103      *
1395009402231-   C                   ENDSL
139600941103      *
139700941103      * funzione non eseguibile per errore :
139800941103      *
1399009402231    C                   SELECT
140000941103      * 1 = funzione richiamata chiusa in errore
140100941103    > *  eventualmente gestire altri codici di errore
140200941012    >C     ERR010        WHENEQ    '1'
140300940223     C                   MOVE      *ON           $ESCI                          sospese READC
140400940223     C                   SETON                                        5299      emiss.SFLMSGID
140500941103      *
1406009402231-   C                   ENDSL
140700941103      *
140800940223     C                   ENDSR
140900940223     C/EJECT
141000941103      ************************************************************
141100941103      * GESTIONE VALORI ASSUNTI
141200941103      ************************************************************
141300940131     C     GESD2         BEGSR
141400941103      *
141500940223     C     $INZD2        IFEQ      *ON
141600940131     C                   EXSR      INZD2
141700940223     C                   MOVE      *OFF          $INZD2
141800940131     C                   END
141900941103      *
142000940202     C     $LASTV        IFNE      'D2'
142100940923     C                   EXSR      GESPIE
142200940202     C                   MOVE      $LASTV        $LASTG
142300940202     C                   MOVE      'D2'          $LASTV
142400940707     C                   ENDIF
142500941103      *
142600941103      * se esistono errori sulla videata
142700941103      * emetto la write del formato a indicatori spenti per vedere
142800941103      * le eventuali decodifiche
142900940707     C     *IN99         IFEQ      *ON
143000940707     C                   MOVEA     *IN           WIN
143100940707     C                   MOVE      *ALL'0'       IN5098           49
143200940707     C                   MOVEA     IN5098        *IN(50)
143300940707     C                   WRITE     FMTD2
143400940707     C                   MOVEA     WIN           *IN
143500940707     C                   ENDIF
143600941103      *
143700940131     C                   EXFMT     FMTD2
143800941103      *
143900940131     C                   SELECT
144000941103      * F3=Fine
144100940506     C     $TASTO        WHENEQ    F03
144200940309     C                   EXSR      F03D2
144300941103      * F12=Ritorno
144400940506     C     $TASTO        WHENEQ    F12
144500940309     C                   EXSR      F12D2
144600941103      * F24=Altri tasti
144700940916     C     $TASTO        WHENEQ    F24
144800940916     C                   EXSR      F24D2
144900941103      *
145000940131     C                   OTHER
145100940131     C                   EXSR      CTRD2
1452009401312    C     *IN99         IFEQ      *OFF
145300940211     C                   MOVE      $LASTG        $GEST
1454009401313    C                   SELECT
145500940202     C     $LASTG        WHENEQ    'S1'
145600941103      * se è stato modificato l'ordinamento di default, ricarico la lista
145700940314     C     $ORD          IFNE      D2ORD
145800940223     C                   MOVE      *ON           $INZS1
145900941103      ***********          MOVE *BLANK    W1POS
146000940131     C                   END
146100940202     C     $LASTG        WHENEQ    'D1'
146200941103      * per ora niente
1463009401313-   C                   ENDSL
146400941103      * definisco i nuovi valori di default
146500940201     C                   Z-ADD     D2LOP         $LOP
146600940131     C                   Z-ADD     D2ORD         $ORD
146700941103    > * << elenco altri dati default >>
1468009401312-   C                   END
146900941103      *
147000940131     C                   ENDSL
147100941103      *
147200940131     C                   ENDSR
147300940223     C/EJECT
147400941103      ************************************************************
147500941103      * INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
147600941103      ************************************************************
147700940201     C     INZD2         BEGSR
147800941103      *
147900940131     C                   CLEAR                   FMTD2
148000940201     C                   MOVEL     $LOP          D2LOP
148100940831     C                   MOVE      $ORD          D2ORD
148200941103    > * << elenco altri dati default >>
148300941103      *
148400940131     C                   ENDSR
148500940309     C/EJECT
148600941103      ************************************************************
148700941103      * GESTIONE F03 VIDEO D2
148800941103      ************************************************************
148900940309     C     F03D2         BEGSR
149000941103      *
149100940309     C                   MOVE      *ON           $FINE
149200941103      * fine programma
149300940325     C                   EXSR      ENDPGM
149400941103      *
149500940309     C                   ENDSR
149600940309     C/EJECT
149700941103      ************************************************************
149800941103      * GESTIONE F12 VIDEO D2
149900941103      ************************************************************
150000940309     C     F12D2         BEGSR
150100941103      *
150200940309     C                   MOVE      $LASTG        $GEST
150300941103      *
150400940309     C                   ENDSR
150500940916     C/EJECT
150600941103      ************************************************************
150700941103      * GESTIONE F24 VIDEO D2
150800941103      ************************************************************
150900940916     C     F24D2         BEGSR
151000941103      *
151100940916     C                   MOVEA     $FX3          $FX
151200940916     C                   MOVEA     $FC3          $FC
151300940916     C                   MOVEA     $FR3          $FR
151400940916     C                   Z-ADD     $ULKD2        $ULK
151500940929     C                   Z-ADD     56            $L01
151600940929     C                   Z-ADD     56            $L02
151700940916     C                   EXSR      RIEKEY
151800940916     C                   Z-ADD     $ULK          $ULKD2            3 0
151900940916     C                   MOVEA     $FC           $FC3
152000940916     C                   MOVEL     $KEY1         Z3KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
152100940916     C                   MOVEL     $KEY2         Z3KE2
152200940916     C                   WRITE     FMTZ3
152300941103      *
152400940916     C                   ENDSR
152500940223     C/EJECT
152600941103      ************************************************************
152700941103      * CONTROLLO VIDEATA VALORI ASSUNTI
152800941103      ************************************************************
152900940131     C     CTRD2         BEGSR
153000941103      *
153100940131     C                   SETOFF                                       99
153200941103      * non imposto pos. del cursore
153300940131     C                   Z-ADD     0             H1RIGA
153400940131     C                   Z-ADD     0             H1COLO
153500941103      *
153600941103      * OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
153700940201     C     D2LOP         IFEQ      0
153800940131     C                   SETON                                          5099
153900940131     C                   END
154000941103      *
154100941103      * OBBLIGATORIO VALORE PER ORDINAMENTO
154200940131     C     D2ORD         IFEQ      0
154300940131     C                   SETON                                          5199
154400940131     C                   END
154500941103    > * << elenco controlli altri dati default >>
154600941103      *
154700940131     C                   ENDSR
154800940223     C/EJECT
154900941103      ************************************************************
155000941103      * AGGIORNAMENTO VALORI ASSUNTI
155100941103      ************************************************************
155200940201     C     AGGMPE        BEGSR
155300941103      *
155400940201     C                   CLEAR                   ANMPE000
155500940201     C                   MOVEL     XSCSOC        MPESOC
155600940201     C                   MOVEL     DSPGM         MPEPGM
155700940201     C                   MOVEL     KNMUS         MPEPRF
155800941013     C                   SELECT
155900941013     C     OPZ009        WHENEQ    '01'
156000941013     C                   MOVEL     'PX'          MPETIP
156100941013     C                   OTHER
156200941013     C                   MOVEL     'PV'          MPETIP
156300941013     C                   ENDSL
156400940201     C     K4MPE2        CHAIN     ANMPE02L                           21
156500940201     C                   MOVEL     DSMPE         MPEPAR
156600940201     C     *IN21         IFEQ      *ON
156700940201     C                   WRITE     ANMPE000
156800940201     C                   ELSE
156900940201     C                   UPDATE    ANMPE000
157000940201     C                   END
157100941103      *
157200940201     C                   ENDSR
157300940223     C/EJECT
157400941103      ************************************************************
157500941103      * OPERAZIONI INIZIALI
157600941103      ************************************************************
157700940131     C     *INZSR        BEGSR
157800941103      *
157900941103      * Reperimento parametri
158000941103      *
158100940117     C     *ENTRY        PLIST
158200940117     C                   PARM                    KPJBA
158300941103      *
158400941103      * Reperimento dati società
158500941103      *
158600941103     C                   MOVEL     'SOC001'      TIPXSC
158700941103     C                   MOVEL     *BLANK        SOCXSC
158800941103     C                   EXSR      REPSOC
158900941103     C     RTNXSC        IFNE      '1'
159000941103     C                   MOVEL     XSOCDS        SOC001
159100941103     C                   ELSE
159200941103     C                   MOVE      *ON           $FINE
159300941103     C                   EXSR      ENDPGM
159400941103     C                   ENDIF
159500941103      * Reperimento autorizzazioni
159600941103      *
159700941103    > * << nel caso l'anagrafica non gestisse le autorizzazioni
159800941103    > *    asteriscare la chiamata alla subroutine REPAUT >>
159900941103    > **************       EXSR REPAUT
160000941103      *
160100941103      * Reperimento opzioni
160200941103      *
160300940128     C                   EXSR      REPOPZ
160400941103      *
160500941103      * Reperimento tasti di funzione
160600941103      *
160700940506     C                   EXSR      REPKEY
160701980721     C* Chiamata a routine per il reperimento delle opzioni
160702980721     C* personalizzate
160705980721     C                   CAllb     'X46USREXT'
160706980721     C                   PARM                    $OP1
160707980721     C                   PARM                    $TX1
160708980721     C                   PARM                    $FL1
160709980721     C                   PARM                    $PGO1
160710980721     C                   PARM      $dimOp        NumOpz
160711980721     C                   PARM                    $FX2
160712980721     C                   PARM                    $FC2
160713980721     C                   PARM                    $PGF2
160714980721     C                   PARM                    DsPgm
160715980721     C                   PARM                    xsccut
160716980721     C                   PARM                    xscgrp
160717980721     C*
160718980721     C                   Z-add     1             OPZLEN
160719980721     C     *blank        Lookup    $TX1(opzlen)                           21
160720980721     C                   if        *in21 = *on
160721980721     C                   sub       1             OPZLEN
160722980721     C                   else
160723980721     C                   Z-add     $DimOp        OPZLEN
160724980721     C                   endif
160725980721     C*
160800941103      *
160900941103      *
161000940117     C                   ENDSR
161100940225     C/EJECT
161200941103      ************************************************************
161300941103      * REPERIMENTO AUTORIZZAZIONI UTENTE
161400941103      ************************************************************
161500941103      *          REPAUT    BEGSR
161600941103      *
161700941103      *                    CLEARXAUT
161800941103      *
161900941103      *                    Z-ADD0         XCAGRP           GRUPPO
162000941103      *                    MOVELKNMUS     XCAPRF           PROFILO
162100941103    > *                    MOVEL'OBJALL'  XCAFNC           FUNZIONE
162200941103      *                    MOVEL*ZERO     XCAVFU           VARIABILE
162300941103      *                    MOVE 'CK'      XCATCK           TIPO CTRL
162400941103      *
162500941103      *                    CALL 'XCHKAUT'
162600941103      *                    PARM           XAUT
162700941103      * utente non abilitato
162800941103      *          XCARTN    IFEQ 0
162900941103    > *                    MOVELXCADDS    AUTDS
163000941103      *                    ELSE
163100941103    > *                    CLEARAUTDS
163200941103      *                    END
163300941103      *
163400941103      *                    ENDSR
163500940223     C/EJECT
163600941103      ************************************************************
163700941103      * INIZIALIZZAZIONE VARIABILI
163800941103      ************************************************************
163900940223     C     INZVAR        BEGSR
164000941103      *
164100941103      *
164200941103      * Dati da PGM chiamante
164300941103      *
164400941012    >C                   MOVEL     KPJBU         A009DS
164401980721     C* Se società diversa da quella impostata nella *inzsr, rieseguo
164402980721     C* reperimento dati società
164403980721    >C                   IF        Soc009 <> xscsoc  and
164404980721    >C                             Opz009 <> *blank
164405980721     C                   MOVEL     'SOC001'      TIPXSC
164406980721    >C                   MOVEL     Soc009        SOCXSC
164407980721     C                   EXSR      REPSOC
164408980721     C     RTNXSC        IFNE      '1'
164409980721     C                   MOVEL     XSOCDS        SOC001
164410980721     C                   ELSE
164411980721     C                   MOVE      *ON           $FINE
164412980721    >C                   MOVE      '1'           Err009
164413980721     C                   EXSR      ENDPGM
164414980721     C                   ENDIF
164415980721     C                   ENDIF
164500941103      *
164600941103      * Reperimento valori default utente
164700941103      *
164800941026     C                   EXSR      REPDFT
164900941103      *
165000941103      * Definizione delle opzioni utilizzabili a pgm
165100941103      *
165200940506     C                   EXSR      DEFOPZ
165300941103      * Variabili per gestione videate
165400941103      *
165500940223     C                   MOVE      *OFF          $FINE                          fine pgm
165600940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
165700940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
165800940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
165900940223     C                   MOVE      *OFF          $EFILE                         memo fine file
166000940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
166100940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
166200940509     C                   MOVE      *HIVAL        $LASTV
166300940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
166400940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
166500940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
166600941103      *
166700940318     C                   MOVE      *ON           $INZD1
166800940318     C                   MOVE      *ON           $INZS1
166900941103      *
167000941103      *
167100941103      *
167200940223     C                   SELECT
167300940223     C     $LOP          WHENEQ    1
167400940223     C                   MOVE      'D1'          $GEST
167500940223     C     $LOP          WHENEQ    2
167600940223     C                   MOVE      'S1'          $GEST
167700941104     C                   EXSR      INZD1
167800940223     C                   ENDSL
167900941103      *
168000941103      * Variabili appoggio
168100941103      *
168200940223     C                   Z-ADD     SFLPAG        WPAG
168300941103      *
168400941103      * Valorizzazione campi univoci testate
168500941103      *
168600940223     C                   CLEAR                   FMTT1
168700940223     C                   MOVEL     KNSIF         NOMSIF
168800941110     C                   MOVEL     XSCDSI        NOMDIT
168900941028     C                   MOVEL     DSPGM         NOMPGM
169000980716     C*
169100940223     C                   ENDSR
169200940223     C/EJECT
169300941103      ************************************************************
169400941103      * REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
169500941103      ************************************************************
169600940131     C     REPDFT        BEGSR
169700941103      *
169800940201     C                   MOVEL     XSCSOC        MPESOC
169900940201     C                   MOVEL     DSPGM         MPEPGM
170000940131     C                   MOVEL     KNMUS         MPEPRF
170100941013     C                   SELECT
170200941013     C     OPZ009        WHENEQ    '01'
170300941013     C                   MOVEL     'PX'          MPETIP
170400941013     C                   OTHER
170500941013     C                   MOVEL     'PV'          MPETIP
170600941013     C                   ENDSL
170700940202     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
170800940131     C     *IN21         IFEQ      *OFF
170900940131     C                   MOVEL     MPEPAR        DSMPE
171000941103      * nel caso sia la prima volta che l'utente apre questa anagrafica
171100941103      * i suoi valori di default vengono decisi a pgm
171200940131     C                   ELSE
171300941115     C                   CLEAR                   DSMPE
171400940201     C                   Z-ADD     1             $LOP
171500940131     C                   Z-ADD     1             $ORD
171600941103    > * << elenco altri dati default >>
171700940131     C                   END
171800941103      *
171900940131     C                   ENDSR
172000940223     C/EJECT
172100941103      ************************************************************
172200941103      * DEFINIZIONE OPZIONI UTILIZZABILI A PGM
172300941103      ************************************************************
172400940225     C     DEFOPZ        BEGSR
172500941103      *
172600941103      * Questa routine deve valorizzare la schiera $FL1
172700941103      * con S o N a seconda della utilizzabilità o meno
172800941103      * dell'opzione correlata
172900941103      *
173000941103      * Inizialmente si considerano tutte le opzioni come valide
173100940317     C     1             DO        $DIMOP        X
173200940225     C                   MOVE      'S'           $FL1(X)
173300940225     C                   END
173400941103      *
173500941103      * Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
173600941103      * con S o N a seconda della utilizzabilità o meno
173700941103      * del tasto funzione correlato
173800941103      *
173900941103      * Inizialmente ripristino le abilitazioni come da schiera a tempo
174000941103      * di compilazione
174100940715     C                   RESET                   $FC1
174200940715     C                   RESET                   $FR1
174300940715     C                   RESET                   $FC2
174400940715     C                   RESET                   $FR2
174500940919     C                   RESET                   $FC3
174600940919     C                   RESET                   $FR3
174700941103      *
174800941103      * Dopodichè, a seconda del richiamo e delle autorizzazioni
174900941103      * invalido alcune opzioni
175000940225     C                   SELECT
175100941103      * Se pgm richiamato in ricerca disattivo :
175200941012    >C     OPZ009        WHENEQ    '01'
175300941103      * F10=Immissione
175400940506     C                   Z-ADD     10            X
175500940720     C                   MOVE      '0'           $FC2(X)
175600941103      * F3=Fine
175700940603     C                   Z-ADD     3             X
175800940720     C                   MOVE      '0'           $FC2(X)
175900941103      * 2=Modifica
176000940307     C                   Z-ADD     1             X
176100940225     C     02            LOOKUP    $OP1(X)                                21
176200940225     C     *IN21         IFEQ      *ON
176300940225     C                   MOVE      'N'           $FL1(X)
176400940225     C                   ENDIF
176500941103      * 3=Copia
176600940225     C                   Z-ADD     1             X
176700940225     C     03            LOOKUP    $OP1(X)                                21
176800940225     C     *IN21         IFEQ      *ON
176900940225     C                   MOVE      'N'           $FL1(X)
177000940225     C                   ENDIF
177100941103      * 4=Annullamento
177200940225     C                   Z-ADD     1             X
177300940225     C     04            LOOKUP    $OP1(X)                                21
177400940225     C     *IN21         IFEQ      *ON
177500940225     C                   MOVE      'N'           $FL1(X)
177600940225     C                   ENDIF
177700941103    > * << aggiungere nollo stesso modo le altre opzioni da
177800941103    > *    disattivare (quelle specifiche per ogni pgm) >>
177900941103      * Se pgm richiamato in interrogazione disattivo :
178000941012    >C     OPZ009        WHENEQ    '05'
178100941103      * F10=Immissione
178200940615     C                   Z-ADD     10            X
178300940720     C                   MOVE      '0'           $FC2(X)
178400941103      * 1=Selezione
178500940225     C                   Z-ADD     1             X
178600940225     C     01            LOOKUP    $OP1(X)                                21
178700940225     C     *IN21         IFEQ      *ON
178800940225     C                   MOVE      'N'           $FL1(X)
178900940225     C                   ENDIF
179000941103      * 2=Modifica
179100940225     C                   Z-ADD     1             X
179200940225     C     02            LOOKUP    $OP1(X)                                21
179300940225     C     *IN21         IFEQ      *ON
179400940225     C                   MOVE      'N'           $FL1(X)
179500940225     C                   ENDIF
179600941103      * 3=Copia
179700940225     C                   Z-ADD     1             X
179800940225     C     03            LOOKUP    $OP1(X)                                21
179900940225     C     *IN21         IFEQ      *ON
180000940225     C                   MOVE      'N'           $FL1(X)
180100940225     C                   ENDIF
180200941103      * 4=Annullamento
180300940225     C                   Z-ADD     1             X
180400940225     C     04            LOOKUP    $OP1(X)                                21
180500940225     C     *IN21         IFEQ      *ON
180600940225     C                   MOVE      'N'           $FL1(X)
180700940225     C                   ENDIF
180800941103      *
180900940225     C                   OTHER
181000941103      * Se pgm non richiamato, disattivo subito :
181100941103      * 1=Selezione
181200940225     C                   Z-ADD     1             X
181300940225     C     01            LOOKUP    $OP1(X)                                21
181400940225     C     *IN21         IFEQ      *ON
181500940225     C                   MOVE      'N'           $FL1(X)
181600940225     C                   ENDIF
181700941103      * poi, disattivo a seconda delle autorizzazioni :
181800941103    > *********  AUTXXX    IFEQ 'N'
181900941103    > *********            Z-ADD1         X
182000941103    > *********  'XX'      LOKUP$OP1,X                   21
182100941103    > *********  *IN21     IFEQ *ON
182200941103    > *********            MOVE 'N'       $FL1,X
182300941103    > *********            ENDIF
182400941103    > *********            ENDIF
182500941103      *
182600940225     C                   ENDSL
182700941103      *
182800940225     C                   ENDSR
182900940225     C/EJECT
183000941103      ************************************************************
183100941103      * REPERIMENTO OPZIONI
183200941103      ************************************************************
183300940225     C     REPOPZ        BEGSR
183400941103      *
183500940314     C     1             DO        $DIMOP        X
183600941103      *
183700940223     C     $MS1(X)       IFNE      *BLANK
183800941103      *
183900940223     C                   MOVEL     $MS1(X)       IDMSG
184000940128     C                   CALL      'XRTVM'
184100940201     C                   PARM                    IDMSG            27
184200940201     C                   PARM                    TXTMSG          100
184300940201     C                   PARM                    ERRMSG            1
184400941103      *
184500940128     C     ERRMSG        IFNE      *ON
184600940223     C                   MOVEL     TXTMSG        $TX1(X)
184700940128     C                   ELSE
184800940223     C                   MOVEL     *ALL'?'       $TX1(X)
184900940128     C                   END
185000941103      *
185100940128     C                   END
185200941103      *
185300940128     C                   END
185400941103      *
185500940128     C                   ENDSR
185600940506     C/EJECT
185700941103      ************************************************************
185800941103      * REPERIMENTO TASTI DI FUNZIONE
185900941103      ************************************************************
186000940506     C     REPKEY        BEGSR
186100940506     C                   MOVEA     $FM1          $FM
186200950906     C                   MOVEA     $FC1          $FC
186300940506     C                   EXSR      REPKKK
186400940506     C                   MOVEA     $FX           $FX1
186500941103      *
186600940506     C                   MOVEA     $FM2          $FM
186700950906     C                   MOVEA     $FC2          $FC
186800940506     C                   EXSR      REPKKK
186900940506     C                   MOVEA     $FX           $FX2
187000941103      *
187100940919     C                   MOVEA     $FM3          $FM
187200950906     C                   MOVEA     $FC3          $FC
187300940919     C                   EXSR      REPKKK
187400940919     C                   MOVEA     $FX           $FX3
187500940919     C                   ENDSR
187600940506     C/EJECT
187700941103      ************************************************************
187800941103      * REPERIMENTO TASTI DI FUNZIONE
187900941103      ************************************************************
188000940506     C     REPKKK        BEGSR
188100941103      *
188200940506     C                   CLEAR                   $FX
188300940506     C     1             DO        24            X
188400941103      *
188500940506     C     $FM(X)        IFNE      *BLANK
188600950906     C     $FC(X)        ANDNE     '0'
188700941103      *
188800940506     C                   MOVEL     $FM(X)        IDMSG
188900940506     C                   CALL      'XRTVM'
189000940506     C                   PARM                    IDMSG            27
189100940506     C                   PARM                    TXTMSG          100
189200940506     C                   PARM                    ERRMSG            1
189300941103      *
189400940506     C     ERRMSG        IFNE      *ON
189500940506     C                   MOVEL     TXTMSG        $FX(X)
189600940506     C                   ELSE
189700940506     C                   MOVEL     *ALL' '       $FX(X)
189800940506     C                   END
189900941103      *
190000940506     C                   END
190100941103      *
190200940506     C                   END
190300941103      *
190400940506     C                   ENDSR
190500940223     C/EJECT
190600941103      ************************************************************
190700941103      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
190800941103      ************************************************************
190900940223     C     DEFCAM        BEGSR
191000941103      *
191100941103      * klist
191200941103      *
191300941103    > * ANSIF01L
191400941013    >C     K1SIF1        KLIST
191500941013    >C                   KFLD                    SIFSOC
191600941103      *
191700941103    > * ANUNI01L
191800941012    >C     K2UNI1        KLIST
191900941012    >C                   KFLD                    UNISOC
192000941013    >C                   KFLD                    UNIUNI
192100941103      *
192200941103      * ANMPE02L
192300940128     C     K4MPE2        KLIST
192400940128     C                   KFLD                    MPESOC
192500940201     C                   KFLD                    MPEPGM
192600940128     C                   KFLD                    MPETIP
192700940128     C                   KFLD                    MPEPRF
192800941103      *
192900941103      * Variabili per gestione videate
193000941103      *
193100940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
193200940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
193300940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
193400940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
193500940223     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
193600940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
193700940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
193800940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
193900940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
194000940223     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
194100940224     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
194200940224     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
194300941103      *
194400941103      * Variabili appoggio sempre presenti in tutte le anagrafiche
194500941103      *
194600940223     C     *LIKE         DEFINE    C1RCD         S1NRR
194700940223     C     *LIKE         DEFINE    C1NRR         WSFL
194800940223     C     *LIKE         DEFINE    C1RCD         WMAX
194900940223     C     *LIKE         DEFINE    C1RCD         WPAG
195000940223     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
195100940608     C     *LIKE         DEFINE    *INKR         WINKR                                      ne S1
195200940223     C                   MOVE      *BLANK        WWGRP             3
195300940223     C                   Z-ADD     0             CURR              2 0
195400940223     C                   Z-ADD     0             CURC              2 0
195500940223     C                   MOVE      *BLANK        WIN              99
195600941103      ************LIKE     DEFN C1POS     W1POS
195700941103      *
195800941103      * Variabili appoggio specifiche della singola anagrafica
195900941103      *
196000941103      * campi per gestione F4=Ricerca
196100941103    > ***        *LIKE     DEFN ANAxxx    WWxxx
196200941103      * campi per parzializzazioni su rcd (solo alfanumerici)
196300941103      *
196400941103      * Indici
196500941103      *
196600940223     C                   Z-ADD     0             X                 3 0
196700940223     C                   Z-ADD     0             Y                 3 0
196800941103      *
196900940117     C                   ENDSR
197000940223     C/EJECT
197100941103      ************************************************************
197200941103      * Reperimento dati società
197300941103      ************************************************************
197400941103     C     REPSOC        BEGSR
197500941103      *
197600941103     C                   CALL      'XSOC'
197700941103     C                   PARM                    TIPXSC            6
197800941103     C                   PARM                    SOCXSC            3
197900941103     C                   PARM                    CDSXSC            9 0
198000941103     C                   PARM                    MODXSC            3
198100941103     C                   PARM      *BLANKS       RTNXSC            1
198200941103     C                   PARM                    XSOCDS
198300941103     C                   PARM                    KPJBA
198400941103      *
198500941103     C                   ENDSR
198600950906**
198700941012PROMSG    *LIBL     OPZ0001
198800941012PROMSG    *LIBL     OPZ0002
198900940131PROMSG    *LIBL     OPZ0003
199000940131PROMSG    *LIBL     OPZ0004
199100940131PROMSG    *LIBL     OPZ0005
199200940201**
19930094022501S      <Selezione
19940094022502S      <Modifica
19950094022503S      <Copia
19960094022504S      <Annullamento
19970094022505S      <Visualizzazione
199800940506** TASTI DI FUNZIONE  D1
199900940506PROMSG    *LIBL     KEY0001  01
200000940506PROMSG    *LIBL     KEY0002  02
200100940506PROMSG    *LIBL     KEY0003  03
200200940506PROMSG    *LIBL     KEY0004  04
200300940506PROMSG    *LIBL     KEY0005  05
200400940506PROMSG    *LIBL     KEY0006  06
200500940506PROMSG    *LIBL     KEY0007  07
200600940506PROMSG    *LIBL     KEY0008  08
200700940506PROMSG    *LIBL     KEY0009  09
200800940506PROMSG    *LIBL     KEY0010  10
200900940506PROMSG    *LIBL     KEY0011  11
201000940506PROMSG    *LIBL     KEY0012  12
201100940506PROMSG    *LIBL     KEY0013  13
201200940506PROMSG    *LIBL     KEY0014  14
201300940506PROMSG    *LIBL     KEY0015  15
201400940506PROMSG    *LIBL     KEY0016  16
201500940506PROMSG    *LIBL     KEY0017  17
201600940506PROMSG    *LIBL     KEY0018  18
201700940506PROMSG    *LIBL     KEY0019  19
201800940506PROMSG    *LIBL     KEY0020  20
201900940506PROMSG    *LIBL     KEY0021  21
202000940506PROMSG    *LIBL     KEY0022  22
202100940506PROMSG    *LIBL     KEY0023  23
202200940506PROMSG    *LIBL     KEY0024  24
202300940506**  ABILITAZIONE D1
2024009407200N  01
2025009407200N  02
2026009407201N  03
2027009407201N  04
2028009407200N  05
2029009407200N  06
2030009407200N  07
2031009407200N  08
2032009407200N  09
2033009407200N  10
2034009407200N  11
2035009407201N  12
2036009407200N  13
2037009407200N  14
2038009407200N  15
2039009407200N  16
2040009407200N  17
2041009407201N  18
2042009407200N  19
2043009407200N  20
2044009407200N  21
2045009407200N  22
2046009509060N  23   Impostare a '1' se si utilizzano opzioni
2047009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
204800940916** TASTI DI FUNZIONE  S1
204900940916PROMSG    *LIBL     KEY0001  01
205000940916PROMSG    *LIBL     KEY0002  02
205100940916PROMSG    *LIBL     KEY0003  03
205200940916PROMSG    *LIBL     KEY0004  04
205300940916PROMSG    *LIBL     KEY0005  05
205400940916PROMSG    *LIBL     KEY0006  06
205500940916PROMSG    *LIBL     KEY0007  07
205600940916PROMSG    *LIBL     KEY0008  08
205700940916PROMSG    *LIBL     KEY0009  09
205800940916PROMSG    *LIBL     KEY0010  10
205900940916PROMSG    *LIBL     KEY0011  11
206000940916PROMSG    *LIBL     KEY0012  12
206100940916PROMSG    *LIBL     KEY0013  13
206200940916PROMSG    *LIBL     KEY0014  14
206300940916PROMSG    *LIBL     KEY0015  15
206400940916PROMSG    *LIBL     KEY0016  16
206500940916PROMSG    *LIBL     KEY0017  17
206600940916PROMSG    *LIBL     KEY0018  18
206700940916PROMSG    *LIBL     KEY0019  19
206800940916PROMSG    *LIBL     KEY0020  20
206900940916PROMSG    *LIBL     KEY0021  21
207000940916PROMSG    *LIBL     KEY0022  22
207100940916PROMSG    *LIBL     KEY0023  23
207200940916PROMSG    *LIBL     KEY0024  24
207300940916**  ABILITAZIONE S1
2074009409160N  01
2075009409160N  02
2076009409161N  03
2077009409160N  04
2078009409160N  05
2079009409160N  06
2080009409160N  07
2081009409160N  08
2082009409160N  09
2083009409161N  10
2084009409160N  11
2085009409161N  12
2086009409160N  13
2087009409160N  14
2088009409160N  15
2089009409160N  16
2090009409161N  17
2091009409161N  18
2092009409160N  19
2093009409160N  20
2094009409160N  21
2095009409160N  22
2096009509061N  23   Impostare a '1' se si utilizzano opzioni
2097009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
209800940916** TASTI DI FUNZIONE  D3
209900940506PROMSG    *LIBL     KEY0001  01
210000940506PROMSG    *LIBL     KEY0002  02
210100940506PROMSG    *LIBL     KEY0003  03
210200940506PROMSG    *LIBL     KEY0004  04
210300940506PROMSG    *LIBL     KEY0005  05
210400940506PROMSG    *LIBL     KEY0006  06
210500940506PROMSG    *LIBL     KEY0007  07
210600940506PROMSG    *LIBL     KEY0008  08
210700940506PROMSG    *LIBL     KEY0009  09
210800940506PROMSG    *LIBL     KEY0010  10
210900940506PROMSG    *LIBL     KEY0011  11
211000940506PROMSG    *LIBL     KEY0012  12
211100940506PROMSG    *LIBL     KEY0013  13
211200940506PROMSG    *LIBL     KEY0014  14
211300940506PROMSG    *LIBL     KEY0015  15
211400940506PROMSG    *LIBL     KEY0016  16
211500940506PROMSG    *LIBL     KEY0017  17
211600940506PROMSG    *LIBL     KEY0018  18
211700940506PROMSG    *LIBL     KEY0019  19
211800940506PROMSG    *LIBL     KEY0020  20
211900940506PROMSG    *LIBL     KEY0021  21
212000940506PROMSG    *LIBL     KEY0022  22
212100940506PROMSG    *LIBL     KEY0023  23
212200940506PROMSG    *LIBL     KEY0024  24
212300940916**  ABILITAZIONE D3
2124009407200N  01
2125009407200N  02
2126009407201N  03
2127009407200N  04
2128009407200N  05
2129009407200N  06
2130009407200N  07
2131009407200N  08
2132009407200N  09
2133009409160N  10
2134009407200N  11
2135009407201N  12
2136009407200N  13
2137009407200N  14
2138009407200N  15
2139009407200N  16
2140009409160N  17
2141009409160N  18
2142009407200N  19
2143009407200N  20
2144009407200N  21
2145009407200N  22
2146009509060N  23   Impostare a '1' se si utilizzano opzioni
2147009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
