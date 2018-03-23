000100100430     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PKXBND) ACTGRP(QILE)
000200960930     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000300060712B0883X* Eliminata bnddir PKXBND da parms compilazione
000400941212     H DECEDIT('0,') DATEDIT(*DMY.)
000500940127      *----------*
000600941206      * ANA008R  *
000700940127      *----------*
000800940117      *
000900941206      *            GESTIONE ANAGRAFICA OGGETTI
001000940223      *
001100940223      *
001200940223      * DOCUMENTI MICROANALISI :
001300940223      *
001400940223      * NOTE TECNICHE :
001500940223      *
001600940223      *
001700940223      * PERSONALIZZAZIONI
001800940223      *
001900940223      *
002000940223      *
002100941012      *               MAPPA INDICATORI STD
002200940223      *
002300941212      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002400941212      *  04           CANCELLAZIONE (si puo' modif. solo data validità)
002500941212      *               USATO ANCHE IN VARIAZIONE SE DATA FINE VALIDITA'
002600941212      *               MINORE DI UDATE.
002700940223      *  21           GENERICO OPERAZIONI I/O
002800940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002900971030      *  23           GENERICO QCMDEXC
003000940223      *  30           SFLDSP
003100940223      * N31           SFLCLR
003200940128      *  31           SFLDSPCTL
003300940128      *  32           SFLNXTCHG
003400940128      *  33           SFLEND
003500940128      *  39           OF PRTF
003600940315      *  40 <---> 49  DSPATR ERRORI SU SFL
003700941206      *  Specificare l'uso dei singoli indicatori
003800941213      *  50 <---> 98  ERRORI SU VIDEO
003900941206      *  Specificare l'uso dei singoli indicatori
004000980710      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
004100940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
004200940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
004300940128      *  99           INDIC. GENERALE DI ERRORE
004400941025      *----------------------------------------------------*
004500941206     FANSIF01L  UF A E           K DISK
004600941213     F                                     RENAME(ANSIF000:ANSIF01)
004700971030     FKFSIF01L  IF   E           K DISK    USROPN
004800941206     FANA008D   CF   E             WORKSTN
004900941206     F                                     RENAME(A008D1:FMTD1)
005000941206     F                                     RENAME(A008T1:FMTT1)
005100941012     F                                     RENAME(A008Z1:FMTZ1)
005200941025     F* eventuali schermi supplementari
005300941206     F                                     RENAME(A008D2:FMTD2)
005400941206     F                                     RENAME(A008D3:FMTD3)
005500941206     F                                     RENAME(A008D4:FMTD4)
005600001004B0273F                                     RENAME(A008D5:FMTD5)
005700940201     F                                     INFDS(DSFMT)
005800950615      *----------------------------------------------------*
005900980916      * Prototipo per le procedura Euro
006000981103     D/COPY *LIBL/SRCCPY,PJX002PR
006100980916      *----------------------------------------------------*
006200950426     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
006300950426     D $ID             S             27    DIM(5) ALT($OZ)
006400950615      * Schiere per la gestione dei tasti di funzione
006500940506     D $FM             S             27    DIM(24)
006600940506     D $FX             S             50    DIM(24)
006700940506     D $FC             S              1    DIM(24)
006800940506     D $FR             S              1    DIM(24)
006900950615      * Video D1
007000940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
007100940506     D $FX1            S             50    DIM(24) ALT($FM1)
007200940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
007300940506     D $FR1            S              1    DIM(24) ALT($FC1)
007400971030      * Schiera per Qcmdexc
007500971030     D $QCM            S             30    DIM(2) CTDATA PERRCD(1)
007600941213      * Parametri per ricerca codice ANATB00F
007700941213     D XTABDS          DS                  INZ
007800941213      * '  ' = Gestione
007900941213      * '01' = Ricerca
008000941213      * '02' = Gestione
008100941213      * '05' = Visualizzazione
008200941213      * '11' = Abilitazione
008300941213     D  OPZTAB                 1      2
008400941213      * 0 = ritorno normale
008500941213      * 1 = F3
008600941213      * 2 = F12
008700941213     D  RETTAB                 3      3
008800941213      * operazione eseguite dal pgm chiamato :
008900941213      * 0 = nessuna operazione
009000941213      * 1 = eseguito aggiornamento
009100941213     D  OPRTAB                 4      4
009200941213      * funzione non eseguibile per errore :
009300941213      * 0 = nessun errore
009400941213      * 1 = funzione richiamata chiusa in errore
009500941213      * Aggiungere eventuali altri codici di errore
009600941213     D  ERRTAB                 5      5
009700941213      * Azienda
009800941213     D  AZITAB                 6      8
009900941213      * Lingua
010000941213     D  LINTAB                 9      9
010100941213      * Codice scelto
010200941213     D  KEYTAB                10     24
010300941213      * riga cursore chiamante
010400941213     D  RIGTAB                25     26  0
010500941213      * colonna cursore chiamante
010600941213     D  COLTAB                27     28  0
010700941213     D ERRORE          C                   CONST('Codice errato')
010800941213     D DATA            C                   CONST('0001-01-01')
010900950615      *-------------
011000950615      * Passaggio Parametri
011100940127     D KPJBA         E DS
011200950615      *-------------
011300950615      * DS Esterna gestione messaggi per COMIT
011400940318     D XMSGDS        E DS
011500940318     D  STS          E                     EXTFLD(MSGSTS)
011600940721     D                                     DIM(333)
011700940318     D  JBA          E                     EXTFLD(MSGJBA)
011800940721     D                                     DIM(502)
011900980811     D*-------------
012000980811     D* Schiera per XMSGR
012100980811     D $COS            S             27    DIM(1) CTDATA PERRCD(1)
012200940203     D*-------------
012300941012      * Passaggio Parametri al PGM XATB
012400941012     D XATBDS        E DS                  INZ
012500950615      * Passaggio Parametri al PGM di manutenzione
012600950616     D A008DS        E DS                  EXTNAME(ANA008DS)
012700950615      * Passaggio Parametri al PGM di ricerca
012800950616     D A015DS        E DS                  EXTNAME(ANA015DS)
012900950322      *DS per passaggio dati pgm User Exit
013000950322     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
013100950616     D G12DS         E DS                  EXTNAME(ANGG12DS)
013200950616     D G15DS         E DS                  EXTNAME(ANGG15DS)
013300950616     D G16DS         E DS                  EXTNAME(ANGG16DS)
013400950616     D C17DS         E DS                  EXTNAME(ANGC17DS)
013500950616      *-----------------------------------------------------------
013600950616      * inserire tutte le altre DS di passaggio parametri
013700950616      *-----------------------------------------------------------
013800940201     D DSFMT           DS
013900940506     D  $TASTO               369    369
014000940201     D  NRG                  370    370
014100940201     D  NCL                  371    371
014200941212      *
014300941212     D                 DS           512
014400941212     D  GIORNO                 1      2  0
014500941212     D  MESE                   3      4  0
014600941212     D  ANNO                   5      6  0
014700941212     D  DGMA06                 1      6  0
014800950615      *-------------
014900950615      * posizione cursore
015000940201     D CURSOR          DS
015100940223     D  RIRI                   1      2B 0 INZ
015200940201     D  R$$                    2      2
015300940223     D  COCO                   3      4B 0 INZ
015400940201     D  C$$                    4      4
015500950615      *-------------
015600950615      * Reperimento nome PGM
015700940207     D STATUS         SDS           333
015800940207     D  DSPGM            *PROC
015900950615      *-------------
016000950615      * Costanti
016100950615      *-------------
016200950615      * Tasti di funzione
016300940506     D F01             C                   CONST(X'31')
016400940506     D F02             C                   CONST(X'32')
016500940506     D F03             C                   CONST(X'33')
016600940506     D F04             C                   CONST(X'34')
016700940506     D F05             C                   CONST(X'35')
016800940506     D F06             C                   CONST(X'36')
016900940506     D F07             C                   CONST(X'37')
017000940506     D F08             C                   CONST(X'38')
017100940506     D F09             C                   CONST(X'39')
017200940506     D F10             C                   CONST(X'3A')
017300940506     D F11             C                   CONST(X'3B')
017400940506     D F12             C                   CONST(X'3C')
017500940506     D F13             C                   CONST(X'B1')
017600940506     D F14             C                   CONST(X'B2')
017700940506     D F15             C                   CONST(X'B3')
017800940506     D F16             C                   CONST(X'B4')
017900940506     D F17             C                   CONST(X'B5')
018000940506     D F18             C                   CONST(X'B6')
018100940506     D F19             C                   CONST(X'B7')
018200940506     D F20             C                   CONST(X'B8')
018300940506     D F21             C                   CONST(X'B9')
018400940506     D F22             C                   CONST(X'BA')
018500940506     D F23             C                   CONST(X'BB')
018600940506     D F24             C                   CONST(X'BC')
018700940506     D ENTER           C                   CONST(X'F1')
018800940506     D ROLDWN          C                   CONST(X'F4')
018900940506     D ROLLUP          C                   CONST(X'F5')
019000970102     D*-------------
019100970102     D* Sottotitolo di testata
019200950616     D SOTTIT          C                   CONST('                    ')
019300970102     D*------------------------------------------------------
019400970102     D* DATMIN e DATMAX
019500970102     D*
019600941212     D DATMIN          C                   CONST('0001-01-01')
019700941212     D DATMAX          C                   CONST('9999-12-31')
019800970102     D*------------------------------------------------------
019900970102     D* Dati di ambiente ottenuti da XSOC
020000941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
020100970102     D*-----------
020200970102     D* DS Interna per dati di output di XSOC
020300941103     D XSOCDS          DS          1000
020400970103     D*-----------
020500970102     D*Struttura procedure gestite
020600941206     D SIFSKP          DS            50
020700941206     D  SKP01                  1      1
020800941206     D  SKP02                  2      2
020900941206     D  SKP03                  3      3
021000941206     D  SKP04                  4      4
021100941206     D  SKP05                  5      5
021200941206     D  SKP06                  6      6
021300941206     D  SKP07                  7      7
021400941206     D  SKP08                  8      8
021500941206     D  SKP09                  9      9
021600941206     D  SKP10                 10     10
021700941206     D  SKP11                 11     11
021800941206     D  SKP12                 12     12
021900941206     D  SKP13                 13     13
022000941206     D  SKP14                 14     14
022100941206     D  SKP15                 15     15
022200941206     D  SKP16                 16     16
022300941206     D  SKP17                 17     17
022400941206     D  SKP18                 18     18
022500941206     D  SKP19                 19     19
022600941206     D  SKP20                 20     20
022700970102     D*Flag operativi
022800950615     D                 DS
022900941206     D  OPE01                  1      1
023000941206     D  OPE02                  2      2
023100941206     D  OPE03                  3      3
023200941206     D  OPE04                  4      4
023300941206     D  OPE05                  5      5
023400941206     D  OPE06                  6      6
023500941206     D  OPE07                  7      7
023600941206     D  OPE08                  8      8
023700941206     D  OPE09                  9      9
023800941206     D  OPE10                 10     10
023900941206     D  OPE11                 11     11
024000941206     D  OPE12                 12     12
024100941206     D  OPE13                 13     13
024200941206     D  OPE14                 14     14
024300941206     D  OPE15                 15     15
024400001004B0273D  OPE16                 16     16
024500001004  "  D  OPE17                 17     17
024600001004  "  D  OPE18                 18     18
024700001004  "  D  OPE19                 19     19
024800001004  "  D  OPE20                 20     20
024900001004  "  D  OPE21                 21     21
025000001004  "  D  OPE22                 22     22
025100001004  "  D  OPE23                 23     23
025200001004  "  D  OPE24                 24     24
025300001004  "  D  OPE25                 25     25
025400001004  "  D  OPE26                 26     26
025500001004  "  D  OPE27                 27     27
025600001004  "  D  OPE28                 28     28
025700001004  "  D  OPE29                 29     29
025800001004  "  D  OPE30                 30     30
025900001004  "  D  SIFOPE                 1     30
026000001004B0273X*****  SIFOPE                 1     15
026100100505R002 D  SIFSOGG               31     38
026200950615     D  SIFFLG                 1     50
026300950615      *Flag operativi a video
026400950615     D D4FODS          DS            50
026500950615     D  D4FO01                 1      1
026600950615     D  D4FO02                 2      2
026700950615     D  D4FO03                 3      3
026800950615     D  D4FO04                 4      4
026900950615     D  D4FO05                 5      5
027000950615     D  D4FO06                 6      6
027100950615     D  D4FO07                 7      7
027200950615     D  D4FO08                 8      8
027300950615     D  D4FO09                 9      9
027400950615     D  D4FO10                10     10
027500950615     D  D4FO11                11     11
027600950615     D  D4FO12                12     12
027700950615     D  D4FO13                13     13
027800950615     D  D4FO14                14     14
027900950615     D  D4FO15                15     15
028000001004B0273D  D5FO16                16     16
028100001004  "  D  D5FO17                17     17
028200001004  "  D  D5FO18                18     18
028300001004  "  D  D5FO19                19     19
028400001004  "  D  D5FO20                20     20
028500001004  "  D  D5FO21                21     21
028600001004  "  D  D5FO22                22     22
028700001004  "  D  D5FO23                23     23
028800001004  "  D  D5FO24                24     24
028900001004  "  D  D5FO25                25     25
029000001004  "  D  D5FO26                26     26
029100001004  "  D  D5FO27                27     27
029200001004  "  D  D5FO28                28     28
029300001004  "  D  D5FO29                29     29
029400001004B0273D  D5FO30                30     30
029500970102     D*-------------
029600941212     D* Parametri in ricezione
029700950615     D A007DS        E DS                  EXTNAME(ANA007DS)
029800961016     D*-------------
029900961016     D* Ricerca legende
030000961016     D Ana964Ds      E DS                  INZ
030100961016     D*-------------
030200961016     D* Controlli validità legende
030300961016     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
030400100505R002 D*-------------
030500080306  "  D* DS esterna per dati soggetti
030600080306  "  D DVASOG        E DS                  INZ
030700080306  "  D*-------------
030800080306  "  D* Definizioni parametri driver anagrafici
030900080306  "  D Dvaric          S              1
031000080306  "  D DvaSocieta      S              3
031100080306  "  D DvaUnita        S              8
031200080306  "  D DvaStrutt       S             10
031300080306  "  D DvaDtRif        S               D
031400080306  "  D DvaLenOut       S              9B 0
031500080306  "  D DvaSnatura      S              1
031600080306  "  D DvaCodice       S              8
031700080306  "  D DvaLineav       S              3
031800080306  "  D DvaFiliale      S              3  0
031900080306  "  D DvaSogg         S              8
032000080306  "  D DvaTpInd        S              2
032100080306  "  D DvaCdInd        S              3
032200080306  "  D DvaErrore       S              1
032300080306  "  D DvaOutput       S           4000
032400080306  "  D*-------------
032500080306  "  D* ricerca soggetti
032600100505R002 D ANA040DS      E DS                  INZ
032700961016     D*-------------
032800100505R002 X**** DtRifDiv                      10A   Inz('0001-01-01')
032900100505R002 D DtRifDiv        S             10A   Inz('0001-01-01')
033000000120A0740D ZDateISO        S               D
033100980916     D*-------------
033200970102     D* RIDENOMINAZIONE CAMPI
033300950615      *----------------------------------------------------*
033400941213     IANSIF01
033500941206     I              SIFSOCIETA                  SIFSOC
033600941206     I              SIFCDSIA                    SIFSIA
033700941206     I              SIFCDSIAGR                  SIFSI2
033800941206     I              SIFUBICAZ                   SIFMCH
033900941206     I              SIFSYSINF                   SIFNSI
034000941206     I              SIFLINGUA                   SIFLIN
034100941206     I              SIFFLGOPE                   SIFFLG
034200941206     I              SIFDESBREV                  SIFDSI
034300941206     I              SIFDIVISA                   SIFDIV
034400941206     I              SIFCAPOFCT                  SIFCPC
034500941206     I              SIFCAPOFAN                  SIFCPA
034600941206     I              SIFSOCFITT                  SIFSCF
034700941206     I              SIFCAPOFSN                  SIFCPF
034800941206     I              SIFINPUT                    SIFINP
034900941206     I              SIFLOCALIT                  SIFCIT
035000941206     I              SIFCDFISC                   SIFCDF
035100941206     I              SIFPARTIVA                  SIFIVA
035200941206     I              SIFPROV                     SIFPRV
035300941206     I              SIFSTATO                    SIFSTA
035400941206     I              SIFTELEFON                  SIFTEL
035500941206     I              SIFTELEX                    SIFTLX
035600941206     I              SIFTELEFAX                  SIFFAX
035700941206     I              SIFDTISCC                   SIFDTI
035800941206     I              SIFNATGIUR                  SIFNGR
035900941206     I              SIFDTAPBIL                  SIFDAB
036000941206     I              SIFDTSCBIL                  SIFDSA
036100941206     I              SIFISCCIAA                  SIFCCT
036200941206     I              SIFATCCIAA                  SIFATT
036300941206     I              SIFDTFIVL                   SIFDFV
036400941206     I              SIFCDCTL20                  SIFCDC
036500960102     I              SIFSALDOC                   SIFSLC
036600960102     I              SIFSALDOA                   SIFSLA
036700941103      *----------------------------------------------------*
036800941103      *                MAIN LINE PROGRAM
036900941103      *----------------------------------------------------*
037000941103      *
037100941103      * inizializzazione variabili
037200940223     C                   EXSR      INZVAR
037300941201      *
037400940223     C     $FINE         DOWEQ     *OFF
037500940202     C     $GEST         CASEQ     'D1'          GESD1
037600950615     C     $GEST         CASEQ     'D2'          GESD2
037700950615     C     $GEST         CASEQ     'D3'          GESD3
037800950615     C     $GEST         CASEQ     'D4'          GESD4
037900001004B0273C     $GEST         CASEQ     'D5'          GESD5
038000940117     C                   END
038100940117     C                   END
038200941206      *
038300941206      * fine programma
038400940325     C                   EXSR      ENDPGM
038500940325     C/EJECT
038600941206      ************************************************************
038700941206      * RICERCHE
038800941206      ************************************************************
038900940325     C     ENDPGM        BEGSR
039000941206      *
039100941206      * disallocazione rcd nel caso di modifica/annullamento
039200950615     C     OPZ008        IFEQ      '02'
039300950615     C     OPZ008        OREQ      '04'
039400950615     C                   UNLOCK    ANSIF01L                             22
039500940325     C                   END
039600941206      *
039700941206      * passaggio dati al pgm chiamante
039800950615     C                   MOVEL     A008DS        KPJBU
039900941206      *
040000940325     C                   SETON                                            RT
040100940616     C                   RETURN
040200941206      *
040300940325     C                   ENDSR
040400940325     C/EJECT
040500941206      ************************************************************
040600941206      * RICERCHE
040700941206      ************************************************************
040800940127     C     SEARCH        BEGSR
040900941206      *
041000941206      * determino Riga/Colonna del cursore
041100940127     C                   MOVE      NRG           R$$
041200940127     C                   MOVE      NCL           C$$
041300940127     C                   Z-ADD     RIRI          CURR
041400940127     C                   Z-ADD     COCO          CURC
041500941206      *
041600941206      * I campi che al momento dell'F4 risultano protetti non devono
041700941206      * essere abilitati alla funzione di ricerca
041800100505R002 C*
041900080306  "  C     D2SOGG        IFNE      *BLANK
042000080306  "  C                   CALLb     'XALLINEA'
042100080306  "  C                   PARM                    D2SOGG
042200080306  "  C                   PARM      8             $LEN              5 0
042300080306  "  C                   PARM                    XSCINP
042400100505R002 C                   ENDIF
042500941206      *
0426009401271    C                   SELECT
042700980116      *
042800980116      * Stato
042900950615     C     H1NMFL        WHENEQ    'D1STA'
043000941213     C                   MOVE      *BLANK        WWSTA
043100941213     C                   MOVE      WWSTA         KEYTAB
043200941213     C                   MOVE      '01'          OPZTAB
043300941213     C                   MOVE      XSCSOC        AZITAB
043400941213     C                   MOVE      XSCLIN        LINTAB
043500941213     C                   CLEAR                   KPJBU
043600941213     C                   MOVEL     XTABDS        KPJBU
043700941213     C                   CALL      'ANGG12R'
043800941213     C                   PARM                    KPJBA
043900941213     C                   MOVEL     KPJBU         XTABDS
044000941213     C                   MOVE      KEYTAB        WWSTA
0441009506152    C     WWSTA         IFNE      *BLANKS
044200950615     C                   MOVEL     WWSTA         D1STA
044300950616     C                   MOVEL     *BLANK        D1STAD
0444009506152-   C                   ENDIF
044500980116      *
044600980116      * Natura giuridica
044700950615     C     H1NMFL        WHENEQ    'D1NGR'
044800941213     C                   MOVE      *BLANK        WWNGR
044900941213     C                   MOVE      WWNGR         KEYTAB
045000941213     C                   MOVE      '01'          OPZTAB
045100941213     C                   MOVE      XSCSOC        AZITAB
045200941213     C                   MOVE      XSCLIN        LINTAB
045300941213     C                   CLEAR                   KPJBU
045400941213     C                   MOVEL     XTABDS        KPJBU
045500941213     C                   CALL      'ANGG16R'
045600941213     C                   PARM                    KPJBA
045700941213     C                   MOVEL     KPJBU         XTABDS
045800941213     C                   MOVE      KEYTAB        WWNGR
0459009506152    C     WWNGR         IFNE      *BLANKS
046000950615     C                   MOVEL     WWNGR         D1NGR
046100950616     C                   MOVEL     *BLANK        D1NGRD
0462009506152-   C                   ENDIF
046300980116      *
046400980116      * Divisa
046500950615     C     H1NMFL        WHENEQ    'D1DIV'
046600950615     C                   RESET                   A015DS
046700950615     C                   MOVEL     '01'          OPZ015
046800950615     C                   MOVEL     A015DS        KPJBU
046900950615     C                   CALL      'ANA015R'
047000950615     C                   PARM                    KPJBA
047100950615     C                   MOVEL     KPJBU         A015DS
047200980915     C     DIV015        IFNE      *BLANK
047300980915     C                   MOVEL     DIV015        D1DIV
047400950616     C                   MOVEL     *BLANK        D1DIVD
047500950615     C                   ENDIF
047600980116      *
047700980116      * Lingua
047800950616     C     H1NMFL        WHENEQ    'D1LIN'
047900001004B0273C     H1NMFL        OREQ      'D5FO17'
048000001004  "  C     H1NMFL        OREQ      'D5FO18'
048100001004  "  C     H1NMFL        OREQ      'D5FO19'
048200001004  "  C     H1NMFL        OREQ      'D5FO20'
048300001004  "  C     H1NMFL        OREQ      'D5FO21'
048400001004  "  C     H1NMFL        OREQ      'D5FO22'
048500001004  "  C     H1NMFL        OREQ      'D5FO23'
048600001004  "  C     H1NMFL        OREQ      'D5FO24'
048700001004B0273C     H1NMFL        OREQ      'D5FO25'
048800941213     C                   MOVE      *BLANK        WWLIN
048900941213     C                   MOVE      WWLIN         KEYTAB
049000941213     C                   MOVE      '01'          OPZTAB
049100941213     C                   MOVE      XSCSOC        AZITAB
049200941213     C                   MOVE      XSCLIN        LINTAB
049300941213     C                   CLEAR                   KPJBU
049400941213     C                   MOVEL     XTABDS        KPJBU
049500941213     C                   CALL      'ANGG15R'
049600941213     C                   PARM                    KPJBA
049700941213     C                   MOVEL     KPJBU         XTABDS
049800941213     C                   MOVE      KEYTAB        WWLIN
0499009506152    C     WWLIN         IFNE      *BLANKS
050000001004B0273C                   Select
050100001004B0273C                   When      H1NMFL = 'D1LIN'
050200950615     C                   MOVEL     WWLIN         D1LIN
050300950616     C                   MOVEL     *BLANK        D1LIND
050400001004B0273C                   When      H1NMFL = 'D5FO17'
050500001004  "  C                   MoveL     WWLin         D5FO17
050600001004  "  C                   Clear                   D5FO17D
050700001004  "  C                   When      H1NMFL = 'D5FO18'
050800001004  "  C                   MoveL     WWLin         D5FO18
050900001004  "  C                   Clear                   D5FO18D
051000001004  "  C                   When      H1NMFL = 'D5FO19'
051100001004  "  C                   MoveL     WWLin         D5FO19
051200001004  "  C                   Clear                   D5FO19D
051300001004  "  C                   When      H1NMFL = 'D5FO20'
051400001004  "  C                   MoveL     WWLin         D5FO20
051500001004  "  C                   Clear                   D5FO20D
051600001004  "  C                   When      H1NMFL = 'D5FO21'
051700001004  "  C                   MoveL     WWLin         D5FO21
051800001004  "  C                   Clear                   D5FO21D
051900001004  "  C                   When      H1NMFL = 'D5FO22'
052000001004  "  C                   MoveL     WWLin         D5FO22
052100001004  "  C                   Clear                   D5FO22D
052200001004  "  C                   When      H1NMFL = 'D5FO23'
052300001004  "  C                   MoveL     WWLin         D5FO23
052400001004  "  C                   Clear                   D5FO23D
052500001004  "  C                   When      H1NMFL = 'D5FO24'
052600001004  "  C                   MoveL     WWLin         D5FO24
052700001004  "  C                   Clear                   D5FO24D
052800001004  "  C                   When      H1NMFL = 'D5FO25'
052900001004  "  C                   MoveL     WWLin         D5FO25
053000001004  "  C                   Clear                   D5FO25D
053100001004B0273C                   EndSl
0532009506152-   C                   ENDIF
053300100505R002 C*
053400080306  "  C* FMTD2 - codice soggetto
053500080306  "  C     H1NMFL        WHENEQ    'D2SOGG'
053600100505R002 C                   EXSR      F4_Sogg
053700980116      *
053800980116      * Provincia
053900950616     C     H1NMFL        WHENEQ    'D2PRV'
054000941213     C                   MOVE      *BLANK        WWPRV
054100941213     C                   MOVE      WWPRV         KEYTAB
054200941213     C                   MOVE      '01'          OPZTAB
054300941213     C                   MOVE      XSCSOC        AZITAB
054400941213     C                   MOVE      XSCLIN        LINTAB
054500941213     C                   CLEAR                   KPJBU
054600941213     C                   MOVEL     XTABDS        KPJBU
054700941213     C                   CALL      'ANGC17R'
054800941213     C                   PARM                    KPJBA
054900941213     C                   MOVEL     KPJBU         XTABDS
055000941213     C                   MOVE      KEYTAB        WWPRV
0551009412132   >C     WWPRV         IFNE      *BLANKS
055200941213    >C                   MOVEL     WWPRV         D2PRV
0553009412132-  >C                   ENDIF
055400980116      *
055500980116      * Provincia
055600950616     C     H1NMFL        WHENEQ    'D2PRS'
055700941213     C                   MOVE      *BLANK        WWPRV
055800941213     C                   MOVE      WWPRV         KEYTAB
055900941213     C                   MOVE      '01'          OPZTAB
056000941213     C                   MOVE      XSCSOC        AZITAB
056100941213     C                   MOVE      XSCLIN        LINTAB
056200941213     C                   CLEAR                   KPJBU
056300941213     C                   MOVEL     XTABDS        KPJBU
056400941213     C                   CALL      'ANGC17R'
056500941213     C                   PARM                    KPJBA
056600941213     C                   MOVEL     KPJBU         XTABDS
056700941213     C                   MOVE      KEYTAB        WWPRV
0568009412132   >C     WWPRV         IFNE      *BLANKS
056900941213    >C                   MOVEL     WWPRV         D2PRS
0570009412132-  >C                   ENDIF
057100980116      *
057200980324      * Tesoreria
057300980324     C     H1NMFL        WHENEQ    'D3SK01'
057400961016     C                   RESET                   Ana964DS
057500961016     C                   MOVEL     '01'          OPZ964
057600961016     C                   MOVEL     'ANDIZ000'    TRC964
057700980324     C                   MOVEL     'DSPTESOR'    CAM964
057800961016     C                   MOVEL     ANA964DS      KPJBU
057900961016     C                   CALL      'ANA964R'
058000961016     C                   PARM                    KPJBA
058100961016     C                   MOVEL     KPJBU         ANA964DS
058200961016     C     val964        IFNE      *BLANK
058300980324     C                   MOVEL     val964        D3SK01
058400961016     C                   ENDIF
058500980324      *
058600980324      * Legislazione
058700980324     C     H1NMFL        WHENEQ    'D3SK02'
058800980324     C                   RESET                   Ana964DS
058900980324     C                   MOVEL     '01'          OPZ964
059000980324     C                   MOVEL     'ANDIZ000'    TRC964
059100980324     C                   MOVEL     'DSPLEGISLA'  CAM964
059200980324     C                   MOVEL     ANA964DS      KPJBU
059300980324     C                   CALL      'ANA964R'
059400980324     C                   PARM                    KPJBA
059500980324     C                   MOVEL     KPJBU         ANA964DS
059600980324     C     val964        IFNE      *BLANK
059700980324     C                   MOVEL     val964        D3SK02
059800980324     C                   ENDIF
059900980116      *
060000961125      * Tipo software gestionale
060100961125     C     H1NMFL        WHENEQ    'D3SK03'
060200961125     C                   RESET                   Ana964DS
060300961125     C                   MOVEL     '01'          OPZ964
060400961125     C                   MOVEL     'ANDIZ000'    TRC964
060500961125     C                   MOVEL     'DSPTPSWGES'  CAM964
060600961125     C                   MOVEL     ANA964DS      KPJBU
060700961125     C                   CALL      'ANA964R'
060800961125     C                   PARM                    KPJBA
060900961125     C                   MOVEL     KPJBU         ANA964DS
061000961125     C*
061100961125     C     val964        IFNE      *BLANK
061200961125     C                   MOVEL     val964        D3SK03
061300961125     C                   ENDIF
061400980116      *
061500980116      * Allineamento campi input
061600980116     C     H1NMFL        WHENEQ    'D3INP'
061700980116     C                   RESET                   Ana964DS
061800980116     C                   MOVEL     '01'          OPZ964
061900980116     C                   MOVEL     'ANSIF000'    TRC964
062000980116     C                   MOVEL     'SIFINPUT'    CAM964
062100980116     C                   MOVE      *ON           ALL964
062200980116     C                   MOVEL     ANA964DS      KPJBU
062300980116     C                   CALL      'ANA964R'
062400980116     C                   PARM                    KPJBA
062500980116     C                   MOVEL     KPJBU         ANA964DS
062600980116     C     ret964        IFeq      *BLANK
062700980116     C                   MOVEL     val964        D3INP
062800980116     C                   ENDIF
062900980710      *
063000980710      * Regime IVA
063100980710     C     H1NMFL        WHENEQ    'D4FO09'
063200980710     C                   RESET                   Ana964DS
063300980710     C                   MOVEL     '01'          OPZ964
063400980710     C                   MOVEL     'ANDIZ000'    TRC964
063500980710     C                   MOVEL     'DSPIVARGM'   CAM964
063600980710     C                   MOVEL     ANA964DS      KPJBU
063700980710     C                   CALL      'ANA964R'
063800980710     C                   PARM                    KPJBA
063900980710     C                   MOVEL     KPJBU         ANA964DS
064000980710     C     val964        IFNE      *BLANK
064100980710     C                   MOVEL     val964        D4FO09
064200980710     C                   MOVEL     des964        D4FO09D
064300980710     C                   ENDIF
064400980710      *
064500980710      * Liquidazione IVA
064600980710     C     H1NMFL        WHENEQ    'D4FO10'
064700980710     C                   RESET                   Ana964DS
064800980710     C                   MOVEL     '01'          OPZ964
064900980710     C                   MOVEL     'ANDIZ000'    TRC964
065000980710     C                   MOVEL     'DSPIVALIQ'   CAM964
065100980710     C                   MOVEL     ANA964DS      KPJBU
065200980710     C                   CALL      'ANA964R'
065300980710     C                   PARM                    KPJBA
065400980710     C                   MOVEL     KPJBU         ANA964DS
065500980710     C     val964        IFNE      *BLANK
065600980710     C                   MOVEL     val964        D4FO10
065700980710     C                   MOVEL     des964        D4FO10D
065800980710     C                   ENDIF
065900021007      *
066000021007c1582 * Regole controllo entità
066100021007  "  C     H1NMFL        WHENEQ    'D4FO11'
066200021007  "  C                   RESET                   Ana964DS
066300021007  "  C                   MOVEL     '01'          OPZ964
066400021007  "  C                   MOVEL     'ANDIZ000'    TRC964
066500021007  "  C                   MOVEL     'DSPSINO  '   CAM964
066600021007  "  C                   MOVEL     ANA964DS      KPJBU
066700021007  "  C                   CALL      'ANA964R'
066800021007  "  C                   PARM                    KPJBA
066900021007  "  C                   MOVEL     KPJBU         ANA964DS
067000021007  "  C     Ret964        IFNE      '2'
067100021007  "  C                   MOVEL     val964        D4FO11
067200021007  "  C                   MOVEL     des964        D4FO11D
067300021007c1582C                   ENDIF
067400001009      *
067500001010B0278 * Società fittizia
067600001010  "  C     H1NMFL        WHENEQ    'D1SCF'
067700001010  "  C                   RESET                   Ana964DS
067800001010  "  C                   MOVEL     '01'          OPZ964
067900001010  "  C                   MOVEL     'ANDIZ000'    TRC964
068000001010  "  C                   MOVEL     'DSPSINO'     CAM964
068100001010  "  C                   MOVEL     *OFF          ALL964
068200001010  "  C                   MOVEL     ANA964DS      KPJBU
068300001010  "  C                   CALL      'ANA964R'
068400001010  "  C                   PARM                    KPJBA
068500001010  "  C                   MOVEL     KPJBU         ANA964DS
068600001010  "  C     Ret964        IFNE      '2'
068700001010  "  C                   MOVEL     val964        D1Scf
068800001010  "  C                   ENDIF
068900001010  "   *
069000001010  "   * Saldi per data registrazione
069100001010  "  C     H1NMFL        WHENEQ    'D3SK04'
069200001010  "  C                   RESET                   Ana964DS
069300001010  "  C                   MOVEL     '01'          OPZ964
069400001010  "  C                   MOVEL     'ANDIZ000'    TRC964
069500001010  "  C                   MOVEL     'DSPSINO'     CAM964
069600001010  "  C                   MOVEL     *OFF          ALL964
069700001010  "  C                   MOVEL     ANA964DS      KPJBU
069800001010  "  C                   CALL      'ANA964R'
069900001010  "  C                   PARM                    KPJBA
070000001010  "  C                   MOVEL     KPJBU         ANA964DS
070100001010  "  C     Ret964        IFNE      '2'
070200001010  "  C                   MOVEL     val964        D3SK04
070300001010  "  C                   ENDIF
070400001010  "   *
070500001010  "   * Saldi per data competenza
070600001010  "  C     H1NMFL        WHENEQ    'D3SK05'
070700001010  "  C                   RESET                   Ana964DS
070800001010  "  C                   MOVEL     '01'          OPZ964
070900050113  "  C                   MOVEL     'ANDIZ000'    TRC964
071000050113  "  C                   MOVEL     'DSPSINO'     CAM964
071100050113  "  C                   MOVEL     *OFF          ALL964
071200001010  "  C                   MOVEL     ANA964DS      KPJBU
071300001010  "  C                   CALL      'ANA964R'
071400001010  "  C                   PARM                    KPJBA
071500001010  "  C                   MOVEL     KPJBU         ANA964DS
071600001010  "  C     Ret964        IFNE      '2'
071700001010  "  C                   MOVEL     val964        D3SK05
071800001010  "  C                   ENDIF
071900001010  "   *
072000001010  "   * registrazioni intercompany
072100001010  "  C     H1NMFL        WHENEQ    'D3SK07'
072200001010  "  C                   RESET                   Ana964DS
072300001010  "  C                   MOVEL     '01'          OPZ964
072400050113B0278C                   MOVEL     'ANDIZ000'    TRC964
072500050113C1813X**exB0278          MOVEL     'DSPSINO'     CAM964
072600050113C1813C                   MOVEL     'DSPSOCINT'   CAM964
072700050113B0278C                   MOVEL     *OFF          ALL964
072800001010  "  C                   MOVEL     ANA964DS      KPJBU
072900001010  "  C                   CALL      'ANA964R'
073000001010  "  C                   PARM                    KPJBA
073100001010  "  C                   MOVEL     KPJBU         ANA964DS
073200001010  "  C     Ret964        IFNE      '2'
073300001010  "  C                   MOVEL     val964        D3SK07
073400001010  "  C                   ENDIF
073500001010  "   * gestione unità
073600001010  "  C     H1NMFL        WHENEQ    'D4FO01'
073700001010  "  C                   RESET                   Ana964DS
073800001010  "  C                   MOVEL     '01'          OPZ964
073900001010  "  C                   MOVEL     'ANDIZ000'    TRC964
074000001010  "  C                   MOVEL     'DSPSINO'     CAM964
074100001010  "  C                   MOVEL     *OFF          ALL964
074200001010  "  C                   MOVEL     ANA964DS      KPJBU
074300001010  "  C                   CALL      'ANA964R'
074400001010  "  C                   PARM                    KPJBA
074500001010  "  C                   MOVEL     KPJBU         ANA964DS
074600001010  "  C     Ret964        IFNE      '2'
074700001010  "  C                   MOVEL     val964        D4FO01
074800001010  "  C                   ENDIF
074900001010  "   * gestione PUBBLICA
075000001010  "  C     H1NMFL        WHENEQ    'D4FO02'
075100001010  "  C                   RESET                   Ana964DS
075200001010  "  C                   MOVEL     '01'          OPZ964
075300001010  "  C                   MOVEL     'ANDIZ000'    TRC964
075400001010  "  C                   MOVEL     'DSPSINO'     CAM964
075500001010  "  C                   MOVEL     *OFF          ALL964
075600001010  "  C                   MOVEL     ANA964DS      KPJBU
075700001010  "  C                   CALL      'ANA964R'
075800001010  "  C                   PARM                    KPJBA
075900001010  "  C                   MOVEL     KPJBU         ANA964DS
076000001010  "  C     Ret964        IFNE      '2'
076100001010  "  C                   MOVEL     val964        D4FO02
076200001010  "  C                   ENDIF
076300001010  "   * tipo plafond
076400001010  "  C     H1NMFL        WHENEQ    'D4FO03'
076500001010  "  C                   RESET                   Ana964DS
076600001010  "  C                   MOVEL     '01'          OPZ964
076700001010  "  C                   MOVEL     'ANDIZ000'    TRC964
076800001010  "  C                   MOVEL     'DSPPLAFOND'  CAM964
076900001010  "  C                   MOVEL     *OFF          ALL964
077000001010  "  C                   MOVEL     ANA964DS      KPJBU
077100001010  "  C                   CALL      'ANA964R'
077200001010  "  C                   PARM                    KPJBA
077300001010  "  C                   MOVEL     KPJBU         ANA964DS
077400001010  "  C     Ret964        IFNE      '2'
077500001010  "  C                   MOVEL     val964        D4FO03
077600001010  "  C                   ENDIF
077700001010  "   * ventilazione iva
077800001010  "  C     H1NMFL        WHENEQ    'D4FO04'
077900001010  "  C                   RESET                   Ana964DS
078000001010  "  C                   MOVEL     '01'          OPZ964
078100001010  "  C                   MOVEL     'ANDIZ000'    TRC964
078200001010  "  C                   MOVEL     'DSPSINO'     CAM964
078300001010  "  C                   MOVEL     *OFF          ALL964
078400001010  "  C                   MOVEL     ANA964DS      KPJBU
078500001010  "  C                   CALL      'ANA964R'
078600001010  "  C                   PARM                    KPJBA
078700001010  "  C                   MOVEL     KPJBU         ANA964DS
078800001010  "  C     Ret964        IFNE      '2'
078900001010  "  C                   MOVEL     val964        D4FO04
079000001010  "  C                   ENDIF
079100001010  "   * unita=ldv
079200001010  "  C     H1NMFL        WHENEQ    'D4FO05'
079300001010  "  C                   RESET                   Ana964DS
079400001010  "  C                   MOVEL     '01'          OPZ964
079500001010  "  C                   MOVEL     'ANDIZ000'    TRC964
079600001010  "  C                   MOVEL     'DSPSINO'     CAM964
079700001010  "  C                   MOVEL     *OFF          ALL964
079800001010  "  C                   MOVEL     ANA964DS      KPJBU
079900001010  "  C                   CALL      'ANA964R'
080000001010  "  C                   PARM                    KPJBA
080100001010  "  C                   MOVEL     KPJBU         ANA964DS
080200001010  "  C     Ret964        IFNE      '2'
080300001010  "  C                   MOVEL     val964        D4FO05
080400001010  "  C                   ENDIF
080500001010  "   * lavori automatici
080600001010  "  C     H1NMFL        WHENEQ    'D4FO06'
080700001010  "  C                   RESET                   Ana964DS
080800001010  "  C                   MOVEL     '01'          OPZ964
080900001010  "  C                   MOVEL     'ANDIZ000'    TRC964
081000001010  "  C                   MOVEL     'DSPSINO'     CAM964
081100001010  "  C                   MOVEL     *OFF          ALL964
081200001010  "  C                   MOVEL     ANA964DS      KPJBU
081300001010  "  C                   CALL      'ANA964R'
081400001010  "  C                   PARM                    KPJBA
081500001010  "  C                   MOVEL     KPJBU         ANA964DS
081600001010  "  C     Ret964        IFNE      '2'
081700001010  "  C                   MOVEL     val964        D4FO06
081800001010  "  C                   ENDIF
081900001010  "   * reperimento lingua
082000001010  "  C     H1NMFL        WHENEQ    'D5FO16'
082100001010  "  C                   RESET                   Ana964DS
082200001010  "  C                   MOVEL     '01'          OPZ964
082300001010  "  C                   MOVEL     'ANDIZ000'    TRC964
082400001010  "  C                   MOVEL     'DSPREPLIN'   CAM964
082500001010  "  C                   MOVEL     *OFF          ALL964
082600001010  "  C                   MOVEL     ANA964DS      KPJBU
082700001010  "  C                   CALL      'ANA964R'
082800001010  "  C                   PARM                    KPJBA
082900001010  "  C                   MOVEL     KPJBU         ANA964DS
083000001010  "  C     Ret964        IFNE      '2'
083100001011  "  C                   MOVEL     val964        D5FO16
083200001010B0278C                   ENDIF
083300011211A0910C*
083400011211  "  C* Presenza Laguna
083500011211  "  C     H1NMFL        WHENEQ    'D3SK08'
083600011211  "  C                   RESET                   Ana964DS
083700011211  "  C                   MOVEL     '01'          OPZ964
083800011211  "  C                   MOVEL     'ANDIZ000'    TRC964
083900011211  "  C                   MOVEL     'DSPSINO'     CAM964
084000011211  "  C                   MOVEL     *OFF          ALL964
084100011211  "  C                   MOVEL     ANA964DS      KPJBU
084200011211  "  C                   CALL      'ANA964R'
084300011211  "  C                   PARM                    KPJBA
084400011211  "  C                   MOVEL     KPJBU         ANA964DS
084500011211  "  C     Ret964        IFNE      '2'
084600011211  "  C                   MOVEL     val964        D3SK08
084700011211A0910C                   ENDIF
084800941206      *
084900941206      * ricerca non abilitata in questa posizione
0850009402231O   C                   OTHER
085100940926     C                   SETON                                        9899
085200941206      *
0853009401271-   C                   ENDSL
085400941206      *
085500961016      * imposto pos. del cursore
085600961016     C                   Z-ADD     CURR          H1RIGA
085700961016     C                   Z-ADD     CURC          H1COLO
085800961016      *
085900940506     C                   MOVE      *HIVAL        $LASTV
086000940127     C                   ENDSR
086100940225     C/EJECT
086200941206      ************************************************************
086300941206      * GESTIONE EMISSIONE TASTI FUNZIONE
086400941206      ************************************************************
086500940225     C     GESPIE        BEGSR
086600941206      *
086700940225     C                   SELECT
086800941206      *
086900940506     C     $GEST         WHENEQ    'D1'
087000940506     C     $ULKD1        IFEQ      0
087100941206      * riempimento opzioni
087200940506     C                   EXSR      F24D1
087300940506     C                   ELSE
087400940506     C                   WRITE     FMTZ1
087500940506     C                   ENDIF
087600941206      *
087700941206     C     $GEST         WHENEQ    'D2'
087800941206     C     $ULKD1        IFEQ      0
087900941206      **riempimento opzioni
088000941206     C                   EXSR      F24D2
088100941206     C                   ELSE
088200941206     C                   WRITE     FMTZ1
088300941206     C                   ENDIF
088400941206      *
088500941206     C     $GEST         WHENEQ    'D3'
088600941206     C     $ULKD1        IFEQ      0
088700941206      **riempimento opzioni
088800941206     C                   EXSR      F24D3
088900941206     C                   ELSE
089000941206     C                   WRITE     FMTZ1
089100941206     C                   ENDIF
089200941206      *
089300941206     C     $GEST         WHENEQ    'D4'
089400941206     C     $ULKD1        IFEQ      0
089500941206      **riempimento opzioni
089600941206     C                   EXSR      F24D4
089700941206     C                   ELSE
089800941206     C                   WRITE     FMTZ1
089900941206     C                   ENDIF
090000001004B0273 *
090100001004  "  C     $GEST         WHENEQ    'D5'
090200001004  "  C     $ULKD1        IFEQ      0
090300001004  "   **riempimento opzioni
090400001004  "  C                   EXSR      F24D5
090500001004  "  C                   ELSE
090600001004  "  C                   WRITE     FMTZ1
090700001004  "  C                   ENDIF
090800001004B0273 *
090900940225     C                   ENDSL
091000941206      *
091100940225     C                   ENDSR
091200940131     C/EJECT
091300100505R002 C************************************************************
091400080306  "  C* Ricerca codice soggetto
091500080306  "  C************************************************************
091600100505R002 C     F4_Sogg       BEGSR
091700080306     C*
091800080306     C                   RESET                   ANA040DS
091900080306     C*
092000080306     C                   MOVEL     '01'          OPZ040
092100080306     C                   MoveL     D2Sogg        Cod040
092200080306     C                   MOVEL     ANA040DS      KPJBU
092300080306     C                   CALL      'ANA040R'
092400080306     C                   PARM                    KPJBA
092500080306     C*
092600080306     C                   MOVEL     KPJBU         ANA040DS
092700080306     C*
092800080306     C                   if        Ret040     <> '2'
092900080306     C                   MoveL     Cod040        D2Sogg
093000080306     C                   endif
093100080306     C*
093200100505R002 C                   ENDSR
093300100505R002  /EJECT
093400941206      ************************************************************
093500941206      * GESTIONE VIDEO RECORD D1
093600941206      ************************************************************
093700940127     C     GESD1         BEGSR
093800941206      * inizializzazione videata
093900940223     C     $INZD1        IFEQ      *ON
094000941206     C                   EXSR      INZD1
094100940223     C                   MOVE      *OFF          $INZD1
094200940117     C                   END
094300941206      *
094400941206      * emissione piede videata se proveniente da altra
094500941206      * salvataggio valore formato di provenienza
094600940207     C     $LASTV        IFNE      'D1'
094700940207     C                   WRITE     FMTT1
094800940225     C                   EXSR      GESPIE
094900940202     C                   MOVE      $LASTV        $LASTG
095000940202     C                   MOVE      'D1'          $LASTV
095100940707     C                   ENDIF
095200941206      *
095300941206      * se esistono errori sulla videata
095400941206      * emetto la write del formato a indicatori spenti per vedere
095500941206      * le eventuali decodifiche
095600941206      *
095700940707     C     *IN99         IFEQ      *ON
095800940707     C     $GEST         ANDEQ     'D1'
095900940707     C                   MOVEA     *IN           WIN
096000940707     C                   MOVE      *ALL'0'       IN5098           49
096100940707     C                   MOVEA     IN5098        *IN(50)
096200940707     C                   WRITE     FMTD1
096300940707     C                   MOVEA     WIN           *IN
096400940707     C                   ENDIF
096500941206      *
096600940203     C                   SELECT
096700941206      * Annullamento
096800941206      * Visualizzazione
096900941212    >C     OPZ008        WHENEQ    '05'
097000940203     C                   WRITE     FMTD1
097100940203     C                   EXFMT     PROTECT
097200941206      * Immissione/Modifica/Copia
097300940203     C                   OTHER
097400940203     C                   EXFMT     FMTD1
097500940203     C                   ENDSL
097600941206      * controllo abilitazione tasti funzione
097700941206      *
097800940506     C                   EXSR      CTRKD1
097900941206      *
098000940506     C     *IN99         IFEQ      *OFF
098100941206      *
0982009402031    C                   SELECT
098300941206      * ROLLUP
098400941206    >C     $TASTO        WHENEQ    ROLLUP
098500941206    >C                   MOVE      'D2'          $GEST
098600941206      * F2=Decodifica
098700940916     C     $TASTO        WHENEQ    F02
098800940916     C                   EXSR      F02D1
098900941206      * F3=Fine
099000940506     C     $TASTO        WHENEQ    F03
099100940309     C                   EXSR      F03D1
099200941206      * F4=Ricerca
099300940506     C     $TASTO        WHENEQ    F04
099400940127     C                   EXSR      SEARCH
099500941206      * F5=Refresh
099600940506     C     $TASTO        WHENEQ    F05
099700940309     C                   EXSR      F05D1
099800941206      * F8=Successivo
099900940506     C     $TASTO        WHENEQ    F08
100000940309     C                   EXSR      F08D1
100100941206      * F12=Ritorno
100200940506     C     $TASTO        WHENEQ    F12
100300940309     C                   EXSR      F12D1
100400941206      * F24=Altri tasti
100500940506     C     $TASTO        WHENEQ    F24
100600940315     C                   EXSR      F24D1
100700941206      *
1008009402031O   C                   OTHER
100900940127     C                   EXSR      CTRD1
101000941206      * aggiungere le chiamate ai controlli delle videate supplementari
101100950616     C     *IN99         IFEQ      *OFF
101200950616     C                   EXSR      CTRD2
101300950616     C     *IN99         IFEQ      *ON
101400970603     C                   MOVE      *ALL'0'       IN5098
101500970603     C                   MOVEA     IN5098        *IN(50)
101600970603     C****               MOVEA     *ALL'0'       *IN(50)
101700970603     C                   SETON                                            96
101800950616     C                   ELSE
101900950616     C                   EXSR      CTRD3
102000950616     C     *IN99         IFEQ      *ON
102100970603     C                   MOVE      *ALL'0'       IN5098
102200970603     C                   MOVEA     IN5098        *IN(50)
102300970603     C****               MOVEA     *ALL'0'       *IN(50)
102400970603     C                   SETON                                            96
102500941213    >C                   ELSE
102600941206    >C                   EXSR      CTRD4
102700941206    >C     *IN99         IFEQ      *ON
102800970603     C                   MOVE      *ALL'0'       IN5098
102900970603     C                   MOVEA     IN5098        *IN(50)
103000970603     C****               MOVEA     *ALL'0'       *IN(50)
103100970603     C                   SETON                                            96
103200001004B0273C                   ELSE
103300001004  "  C                   EXSR      CTRD5
103400001004  "  C     *IN99         IFEQ      *ON
103500001004  "  C                   MOVE      *ALL'0'       IN5098
103600001004  "  C                   MOVEA     IN5098        *IN(50)
103700001004  "  C                   SETON                                            96
103800001004B0273C                   ENDIF
103900941206    >C                   ENDIF
104000941213    >C                   ENDIF
104100941213    >C                   ENDIF
104200941206    >C                   ENDIF
104300941206      *
1044009405062    C     $TASTO        IFEQ      F06
104500940207     C     *IN99         ANDEQ     *OFF
104600980811      * Avverto utente: le modifiche sono operative dal prossimo collegament
104700980811     C                   EXSR      AvvertoUte
104800941206      * eseguo aggiornamento
104900941206    >C                   EXSR      AGGANA
105000941206      * eseguo operazioni del dopo-aggiornamento
1051009403183    C     *IN99         IFEQ      *OFF
105200940318     C                   EXSR      GESAGG
1053009403183-   C                   ENDIF
1054009402242-   C                   ENDIF
105500941206      *
1056009402241-   C                   ENDSL
105700941206      *
1058009405062-   C                   ENDIF
105900940117     C                   ENDSR
106000940506     C/EJECT
106100980811      ******************************************************
106200980811      * Avverto l'Utente che le modifiche sono operative   *
106300980811      * dal prossimo collegamento                          *
106400980811      ******************************************************
106500980811     C     AvvertoUte    BEGSR
106600980811      *
106700980811      * Reperisco i messaggi per la finestrina di XmsgR
106800980811     C                   EXSR      RepMsg
106900980811      *
107000980811      * Chiamo la gestioen della finestrina per chiedere conferma all'utente
107100980811     C                   Z-ADD     0             MSGMSG
107200980811     C                   MOVEL     'N'           MSGRSP
107300980811     C                   MOVEL     *Blank        MSGTPR
107400980811     C                   MOVEL     'S'           MSGEMV
107500980811     C                   Z-ADD     10            MSGRGP
107600980811     C                   MOVEL     'N'           MSGLCK
107700980811     C                   MOVEL     'N'           MSGCNL
107800980811     C                   MOVEL     'N'           MSGVID
107900980811     C                   MOVEL     'N'           MSGOPE
108000980811     C                   MOVEL     'N'           MSGSTP
108100980811     C*
108200980811     C                   CALL      'XMSGR'
108300980811     C                   PARM                    XMSGDS
108400980811     C                   PARM                    XMSGTXT         135
108500980811     C*
108600980811     C     XMsgPgm       ENDSR
108700980811     C/EJECT
108800980811     C************************************************************
108900980811     C* Reperimento messaggi
109000980811     C************************************************************
109100980811     C     RepMsg        BEGSR
109200980811      *
109300980811     C                   MoveL     $Cos(1)       IDMsg
109400980811     C                   CALLB     'XRTVM'
109500980811     C                   PARM                    IDMSG            27
109600980811     C                   PARM                    TXTMSG          100
109700980811     C                   PARM                    ERRMSG            1
109800980811      *
109900980811     C     ErrMsg        IfEq      *On
110000980811     C                   MoveL     *ALL' '       TxtMsg
110100980811     C                   EndIf
110200980811      *  "AVVERTIMENTO: le modifiche saranno operative dal prossimo collegamento.
110300980811     C                   MoveL     TxtMsg        MsgMS1
110400980811      *
110500980811     C     XRepMsg       ENDSR
110600980811     C/EJECT
110700941206      ************************************************************
110800941206      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
110900941206      ************************************************************
111000940506     C     CTRKD1        BEGSR
111100940506     C                   SETOFF                                       99
111200940506     C                   MOVEA     $FC1          $FC
111300940506     C                   EXSR      CTRKEY
111400940506     C     *IN99         IFEQ      *ON
111500940506     C                   SETON                                        97
111600940506     C                   ENDIF
111700940506     C                   ENDSR
111800941206     C/EJECT
111900940117      ************************************************************
112000940207      * INIZIALIZZAZIONE VIDEATA DATI
112100940117      ************************************************************
112200941206     C     INZD1         BEGSR
112300941206      *
112400940131     C                   CLEAR                   FMTD1
112500941206     C                   CLEAR                   FMTD2
112600941206     C                   CLEAR                   FMTD3
112700941206     C                   CLEAR                   FMTD4
112800001004B0273C                   CLEAR                   FMTD5
112900941206      *
113000001009B0278C******             Move      XScOn         D1On1
113100001009B0278C******             Move      XScOff        D1Off1
113200981106      *
113300940225     C                   Z-ADD     1             $PIED1
113400941206      *
113500941206      * valorizzo la chiave univoca per reperire i dati
113600941206      * se non immissione
1137009506161    C     OPZ008        IFNE      '01'
113800950616      *-------------------------------------------------------------
113900950616      * impostare i campi chiave ricevuti
114000950616      *-------------------------------------------------------------
114100950616     C                   MOVEL     SOC008        SIFSOC
114200941206      *
1143009403182    C     *IN22         DOUEQ     *OFF
114400940318     C     MSGRTN        ORNE      'R'
114500941206      * se Modifica/Annullamento, il rcd che leggo viene bloccato
1146009506163    C     OPZ008        IFEQ      '02'
114700950616     C     OPZ008        OREQ      '04'
114800950616     C     K1SIF1        CHAIN     ANSIF01                            2122
114900941206      * se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
1150009403183E   C                   ELSE
115100950921     C     K1SIF1        CHAIN(N)  ANSIF01                            2122
1152009403183-   C                   ENDIF
115300941206      *
115400941206      * Gestione RCD allocato
1155009403183    C     *IN22         IFEQ      *ON
115600950616     C                   MOVEL     'ANSIF00F'    MSGFIL
115700940318     C                   EXSR      MSGPGM
1158009403183-   C                   ENDIF
1159009403182-   C                   ENDDO
116000941206      * se il rcd da gestire non viene trovato
116100941206      * oppure è allocato da altro pgm
116200941206      * ritorno al pgm guida avvertendolo dell'errore
1163009402242    C     *IN21         IFEQ      *ON
116400940318     C     *IN22         OREQ      *ON
116500940224     C                   MOVE      *ON           $FINE
116600950616     C                   MOVE      '1'           ERR008
116700941206      *
116800941206      * altrimenti
1169009402242E   C                   ELSE
117000941206      * riempio la videata
117100950111      * salvo la data fine validità per evitare che si sporchi nelle
117200950111      * varie decodifiche
117300950111     C                   MOVE      SIFDFV        WWWDFV
117400940224     C                   EXSR      RIED1
117500941206     C                   EXSR      RIED2
117600941206     C                   EXSR      RIED3
117700001004B0273X*****              EXSR      RIED4
117800001004B0273C                   EXSR      RIED4D5
117900941206      *
118000950616     C                   EXSR      CTRD1
118100950616     C                   EXSR      CTRD2
118200950616     C                   EXSR      CTRD3
118300950616     C                   EXSR      CTRD4
118400001004B0273C                   EXSR      CTRD5
118500940224     C                   MOVE      *ALL'0'       IN5098
118600940224     C                   MOVEA     IN5098        *IN(50)
118700941206      * definizione indicatori su video
118800941212     C                   SETOFF                                       020401
118900941212      *
1190009402243    C                   SELECT
119100950616     C     OPZ008        WHENEQ    '02'
119200940224     C                   SETON                                        02        PR campi key
119300941212      *Se variazione e data validita' < udate  permetto la sola
119400941212      *modifica della data validita'
119500000120A0740C*****              MOVEL     UYEAR         ANNO
119600000120  "  C*****              MOVEL     UMONTH        MESE
119700000120  "  C*****              MOVEL     UDAY          GIORNO
119800000120  "  C*****              MOVEL     DGMA06        XDTGMA
119900000120A0740C                   MOVEL     ZDate         XDTGMA
120000980915     C                   CALLB     'XDT4000'
120100980915     C                   PARM                    XDTAMG           10
120200980915     C                   PARM                    XDTGMA            6 0
120300980915     C                   PARM      4             XDTSTA            1 0
120400980915 4   C     XDTSTA        IFEQ      0                                            *
120500980915     C                   MOVE      XDTAMG        DAMG10
120600980915 4-  C                   ENDIF                                                  *
120700980915     C     DAMG10        IFGT      WWWDFV
120800950116     C     WWWDFV        ANDNE     *BLANK
120900950116     C     WWWDFV        ANDNE     DATMIN
121000941212     C                   SETON                                          04
121100941212     C                   ENDIF                                                  *
121200941212      *
121300950616     C     OPZ008        WHENEQ    '04'
121400950110     C                   SETON                                        04        PR campi
121500950616     C     OPZ008        WHENEQ    '05'
1216009402243-   C                   ENDSL
121700941206      *
1218009402242-   C                   ENDIF
121900941206      *
122000950616     C                   ELSE
122100961010     C* F10
122200961010     C                   MOVE      Nomsys        D1mch
122300950616     C                   MOVE      '0'           D4FO01
122400951016     C                   MOVE      '0'           D4FO02
122500951106     C                   MOVE      '0'           D4FO03
122600980116     C***                MOVE      '1'           D4FO04
122700960902     C                   MOVE      '0'           D4FO05
122800960902     C                   MOVE      '0'           D4FO06
122900960806     C                   MOVE      'D'           D4FO07
123000960806     C                   MOVE      'A'           D4FO08
1231009506161-   C                   ENDIF
123200941206      *
123300940117     C                   ENDSR
123400940207     C/EJECT
123500941206      ************************************************************
123600941206      * RIEMPIMENTO VIDEATA  D1
123700941206      ************************************************************
123800940207     C     RIED1         BEGSR
123900941206      *
124000950616      *----------------------------------------------------------
124100950616      * impostare i campi della videata
124200950616      *----------------------------------------------------------
124300950616     C                   MOVEL     SIFSOC        D1SOC
124400950616     C                   MOVEL     SIFDSI        D1DSI
124500950616     C                   MOVEL     SIFNSI        D1NSI
124600950616     C                   MOVEL     SIFSIA        D1SIA
124700950616     C                   MOVEL     SIFSI2        D1SI2
124800950616     C                   MOVEL     SIFMCH        D1MCH
124900950616     C                   MOVEL     SIFSCF        D1SCF
125000950616     C                   MOVEL     SIFCPC        D1CPC
125100950616     C                   MOVEL     SIFCPA        D1CPA
125200950616     C                   MOVEL     SIFCPF        D1CPF
125300950616     C                   MOVEL     SIFLIN        D1LIN
125400950616     C                   MOVEL     SIFDIV        D1DIV
125500950616     C                   MOVEL     SIFSTA        D1STA
125600950616     C                   MOVEL     SIFNGR        D1NGR
125700950616     C                   MOVEL     SIFCDF        D1CDF
125800950616     C                   MOVEL     SIFIVA        D1IVA
125900950616      *
126000941206     C                   MOVEL     SIFDFV        XDTAMG                         *
126100960930     C                   CALLB     'XDT4000'
126200941201     C                   PARM                    XDTAMG           10
126300941201     C                   PARM                    XDTGMA            6 0
126400941201     C                   PARM      2             XDTSTA            1 0
126500941201 4   C     XDTSTA        IFEQ      0                                            *
126600941206     C                   Z-ADD     XDTGMA        D1DFV
126700941201     C                   ENDIF
126800941206      *
126900941201     C                   MOVEL     SIFDAB        XDTAMG                         *
127000960930     C                   CALLB     'XDT4000'
127100941201     C                   PARM                    XDTAMG           10
127200941201     C                   PARM                    XDTGMA            6 0
127300941201     C                   PARM      2             XDTSTA            1 0
127400941201 4   C     XDTSTA        IFEQ      0                                            *
127500941201     C                   Z-ADD     XDTGMA        D1DAB
127600941201     C                   ENDIF
127700941206      *
127800941206     C                   MOVEL     SIFDSA        XDTAMG                         *
127900960930     C                   CALLB     'XDT4000'
128000941206     C                   PARM                    XDTAMG           10
128100941206     C                   PARM                    XDTGMA            6 0
128200941206     C                   PARM      2             XDTSTA            1 0
128300941206 4   C     XDTSTA        IFEQ      0                                            *
128400941206     C                   Z-ADD     XDTGMA        D1DSA
128500941206     C                   ENDIF
128600960827      *
128700960827     C                   MOVEL     SIFSlc        D1LIBDATHB
128800941206      *
128900940207     C                   ENDSR
129000940916     C/EJECT
129100941206      ************************************************************
129200941206      * GESTIONE F02 VIDEO D1
129300941206      ************************************************************
129400940916     C     F02D1         BEGSR
129500941206      *
129600941206      * per decodificare la videata richiamo la routine dei ctrl
129700941206      * e poi disattivo gli eventuali messaggi di errore
129800940916     C                   EXSR      CTRD1
129900941206      *
130000940916     C                   MOVE      *ALL'0'       IN5098
130100940916     C                   MOVEA     IN5098        *IN(50)
130200941206      *
130300940916     C                   ENDSR
130400940309     C/EJECT
130500941206      ************************************************************
130600941206      * GESTIONE F03 VIDEO D1
130700941206      ************************************************************
130800940309     C     F03D1         BEGSR
130900941206      *
131000940309     C                   MOVE      *ON           $FINE
131100941012    >C                   MOVE      '1'           RET008
131200941206      * fine programma
131300940325     C                   EXSR      ENDPGM
131400941206      *
131500940325     C                   ENDSR
131600941206     C/EJECT
131700941206      ************************************************************
131800941206      * GESTIONE F03 VIDEO D4
131900941206      ************************************************************
132000941206     C     F03D4         BEGSR
132100941206      *
132200941206     C                   MOVE      *ON           $FINE
132300941206    >C                   MOVE      '1'           RET008
132400941206      * fine programma
132500941206     C                   EXSR      ENDPGM
132600941206      *
132700941206     C                   ENDSR
132800940309     C/EJECT
132900941206      ************************************************************
133000941206      * GESTIONE F05 VIDEO D1
133100941206      ************************************************************
133200940309     C     F05D1         BEGSR
133300941206      *
133400940309     C                   MOVE      *ON           $INZD1
133500941206      *
133600940309     C                   ENDSR
133700940309     C/EJECT
133800941206      ************************************************************
133900941206      * GESTIONE F08 VIDEO D1
134000941206      ************************************************************
134100940309     C     F08D1         BEGSR
134200941206      *
134300940309     C                   MOVE      *ON           $FINE
134400941012    >C                   MOVE      '0'           RET008
134500941206      *
134600940309     C                   ENDSR
134700940309     C/EJECT
134800941206      ************************************************************
134900941206      * GESTIONE F12 VIDEO D1
135000941206      ************************************************************
135100940309     C     F12D1         BEGSR
135200941206      *
135300940309     C                   MOVE      *ON           $FINE
135400941012    >C                   MOVE      '2'           RET008
135500941206      *
135600940309     C                   ENDSR
135700940309     C/EJECT
135800941206      ************************************************************
135900941206      * GESTIONE F12 VIDEO D3
136000941206      ************************************************************
136100941206     C     F12D3         BEGSR
136200941206      *
136300950505     C                   MOVE      *ON           $FINE
136400950505    >C                   MOVE      '2'           RET008
136500941206      *
136600941206     C                   ENDSR
136700941206     C/EJECT
136800941206      ************************************************************
136900941206      * GESTIONE F12 VIDEO D4
137000941206      ************************************************************
137100941206     C     F12D4         BEGSR
137200941206      *
137300950505     C                   MOVE      *ON           $FINE
137400950505    >C                   MOVE      '2'           RET008
137500941206      *
137600941206     C                   ENDSR
137700941206     C/EJECT
137800941206      ************************************************************
137900941206      * GESTIONE F24 VIDEO D1
138000941206      ************************************************************
138100940309     C     F24D1         BEGSR
138200941206      *
138300941206      *
138400940506     C                   MOVEA     $FX1          $FX
138500940506     C                   MOVEA     $FC1          $FC
138600940506     C                   MOVEA     $FR1          $FR
138700940506     C                   Z-ADD     $ULKD1        $ULK
138800940506     C                   EXSR      RIEKEY
138900940506     C                   Z-ADD     $ULK          $ULKD1            3 0
139000940720     C                   MOVEA     $FC           $FC1
139100940506     C                   MOVEL     $KEY1         Z1KE1
139200060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
139300940506     C                   MOVEL     $KEY2         Z1KE2
139400940506     C                   WRITE     FMTZ1
139500941206      *
139600940309     C                   ENDSR
139700940207     C/EJECT
139800941206      ************************************************************
139900941206      * CONTROLLO VIDEATA
140000941206      ************************************************************
140100940127     C     CTRD1         BEGSR
140200941206      *
140300940127     C                   SETOFF                                       99
140400941206      * non imposto pos. del cursore
140500940127     C                   Z-ADD     0             H1RIGA
140600940127     C                   Z-ADD     0             H1COLO
140700000104      *
140800000104B0145 * Controllo sempre la data fine validità (anche in annullamento)
140900000104  "  C     D1DFV         IFNE      *ZERO
141000000104  "  C                   Z-ADD     D1DFV         XDTGMA                         *
141100000104  "  C                   CALLB     'XDT4000'
141200000104  "  C                   PARM                    XDTAMG           10
141300000104  "  C                   PARM                    XDTGMA            6 0
141400000104  "  C                   PARM      3             XDTSTA            1 0
141500000104  "  C     XDTSTA        IFNE      0                                            *
141600000104  "  C                   SETON                                          5199    *
141700000104  "  C                   ENDIF
141800000104B0145C                   ENDIF
141900941206      *
142000941206      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
142100941206      *
142200941214   1BC     OPZ008        IFNE      '04'
142300941206      *
142400941206    > *-------------------------------------------------------------
142500941206    > * inserire i controlli
142600941206    > * Accendere l'indicatore 99 per ogni errore
142700941206    > *-------------------------------------------------------------
142800961010    > * CTRL obbligatorietà codice macchina
142900961010    > * Se è blank propongo nomsys
143000961010     C     D1Mch         IFEQ      *BLANK
143100961010    >C                   Movel     NomSys        D1Mch
143200941213     C                   ENDIF
143300941213      *
143400961010    > * CTRL obbligatorietà società e decodifica
143500961010     C     D1SOC         IFEQ      *BLANK
143600961010    >C                   SETON                                        5099
143700961010     C                   ENDIF
143800970603      *
143900970603    > * CTRL obbligatorietà flag
144000001009B0278C***  D1SCF         IFNE      XScOff
144100001009 "   c***  D1Scf         ANDNE     XScOn
144200001009 "  >C***                SETON                                        7099
144300001010 "   C***                ENDIF
144400001010 "   C* Controllo società fittizia
144500001010 "   C                   MOVEL     *BLANK        D1ScfD
144600001010 "   C                   RESET                   X07DS
144700001010 "   C                   MOVE      '1'           X07RIC
144800001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
144900001010 "   C                   MOVEL     'DSPSINO'     X07CAM
145000001010 "   C                   MOVEL     D1SCF         X07VAL
145100001010 "   C                   CALLb     'X07VALR'
145200001010 "   C                   PARM                    X07DS
145300001010 "   C     X07ERR        IFEQ      '0'
145400001010 "   C                   MOVEL     X07DES        D1SCFD
145500001010 "   C                   ELSE
145600001010 "   C                   Seton                                        7099
145700001010b0278C                   ENDIF
145800961010      *
145900950615      * CTRL obbligatorietà moneta di conto
146000950615     C                   MOVE      *BLANK        D1DIVD
146100950615     C     D1DIV         IFEQ      *BLANK
146200950615     C                   SETON                                        6799
146300030103B0514 * se la divisa sulla società in manutenzione non esiste
146400030103  "   * il relativo nr decimali deve essere azzerato
146500030103B0514C                   move      *blank        D3SK06
146600950615     C                   ELSE
146700980915     C* Procedura controllo divisa
146800980916     C                   IF        X0201RpDVS(D1Div:DtRifDiv:
146900980916     C                                    ANDVS00F:%size(ANDVS00F):
147000980916     C                                    ANDVX00F:%size(ANDVX00F)) = 1
147100950615     C                   SETON                                          9980
147200030103B0514 * se la divisa sulla società in manutenzione non esiste
147300030103  "   * il relativo nr decimali deve essere azzerato
147400030103B0514C                   move      *blank        D3SK06
147500950615     C                   ELSE
147600980915     C                   MOVEL     DVSDesBrev    D1DIVD
147700030103B0514 * se si modifica la divisa sulla società in manutenzione
147800030103  "   * deve essere aggiornato il relativo nr decimali (campo protetto)
147900030103B0514C                   move      DVXNrDec      D3SK06
148000950615     C                   ENDIF
148100950615     C                   ENDIF
148200950615      * CTRL obbligatorietà capofila per anagrafiche
148300950615     C     D1CPA         IFEQ      *BLANK
148400950615     C                   SETON                                        6899
148500950615     C                   ELSE
148600941214     C                   MOVE      *BLANK        D1CPAD
148700941213     C* CTRL validità capofila per anagrafiche
148800941213     C                   MOVE      D1CPA         SIFSOC
148900950921     C     K1SIF1        CHAIN(N)  ANSIF01                            21
149000941214     C     *IN21         IFEQ      *ON
149100941214    >C     D1CPA         IFNE      D1SOC
149200941213     C                   SETON                                        7699
149300941214     C                   ENDIF
149400941214     C                   ELSE
149500941214     C                   MOVEL     SIFDSI        D1CPAD
149600941214     C                   ENDIF
149700941213    >C                   ENDIF
149800980220    > * CTRL obbligatorietà capofila di controllo
149900941213    >C     D1CPC         IFEQ      *BLANK
150000941213    >C                   SETON                                        6999
150100941213    >C                   ELSE
150200941214     C                   MOVE      *BLANK        D1CPCD
150300941213     C* CTRL validità capofila di controllo
150400941213     C                   MOVE      D1CPC         SIFSOC
150500950921     C     K1SIF1        CHAIN(N)  ANSIF01                            21
150600941214     C     *IN21         IFEQ      *ON
150700941214    >C     D1CPC         IFNE      D1SOC
150800941213     C                   SETON                                        7799
150900941214     C                   ENDIF
151000941214     C                   ELSE
151100941214     C                   MOVEL     SIFDSI        D1CPCD
151200941214     C                   ENDIF
151300941213    >C                   ENDIF
151400941213    > * CTRL obbligatorietà descrizione
151500941213    >C     D1DSI         IFEQ      *BLANK
151600941213    >C                   SETON                                        6599
151700941213    >C                   ENDIF
151800941213    > * CTRL obbligatorietà S.I
151900941213    >C     D1NSI         IFEQ      *BLANK
152000941213    >C                   SETON                                        6699
152100941213    >C                   ELSE
152200961010     C* Controllo di esistenza solo se cod.macchina=NomSys
152300961010     C     D1Mch         IFEQ      NomSys
152400941213     C                   MOVEL     D1NSI         KNMSI
152500941213     C     KNMSI         CHAIN     KFSIF01L                           21
152600941213     C     *IN21         IFEQ      *ON
152700941213     C                   SETON                                        7899
152800941213     C                   ELSE
152900941213     C                   MOVEL     KDSSI         D1NSID
153000941213     C                   ENDIF
153100961010     C                   ENDIF
153200941213    >C                   ENDIF
153300941213    > * CTRL date
153400000104B0145C**** D1DFV         IFNE      *ZERO
153500000104  "  C****               Z-ADD     D1DFV         XDTGMA                         *
153600000104  "  C****               CALLB     'XDT4000'
153700000104  "  C****               PARM                    XDTAMG           10
153800000104  "  C****               PARM                    XDTGMA            6 0
153900000104  "  C****               PARM      3             XDTSTA            1 0
154000000104  "  C**** XDTSTA        IFNE      0                                            *
154100000104  "  C****               SETON                                          5199    *
154200000104  "  C****               ENDIF
154300000104B0145C****               ENDIF
154400941206      *
154500941201     C     D1DAB         IFNE      *ZERO
154600941201     C                   Z-ADD     D1DAB         XDTGMA                         *
154700960930     C                   CALLB     'XDT4000'
154800941201     C                   PARM                    XDTAMG           10
154900941201     C                   PARM                    XDTGMA            6 0
155000941201     C                   PARM      3             XDTSTA            1 0
155100941201 4   C     XDTSTA        IFNE      0                                            *
155200941201     C                   SETON                                          5299
155300941201     C                   ENDIF
155400941201     C                   ENDIF
155500941206      *
155600941201     C     D1DSA         IFNE      *ZERO
155700941201     C                   Z-ADD     D1DSA         XDTGMA                         *
155800960930     C                   CALLB     'XDT4000'
155900941201     C                   PARM                    XDTAMG           10
156000941201     C                   PARM                    XDTGMA            6 0
156100941201     C                   PARM      3             XDTSTA            1 0
156200941213 4   C     XDTSTA        IFNE      0                                            *
156300941201     C                   SETON                                          5399    *
156400941201     C                   ENDIF
156500941201     C                   ENDIF
156600941212     C* CTRL VALIDITA' STATO
156700941212     C     D1STA         IFNE      *BLANK
156800941213     C                   MOVEL     *BLANK        D1STAD
156900941212     C                   MOVE      '1'           XTBRIC
157000941212     C                   MOVE      XSCSOC        XTBAZI
157100941212     C                   MOVE      *ZERO         XTBKEY
157200941212     C                   MOVE      'G12'         XTBCOD
157300941212     C                   MOVE      D1STA         XTBKEY
157400960930     C                   CALLB     'XATB'
157500941212     C                   PARM                    XATBDS
157600941212     C     XTBERR        IFEQ      '0'
157700941213     C                   MOVEL     XTBUNI        G12DS
157800941213     C                   MOVEL     DESG12        D1STAD
157900941212     C*
158000941212     C*
158100941212     C                   ELSE
158200941213     C                   SETON                                        5599
158300941212     C                   ENDIF
158400941212     C                   ELSE
158500941212     C                   SETON                                        5699
158600941212     C                   ENDIF
158700941213      *
158800941213     C* CTRL OBBLIGATORIETA' LINGUA
158900941213     C     D1LIN         IFEQ      *BLANK
159000941213     C                   SETON                                        5799
159100941213     C*
159200941213     C* CTRL VALIDITA' LINGUA
159300941213     C                   ELSE
159400941213     C                   MOVEL     *BLANK        D1LIND
159500941213     C                   MOVE      '1'           XTBRIC
159600941213     C                   MOVE      XSCSOC        XTBAZI
159700941213     C                   MOVE      *ZERO         XTBKEY
159800941213     C                   MOVE      'G15'         XTBCOD
159900941213     C                   MOVE      D1LIN         XTBKEY
160000960930     C                   CALLB     'XATB'
160100941213     C                   PARM                    XATBDS
160200941213     C     XTBERR        IFEQ      '0'
160300941213     C                   MOVEL     XTBUNI        G15DS
160400941213     C                   MOVEL     DESG15        D1LIND
160500941213     C                   ELSE
160600941213     C                   SETON                                        5899
160700941213     C                   ENDIF
160800941213     C                   ENDIF
160900941213     C*
161000941213     C* CTRL OBBLIGATORIETA' CODICE FISCALE
161100941213     C***        D1CDF     IFEQ *BLANK
161200941213     C****                 SETON                     7499
161300941213     C*
161400941213     C* CTRL VALIDITA' COD. FISCALE
161500941213     C*****                ELSE
161600941213     C*                    CALL 'XCDFIS'
161700941213     C*                    PARM           D1CDF
161800941213     C*                    PARM           XSTATO  10
161900941213     C*          XSTATO    IFLT 0
162000941213     C*                    SETON                     6099
162100941213     C*                    ENDIF
162200941213     C********             ENDIF
162300970603CA+  C*
162400970603CA+  C* Viene fatto il controllo della partita iva solo DOPO che è stato
162500970603CA+  C* impostato D3SK02 (Legislazione) nella terza videta e se questo è
162600970603CA+  C* = 1 (Legislazione italiana)
162700970603CA+  C*
162800970603CA+  C                   IF        D3Sk02 = '1'
162900970603CA+  C*
163000970603     C* CTRL OBBLIGATORIETA' PARTITA IVA
163100970603     C     D1IVA         IFEQ      *BLANK
163200970603     C                   SETON                                        6199
163300970603     C*
163400970603     C* CTRL VALIDITA' PARTITA IVA
163500970603     C                   ELSE
163600020201B0334X***                CALL      'XPARIVA'
163700020201B0334C                   CALLB     'XPARIVA'
163800970603     C                   PARM                    D1IVA
163900970603     C                   PARM                    XSTATO            1 0
164000970603     C                   PARM                    XScsoc
164100970603     C     XSTATO        IFLT      0
164200980220     C                   SETON                                        62
164300970603     C                   ENDIF
164400970603     C                   ENDIF
164500970603CA+  C                   ENDIF
164600941213     C*
164700941213     C*
164800941213     C* CTRL OBBLIGATORIETA' NATURA GIURIDICA
164900941213     C     D1NGR         IFEQ      *BLANK
165000941213     C                   SETON                                        6399
165100941213     C*
165200941213     C* CTRL VALIDITA' NATURA GIURIDICA
165300941213     C                   ELSE
165400941213     C                   MOVEL     *BLANK        D1NGRD
165500941213     C                   MOVE      '1'           XTBRIC
165600941213     C                   MOVE      XSCSOC        XTBAZI
165700941213     C                   MOVE      *ZERO         XTBKEY
165800941213     C                   MOVE      'G16'         XTBCOD
165900941213     C                   MOVE      D1NGR         XTBKEY
166000960930     C                   CALLB     'XATB'
166100941213     C                   PARM                    XATBDS
166200941213     C     XTBERR        IFEQ      '0'
166300941213     C                   MOVEL     XTBUNI        G16DS
166400941213     C                   MOVEL     DE1G16        D1NGRD
166500941213     C                   ELSE
166600941213     C                   SETON                                        6499
166700941213     C                   ENDIF
166800941213     C                   ENDIF
166900941206      *
167000941206      *
167100941206      * CTRL VALIDITA' PACKAGE
167200941206      *
167300941206      * CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
167400941214   2BC     OPZ008        IFEQ      '01'
167500941012    >C     OPZ008        OREQ      '03'
167600941206    > *-------------------------------------------------------------
167700941206    > * impostare i campi chiave e verificare l'esistenza del record
167800941206    > *-------------------------------------------------------------
167900941012    >C                   MOVEL     D1SOC         SIFSOC
168000941213    >C     K1SIF1        SETLL     ANSIF01                                21
168100940224     C     *IN21         IFEQ      '1'
168200940223     C                   SETON                                        5999
168300940224     C                   ENDIF
168400941214   2EC                   ENDIF
168500941206      *
168600941214   1EC                   ENDIF
168700941206      *
168800940117     C                   ENDSR
168900940207     C/EJECT
169000941206      ************************************************************
169100941206      * GESTIONE VIDEO RECORD  D2
169200941206      ************************************************************
169300941206    >C     GESD2         BEGSR
169400941206      * inizializzazione videata
169500941206     C     $INZD1        IFEQ      *ON
169600941206     C                   EXSR      INZD1
169700941206     C                   MOVE      *OFF          $INZD1
169800941206     C                   END
169900941206      * emissione piede videata se proveniente da altra
170000941206      * salvataggio valore formato di provenienza
170100941206     C     $LASTV        IFNE      'D2'
170200941206     C                   WRITE     FMTT1
170300941206     C                   EXSR      GESPIE
170400941206     C                   MOVE      $LASTV        $LASTG
170500941206     C                   MOVE      'D2'          $LASTV
170600941213     C                   END
170700941206      *
170800941206      * se esistono errori sulla videata
170900941206      * emetto la write del formato a indicatori spenti per vedere
171000941206      * le eventuali decodifiche
171100941206     C     *IN99         IFEQ      *ON
171200941206     C     $GEST         ANDEQ     'D2'
171300941206     C                   MOVEA     *IN           WIN
171400941206     C                   MOVE      *ALL'0'       IN5098           49
171500941206     C                   MOVEA     IN5098        *IN(50)
171600941206     C                   WRITE     FMTD2
171700941206     C                   MOVEA     WIN           *IN
171800941206     C                   ENDIF
171900941206      *
172000941206     C                   SELECT
172100941206      * Annullamento
172200941206     C     OPZ008        WHENEQ    '04'
172300941206      * Visualizzazione
172400941206     C     OPZ008        OREQ      '05'
172500941206     C                   WRITE     FMTD2
172600941206     C                   EXFMT     PROTECT
172700941206      * Immissione/Modifica/Copia
172800941206     C                   OTHER
172900941206     C                   EXFMT     FMTD2
173000941206     C                   ENDSL
173100941206      * controllo abilitazione tasti funzione
173200941206      *
173300941206     C                   EXSR      CTRKD1
173400941206      *
173500941206     C     *IN99         IFEQ      *OFF
173600941206      *
1737009412061    C                   SELECT
173800941206      * ROLDOWN
173900941206     C     $TASTO        WHENEQ    ROLDWN
174000941206     C                   MOVE      'D1'          $GEST
174100941206      * ROLLUP
174200941206     C     $TASTO        WHENEQ    ROLLUP
174300941206     C                   MOVE      'D3'          $GEST
174400941206      * F3=Fine
174500941206     C     $TASTO        WHENEQ    F03
174600941206     C                   EXSR      F03D2
174700941206      * F4=Ricerca
174800941206     C     $TASTO        WHENEQ    F04
174900941206     C                   EXSR      SEARCH
175000941206      * F5=Refresh
175100941206     C     $TASTO        WHENEQ    F05
175200941206     C                   EXSR      F05D2
175300941206      * F8=Successivo
175400941206     C     $TASTO        WHENEQ    F08
175500941206     C                   EXSR      F08D2
175600941206      * F12=Ritorno
175700941206     C     $TASTO        WHENEQ    F12
175800941206     C                   EXSR      F12D2
175900941206      * F24=Altri tasti
176000941206     C     $TASTO        WHENEQ    F24
176100941206     C                   EXSR      F24D2
176200941206      *
1763009412061O   C                   OTHER
176400941206     C                   EXSR      CTRD2
176500941206     C     *IN99         IFEQ      *OFF
176600941206     C                   EXSR      CTRD1
176700941213     C     *IN99         IFEQ      *ON
176800970603     C                   MOVE      *ALL'0'       IN5098
176900970603     C                   MOVEA     IN5098        *IN(50)
177000970603     C****               MOVEA     *ALL'0'       *IN(50)
177100970603     C                   SETON                                            96
177200941213     C                   ELSE
177300941206     C                   EXSR      CTRD3
177400941213     C     *IN99         IFEQ      *ON
177500970603     C                   MOVE      *ALL'0'       IN5098
177600970603     C                   MOVEA     IN5098        *IN(50)
177700970603     C****               MOVEA     *ALL'0'       *IN(50)
177800970603     C                   SETON                                            96
177900941213     C                   ELSE
178000941206     C                   EXSR      CTRD4
178100941206     C     *IN99         IFEQ      *ON
178200970603     C                   MOVE      *ALL'0'       IN5098
178300970603     C                   MOVEA     IN5098        *IN(50)
178400970603     C****               MOVEA     *ALL'0'       *IN(50)
178500970603     C                   SETON                                            96
178600001004B0273C                   ELSE
178700001004  "  C                   EXSR      CTRD5
178800001004  "  C     *IN99         IFEQ      *ON
178900001004  "  C                   MOVE      *ALL'0'       IN5098
179000001004  "  C                   MOVEA     IN5098        *IN(50)
179100001004  "  C                   SETON                                            96
179200001004B0273C                   ENDIF
179300941206     C                   ENDIF
179400941213     C                   ENDIF
179500941213     C                   ENDIF
179600941206     C                   ENDIF
179700941206      *
179800941206      *
1799009412062    C     $TASTO        IFEQ      F06
180000941206     C     *IN99         ANDEQ     *OFF
180100980811      * Avverto utente: le modifiche sono operative dal prossimo collegament
180200980811     C                   EXSR      AvvertoUte
180300941206      * eseguo aggiornamento
180400941206     C                   EXSR      AGGANA
180500941206      * eseguo operazioni del dopo-aggiornamento
180600941206     C                   EXSR      GESAGG
1807009412062-   C                   ENDIF
180800941206      *
1809009412061-   C                   ENDSL
181000941206      *
1811009412062-   C                   ENDIF
181200941206    >C                   ENDSR
181300941206     C/EJECT
181400941206      ************************************************************
181500941206      * RIEMPIMENTO VIDEATA D2
181600941206      ************************************************************
181700941206    >C     RIED2         BEGSR
181800941206      *
181900941206    >C                   MOVEL     SIFRGS        D2RGS
182000100505R002 C                   MOVEL     SIFSogg       D2Sogg
182100941206    >C                   MOVEL     SIFVIA        D2VIA
182200941206    >C                   MOVEL     SIFCIT        D2CIT
182300941206    >C                   MOVEL     SIFCAP        D2CAP
182400941206    >C                   MOVEL     SIFPRV        D2PRV
182500941206    >C                   MOVEL     SIFTEL        D2TEL
182600970807    >C                   MOVEL     SIFFax        D2Fax
182700941206    >C                   MOVEL     SIFTLX        D2TLX
182800970807    >C                   MOVEL     SIFTLS        D2TLS
182900941206    >C                   MOVEL     SIFVSC        D2VSC
183000941206    >C                   MOVEL     SIFCSC        D2CSC
183100941206    >C                   MOVEL     SIFCAS        D2CAS
183200941206    >C                   MOVEL     SIFPRS        D2PRS
183300941206    >C                   MOVEL     SIFCTI        D2CTI
183400941206    >C                   MOVEL     SIFCCT        D2CCT
183500941206    >C                   MOVEL     SIFATT        D2ATT
183600941206    >C                   MOVEL     SIFESA        D2ESA
183700941206    >C                   MOVEL     SIFCDC        D2CDC
183800941206      *
183900941206    >C                   MOVEL     SIFDTI        XDTAMG
184000960930     C                   CALLB     'XDT4000'
184100941206     C                   PARM                    XDTAMG           10
184200941206     C                   PARM                    XDTGMA            6 0
184300941206     C                   PARM      2             XDTSTA            1 0
184400941206     C     XDTSTA        IFEQ      0                                            *
184500941206     C                   MOVE      XDTGMA        D2DTI                          *
184600941206     C                   ENDIF                                                  *
184700941206      *
184800941206    >C                   ENDSR
184900941206     C/EJECT
185000941206      ************************************************************
185100941206      * GESTIONE F02 VIDEO D2
185200941206      ************************************************************
185300941206     C     F02D2         BEGSR
185400941206      *
185500941206      * per decodificare la videata richiamo la routine dei ctrl
185600941206      * e poi disattivo gli eventuali messaggi di errore
185700941206     C                   EXSR      CTRD2
185800941206      *
185900941206     C                   MOVE      *ALL'0'       IN5098
186000941206     C                   MOVEA     IN5098        *IN(50)
186100941206      *
186200941206     C                   ENDSR
186300941206     C/EJECT
186400941206      ************************************************************
186500941206      * GESTIONE F03 VIDEO D2
186600941206      ************************************************************
186700941206     C     F03D2         BEGSR
186800941206      *
186900941206     C                   MOVE      *ON           $FINE
187000941206    >C                   MOVE      '1'           RET008
187100941206      * fine programma
187200941206     C                   EXSR      ENDPGM
187300941206      *
187400941206     C                   ENDSR
187500941206     C/EJECT
187600941206      ************************************************************
187700940510     C/EJECT
187800941206      ************************************************************
187900941206      * GESTIONE F05 VIDEO D2
188000941206      * il tasto F5 su qualsiasi videata provoca il refresh di tutte
188100941206      * e il riposizionamento alla prima
188200941206      ************************************************************
188300941206    >C     F05D2         BEGSR
188400941206      *
188500941206     C                   MOVE      *ON           $INZD1
188600941206     C                   MOVE      'D2'          $GEST
188700941206      *
188800941206    >C                   ENDSR
188900941206     C/EJECT
189000941206      ************************************************************
189100941206      * GESTIONE F08 VIDEO D2
189200941206      ************************************************************
189300941206     C     F08D2         BEGSR
189400941206      *
189500941206     C                   MOVE      *ON           $FINE
189600941206    >C                   MOVE      '0'           RET008
189700941206      *
189800941206     C                   ENDSR
189900941206     C/EJECT
190000941206      ************************************************************
190100941206      * GESTIONE F08 VIDEO D3
190200941206      ************************************************************
190300941206     C     F08D3         BEGSR
190400941206      *
190500941206     C                   MOVE      *ON           $FINE
190600941206    >C                   MOVE      '0'           RET008
190700941206      *
190800941206     C                   ENDSR
190900941206     C/EJECT
191000941206      ************************************************************
191100941206      * GESTIONE F08 VIDEO D4
191200941206      ************************************************************
191300941206     C     F08D4         BEGSR
191400941206      *
191500941206     C                   MOVE      *ON           $FINE
191600941206    >C                   MOVE      '0'           RET008
191700941206      *
191800941206     C                   ENDSR
191900941206     C/EJECT
192000941206      ************************************************************
192100941206      * GESTIONE F12 VIDEO D2
192200941206      ************************************************************
192300941206     C     F12D2         BEGSR
192400941206      *
192500950505     C                   MOVE      *ON           $FINE
192600950505    >C                   MOVE      '2'           RET008
192700941206      *
192800941206     C                   ENDSR
192900941206      ************************************************************
193000941206      * GESTIONE F24 VIDEO D2
193100941206      ************************************************************
193200941206    >C     F24D2         BEGSR
193300941206      *
193400941206      *
193500941206     C                   MOVEA     $FX1          $FX
193600941206     C                   MOVEA     $FC1          $FC
193700941206     C                   MOVEA     $FR1          $FR
193800941206     C                   Z-ADD     $ULKD1        $ULK
193900941206     C                   EXSR      RIEKEY
194000941206     C                   Z-ADD     $ULK          $ULKD1            3 0
194100941206     C                   MOVEA     $FC           $FC1
194200941206     C                   MOVEL     $KEY1         Z1KE1
194300060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
194400941206     C                   MOVEL     $KEY2         Z1KE2
194500941206     C                   WRITE     FMTZ1
194600941206    >C                   ENDSR
194700941206      ************************************************************
194800941206      * CONTROLLO VIDEATA  D2
194900941206      ************************************************************
195000941206    >C     CTRD2         BEGSR
195100941206      *
195200941206     C                   SETOFF                                       99
195300941206      * non imposto pos. del cursore
195400941206     C                   Z-ADD     0             H1RIGA
195500941206     C                   Z-ADD     0             H1COLO
195600941206      *
195700941206      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
195800941206      *
195900941213   1BC     OPZ008        IFNE      '04'
196000941206      *
196100941206    > * INSERIRE I CONTROLLI SPECIFICI DELLA VIDEATA
196200970603      * utilizzare anche indicatori uguali nelle diverse videate
196300941213     C*
196400941213   2BC     D2RGS         IFEQ      *BLANK
196500941213     C                   SETON                                        7999
196600941214   2EC                   ENDIF
196700941213      *
196800100505R002 C* controllo validità codice soggetto collegato
196900100505R002 C                   EXSR      CtrSogg
197000941206      *
197100941213     C* CTRL VALIDITA' CAP
197200941213   2BC     D1STA         IFNE      *BLANK
197300941213     C                   MOVE      '1'           XTBRIC
197400941213     C                   MOVE      XSCSOC        XTBAZI
197500941213     C                   MOVE      *ZERO         XTBKEY
197600941213     C                   MOVE      'G12'         XTBCOD
197700941213     C                   MOVE      D1STA         XTBKEY
197800960930     C                   CALLB     'XATB'
197900941213     C                   PARM                    XATBDS
198000941213   3BC     XTBERR        IFEQ      '0'
198100941213     C     D2CAP         ANDNE     *BLANK
198200941213     C*
198300941213     C* CTRL NR.CARATTERI DEL D2CAP
198400941213     C     ' '           CHECKR    D2CAP         X
198500941213   4BC     X             IFNE      CAPG12
198600941213     C                   SETON                                        7099
198700941213   4EC                   ENDIF
198800941213     C* CTRL NR.CARATTERI DEL D2CAS
198900941213     C     ' '           CHECKR    D2CAS         X
199000941213   4BC     X             IFNE      CAPG12
199100941213     C     D2CAS         ANDNE     *BLANK
199200941213     C                   SETON                                        7199
199300941213   4EC                   ENDIF
199400941213     C*
199500941213   3EC                   ENDIF
199600941213   2EC                   ENDIF
199700941213     C* CTRL VALIDITA' CAP
199800941213   2BC     D2PRV         IFNE      *BLANK
199900941213     C                   MOVE      '1'           XTBRIC
200000941213     C                   MOVE      XSCSOC        XTBAZI
200100941213     C                   MOVE      *ZERO         XTBKEY
200200941213     C                   MOVE      'C17'         XTBCOD
200300941213     C                   MOVE      D2PRV         WRK06A            6
200400941213     C                   MOVEL     D1STA         WRK06A
200500941213     C                   MOVE      WRK06A        XTBKEY
200600960930     C                   CALLB     'XATB'
200700941213     C                   PARM                    XATBDS
200800941213   3BC     XTBERR        IFEQ      '0'
200900941213     C                   MOVEL     XTBUNI        C17DS
201000941213   3XC                   ELSE
201100941213     C                   SETON                                        7299
201200941213   3EC                   ENDIF
201300941213   2EC                   ENDIF
201400941206      *
201500941206      *
201600941213     C* CTRL VALIDITA' PROVINCIA
201700941213   2BC     D2PRS         IFNE      *BLANK
201800941213     C                   MOVE      '1'           XTBRIC
201900941213     C                   MOVE      XSCSOC        XTBAZI
202000941213     C                   MOVE      *ZERO         XTBKEY
202100941213     C                   MOVE      'C17'         XTBCOD
202200941213     C                   MOVE      D2PRS         WRK06A            6
202300941213     C                   MOVEL     D1STA         WRK06A
202400941213     C                   MOVE      WRK06A        XTBKEY
202500960930     C                   CALLB     'XATB'
202600941213     C                   PARM                    XATBDS
202700941213   3BC     XTBERR        IFEQ      '0'
202800941213     C                   MOVEL     XTBUNI        C17DS
202900941213     C                   ELSE
203000941213     C                   SETON                                        7399
203100941213   3EC                   ENDIF
203200941213   2EC                   ENDIF
203300941213    > * CTRL date
203400941213     C     D2DTI         IFNE      *ZERO
203500941213     C                   Z-ADD     D2DTI         XDTGMA                         *
203600960930     C                   CALLB     'XDT4000'
203700941213     C                   PARM                    XDTAMG           10
203800941213     C                   PARM                    XDTGMA            6 0
203900941213     C                   PARM      3             XDTSTA            1 0
204000941213 4   C     XDTSTA        IFNE      0                                            *
204100941213     C                   SETON                                          7599    *
204200941213     C                   ENDIF
204300941213     C                   ENDIF
204400941213      *
204500941213   1EC                   END
204600941213      *
204700941206     C                   ENDSR
204800941206     C/EJECT
204900100505R002 C************************************************************
205000080306  "  C* Controllo validità codice soggetto collegato
205100080306  "  C************************************************************
205200100505R002 C     CtrSogg       BEGSR
205300080306     C*
205400080306     C                   Clear                   D2SoggD                        *
205500080306     C*
205600080306     C                   if        D2Sogg     <> *Blank
205700080306     C                   CALLB     'XALLINEA'
205800080306     C                   Parm                    D2Sogg
205900080306     C                   Parm      8             $Len              5 0
206000080306     C                   Parm                    XScInp
206100080306     C*
206200080306     C                   EXSR      CallDvaSog
206300080306     C                   if        DVAErrore  <> '1'
206400080306     C*
206500080306     C                   Eval      %subst(DVASOG:1:DVALENOUT)
206600080306     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
206700080306     C*
206800080306     C                   MoveL     DVSDes        D2SoggD                        *
206900080306     C*
207000080306     C                   else
207100080306     C*
207200080306     C                   SetOn                                        7699
207300080306     C*
207400080306     C                   endif
207500080306     C*
207600080306     C                   endif
207700080306     C*
207800100505R002 C                   ENDSR
207900100505R002 C/EJECT
208000100505R002 C************************************************************
208100080306  "  C* Chiamata al driver dei soggetti
208200080306  "  C************************************************************
208300100505R002 C     CallDvaSog    BEGSR
208400080306     C*
208500080306     C                   EVAL      DVALenOut = %Size(DVASOG)
208600080306     C*
208700080306     C                   CALLB     'NDDVASOG'
208800080306     C                   PARM      '1'           DVARIC
208900080306     C                   PARM      D1Soc         DVASOCIETA
209000080306     C                   PARM                    DVAUNITA
209100080306     C                   PARM      'DVASOG    '  DVASTRUTT
209200080306     C                   PARM                    DVADTRIF
209300080306     C                   PARM                    DVALENOUT
209400080306     C* Se si accede con il codice cli/for impostare i campi seguenti
209500080306     C* se si accede con il soggetto ometterli e impostare gli altri
209600080306     C*                  PARM                    DVASNATURA
209700080306     C*                  PARM                    DVACODICE
209800080306     C*                  PARM                    DVALINEAV
209900080306     C*                  PARM                    DVAFILIALE
210000080306     C                   PARM                    *omit
210100080306     C                   PARM                    *omit
210200080306     C                   PARM                    *omit
210300080306     C                   PARM                    *omit
210400080306     C                   PARM      D2Sogg        DVSSOGG
210500080306     C                   PARM      *Blank        DVSTPIND
210600080306     C                   PARM      *Blank        DVSCDIND
210700080306     C                   PARM                    DVAERRORE
210800080306     C                   PARM                    DVAOUTPUT
210900080306     C*
211000100505R002 C                   ENDSR
211100100505R002  /EJECT
211200941206      ************************************************************
211300941206      * GESTIONE VIDEO RECORD  D3
211400941206      ************************************************************
211500941206    >C     GESD3         BEGSR
211600941206      * inizializzazione videata
211700941206     C     $INZD1        IFEQ      *ON
211800941206     C                   EXSR      INZD1
211900941206     C                   MOVE      *OFF          $INZD1
212000941206     C                   END
212100941206      * emissione piede videata se proveniente da altra
212200941206      * salvataggio valore formato di provenienza
212300941206     C     $LASTV        IFNE      'D3'
212400941206     C                   WRITE     FMTT1
212500941206     C                   EXSR      GESPIE
212600941206     C                   MOVE      $LASTV        $LASTG
212700941206     C                   MOVE      'D3'          $LASTV
212800941206     C                   END
212900941206      *
213000941206      * se esistono errori sulla videata
213100941206      * emetto la write del formato a indicatori spenti per vedere
213200941206      * le eventuali decodifiche
213300941206     C     *IN99         IFEQ      *ON
213400941206     C     $GEST         ANDEQ     'D3'
213500941206     C                   MOVEA     *IN           WIN
213600941206     C                   MOVE      *ALL'0'       IN5098           49
213700941206     C                   MOVEA     IN5098        *IN(50)
213800941206     C                   WRITE     FMTD3
213900941206     C                   MOVEA     WIN           *IN
214000941206     C                   ENDIF
214100941206      *
214200941206     C                   SELECT
214300941206      * Annullamento
214400941206     C     OPZ008        WHENEQ    '04'
214500941212     C                   EXFMT     FMTD3
214600941206      * Visualizzazione
214700941212     C     OPZ008        WHENEQ    '05'
214800941206     C                   WRITE     FMTD3
214900941206     C                   EXFMT     PROTECT
215000941206      * Immissione/Modifica/Copia
215100941206     C                   OTHER
215200941206     C                   EXFMT     FMTD3
215300941206     C                   ENDSL
215400941206      * controllo abilitazione tasti funzione
215500941206      *
215600941206     C                   EXSR      CTRKD1
215700941206      *
215800941206     C     *IN99         IFEQ      *OFF
215900941206      *
2160009412061    C                   SELECT
216100941206      * ROLDOWN
216200941206     C     $TASTO        WHENEQ    ROLDWN
216300941206     C                   MOVE      'D2'          $GEST
216400941206      * ROLLUP
216500941206     C     $TASTO        WHENEQ    ROLLUP
216600941206     C                   MOVE      'D4'          $GEST
216700941206      * F3=Fine
216800941206     C     $TASTO        WHENEQ    F03
216900941206     C                   EXSR      F03D3
217000941206      * F4=Ricerca
217100941206     C     $TASTO        WHENEQ    F04
217200941206     C                   EXSR      SEARCH
217300941206      * F5=Refresh
217400941206     C     $TASTO        WHENEQ    F05
217500941206     C                   EXSR      F05D3
217600941206      * F8=Successivo
217700941206     C     $TASTO        WHENEQ    F08
217800941206     C                   EXSR      F08D3
217900941206      * F12=Ritorno
218000941206     C     $TASTO        WHENEQ    F12
218100941206     C                   EXSR      F12D3
218200941206      * F24=Altri tasti
218300941206     C     $TASTO        WHENEQ    F24
218400941206     C                   EXSR      F24D3
218500941206      *
2186009412061O   C                   OTHER
218700941206     C                   EXSR      CTRD3
218800941206     C     *IN99         IFEQ      *OFF
218900941206     C                   EXSR      CTRD1
219000941213     C     *IN99         IFEQ      *ON
219100970603     C                   MOVE      *ALL'0'       IN5098
219200970603     C                   MOVEA     IN5098        *IN(50)
219300970603     C****               MOVEA     *ALL'0'       *IN(50)
219400970603     C                   SETON                                            96
219500941213     C                   ELSE
219600941206     C                   EXSR      CTRD2
219700941213     C     *IN99         IFEQ      *ON
219800970603     C                   MOVE      *ALL'0'       IN5098
219900970603     C                   MOVEA     IN5098        *IN(50)
220000970603     C****               MOVEA     *ALL'0'       *IN(50)
220100970603     C                   SETON                                            96
220200941213     C                   ELSE
220300941213     C                   EXSR      CTRD4
220400941206     C     *IN99         IFEQ      *ON
220500970603     C                   MOVE      *ALL'0'       IN5098
220600970603     C                   MOVEA     IN5098        *IN(50)
220700970603     C****               MOVEA     *ALL'0'       *IN(50)
220800970603     C                   SETON                                            96
220900001004B0273C                   ELSE
221000001004  "  C                   EXSR      CTRD5
221100001004  "  C     *IN99         IFEQ      *ON
221200001004  "  C                   MOVE      *ALL'0'       IN5098
221300001004  "  C                   MOVEA     IN5098        *IN(50)
221400001004  "  C                   SETON                                            96
221500001004B0273C                   ENDIF
221600941206     C                   ENDIF
221700941206     C                   ENDIF
221800941213     C                   ENDIF
221900941213     C                   ENDIF
222000941206      *
222100941206      *
2222009412062    C     $TASTO        IFEQ      F06
222300941206     C     *IN99         ANDEQ     *OFF
222400980811      * Avverto utente: le modifiche sono operative dal prossimo collegament
222500980811     C                   EXSR      AvvertoUte
222600941206      * eseguo aggiornamento
222700941206     C                   EXSR      AGGANA
222800941206      * eseguo operazioni del dopo-aggiornamento
222900941206     C                   EXSR      GESAGG
2230009412062-   C                   ENDIF
223100941206      *
2232009412061-   C                   ENDSL
223300941206      *
2234009412062-   C                   ENDIF
223500941206    >C                   ENDSR
223600941206      ************************************************************
223700941206      * CONTROLLO VIDEATA  D3
223800941206      ************************************************************
223900941206    >C     CTRD3         BEGSR
224000941206      *
224100941206     C                   SETOFF                                       99
224200941206      * non imposto pos. del cursore
224300941206     C                   Z-ADD     0             H1RIGA
224400941206     C                   Z-ADD     0             H1COLO
224500941206      *
224600941206      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
224700941206      *
2248009412061    C     OPZ008        IFNE      '04'
224900941206      *
225000941206    > * INSERIRE I CONTROLLI SPECIFICI DELLA VIDEATA
225100970603      * utilizzare anche indicatori uguali nelle diverse videate
225200010213a0865C*
225300010213  "  C*Controllo obbligatorietà per 'Valore a video per SI-NO'
225400010213  "  C                   If        D3On = *blanks
225500010213  "  C                   seton                                        7199
225600010213  "  C                   EndIf
225700010213  "  C                   If        D3Off= *blanks
225800010213  "  C                   seton                                        7299
225900010213a0865C                   EndIf
226000980324     C****
226100980324    >C**CTRL obbligatorietà flag per tesoreria
226200980324     C**** D3Sk01        IFne      '0'
226300980324     C**** D3Sk01        andne     '1'
226400980324    >C****               SetOn                                        5099
226500980324     C****               ENDIF
226600980324     C*
226700980324     C* Controllo Tesoreria
226800980324     C                   MOVEL     *BLANK        D3Sk01D
226900980324     C                   RESET                   X07DS
227000980324     C                   MOVE      '1'           X07RIC
227100980324     C                   MOVEL     'ANDIZ000'    X07TRC
227200980324     C                   MOVEL     'DSPTESOR'    X07CAM
227300980324     C                   MOVEL     D3Sk01        X07VAL
227400980324     C                   CALLb     'X07VALR'
227500980324     C                   PARM                    X07DS
227600980324   B3C     X07ERR        IFEQ      '0'
227700980324     C                   MOVEL     X07DES        D3Sk01D
227800980324   X3C                   ELSE
227900980324     C                   Seton                                        5099
228000980324   E3C                   ENDIF
228100970603     C*
228200961016     C* Controllo Legislazione
228300961016     C                   MOVEL     *BLANK        D3Sk02D
228400961016     C                   RESET                   X07DS
228500961016     C                   MOVE      '1'           X07RIC
228600961016     C                   MOVEL     'ANDIZ000'    X07TRC
228700961016     C                   MOVEL     'DSPLEGISLA'  X07CAM
228800961016     C                   MOVEL     D3Sk02        X07VAL
228900961016     C                   CALLb     'X07VALR'
229000961016     C                   PARM                    X07DS
229100961016   B3C     X07ERR        IFEQ      '0'
229200961016     C                   MOVEL     X07DES        D3Sk02D
229300961016   X3C                   ELSE
229400961016     C                   Seton                                        5499
229500961016   E3C                   ENDIF
229600970603     C*
229700970603     C* Controllo tipo software gestionale
229800970603     C                   MOVEL     *BLANK        D3Sk03D
229900970603     C                   RESET                   X07DS
230000970603     C                   MOVE      '1'           X07RIC
230100970603     C                   MOVEL     'ANDIZ000'    X07TRC
230200970603     C                   MOVEL     'DSPTPSWGES'  X07CAM
230300970603     C                   MOVEL     D3Sk03        X07VAL
230400970603     C                   CALLb     'X07VALR'
230500970603     C                   PARM                    X07DS
230600970603   B3C     X07ERR        IFEQ      '0'
230700970603     C                   MOVEL     X07DES        D3Sk03D
230800970603   X3C                   ELSE
230900970603     C                   Seton                                        8999
231000970603   E3C                   ENDIF
231100980116     C*
231200980116     C* Controllo allineamento campi input
231300980116     C                   MOVEL     *BLANK        D3InpD
231400980116     C                   RESET                   X07DS
231500980116     C                   MOVE      '1'           X07RIC
231600980116     C                   MOVEL     'ANSIF000'    X07TRC
231700980116     C                   MOVEL     'SIFINPUT'    X07CAM
231800980116     C                   MOVE      *ON           X07ALL
231900980116     C                   MOVEL     D3Inp         X07VAL
232000980116     C                   CALLb     'X07VALR'
232100980116     C                   PARM                    X07DS
232200980116   B3C     X07ERR        IFEQ      '0'
232300980116     C                   MOVEL     X07DES        D3InpD
232400980116   X3C                   ELSE
232500980116     C                   Seton                                        8499
232600980116   E3C                   ENDIF
232700970603     C*
232800001010B0278 * CTRL obbligatorietà flag per saldi x data registrazione
232900001010 "   C***  D3Sk04        IFne      '0'
233000001010 "   C***  D3Sk04        andne     '1'
233100001010 "  >C***                SetOn                                        9099
233200001010 "   C***                ENDIF
233300001010 "   C                   MOVEL     *BLANK        D3SK04D
233400001010 "   C                   RESET                   X07DS
233500001010 "   C                   MOVE      '1'           X07RIC
233600001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
233700001010 "   C                   MOVEL     'DSPSINO'     X07CAM
233800001010 "   C                   MOVEL     D3SK04        X07VAL
233900001010 "   C                   CALLb     'X07VALR'
234000001010 "   C                   PARM                    X07DS
234100001010 "   C     X07ERR        IFEQ      '0'
234200001010 "   C                   MOVEL     X07DES        D3SK04D
234300001010 "   C                   ELSE
234400001010 "   C                   Seton                                        9099
234500001010b0278C                   ENDIF
234600970603     C*
234700970603    > * CTRL obbligatorietà flag per saldi x data competenza Co.Ge.
234800001010B0278C***  D3Sk05        IFne      '0'
234900001010 "   C***  D3Sk05        andne     '1'
235000001010 "  >C***                SetOn                                        9199
235100001010 "   C***                ENDIF
235200001010 "   C                   MOVEL     *BLANK        D3SK05D
235300001010 "   C                   RESET                   X07DS
235400001010 "   C                   MOVE      '1'           X07RIC
235500001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
235600001010 "   C                   MOVEL     'DSPSINO'     X07CAM
235700001010 "   C                   MOVEL     D3SK05        X07VAL
235800001010 "   C                   CALLb     'X07VALR'
235900001010 "   C                   PARM                    X07DS
236000001010 "   C     X07ERR        IFEQ      '0'
236100001010 "   C                   MOVEL     X07DES        D3SK05D
236200001010 "   C                   ELSE
236300001010 "   C                   Seton                                        9199
236400001010 "   C                   ENDIF
236500001010 "    *
236600001010 "    * CTRL obbligatorietà flag per registrazioni intercompany
236700001010 "   C***  D3Sk07        IFNE      '0'
236800001010 "   C***  D3Sk07        ANDNE     '1'
236900001010 "   C***                SetOn                                        5199
237000001010 "   C***                ENDIF
237100001010 "   C                   MOVEL     *BLANK        D3SK07D
237200001010 "   C                   RESET                   X07DS
237300001010 "   C                   MOVE      '1'           X07RIC
237400050113B0278C                   MOVEL     'ANDIZ000'    X07TRC
237500050113C1813X**exB0278          MOVEL     'DSPSINO'     X07CAM
237600050113C1813C                   MOVEL     'DSPSOCINT'   X07CAM
237700050113B0278C                   MOVEL     D3SK07        X07VAL
237800001010 "   C                   CALLb     'X07VALR'
237900001010 "   C                   PARM                    X07DS
238000001010 "   C     X07ERR        IFEQ      '0'
238100001010 "   C                   MOVEL     X07DES        D3SK07D
238200001010 "   C                   ELSE
238300001010 "   C                   Seton                                        5199
238400001010B0278C                   ENDIF
238500011211A0910 *
238600011211 "    * CTRL validità flag presenza Laguna
238700011211 "   C                   MOVEL     *BLANK        D3SK08D
238800011211 "   C                   RESET                   X07DS
238900011211 "   C                   MOVE      '1'           X07RIC
239000011211 "   C                   MOVEL     'ANDIZ000'    X07TRC
239100011211 "   C                   MOVEL     'DSPSINO'     X07CAM
239200011211 "   C                   MOVEL     D3SK08        X07VAL
239300011211 "   C                   CALLb     'X07VALR'
239400011211 "   C                   PARM                    X07DS
239500011211 "   C     X07ERR        IFEQ      '0'
239600011211 "   C                   MOVEL     X07DES        D3SK08D
239700011211 "   C                   ELSE
239800011211 "   C                   Seton                                        7499
239900011211A0910C                   ENDIF
240000961016     C*
240100941206      *
2402009412061-   C                   END
240300941206      *
240400941206     C                   ENDSR
240500941206     C/EJECT
240600941206      ************************************************************
240700941206      * RIEMPIMENTO VIDEATA D3
240800941206      ************************************************************
240900941206    >C     RIED3         BEGSR
241000941206      *
241100941206    >C                   MOVEL     SKP01         D3SK01
241200941206    >C                   MOVEL     SKP02         D3SK02
241300941206    >C                   MOVEL     SKP03         D3SK03
241400941206    >C                   MOVEL     SKP04         D3SK04
241500941206    >C                   MOVEL     SKP05         D3SK05
241600941206    >C                   MOVEL     SKP06         D3SK06
241700030103B0514X* XScSkp è un campo di XSoc001ds valorizzata da XSoc con
241800030103B0514X* i dati della società di collegamento
241900030103B0514X****************** Eval      D3SK06     =  %Subst(XScSkp:6:1)
242000030103B0514 * Reperisco nr decimali divisa della società in manutenzione
242100030103  "   * solo per renderlo visibile all'utente:
242200030103  "   * nr decimali della divisa della società non risiede su ansif00f
242300030103  "   * ma solo reperito da xsoc.
242400030103  "  C                   if        X0201RpDVS(D1Div : DtRifDiv:
242500030103  "  C                                    ANDVS00F: %size(ANDVS00F):
242600030103  "  C                                    ANDVX00F: %size(ANDVX00F)) =0
242700030103  "  C                   move      DVXNrDec      D3SK06
242800030103  "  C                   endif
242900030103B0514 *
243000941206    >C                   MOVEL     SKP07         D3SK07
243100941206    >C                   MOVEL     SKP08         D3SK08
243200941206    >C                   MOVEL     SKP09         D3SK09
243300941206    >C                   MOVEL     SKP10         D3SK10
243400941206    >C                   MOVEL     SKP11         D3SK11
243500941206      *
243600941206    >C                   MOVEL     SIFINP        D3INP
243700941214    >C                   MOVEL     SIFON         D3ON
243800941214    >C                   MOVEL     SIFOFF        D3OFF
243900941206      *
244000941206    >C                   ENDSR
244100941206     C/EJECT
244200941206      ************************************************************
244300941206      * GESTIONE F02 VIDEO D3
244400941206      ************************************************************
244500941206     C     F02D3         BEGSR
244600941206      *
244700941206      * per decodificare la videata richiamo la routine dei ctrl
244800941206      * e poi disattivo gli eventuali messaggi di errore
244900941206     C                   EXSR      CTRD3
245000941206      *
245100941206     C                   MOVE      *ALL'0'       IN5098
245200941206     C                   MOVEA     IN5098        *IN(50)
245300941206      *
245400941206     C                   ENDSR
245500941206     C/EJECT
245600941206      ************************************************************
245700941206      * GESTIONE F03 VIDEO D2
245800941206      ************************************************************
245900941206     C     F03D3         BEGSR
246000941206      *
246100941206     C                   MOVE      *ON           $FINE
246200941206    >C                   MOVE      '1'           RET008
246300941206      * fine programma
246400941206     C                   EXSR      ENDPGM
246500941206      *
246600941206     C                   ENDSR
246700941206     C/EJECT
246800941206     C/EJECT
246900941206      ************************************************************
247000941206      * GESTIONE F05 VIDEO D3
247100941206      * il tasto F5 su qualsiasi videata provoca il refresh di tutte
247200941206      * e il riposizionamento alla prima
247300941206      ************************************************************
247400941206    >C     F05D3         BEGSR
247500941206      *
247600941206     C                   MOVE      *ON           $INZD1
247700941206     C                   MOVE      'D3'          $GEST
247800941206      *
247900941206    >C                   ENDSR
248000941206      ************************************************************
248100941206      * GESTIONE F24 VIDEO D3
248200941206      ************************************************************
248300941206    >C     F24D3         BEGSR
248400941206      *
248500941206      *
248600941206     C                   MOVEA     $FX1          $FX
248700941206     C                   MOVEA     $FC1          $FC
248800941206     C                   MOVEA     $FR1          $FR
248900941206     C                   Z-ADD     $ULKD1        $ULK
249000941206     C                   EXSR      RIEKEY
249100941206     C                   Z-ADD     $ULK          $ULKD1            3 0
249200941206     C                   MOVEA     $FC           $FC1
249300941206     C                   MOVEL     $KEY1         Z1KE1
249400060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
249500941206     C                   MOVEL     $KEY2         Z1KE2
249600941206     C                   WRITE     FMTZ1
249700941206    >C                   ENDSR
249800941206     C/EJECT
249900941206      ************************************************************
250000941206      * GESTIONE VIDEO RECORD  D4                                *
250100941206      ************************************************************
250200941206    >C     GESD4         BEGSR
250300941206      * inizializzazione videata
250400941206     C     $INZD1        IFEQ      *ON
250500941206     C                   EXSR      INZD1
250600941206     C                   MOVE      *OFF          $INZD1
250700941206     C                   END
250800941206      * emissione piede videata se proveniente da altra
250900941206      * salvataggio valore formato di provenienza
251000941206     C     $LASTV        IFNE      'D4'
251100941206     C                   WRITE     FMTT1
251200941206     C                   EXSR      GESPIE
251300941206     C                   MOVE      $LASTV        $LASTG
251400941206     C                   MOVE      'D4'          $LASTV
251500941206     C                   END
251600941206      *
251700941206      * se esistono errori sulla videata
251800941206      * emetto la write del formato a indicatori spenti per vedere
251900941206      * le eventuali decodifiche
252000941206     C     *IN99         IFEQ      *ON
252100941206     C     $GEST         ANDEQ     'D4'
252200941206     C                   MOVEA     *IN           WIN
252300941206     C                   MOVE      *ALL'0'       IN5098           49
252400941206     C                   MOVEA     IN5098        *IN(50)
252500941206     C                   WRITE     FMTD4
252600941206     C                   MOVEA     WIN           *IN
252700941206     C                   ENDIF
252800941206      *
252900941206     C                   SELECT
253000941206      * Annullamento
253100941206     C     OPZ008        WHENEQ    '04'
253200941206      * Visualizzazione
253300941206     C     OPZ008        OREQ      '05'
253400941206     C                   WRITE     FMTD4
253500941206     C                   EXFMT     PROTECT
253600941206      * Immissione/Modifica/Copia
253700941206     C                   OTHER
253800941206     C                   EXFMT     FMTD4
253900941206     C                   ENDSL
254000941206      * controllo abilitazione tasti funzione
254100941206      *
254200941206     C                   EXSR      CTRKD1
254300941206      *
254400941206     C     *IN99         IFEQ      *OFF
254500941206      *
2546009412061    C                   SELECT
254700941206      * ROLDOWN
254800941206     C     $TASTO        WHENEQ    ROLDWN
254900941206     C                   MOVE      'D3'          $GEST
255000001004B0273 * ROLLUP
255100001004  "  C     $TASTO        WHENEQ    ROLLUP
255200001004B0273C                   MOVE      'D5'          $GEST
255300941206      * F3=Fine
255400941206     C     $TASTO        WHENEQ    F03
255500941206     C                   EXSR      F03D4
255600941206      * F4=Ricerca
255700941206     C     $TASTO        WHENEQ    F04
255800941206     C                   EXSR      SEARCH
255900941206      * F5=Refresh
256000941206     C     $TASTO        WHENEQ    F05
256100941206     C                   EXSR      F05D4
256200941206      * F8=Successivo
256300941206     C     $TASTO        WHENEQ    F08
256400941206     C                   EXSR      F08D4
256500941206      * F12=Ritorno
256600941206     C     $TASTO        WHENEQ    F12
256700941206     C                   EXSR      F12D4
256800941206      * F24=Altri tasti
256900941206     C     $TASTO        WHENEQ    F24
257000941206     C                   EXSR      F24D4
257100941206      *
2572009412061O   C                   OTHER
257300941206     C                   EXSR      CTRD4
257400941206     C     *IN99         IFEQ      *OFF
257500941206     C                   EXSR      CTRD3
257600941213     C     *IN99         IFEQ      *ON
257700970603     C                   MOVE      *ALL'0'       IN5098
257800970603     C                   MOVEA     IN5098        *IN(50)
257900970603     C****               MOVEA     *ALL'0'       *IN(50)
258000970603     C                   SETON                                            96
258100941213     C                   ELSE
258200941206     C                   EXSR      CTRD1
258300941213     C     *IN99         IFEQ      *ON
258400970603     C                   MOVE      *ALL'0'       IN5098
258500970603     C                   MOVEA     IN5098        *IN(50)
258600970603     C****               MOVEA     *ALL'0'       *IN(50)
258700970603     C                   SETON                                            96
258800941213     C                   ELSE
258900941206     C                   EXSR      CTRD2
259000941206     C     *IN99         IFEQ      *ON
259100970603     C                   MOVE      *ALL'0'       IN5098
259200970603     C                   MOVEA     IN5098        *IN(50)
259300970603     C****               MOVEA     *ALL'0'       *IN(50)
259400970603     C                   SETON                                            96
259500001004B0273C                   ELSE
259600001004  "  C                   EXSR      CTRD5
259700001004  "  C     *IN99         IFEQ      *ON
259800001004  "  C                   MOVE      *ALL'0'       IN5098
259900001004  "  C                   MOVEA     IN5098        *IN(50)
260000001004  "  C                   SETON                                            96
260100001004B0273C                   ENDIF
260200941206     C                   ENDIF
260300941213     C                   ENDIF
260400941206     C                   ENDIF
260500941213     C                   ENDIF
260600941206      *
260700941206      *
2608009412062    C     $TASTO        IFEQ      F06
260900941206     C     *IN99         ANDEQ     *OFF
261000980811      * Avverto utente: le modifiche sono operative dal prossimo collegament
261100980811     C                   EXSR      AvvertoUte
261200941206      * eseguo aggiornamento
261300941206     C                   EXSR      AGGANA
261400941206      * eseguo operazioni del dopo-aggiornamento
261500941206     C                   EXSR      GESAGG
2616009412062-   C                   ENDIF
261700941206      *
2618009412061-   C                   ENDSL
261900941206      *
2620009412062-   C                   ENDIF
262100941206    >C                   ENDSR
262200941206     C/EJECT
262300941206      ************************************************************
262400001004B0273 * RIEMPIMENTO VIDEATA D4 e D5
262500001004B0273X****** RIEMPIMENTO VIDEATA D4
262600941206      ************************************************************
262700001004B0273C     RIED4D5       BEGSR
262800001004B0273X*****RIED4         BEGSR
262900941206      *
263000950615    >C                   MOVEL     SIFOPE        D4FODS
263100960806     C     D4FO07        IFEQ      *BLANK
263200960806     C                   MOVEL     'D'           D4FO07
263300960806     C                   ENDIF
263400960806     C     D4FO08        IFEQ      *BLANK
263500960806     C                   MOVEL     'A'           D4FO08
263600960806     C                   ENDIF
263700941206      *
263800941206    >C                   ENDSR
263900941206     C/EJECT
264000941206      ************************************************************
264100941206      * GESTIONE F05 VIDEO D4
264200941206      * il tasto F5 su qualsiasi videata provoca il refresh di tutte
264300941206      * e il riposizionamento alla prima
264400941206      ************************************************************
264500941206    >C     F05D4         BEGSR
264600941206      *
264700941206     C                   MOVE      *ON           $INZD1
264800941206     C                   MOVE      'D4'          $GEST
264900941206      *
265000941206    >C                   ENDSR
265100941206      ************************************************************
265200941206      * GESTIONE F24 VIDEO D2
265300941206      ************************************************************
265400941206    >C     F24D4         BEGSR
265500941206      *
265600941206      *
265700941206     C                   MOVEA     $FX1          $FX
265800941206     C                   MOVEA     $FC1          $FC
265900941206     C                   MOVEA     $FR1          $FR
266000941206     C                   Z-ADD     $ULKD1        $ULK
266100941206     C                   EXSR      RIEKEY
266200941206     C                   Z-ADD     $ULK          $ULKD1            3 0
266300941206     C                   MOVEA     $FC           $FC1
266400941206     C                   MOVEL     $KEY1         Z1KE1
266500060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
266600941206     C                   MOVEL     $KEY2         Z1KE2
266700941206     C                   WRITE     FMTZ1
266800941206    >C                   ENDSR
266900941206      ************************************************************
267000941206      * CONTROLLO VIDEATA  D2
267100941206      ************************************************************
267200941206    >C     CTRD4         BEGSR
267300941206      *
267400941206     C                   SETOFF                                       99
267500941206      * non imposto pos. del cursore
267600941206     C                   Z-ADD     0             H1RIGA
267700941206     C                   Z-ADD     0             H1COLO
267800941206      *
267900941206      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
268000941206      *
2681009412061    C     OPZ008        IFNE      '04'
268200941206      *
268300941206    > * INSERIRE I CONTROLLI SPECIFICI DELLA VIDEATA
268400941206      * utilizzare indicatori diversi nelle diverse videate
268500941206      *
268600001010     C* Gestione unità
268700001010B0278C***  D4FO01        IFNE      '0'
268800001010 "  >C***  D4FO01        ANDNE     '1'
268900001010 "  >C***                SETON                                        8199
269000001010 "  >C***                END
269100001010 "   C                   MOVEL     *BLANK        D4FO01D
269200001010 "   C                   RESET                   X07DS
269300001010 "   C                   MOVE      '1'           X07RIC
269400001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
269500001010 "   C                   MOVEL     'DSPSINO'     X07CAM
269600001010 "   C                   MOVEL     D4FO01        X07VAL
269700001010 "   C                   CALLb     'X07VALR'
269800001010 "   C                   PARM                    X07DS
269900001010 "   C     X07ERR        IFEQ      '0'
270000001010 "   C                   MOVEL     X07DES        D4FO01D
270100001010 "   C                   ELSE
270200001010 "   C                   Seton                                        8199
270300001010b0278C                   ENDIF
270400951016      *
270500001010     C* Gestione pubblica
270600001010b0278C***  D4FO02        IFNE      '0'
270700001010 "  >C***  D4FO02        ANDNE     '1'
270800001010 "  >C***                SETON                                        8299
270900001010 "  >C***                END
271000001010 "   C                   MOVEL     *BLANK        D4FO02D
271100001010 "   C                   RESET                   X07DS
271200001010 "   C                   MOVE      '1'           X07RIC
271300001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
271400001010 "   C                   MOVEL     'DSPSINO'     X07CAM
271500001010 "   C                   MOVEL     D4FO02        X07VAL
271600001010 "   C                   CALLb     'X07VALR'
271700001010 "   C                   PARM                    X07DS
271800001010 "   C     X07ERR        IFEQ      '0'
271900001010 "   C                   MOVEL     X07DES        D4FO02D
272000001010 "   C                   ELSE
272100001010 "   C                   Seton                                        8299
272200001010b0278C                   ENDIF
272300941206      *
272400001010     C* Tipo plafond
272500001010b0278C***  D4FO03        IFNE      '1'
272600001010 "  >C***  D4FO03        ANDNE     '2'
272700001010 "  >C***  D4FO03        ANDNE     '0'
272800001010 "  >C***                SETON                                        8399
272900001010 "   C                   MOVEL     *BLANK        D4FO03D
273000001010 "   C                   RESET                   X07DS
273100001010 "   C                   MOVE      '1'           X07RIC
273200001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
273300001010 "   C                   MOVEL     'DSPPLAFOND'  X07CAM
273400001010 "   C                   MOVEL     D4FO03        X07VAL
273500001010 "   C                   CALLb     'X07VALR'
273600001010 "   C                   PARM                    X07DS
273700001010 "   C     X07ERR        IFEQ      '0'
273800001010B0278C                   MOVEL     X07DES        D4FO03D
273900001010     c* Plafond non gestibile se Tipo Liq. IVA <> da mensile
274000001010    >C     D4FO03        IFNE      '0'
274100001010    >C     D4FO10        ANDNE     '1'
274200001010    >C                   SETON                                        9299
274300001010    >C                   ENDIF
274400040302B0278C                   ELSE
274500001010 "   C                   Seton                                        8399
274600001010b0278C                   ENDIF
274700001005C1357 *
274800040302  "  C* Ventilazione IVA
274900001010  "  C***  D4FO04        IFNE      '1'
275000001010  "  C***  D4FO04        ANDNE     '0'
275100001010  "  C***                SETON                                        8499
275200001010C1357C***                ENDIF
275300001010B0278C                   MOVEL     *BLANK        D4FO04D
275400001010 "   C                   RESET                   X07DS
275500001010 "   C                   MOVE      '1'           X07RIC
275600001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
275700001010 "   C                   MOVEL     'DSPSINO'     X07CAM
275800001010 "   C                   MOVEL     D4FO04        X07VAL
275900001010 "   C                   CALLb     'X07VALR'
276000001010 "   C                   PARM                    X07DS
276100001010 "   C     X07ERR        IFEQ      '0'
276200001010 "   C                   MOVEL     X07DES        D4FO04D
276300001010 "   C                   ELSE
276400001010 "   C                   Seton                                        8499
276500001010b0278C                   ENDIF
276600960415      *
276700001010     C* Unità = linea di vendita
276800001010b0278C***  D4FO05        IFNE      '1'
276900001010 "  >C***  D4FO05        ANDNE     '0'
277000001010 "  >C***                SETON                                        8599
277100001010 "  >C***                END
277200001010 "   C                   MOVEL     *BLANK        D4FO05D
277300001010 "   C                   RESET                   X07DS
277400001010 "   C                   MOVE      '1'           X07RIC
277500001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
277600001010 "   C                   MOVEL     'DSPSINO'     X07CAM
277700001010 "   C                   MOVEL     D4FO05        X07VAL
277800001010 "   C                   CALLb     'X07VALR'
277900001010 "   C                   PARM                    X07DS
278000001010 "   C     X07ERR        IFEQ      '0'
278100001010 "   C                   MOVEL     X07DES        D4FO05D
278200001010 "   C                   ELSE
278300001010 "   C                   Seton                                        8599
278400001010b0278C                   ENDIF
278500960514      *
278600960902    >C     D4FO05        IFEQ      '1'
278700960514    >C     D4FO01        ANDNE     '1'
278800960514    >C                   SETON                                        9599
278900960514    >C                   END
279000960514      *
279100001010     C* Gestione lavori automatici
279200001010b0278C***  D4FO06        IFNE      '1'
279300001010 "  >C***  D4FO06        ANDNE     '0'
279400001010 "  >C***                SETON                                        8699
279500001010 "  >C***                END
279600001010 "   C                   MOVEL     *BLANK        D4FO06D
279700001010 "   C                   RESET                   X07DS
279800001010 "   C                   MOVE      '1'           X07RIC
279900001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
280000001010 "   C                   MOVEL     'DSPSINO'     X07CAM
280100001010 "   C                   MOVEL     D4FO06        X07VAL
280200001010 "   C                   CALLb     'X07VALR'
280300001010 "   C                   PARM                    X07DS
280400001010 "   C     X07ERR        IFEQ      '0'
280500001010 "   C                   MOVEL     X07DES        D4FO06D
280600001010 "   C                   ELSE
280700001010 "   C                   Seton                                        8699
280800001010b0278C                   ENDIF
280900941206      *
281000961017    >C     D4FO07        IfEQ      *blank
281100961017    >C                   SETON                                        8799
281200961017    >C                   END
281300961017      *
281400961017    >C     D4FO08        IfEQ      *blank
281500961017    >C                   SETON                                        8899
281600961017    >C                   END
281700980710      *
2818009412061-   C                   END
281900980710     C*
282000980710     C* Controllo regime IVA
282100980821     C                   Clear                   D4FO09D
282200980713     C     D4FO09        IfNE      *blank
282300980710     C                   MOVEL     *BLANK        D4FO09D
282400980710     C                   RESET                   X07DS
282500980710     C                   MOVE      '1'           X07RIC
282600980710     C                   MOVEL     'ANDIZ000'    X07TRC
282700980710     C                   MOVEL     'DSPIVARGM'   X07CAM
282800980710     C                   MOVEL     D4FO09        X07VAL
282900980710     C                   CALLb     'X07VALR'
283000980710     C                   PARM                    X07DS
283100980710   B3C     X07ERR        IFEQ      '0'
283200980710     C                   MOVEL     X07DES        D4FO09D
283300980710   X3C                   ELSE
283400980710     C                   Seton                                        9499
283500980710   E3C                   ENDIF
283600980821   E3C                   ELSE
283700980821     C                   Seton                                        9499
283800980713     C                   ENDIF
283900980710     C*
284000980803     C* Controllo tipo liquidazione
284100980821     C                   Clear                   D4FO10D
284200980713     C     D4FO10        IfNE      *blank
284300980710     C                   MOVEL     *BLANK        D4FO10D
284400980710     C                   RESET                   X07DS
284500980710     C                   MOVE      '1'           X07RIC
284600980710     C                   MOVEL     'ANDIZ000'    X07TRC
284700980710     C                   MOVEL     'DSPIVALIQ'   X07CAM
284800980710     C                   MOVEL     D4FO10        X07VAL
284900980710     C                   CALLb     'X07VALR'
285000980710     C                   PARM                    X07DS
285100980710   B3C     X07ERR        IFEQ      '0'
285200980710     C                   MOVEL     X07DES        D4FO10D
285300980710   X3C                   ELSE
285400980713     C                   Seton                                        9399
285500980710   E3C                   ENDIF
285600980821   E3C                   ELSE
285700980821     C                   Seton                                        9399
285800980713     C                   ENDIF
285900021007     c*
286000021007c1582C* Controllo regole controllo entità
286100021007 "   C                   Clear                   D4FO11D
286200021007 "   C     D4FO11        IfNE      *blank
286300021007 "   C                   RESET                   X07DS
286400021007 "   C                   MOVE      '1'           X07RIC
286500021007 "   C                   MOVEL     'ANDIZ000'    X07TRC
286600021007 "   C                   MOVEL     'DSPSINO  '   X07CAM
286700021007 "   C                   MOVEL     D4FO11        X07VAL
286800021007 "   C                   CALLb     'X07VALR'
286900021007 "   C                   PARM                    X07DS
287000021007 " B3C     X07ERR        IFEQ      '0'
287100021007 "   C                   MOVEL     X07DES        D4FO11D
287200021007 " X3C                   ELSE
287300021007 "   C                   Seton                                        8099
287400021007 " E3C                   ENDIF
287500021007 " E3C                   ELSE
287600021007 "   C                   Seton                                        8099
287700021007C1582C                   ENDIF
287800020311C1550 *
287900020311  "   * Se regime IVA trasportat. mista il tipo liq deve essere mensile
288000020311  "   * (se regime trasport. mista, è possibile definire per ogni libro
288100020311  "   *  uno slittamento e un tipo liquidazione, ma per quei libri
288200020311  "   *  che non hanno slittamento, è valida il tipo di liquid.
288300020311  "   *  definito sulla società)
288400020311  "  C                   if        D4Fo09='2' and D4Fo10<>'1'
288500020311  "  C                   eval      *In89 = *on
288600020311  "  C                   eval      *In99 = *on
288700020311C1550C                   endif
288800020311     C*
288900941206      *
289000941206     C                   ENDSR
289100001004B0273 ************************************************************
289200001004  "   * GESTIONE F03 VIDEO D5
289300001004  "   ************************************************************
289400001004B0273C     F03D5         BEGSR
289500001004      *
289600001004     C                   MOVE      *ON           $FINE
289700001004    >C                   MOVE      '1'           RET008
289800001004      * fine programma
289900001004     C                   EXSR      ENDPGM
290000001004      *
290100001004B0273C                   ENDSR
290200001004B0273 ************************************************************
290300001004  "   * GESTIONE F05 VIDEO D5
290400001004  "   * il tasto F5 su qualsiasi videata provoca il refresh di tutte
290500001004  "   * e il riposizionamento alla prima
290600001004  "   ************************************************************
290700001004B0273C     F05D5         BEGSR
290800001004      *
290900001004     C                   MOVE      *ON           $INZD1
291000001004     C                   MOVE      'D5'          $GEST
291100001004      *
291200001004B0273C                   ENDSR
291300001004B0273 ************************************************************
291400001004  "   * GESTIONE F08 VIDEO D5
291500001004  "   ************************************************************
291600001004B0273C     F08D5         BEGSR
291700001004      *
291800001004     C                   MOVE      *ON           $FINE
291900001004    >C                   MOVE      '0'           RET008
292000001004      *
292100001004B0273C                   ENDSR
292200001004B0273 ************************************************************
292300001004  "   * GESTIONE F12 VIDEO D5
292400001004  "   ************************************************************
292500001004B0273C     F12D5         BEGSR
292600001004      *
292700001004     C                   MOVE      *ON           $FINE
292800001004    >C                   MOVE      '2'           RET008
292900001004      *
293000001004B0273C                   ENDSR
293100001004B0273 ************************************************************
293200001004  "   * GESTIONE F24 VIDEO D5
293300001004  "   ************************************************************
293400001004B0273C     F24D5         BEGSR
293500001004      *
293600001004     C                   MOVEA     $FX1          $FX
293700001004     C                   MOVEA     $FC1          $FC
293800001004     C                   MOVEA     $FR1          $FR
293900001004     C                   Z-ADD     $ULKD1        $ULK
294000001004     C                   EXSR      RIEKEY
294100001004     C                   Z-ADD     $ULK          $ULKD1            3 0
294200001004     C                   MOVEA     $FC           $FC1
294300001004     C                   MOVEL     $KEY1         Z1KE1
294400060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
294500001004     C                   MOVEL     $KEY2         Z1KE2
294600001004     C                   WRITE     FMTZ1
294700001004      *
294800001004B0273C                   ENDSR
294900001004B0273 ************************************************************
295000001004  "   * GESTIONE VIDEO RECORD  D5                                *
295100001004  "   ************************************************************
295200001004B0273C     GESD5         BEGSR
295300001004      * inizializzazione videata
295400001004     C     $INZD1        IFEQ      *ON
295500001004     C                   EXSR      INZD1
295600001004     C                   MOVE      *OFF          $INZD1
295700001004     C                   ENDIF
295800001004      * emissione piede videata se proveniente da altra
295900001004      * salvataggio valore formato di provenienza
296000001004     C     $LASTV        IFNE      'D5'
296100001004     C                   WRITE     FMTT1
296200001004     C                   EXSR      GESPIE
296300001004     C                   MOVE      $LASTV        $LASTG
296400001004     C                   MOVE      'D5'          $LASTV
296500001004     C                   ENDIF
296600001004      *
296700001004      * se esistono errori sulla videata
296800001004      * emetto la write del formato a indicatori spenti per vedere
296900001004      * le eventuali decodifiche
297000001004     C     *IN99         IFEQ      *ON
297100001004     C     $GEST         ANDEQ     'D5'
297200001004     C                   MOVEA     *IN           WIN
297300001004     C                   MOVE      *ALL'0'       IN5098           49
297400001004     C                   MOVEA     IN5098        *IN(50)
297500001004     C                   WRITE     FMTD5
297600001004     C                   MOVEA     WIN           *IN
297700001004     C                   ENDIF
297800001004      *
297900001004     C                   SELECT
298000001004      * Annullamento
298100001004     C     OPZ008        WHENEQ    '04'
298200001004      * Visualizzazione
298300001004     C     OPZ008        OREQ      '05'
298400001004     C                   WRITE     FMTD5
298500001004     C                   EXFMT     PROTECT
298600001004      * Immissione/Modifica/Copia
298700001004     C                   OTHER
298800001004     C                   EXFMT     FMTD5
298900001004     C                   ENDSL
299000001004      * controllo abilitazione tasti funzione
299100001004      *
299200001004     C                   EXSR      CTRKD1
299300001004      *
299400001004     C     *IN99         IFEQ      *OFF
299500001004      *
299600001004     C                   SELECT
299700001004      * ROLDOWN
299800001004     C     $TASTO        WHENEQ    ROLDWN
299900001004     C                   MOVE      'D4'          $GEST
300000001004      * F3=Fine
300100001004     C     $TASTO        WHENEQ    F03
300200001004     C                   EXSR      F03D5
300300001004      * F4=Ricerca
300400001004     C     $TASTO        WHENEQ    F04
300500001004     C                   EXSR      SEARCH
300600001004      * F5=Refresh
300700001004     C     $TASTO        WHENEQ    F05
300800001004     C                   EXSR      F05D5
300900001004      * F8=Successivo
301000001004     C     $TASTO        WHENEQ    F08
301100001004     C                   EXSR      F08D5
301200001004      * F12=Ritorno
301300001004     C     $TASTO        WHENEQ    F12
301400001004     C                   EXSR      F12D5
301500001004      * F24=Altri tasti
301600001004     C     $TASTO        WHENEQ    F24
301700001004     C                   EXSR      F24D5
301800001004      *
301900001004     C                   OTHER
302000001004     C                   EXSR      CTRD5
302100001004     C     *IN99         IFEQ      *OFF
302200001004     C                   EXSR      CTRD3
302300001004     C     *IN99         IFEQ      *ON
302400001004     C                   MOVE      *ALL'0'       IN5098
302500001004     C                   MOVEA     IN5098        *IN(50)
302600001004     C                   SETON                                            96
302700001004     C                   ELSE
302800001004     C                   EXSR      CTRD1
302900001004     C     *IN99         IFEQ      *ON
303000001004     C                   MOVE      *ALL'0'       IN5098
303100001004     C                   MOVEA     IN5098        *IN(50)
303200001004     C                   SETON                                            96
303300001004     C                   ELSE
303400001004     C                   EXSR      CTRD2
303500001004     C     *IN99         IFEQ      *ON
303600001004     C                   MOVE      *ALL'0'       IN5098
303700001004     C                   MOVEA     IN5098        *IN(50)
303800001004     C                   SETON                                            96
303900001004     C                   ELSE
304000001004     C                   EXSR      CTRD4
304100001004     C     *IN99         IFEQ      *ON
304200001004     C                   MOVE      *ALL'0'       IN5098
304300001004     C                   MOVEA     IN5098        *IN(50)
304400001004     C                   SETON                                            96
304500001004     C                   ENDIF
304600001004     C                   ENDIF
304700001004     C                   ENDIF
304800001004     C                   ENDIF
304900001004     C                   ENDIF
305000001004      *
305100001004      *
305200001004     C     $TASTO        IFEQ      F06
305300001004     C     *IN99         ANDEQ     *OFF
305400001004      * Avverto utente: le modifiche sono operative dal prossimo collegament
305500001004     C                   EXSR      AvvertoUte
305600001004      * eseguo aggiornamento
305700001004     C                   EXSR      AGGANA
305800001004      * eseguo operazioni del dopo-aggiornamento
305900001004     C                   EXSR      GESAGG
306000001004     C                   ENDIF
306100001004      *
306200001004     C                   ENDSL
306300001004      *
306400001004     C                   ENDIF
306500001004B0273C                   ENDSR
306600001004B0273 ************************************************************
306700001004  "   * CONTROLLO VIDEATA  D5
306800001004  "   ************************************************************
306900001004B0273C     CTRD5         BEGSR
307000001004      *
307100001004     C                   SETOFF                                       99
307200001004      * non imposto pos. del cursore
307300001004     C                   Z-ADD     0             H1RIGA
307400001004     C                   Z-ADD     0             H1COLO
307500001004      *
307600001004      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
307700001004      *
307800001004     C     OPZ008        IFNE      '04'
307900001004      *
308000001004      * Flag reperimento lingua
308100001010B0278C***                If        D5FO16 <> '0' and D5FO16 <> '1'
308200001010 "   C***                SetOn                                        5099
308300001010 "   C***                EndIf
308400001010 "   C                   MOVEL     *BLANK        D5FO16D
308500001010 "   C                   RESET                   X07DS
308600001010 "   C                   MOVE      '1'           X07RIC
308700001010 "   C                   MOVEL     'ANDIZ000'    X07TRC
308800001010 "   C                   MOVEL     'DSPREPLIN'   X07CAM
308900001010 "   C                   MOVEL     D5FO16        X07VAL
309000001010 "   C                   CALLb     'X07VALR'
309100001010 "   C                   PARM                    X07DS
309200001010 "   C     X07ERR        IFEQ      '0'
309300001010 "   C                   MOVEL     X07DES        D5FO16D
309400001010 "   C                   ELSE
309500001010 "   C                   Seton                                        5099
309600001010b0278C                   ENDIF
309700001004      *
309800001004      * 9 flag conversione lingue A.B.P. (17-25)
309900001004     C                   Clear                   D5FO17D
310000001004     C                   If        D5FO17 <> *Blank
310100001004     C                   Eval      WWLin = D5FO17
310200001004     C                   ExSr      CtrLingua
310300001004     C     XTBERR        IFEQ      '0'
310400001004     C                   MOVEL     DESG15        D5FO17D
310500001004     C                   ELSE
310600001004     C                   SETON                                        5199
310700001004     C                   ENDIF
310800001004     C                   EndIf
310900001004      *
311000001004     C                   Clear                   D5FO18D
311100001004     C                   If        D5FO18 <> *Blank
311200001004     C                   Eval      WWLin = D5FO18
311300001004     C                   ExSr      CtrLingua
311400001004     C     XTBERR        IFEQ      '0'
311500001004     C                   MOVEL     DESG15        D5FO18D
311600001004     C                   ELSE
311700001004     C                   SETON                                        5299
311800001004     C                   ENDIF
311900001004     C                   EndIf
312000001004      *
312100001004     C                   Clear                   D5FO19D
312200001004     C                   If        D5FO19 <> *Blank
312300001004     C                   Eval      WWLin = D5FO19
312400001004     C                   ExSr      CtrLingua
312500001004     C     XTBERR        IFEQ      '0'
312600001004     C                   MOVEL     DESG15        D5FO19D
312700001004     C                   ELSE
312800001004     C                   SETON                                        5399
312900001004     C                   ENDIF
313000001004     C                   EndIf
313100001004      *
313200001004     C                   Clear                   D5FO20D
313300001004     C                   If        D5FO20 <> *Blank
313400001004     C                   Eval      WWLin = D5FO20
313500001004     C                   ExSr      CtrLingua
313600001004     C     XTBERR        IFEQ      '0'
313700001004     C                   MOVEL     DESG15        D5FO20D
313800001004     C                   ELSE
313900001004     C                   SETON                                        5499
314000001004     C                   ENDIF
314100001004     C                   EndIf
314200001004      *
314300001004     C                   Clear                   D5FO21D
314400001004     C                   If        D5FO21 <> *Blank
314500001004     C                   Eval      WWLin = D5FO21
314600001004     C                   ExSr      CtrLingua
314700001004     C     XTBERR        IFEQ      '0'
314800001004     C                   MOVEL     DESG15        D5FO21D
314900001004     C                   ELSE
315000001004     C                   SETON                                        5599
315100001004     C                   ENDIF
315200001004     C                   EndIf
315300001004      *
315400001004     C                   Clear                   D5FO22D
315500001004     C                   If        D5FO22 <> *Blank
315600001004     C                   Eval      WWLin = D5FO22
315700001004     C                   ExSr      CtrLingua
315800001004     C     XTBERR        IFEQ      '0'
315900001004     C                   MOVEL     DESG15        D5FO22D
316000001004     C                   ELSE
316100001004     C                   SETON                                        5699
316200001004     C                   ENDIF
316300001004     C                   EndIf
316400001004      *
316500001004     C                   Clear                   D5FO23D
316600001004     C                   If        D5FO23 <> *Blank
316700001004     C                   Eval      WWLin = D5FO23
316800001004     C                   ExSr      CtrLingua
316900001004     C     XTBERR        IFEQ      '0'
317000001004     C                   MOVEL     DESG15        D5FO23D
317100001004     C                   ELSE
317200001004     C                   SETON                                        5799
317300001004     C                   ENDIF
317400001004     C                   EndIf
317500001004      *
317600001004     C                   Clear                   D5FO24D
317700001004     C                   If        D5FO24 <> *Blank
317800001004     C                   Eval      WWLin = D5FO24
317900001004     C                   ExSr      CtrLingua
318000001004     C     XTBERR        IFEQ      '0'
318100001004     C                   MOVEL     DESG15        D5FO24D
318200001004     C                   ELSE
318300001004     C                   SETON                                        5899
318400001004     C                   ENDIF
318500001004     C                   EndIf
318600001004      *
318700001004     C                   Clear                   D5FO25D
318800001004     C                   If        D5FO25 <> *Blank
318900001004     C                   Eval      WWLin = D5FO25
319000001004     C                   ExSr      CtrLingua
319100001004     C     XTBERR        IFEQ      '0'
319200001004     C                   MOVEL     DESG15        D5FO25D
319300001004     C                   ELSE
319400001004     C                   SETON                                        5999
319500001004     C                   ENDIF
319600001004     C                   EndIf
319700001004      *
319800001004     C                   ENDIF
319900001004      *
320000001004B0273C                   ENDSR
320100001004B0273 ************************************************************
320200001004  "   * Controllo codice lingua su tabella G15
320300001004  "   ************************************************************
320400001004B0273C     CtrLingua     BEGSR
320500001004      *
320600001004     C                   MOVE      '1'           XTBRIC
320700001004     C                   MOVE      XSCSOC        XTBAZI
320800001004     C                   MOVE      *ZERO         XTBKEY
320900001004     C                   MOVE      'G15'         XTBCOD
321000001004     C                   MOVE      WWLin         XTBKEY
321100001004     C                   CALLB     'XATB'
321200001004     C                   PARM                    XATBDS
321300001004     C     XTBERR        IFEQ      '0'
321400001004     C                   MOVEL     XTBUNI        G15DS
321500001004     C                   ENDIF
321600001004      *
321700001004B0273C                   ENDSR
321800941206    > ************************************************************
321900941206    > * ACCESSO A TABELLE ANTAB00F
322000941206    > ************************************************************
322100941012    >C     LETTAB        BEGSR
322200941206    > *
322300941206    > * Input :
322400941206    > * - TABGRU/TABCOD = chiave univoca ANTAB00F
322500941206    > * Output :
322600941206    > * - WTBERR = flag segnalazione errore reperimento tabella
322700941206    > *
322800960930    >C                   CALLB     'XATB'
322900941012    >C                   PARM                    XATBDS
323000941206    > *
323100941012    >C                   MOVE      *OFF          WTBERR
323200941012    >C     XTBERR        IFEQ      *ON
323300941012    >C                   MOVE      *BLANK        XTBUNI
323400941012    >C                   MOVE      *ON           WTBERR
323500941012    >C                   ENDIF
323600941206    > *
323700941012    >C                   ENDSR
323800940224     C/EJECT
323900941206      ************************************************************
324000941206    > * AGGIORNAMENTO ANAGRAFICA
324100941206      ************************************************************
324200941206    >C     AGGANA        BEGSR
324300941206      *
324400940207     C                   SELECT
324500951013     C     OPZ008        WHENEQ    '01'
324600951013     C     OPZ008        OREQ      '03'
324700951013     C                   EXSR      RIESIF
324800951013     C                   WRITE     ANSIF01                              22
324900941206      * se rcd non scrivibile attivo errore
325000940318     C     *IN22         IFEQ      *ON
325100951013     C                   SETON                                        5999
325200940318     C                   END
325300951013     C     OPZ008        WHENEQ    '02'
325400951013     C                   MOVEL     SOC008        SIFSOC
325500951013     C     K1SIF1        CHAIN     ANSIF01                            2122
325600951013     C                   EXSR      RIESIF
325700951013     C                   UPDATE    ANSIF01
325800951013     C     OPZ008        WHENEQ    '04'
325900951013     C                   EXSR      RIESIF
326000951013     C                   UPDATE    ANSIF01
326100950322     C                   ENDSL
326200950322      *
326300950322      * richiama pgm User Exit
326400950322      *
326500950322     C     USRERR        IFEQ      *BLANK
326600950322     C     *IN99         ANDEQ     *OFF
326700950322      * imposta chiave univoca  per user exit
326800950322     C                   MOVE      XSCSOC        XUKSOC
326900950322     C                   MOVE      XSCDUN        XUKDUN
327000950322     C                   MOVEL     NOMPGM        XUKPGM
327100950322    >C                   MOVEL     SIFSOC        XUKFL1
327200950322    >C                   MOVEL     XUKDS         KPJBU
327300950322     C     NOMPGM        CAT       'U':0         USREXT           10
327400950322     C                   CALL      USREXT                               21
327500950322     C                   PARM                    KPJBA
327600950322     C     *IN21         IFEQ      *ON
327700950322     C                   MOVE      *ON           USRERR            1
327800950322     C                   ENDIF
327900950322     C                   ENDIF
328000941206      *
328100940203     C                   ENDSR
328200940224     C/EJECT
328300941206      ************************************************************
328400941206      * RIEMPIMENTO FILE IN AGGIORNAMENTO
328500941206      ************************************************************
328600941012    >C     RIESIF        BEGSR
328700941206      *
328800010502B0369C*
328900010502  "  C* salvo il valore dei 10 ultimi caratteri di SIFSKP e di
329000010502  "  C* SIFFLGOPE, in quanto
329100010502  "  C* potrebbero essere usati per gestire procedure personalizzate
329200010502  "  C                   Move(P)   SIFSKP        SavSKP10         10
329300010502  "  C                   Move(P)   SIFFlg        SavFlg10         10
329400010502B0369C*
329500941213     C                   CLEAR                   ANSIF01
329600941206    > *-------------------------------------------------------------
329700941206    > * impostare i campi del file
329800941206    > *-------------------------------------------------------------
329900941206      *Videata D1
330000950110    >C                   MOVEL     D1SOC         SIFSOC
330100950110    >C                   MOVEL     D1DSI         SIFDSI
330200950110    >C                   MOVEL     D1NSI         SIFNSI
330300950110    >C                   MOVEL     D1SIA         SIFSIA
330400950110    >C                   MOVEL     D1SI2         SIFSI2
330500950110    >C                   MOVEL     D1MCH         SIFMCH
330600950110    >C                   MOVEL     D1SCF         SIFSCF
330700950110    >C                   MOVEL     D1CPC         SIFCPC
330800950110    >C                   MOVEL     D1CPA         SIFCPA
330900950110    >C                   MOVEL     D1CPF         SIFCPF
331000950110    >C                   MOVEL     D1LIN         SIFLIN
331100950110    >C                   MOVEL     D1DIV         SIFDIV
331200950110    >C                   MOVEL     D1STA         SIFSTA
331300950110    >C                   MOVEL     D1NGR         SIFNGR
331400950110    >C                   MOVEL     D1CDF         SIFCDF
331500950110    >C                   MOVEL     D1IVA         SIFIVA
331600941206      **
331700941206     C                   MOVEL     D1DFV         XDTGMA
331800960930     C                   CALLB     'XDT4000'
331900941206     C                   PARM                    XDTAMG           10
332000941206     C                   PARM                    XDTGMA            6 0
332100941206     C                   PARM      4             XDTSTA            1 0
332200941206 4   C     XDTSTA        IFEQ      0                                            *
332300941206     C                   MOVEL     XDTAMG        SIFDFV
332400941206     C                   ENDIF
332500941206      *
332600941206     C                   MOVEL     D1DAB         XDTGMA                         *
332700960930     C                   CALLB     'XDT4000'
332800941206     C                   PARM                    XDTAMG           10
332900941206     C                   PARM                    XDTGMA            6 0
333000941206     C                   PARM      4             XDTSTA            1 0
333100941206 4   C     XDTSTA        IFEQ      0                                            *
333200941206     C                   MOVEL     XDTAMG        SIFDAB
333300941206     C                   ENDIF
333400941206      *
333500941206     C                   MOVEL     D1DSA         XDTGMA                         *
333600960930     C                   CALLB     'XDT4000'
333700941206     C                   PARM                    XDTAMG           10
333800941206     C                   PARM                    XDTGMA            6 0
333900941206     C                   PARM      4             XDTSTA            1 0
334000941206 4   C     XDTSTA        IFEQ      0                                            *
334100941206     C                   MOVEL     XDTAMG        SIFDSA
334200941206     C                   ENDIF
334300960827      *
334400960827     C                   MOVEL     D1LibDatHB    SifSlC                         *
334500941206      *Videata D2
334600941206    >C                   MOVEL     D2RGS         SIFRGS
334700100505R002 C                   MOVEL     D2Sogg        SIFSogg
334800941206    >C                   MOVEL     D2VIA         SIFVIA
334900941206    >C                   MOVEL     D2CIT         SIFCIT
335000941206    >C                   MOVEL     D2CAP         SIFCAP
335100941206    >C                   MOVEL     D2PRV         SIFPRV
335200941206    >C                   MOVEL     D2TEL         SIFTEL
335300970807    >C                   MOVEL     D2Fax         SIFFax
335400941206    >C                   MOVEL     D2TLX         SIFTLX
335500970807    >C                   MOVEL     D2TLS         SIFTLS
335600941206    >C                   MOVEL     D2VSC         SIFVSC
335700941206    >C                   MOVEL     D2CSC         SIFCSC
335800941206    >C                   MOVEL     D2CAS         SIFCAS
335900941206    >C                   MOVEL     D2PRS         SIFPRS
336000941206    >C                   MOVEL     D2CTI         SIFCTI
336100941206    >C                   MOVEL     D2CCT         SIFCCT
336200941206    >C                   MOVEL     D2ATT         SIFATT
336300941206    >C                   MOVEL     D2ESA         SIFESA
336400941206    >C                   MOVEL     D2CDC         SIFCDC
336500941206      *
336600941206    >C                   MOVEL     D2DTI         XDTGMA
336700960930     C                   CALLB     'XDT4000'
336800941206     C                   PARM                    XDTAMG           10
336900941206     C                   PARM                    XDTGMA            6 0
337000941206     C                   PARM      4             XDTSTA            1 0
337100941206     C     XDTSTA        IFEQ      0                                            *
337200941206     C                   MOVEL     XDTAMG        SIFDTI
337300941206     C                   ENDIF                                                  *
337400941206      *Videata D3
337500941206    >C                   MOVEL     D3SK01        SKP01
337600941206    >C                   MOVEL     D3SK02        SKP02
337700941206    >C                   MOVEL     D3SK03        SKP03
337800941206    >C                   MOVEL     D3SK04        SKP04
337900941206    >C                   MOVEL     D3SK05        SKP05
338000030103B0514 * nr decimali della divisa della società non risiede su ansif00f
338100030103B0514 * ma viene solo reperito da xsoc.
338200980811    >C*****              MOVEL     D3SK06        SKP06
338300941206    >C                   MOVEL     D3SK07        SKP07
338400941206    >C                   MOVEL     D3SK08        SKP08
338500941206    >C                   MOVEL     D3SK09        SKP09
338600941206    >C                   MOVEL     D3SK10        SKP10
338700941206    >C                   MOVEL     D3SK11        SKP11
338800010502B0369C*
338900010502  "  C* SOLO in caso di modifica ripristino i valori salvati
339000010502  "  C                   if        OPZ008     =  '02'
339100010502  "  C                   Move      SavSKP10      SIFSKP
339200010502  "  C                   Move      SavFlg10      SIFFlg
339300010502  "  C                   endif
339400010502B0369C*
339500941206      *
339600941206    >C                   MOVEL     D3INP         SIFINP
339700941214    >C                   MOVEL     D3ON          SIFON
339800941214    >C                   MOVEL     D3OFF         SIFOFF
339900001004B0273 * Videata D4 e D5
340000001004B0273X******Videata D4
340100950615    >C                   MOVEL     D4FODS        SIFOPE
340200950615      *
340300950615    >C*                    MOVELD4FO01    OPE01
340400950615    >C*                    MOVELD4FO02    OPE02
340500950615    >C*                    MOVELD4FO03    OPE03
340600950615    >C*                    MOVELD4FO04    OPE04
340700950615    >C*                    MOVELD4FO05    OPE05
340800950615    >C*                    MOVELD4FO06    OPE06
340900950615    >C*                    MOVELD4FO07    OPE07
341000950615    >C*                    MOVELD4FO08    OPE08
341100950615    >C*                    MOVELD4FO09    OPE09
341200950615    >C*                    MOVELD4FO10    OPE10
341300950615    >C*                    MOVELD4FO11    OPE11
341400950615    >C*                    MOVELD4FO12    OPE12
341500950615    >C*                    MOVELD4FO13    OPE13
341600950615    >C*                    MOVELD4FO14    OPE14
341700941206      *
341800940207     C                   ENDSR
341900940224     C/EJECT
342000941206      ************************************************************
342100941206      * GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
342200941206      ************************************************************
342300940224     C     GESAGG        BEGSR
342400941206      *
342500941206      * segnalo al pgm chiamante l'avvenuta conferma
342600941012    >C                   MOVE      *ON           OPR008
342700941206      * segnalo al pgm chiamante l'aver premuto l'F6
342800941012    >C                   MOVE      '0'           RET008
342900941206      *
3430009402241    C                   SELECT
343100941206      * nel caso di immissione, ripristino la videata iniziale
343200941012    >C     OPZ008        WHENEQ    '01'
343300940224     C                   MOVE      *ON           $INZD1
343400941206      * altrimenti ritorno al pgm chiamante
343500940224     C                   OTHER
343600940224     C                   MOVE      *ON           $FINE
3437009402241-   C                   ENDSL
343800941206      *
343900940224     C                   ENDSR
344000940318     C/EJECT
344100941206      ******************************************************
344200941206      * PGM PER GESTIONE MESSAGGI ERRORE                   *
344300941206      ******************************************************
344400940318     C     MSGPGM        BEGSR
344500941206      *
344600940318     C                   Z-ADD     2             MSGMSG
344700940318     C                   MOVEL     'S'           MSGRSP
344800940318     C                   MOVEL     'CR'          MSGTPR
344900940318     C                   MOVEL     'S'           MSGEMV
345000940318     C                   Z-ADD     10            MSGRGP
345100940318     C                   MOVEL     'S'           MSGLCK
345200940318     C                   MOVEA     STATUS        STS
345300940318     C                   MOVEL     'N'           MSGCNL
345400940318     C                   MOVEL     'N'           MSGVID
345500940318     C                   MOVEL     'N'           MSGOPE
345600940318     C                   MOVEL     'N'           MSGSTP
345700940318     C                   CALL      'XMSGR'
345800940318     C                   PARM                    XMSGDS
345900941206      *
346000940318     C                   ENDSR
346100940207     C/EJECT
346200941206      ************************************************************
346300941206      * OPERAZIONI INIZIALI
346400941206      ************************************************************
346500940131     C     *INZSR        BEGSR
346600000120      *
346700000120A0740C     *DMY          Move      Udate         ZDateISO
346800000120A0740C     *JobRun       Move      ZDateISO      ZDate             6 0
346900941206      *
347000941206      * Reperimento parametri
347100941206      *
347200940117     C     *ENTRY        PLIST
347300940117     C                   PARM                    KPJBA
347400941103      *
347500941103      * Reperimento dati società
347600941103      *
347700941103     C                   MOVEL     'SOC001'      TIPXSC
347800941103     C                   MOVEL     *BLANK        SOCXSC
347900941103     C                   EXSR      REPSOC
348000941103     C     RTNXSC        IFNE      '1'
348100941103     C                   MOVEL     XSOCDS        SOC001
348200941103     C                   ELSE
348300941103     C                   MOVE      *ON           $FINE
348400941103     C                   EXSR      ENDPGM
348500941103     C                   ENDIF
348600961010      *
348700961010      * Reperimento nome sistema informatico
348800961010     C                   CALL      'XNETA'                              21
348900961010     C                   PARM                    NOMSYS            8
349000971030      *
349100971030      * Reperimento nome libreria modulo base
349200991015A0709C****               MOVE      *BLANK        NOMLIB           10
349300991015A0709C****               CALL      'PAX008C'
349400991015A0709C****               PARM                    NOMLIB
349500991015      *
349600991015A0709 * Richiamo del nuovo programma di reperimento Libreria del
349700991015  "   * modulo base
349800060711A0709C                   MOVE      *BLANK        NOMLIB           10
349900100430-----X*  ripristinata A0709
350000060711B0883X*** ex-A0709       CALLb     'BAK005C'
350100100430B0883C***                CALL      'BAK005C'
350200100430     C                   CALLb     'BAK005C'
350300060711A0709C                   PARM                    NOMLIB
350400991015A0709C                   PARM                    RtnCde005         1
350500971030     C                   IF        NOMLIB = 'ACGGAA    '
350600971030     C                   EXSR      OVRFIL
350700971030     C                   ENDIF
350800971030     C                   IF        *IN23 = *OFF
350900971030     C                   OPEN      KFSIF01L
351000971030     C                   ENDIF
351100941206      *
351200941206      * Reperimento tasti di funzione
351300940506     C                   EXSR      REPKEY
351400941206      *
351500940223     C                   ENDSR
351600940223     C/EJECT
351700941206      ************************************************************
351800941206      * REPERIMENTO TASTI DI FUNZIONE
351900941206      ************************************************************
352000950906     C     REPKEY        BEGSR
352100940506     C                   MOVEA     $FM1          $FM
352200950906     C                   MOVEA     $FC1          $FC
352300940506     C                   EXSR      REPKKK
352400940506     C                   MOVEA     $FX           $FX1
352500941206      *
352600940614     C                   ENDSR
352700940506     C/EJECT
352800941206      ************************************************************
352900941206      * REPERIMENTO TASTI DI FUNZIONE
353000941206      ************************************************************
353100940506     C     REPKKK        BEGSR
353200941206      *
353300940506     C                   CLEAR                   $FX
353400940506     C     1             DO        24            X
353500941206      *
353600940506     C     $FM(X)        IFNE      *BLANK
353700950906     C     $FC(X)        ANDNE     '0'
353800941212      *
353900940506     C                   MOVEL     $FM(X)        IDMSG
354000960930     C                   CALLB     'XRTVM'
354100940506     C                   PARM                    IDMSG            27
354200940506     C                   PARM                    TXTMSG          100
354300940506     C                   PARM                    ERRMSG            1
354400941206      *
354500940506     C     ERRMSG        IFNE      *ON
354600940506     C                   MOVEL     TXTMSG        $FX(X)
354700940506     C                   ELSE
354800940506     C                   MOVEL     *ALL' '       $FX(X)
354900940506     C                   END
355000941206      *
355100940506     C                   END
355200941206      *
355300940506     C                   END
355400941206      *
355500940506     C                   ENDSR
355600940509     C/EJECT
355700941206      ************************************************************
355800941206      * INIZIALIZZAZIONE VARIABILI
355900941206      ************************************************************
356000940223     C     INZVAR        BEGSR
356100941206      *
356200941206      * Dati da PGM guida
356300941206      *
356400941012    >C                   MOVEL     KPJBU         A008DS
356500941206      *
356600941206      * Definizione dei tasti funzione utilizzabili a pgm
356700941206      *
356800940715     C                   EXSR      DEFOPZ
356900941206      *
357000941206      * Variabili per gestione videate
357100941206      *
357200940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
357300940223     C                   MOVE      *OFF          $FINE                          fine pgm
357400940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
357500940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
357600940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
357700940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
357800941206      *
357900941206      * Variabili appoggio
358000941206      *
358100940224     C                   Z-ADD     0             CURR
358200940224     C                   Z-ADD     0             CURC
358300941206      *
358400941206      * Valorizzazione campi univoci testate
358500941206      *
358600940207     C                   CLEAR                   FMTT1
358700940207     C                   MOVEL     KNSIF         NOMSIF
358800941111     C                   MOVEL     XSCDSI        NOMDIT
358900941027     C                   MOVEL     DSPGM         NOMPGM
359000950426     C* recupero il sottotitolo (se c'è)
359100950426     C                   MOVEL     *BLANK        T1TIT
359200950426     C     SOTTIT        IFNE      *BLANK
359300950426     C                   MOVEL     SOTTIT        IDMSG
359400960930     C                   CALLB     'XRTVM'
359500950426     C                   PARM                    IDMSG            27
359600950426     C                   PARM                    TXTMSG          100
359700950426     C                   PARM                    ERRMSG            1
359800950426     C                   PARM      *ON           CTRMSG            1            centratura
359900950426     C                   PARM      30            LENMSG            3 0          lun output.
360000950426     C     ERRMSG        IFNE      *ON
360100950426     C                   MOVEL     TXTMSG        T1TIT
360200950426     C                   ELSE
360300950426     C                   MOVEL     *ALL'?'       T1TIT
360400950426     C                   ENDIF
360500950426     C                   ENDIF
360600950426     C* recupero il tipo operazione
360700940224     C                   Z-ADD     1             X
360800941012    >C     OPZ008        LOOKUP    $OZ(X)                                 21
360900940224     C     *IN21         IFEQ      *ON
361000950426     C                   MOVEL     $ID(X)        IDMSG
361100960930     C                   CALLB     'XRTVM'
361200950426     C                   PARM                    IDMSG            27
361300950426     C                   PARM                    TXTMSG          100
361400950426     C                   PARM                    ERRMSG            1
361500950426     C                   PARM      *ON           CTRMSG            1            centratura
361600950426     C                   PARM      30            LENMSG            3 0          lun output.
361700950426     C     ERRMSG        IFNE      *ON
361800950426     C                   MOVEL     TXTMSG        T1OPE
361900950426     C                   ELSE
362000950426     C                   MOVEL     *ALL'?'       T1OPE
362100950426     C                   ENDIF
362200950426     C                   ENDIF
362300941206      *
362400941206      * Inizializzazione/Decodifica prima videata
362500941206      *
362600940224     C                   EXSR      INZD1
362700940224     C                   MOVE      *OFF          $INZD1
362800941206      *
362900940117     C                   ENDSR
363000940715     C/EJECT
363100941206      ************************************************************
363200941206      * DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
363300941206      ************************************************************
363400940715     C     DEFOPZ        BEGSR
363500941206      *
363600941206      * Questa routine deve valorizzare la schiera $FC1/$FR1
363700941206      * con S o N a seconda della utilizzabilità o meno
363800941206      * del tasto funzione correlato
363900941206      *
364000941206      * Inizialmente ripristino le abilitazioni come da schiera a tempo
364100941206      * di compilazione
364200940715     C                   RESET                   $FC1
364300940715     C                   RESET                   $FR1
364400941206      *
364500941206      * Dopodichè, a seconda del richiamo e delle autorizzazioni
364600941206      * invalido alcune opzioni
364700940715     C                   SELECT
364800941206      * Se pgm richiamato in interrogazione disattivo :
364900941012    >C     OPZ008        WHENEQ    '05'
365000941206      * F4=Ricerca
365100940715     C                   Z-ADD     4             X
365200940720     C                   MOVE      '0'           $FC1(X)
365300941206      * F5=Ripristino
365400940715     C                   Z-ADD     5             X
365500940720     C                   MOVE      '0'           $FC1(X)
365600941206      * F6=Conferma
365700940715     C                   Z-ADD     06            X
365800940720     C                   MOVE      '0'           $FC1(X)
365900941206      * Se pgm richiamato in annullamento disattivo :
366000941117    >C     OPZ008        WHENEQ    '04'
366100941206      * F4=Ricerca
366200940715     C                   Z-ADD     4             X
366300940720     C                   MOVE      '0'           $FC1(X)
366400941206      * F5=Ripristino
366500940715     C                   Z-ADD     5             X
366600940720     C                   MOVE      '0'           $FC1(X)
366700941206      *
366800940715     C                   OTHER
366900941206      *
367000940715     C                   ENDSL
367100941206      *
367200941206      * Disattivo a seconda delle autorizzazioni :
367300941206    > *********  AUTXXX    IFEQ 'N'
367400941206    > *********            Z-ADD1         X
367500941206    > *********  'XX'      LOKUP$OP1,X                   21
367600941206    > *********  *IN21     IFEQ *ON
367700941206    > *********            MOVE 'N'       $FL1,X
367800941206    > *********            ENDIF
367900941206    > *********            ENDIF
368000941206      *
368100940715     C                   ENDSR
368200941206      ************************************************************
368300941206      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
368400941206      ************************************************************
368500940223     C     DEFCAM        BEGSR
368600941206      *
368700941206      * klist
368800941206      *
368900941206      *
369000941206    > * ANSIF01L
369100941012    >C     K1SIF1        KLIST
369200941012    >C                   KFLD                    SIFSOC
369300941206      *
369400941206      * Variabili per gestione videate
369500941206      *
369600940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
369700940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
369800940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
369900940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
370000940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
370100940225     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
370200941206      *
370300941206      * Variabili appoggio
370400941206      *
370500941213    >C     *LIKE         DEFINE    D1STA         WWSTA
370600941213    >C     *LIKE         DEFINE    D1NGR         WWNGR
370700941213    >C     *LIKE         DEFINE    D1LIN         WWLIN
370800941213    >C     *LIKE         DEFINE    D2PRV         WWPRV
370900950111    >C     *LIKE         DEFINE    SIFDFV        WWWDFV
371000941213      *
371100940223     C                   Z-ADD     0             CURR              2 0
371200940223     C                   Z-ADD     0             CURC              2 0
371300940223     C                   MOVE      *ZEROS        WIN              99
371400940224     C                   MOVE      *BLANK        WTBERR            1
371500980915     C                   MOVE      *BLANK        DAMG10           10
371600941206      *
371700941206      * Indici
371800941206      *
371900940223     C                   Z-ADD     0             X                 3 0
372000940223     C                   Z-ADD     0             Y                 3 0
372100941206      *
372200940117     C                   ENDSR
372300941206      ************************************************************
372400940509     C/EJECT
372500941206      ************************************************************
372600941206      * CONTROLLO ABILITAZIONE TASTI
372700941206      ************************************************************
372800940509     C     CTRKEY        BEGSR
372900940509     C                   SELECT
373000940509     C     $TASTO        WHENEQ    F01
373100940720     C     $FC(1)        IFEQ      '0'
373200940509     C                   SETON                                        99
373300940509     C                   ENDIF
373400940509     C     $TASTO        WHENEQ    F02
373500940720     C     $FC(2)        IFEQ      '0'
373600940509     C                   SETON                                        99
373700940509     C                   ENDIF
373800940509     C     $TASTO        WHENEQ    F03
373900940720     C     $FC(3)        IFEQ      '0'
374000940509     C                   SETON                                        99
374100940509     C                   ENDIF
374200940509     C     $TASTO        WHENEQ    F04
374300940720     C     $FC(4)        IFEQ      '0'
374400940509     C                   SETON                                        99
374500940509     C                   ENDIF
374600940509     C     $TASTO        WHENEQ    F05
374700940720     C     $FC(5)        IFEQ      '0'
374800940509     C                   SETON                                        99
374900940509     C                   ENDIF
375000940509     C     $TASTO        WHENEQ    F06
375100940720     C     $FC(6)        IFEQ      '0'
375200940509     C                   SETON                                        99
375300940509     C                   ENDIF
375400940509     C     $TASTO        WHENEQ    F07
375500940720     C     $FC(7)        IFEQ      '0'
375600940509     C                   SETON                                        99
375700940509     C                   ENDIF
375800940509     C     $TASTO        WHENEQ    F08
375900940720     C     $FC(8)        IFEQ      '0'
376000940509     C                   SETON                                        99
376100940509     C                   ENDIF
376200940509     C     $TASTO        WHENEQ    F09
376300940720     C     $FC(09)       IFEQ      '0'
376400940509     C                   SETON                                        99
376500940509     C                   ENDIF
376600940509     C     $TASTO        WHENEQ    F10
376700940720     C     $FC(10)       IFEQ      '0'
376800940509     C                   SETON                                        99
376900940509     C                   ENDIF
377000940509     C     $TASTO        WHENEQ    F11
377100940720     C     $FC(11)       IFEQ      '0'
377200940509     C                   SETON                                        99
377300940509     C                   ENDIF
377400940509     C     $TASTO        WHENEQ    F12
377500940720     C     $FC(12)       IFEQ      '0'
377600940509     C                   SETON                                        99
377700940509     C                   ENDIF
377800940509     C     $TASTO        WHENEQ    F13
377900940720     C     $FC(13)       IFEQ      '0'
378000940509     C                   SETON                                        99
378100940509     C                   ENDIF
378200940509     C     $TASTO        WHENEQ    F14
378300940720     C     $FC(14)       IFEQ      '0'
378400940509     C                   SETON                                        99
378500940509     C                   ENDIF
378600940509     C     $TASTO        WHENEQ    F15
378700940720     C     $FC(15)       IFEQ      '0'
378800940509     C                   SETON                                        99
378900940509     C                   ENDIF
379000940509     C     $TASTO        WHENEQ    F16
379100940720     C     $FC(16)       IFEQ      '0'
379200940509     C                   SETON                                        99
379300940509     C                   ENDIF
379400940509     C     $TASTO        WHENEQ    F17
379500940720     C     $FC(17)       IFEQ      '0'
379600940509     C                   SETON                                        99
379700940509     C                   ENDIF
379800940509     C     $TASTO        WHENEQ    F18
379900940720     C     $FC(18)       IFEQ      '0'
380000940509     C                   SETON                                        99
380100940509     C                   ENDIF
380200940509     C     $TASTO        WHENEQ    F19
380300940720     C     $FC(19)       IFEQ      '0'
380400940509     C                   SETON                                        99
380500940509     C                   ENDIF
380600940509     C     $TASTO        WHENEQ    F20
380700940720     C     $FC(20)       IFEQ      '0'
380800940509     C                   SETON                                        99
380900940509     C                   ENDIF
381000940509     C     $TASTO        WHENEQ    F21
381100940720     C     $FC(21)       IFEQ      '0'
381200940509     C                   SETON                                        99
381300940509     C                   ENDIF
381400940509     C     $TASTO        WHENEQ    F22
381500940720     C     $FC(22)       IFEQ      '0'
381600940509     C                   SETON                                        99
381700940509     C                   ENDIF
381800940509     C     $TASTO        WHENEQ    F23
381900940720     C     $FC(23)       IFEQ      '0'
382000940509     C                   SETON                                        99
382100940509     C                   ENDIF
382200940509     C     $TASTO        WHENEQ    F24
382300940720     C     $FC(24)       IFEQ      '0'
382400940509     C                   SETON                                        99
382500940509     C                   ENDIF
382600940509     C                   ENDSL
382700940509     C                   ENDSR
382800940509     C/EJECT
382900941206      ************************************************************
383000941206      * RIEMPIMENTO TASTI FUNZIONE
383100941206      ************************************************************
383200940509     C     RIEKEY        BEGSR
383300941206      *
383400941206      * Input :
383500941206      * - $FX = schiera con tutti i testi disponibili
383600941206      * - $FC = schiere con i flag di validità associati ai testi
383700941206      * - $FR = schiere con i flag di REVERSE IMAGE
383800941206      * - $ULK = ultimo testo caricato
383900941206      * - $L01 = Lunghezza prima riga
384000941206      * - $L02 = Lunghezza seconda riga
384100941206      * Output :
384200941206      * - $ULK
384300941206      * - $KEY1= 1° riga di tasti funzione
384400941206      * - $KEY2= 2° riga di tasti funzione
384500941206      *
384600960930     C                   CALLB     'XKEYMSG'
384700941206      * input
384800940509     C                   PARM                    $FX
384900940509     C                   PARM                    $FC
385000940509     C                   PARM                    $FR
385100940509     C                   PARM                    $ULK              3 0
385200941206      * output
385300940509     C                   PARM                    $KEY1            79
385400940509     C                   PARM                    $KEY2            79
385500941206    > * input
385600941206    > *************        PARM           $L01    30
385700941206    > ************         PARM           $L02    30
385800941206      *
385900060224A1148C                   PARM      79            $L01              3 0
386000060224A1148C                   PARM      79            $L02              3 0
386100060224A1148C*
386200060224A1148C                   PARM                    $ALLFUNCT       480
386300060224A1148C*
386400940509     C                   ENDSR
386500941103     C/EJECT
386600941103      ************************************************************
386700941103      * Reperimento dati società
386800941103      ************************************************************
386900941103     C     REPSOC        BEGSR
387000941103      *
387100960930     C                   CALLB     'XSOC'
387200941103     C                   PARM                    TIPXSC            6
387300941103     C                   PARM                    SOCXSC            3
387400941103     C                   PARM                    CDSXSC            9 0
387500941103     C                   PARM                    MODXSC            3
387600941103     C                   PARM      *BLANKS       RTNXSC            1
387700941103     C                   PARM                    XSOCDS
387800941103     C                   PARM                    KPJBA
387900941103      *
388000941103     C                   ENDSR
388100971030      ************************************************************
388200971030      * Ovrdbf file sistema informativi
388300971030      ************************************************************
388400971030     C     OVRFIL        BEGSR
388500971030      *
388600971030     C                   MOVE      *BLANK        WO80             80
388700971030     C                   Z-ADD     1             I1                3 0
388800971030     C     $QCM(I1)      CAT       NOMLIB:0      WO80
388900971030     C                   ADD       1             I1
389000971030     C     WO80          CAT       $QCM(I1):0    WO80
389100971030     C                   CALL      'QCMDEXC'                            23
389200971030     C                   PARM                    WO80
389300971030     C                   PARM      80            LEN              15 5
389400971030      *
389500971030     C                   ENDSR
389600971030     C*----------------------------------------------------------
389700950906**
38980095042601PROMSG    *LIBL     COS0001
38990095042602PROMSG    *LIBL     COS0002
39000097010203PROMSG    *LIBL     COS0003
39010097010204PROMSG    *LIBL     COS0227
39020095042605PROMSG    *LIBL     COS0005
390300940509** TASTI DI FUNZIONE  D1
390400940509PROMSG    *LIBL     KEY0001  01
390500941012PROMSG    *LIBL     KEY0002  02
390600940509PROMSG    *LIBL     KEY0003  03
390700940509PROMSG    *LIBL     KEY0004  04
390800940509PROMSG    *LIBL     KEY0005  05
390900940509PROMSG    *LIBL     KEY0006  06
391000940509PROMSG    *LIBL     KEY0007  07
391100940509PROMSG    *LIBL     KEY0008  08
391200940509PROMSG    *LIBL     KEY0009  09
391300940509PROMSG    *LIBL     KEY0010  10
391400940509PROMSG    *LIBL     KEY0011  11
391500940509PROMSG    *LIBL     KEY0012  12
391600940509PROMSG    *LIBL     KEY0013  13
391700940509PROMSG    *LIBL     KEY0014  14
391800940509PROMSG    *LIBL     KEY0015  15
391900940509PROMSG    *LIBL     KEY0016  16
392000940509PROMSG    *LIBL     KEY0017  17
392100940509PROMSG    *LIBL     KEY0018  18
392200940509PROMSG    *LIBL     KEY0019  19
392300940509PROMSG    *LIBL     KEY0020  20
392400940509PROMSG    *LIBL     KEY0021  21
392500940509PROMSG    *LIBL     KEY0022  22
392600940509PROMSG    *LIBL     KEY0023  23
392700940509PROMSG    *LIBL     KEY0024  24
392800940509**  ABILITAZIONE D1
3929009407200N  01
3930009407200N  02
3931009407201N  03
3932009407201N  04
3933009407201N  05
3934009407201N  06
3935009407200N  07
3936009407201N  08
3937009407200N  09
3938009412060N  10
3939009407200N  11
3940009407201N  12
3941009407200N  13
3942009407200N  14
3943009407200N  15
3944009407200N  16
3945009407200N  17
3946009407200N  18
3947009407200N  19
3948009407200N  20
3949009407200N  21
3950009407200N  22
3951009509060N  23   Impostare a '1' se si utilizzano opzioni
3952009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
395300971030**  STRINGA PER QCMDEXC
395400971030OVRDBF FILE(KFSIF01L) TOFILE(
395500971030/KFSIF01L) LVLCHK(*NO)
395600980811** Costante x XMSGR
395700980811PROMSG    *LIBL     COS1220    AVVERTIMENTO: le modifiche saranno operative dal prossimo collegament
