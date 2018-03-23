000100000000     H*------------------------------------------------------------------------*
000200000613     H* STAMPA STATISTICA INFRAGRUPPO <= 100 Kg.
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000301     FSISDR01L  IF   E           K DISK    USROPN
000900000302     FWFTER01L  UF A E           K DISK    USROPN
001000000302     FWFTER02L  IF   E           K DISK    USROPN
001100000302     F                                     RENAME(WFTER000:WFTERXXX)
001200991223     FAZORG01L  IF   E           K DISK
001300000301     FTABEL00F  IF   E           K DISK
001400000301     FTIST30P   O    E             PRINTER OFLIND(*IN01) USROPN
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* INTESTAZIONI STAMPA
002000000000     D*-------------------
002100000302     D TES             S             40    DIM(5) CTDATA PERRCD(1)              RIGHE TESTATA
002200000301     D*-------------------
002300000301     D* ELENCO TERMINAL <-> TERMINAL
002400000301     D*-------------------
002500000301     D TER             S              3  0 DIM(20) CTDATA PERRCD(1)             TERMINAL VALIDI
002600000301     D*-------------------
002700000301     D* ELENCO TERMINAL BARTOLINI
002800000301     D*-------------------
002900000301     D BAR             S              3  0 DIM(10) CTDATA PERRCD(1)             TERMINAL BARTOLINI
003000000302     D*-------------------
003100000302     D* ELENCO TERMINAL SDI
003200000302     D*-------------------
003300000302     D SDI             S              3  0 DIM(10) CTDATA PERRCD(1)             TERMINAL SDI
003400000000     D*-------------------
003500000613     D* SCHIERA FILIALI NETWORK BARTOLINI
003600000000     D*-------------------
003700000000     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
003800000000     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
003900991227     D ARE             S              3  0 DIM(500)                             *AREA
004000991227     D DVS             S              1    DIM(500)                             *DIVISIONE
004100000613     D*-------------------
004200000613     D* SCHIERA FILIALI NETWORK POSTE
004300000613     D*-------------------
004400000613     D PFIL            S              3  0 DIM(500)                             *CODICE FILIALE
004500000613     D PDFIL           S             20    DIM(500)                             *DESCRIZIONE FILIALE
004600000000     D*------------------------------------------------------------------------*
004700000000     D* INPUT
004800000000     D*------------------------------------------------------------------------*
004900000000     D*-------------------
005000000000     D* ARCHITETTURA
005100000000     D*-------------------
005200000000     D KPJBA         E DS
005300991223     D*-------------------
005400000301     D* DS LETTUTA 148° DESCRIZIONE ORGANIGRAMMA
005500000301     D*-------------------
005600000301     D OG148         E DS
005700000613     D*-------------------
005800000613     D* DS LETTUTA 143° DESCRIZIONE ORGANIGRAMMA
005900000613     D*-------------------
006000000613     D OG143         E DS
006100000301     D*-------------------
006200000301     D* DS LETTUTA TABELLA "2L"
006300000301     D*-------------------
006400000301     D DS2L          E DS
006500000000     D*-------------------
006600000000     D* DS PARAMETRI DI LANCIO PGM
006700000000     D*-------------------
006800991223     D PARAM           DS
006900991223     D  PARDAI                 1      6  0
007000991223     D  PARDAF                 7     12  0
007100000613     D  PARPT                 13     13
007200991223     D*-------------------
007300000302     D* DS VARIE SCOMPOSIZIONE DATE INIZ/FIN/WRK
007400991223     D*-------------------
007500991227     D                 DS                  INZ
007600991227     D  DIANN                  1      4  0
007700991227     D  DIMES                  5      6  0
007800991227     D  DIAAM                  1      6  0
007900991223     D                 DS                  INZ
008000000301     D  DPANN                  1      4  0
008100000301     D  DPMES                  5      6  0
008200000301     D  DPAAM                  1      6  0
008300000302     D                 DS                  INZ
008400000302     D  DXANN                  1      4  0
008500000302     D  DXMES                  5      6  0
008600000302     D  DXAAM                  1      6  0
008700000302     D  DXGIO                  7      8  0
008800000302     D  DXDAT                  1      8  0
008900000302     D                 DS                  INZ
009000000302     D  DTMES                  1      2  0
009100000303     D  DTFIL                  3      3
009200000302     D  DTAAA                  4      7  0
009300000302     D  DTDAT                  1      7
009400991223     D*-------------------
009500991223     D* DS REPERIMENTO S.I.
009600991223     D*-------------------
009700991223     D BS50DS        E DS                  EXTNAME(TIBS50DS)
009800000301     D*-------------------
009900000301     D* DS REPERIMENTO TERMINAL
010000000301     D*-------------------
010100000301     D LV55DS        E DS                  EXTNAME(FNLV55DS)
010200000301     D*-------------------
010300000000     C*------------------------------------------------------------------------*
010400991227     C* MAIN LINE
010500000000     C*------------------------------------------------------------------------*
010600000000     C*
010700000302     C* AVVIO SCRITTURA SU FILE DI WRK
010800000614     C                   EXSR      WRIWFT
010900000302     C*
011000000302     C* PRIMO    GIRO => PER PARTENZA SDI - ARRIVO BAR
011100000301     C                   EXSR      GIRO1
011200000301     C*
011300000302     C* SECONDDO GIRO => PER ARRIVI BAR - PARTENZE SDI
011400000301     C                   EXSR      GIRO2
011500000301     C*
011600000301     C                   SETON                                        LR
011700000301     C*------------------------------------------------------------------------*
011800000301     C* GIRO1 - ROUTINE PER ESECUZIONE TUTTE ROUTINE RELATIVE AL 1° GIRO
011900000301     C*------------------------------------------------------------------------*
012000000301     C     GIRO1         BEGSR
012100000301     C*
012200000301     C* VALORIZZO FLAG X DETERMINAZIONE STATO AVANZAMENTO PROCEDURA
012300000302     C                   MOVEL     '0'           FINL00            1            *FINE FILE
012400000301     C                   MOVEL     '1'           GIRO              1
012500000301     C                   MOVEL     TES(1)        STPTIT
012600000302     C                   MOVEL     TES(4)        STPTER1
012700000302     C                   MOVEL     TES(5)        STPTER2
012800000301     C*
012900000301     C* ESEGUO OPERAZIONI DI APERTURA FILE INTERESSATI A GIRO 1
013000000302     C                   OPEN      WFTER01L
013100000301     C                   OPEN      TIST30P
013200000302     C                   SETOFF                                       01
013300000302     C                   WRITE     ST30TE
013400000301     C*
013500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
013600000000     C                   EXSR      SETFIL
013700000302     C* RE-INIZIALIZZAZIONE TOTALIZZATORI
013800000302     C                   EXSR      AZZTOT
013900000301     C*
014000000301     C* CICLO FINO A FINE FILE SALDI
014100000301DO  1C     FINL00        DOWEQ     '0'                                          --- 1 -->
014200000301     C*
014300000301     C* OPERAZIONI PER NUOVO TERMINAL
014400000301     C                   EXSR      NEWTER
014500000301     C*
014600000301     C* CICLO FINO A ROTTURA DI TERMINAL
014700000301DO  2C     FINL00        DOWEQ     '0'                                          --- 2 -->
014800000301     C     TESTER        ANDEQ     DEPTER
014900000301     C*
015000000301     C* OPERAZIONI PER NUOVA FILIALE
015100000301     C                   EXSR      NEWFIL
015200000301     C*
015300000301     C* CICLO FINO A ROTTURA DI FILIALE
015400000301DO  3C     FINL00        DOWEQ     '0'                                          --- 3 -->
015500000301     C     TESTER        ANDEQ     DEPTER
015600000301     C     TESXXX        ANDEQ     DEPXXX
015700000301     C*
015800000301     C* OPERAZIONI E CONSIDERAZIONI
015900000301     C                   EXSR      ESEGUI
016000000301     C*
016100000301E   3C                   ENDDO                                                  <-- 4 ---
016200000301     C*
016300000301     C* STAMPA IL DETTAGLIO TERMINAL
016400000301     C                   EXSR      STAFIL
016500000301     C*
016600000301E   2C                   ENDDO                                                  <-- 2 ---
016700000301     C*
016800000301     C* STAMPA IL TOTALE TERMINAL
016900000301     C                   EXSR      STATER
017000000301     C*
017100000301E   1C                   ENDDO                                                  <-- 1 ---
017200000301     C*
017300000301     C* STAMPA IL TOTALE GENERALE
017400000301     C                   EXSR      STAGEN
017500000301     C*
017600000301     C* STAMPA RIGA FINE STAMPA
017700000301     C                   WRITE     ST30FI
017800000301     C*
017900000301     C* ESEGUO OPERAZIONI DI CHIUSURA FILE INTERESSATI A GIRO 1
018000000302     C                   CLOSE     WFTER01L
018100000301     C                   CLOSE     TIST30P
018200000301     C*
018300000301     C                   ENDSR
018400000301     C*------------------------------------------------------------------------*
018500000301     C* GIRO2 - ROUTINE PER ESECUZIONE TUTTE ROUTINE RELATIVE AL 2° GIRO
018600000301     C*------------------------------------------------------------------------*
018700000301     C     GIRO2         BEGSR
018800000301     C*
018900000301     C* VALORIZZO FLAG X DETERMINAZIONE STATO AVANZAMENTO PROCEDURA
019000000302     C                   MOVEL     '0'           FINL00            1            *FINE FILE
019100000301     C                   MOVEL     '2'           GIRO              1
019200000301     C                   MOVEL     TES(2)        STPTIT
019300000302     C                   MOVEL     TES(5)        STPTER1
019400000302     C                   MOVEL     TES(4)        STPTER2
019500000301     C*
019600000301     C* ESEGUO OPERAZIONI DI APERTURA FILE INTERESSATI A GIRO 1
019700000302     C                   OPEN      WFTER02L
019800000301     C                   OPEN      TIST30P
019900000302     C                   SETOFF                                       01
020000000302     C                   WRITE     ST30TE
020100000301     C*
020200000301     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
020300000301     C                   EXSR      SETFIL
020400000302     C* RE-INIZIALIZZAZIONE TOTALIZZATORI
020500000302     C                   EXSR      AZZTOT
020600000301     C*
020700000301     C* CICLO FINO A FINE FILE SALDI
020800000301DO  1C     FINL00        DOWEQ     '0'                                          --- 1 -->
020900000301     C*
021000000301     C* OPERAZIONI PER NUOVO TERMINAL
021100000301     C                   EXSR      NEWTER
021200000301     C*
021300000301     C* CICLO FINO A ROTTURA DI TERMINAL
021400000301DO  2C     FINL00        DOWEQ     '0'                                          --- 2 -->
021500000301     C     TESTER        ANDEQ     DEPTER
021600000301     C*
021700000301     C* OPERAZIONI PER NUOVA FILIALE
021800000301     C                   EXSR      NEWFIL
021900000301     C*
022000000301     C* CICLO FINO A ROTTURA DI FILIALE
022100000301DO  3C     FINL00        DOWEQ     '0'                                          --- 3 -->
022200000301     C     TESTER        ANDEQ     DEPTER
022300000301     C     TESXXX        ANDEQ     DEPXXX
022400000301     C*
022500000301     C* OPERAZIONI E CONSIDERAZIONI
022600000301     C                   EXSR      ESEGUI
022700000301     C*
022800000301E   3C                   ENDDO                                                  <-- 4 ---
022900000301     C*
023000000301     C* STAMPA IL DETTAGLIO TERMINAL
023100000301     C                   EXSR      STAFIL
023200000301     C*
023300000301E   2C                   ENDDO                                                  <-- 2 ---
023400000301     C*
023500000301     C* STAMPA IL TOTALE TERMINAL
023600000301     C                   EXSR      STATER
023700000301     C*
023800000301E   1C                   ENDDO                                                  <-- 1 ---
023900000301     C*
024000000301     C* STAMPA IL TOTALE GENERALE
024100000301     C                   EXSR      STAGEN
024200000301     C*
024300000301     C* STAMPA RIGA FINE STAMPA
024400000301     C                   WRITE     ST30FI
024500000301     C*
024600000301     C* ESEGUO OPERAZIONI DI CHIUSURA FILE INTERESSATI A GIRO 1
024700000302     C                   CLOSE     WFTER02L
024800000301     C                   CLOSE     TIST30P
024900000301     C*
025000000301     C                   ENDSR
025100000302     C*------------------------------------------------------------------------*
025200000302     C* AZZTOT - AZZERAMENTO TOTALIZZATORI
025300000302     C*------------------------------------------------------------------------*
025400000302     C     AZZTOT        BEGSR
025500000302     C*
025600000302     C* AZZERA I TOTALI DELLA FILIALE
025700000302     C                   Z-ADD     *ZEROS        TFSPE             8 0
025800000302     C                   Z-ADD     *ZEROS        TFPES            11 3
025900000302     C                   Z-ADD     *ZEROS        TFVOL            11 3
026000000302     C                   Z-ADD     *ZEROS        TFCOL             9 0
026100000302     C                   Z-ADD     *ZEROS        TFSPE2            8 0
026200000302     C                   Z-ADD     *ZEROS        TFPES2           11 3
026300000302     C                   Z-ADD     *ZEROS        TFVOL2           11 3
026400000302     C                   Z-ADD     *ZEROS        TFCOL2            9 0
026500000302     C                   Z-ADD     *ZEROS        TFSPET            8 0
026600000302     C                   Z-ADD     *ZEROS        TFPEST           11 3
026700000302     C                   Z-ADD     *ZEROS        TFVOLT           11 3
026800000302     C                   Z-ADD     *ZEROS        TFCOLT            9 0
026900000302     C*
027000000302     C* AZZERA I TOTALI DEL TERMINAL
027100000302     C                   Z-ADD     *ZEROS        TTSPE             8 0
027200000302     C                   Z-ADD     *ZEROS        TTPES            11 3
027300000302     C                   Z-ADD     *ZEROS        TTVOL            11 3
027400000302     C                   Z-ADD     *ZEROS        TTCOL             9 0
027500000302     C                   Z-ADD     *ZEROS        TTSPE2            8 0
027600000302     C                   Z-ADD     *ZEROS        TTPES2           11 3
027700000302     C                   Z-ADD     *ZEROS        TTVOL2           11 3
027800000302     C                   Z-ADD     *ZEROS        TTCOL2            9 0
027900000302     C                   Z-ADD     *ZEROS        TTSPET            8 0
028000000302     C                   Z-ADD     *ZEROS        TTPEST           11 3
028100000302     C                   Z-ADD     *ZEROS        TTVOLT           11 3
028200000302     C                   Z-ADD     *ZEROS        TTCOLT            9 0
028300000302     C*
028400000302     C* AZZERA I TOTALI GENERALI
028500000302     C                   Z-ADD     *ZEROS        TGSPE             8 0
028600000302     C                   Z-ADD     *ZEROS        TGPES            11 3
028700000302     C                   Z-ADD     *ZEROS        TGVOL            11 3
028800000302     C                   Z-ADD     *ZEROS        TGCOL             9 0
028900000302     C                   Z-ADD     *ZEROS        TGSPE2            8 0
029000000302     C                   Z-ADD     *ZEROS        TGPES2           11 3
029100000302     C                   Z-ADD     *ZEROS        TGVOL2           11 3
029200000302     C                   Z-ADD     *ZEROS        TGCOL2            9 0
029300000302     C                   Z-ADD     *ZEROS        TGSPET            8 0
029400000302     C                   Z-ADD     *ZEROS        TGPEST           11 3
029500000302     C                   Z-ADD     *ZEROS        TGVOLT           11 3
029600000302     C                   Z-ADD     *ZEROS        TGCOLT            9 0
029700000302     C*
029800000302     C                   ENDSR
029900991228     C*------------------------------------------------------------------------*
030000000301     C* ESEGUI - OPERAZIONI E CONSIDERAZIONI PRINCIPALI
030100991228     C*------------------------------------------------------------------------*
030200991228     C     ESEGUI        BEGSR
030300991228     C*
030400000301     C* MEMORIZZA I SALDI PER TERMINAL
030500000301     C                   EXSR      MEMTER
030600991228     C*
030700000301     C* LEGGE IL RECORD SUCCESSIVO DEI SALDI
030800000301     C                   EXSR      LETFIL
030900991228     C*
031000000301     C                   ENDSR
031100000000     C*------------------------------------------------------------------------*
031200000301     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
031300000000     C*------------------------------------------------------------------------*
031400000000     C     SETFIL        BEGSR
031500000000     C*
031600000301     C                   Z-ADD     PARDAI        DIAAM
031700000301     C*
031800991227     C* POSIZIONAMENTO A INIZIO FILE AZORG02L
031900000301     C     GIRO          IFEQ      '1'
032000000302     C     DIANN         SETLL     WFTER01L                           89        *FINE-ARCHIV
032100000301     C                   ENDIF
032200000000     C*
032300000301     C     GIRO          IFEQ      '2'
032400000302     C     DIANN         SETLL     WFTER02L                           89        *FINE-ARCHIV
032500000301     C                   ENDIF
032600000301     C*
032700991223     C     *IN89         IFEQ      '1'
032800000000     C                   MOVEL     '1'           FINL00
032900000000     C                   ELSE
033000000000     C                   EXSR      LETFIL
033100991223     C                   ENDIF
033200000000     C*
033300000000     C                   ENDSR
033400000000     C*------------------------------------------------------------------------*
033500991223     C* LETFIL - LETTURA PROSSIMO RECORD
033600000000     C*------------------------------------------------------------------------*
033700991227     C     LETFIL        BEGSR
033800000000     C*
033900000000     C* LEGGO FINO A:
034000991223     C*     - FINE ORGANIGRAMMA
034100991227     C*     - RECORD ORGANIGRAMMA VALIDO
034200000301     C                   MOVEL     'N'           WRECOK
034300991227     C     FINL00        DOWEQ     '0'                                          --- 1 -->
034400000301     C     WRECOK        ANDEQ     'N'                                          --- 1 -->
034500000000     C*
034600991223     C* LETTURE SUCCESSIVE
034700000301     C     GIRO          IFEQ      '1'
034800000302     C                   READ      WFTER01L                               89    *FINE-ARCHIV
034900000301     C                   ENDIF
035000000301     C*
035100000301     C     GIRO          IFEQ      '2'
035200000302     C                   READ      WFTER02L                               89    *FINE-ARCHIV
035300000301     C                   ENDIF
035400000000     C*
035500991223     C     *IN89         IFEQ      '1'                                          *FINE FILE
035600000000     C                   MOVEL     '1'           FINL00
035700000000     C                   ELSE
035800000301     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
035900000301     C                   ENDIF
036000000000     C*
036100991227     C                   ENDDO                                                  <-- 1 ---
036200000000     C*
036300000000     C                   ENDSR
036400000000     C*------------------------------------------------------------------------*
036500991223     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
036600000000     C*------------------------------------------------------------------------*
036700991025     C     CHKREC        BEGSR
036800000000     C*
036900000000     C                   MOVEL     'S'           WRECOK            1
037000000613     C*
037100000613     C* VERIFICO LA FILIALE CORRENTE RISPETTO AI PARAMETRI DI LANCIO
037200000613     C* (SOLO POSTE - SOLO BARTOLINI)
037300000613     C     WRECOK        IFEQ      'S'
037400000613     C     PARPT         IFEQ      'S'
037500000613     C     WFTFLA        LOOKUP    PFIL                                   45
037600000613     C     WFTFLP        LOOKUP    PFIL                                   55
037700000613     C                   IF        *IN45=*ON OR *IN55=*ON
037800000613     C                   ELSE
037900000613     C                   MOVEL     'N'           WRECOK
038000000613     C                   ENDIF
038100000613     C                   ELSE
038200000613     C     WFTFLA        LOOKUP    FIL                                    45
038300000613     C     WFTFLP        LOOKUP    FIL                                    55
038400000613     C                   IF        *IN45=*ON AND *IN55=*ON
038500000613     C                   ELSE
038600000613     C                   MOVEL     'N'           WRECOK
038700000613     C                   ENDIF
038800000613     C                   ENDIF
038900000613     C                   ENDIF
039000000613     C*
039100000613     C* SE NON RIENTRA NEL PERIODO RICHIESTO ESCLUDO IL RECORD
039200000613     C                   Z-ADD     WFTANN        DPANN
039300000613     C                   Z-ADD     WFTMES        DPMES
039400000613     C*
039500000302     C     WRECOK        IFEQ      'S'
039600000302     C     DPAAM         IFGE      PARDAI
039700000302     C     DPAAM         ANDLE     PARDAF
039800000302     C                   ELSE
039900000302     C                   MOVEL     'N'           WRECOK
040000000302     C                   ENDIF
040100000302     C                   ENDIF
040200991223     C*
040300000301     C* PER ROTTURE DI CODICE MEMORIZZO O FILIALE DI PARTENZA O FILIALE DI ARRIVO
040400000301     C* IN BASE A QUALE "GIRO" E' IL CORRENTE
040500000301     C     WRECOK        IFEQ      'S'
040600000301     C     GIRO          IFEQ      '1'
040700000302     C                   Z-ADD     WFTFLP        TESTER
040800000302     C                   Z-ADD     WFTFLA        TESXXX
040900000301     C                   ENDIF
041000000301     C     GIRO          IFEQ      '2'
041100000302     C                   Z-ADD     WFTFLA        TESTER
041200000302     C                   Z-ADD     WFTFLP        TESXXX
041300000301     C                   ENDIF
041400000301     C                   ENDIF
041500000000     C*
041600000000     C                   ENDSR
041700991227     C*------------------------------------------------------------------------*
041800000301     C* MEMTER - MEMORIZZA LE SPEDIZIONI
041900991227     C*------------------------------------------------------------------------*
042000000301     C     MEMTER        BEGSR
042100991227     C*
042200991227     C     WRECOK        IFEQ      'S'
042300000301     C*
042400000301     C* DETERMINO IN QUALE FASCIA DI PESO SOMMARE
042500000302     C     WFTFPE        IFEQ      '01'
042600991223     C*
042700000301     C* TOTALE FILIALE
042800000302     C                   ADD       WFTNSP        TFSPE
042900000302     C                   ADD       WFTPKG        TFPES
043000000302     C                   ADD       WFTNCO        TFCOL
043100000302     C                   ADD       WFTVOL        TFVOL
043200000302     C*
043300000302     C* TOTALE TERMINAL
043400000302     C                   ADD       WFTNSP        TTSPE
043500000302     C                   ADD       WFTPKG        TTPES
043600000302     C                   ADD       WFTNCO        TTCOL
043700000302     C                   ADD       WFTVOL        TTVOL
043800000302     C*
043900000302     C* TOTALE GENERALE
044000000302     C                   ADD       WFTNSP        TGSPE             8 0
044100000302     C                   ADD       WFTPKG        TGPES            11 3
044200000302     C                   ADD       WFTNCO        TGCOL             9 0
044300000302     C                   ADD       WFTVOL        TGVOL            11 3
044400991223     C*
044500000301     C                   ELSE
044600000301     C*
044700000301     C* TOTALE FILIALE
044800000302     C                   ADD       WFTNSP        TFSPE2
044900000302     C                   ADD       WFTPKG        TFPES2
045000000302     C                   ADD       WFTNCO        TFCOL2
045100000302     C                   ADD       WFTVOL        TFVOL2
045200000302     C* TOTALE TERMINAL
045300000302     C                   ADD       WFTNSP        TTSPE2
045400000302     C                   ADD       WFTPKG        TTPES2
045500000302     C                   ADD       WFTNCO        TTCOL2
045600000302     C                   ADD       WFTVOL        TTVOL2
045700000302     C*
045800000302     C* TOTALE GENERALE
045900000302     C                   ADD       WFTNSP        TGSPE2            8 0
046000000302     C                   ADD       WFTPKG        TGPES2           11 3
046100000302     C                   ADD       WFTNCO        TGCOL2            9 0
046200000302     C                   ADD       WFTVOL        TGVOL2           11 3
046300000301     C*
046400000301     C                   ENDIF
046500000302     C*
046600000302     C* TOTALE FILIALE
046700000302     C                   ADD       WFTNSP        TFSPET
046800000302     C                   ADD       WFTPKG        TFPEST
046900000302     C                   ADD       WFTNCO        TFCOLT
047000000302     C                   ADD       WFTVOL        TFVOLT
047100000302     C*
047200000302     C* TOTALE TERMINAL
047300000302     C                   ADD       WFTNSP        TTSPET
047400000302     C                   ADD       WFTPKG        TTPEST
047500000302     C                   ADD       WFTNCO        TTCOLT
047600000302     C                   ADD       WFTVOL        TTVOLT
047700000302     C*
047800000302     C* TOTALE GENERALE
047900000302     C                   ADD       WFTNSP        TGSPET            8 0
048000000302     C                   ADD       WFTPKG        TGPEST           11 3
048100000302     C                   ADD       WFTNCO        TGCOLT            9 0
048200000302     C                   ADD       WFTVOL        TGVOLT           11 3
048300000301     C*
048400000301     C                   ENDIF
048500000128     C*
048600991227     C                   ENDSR
048700000301     C*------------------------------------------------------------------------*
048800000301     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
048900000301     C*------------------------------------------------------------------------*
049000000301     C     NEWFIL        BEGSR
049100000301     C*
049200000301     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
049300000301     C                   Z-ADD     TESXXX        DEPXXX
049400000301     C*
049500000301     C* --------- OPERAZIONI PER DATI DI "ARRIVO" -----------
049600000301     C* DECODIFICA LA FILIALE
049700000302     C                   MOVEL     TESXXX        TERMINAL
049800000302     C                   EXSR      DECTER
049900000302     C*
050000000302     C* --------- OPERAZIONI PER DATI DI "PARTENZA" -----------
050100000302     C* IMPOSTAZIONI CAMPI IN TESTATA
050200000302     C                   MOVEL(P)  DEPXXX        STPTAC
050300000301     C                   MOVEL(P)  DESFIL        STPTAD
050400000301     C*
050500000301     C* AZZERA I TOTALI DELLA FILIALE
050600000301     C                   Z-ADD     *ZEROS        TFSPE             8 0
050700000301     C                   Z-ADD     *ZEROS        TFPES            11 3
050800000301     C                   Z-ADD     *ZEROS        TFVOL            11 3
050900000301     C                   Z-ADD     *ZEROS        TFCOL             9 0
051000000301     C                   Z-ADD     *ZEROS        TFSPE2            8 0
051100000301     C                   Z-ADD     *ZEROS        TFPES2           11 3
051200000301     C                   Z-ADD     *ZEROS        TFVOL2           11 3
051300000301     C                   Z-ADD     *ZEROS        TFCOL2            9 0
051400000302     C                   Z-ADD     *ZEROS        TFSPET            8 0
051500000302     C                   Z-ADD     *ZEROS        TFPEST           11 3
051600000302     C                   Z-ADD     *ZEROS        TFVOLT           11 3
051700000302     C                   Z-ADD     *ZEROS        TFCOLT            9 0
051800000301     C*
051900000301     C                   ENDSR
052000000302     C*------------------------------------------------------------------------*
052100000301     C* NEWTER - OPERAZIONI PER NUOVA TERMINAL
052200000000     C*------------------------------------------------------------------------*
052300000301     C     NEWTER        BEGSR
052400991227     C*
052500000301     C* MEMORIZZA IL NUOVO TERMINAL IN UN DEPOSITO
052600000301     C                   Z-ADD     TESTER        DEPTER
052700000301     C*
052800000301     C* --------- OPERAZIONI PER DATI DI "PARTENZA" -----------
052900000301     C* DECODIFICA IL TERMINAL
053000000301     C                   MOVEL     TESTER        TERMINAL
053100000301     C                   EXSR      DECTER
053200991124     C*
053300991124     C* IMPOSTAZIONI CAMPI IN TESTATA
053400000301     C                   MOVEL(P)  DEPTER        STPTPC
053500000301     C                   MOVEL(P)  DESFIL        STPTPD
053600000302     C                   MOVEL(P)  STPTPD        SAVTPD
053700000000     C*
053800000301     C* AZZERA I TOTALI DEL TERMINAL
053900000301     C                   Z-ADD     *ZEROS        TTSPE             8 0
054000000301     C                   Z-ADD     *ZEROS        TTPES            11 3
054100000301     C                   Z-ADD     *ZEROS        TTVOL            11 3
054200000301     C                   Z-ADD     *ZEROS        TTCOL             9 0
054300000301     C                   Z-ADD     *ZEROS        TTSPE2            8 0
054400000301     C                   Z-ADD     *ZEROS        TTPES2           11 3
054500000301     C                   Z-ADD     *ZEROS        TTVOL2           11 3
054600000301     C                   Z-ADD     *ZEROS        TTCOL2            9 0
054700000302     C                   Z-ADD     *ZEROS        TTSPET            8 0
054800000302     C                   Z-ADD     *ZEROS        TTPEST           11 3
054900000302     C                   Z-ADD     *ZEROS        TTVOLT           11 3
055000000302     C                   Z-ADD     *ZEROS        TTCOLT            9 0
055100000000     C*
055200991227     C                   ENDSR
055300991230     C*------------------------------------------------------------------------*
055400000301     C* STAFIL - STAMPA TOTALE FILIALE
055500991230     C*------------------------------------------------------------------------*
055600000301     C     STAFIL        BEGSR
055700991230     C*
055800000301     C                   Z-ADD     TFSPE         STPSPE
055900000301     C                   Z-ADD     TFPES         STPPES
056000000301     C                   Z-ADD     TFVOL         STPVOL
056100000301     C                   Z-ADD     TFCOL         STPCOL
056200000301     C                   Z-ADD     TFSPE2        STPSPE2
056300000301     C                   Z-ADD     TFPES2        STPPES2
056400000301     C                   Z-ADD     TFVOL2        STPVOL2
056500000301     C                   Z-ADD     TFCOL2        STPCOL2
056600000302     C                   Z-ADD     TFSPET        STPSPET
056700000302     C                   Z-ADD     TFPEST        STPPEST
056800000302     C                   Z-ADD     TFVOLT        STPVOLT
056900000302     C                   Z-ADD     TFCOLT        STPCOLT
057000991230     C*
057100991230     C* STAMPO RIGA DETTAGLIO
057200000302     C     STPTPC        IFEQ      SAVTPC
057300000302     C                   MOVEL     *BLANKS       STPTPC
057400000302     C                   MOVEL     TES(3)        STPTPD
057500000302     C                   ELSE
057600000302     C                   MOVEL     STPTPC        SAVTPC
057700000302     C                   ENDIF
057800000301     C*
057900000301     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
058000000301     C     *IN01         IFEQ      '1'
058100000301     C                   WRITE     ST30RI                                       *RIGA DI SEPARAZIONE
058200000301     C                   WRITE     ST30TE
058300000301     C                   SETOFF                                       01
058400000301     C                   ENDIF
058500000301     C*
058600000301     C* STAMPO RIGA DETTAGLIO
058700000301     C                   WRITE     ST30DE
058800000301     C*
058900991230     C                   ENDSR
059000000301     C*------------------------------------------------------------------------*
059100000301     C* STATER - STAMPA TOTALE TERMINAL
059200000301     C*------------------------------------------------------------------------*
059300000301     C     STATER        BEGSR
059400000301     C*
059500000301     C*
059600000301     C                   MOVEL     'TOT'         STPTPC
059700000302     C                   MOVEL     SAVTPD        STPTPD
059800000302     C                   MOVEL     *BLANKS       STPTAC
059900000302     C                   MOVEL     *BLANKS       STPTAD
060000000302     C                   Z-ADD     TTSPE         STPSPE
060100000301     C                   Z-ADD     TTPES         STPPES
060200000301     C                   Z-ADD     TTVOL         STPVOL
060300000301     C                   Z-ADD     TTCOL         STPCOL
060400000302     C                   Z-ADD     TTSPE2        STPSPE2
060500000301     C                   Z-ADD     TTPES2        STPPES2
060600000301     C                   Z-ADD     TTVOL2        STPVOL2
060700000301     C                   Z-ADD     TTCOL2        STPCOL2
060800000302     C                   Z-ADD     TTSPET        STPSPET
060900000302     C                   Z-ADD     TTPEST        STPPEST
061000000302     C                   Z-ADD     TTVOLT        STPVOLT
061100000302     C                   Z-ADD     TTCOLT        STPCOLT
061200000301     C*
061300000301     C* STAMPO RIGA TOTALE TERMINAL
061400000301     C*
061500000301     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
061600000301     C     *IN01         IFEQ      '1'
061700000301     C                   WRITE     ST30RI                                       *RIGA DI SEPARAZIONE
061800000301     C                   WRITE     ST30TE
061900000301     C                   SETOFF                                       01
062000000301     C                   ENDIF
062100000301     C*
062200000301     C* STAMPO RIGA DETTAGLIO
062300000303     C                   WRITE     ST30RI
062400000303     C                   SETON                                        04
062500000301     C                   WRITE     ST30DE
062600000303     C                   SETOFF                                       04
062700000303     C                   WRITE     ST30RI
062800000301     C*
062900000301     C                   ENDSR
063000000301     C*------------------------------------------------------------------------*
063100000301     C* STAGEN - STAMPA TOTALE GENERALE
063200000301     C*------------------------------------------------------------------------*
063300000301     C     STAGEN        BEGSR
063400000301     C*
063500000301     C*
063600000301     C                   MOVEL     'TOT'         STPTPC
063700000302     C                   MOVEL(P)  'GENERALE'    STPTPD
063800000302     C                   MOVEL     *BLANKS       STPTAC
063900000302     C                   MOVEL     *BLANKS       STPTAD
064000000302     C                   Z-ADD     TGSPE         STPSPE
064100000301     C                   Z-ADD     TGPES         STPPES
064200000301     C                   Z-ADD     TGVOL         STPVOL
064300000301     C                   Z-ADD     TGCOL         STPCOL
064400000302     C                   Z-ADD     TGSPE2        STPSPE2
064500000301     C                   Z-ADD     TGPES2        STPPES2
064600000301     C                   Z-ADD     TGVOL2        STPVOL2
064700000301     C                   Z-ADD     TGCOL2        STPCOL2
064800000302     C                   Z-ADD     TGSPET        STPSPET
064900000302     C                   Z-ADD     TGPEST        STPPEST
065000000302     C                   Z-ADD     TGVOLT        STPVOLT
065100000302     C                   Z-ADD     TGCOLT        STPCOLT
065200000301     C*
065300000301     C* STAMPO RIGA TOTALE GENERALE
065400000301     C*
065500000301     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
065600000301     C     *IN01         IFEQ      '1'
065700000301     C                   WRITE     ST30RI                                       *RIGA DI SEPARAZIONE
065800000301     C                   WRITE     ST30TE
065900000301     C                   SETOFF                                       01
066000000301     C                   ELSE
066100000301     C                   WRITE     ST30RI                                       *RIGA DI SEPARAZIONE
066200000301     C                   ENDIF
066300000301     C*
066400000301     C* STAMPO RIGA DETTAGLIO
066500000303     C                   SETON                                        04
066600000301     C                   WRITE     ST30DE
066700000303     C                   SETOFF                                       04
066800000301     C                   WRITE     ST30RI                                       *RIGA DI SEPARAZIONE
066900000301     C*
067000000301     C                   ENDSR
067100000000     C*------------------------------------------------------------------------*
067200000301     C* DECTER - DECODIFICA TERMINAL
067300000000     C*------------------------------------------------------------------------*
067400000301     C     DECTER        BEGSR
067500000000     C*
067600000000     C                   Z-ADD     1             I
067700000301     C     TERMINAL      LOOKUP    FIL(I)                                 95
067800000301     C     *IN95         IFEQ      *OFF
067900000000     C                   MOVEL     *ALL'*'       DESFIL           20
068000000000     C                   ELSE
068100000000     C                   MOVEL     DFIL(I)       DESFIL
068200000000     C                   END
068300991026     C*
068400991123     C                   ENDSR
068500000000     C*------------------------------------------------------------------------*
068600000000     C* *INZSR - ROUTINE INIZIALE
068700000000     C*------------------------------------------------------------------------*
068800000000     C     *INZSR        BEGSR
068900000000     C*--------------------
069000000000     C* RICEVIMENTO PARAMENTRI
069100000000     C*--------------------
069200000000     C     *ENTRY        PLIST
069300000000     C                   PARM                    KPJBA
069400000000     C                   MOVEL     KPJBU         PARAM
069500991227     C*
069600000000     C*--------------------
069700000000     C* CAMPI RIFERITI AL DATA BASE
069800000000     C*--------------------
069900000301     C     *LIKE         DEFINE    TBLKUT        KTBUTE
070000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
070100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
070200000301     C     *LIKE         DEFINE    ORGFIL        TERMINAL
070300000301     C     *LIKE         DEFINE    ORGFIL        TESTER
070400000301     C     *LIKE         DEFINE    ORGFIL        TESXXX
070500000301     C     *LIKE         DEFINE    ORGFIL        DEPTER
070600000301     C     *LIKE         DEFINE    ORGFIL        DEPXXX
070700000302     C     *LIKE         DEFINE    STPTPC        SAVTPC
070800000302     C     *LIKE         DEFINE    STPTPD        SAVTPD
070900000302     C     *LIKE         DEFINE    WFTANN        KWTANN
071000000302     C     *LIKE         DEFINE    WFTMES        KWTMES
071100000301     C*
071200000301     C*--------------------
071300000301     C* CHIAVI DI LETTURA
071400000301     C*--------------------
071500000301     C* CHIAVE LETTURA TABEL00F - COMPLETA
071600000301     C     KEYTAB        KLIST
071700000301     C                   KFLD                    KTBUTE
071800000301     C                   KFLD                    KTBCOD
071900000301     C                   KFLD                    KTBKEY
072000000301     C*
072100000302     C* CHIAVE LETTURA WFTER01L - COMPLETA
072200000302     C     KEYWFT        KLIST
072300000302     C                   KFLD                    KWTANN
072400000302     C                   KFLD                    KWTMES
072500000302     C*
072600000000     C*--------------------
072700000000     C* CAMPI DI CONTROLLO
072800000000     C*--------------------
072900000302     C                   MOVEL     *BLANKS       SAVTPC
073000000302     C                   MOVEL     *BLANKS       SAVTPD
073100000303     C*--------------------
073200000303     C* IMPOSTAZIONI IN STAMPA PARAMETRI DI LANCIO
073300000303     C*--------------------
073400000303     C                   MOVEL     '/'           DTFIL
073500000303     C                   MOVEL     PARDAI        DTAAA
073600000303     C                   MOVE      PARDAI        DTMES
073700000303     C                   MOVEL     DTDAT         STPDAI
073800000303     C                   MOVEL     PARDAF        DTAAA
073900000303     C                   MOVE      PARDAF        DTMES
074000000303     C                   MOVEL     DTDAT         STPDAF
074100000613     C*
074200000613     C                   IF        PARPT='S'
074300000613     C                   EVAL      STPPT='(POSTE)'
074400000613     C                   ELSE
074500000613     C                   EVAL      STPPT=' (BAR) '
074600000613     C                   ENDIF
074700000302     C*
074800000302     C* CANCELLA PREVENTIVAMENTE DAL FILE DI WRK IL PERIODO DI LANCIO
074900000614     C                   EXSR      CANWFT
075000000000     C*
075100000000     C* CARICA ORGANIGRAMMA
075200000000     C                   EXSR      CARORG
075300000000     C*
075400000000     C                   ENDSR
075500000302     C*------------------------------------------------------------------------*
075600000302     C* WRIWFT - RIEMPIMENYTO WRK-FILE
075700000302     C*------------------------------------------------------------------------*
075800000302     C     WRIWFT        BEGSR
075900000302     C*
076000000302     C                   OPEN      WFTER01L
076100000302     C                   OPEN      SISDR01L
076200000302     C*
076300000302     C                   Z-ADD     PARDAI        DIAAM
076400000302     C*
076500000302     C     DIANN         SETLL     SISDR01L                           86
076600000303     C                   READ      SISDR01L                               86
076700000302     C     *IN86         DOWEQ     *OFF
076800000302     C                   EXSR      CHKWRI
076900000302     C     WRECOK        IFEQ      'S'
077000000302     C                   Z-ADD     SDRANN        WFTANN
077100000302     C                   Z-ADD     SDRMES        WFTMES
077200000302     C                   Z-ADD     SDRNSP        WFTNSP
077300000302     C                   Z-ADD     SDRPKG        WFTPKG
077400000302     C                   Z-ADD     SDRNCO        WFTNCO
077500000302     C                   Z-ADD     SDRVOL        WFTVOL
077600000302     C                   WRITE     WFTER000
077700000302     C                   ENDIF
077800000302     C                   READ      SISDR01L                               86
077900000302     C                   ENDDO
078000000302     C*
078100000302     C                   CLOSE     WFTER01L
078200000302     C                   CLOSE     SISDR01L
078300000302     C*
078400000302     C                   ENDSR
078500000302     C*------------------------------------------------------------------------*
078600000302     C* CHKWRI - CONTROLLA VALIDITA' DEL RECORD DA SCRIVERE IN WRK-FILE
078700000302     C*------------------------------------------------------------------------*
078800000302     C     CHKWRI        BEGSR
078900000302     C*
079000000302     C                   MOVEL     'S'           WRECOK            1
079100000302     C*
079200000302     C                   Z-ADD     SDRANN        DPANN
079300000302     C                   Z-ADD     SDRMES        DPMES
079400000302     C*
079500000302     C* SE NON RIENTRA NEL PERIODO RICHIESTO ESCLUDO IL RECORD
079600000302     C     WRECOK        IFEQ      'S'
079700000302     C     DPAAM         IFGE      PARDAI
079800000302     C     DPAAM         ANDLE     PARDAF
079900000302     C                   ELSE
080000000302     C                   MOVEL     'N'           WRECOK
080100000302     C                   ENDIF
080200000302     C                   ENDIF
080300000302     C*
080400000302     C* CONTROLLO PER TIPO RAGGR. BOLLE
080500000302     C     WRECOK        IFEQ      'S'
080600000303     C     SDRRBL        IFEQ      'N'
080700000303     C     SDRRBL        OREQ      'S'
080800000303     C                   ELSE
080900000302     C                   MOVEL     'N'           WRECOK
081000000302     C                   ENDIF
081100000302     C                   ENDIF
081200000302     C*
081300000302     C* ESCLUDO I SALDI PER SCAGLIONI NON DA 0 A 100 KG.
081400000302     C     WRECOK        IFEQ      'S'
081500000302     C*
081600000302     C* DECODIFICA FASCE DI PESO
081700020326     C     DPAAM         IFLE      199812                                       *FASCE FINO AL '98
081800000302     C                   MOVEL     'SDR'         CHIAVE            5
081900020328     C                   ELSE                                                   *FASCE DAL '99
082000020326     C                   MOVEL     'S9R'         CHIAVE
082100020326     C                   ENDIF
082200000302     C                   MOVE      SDRFPE        CHIAVE
082300000302     C                   Z-ADD     1             KTBUTE
082400000302     C                   MOVEL     '2L'          KTBCOD
082500000302     C                   MOVEL     CHIAVE        KTBKEY
082600000302     C     KEYTAB        CHAIN     TABEL00F                           99
082700000302     C     *IN99         IFEQ      *OFF
082800000302     C                   MOVEL     TBLUNI        DS2L
082900000302     C     §2LKGI        IFGE      0
083000000302     C     §2LKGF        ANDLE     50
083100000302     C                   MOVEL     '01'          WFTFPE
083200000302     C                   ELSE
083300000302     C     §2LKGI        IFGE      50
083400000302     C     §2LKGF        ANDLE     100
083500000302     C                   MOVEL     '02'          WFTFPE
083600000302     C                   ELSE
083700000302     C                   MOVEL     'N'           WRECOK
083800000302     C                   ENDIF
083900000302     C                   ENDIF
084000000302     C                   ELSE
084100000302     C                   MOVEL     'N'           WRECOK
084200000302     C                   ENDIF
084300000302     C                   ENDIF
084400000302     C*
084500000302     C* CONTROLLO SE LE FILIALI PRESENTI NEI SALDI SONO DA CONSIDERARE
084600000302     C     WRECOK        IFEQ      'S'
084700000302     C                   CLEAR                   LV55DS
084800000302     C                   Z-ADD     SDRANN        DXANN
084900000302     C                   Z-ADD     SDRMES        DXMES
085000000302     C                   Z-ADD     01            DXGIO
085100000302     C                   Z-ADD     DXDAT         D55DRF
085200000302     C                   Z-ADD     SDRFLP        D55LIN
085300000302     C                   CALL      'FNLV55R'
085400000302     C                   PARM                    LV55DS
085500000302     C     D55ERR        IFEQ      *BLANKS
085600000302     C                   SETOFF                                       11
085700000302     C     D55TFP        LOOKUP    SDI                                    11
085800000302     C     *IN11         IFEQ      *OFF
085900000302     C                   MOVEL     'N'           WRECOK
086000000302     C                   ELSE
086100000302     C                   Z-ADD     D55TFP        WFTFLP
086200000302     C                   ENDIF
086300000302     C                   ELSE
086400000302     C                   MOVEL     'N'           WRECOK
086500000302     C                   ENDIF
086600000302     C*
086700000302     C                   Z-ADD     SDRFLA        D55LIN
086800000302     C                   CALL      'FNLV55R'
086900000302     C                   PARM                    LV55DS
087000000302     C     D55ERR        IFEQ      *BLANKS
087100000302     C                   SETOFF                                       11
087200000302     C     D55TFA        LOOKUP    BAR                                    11
087300000302     C     *IN11         IFEQ      *OFF
087400000302     C                   MOVEL     'N'           WRECOK
087500000302     C                   ELSE
087600000302     C                   Z-ADD     D55TFA        WFTFLA
087700000302     C                   ENDIF
087800000302     C                   ELSE
087900000302     C                   MOVEL     'N'           WRECOK
088000000302     C                   ENDIF
088100000302     C*
088200000302     C                   ENDIF
088300000302     C*
088400000302     C                   ENDSR
088500000302     C*------------------------------------------------------------------------*
088600000302     C* CANWFT - CANCELLAZIONE WRK-FILE X PERIODO
088700000302     C*------------------------------------------------------------------------*
088800000302     C     CANWFT        BEGSR
088900000302     C*
089000000302     C                   OPEN      WFTER01L
089100000302     C*
089200000302     C                   MOVEL     PARDAI        KWTANN
089300000302     C                   MOVE      PARDAI        KWTMES
089400000302     C     KEYWFT        SETLL     WFTER01L                           87
089500000302     C     *IN87         DOWEQ     *OFF
089600000614     C                   READ      WFTER01L                               87
089700000614     C                   MOVEL     WFTANN        DEPDAT            6 0
089800000614     C                   MOVE      WFTMES        DEPDAT
089900000614     C                   IF        DEPDAT<=PARDAF
090000000302     C  N87              DELETE    WFTER01L
090100000614     C                   ENDIF
090200000302     C                   ENDDO
090300000302     C*
090400000302     C                   CLOSE     WFTER01L
090500000302     C*
090600000302     C                   ENDSR
090700000302     C*------------------------------------------------------------------------*
090800000301     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
090900000000     C*------------------------------------------------------------------------*
091000000000     C     CARORG        BEGSR
091100000000     C*
091200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
091300000301     C     *LOVAL        SETLL     AZORG01L                           98        *FINE-ARCHIV
091400000000     C  N98              READ      AZORG01L                               98
091500000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
091600000000     C     *IN98         DOWEQ     '0'                                          --- 1 -->
091700000000     C     ORGFAG        IFNE      'V'
091800000301     C     ORGFVA        ANDEQ     ' '
091900000302     C                   SETOFF                                       11
092000000302     C     ORGFIL        LOOKUP    TER                                    11
092100000613     C     *IN11         IFEQ      *ON
092200000613     C                   MOVEL     ORGDE3        OG143
092300000613     C                   ADD       1             I
092400020627     C                   IF        §OGNTW='PPT'
092500000613     C                   Z-ADD     ORGFIL        PFIL(I)
092600000613     C                   MOVEL     ORGDES        PDFIL(I)
092700000613     C                   ELSE
092800000000     C                   Z-ADD     ORGFIL        FIL(I)
092900000000     C                   MOVEL     ORGDES        DFIL(I)
093000000613     C                   ENDIF
093100000302     C                   ENDIF
093200000301     C                   ENDIF
093300000000     C                   READ      AZORG01L                               98
093400000301     C                   ENDDO                                                  <-- 1 ---
093500000000     C*
093600000000     C                   ENDSR
093700991123     C*
093800991123     C*------------------------------------------------------------------------*
093900991123     C*
094000000000** -TES-   (DESCRIZIONI  STAMPA)
094100000303   PARTENZA TERMINAL SDI A BARTOLINI                                       1
094200000303   ARRIVI TERMINAL BARTOLINI DA SDI                                        2
094300000302"                                                                          3
094400000303TERMINAL di PARTENZA                                                       4
094500000303TERMINAL di ARRIVO                                                         5
094600000301** -TER-   ELENCO TERMINAL <-> TERMINAL
094700000301001
094800000301005
094900000301006
095000000301028
095100000301043
095200000301049
095300000301050
095400000301063
095500000301067
095600000301089
095700000301202
095800000301210
095900000301214
096000000301220
096100000301224
096200000301249
096300000301255
096400000301275
096500000301277
096600000301280
096700000301** -BAR-   ELENCO TERMINAL BARTOLINI
096800000301001
096900000301005
097000000301006
097100000301028
097200000301043
097300000301049
097400000301050
097500000301063
097600000301067
097700000301089
097800000301** -SDI-   ELENCO TERMINAL SDI
097900000301202
098000000301210
098100000301214
098200000301220
098300000301224
098400000301249
098500000301255
098600000301275
098700000301277
098800000301280
