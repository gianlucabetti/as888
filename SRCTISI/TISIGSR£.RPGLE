000100000000     H*------------------------------------------------------------------------*
000200030124     H* STAMPA MOVIMENTI GABBIE E BANCALI                   ESEGUE STAMPA      *
000300950206     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500950206     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800030618     FAZMGB03L  IF   E           K DISK    RENAME(AZMGB000:AZMGB003) PREFIX(C_)
000900030124     FAZMGB04L  IF   E           K DISK
001000030124     FAZAGB01L  IF   E           K DISK
001100030124     FCNACO00F  IF   E           K DISK
001200030124     FAZDST04L  IF   E           K DISK
001300030124     FAZORG01L  IF   E           K DISK
001400030124     FTNTBE01L  IF   E           K DISK
001500030124     FTISIGSP   O    E             PRINTER OFLIND(*IN01)
001600030127     FTISIGSP1  O    E             PRINTER OFLIND(*IN02)
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000020718     D*------------
002100030124     D* SCHIERA CAUSALI
002200020718     D*------------
002300030127     D SKCODCAU        S              4    DIM(100)
002400030127     D SKUNICAU        S             48    DIM(100)
002500030124     D*------------
002600030124     D* SCHIERA ARTICOLI
002700030124     D*------------
002800030618     D SKCODART        S              5    DIM(100)
002900030127     D SKDESART        S             30    DIM(100)
003000000808     D*------------------
003100000808     D* DS CONTROLLO DATA (8)
003200000808     D*------------------
003300000808     D WLBDA8          DS                  INZ
003400000808     D  G08DAT                 1      8  0
003500000808     D  G08INV                 9     16  0
003600000808     D  G08ERR                17     17
003700000808     D  G08TGI                18     22  0
003800000000     D*------------------
003900000000     D* ARCHITETTURA
004000000000     D*------------------
004100000000     D KPJBA         E DS
004200020722     D*------------------
004300030124     D* DS TABELLA "GGB"
004400020722     D*------------------
004500030124     D DGGB          E DS
004600000000     D*------------------
004700020718     D* DS X PASSAGGIO PARAMETRI
004800020718     D*------------------
004900030124     D PARAM           DS                  INZ
005000030124     D  PARDA1                        8  0
005100030124     D  PARTP1                        1
005200030124     D  PARSO1                        8
005300030124     D  PARCO1                       15
005400030124     D  PARSEL                        1
005500030124     D  PARDET                        1
005600030124     D  PARDDA                        8  0
005700030124     D  PARDAA                        8  0
005800030305     D  PARPO1                        3
005900030618     D  PARSSN                        1
006000030618     D  PARCA1                        4
006100030618     D  PARCA2                        4
006200020923     D*-------------------
006300020923     D* DS REPERIMENTO DATI UTENTE
006400020923     D*-------------------
006500020923     D TIBS34DS      E DS                                                       *Profili utente
006600020923     D DDATIUTE      E DS                                                       *Dati utente
006700020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006800030127     D*------------------
006900030127     D* VARIABILI DI TOTALIZZAZIONE
007000030127     D*------------------
007100030127     D TotSOG          S             18  3 INZ(*zeros)
007200030408     D TotSOGGEN       S             18  3 INZ(*zeros)
007300030127     D TotCOD          S             18  3 INZ(*zeros)
007400030127     D DepTIP          S                   LIKE(mgbTIP)
007500030127     D DepSOG          S                   LIKE(mgbSOG)
007600030311     D DepDSO          S                   LIKE(stdDSO)
007700030311     D DepCOD          S                   LIKE(mgbCOD)
007800030127     D*------------
007900030127     D* SCHIERA DI TOTALIZZAZIONE
008000030127     D*------------
008100030127     D WRKCODCAU       S              4    DIM(100)
008200030127     D WRKTOTCAU       S             18  3 DIM(100)
008300020722     D*------------------
008400030124     D* VARIABILI DI WRK
008500020722     D*------------------
008600020722     D I               S              4  0 INZ
008700030127     D J               S              4  0 INZ
008800030124     D WDA8            S              8  0 INZ
008900030124     D DAW8            S              8  0 INZ
009000030124     D W_Fine          S              1    INZ('0')
009100030124     D WRECOKDET       S              1    INZ('S')
009200030124     D WRECOKSAL       S              1    INZ('S')
009300000000     C*------------------------------------------------------------------------*
009400000000     C*?MAIN LINE
009500000000     C*------------------------------------------------------------------------*
009600020718     C*
009700020718     C* STAMPO LA TESTATA E I PARAMETRI DI LANCIO
009800020718     C                   EXSR      STATES
009900020718     C*
010000030124     C* CICLO SUI MOVIMENTI E TOTALIZZO/STAMPO DETAGLIO
010100030124     C                   EXSR      REPMOV
010200020718     C*
010300021120     C* AL TERMINE STAMPO LE CHIUSURE PROSPETTI
010400030618     C                   IF        PARSSN = 'S'
010500030128     C                   WRITE     SIGSRI1
010600030128     C                   WRITE     SIGSFI1
010700030618     C                   ENDIF
010800021120     C                   IF        PARDET = 'S'
010900030124     C                   WRITE     SIGSRI
011000030124     C                   WRITE     SIGSFI
011100021120     C                   ENDIF
011200020523     C*
011300020523     C                   SETON                                        LR
011400020718     C*------------------------------------------------------------------------*
011500030124     C*?STATES - STAMPA LA TESTATA ED I PARAMETRI DI LANCIO
011600020718     C*------------------------------------------------------------------------*
011700020718     C     STATES        BEGSR
011800021120     C*
011900030127     C* ESEGUO SEMPRE LA STAMPA X SALDI =>
012000030127     C                   EXSR      STATESS
012100030127     C*
012200030127     C* SE RICHIESTA STAMPA DI DETTAGLIO =>
012300030127     C                   IF        PARDET = 'S'
012400030127     C                   EXSR      STATESD
012500030127     C                   ENDIF
012600030124     C*
012700020718     C                   ENDSR
012800030127     C*------------------------------------------------------------------------*
012900030127     C*?STATESS - STAMPA LA TESTATA ED I PARAMETRI DI LANCIO DELLA STAMPA SALDI MOVIMENTI
013000030127     C*------------------------------------------------------------------------*
013100030127     C     STATESS       BEGSR
013200030127     C*
013300030127     C* - STAMPO IL TITOLO
013400030618     C                   IF        PARSSN = 'S'
013500030127     C                   WRITE     SIGSPT1
013600030618     C                   ENDIF
013700030127     C*
013800030127     C* - STAMPO I PARAMETRI
013900030618     C                   EVAL      STPCA1 = PARCA1
014000030618     C                   EVAL      STPCA2 = PARCA2
014100030618     C                   IF        PARCA1 = *BLANKS AND
014200030618     C                             PARCA2 = *BLANKS
014300030618     C                   EVAL      STPCA1 = 'Tutte'
014400030618     C                   ENDIF
014500030618     C*
014600030127     C                   IF        PARDA1 > *zeros
014700030127     C                   Z-ADD     PARDA1        DAW8                           * Saldo al
014800030127     C                   ELSE
014900030127     C                   Z-ADD     datcor        DAW8
015000030127     C                   ENDIF
015100030127     C                   EXSR      CHKDA8
015200030127     C                   EVAL      STPDAL = WDA8
015300030127     C*
015400030305     C                   IF        PARSO1 <> *zeros
015500030127     C                   MOVEL     PARSO1        STPSOG                         * Soggetto
015600030127     C                   IF        PARTP1 = 'C'
015700030127     C                   EVAL      STPDE1 = '(Cliente)'
015800030127     C                   EVAL      ACOKUT = 1
015900030127     C                   EVAL      ACOKCC = DUTKCI
016000030127     C                   MOVE      PARSO1        ACOKSC
016100030127     C     KEYACO        CHAIN     CNACO00F
016200030127     C                   IF        %found(CNACO00F)
016300030127     C                   EVAL      STPDSO = ACORAG
016400030127     C                   ELSE
016500030127     C                   EVAL      STPDSO = *ALL'*'
016600030127     C                   ENDIF
016700030127     C                   ENDIF
016800030127     C*
016900030127     C                   IF        PARTP1 = 'D'
017000030127     C                   EVAL      STPDE1 = '(Destinatario)'
017100030127     C                   MOVE      PARSO1        DSTCOD
017200030127     C     KEYDST        CHAIN     AZDST04L
017300030127     C                   IF        %found(AZDST04L)
017400030127     C                   EVAL      STPDSO = DSTRAG
017500030127     C                   ELSE
017600030127     C                   EVAL      STPDSO = *ALL'*'
017700030127     C                   ENDIF
017800030127     C                   ENDIF
017900030127     C*
018000030127     C                   IF        PARTP1 = 'F'
018100150210     C                   EVAL      STPDE1 = '(Fil.)'
018200030612     C                   MOVE      PARSO1        ORGFIL
018300030127     C     KEYORG        CHAIN     AZORG01L
018400030127     C                   IF        %found(AZORG01L)
018500030127     C                   EVAL      STPDSO = ORGDES
018600030127     C                   ELSE
018700030127     C                   EVAL      STPDSO = *ALL'*'
018800030127     C                   ENDIF
018900030127     C                   ENDIF
019000030127     C*
019100030127     C                   ELSE
019200030128     C                   EVAL      STPSOG = 'Tutti'
019300030127     C                   IF        PARTP1 = 'C'
019400030128     C                   EVAL      STPDSO = 'i clienti'
019500030305     C                   IF        PARPO1 <> *zeros
019600150210     C                   EVAL      STPDSO = %trim(STPDSO)+' del Fil. '+PARPO1
019700030305     C                   ENDIF
019800030127     C                   ENDIF
019900030127     C                   IF        PARTP1 = 'D'
020000030128     C                   EVAL      STPDSO = 'i destinatari'
020100030127     C                   ENDIF
020200030127     C                   IF        PARTP1 = 'F'
020300150210     C                   EVAL      STPDSO = 'i Fil.'
020400030127     C                   ENDIF
020500030127     C                   ENDIF
020600030127     C*
020700030127     C                   IF        PARCO1 <> *blanks
020800030127     C                   MOVEL     PARCO1        STPCOD                         * Articolo
020900030127     C                   Z-ADD     1             I
021000030127     C                   SETOFF                                       55
021100030127     C     STPCOD        LOOKUP    SKCODART(I)                            55
021200030127     C                   IF        %found
021300030127     C                   EVAL      STPDCO = SKDESART(I)
021400030127     C                   ELSE
021500030128     C                   EVAL      STPDCO = *ALL'*'
021600030127     C                   ENDIF
021700030127     C*
021800030127     C                   ELSE
021900030128     C                   EVAL      STPCOD = 'Tutti'
022000030127     C                   ENDIF
022100030127     C*
022200030127     C                   IF        PARSEL = 'S'
022300030127     C                   EVAL      STPSEL  ='Sì'
022400030127     C                   ELSE
022500030127     C                   EVAL      STPSEL  ='No'
022600030127     C                   ENDIF
022700030127     C*
022800030618     C                   IF        PARSSN = 'S'
022900030127     C                   WRITE     SIGST11
023000030127     C                   WRITE     SIGSPI1
023100030618     C                   ENDIF
023200030127     C*
023300030127     C                   ENDSR
023400030127     C*------------------------------------------------------------------------*
023500030127     C*?STATESD - STAMPA LA TESTATA ED I PARAMETRI DI LANCIO DELLA STAMPA DI DETTAGLIO MOVIMENTI
023600030127     C*------------------------------------------------------------------------*
023700030127     C     STATESD       BEGSR
023800030127     C*
023900030127     C* - STAMPO IL TITOLO
024000030127     C                   WRITE     SIGSPT
024100030127     C*
024200030127     C* - STAMPO I PARAMETRI
024300030128     C                   Z-ADD     PARDDA        DAW8                           * Periodo dal
024400030128     C                   EXSR      CHKDA8
024500030128     C                   EVAL      STPDAL = WDA8
024600030128     C*
024700030128     C                   Z-ADD     PARDAA        DAW8                           * Periodo al
024800030128     C                   EXSR      CHKDA8
024900030128     C                   EVAL      STPAL  = WDA8
025000030128     C*
025100030127     C                   WRITE     SIGST1
025200030127     C                   WRITE     SIGSPI
025300030127     C*
025400030127     C                   ENDSR
025500020718     C*------------------------------------------------------------------------*
025600030124     C*?REPMOV - LEGGE I MOVIMENTI E TOTALIZZA
025700020718     C*------------------------------------------------------------------------*
025800030124     C     REPMOV        BEGSR
025900030124     C*
026000030124     C                   EVAL      W_Fine = '0'
026100030124     C*
026200030124     C* POSIZIONAMENTO SUL FILE MOVIMENTI
026300030124     C                   EVAL      MGBTIP = PARTP1
026400030124     C                   EVAL      MGBSOG = PARSO1
026500030124     C                   EVAL      MGBCOD = PARCO1
026600030124     C     KEYMGBP       SETLL     AZMGB04L                           99
026700030124     C                   IF        *IN99 = *OFF
026800030124     C                   READ      AZMGB04L
026900030124     C                   DOW       not %eof(AZMGB04L) AND W_Fine = '0'
027000030124     C*
027100030124     C* ESEGUO CONTROLLI DI VALIDITA RISPETTO AI PARAMETRI DI LANCIO
027200030124     C                   EXSR      CHKREC
027300030124     C*
027400030124     C* SE RECORD OK E RICHIESTA STAMPA DETTAGLIO => ESEGUO
027500030124     C                   IF        WRECOKDET = 'S' AND
027600030124     C                             PARDET = 'S'
027700030618     C                   EXSR      REPMOVCORR
027800030124     C                   EXSR      STADET
027900030124     C                   ENDIF
028000030127     C*
028100030127     C* SE RECORD OK ESEGUO TOTALIZZAZIONE X CLIENTE / ARTICOLO / CAUSALE
028200030618     C                   IF        WRECOKSAL = 'S' AND
028300030618     C                             PARSSN = 'S'
028400030128     C                   EXSR      TOTROT
028500030127     C                   ENDIF
028600030124     C                   READ      AZMGB04L
028700030124     C                   ENDDO
028800030127     C*
028900030618     C* A FINE CICLO SCARICO LE MEMORIZZAZIONI SOSPESE (SE RICHIESTO NEL LANCIO)
029000030618     C                   IF        PARSSN = 'S'
029100030128     C                   EXSR      STASALCAUTOT
029200030128     C                   EXSR      STASALCODTOT
029300030128     C                   EXSR      STASALSOGTOT
029400030408     C                   EXSR      STASALGENTOT
029500030618     C                   ENDIF
029600030124     C                   ENDIF
029700020718     C*
029800020718     C                   ENDSR
029900030618     C*------------------------------------------------------------------------*
030000030618     C*?REPMOVCORR - LEGGE IL MOVIMENTO CORRELATO A QUELLO CORRENTE
030100030618     C*------------------------------------------------------------------------*
030200030618     C     REPMOVCORR    BEGSR
030300030618     C*
030400030618     C* Inizializzo i campi in stampa relativi al soggetto correlato
030500030618     C                   CLEAR                   STDSOC
030600030618     C                   CLEAR                   STDDE3
030700030618     C*
030800030618     C* Mi posiziono per progressivo univoco (coppia movimenti)
030900030618     C     mgbPRG        SETLL     azmgb03l
031000030618     C                   IF        %equal(azmgb03l)
031100030618     C* Leggo il 1° movimento e verifica codice soggetto
031200030618     C     mgbPRG        READE     azmgb03l
031300030618     C                   IF        not %eof(azmgb03l)
031400030618     C                   IF        C_mgbSOG <> mgbSOG
031500030618     C                   MOVE(P)   C_mgbSOG      STDSOC
031600030618     C                   ELSE
031700030618     C* Leggo il 2° movimento e verifica codice soggetto
031800030618     C     mgbPRG        READE     azmgb03l
031900030618     C                   IF        not %eof(azmgb03l)
032000030618     C                   IF        C_mgbSOG <> mgbSOG
032100030618     C                   MOVE(P)   C_mgbSOG      STDSOC
032200030618     C                   ENDIF
032300030618     C                   ENDIF
032400030618     C                   ENDIF
032500030618     C                   ENDIF
032600030618     C                   ENDIF
032700030618     C*
032800030618     C                   ENDSR
032900030127     C*------------------------------------------------------------------------*
033000030128     C*?TOTROT - ROUTINE DI VERIFICA ROTTURE DI CODICE
033100030127     C*------------------------------------------------------------------------*
033200030128     C     TOTROT        BEGSR
033300030127     C*
033400030127     C* VERIFICO ROTTURA X SOGGETTO
033500030127     C                   IF        mgbTIP = DepTIP AND
033600030127     C                             mgbSOG = DepSOG
033700030127     C                   ELSE
033800030127     C                   IF        DepTIP <> *blanks AND DepSOG <> *blanks
033900030127     C                   EXSR      STASALCAUTOT
034000030127     C                   EXSR      STASALCODTOT
034100030127     C                   EXSR      STASALSOGTOT
034200030128     C                   CLEAR                   WRKTOTCAU
034300030127     C                   ENDIF
034400030127     C                   EVAL      DepTIP = mgbTIP
034500030127     C                   EVAL      DepSOG = mgbSOG
034600030311     C                   EVAL      DepDSO = stdDSO
034700030127     C                   EVAL      DepCOD = mgbCOD
034800030127     C*
034900030127     C* PRIMA DI OGNI NUOVO SOGGETTO LASCIO UNA RIGA VUOTA DI SEPARAZIONE CON I ISALDI PRECEDENTI
035000030127     C                   WRITE     SIGSRI1
035100030127     C*
035200030127     C                   SETOFF                                       707172
035300030127     C                   SETON                                        70
035400030127     C*
035500030127     C                   EXSR      STASALDET
035600030127     C                   ENDIF
035700030127     C*
035800030127     C* VERIFICO ROTTURA X ARTICOLO
035900030127     C                   IF        mgbCOD <> DepCOD
036000030127     C                   IF        DepCOD <> *blanks
036100030127     C                   EXSR      STASALCAUTOT
036200030127     C                   EXSR      STASALCODTOT
036300030128     C                   CLEAR                   WRKTOTCAU
036400030127     C                   ENDIF
036500030127     C                   EVAL      DepCOD = mgbCOD
036600030127     C*
036700030127     C                   EXSR      REPSALCOD
036800030127     C                   ENDIF
036900030127     C*
037000030127     C* TOTALIZZO
037100030128     C                   EXSR      MEMDAT
037200030127     C*
037300030127     C                   ENDSR
037400030128     C*------------------------------------------------------------------------*
037500030128     C*?MEMDAT - ROUTINE DI TOTALIZZAZIONE X SALDI
037600030128     C*------------------------------------------------------------------------*
037700030128     C     MEMDAT        BEGSR
037800030128     C*
037900030128     C* TOTALIZZO
038000030128     C                   EVAL      TotSOG = TotSOG + MGBQTA
038100030408     C                   EVAL      TotSOGGEN = TotSOGGEN + MGBQTA
038200030128     C                   EVAL      TotCOD = TotCOD + MGBQTA
038300030128     C*
038400030128     C                   Z-ADD     1             I
038500030128     C                   SETOFF                                       55
038600030128     C     MGBCAU        LOOKUP    WRKCODCAU(I)                           55
038700030128     C                   IF        %found
038800030128     C                   EVAL      WRKTOTCAU(I) = WRKTOTCAU(I) + MGBQTA
038900030128     C                   ENDIF
039000030128     C*
039100030128     C                   ENDSR
039200030127     C*------------------------------------------------------------------------*
039300030127     C*?STASALDET - ROUTINE DI STAMPA DETTAGLIO X SALDO
039400030127     C*------------------------------------------------------------------------*
039500030127     C     STASALDET     BEGSR
039600030127     C*
039700030127     C* REPERISCO LE DESCRIZIONI X STAMPA SALDI
039800030127     C                   EXSR      REPSALSOG
039900030127     C                   EXSR      REPSALCOD
040000030127     C*
040100030127     C* QUINDI VADO IN STAMPA
040200030128     C                   IF        *IN02 = *ON
040300030128     C                   WRITE     SIGSPT1
040400030128     C                   WRITE     SIGSPI1
040500030128     C                   SETOFF                                       02
040600030128     C                   ENDIF
040700030127     C                   WRITE     SIGSP11
040800030127     C*
040900030127     C                   ENDSR
041000030127     C*------------------------------------------------------------------------*
041100030127     C*?REPSALSOG - ROUTINE DI REPERIMENTO DESCRIZIONI SOGGETTO X STAMPA SALDI
041200030127     C*------------------------------------------------------------------------*
041300030127     C     REPSALSOG     BEGSR
041400030127     C*
041500030128     C                   MOVEL     DepSOG        STDSOG
041600030127     C* REPERISCO DESCRIZIONE SOGGETTO
041700030127     C                   IF        DepTIP = 'C'
041800030127     C                   EVAL      STDDE2 = '(Cli)'
041900030127     C                   EVAL      ACOKUT = 1
042000030127     C                   EVAL      ACOKCC = DUTKCI
042100030127     C                   MOVE      STDSOG        ACOKSC
042200030127     C     KEYACO        CHAIN     CNACO00F
042300030127     C                   IF        %found(CNACO00F)
042400030127     C                   EVAL      STDDSO = ACORAG
042500030127     C                   ELSE
042600030127     C                   EVAL      STDDSO = *ALL'*'
042700030127     C                   ENDIF
042800030127     C                   ENDIF
042900030127     C                   IF        DepTIP = 'D'
043000030127     C                   EVAL      STDDE2 = '(Dest)'
043100030127     C                   MOVE      STDSOG        DSTCOD
043200030127     C     KEYDST        CHAIN     AZDST04L
043300030127     C                   IF        %found(AZDST04L)
043400030127     C                   EVAL      STDDSO = DSTRAG
043500030127     C                   ELSE
043600030127     C                   EVAL      STDDSO = *ALL'*'
043700030127     C                   ENDIF
043800030127     C                   ENDIF
043900030127     C                   IF        DepTIP = 'F'
044000150210     C                   EVAL      STDDE2 = '(Fil.)'
044100030127     C                   MOVE      STDSOG        ORGFIL
044200030127     C     KEYORG        CHAIN     AZORG01L
044300030127     C                   IF        %found(AZORG01L)
044400030127     C                   EVAL      STDDSO = ORGDES
044500030127     C                   ELSE
044600030127     C                   EVAL      STDDSO = *ALL'*'
044700030127     C                   ENDIF
044800030127     C                   ENDIF
044900030127     C*
045000030127     C                   ENDSR
045100030127     C*------------------------------------------------------------------------*
045200030127     C*?REPSALCOD - ROUTINE DI REPERIMENTO DESCRIZIONI ARTICOLO X STAMPA SALDI
045300030127     C*------------------------------------------------------------------------*
045400030127     C     REPSALCOD     BEGSR
045500030127     C*
045600030127     C                   EVAL      STDCOD = DepCOD
045700030127     C* REPERISCO DESCRIZOINE ARTICOLO
045800030127     C                   Z-ADD     1             I
045900030127     C                   SETOFF                                       55
046000030127     C     STDCOD        LOOKUP    SKCODART(I)                            55
046100030127     C                   IF        %found
046200030127     C                   EVAL      STDDCO = SKDESART(I)
046300030127     C                   ELSE
046400030127     C                   EVAL      STDDCO = *ALL'*'
046500030127     C                   ENDIF
046600030127     C*
046700030127     C                   ENDSR
046800030127     C*------------------------------------------------------------------------*
046900030127     C*?REPSALCAU - ROUTINE DI REPERIMENTO DESCRIZIONI CAUSALE X STAMPA SALDI
047000030127     C*------------------------------------------------------------------------*
047100030127     C     REPSALCAU     BEGSR
047200030127     C*
047300030127     C                   EVAL      STDCAU = WRKCODCAU(J)
047400030127     C* REPERISCO TIPO E DESCRIZIONE CAUSALE
047500030127     C                   Z-ADD     1             I
047600030127     C                   SETOFF                                       55
047700030127     C     STDCAU        LOOKUP    SKCODCAU(I)                            55
047800030127     C                   IF        %found
047900030127     C                   EVAL      DGGB = SKUNICAU(I)
048000030127     C                   EVAL      STDTIM = §GGBTIM
048100030127     C                   EVAL      STDDCA = §GGBDES
048200030127     C                   ELSE
048300030127     C                   EVAL      STDTIM = *ALL'*'
048400030127     C                   EVAL      STDDCA = *ALL'*'
048500030127     C                   ENDIF
048600030127     C*
048700030127     C                   ENDSR
048800030127     C*------------------------------------------------------------------------*
048900030127     C*?STASALSOGTOT - ROUTINE DI STAMPA TOTALE SALDO X SOGGETTO
049000030127     C*------------------------------------------------------------------------*
049100030127     C     STASALSOGTOT  BEGSR
049200030127     C*
049300030127     C                   EVAL      STTSAL = TotSOG
049400030127     C                   EVAL      STTDD3 = 'SALDO SOGGETTO: ' +
049500030311     C***                                   %editw(DepSOG:'        ') + ' ' +
049600030311     C                                      %subst(DepSOG:2:7) + ' ' +
049700031113     C***                                   DepDSO
049800031113     C                                      stdDSO
049900030127     C*
050000030128     C                   IF        *IN02 = *ON
050100030128     C                   WRITE     SIGSPT1
050200030128     C                   WRITE     SIGSPI1
050300030128     C                   SETOFF                                       02
050400030128     C                   ENDIF
050500030127     C                   WRITE     SIGSP21
050600030127     C*
050700030127     C                   Z-ADD     *zeros        TotSOG
050800030127     C*
050900030127     C                   ENDSR
051000030408     C*------------------------------------------------------------------------*
051100030408     C*?STASALGENTOT - ROUTINE DI STAMPA TOTALE SALDO GENERALE SOGGETTO
051200030408     C*------------------------------------------------------------------------*
051300030408     C     STASALGENTOT  BEGSR
051400030408     C*
051500030408     C* PRIMA DELLA RIGA DI TOTALE GENERALE STAMPO UNA RIGA VUOTA
051600030408     C                   WRITE     SIGSRI1
051700030408     C*
051800030408     C                   EVAL      STTSAL = TotSOGGEN
051900030408     C                   EVAL      STTDD3 = 'SALDO MOVIMENTI STAMPATI: '
052000030408     C*
052100030408     C                   IF        *IN02 = *ON
052200030408     C                   WRITE     SIGSPT1
052300030408     C                   WRITE     SIGSPI1
052400030408     C                   SETOFF                                       02
052500030408     C                   ENDIF
052600030408     C                   WRITE     SIGSP21
052700030408     C*
052800030408     C                   Z-ADD     *zeros        TotSOGGEN
052900030408     C*
053000030408     C                   ENDSR
053100030127     C*------------------------------------------------------------------------*
053200030127     C*?STASALCODTOT - ROUTINE DI STAMPA TOTALE SALDO X ARTICOLO
053300030127     C*------------------------------------------------------------------------*
053400030127     C     STASALCODTOT  BEGSR
053500030127     C*
053600030127     C                   EVAL      STTSAL = TotCOD
053700030127     C                   EVAL      STTDD3 = '      ' + 'SALDO ARTICOLO: ' +
053800030128     C                                      STDCOD + ' ' + STDDCO
053900030127     C*
054000030128     C                   IF        *IN02 = *ON
054100030128     C                   WRITE     SIGSPT1
054200030128     C                   WRITE     SIGSPI1
054300030128     C                   SETOFF                                       02
054400030128     C                   ENDIF
054500030127     C                   WRITE     SIGSP21
054600030127     C*
054700030127     C                   Z-ADD     *zeros        TotCOD
054800030127     C*
054900030127     C                   ENDSR
055000030127     C*------------------------------------------------------------------------*
055100030127     C*?STASALCAUTOT - ROUTINE DI STAMPA TOTALE SALDO X CAUSALE
055200030127     C*------------------------------------------------------------------------*
055300030127     C     STASALCAUTOT  BEGSR
055400030127     C*
055500030127     C* SE RICHIESTO NEL LANCIO
055600030127     C                   IF        PARSEL = 'S'
055700030127     C*
055800030127     C* CICLO SULLA SCHIERA DI MEMORIZZAZIONE CAUSALI
055900030127     C     1             DO        100           J
056000030128     C                   IF        WRKTOTCAU(J) <> *zeros
056100030127     C                   EXSR      REPSALCAU
056200030127     C                   EVAL      STTSAL = WRKTOTCAU(J)
056300030127     C                   EVAL      STTDD3 = '           ' + 'SALDO CAUSALE: ' +
056400030128     C                                      STDCAU + ' ' + STDDCA
056500030127     C*
056600030128     C                   IF        *IN02 = *ON
056700030128     C                   WRITE     SIGSPT1
056800030128     C                   WRITE     SIGSPI1
056900030128     C                   SETOFF                                       02
057000030128     C                   ENDIF
057100030127     C                   WRITE     SIGSP21
057200030127     C                   ENDIF
057300030127     C                   ENDDO
057400030127     C                   ENDIF
057500030127     C*
057600030127     C                   ENDSR
057700020718     C*------------------------------------------------------------------------*
057800030124     C*?CHKREC - ROUTINE DI CONTROLLO VALIDITA' RECORD
057900020718     C*------------------------------------------------------------------------*
058000030124     C     CHKREC        BEGSR
058100020725     C*
058200030124     C                   MOVEL     'S'           WRECOKDET
058300030124     C                   MOVEL     'S'           WRECOKSAL
058400030124     C*
058500030124     C* SE RICHIESTO UN TIPO/SOGGETTO CONTROLLO
058600030124     C                   IF        PARTP1 <> *BLANKS
058700030124     C                   IF        MGBTIP = PARTP1
058800030124     C                   ELSE
058900030124     C                   MOVEL     'N'           WRECOKDET
059000030124     C                   MOVEL     'N'           WRECOKSAL
059100030124     C                   ENDIF
059200030124     C                   ENDIF
059300030612     C                   IF        PARTP1 =  'C'     AND
059400030612     C                             PARPO1 <> *ZEROS  AND
059500030612     C                             PARPO1 <> *BLANKS
059600030305     C                   IF        MGBTIP = PARTP1 AND
059700030305     C                             %subst(MGBSOG:2:3) = PARPO1
059800030305     C                   ELSE
059900030305     C                   MOVEL     'N'           WRECOKDET
060000030305     C                   MOVEL     'N'           WRECOKSAL
060100030305     C                   ENDIF
060200030305     C                   ENDIF
060300030305     C                   IF        PARSO1 <> *ZEROS
060400030124     C                   IF        MGBTIP = PARTP1 AND
060500030124     C                             MGBSOG = PARSO1
060600030124     C                   ELSE
060700030124     C                   EVAL      W_Fine = '1'
060800030124     C                   MOVEL     'N'           WRECOKDET
060900030124     C                   MOVEL     'N'           WRECOKSAL
061000030124     C                   ENDIF
061100030124     C                   ENDIF
061200030124     C*
061300030124     C* SE RICHIESTO UN ARTICOLO CONTROLLO
061400030124     C                   IF        PARCO1 <> *BLANKS
061500030124     C                   IF        MGBCOD = PARCO1
061600030124     C                   ELSE
061700030124     C                   MOVEL     'N'           WRECOKDET
061800030124     C                   MOVEL     'N'           WRECOKSAL
061900030124     C                   ENDIF
062000030124     C                   ENDIF
062100030618     C*
062200030618     C* SE RICHIESTA UNA/DUE CAUSALI
062300030618     C                   IF        (PARCA1 <> *BLANKS AND MGBCAU = PARCA1)  OR
062400030618     C                             (PARCA2 <> *BLANKS AND MGBCAU = PARCA2)  OR
062500030618     C                             (PARCA1  = *BLANKS AND PARCA2 = *BLANKS)
062600030618     C                   ELSE
062700030618     C                   MOVEL     'N'           WRECOKDET
062800030618     C                   MOVEL     'N'           WRECOKSAL
062900030618     C                   ENDIF
063000030124     C*
063100030127     C* SE RICHIESTA UNA DATA "SALDO AL" CONTROLLO
063200030124     C                   IF        PARDA1 > *ZEROS
063300030124     C                   IF        MGBDAT <= PARDA1
063400030124     C                   ELSE
063500030124     C                   MOVEL     'N'           WRECOKSAL
063600030124     C                   ENDIF
063700030124     C                   ENDIF
063800030124     C*
063900030124     C* SE RICHIESTA DETTAGLIO CONTROLLO LE DATE DEL PERIODO IMMESSO NEL LANCIO
064000030124     C                   IF        PARDET = 'S'
064100030124     C                   IF        MGBDAT >= PARDDA AND
064200030124     C                             MGBDAT <= PARDAA
064300030124     C                   ELSE
064400030124     C                   MOVEL     'N'           WRECOKDET
064500030124     C                   ENDIF
064600030124     C                   ENDIF
064700020718     C*
064800020718     C                   ENDSR
064900021120     C*------------------------------------------------------------------------*
065000030124     C*?STADET - ROUTINE DI STAMPA DETTAGLIO MOVIMENTI
065100021120     C*------------------------------------------------------------------------*
065200021120     C     STADET        BEGSR
065300021120     C*
065400030127     C                   Z-ADD     MGBDAT        DAW8                           * Periodo dal
065500030127     C                   EXSR      CHKDA8
065600030127     C                   EVAL      STDDAT = WDA8
065700030124     C                   EVAL      STDORA = MGBORA
065800030124     C                   EVAL      STDCAU = MGBCAU
065900030124     C* REPERISCO TIPO E DESCRIZIONE CAUSALE
066000030124     C                   Z-ADD     1             I
066100030124     C                   SETOFF                                       55
066200030124     C     STDCAU        LOOKUP    SKCODCAU(I)                            55
066300030124     C                   IF        %found
066400030124     C                   EVAL      DGGB = SKUNICAU(I)
066500030124     C                   EVAL      STDTIM = §GGBTIM
066600030124     C                   EVAL      STDDCA = §GGBDES
066700030124     C                   ELSE
066800030124     C                   EVAL      STDTIM = *ALL'*'
066900030124     C                   EVAL      STDDCA = *ALL'*'
067000030124     C                   ENDIF
067100030618     C* SOGGETTO MOVIMENTO
067200030128     C                   MOVEL     MGBSOG        STDSOG
067300030618     C* REPERISCO DESCRIZIONE SOGGETTO MOVIMENTO
067400030124     C                   IF        MGBTIP = 'C'
067500030124     C                   EVAL      STDDE2 = '(Cli)'
067600030124     C                   EVAL      ACOKUT = 1
067700030124     C                   EVAL      ACOKCC = DUTKCI
067800030124     C                   MOVE      STDSOG        ACOKSC
067900030124     C     KEYACO        CHAIN     CNACO00F
068000030124     C                   IF        %found(CNACO00F)
068100030124     C                   EVAL      STDDSO = ACORAG
068200030124     C                   ELSE
068300030124     C                   EVAL      STDDSO = *ALL'*'
068400030124     C                   ENDIF
068500030124     C                   ENDIF
068600030124     C                   IF        MGBTIP = 'D'
068700030124     C                   EVAL      STDDE2 = '(Dest)'
068800030124     C                   MOVE      STDSOG        DSTCOD
068900030124     C     KEYDST        CHAIN     AZDST04L
069000030124     C                   IF        %found(AZDST04L)
069100030124     C                   EVAL      STDDSO = DSTRAG
069200030124     C                   ELSE
069300030124     C                   EVAL      STDDSO = *ALL'*'
069400030124     C                   ENDIF
069500030124     C                   ENDIF
069600030124     C                   IF        MGBTIP = 'F'
069700150210     C                   EVAL      STDDE2 = '(Fil.)'
069800030124     C                   MOVE      STDSOG        ORGFIL
069900030124     C     KEYORG        CHAIN     AZORG01L
070000030124     C                   IF        %found(AZORG01L)
070100030124     C                   EVAL      STDDSO = ORGDES
070200030124     C                   ELSE
070300030124     C                   EVAL      STDDSO = *ALL'*'
070400030124     C                   ENDIF
070500030124     C                   ENDIF
070600030618     C* SOGGETTO MOVIMENTO CORRELATO SE PRESENTE
070700030618     C                   IF        STDSOC > *zeros
070800030618     C* REPERISCO DESCRIZIONE SOGGETTO MOVIMENTO CORRELATO
070900030618     C                   IF        C_MGBTIP = 'C'
071000030618     C                   EVAL      STDDE3 = '(Cli)'
071100030618     C                   EVAL      ACOKUT = 1
071200030618     C                   EVAL      ACOKCC = DUTKCI
071300030618     C                   MOVE      STDSOC        ACOKSC
071400030618     C     KEYACO        CHAIN     CNACO00F
071500030618     C                   IF        %found(CNACO00F)
071600030618     C                   EVAL      STDDSC = ACORAG
071700030618     C                   ELSE
071800030618     C                   EVAL      STDDSC = *ALL'*'
071900030618     C                   ENDIF
072000030618     C                   ENDIF
072100030618     C                   IF        C_MGBTIP = 'D'
072200030618     C                   EVAL      STDDE3 = '(Dest)'
072300030618     C                   MOVE      STDSOC        DSTCOD
072400030618     C     KEYDST        CHAIN     AZDST04L
072500030618     C                   IF        %found(AZDST04L)
072600030618     C                   EVAL      STDDSC = DSTRAG
072700030618     C                   ELSE
072800030618     C                   EVAL      STDDSC = *ALL'*'
072900030618     C                   ENDIF
073000030618     C                   ENDIF
073100030618     C                   IF        C_MGBTIP = 'F'
073200150210     C                   EVAL      STDDE3 = '(Fil.)'
073300030618     C                   MOVE      STDSOC        ORGFIL
073400030618     C     KEYORG        CHAIN     AZORG01L
073500030618     C                   IF        %found(AZORG01L)
073600030618     C                   EVAL      STDDSC = ORGDES
073700030618     C                   ELSE
073800030618     C                   EVAL      STDDSC = *ALL'*'
073900030618     C                   ENDIF
074000030618     C                   ENDIF
074100030618     C                   ENDIF
074200030124     C*
074300030124     C                   EVAL      STDCOD = MGBCOD
074400030124     C* REPERISCO DESCRIZOINE ARTICOLO
074500030124     C                   Z-ADD     1             I
074600030124     C                   SETOFF                                       55
074700030124     C     STDCOD        LOOKUP    SKCODART(I)                            55
074800030124     C                   IF        %found
074900030124     C                   EVAL      STDDCO = SKDESART(I)
075000030124     C                   ELSE
075100030124     C                   EVAL      STDDCO = *ALL'*'
075200030124     C                   ENDIF
075300030124     C*
075400030124     C                   EVAL      STDQTA = MGBQTA
075500030124     C                   EVAL      STDRDO = MGBRDO
075600030124     C                   EVAL      STDUTE = MGBUTE
075700021120     C*
075800030128     C                   IF        *IN01 = *ON
075900030124     C                   WRITE     SIGSPT
076000030124     C                   WRITE     SIGSPI
076100030128     C                   SETOFF                                       01
076200021120     C                   ENDIF
076300030124     C                   WRITE     SIGSP1
076400021120     C*
076500021120     C                   ENDSR
076600030124     C*------------------------------------------------------------------------*
076700030124     C* CHKDA8 - CONTROLLA LA DATA (8)
076800030124     C*------------------------------------------------------------------------*
076900030124     C     CHKDA8        BEGSR
077000030124     C*
077100030124     C                   IF        DAW8 > *ZEROS
077200030124     C                   Z-ADD     *ZEROS        G08DAT
077300030124     C                   Z-ADD     DAW8          G08INV
077400030124     C                   MOVEL     '3'           G08ERR
077500030124     C                   CALL      'XSRDA8'
077600030124     C                   PARM                    WLBDA8
077700030124     C                   Z-ADD     G08DAT        WDA8
077800030124     C                   Z-ADD     G08INV        DAW8
077900030124     C                   ELSE
078000030124     C                   Z-ADD     WDA8          G08DAT
078100030124     C                   Z-ADD     *ZEROS        G08INV
078200030124     C                   MOVEL     '0'           G08ERR
078300030124     C                   CALL      'XSRDA8'
078400030124     C                   PARM                    WLBDA8
078500030124     C                   Z-ADD     G08DAT        WDA8
078600030124     C                   Z-ADD     G08INV        DAW8
078700030124     C                   ENDIF
078800030124     C*
078900030124     C                   ENDSR
079000020923     C*--------------------------------------------------------------------------------------------*
079100020923     C* REPERISCE I DATI UTENTE
079200020923     C*--------------------------------------------------------------------------------------------*
079300020923     C     REPDATIUTE    BEGSR
079400020923     C*
079500020923     C* INIZIALIZZA VARIABILI DI WRK
079600020923     C                   CLEAR                   TIBS34DS
079700020923     C                   CLEAR                   AZUTEDS
079800020923     C                   CLEAR                   DDATIUTE
079900020923     C*
080000020923     C     *DTAARA       DEFINE    §azute        azuteds
080100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
080200020923     C                   IN(E)     *DTAARA
080300030124     C                   IF        %Error
080400030124     C                   EVAL      I34Tla = 'L'
080500020923     C                   CALL      'TIBS34R'
080600020923     C                   PARM                    Tibs34Ds
080700020923     C                   IN        *DTAARA
080800030124     C                   ENDIF
080900020923     C*
081000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
081100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
081200020923     C*
081300020923     C                   ENDSR
081400030124     C*--------------------------------------------------------------------------------------------*
081500030124     C* CARTAB - CARICAMENTO DATI TABELLATI
081600030124     C*--------------------------------------------------------------------------------------------*
081700030124     C     CARTAB        BEGSR
081800030124     C*
081900030124     C* CARICAMENTO CAUSALI
082000030124     C                   Z-ADD     *ZEROS        I
082100030124     C                   EVAL      tbeCOD = 'GGB'
082200030124     C     tbeCOD        SETLL     TNTBE01L                           55
082300030124     C                   IF        *IN55 = *OFF
082400030124     C     tbeCOD        READE     TNTBE01L
082500030124     C                   DOW       not %eof(TNTBE01L)
082600030124     C                   ADD       1             I
082700030124     C                   EVAL      SKCODCAU(I) = tbeKE1
082800030124     C                   EVAL      SKUNICAU(I) = tbeUNI
082900030124     C     tbeCOD        READE     TNTBE01L
083000030124     C                   ENDDO
083100030124     C                   ENDIF
083200030127     C* AL TERMINE DUPLICO LA SCHIERA CAUSALI E SORTO IN ORDINE CRESCENTE QUEST'ULTIMA
083300030127     C                   MOVEA     SKCODCAU      WRKCODCAU
083400030127     C                   SORTA     WRKCODCAU
083500030124     C*
083600030124     C* CARICAMENTO ARTICOLI
083700030124     C                   Z-ADD     *ZEROS        I
083800030124     C     *LOVAL        SETLL     AZAGB01L                           55
083900030124     C                   IF        *IN55 = *OFF
084000030124     C                   READ      AZAGB01L
084100030124     C                   DOW       not %eof(AZAGB01L)
084200030124     C                   ADD       1             I
084300030124     C                   EVAL      SKCODART(I) = agbCOD
084400030124     C                   EVAL      SKDESART(I) = agbDES
084500030124     C                   READ      AZAGB01L
084600030124     C                   ENDDO
084700030124     C                   ENDIF
084800030124     C*
084900030124     C                   ENDSR
085000000000     C*------------------------------------------------------------------------*
085100000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
085200000000     C*------------------------------------------------------------------------*
085300000000     C     *INZSR        BEGSR
085400000000     C*
085500000000     C* PARAMETRI IN INPUT AL PGM
085600000000     C     *ENTRY        PLIST
085700020719     C                   PARM                    KPJBA
085800030124     C                   MOVEL     KPJBU         PARAM
085900020923     C*
086000020923     C* REPERISCE I DATI DELL'UTENTE
086100020923     C                   EXSR      REPDATIUTE
086200030124     C*
086300030124     C* CARICA I DATI TABELLATI
086400030124     C                   EXSR      CARTAB
086500030128     C*
086600030128     C* ACCENDO SUBITO QUI INDICATORE X STAMPA DESCRIZIONE SOGGETTO IN GRASSETTO
086700030128     C                   SETON                                        80
086800030127     C*
086900030127     C* REPERISCO DATA E ORA CORRENTI
087000030127     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
087100030127     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
087200030127     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
087300030127     C                   Z-ADD     WN8           G08DAT
087400030127     C                   Z-ADD     *ZEROS        G08INV
087500030127     C                   MOVEL     '0'           G08ERR
087600030127     C                   CALL      'XSRDA8'
087700030127     C                   PARM                    WLBDA8
087800030127     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
087900030124     C*
088000030124     C* CHIAVE LETTURA AZMGB04L - PARZIALE
088100030124     C     KEYMGBP       KLIST
088200030124     C                   KFLD                    MGBTIP
088300030124     C                   KFLD                    MGBSOG
088400030124     C                   KFLD                    MGBCOD
088500020718     C*
088600020718     C* CHIAVE LETTURA CNACO00F - COMPLETA
088700020718     C     KEYACO        KLIST
088800020718     C                   KFLD                    ACOKUT
088900020718     C                   KFLD                    ACOKCC
089000020718     C                   KFLD                    ACOKSC
089100020722     C*
089200030124     C* CHIAVE LETTURA TNTBE01L - PARZIALE
089300030124     C     KEYTBEP       KLIST
089400030124     C                   KFLD                    TBECOD
089500030124     C                   KFLD                    TBEKE1
089600030124     C*
089700030124     C* CHIAVE LETTURA AZORG01L - COMPLETA
089800030124     C     KEYORG        KLIST
089900030124     C                   KFLD                    ORGFIL
090000030124     C*
090100030124     C* CHIAVE LETTURA AZDST04L - COMPLETA
090200030124     C     KEYDST        KLIST
090300030124     C                   KFLD                    DSTCOD
090400020718     C*
090500000000     C                   ENDSR
