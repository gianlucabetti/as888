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
007900150324     D oldDES          S                   LIKE(stdDSO)
008000150408     D oldCOD          S                   LIKE(stdCOD)
008100150408     D oldDCO          S                   LIKE(stdDCO)
008200150324     D*------------
008300030127     D* SCHIERA DI TOTALIZZAZIONE
008400030127     D*------------
008500030127     D WRKCODCAU       S              4    DIM(100)
008600030127     D WRKTOTCAU       S             18  3 DIM(100)
008700020722     D*------------------
008800030124     D* VARIABILI DI WRK
008900020722     D*------------------
009000020722     D I               S              4  0 INZ
009100030127     D J               S              4  0 INZ
009200030124     D WDA8            S              8  0 INZ
009300030124     D DAW8            S              8  0 INZ
009400030124     D W_Fine          S              1    INZ('0')
009500030124     D WRECOKDET       S              1    INZ('S')
009600030124     D WRECOKSAL       S              1    INZ('S')
009700000000     C*------------------------------------------------------------------------*
009800000000     C*?MAIN LINE
009900000000     C*------------------------------------------------------------------------*
010000020718     C*
010100020718     C* STAMPO LA TESTATA E I PARAMETRI DI LANCIO
010200020718     C                   EXSR      STATES
010300020718     C*
010400030124     C* CICLO SUI MOVIMENTI E TOTALIZZO/STAMPO DETAGLIO
010500030124     C                   EXSR      REPMOV
010600020718     C*
010700021120     C* AL TERMINE STAMPO LE CHIUSURE PROSPETTI
010800030618     C                   IF        PARSSN = 'S'
010900030128     C                   WRITE     SIGSRI1
011000030128     C                   WRITE     SIGSFI1
011100030618     C                   ENDIF
011200021120     C                   IF        PARDET = 'S'
011300030124     C                   WRITE     SIGSRI
011400030124     C                   WRITE     SIGSFI
011500021120     C                   ENDIF
011600020523     C*
011700020523     C                   SETON                                        LR
011800020718     C*------------------------------------------------------------------------*
011900030124     C*?STATES - STAMPA LA TESTATA ED I PARAMETRI DI LANCIO
012000020718     C*------------------------------------------------------------------------*
012100020718     C     STATES        BEGSR
012200021120     C*
012300030127     C* ESEGUO SEMPRE LA STAMPA X SALDI =>
012400030127     C                   EXSR      STATESS
012500030127     C*
012600030127     C* SE RICHIESTA STAMPA DI DETTAGLIO =>
012700030127     C                   IF        PARDET = 'S'
012800030127     C                   EXSR      STATESD
012900030127     C                   ENDIF
013000030124     C*
013100020718     C                   ENDSR
013200030127     C*------------------------------------------------------------------------*
013300030127     C*?STATESS - STAMPA LA TESTATA ED I PARAMETRI DI LANCIO DELLA STAMPA SALDI MOVIMENTI
013400030127     C*------------------------------------------------------------------------*
013500030127     C     STATESS       BEGSR
013600030127     C*
013700030127     C* - STAMPO IL TITOLO
013800030618     C                   IF        PARSSN = 'S'
013900030127     C                   WRITE     SIGSPT1
014000030618     C                   ENDIF
014100030127     C*
014200030127     C* - STAMPO I PARAMETRI
014300030618     C                   EVAL      STPCA1 = PARCA1
014400030618     C                   EVAL      STPCA2 = PARCA2
014500030618     C                   IF        PARCA1 = *BLANKS AND
014600030618     C                             PARCA2 = *BLANKS
014700030618     C                   EVAL      STPCA1 = 'Tutte'
014800030618     C                   ENDIF
014900030618     C*
015000030127     C                   IF        PARDA1 > *zeros
015100030127     C                   Z-ADD     PARDA1        DAW8                           * Saldo al
015200030127     C                   ELSE
015300030127     C                   Z-ADD     datcor        DAW8
015400030127     C                   ENDIF
015500030127     C                   EXSR      CHKDA8
015600030127     C                   EVAL      STPDAL = WDA8
015700030127     C*
015800030305     C                   IF        PARSO1 <> *zeros
015900030127     C                   MOVEL     PARSO1        STPSOG                         * Soggetto
016000030127     C                   IF        PARTP1 = 'C'
016100030127     C                   EVAL      STPDE1 = '(Cliente)'
016200030127     C                   EVAL      ACOKUT = 1
016300030127     C                   EVAL      ACOKCC = DUTKCI
016400030127     C                   MOVE      PARSO1        ACOKSC
016500030127     C     KEYACO        CHAIN     CNACO00F
016600030127     C                   IF        %found(CNACO00F)
016700030127     C                   EVAL      STPDSO = ACORAG
016800030127     C                   ELSE
016900030127     C                   EVAL      STPDSO = *ALL'*'
017000030127     C                   ENDIF
017100030127     C                   ENDIF
017200030127     C*
017300030127     C                   IF        PARTP1 = 'D'
017400030127     C                   EVAL      STPDE1 = '(Destinatario)'
017500030127     C                   MOVE      PARSO1        DSTCOD
017600030127     C     KEYDST        CHAIN     AZDST04L
017700030127     C                   IF        %found(AZDST04L)
017800030127     C                   EVAL      STPDSO = DSTRAG
017900030127     C                   ELSE
018000030127     C                   EVAL      STPDSO = *ALL'*'
018100030127     C                   ENDIF
018200030127     C                   ENDIF
018300030127     C*
018400030127     C                   IF        PARTP1 = 'F'
018500150210     C                   EVAL      STPDE1 = '(Fil.)'
018600030612     C                   MOVE      PARSO1        ORGFIL
018700030127     C     KEYORG        CHAIN     AZORG01L
018800030127     C                   IF        %found(AZORG01L)
018900030127     C                   EVAL      STPDSO = ORGDES
019000030127     C                   ELSE
019100030127     C                   EVAL      STPDSO = *ALL'*'
019200030127     C                   ENDIF
019300030127     C                   ENDIF
019400030127     C*
019500030127     C                   ELSE
019600030128     C                   EVAL      STPSOG = 'Tutti'
019700030127     C                   IF        PARTP1 = 'C'
019800030128     C                   EVAL      STPDSO = 'i clienti'
019900030305     C                   IF        PARPO1 <> *zeros
020000150210     C                   EVAL      STPDSO = %trim(STPDSO)+' del Fil. '+PARPO1
020100030305     C                   ENDIF
020200030127     C                   ENDIF
020300030127     C                   IF        PARTP1 = 'D'
020400030128     C                   EVAL      STPDSO = 'i destinatari'
020500030127     C                   ENDIF
020600030127     C                   IF        PARTP1 = 'F'
020700150210     C                   EVAL      STPDSO = 'i Fil.'
020800030127     C                   ENDIF
020900030127     C                   ENDIF
021000030127     C*
021100030127     C                   IF        PARCO1 <> *blanks
021200030127     C                   MOVEL     PARCO1        STPCOD                         * Articolo
021300030127     C                   Z-ADD     1             I
021400030127     C                   SETOFF                                       55
021500030127     C     STPCOD        LOOKUP    SKCODART(I)                            55
021600030127     C                   IF        %found
021700030127     C                   EVAL      STPDCO = SKDESART(I)
021800030127     C                   ELSE
021900030128     C                   EVAL      STPDCO = *ALL'*'
022000030127     C                   ENDIF
022100030127     C*
022200030127     C                   ELSE
022300030128     C                   EVAL      STPCOD = 'Tutti'
022400030127     C                   ENDIF
022500030127     C*
022600030127     C                   IF        PARSEL = 'S'
022700030127     C                   EVAL      STPSEL  ='Sì'
022800030127     C                   ELSE
022900030127     C                   EVAL      STPSEL  ='No'
023000030127     C                   ENDIF
023100030127     C*
023200030618     C                   IF        PARSSN = 'S'
023300030127     C                   WRITE     SIGST11
023400030127     C                   WRITE     SIGSPI1
023500030618     C                   ENDIF
023600030127     C*
023700030127     C                   ENDSR
023800030127     C*------------------------------------------------------------------------*
023900030127     C*?STATESD - STAMPA LA TESTATA ED I PARAMETRI DI LANCIO DELLA STAMPA DI DETTAGLIO MOVIMENTI
024000030127     C*------------------------------------------------------------------------*
024100030127     C     STATESD       BEGSR
024200030127     C*
024300030127     C* - STAMPO IL TITOLO
024400030127     C                   WRITE     SIGSPT
024500030127     C*
024600030127     C* - STAMPO I PARAMETRI
024700030128     C                   Z-ADD     PARDDA        DAW8                           * Periodo dal
024800030128     C                   EXSR      CHKDA8
024900030128     C                   EVAL      STPDAL = WDA8
025000030128     C*
025100030128     C                   Z-ADD     PARDAA        DAW8                           * Periodo al
025200030128     C                   EXSR      CHKDA8
025300030128     C                   EVAL      STPAL  = WDA8
025400030128     C*
025500030127     C                   WRITE     SIGST1
025600030127     C                   WRITE     SIGSPI
025700030127     C*
025800030127     C                   ENDSR
025900020718     C*------------------------------------------------------------------------*
026000030124     C*?REPMOV - LEGGE I MOVIMENTI E TOTALIZZA
026100020718     C*------------------------------------------------------------------------*
026200030124     C     REPMOV        BEGSR
026300030124     C*
026400030124     C                   EVAL      W_Fine = '0'
026500030124     C*
026600030124     C* POSIZIONAMENTO SUL FILE MOVIMENTI
026700030124     C                   EVAL      MGBTIP = PARTP1
026800030124     C                   EVAL      MGBSOG = PARSO1
026900030124     C                   EVAL      MGBCOD = PARCO1
027000030124     C     KEYMGBP       SETLL     AZMGB04L                           99
027100030124     C                   IF        *IN99 = *OFF
027200030124     C                   READ      AZMGB04L
027300030124     C                   DOW       not %eof(AZMGB04L) AND W_Fine = '0'
027400030124     C*
027500030124     C* ESEGUO CONTROLLI DI VALIDITA RISPETTO AI PARAMETRI DI LANCIO
027600030124     C                   EXSR      CHKREC
027700030124     C*
027800030124     C* SE RECORD OK E RICHIESTA STAMPA DETTAGLIO => ESEGUO
027900030124     C                   IF        WRECOKDET = 'S' AND
028000030124     C                             PARDET = 'S'
028100030618     C                   EXSR      REPMOVCORR
028200030124     C                   EXSR      STADET
028300030124     C                   ENDIF
028400030127     C*
028500030127     C* SE RECORD OK ESEGUO TOTALIZZAZIONE X CLIENTE / ARTICOLO / CAUSALE
028600030618     C                   IF        WRECOKSAL = 'S' AND
028700030618     C                             PARSSN = 'S'
028800030128     C                   EXSR      TOTROT
028900030127     C                   ENDIF
029000030124     C                   READ      AZMGB04L
029100030124     C                   ENDDO
029200030127     C*
029300030618     C* A FINE CICLO SCARICO LE MEMORIZZAZIONI SOSPESE (SE RICHIESTO NEL LANCIO)
029400030618     C                   IF        PARSSN = 'S'
029500030128     C                   EXSR      STASALCAUTOT
029600030128     C                   EXSR      STASALCODTOT
029700030128     C                   EXSR      STASALSOGTOT
029800030408     C                   EXSR      STASALGENTOT
029900030618     C                   ENDIF
030000030124     C                   ENDIF
030100020718     C*
030200020718     C                   ENDSR
030300030618     C*------------------------------------------------------------------------*
030400030618     C*?REPMOVCORR - LEGGE IL MOVIMENTO CORRELATO A QUELLO CORRENTE
030500030618     C*------------------------------------------------------------------------*
030600030618     C     REPMOVCORR    BEGSR
030700030618     C*
030800030618     C* Inizializzo i campi in stampa relativi al soggetto correlato
030900030618     C                   CLEAR                   STDSOC
031000030618     C                   CLEAR                   STDDE3
031100030618     C*
031200030618     C* Mi posiziono per progressivo univoco (coppia movimenti)
031300030618     C     mgbPRG        SETLL     azmgb03l
031400030618     C                   IF        %equal(azmgb03l)
031500030618     C* Leggo il 1° movimento e verifica codice soggetto
031600030618     C     mgbPRG        READE     azmgb03l
031700030618     C                   IF        not %eof(azmgb03l)
031800030618     C                   IF        C_mgbSOG <> mgbSOG
031900030618     C                   MOVE(P)   C_mgbSOG      STDSOC
032000030618     C                   ELSE
032100030618     C* Leggo il 2° movimento e verifica codice soggetto
032200030618     C     mgbPRG        READE     azmgb03l
032300030618     C                   IF        not %eof(azmgb03l)
032400030618     C                   IF        C_mgbSOG <> mgbSOG
032500030618     C                   MOVE(P)   C_mgbSOG      STDSOC
032600030618     C                   ENDIF
032700030618     C                   ENDIF
032800030618     C                   ENDIF
032900030618     C                   ENDIF
033000030618     C                   ENDIF
033100030618     C*
033200030618     C                   ENDSR
033300030127     C*------------------------------------------------------------------------*
033400030128     C*?TOTROT - ROUTINE DI VERIFICA ROTTURE DI CODICE
033500030127     C*------------------------------------------------------------------------*
033600030128     C     TOTROT        BEGSR
033700030127     C*
033800030127     C* VERIFICO ROTTURA X SOGGETTO
033900030127     C                   IF        mgbTIP = DepTIP AND
034000030127     C                             mgbSOG = DepSOG
034100030127     C                   ELSE
034200030127     C                   IF        DepTIP <> *blanks AND DepSOG <> *blanks
034300030127     C                   EXSR      STASALCAUTOT
034400030127     C                   EXSR      STASALCODTOT
034500030127     C                   EXSR      STASALSOGTOT
034600030128     C                   CLEAR                   WRKTOTCAU
034700030127     C                   ENDIF
034800030127     C                   EVAL      DepTIP = mgbTIP
034900030127     C                   EVAL      DepSOG = mgbSOG
035000030311     C                   EVAL      DepDSO = stdDSO
035100030127     C                   EVAL      DepCOD = mgbCOD
035200030127     C*
035300030127     C* PRIMA DI OGNI NUOVO SOGGETTO LASCIO UNA RIGA VUOTA DI SEPARAZIONE CON I ISALDI PRECEDENTI
035400030127     C                   WRITE     SIGSRI1
035500030127     C*
035600030127     C                   SETOFF                                       707172
035700030127     C                   SETON                                        70
035800030127     C*
035900030127     C                   EXSR      STASALDET
036000030127     C                   ENDIF
036100030127     C*
036200030127     C* VERIFICO ROTTURA X ARTICOLO
036300030127     C                   IF        mgbCOD <> DepCOD
036400030127     C                   IF        DepCOD <> *blanks
036500030127     C                   EXSR      STASALCAUTOT
036600030127     C                   EXSR      STASALCODTOT
036700030128     C                   CLEAR                   WRKTOTCAU
036800030127     C                   ENDIF
036900030127     C                   EVAL      DepCOD = mgbCOD
037000030127     C*
037100030127     C                   EXSR      REPSALCOD
037200030127     C                   ENDIF
037300030127     C*
037400030127     C* TOTALIZZO
037500030128     C                   EXSR      MEMDAT
037600030127     C*
037700030127     C                   ENDSR
037800030128     C*------------------------------------------------------------------------*
037900030128     C*?MEMDAT - ROUTINE DI TOTALIZZAZIONE X SALDI
038000030128     C*------------------------------------------------------------------------*
038100030128     C     MEMDAT        BEGSR
038200030128     C*
038300030128     C* TOTALIZZO
038400030128     C                   EVAL      TotSOG = TotSOG + MGBQTA
038500030408     C                   EVAL      TotSOGGEN = TotSOGGEN + MGBQTA
038600030128     C                   EVAL      TotCOD = TotCOD + MGBQTA
038700030128     C*
038800030128     C                   Z-ADD     1             I
038900030128     C                   SETOFF                                       55
039000030128     C     MGBCAU        LOOKUP    WRKCODCAU(I)                           55
039100030128     C                   IF        %found
039200030128     C                   EVAL      WRKTOTCAU(I) = WRKTOTCAU(I) + MGBQTA
039300030128     C                   ENDIF
039400030128     C*
039500030128     C                   ENDSR
039600030127     C*------------------------------------------------------------------------*
039700030127     C*?STASALDET - ROUTINE DI STAMPA DETTAGLIO X SALDO
039800030127     C*------------------------------------------------------------------------*
039900030127     C     STASALDET     BEGSR
040000030127     C*
040100030127     C* REPERISCO LE DESCRIZIONI X STAMPA SALDI
040200030127     C                   EXSR      REPSALSOG
040300030127     C                   EXSR      REPSALCOD
040400030127     C*
040500030127     C* QUINDI VADO IN STAMPA
040600030128     C                   IF        *IN02 = *ON
040700030128     C                   WRITE     SIGSPT1
040800030128     C                   WRITE     SIGSPI1
040900030128     C                   SETOFF                                       02
041000030128     C                   ENDIF
041100030127     C                   WRITE     SIGSP11
041200030127     C*
041300030127     C                   ENDSR
041400030127     C*------------------------------------------------------------------------*
041500030127     C*?REPSALSOG - ROUTINE DI REPERIMENTO DESCRIZIONI SOGGETTO X STAMPA SALDI
041600030127     C*------------------------------------------------------------------------*
041700030127     C     REPSALSOG     BEGSR
041800030127     C*
041900030128     C                   MOVEL     DepSOG        STDSOG
042000030127     C* REPERISCO DESCRIZIONE SOGGETTO
042100030127     C                   IF        DepTIP = 'C'
042200030127     C                   EVAL      STDDE2 = '(Cli)'
042300030127     C                   EVAL      ACOKUT = 1
042400030127     C                   EVAL      ACOKCC = DUTKCI
042500030127     C                   MOVE      STDSOG        ACOKSC
042600030127     C     KEYACO        CHAIN     CNACO00F
042700030127     C                   IF        %found(CNACO00F)
042800030127     C                   EVAL      STDDSO = ACORAG
042900030127     C                   ELSE
043000030127     C                   EVAL      STDDSO = *ALL'*'
043100030127     C                   ENDIF
043200030127     C                   ENDIF
043300030127     C                   IF        DepTIP = 'D'
043400030127     C                   EVAL      STDDE2 = '(Dest)'
043500030127     C                   MOVE      STDSOG        DSTCOD
043600030127     C     KEYDST        CHAIN     AZDST04L
043700030127     C                   IF        %found(AZDST04L)
043800030127     C                   EVAL      STDDSO = DSTRAG
043900030127     C                   ELSE
044000030127     C                   EVAL      STDDSO = *ALL'*'
044100030127     C                   ENDIF
044200030127     C                   ENDIF
044300030127     C                   IF        DepTIP = 'F'
044400150210     C                   EVAL      STDDE2 = '(Fil.)'
044500030127     C                   MOVE      STDSOG        ORGFIL
044600030127     C     KEYORG        CHAIN     AZORG01L
044700030127     C                   IF        %found(AZORG01L)
044800030127     C                   EVAL      STDDSO = ORGDES
044900030127     C                   ELSE
045000030127     C                   EVAL      STDDSO = *ALL'*'
045100030127     C                   ENDIF
045200030127     C                   ENDIF
045300150408     c                   eval      oldDES  = STDDSO
045400030127     C*
045500030127     C                   ENDSR
045600030127     C*------------------------------------------------------------------------*
045700030127     C*?REPSALCOD - ROUTINE DI REPERIMENTO DESCRIZIONI ARTICOLO X STAMPA SALDI
045800030127     C*------------------------------------------------------------------------*
045900030127     C     REPSALCOD     BEGSR
046000030127     C*
046100030127     C                   EVAL      STDCOD = DepCOD
046200030127     C* REPERISCO DESCRIZOINE ARTICOLO
046300030127     C                   Z-ADD     1             I
046400030127     C                   SETOFF                                       55
046500030127     C     STDCOD        LOOKUP    SKCODART(I)                            55
046600030127     C                   IF        %found
046700030127     C                   EVAL      STDDCO = SKDESART(I)
046800030127     C                   ELSE
046900030127     C                   EVAL      STDDCO = *ALL'*'
047000030127     C                   ENDIF
047100150408     c                   eval      oldCOD  = STDcod
047200150408     c                   eval      oldDCO  = STDdco
047300030127     C*
047400030127     C                   ENDSR
047500030127     C*------------------------------------------------------------------------*
047600030127     C*?REPSALCAU - ROUTINE DI REPERIMENTO DESCRIZIONI CAUSALE X STAMPA SALDI
047700030127     C*------------------------------------------------------------------------*
047800030127     C     REPSALCAU     BEGSR
047900030127     C*
048000030127     C                   EVAL      STDCAU = WRKCODCAU(J)
048100030127     C* REPERISCO TIPO E DESCRIZIONE CAUSALE
048200030127     C                   Z-ADD     1             I
048300030127     C                   SETOFF                                       55
048400030127     C     STDCAU        LOOKUP    SKCODCAU(I)                            55
048500030127     C                   IF        %found
048600030127     C                   EVAL      DGGB = SKUNICAU(I)
048700030127     C                   EVAL      STDTIM = §GGBTIM
048800030127     C                   EVAL      STDDCA = §GGBDES
048900030127     C                   ELSE
049000030127     C                   EVAL      STDTIM = *ALL'*'
049100030127     C                   EVAL      STDDCA = *ALL'*'
049200030127     C                   ENDIF
049300030127     C*
049400030127     C                   ENDSR
049500030127     C*------------------------------------------------------------------------*
049600030127     C*?STASALSOGTOT - ROUTINE DI STAMPA TOTALE SALDO X SOGGETTO
049700030127     C*------------------------------------------------------------------------*
049800030127     C     STASALSOGTOT  BEGSR
049900030127     C*
050000030127     C                   EVAL      STTSAL = TotSOG
050100030127     C                   EVAL      STTDD3 = 'SALDO SOGGETTO: ' +
050200030311     C***                                   %editw(DepSOG:'        ') + ' ' +
050300030311     C                                      %subst(DepSOG:2:7) + ' ' +
050400031113     C***                                   DepDSO
050500150324     C***                                   stdDSO
050600150324     C                                      oldDES
050700030127     C*
050800030128     C                   IF        *IN02 = *ON
050900030128     C                   WRITE     SIGSPT1
051000030128     C                   WRITE     SIGSPI1
051100030128     C                   SETOFF                                       02
051200030128     C                   ENDIF
051300030127     C                   WRITE     SIGSP21
051400030127     C*
051500030127     C                   Z-ADD     *zeros        TotSOG
051600030127     C*
051700030127     C                   ENDSR
051800030408     C*------------------------------------------------------------------------*
051900030408     C*?STASALGENTOT - ROUTINE DI STAMPA TOTALE SALDO GENERALE SOGGETTO
052000030408     C*------------------------------------------------------------------------*
052100030408     C     STASALGENTOT  BEGSR
052200030408     C*
052300030408     C* PRIMA DELLA RIGA DI TOTALE GENERALE STAMPO UNA RIGA VUOTA
052400030408     C                   WRITE     SIGSRI1
052500030408     C*
052600030408     C                   EVAL      STTSAL = TotSOGGEN
052700030408     C                   EVAL      STTDD3 = 'SALDO MOVIMENTI STAMPATI: '
052800030408     C*
052900030408     C                   IF        *IN02 = *ON
053000030408     C                   WRITE     SIGSPT1
053100030408     C                   WRITE     SIGSPI1
053200030408     C                   SETOFF                                       02
053300030408     C                   ENDIF
053400030408     C                   WRITE     SIGSP21
053500030408     C*
053600030408     C                   Z-ADD     *zeros        TotSOGGEN
053700030408     C*
053800030408     C                   ENDSR
053900030127     C*------------------------------------------------------------------------*
054000030127     C*?STASALCODTOT - ROUTINE DI STAMPA TOTALE SALDO X ARTICOLO
054100030127     C*------------------------------------------------------------------------*
054200030127     C     STASALCODTOT  BEGSR
054300030127     C*
054400030127     C                   EVAL      STTSAL = TotCOD
054500150408     C*****              EVAL      STTDD3 = '      ' + 'SALDO ARTICOLO: ' +
054600150408     C*****                                 STDCOD + ' ' + STDDCO
054700150408     C                   EVAL      STTDD3 = '      ' + 'SALDO ARTICOLO: ' +
054800150408     C                                      oldCOD + ' ' + oldDCO
054900030127     C*
055000030128     C                   IF        *IN02 = *ON
055100030128     C                   WRITE     SIGSPT1
055200030128     C                   WRITE     SIGSPI1
055300030128     C                   SETOFF                                       02
055400030128     C                   ENDIF
055500030127     C                   WRITE     SIGSP21
055600030127     C*
055700030127     C                   Z-ADD     *zeros        TotCOD
055800030127     C*
055900030127     C                   ENDSR
056000030127     C*------------------------------------------------------------------------*
056100030127     C*?STASALCAUTOT - ROUTINE DI STAMPA TOTALE SALDO X CAUSALE
056200030127     C*------------------------------------------------------------------------*
056300030127     C     STASALCAUTOT  BEGSR
056400030127     C*
056500030127     C* SE RICHIESTO NEL LANCIO
056600030127     C                   IF        PARSEL = 'S'
056700030127     C*
056800030127     C* CICLO SULLA SCHIERA DI MEMORIZZAZIONE CAUSALI
056900030127     C     1             DO        100           J
057000030128     C                   IF        WRKTOTCAU(J) <> *zeros
057100030127     C                   EXSR      REPSALCAU
057200030127     C                   EVAL      STTSAL = WRKTOTCAU(J)
057300030127     C                   EVAL      STTDD3 = '           ' + 'SALDO CAUSALE: ' +
057400030128     C                                      STDCAU + ' ' + STDDCA
057500030127     C*
057600030128     C                   IF        *IN02 = *ON
057700030128     C                   WRITE     SIGSPT1
057800030128     C                   WRITE     SIGSPI1
057900030128     C                   SETOFF                                       02
058000030128     C                   ENDIF
058100030127     C                   WRITE     SIGSP21
058200030127     C                   ENDIF
058300030127     C                   ENDDO
058400030127     C                   ENDIF
058500030127     C*
058600030127     C                   ENDSR
058700020718     C*------------------------------------------------------------------------*
058800030124     C*?CHKREC - ROUTINE DI CONTROLLO VALIDITA' RECORD
058900020718     C*------------------------------------------------------------------------*
059000030124     C     CHKREC        BEGSR
059100020725     C*
059200030124     C                   MOVEL     'S'           WRECOKDET
059300030124     C                   MOVEL     'S'           WRECOKSAL
059400030124     C*
059500030124     C* SE RICHIESTO UN TIPO/SOGGETTO CONTROLLO
059600030124     C                   IF        PARTP1 <> *BLANKS
059700030124     C                   IF        MGBTIP = PARTP1
059800030124     C                   ELSE
059900030124     C                   MOVEL     'N'           WRECOKDET
060000030124     C                   MOVEL     'N'           WRECOKSAL
060100030124     C                   ENDIF
060200030124     C                   ENDIF
060300030612     C                   IF        PARTP1 =  'C'     AND
060400030612     C                             PARPO1 <> *ZEROS  AND
060500030612     C                             PARPO1 <> *BLANKS
060600030305     C                   IF        MGBTIP = PARTP1 AND
060700030305     C                             %subst(MGBSOG:2:3) = PARPO1
060800030305     C                   ELSE
060900030305     C                   MOVEL     'N'           WRECOKDET
061000030305     C                   MOVEL     'N'           WRECOKSAL
061100030305     C                   ENDIF
061200030305     C                   ENDIF
061300030305     C                   IF        PARSO1 <> *ZEROS
061400030124     C                   IF        MGBTIP = PARTP1 AND
061500030124     C                             MGBSOG = PARSO1
061600030124     C                   ELSE
061700030124     C                   EVAL      W_Fine = '1'
061800030124     C                   MOVEL     'N'           WRECOKDET
061900030124     C                   MOVEL     'N'           WRECOKSAL
062000030124     C                   ENDIF
062100030124     C                   ENDIF
062200030124     C*
062300030124     C* SE RICHIESTO UN ARTICOLO CONTROLLO
062400030124     C                   IF        PARCO1 <> *BLANKS
062500030124     C                   IF        MGBCOD = PARCO1
062600030124     C                   ELSE
062700030124     C                   MOVEL     'N'           WRECOKDET
062800030124     C                   MOVEL     'N'           WRECOKSAL
062900030124     C                   ENDIF
063000030124     C                   ENDIF
063100030618     C*
063200030618     C* SE RICHIESTA UNA/DUE CAUSALI
063300030618     C                   IF        (PARCA1 <> *BLANKS AND MGBCAU = PARCA1)  OR
063400030618     C                             (PARCA2 <> *BLANKS AND MGBCAU = PARCA2)  OR
063500030618     C                             (PARCA1  = *BLANKS AND PARCA2 = *BLANKS)
063600030618     C                   ELSE
063700030618     C                   MOVEL     'N'           WRECOKDET
063800030618     C                   MOVEL     'N'           WRECOKSAL
063900030618     C                   ENDIF
064000030124     C*
064100030127     C* SE RICHIESTA UNA DATA "SALDO AL" CONTROLLO
064200030124     C                   IF        PARDA1 > *ZEROS
064300030124     C                   IF        MGBDAT <= PARDA1
064400030124     C                   ELSE
064500030124     C                   MOVEL     'N'           WRECOKSAL
064600030124     C                   ENDIF
064700030124     C                   ENDIF
064800030124     C*
064900030124     C* SE RICHIESTA DETTAGLIO CONTROLLO LE DATE DEL PERIODO IMMESSO NEL LANCIO
065000030124     C                   IF        PARDET = 'S'
065100030124     C                   IF        MGBDAT >= PARDDA AND
065200030124     C                             MGBDAT <= PARDAA
065300030124     C                   ELSE
065400030124     C                   MOVEL     'N'           WRECOKDET
065500030124     C                   ENDIF
065600030124     C                   ENDIF
065700020718     C*
065800020718     C                   ENDSR
065900021120     C*------------------------------------------------------------------------*
066000030124     C*?STADET - ROUTINE DI STAMPA DETTAGLIO MOVIMENTI
066100021120     C*------------------------------------------------------------------------*
066200021120     C     STADET        BEGSR
066300021120     C*
066400030127     C                   Z-ADD     MGBDAT        DAW8                           * Periodo dal
066500030127     C                   EXSR      CHKDA8
066600030127     C                   EVAL      STDDAT = WDA8
066700030124     C                   EVAL      STDORA = MGBORA
066800030124     C                   EVAL      STDCAU = MGBCAU
066900030124     C* REPERISCO TIPO E DESCRIZIONE CAUSALE
067000030124     C                   Z-ADD     1             I
067100030124     C                   SETOFF                                       55
067200030124     C     STDCAU        LOOKUP    SKCODCAU(I)                            55
067300030124     C                   IF        %found
067400030124     C                   EVAL      DGGB = SKUNICAU(I)
067500030124     C                   EVAL      STDTIM = §GGBTIM
067600030124     C                   EVAL      STDDCA = §GGBDES
067700030124     C                   ELSE
067800030124     C                   EVAL      STDTIM = *ALL'*'
067900030124     C                   EVAL      STDDCA = *ALL'*'
068000030124     C                   ENDIF
068100030618     C* SOGGETTO MOVIMENTO
068200030128     C                   MOVEL     MGBSOG        STDSOG
068300030618     C* REPERISCO DESCRIZIONE SOGGETTO MOVIMENTO
068400030124     C                   IF        MGBTIP = 'C'
068500030124     C                   EVAL      STDDE2 = '(Cli)'
068600030124     C                   EVAL      ACOKUT = 1
068700030124     C                   EVAL      ACOKCC = DUTKCI
068800030124     C                   MOVE      STDSOG        ACOKSC
068900030124     C     KEYACO        CHAIN     CNACO00F
069000030124     C                   IF        %found(CNACO00F)
069100030124     C                   EVAL      STDDSO = ACORAG
069200030124     C                   ELSE
069300030124     C                   EVAL      STDDSO = *ALL'*'
069400030124     C                   ENDIF
069500030124     C                   ENDIF
069600030124     C                   IF        MGBTIP = 'D'
069700030124     C                   EVAL      STDDE2 = '(Dest)'
069800030124     C                   MOVE      STDSOG        DSTCOD
069900030124     C     KEYDST        CHAIN     AZDST04L
070000030124     C                   IF        %found(AZDST04L)
070100030124     C                   EVAL      STDDSO = DSTRAG
070200030124     C                   ELSE
070300030124     C                   EVAL      STDDSO = *ALL'*'
070400030124     C                   ENDIF
070500030124     C                   ENDIF
070600030124     C                   IF        MGBTIP = 'F'
070700150210     C                   EVAL      STDDE2 = '(Fil.)'
070800030124     C                   MOVE      STDSOG        ORGFIL
070900030124     C     KEYORG        CHAIN     AZORG01L
071000030124     C                   IF        %found(AZORG01L)
071100030124     C                   EVAL      STDDSO = ORGDES
071200030124     C                   ELSE
071300030124     C                   EVAL      STDDSO = *ALL'*'
071400030124     C                   ENDIF
071500030124     C                   ENDIF
071600030618     C* SOGGETTO MOVIMENTO CORRELATO SE PRESENTE
071700030618     C                   IF        STDSOC > *zeros
071800030618     C* REPERISCO DESCRIZIONE SOGGETTO MOVIMENTO CORRELATO
071900030618     C                   IF        C_MGBTIP = 'C'
072000030618     C                   EVAL      STDDE3 = '(Cli)'
072100030618     C                   EVAL      ACOKUT = 1
072200030618     C                   EVAL      ACOKCC = DUTKCI
072300030618     C                   MOVE      STDSOC        ACOKSC
072400030618     C     KEYACO        CHAIN     CNACO00F
072500030618     C                   IF        %found(CNACO00F)
072600030618     C                   EVAL      STDDSC = ACORAG
072700030618     C                   ELSE
072800030618     C                   EVAL      STDDSC = *ALL'*'
072900030618     C                   ENDIF
073000030618     C                   ENDIF
073100030618     C                   IF        C_MGBTIP = 'D'
073200030618     C                   EVAL      STDDE3 = '(Dest)'
073300030618     C                   MOVE      STDSOC        DSTCOD
073400030618     C     KEYDST        CHAIN     AZDST04L
073500030618     C                   IF        %found(AZDST04L)
073600030618     C                   EVAL      STDDSC = DSTRAG
073700030618     C                   ELSE
073800030618     C                   EVAL      STDDSC = *ALL'*'
073900030618     C                   ENDIF
074000030618     C                   ENDIF
074100030618     C                   IF        C_MGBTIP = 'F'
074200150210     C                   EVAL      STDDE3 = '(Fil.)'
074300030618     C                   MOVE      STDSOC        ORGFIL
074400030618     C     KEYORG        CHAIN     AZORG01L
074500030618     C                   IF        %found(AZORG01L)
074600030618     C                   EVAL      STDDSC = ORGDES
074700030618     C                   ELSE
074800030618     C                   EVAL      STDDSC = *ALL'*'
074900030618     C                   ENDIF
075000030618     C                   ENDIF
075100030618     C                   ENDIF
075200030124     C*
075300030124     C                   EVAL      STDCOD = MGBCOD
075400030124     C* REPERISCO DESCRIZOINE ARTICOLO
075500030124     C                   Z-ADD     1             I
075600030124     C                   SETOFF                                       55
075700030124     C     STDCOD        LOOKUP    SKCODART(I)                            55
075800030124     C                   IF        %found
075900030124     C                   EVAL      STDDCO = SKDESART(I)
076000030124     C                   ELSE
076100030124     C                   EVAL      STDDCO = *ALL'*'
076200030124     C                   ENDIF
076300030124     C*
076400030124     C                   EVAL      STDQTA = MGBQTA
076500030124     C                   EVAL      STDRDO = MGBRDO
076600030124     C                   EVAL      STDUTE = MGBUTE
076700021120     C*
076800030128     C                   IF        *IN01 = *ON
076900030124     C                   WRITE     SIGSPT
077000030124     C                   WRITE     SIGSPI
077100030128     C                   SETOFF                                       01
077200021120     C                   ENDIF
077300030124     C                   WRITE     SIGSP1
077400021120     C*
077500021120     C                   ENDSR
077600030124     C*------------------------------------------------------------------------*
077700030124     C* CHKDA8 - CONTROLLA LA DATA (8)
077800030124     C*------------------------------------------------------------------------*
077900030124     C     CHKDA8        BEGSR
078000030124     C*
078100030124     C                   IF        DAW8 > *ZEROS
078200030124     C                   Z-ADD     *ZEROS        G08DAT
078300030124     C                   Z-ADD     DAW8          G08INV
078400030124     C                   MOVEL     '3'           G08ERR
078500030124     C                   CALL      'XSRDA8'
078600030124     C                   PARM                    WLBDA8
078700030124     C                   Z-ADD     G08DAT        WDA8
078800030124     C                   Z-ADD     G08INV        DAW8
078900030124     C                   ELSE
079000030124     C                   Z-ADD     WDA8          G08DAT
079100030124     C                   Z-ADD     *ZEROS        G08INV
079200030124     C                   MOVEL     '0'           G08ERR
079300030124     C                   CALL      'XSRDA8'
079400030124     C                   PARM                    WLBDA8
079500030124     C                   Z-ADD     G08DAT        WDA8
079600030124     C                   Z-ADD     G08INV        DAW8
079700030124     C                   ENDIF
079800030124     C*
079900030124     C                   ENDSR
080000020923     C*--------------------------------------------------------------------------------------------*
080100020923     C* REPERISCE I DATI UTENTE
080200020923     C*--------------------------------------------------------------------------------------------*
080300020923     C     REPDATIUTE    BEGSR
080400020923     C*
080500020923     C* INIZIALIZZA VARIABILI DI WRK
080600020923     C                   CLEAR                   TIBS34DS
080700020923     C                   CLEAR                   AZUTEDS
080800020923     C                   CLEAR                   DDATIUTE
080900020923     C*
081000020923     C     *DTAARA       DEFINE    §azute        azuteds
081100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
081200020923     C                   IN(E)     *DTAARA
081300030124     C                   IF        %Error
081400030124     C                   EVAL      I34Tla = 'L'
081500020923     C                   CALL      'TIBS34R'
081600020923     C                   PARM                    Tibs34Ds
081700020923     C                   IN        *DTAARA
081800030124     C                   ENDIF
081900020923     C*
082000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
082100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
082200020923     C*
082300020923     C                   ENDSR
082400030124     C*--------------------------------------------------------------------------------------------*
082500030124     C* CARTAB - CARICAMENTO DATI TABELLATI
082600030124     C*--------------------------------------------------------------------------------------------*
082700030124     C     CARTAB        BEGSR
082800030124     C*
082900030124     C* CARICAMENTO CAUSALI
083000030124     C                   Z-ADD     *ZEROS        I
083100030124     C                   EVAL      tbeCOD = 'GGB'
083200030124     C     tbeCOD        SETLL     TNTBE01L                           55
083300030124     C                   IF        *IN55 = *OFF
083400030124     C     tbeCOD        READE     TNTBE01L
083500030124     C                   DOW       not %eof(TNTBE01L)
083600030124     C                   ADD       1             I
083700030124     C                   EVAL      SKCODCAU(I) = tbeKE1
083800030124     C                   EVAL      SKUNICAU(I) = tbeUNI
083900030124     C     tbeCOD        READE     TNTBE01L
084000030124     C                   ENDDO
084100030124     C                   ENDIF
084200030127     C* AL TERMINE DUPLICO LA SCHIERA CAUSALI E SORTO IN ORDINE CRESCENTE QUEST'ULTIMA
084300030127     C                   MOVEA     SKCODCAU      WRKCODCAU
084400030127     C                   SORTA     WRKCODCAU
084500030124     C*
084600030124     C* CARICAMENTO ARTICOLI
084700030124     C                   Z-ADD     *ZEROS        I
084800030124     C     *LOVAL        SETLL     AZAGB01L                           55
084900030124     C                   IF        *IN55 = *OFF
085000030124     C                   READ      AZAGB01L
085100030124     C                   DOW       not %eof(AZAGB01L)
085200030124     C                   ADD       1             I
085300030124     C                   EVAL      SKCODART(I) = agbCOD
085400030124     C                   EVAL      SKDESART(I) = agbDES
085500030124     C                   READ      AZAGB01L
085600030124     C                   ENDDO
085700030124     C                   ENDIF
085800030124     C*
085900030124     C                   ENDSR
086000000000     C*------------------------------------------------------------------------*
086100000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
086200000000     C*------------------------------------------------------------------------*
086300000000     C     *INZSR        BEGSR
086400000000     C*
086500000000     C* PARAMETRI IN INPUT AL PGM
086600000000     C     *ENTRY        PLIST
086700020719     C                   PARM                    KPJBA
086800030124     C                   MOVEL     KPJBU         PARAM
086900020923     C*
087000020923     C* REPERISCE I DATI DELL'UTENTE
087100020923     C                   EXSR      REPDATIUTE
087200030124     C*
087300030124     C* CARICA I DATI TABELLATI
087400030124     C                   EXSR      CARTAB
087500030128     C*
087600030128     C* ACCENDO SUBITO QUI INDICATORE X STAMPA DESCRIZIONE SOGGETTO IN GRASSETTO
087700030128     C                   SETON                                        80
087800030127     C*
087900030127     C* REPERISCO DATA E ORA CORRENTI
088000030127     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
088100030127     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
088200030127     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
088300030127     C                   Z-ADD     WN8           G08DAT
088400030127     C                   Z-ADD     *ZEROS        G08INV
088500030127     C                   MOVEL     '0'           G08ERR
088600030127     C                   CALL      'XSRDA8'
088700030127     C                   PARM                    WLBDA8
088800030127     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
088900030124     C*
089000030124     C* CHIAVE LETTURA AZMGB04L - PARZIALE
089100030124     C     KEYMGBP       KLIST
089200030124     C                   KFLD                    MGBTIP
089300030124     C                   KFLD                    MGBSOG
089400030124     C                   KFLD                    MGBCOD
089500020718     C*
089600020718     C* CHIAVE LETTURA CNACO00F - COMPLETA
089700020718     C     KEYACO        KLIST
089800020718     C                   KFLD                    ACOKUT
089900020718     C                   KFLD                    ACOKCC
090000020718     C                   KFLD                    ACOKSC
090100020722     C*
090200030124     C* CHIAVE LETTURA TNTBE01L - PARZIALE
090300030124     C     KEYTBEP       KLIST
090400030124     C                   KFLD                    TBECOD
090500030124     C                   KFLD                    TBEKE1
090600030124     C*
090700030124     C* CHIAVE LETTURA AZORG01L - COMPLETA
090800030124     C     KEYORG        KLIST
090900030124     C                   KFLD                    ORGFIL
091000030124     C*
091100030124     C* CHIAVE LETTURA AZDST04L - COMPLETA
091200030124     C     KEYDST        KLIST
091300030124     C                   KFLD                    DSTCOD
091400020718     C*
091500000000     C                   ENDSR
