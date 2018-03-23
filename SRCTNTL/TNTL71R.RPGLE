000100000000     H*------------------------------------------------------------------------*
000200000000     H* CONTO ECONOMICO TRAINI - CREAZIONE WFILE
000300000000     H* NB: CREA IL WORKFILE DALLA DATA ULTIMA DI PULIZIA GRIGLIA -SE MAGGIORE
000400000000     H*     DELLA DATA INIZIALE RICHIESTA A VIDEO
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000     F*------------------------------------------------------------------------*
000800000000     F* DATA BASE
000900000000     F*------------------------------------------------------------------------*
001000000000     F*---
001100000000     F* STORICO TRAINI
001200000000     F*---
001300000000     FTNTST03L  IF   E           K DISK
001400000000     FTNTSM05L  IF   E           K DISK
001500000000     F*---
001600000000     F* WORKFILE
001700000000     F*---
001800011022     FTETEC01L  UF A E           K DISK
001900000000     F*---
002000000000     F* TARIFFE DI C/ECONOMICO
002100000000     F*---
002200140114     FECEVD06L  IF   E           K DISK
002300000000     F*---
002400000000     F* TABELLE
002500000000     F*---
002600000000     FTABEL00F  IF   E           K DISK
002700011022     FTNTBE01L  IF   E           K DISK
002800000000     F*---
002900000000     F* ORGANIGRAMMA
003000000000     F*---
003100000000     FAZORG01L  IF   E           K DISK
003200000000     D*------------------------------------------------------------------------*
003300011022     D* DATA STRUCTURE
003400000000     D*------------------------------------------------------------------------*
003500011022     D*------------------
003600011022     D* Variabili di controllo
003700011022     D*------------------
003800011022     D $DIVAC          S              3                                         *div INP anag C/Eco
003900011022     D $DIVSTE         S              3                                         *div OUT C/E traini
004000000000     D*------------------
004100011022     D* TIPI TRAINO
004200000000     D*------------------
004300000000     D TTR             S              1    DIM(20)                              *TIPO TRAINO DA USAR
004400000000     D*------------------
004500000000     D* ORGANIGRAMMA
004600000000     D*------------------
004700000000     D ORG             S              3  0 DIM(500)                             *CODICE
004800000000     D ODIT            S              3    DIM(500)                             *DITTA
004900011022     D*------------------
005000011022     D* DS LETTURA TABELLA "GED" PER DIVISE
005100011022     D*------------------
005200011022     D DGEDST        E DS                                                       *STATISTICHE
005300011022     D DGEDAC        E DS                                                       *ANAGRAFICHE C/E
005400000000     D*------------------
005500000000     D* DS TIPI TRAINO
005600000000     D*------------------
005700000000     D DSTV          E DS
005800000000     D*------------------
005900000000     D* DS "FNLV55R" - REPERIMENTO TERMINALs
006000000000     D*------------------
006100000000     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006200011022     D*-------------------
006300011022     D* CONVERSIONI IMPORTI
006400011022     D*-------------------
006500011022     D YEUR          E DS                  EXTNAME(YEURCODS)
006600000000     D*------------------
006700000000     D* DS UNIONE DATA STORICO MC
006800000000     D*------------------
006900000000     D                 DS
007000000000     D  TSMDAT                 1      6  0
007100000000     D  TSMDTN                 1      8  0
007200011022     D*------------------
007300011022     D* CONTROLLA DATA
007400011022     D*------------------
007500011022     D wlbda8          DS                  INZ                                  *controlla data
007600011022     D  g08dat                        8  0                                       -data dritta
007700011022     D  g08inv                        8  0                                       -data invertita
007800011022     D  g08err                        1                                          -errore
007900011022     D  g08tgi                        5  0                                       -giorni fra date
008000000000     D*------------------
008100000000     D* PARAMETRI DI LANCIO
008200000000     D*------------------
008300000000     D PARAM           DS
008400000000     D  PARORD                 1      1
008500000000     D  PARCRE                 2      2
008600000000     D  PARDAI                 3     10  0
008700000000     D  PARDAF                11     18  0
008800000000     D  PARTRI                19     25  0
008900000000     D  PARTRF                26     32  0
009000000000     D  PARTFP                33     35  0
009100000000     D*------------------
009200000000     D* ARCHITETTURA
009300000000     D*------------------
009400000000     D KPJBA         E DS
009500000000     C*------------------------------------------------------------------------*
009600000000     C* MAIN LINE
009700000000     C*------------------------------------------------------------------------*
009800000000     C*
009900000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
010000000000     C                   EXSR      SETFIL
010100000000     C*
010200000000     C* CICLO FINO A FINE FILE
010300000000DO  1C     $FINE         DOWEQ     'N'
010400000000     C*
010500000000     C* ELABORA I METRICUBI DEL TRAINO
010600000000     C                   EXSR      ELATSM
010700000000     C*
010800000000     C* LEGGE IL RECORD SUCCESSIVO
010900000000     C                   EXSR      LETFIL
011000000000E   1C                   ENDDO                                                  *FINE FILE
011100000000     C*
011200000000     C* OPERAZIONI FINALI
011300000000     C                   EXSR      FINSR
011400000000     C*
011500000000     C                   SETON                                        LR
011600000000     C*------------------------------------------------------------------------*
011700000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
011800000000     C*------------------------------------------------------------------------*
011900000000     C     SETFIL        BEGSR
012000000000     C*
012100000000     C                   Z-ADD     WDPTST        KSTDTN                         *DATA ULTIMA PULIZIA
012200000000     C     KEYTST        SETLL     TNTST03L                           99        *FINE ARCHIVIO
012300000000IF  1C     *IN99         IFEQ      *ON
012400000000     C                   MOVEL     'S'           $FINE
012500000000X   1C                   ELSE
012600000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
012700000000E   1C                   ENDIF
012800000000     C*
012900000000     C                   ENDSR
013000000000     C*------------------------------------------------------------------------*
013100000000     C* LETFIL - LETTURA PROSSIMO RECORD
013200000000     C*------------------------------------------------------------------------*
013300000000     C     LETFIL        BEGSR
013400000000     C*
013500000000     C* LEGGE FINO A:
013600000000     C*     - FINE FILE
013700000000     C*     - TROVATO RECORD VALIDO
013800000000     C                   MOVEL     'N'           WRECOK
013900000000DO  1C     $FINE         DOWEQ     'N'
014000000000     C     WRECOK        ANDEQ     'N'
014100000000     C                   READ      TNTST03L                               99
014200000000IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
014300000000     C                   MOVEL     'S'           $FINE
014400000000X   2C                   ELSE
014500000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
014600000000E   2C                   ENDIF
014700000000E   1C                   ENDDO
014800000000     C*
014900000000     C                   ENDSR
015000000000     C*------------------------------------------------------------------------*
015100000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
015200000000     C*------------------------------------------------------------------------*
015300000000     C     CHKREC        BEGSR
015400000000     C*
015500000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
015600000000     C*
015700000000     C* PERIODO NON RICHIESTO, ESCLUDE
015800000000IF  1C     TSTDTN        IFGE      WDPTST                                       > DATI IN GRIGLIA
015900000000     C     TSTDTN        ANDLE     PARDAF                                       < DATA FINALE CHIEST
016000000000X   1C                   ELSE
016100000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
016200000000E   1C                   ENDIF
016300000000     C*
016400000000     C* TRAINO ANNULLATO, ESCLUDE
016500000000IF  1C     TSTATB        IFNE      *BLANKS
016600000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
016700000000E   1C                   ENDIF
016800000000     C*
016900000000     C* TRAINO NON CONFERMATO, ESCLUDE
017000000000IF  1C     TSTCNF        IFEQ      *BLANKS
017100000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
017200000000E   1C                   ENDIF
017300000000     C*
017400000000     C* TRAINO DA NON USARE, ESCLUDE
017500000000     C                   SETOFF                                           28
017600000000     C     TSTTTR        LOOKUP    TTR                                    28
017700000000IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
017800000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
017900000000E   1C                   ENDIF
018000000000     C*
018100000000     C                   ENDSR
018200000000     C*------------------------------------------------------------------------*
018300000000     C* ELATSM - ELABORA I METRICUBI DEL TRAINO
018400000000     C*------------------------------------------------------------------------*
018500000000     C     ELATSM        BEGSR
018600000000     C*
018700000000     C                   MOVEL     'N'           $FTSM
018800000000     C*
018900000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
019000000000     C                   EXSR      SETTSM
019100000000     C*
019200000000     C* CICLO FINO A FINE FILE
019300000000DO  1C     $FTSM         DOWEQ     'N'
019400000000     C*
019500000000     C* OPERAZIONI PER NUOVA LINEA
019600000000     C                   EXSR      NEWLIN
019700000000     C*
019800000000     C* CICLO FINO A ROTTURA LINEA
019900000000DO  2C     $FTSM         DOWEQ     'N'
020000000000     C     TSMTFP        ANDEQ     DEPTFP
020100000000     C     TSMTFF        ANDEQ     DEPTFF
020200000000     C*
020300000000     C* AGGIORNA IL FILE DI LAVORO
020400000000     C                   EXSR      AGGTEC
020500000000     C*
020600000000     C* LEGGE IL RECORD SUCCESSIVO
020700000000     C                   EXSR      LETTSM
020800000000E   2C                   ENDDO                                                  *ROTTURA LINEA
020900000000E   1C                   ENDDO                                                  *FINE FILE
021000000000     C*
021100000000     C                   ENDSR
021200000000     C*------------------------------------------------------------------------*
021300000000     C* SETTSM - POSIZIONAMENTO E PRIMA LETTURA -TNTSM-
021400000000     C*------------------------------------------------------------------------*
021500000000     C     SETTSM        BEGSR
021600000000     C*
021700000000     C                   Z-ADD     TSTTRN        KSMTRN                         *NUMERO TRAINO
021800000000     C                   Z-ADD     TSTDTN        KSMDTN                         *DATA TRAINO
021900000000     C     KEYTSM        SETLL     TNTSM05L                           98        *FINE ARCHIVIO
022000000000IF  1C     *IN98         IFEQ      *ON
022100000000     C                   MOVEL     'S'           $FTSM
022200000000X   1C                   ELSE
022300000000     C                   EXSR      LETTSM                                       *LETTURA RECORD
022400000000E   1C                   ENDIF
022500000000     C*
022600000000     C                   ENDSR
022700000000     C*------------------------------------------------------------------------*
022800000000     C* LETTSM - LETTURA PROSSIMO RECORD -TNTSM-
022900000000     C*------------------------------------------------------------------------*
023000000000     C     LETTSM        BEGSR
023100000000     C*
023200000000     C* LEGGE FINO A:
023300000000     C*     - FINE FILE
023400000000     C*     - TROVATO RECORD VALIDO
023500000000     C                   MOVEL     'N'           WRECOK
023600000000DO  1C     $FTSM         DOWEQ     'N'
023700000000     C     WRECOK        ANDEQ     'N'
023800000000     C     KEYTSM        READE     TNTSM05L                               98
023900000000IF  2C     *IN98         IFEQ      *ON                                          *FINE ARCHIVIO
024000000000     C                   MOVEL     'S'           $FTSM
024100000000X   2C                   ELSE
024200000000     C                   EXSR      CHKTSM                                       *CONTROLLO RECORD
024300000000E   2C                   ENDIF
024400000000E   1C                   ENDDO
024500000000     C*
024600000000     C                   ENDSR
024700000000     C*------------------------------------------------------------------------*
024800000000     C* CHKTSM - CONTROLLA VALIDITA' RECORD -TNTSM-
024900000000     C*------------------------------------------------------------------------*
025000000000     C     CHKTSM        BEGSR
025100000000     C*
025200000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
025300000000     C*
025400000000     C* METRICUBI ANNULLATI, ESCLUDE
025500000000IF  1C     TSMATB        IFNE      *BLANKS
025600000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025700000000E   1C                   ENDIF
025800000000     C*
025900000000     C                   ENDSR
026000000000     C*------------------------------------------------------------------------*
026100000000     C* AGGTEC - AGGIORNA IL FILE DI LAVORO
026200000000     C*------------------------------------------------------------------------*
026300000000     C     AGGTEC        BEGSR
026400000000     C*
026500000000     C* LEGGE IL RECORD DEL WORKFILE
026600011022     C                   Z-ADD     TSMTRN        KECTRN                         *NUMERO TRAINO
026700011022     C                   Z-ADD     TSMDTN        KECDTN                         *DATA TRAINO
026800011022     C                   Z-ADD     TSMTFP        KECTFP                         *FILIALE PARTENZA
026900011022     C                   Z-ADD     TSMTFF        KECTFA                         *FILIALE ARRIVO
027000011022     C     KEYTEC        CHAIN     TETEC01L                           97
027100000000     C* INSERIMENTO
027200000000IF  1C     *IN97         IFEQ      *ON
027300011022     C                   CLEAR                   TETEC000
027400011022     C                   Z-ADD     KECTRN        TECTRN                         *NUMERO TRAINO
027500011022     C                   Z-ADD     KECDTN        TECDTN                         *DATA TRAINO
027600011022     C                   Z-ADD     KECTFP        TECTFP                         *FILIALE PARTENZA
027700011022     C                   Z-ADD     KECTFA        TECTFA                         *FILIALE ARRIVO
027800011022     C                   MOVEL     $DIVSTE       TECDIV                         *DIVISA
027900000000     C     TSMMCA        ADD       TSMMCB        TECMCA                         *MC ASSEGNATI
028000000000     C                   Z-ADD     TSMMCR        TECMCR                         *MC REALI
028100090923     C     TECMCA        MULT(h)   DEPITR        TECCMA                         *COMPETENZA MC ASS
028200090923     C     TECMCR        MULT(h)   DEPITR        TECCMR                         *COMPETENZA MC REALI
028300011022     C                   WRITE     TETEC000
028400000000     C* AGGIORNAMENTO
028500000000X   1C                   ELSE
028600000000     C                   ADD       TSMMCA        TECMCA                         *MC ASSEGNATI
028700000000     C                   ADD       TSMMCB        TECMCA                         *MC ASSEGNATI
028800000000     C                   ADD       TSMMCR        TECMCR                         *MC REALI
028900090923     C     TSMMCA        MULT(h)   DEPITR        N113                           *MC X TARIFFA
029000011022     C                   ADD       N113          TECCMA                         *COMPETENZA MC ASS
029100090923     C     TSMMCB        MULT(h)   DEPITR        N113                           *MC X TARIFFA
029200011022     C                   ADD       N113          TECCMA                         *COMPETENZA MC ASS
029300090923     C     TSMMCR        MULT(h)   DEPITR        N113                           *MC X TARIFFA
029400011022     C                   ADD       N113          TECCMR                         *COMPETENZA MC REALI
029500011022     C                   UPDATE    TETEC000
029600000000E   1C                   ENDIF
029700000000     C*
029800000000     C                   ENDSR
029900000000     C*------------------------------------------------------------------------*
030000000000     C* NEWLIN - OPERAZIONI PER NUOVA LINEA
030100000000     C*------------------------------------------------------------------------*
030200000000     C     NEWLIN        BEGSR
030300000000     C*
030400000000     C* MEMORIZZA LA NUOVA LINEA IN UN DEPOSITO
030500000000     C                   Z-ADD     TSMTFP        DEPTFP                         *FILIALE PARTENZA
030600000000     C                   Z-ADD     TSMTFF        DEPTFF                         *FILIALE FINALE
030700000000     C*
030800000000     C* REPERISCE ALCUNI DATI DEL TERMINAL PARTENZA
030900000000     C                   Z-ADD     1             I
031000000000     C                   SETOFF                                           28
031100000000     C     DEPTFP        LOOKUP    ORG(I)                                 28
031200000000IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
031300000000     C                   MOVEL     ODIT(I)       DEPDI1                         *DITTA xxx
031400000000X   1C                   ELSE                                                   *NON TROVATO
031500000000IF  2C     DEPTFP        IFLT      200
031600000000     C     DEPTFP        ORGT      300
031700000000     C     DEPTFP        ANDLT     400
031800000000     C                   MOVEL     'BAR'         DEPDI1                         *DITTA BAR
031900000000E   2C                   ENDIF
032000000000IF  2C     DEPTFP        IFGT      200
032100000000     C     DEPTFP        ANDLT     300
032200000000     C     DEPTFP        ORGT      400
032300000000     C                   MOVEL     'SDI'         DEPDI1                         *DITTA SDI
032400000000E   2C                   ENDIF
032500000000E   1C                   ENDIF
032600000000     C*
032700000000     C* CALCOLA IL CODICE VOCE DELLA TARIFFA TRAINI LINEA DA UTILIZZARE
032800001213IF  1C     TSMDAT        IFGE      200101                                       *DAL  AL 01/2001
032900001213     C                   Z-ADD     212           WVOC                           *VOCE UNICA X FUSION
033000001213X   1C                   ELSE                                                   *PRIMA DEL 01/2001
033100000000IF  2C     DEPDI1        IFEQ      'BAR'                                        *TERM PARTENZA BAR
033200000000     C                   Z-ADD     210           WVOC                           *VOCE BAR
033300000000X   2C                   ELSE                                                   *TERM PARTENZA SDI
033400000000     C                   Z-ADD     211           WVOC                           *VOCE SDI
033500000000E   2C                   ENDIF
033600000000E   1C                   ENDIF
033700000000     C                   Z-ADD     WVOC          KVDVOC
033800000000     C*
033900000000     C* REPERISCE IL PROGRESSIVO CORRENTE DELLA TARIFFA TRAINI LINEA
034000000000     C                   Z-ADD     *ZEROS        WPRG
034100140114     C     KE2EVD        SETLL     ECEVD06L
034200140114     C     KE2EVD        READE     ECEVD06L                               98
034300000000DO  1C     *IN98         DOWEQ     *OFF
034400000000IF  2C     EVDATB        IFEQ      *BLANKS                                      *TARIFFA VALIDA
034500000000IF  3C     EVDTTV        IFEQ      'F'                                          *TARIFFA DI FILIALE
034600000000IF  4C     TSMDTN        IFGE      EVDDDT                                       *IN DECORRENZA
034700000000     C     TSMDTN        ANDLE     EVDDST
034800000000     C                   Z-ADD     EVDPRG        WPRG                           *PROGRESSIVO CORRENT
034900000000     C                   LEAVE
035000000000E   4C                   ENDIF
035100000000E   3C                   ENDIF
035200000000E   2C                   ENDIF
035300140114     C     KE2EVD        READE     ECEVD06L                               98
035400000000E   1C                   ENDDO
035500000000     C*
035600000000     C* 1) REPERISCE LA COMPETENZA TRAINI DELLA NUOVA LINEA-LINEA
035700000000     C                   Z-ADD     DEPTFP        WFT1                           *FILIALE PARTENZA
035800000000     C                   Z-ADD     DEPTFF        WFT2                           *FILIALE ARRIVO
035900000000     C                   EXSR      REPITR
036000000000IF  1C     $ITR          IFEQ      'N'                                          *NON TROVATA
036100000000     C*
036200000000     C* 2) REPERISCE LA COMPETENZA TRAINI DELLA NUOVA LINEA-TERMINAL
036300000000     C*---
036400000000     C* REPERISCE IL TERMINAL ARRIVO DELLA FILIALE DI ARRIVO
036500000000     C*---
036600000000     C                   CLEAR                   DSLV55
036700000000     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
036800000000     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
036900000000     C                   Z-ADD     DEPTFF        D55LIN                         *LINEA ARRIVO
037000000000     C                   Z-ADD     TSMDTN        D55DRF                         *DATA RIFERIMENTO
037100000000     C                   Z-ADD     DEPTFP        D55LNP                         *LINEA PARTENZA
037200000000     C                   CALL      'FNLV55R'
037300000000     C                   PARM                    DSLV55
037400000000IF  2C     D55ERR        IFEQ      *BLANKS                                      *NO ERRORI
037500000000     C                   Z-ADD     D55TFA        WTFA                           *TERMINAL ARRIVO
037600000000X   2C                   ELSE                                                   *SI ERRORI
037700000000     C                   Z-ADD     DEPTFF        WTFA                           *FILIALE ARRIVO
037800000000E   2C                   ENDIF
037900000000     C                   Z-ADD     DEPTFP        WFT1                           *FILIALE PARTENZA
038000000000     C                   Z-ADD     WTFA          WFT2                           *TERMINAL ARRIVO
038100000000     C                   EXSR      REPITR
038200000000IF  2C     $ITR          IFEQ      'N'                                          *NON TROVATA
038300000000     C*
038400000000     C* 3) REPERISCE LA COMPETENZA TRAINI DEL NUOVO TERMINAL-LINEA
038500000000     C*---
038600000000     C* REPERISCE IL TERMINAL PARTENZA DELLA FILIALE DI PARTENZA
038700000000     C*---
038800000000     C                   CLEAR                   DSLV55
038900000000     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
039000000000     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
039100000000     C                   Z-ADD     DEPTFP        D55LIN                         *LINEA PARTENZA
039200000000     C                   Z-ADD     TSMDTN        D55DRF                         *DATA RIFERIMENTO
039300000000     C                   CALL      'FNLV55R'
039400000000     C                   PARM                    DSLV55
039500000000IF  3C     D55ERR        IFEQ      *BLANKS                                      *NO ERRORI
039600000000     C                   Z-ADD     D55TFP        WTFP                           *TERMINAL PARTENZA
039700000000X   3C                   ELSE                                                   *SI ERRORI
039800000000     C                   Z-ADD     DEPTFP        WTFP                           *FILIALE PARTENZA
039900000000E   3C                   ENDIF
040000000000     C                   Z-ADD     WTFP          WFT1                           *TERMINAL PARTENZA
040100000000     C                   Z-ADD     DEPTFF        WFT2                           *FILIALE ARRIVO
040200000000     C                   EXSR      REPITR
040300000000IF  3C     $ITR          IFEQ      'N'                                          *NON TROVATA
040400000000     C*
040500000000     C* 4) REPERISCE LA COMPETENZA TRAINI DEL NUOVO TERMINAL-TERMINAL
040600000000     C                   Z-ADD     WTFP          WFT1                           *TERMINAL PARTENZA
040700000000     C                   Z-ADD     WTFA          WFT2                           *TERMINAL ARRIVO
040800000000     C                   EXSR      REPITR
040900000000E   3C                   ENDIF
041000000000E   2C                   ENDIF
041100000000E   1C                   ENDIF
041200000000     C*
041300000000     C                   ENDSR
041400000000     C*------------------------------------------------------------------------*
041500000000     C* REPITR - REPERISCE COMPETENZA TRAINI DI UN LINEA
041600000000     C*------------------------------------------------------------------------*
041700000000     C     REPITR        BEGSR
041800000000     C*
041900000000     C* REIMPOSTA LE VARIABILI DI LAVORO
042000000000     C                   MOVEL     'N'           $ITR                           *FLAG TROVATO
042100090923     C                   Z-ADD     *ZEROS        DEPITR           13 5          *IMPORTO TN
042200000000     C*
042300000000     C* LEGGE LA TARIFFA FILIALE DELLA TN IN DECORRENZA ALLA DATA TRAINO
042400000000     C                   Z-ADD     WVOC          KVDVOC                         *VOCE
042500000000     C                   Z-ADD     WPRG          KVDPRG                         *PROGRESSIVO
042600000000     C                   MOVEL     'F'           KVDTTV                         *TARIFFA FILIALE
042700000000     C                   Z-ADD     WFT1          KVDFT1                         *PARTENZA
042800000000     C                   Z-ADD     WFT2          KVDFT2                         *ARRIVO
042900140114     C     KEYEVD        SETLL     ECEVD06L
043000140114     C     KEYEVD        READE     ECEVD06L                               98
043100000000DO  1C     *IN98         DOWEQ     *OFF
043200000000IF  2C     EVDATB        IFEQ      *BLANKS                                      *TARIFFA VALIDA
043300000000     C     TSMDTN        ANDGE     EVDDDT                                       *IN DECORRENZA
043400000000     C     TSMDTN        ANDLE     EVDDST
043500011022     C*---               Z-ADD     EVDITR        DEPITR                         *TN PER MC
043600011022     C*
043700011022     C* SE LA DIVISA DI OUT DEL C/E TRAINI E' DIVERSA DELLA DIVISA DELLE ANAGRAFICHE DEL C/E,
043800011022     C* CONVERTE GLI IMPORTI NELLA DIVISTA DI OUTPUT DEL C/E TRAINI -SECONDO LA DATA CORRENTE-
043900011022     C                   EXSR      CNVEVD
044000000000     C                   MOVEL     'S'           $ITR                           *FLAG TROVATO
044100000000     C                   LEAVE
044200000000E   2C                   ENDIF
044300140114     C     KEYEVD        READE     ECEVD06L                               98
044400000000E   1C                   ENDDO
044500000000     C*
044600000000     C                   ENDSR
044700011022     C*------------------------------------------------------------------------*
044800011022     C* CNVEVD - CONVERTE IMPORTI DI ECEVD
044900011022     C*------------------------------------------------------------------------*
045000011022     C     CNVEVD        BEGSR
045100011022     C*
045200011022IF  1C     $DIVSTE       IFNE      $DIVAC                                       *DIVISE <>
045300011022     C                   MOVEL     $DIVAC        WDVI              3            *DIVISA  INPUT
045400011022     C                   Z-ADD     EVDITR        WIMI             30 9          *IMPORTO INPUT
045500011022     C                   EXSR      CNVEUR
045600011022     C                   Z-ADD     WIMO          DEPITR                         *TN PER MC
045700011022X   1C                   ELSE                                                   *DIVISE =
045800011022     C                   Z-ADD     EVDITR        DEPITR                         *TN PER MC
045900011022E   1C                   ENDIF
046000011022     C*
046100011022     C                   ENDSR
046200011022     C*------------------------------------------------------------------------*
046300011022     C* CONVERTE I CAMPI NELLA DIVISA DI OUTPUT DEL C/ECONOMICO TRAINI
046400011022     C*------------------------------------------------------------------------*
046500011022     C     CNVEUR        BEGSR
046600011022     C*
046700011022     C* AZZERA VARIABILI DI LAVORO
046800011022     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO OUTPUT
046900011022     C                   MOVEL     *BLANKS       NDEC              1            *N° DEC DI OUTPUT
047000011022     C*
047100011022     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT
047200011022     C                   IF        $DIVSTE = 'ITL'
047300011022     C                   EVAL      NDEC = '0'
047400011022     C                   ELSE
047500011022     C                   EVAL      NDEC = '3'
047600011022     C                   ENDIF
047700011022     C*
047800011022IF  1C     WIMI          IFNE      *ZEROS
047900011022     C                   CLEAR                   YEUR
048000011022     C                   MOVEL     WDVI          YECDVI                         *DIVISA INPUT
048100011022     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO INPUT
048200011022     C                   MOVEL     $DIVSTE       YECDVO                         *DIVISA OUTPUT
048300011022     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
048400011022     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
048500011022     C                   CALL      'YEURCO'
048600011022     C                   PARM                    YEUR
048700011022IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
048800011022     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
048900011022X   2C                   ELSE                                                   *SI ERRORI
049000011022     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
049100011022E   2C                   ENDIF
049200011022E   1C                   ENDIF
049300011022     C*
049400011022     C                   ENDSR
049500000000     C*------------------------------------------------------------------------*
049600000000     C* CANTEC - CANCELLA IL WORKFILE
049700000000     C*------------------------------------------------------------------------*
049800000000     C     CANTEC        BEGSR
049900000000     C*
050000000000     C* CANCELLA CIO'CHE SI PUO' RICREARE DALLA GRIGLIA FINO ALLA DATA RICHIESTA
050100011022     C                   Z-ADD     WDPTST        KECDTN                         *DATA ULTIMA PULIZIA
050200011022     C     KE2TEC        SETLL     TETEC01L
050300011022     C                   READ      TETEC01L                               99
050400000000DO  1C     *IN99         DOWEQ     *OFF
050500000000IF  2C     TECDTN        IFGE      WDPTST                                       > DATI IN GRIGLIA
050600000000     C     TECDTN        ANDLE     PARDAF                                       < DATA FINALE CHIEST
050700011022     C                   DELETE    TETEC000
050800000000E   2C                   ENDIF
050900011022     C                   READ      TETEC01L                               99
051000000000E   1C                   ENDDO
051100000000     C*
051200000000     C                   ENDSR
051300000000     C*------------------------------------------------------------------------*
051400000000     C* CARTAB - CARICA LE TABELLE OCCORRENTI
051500000000     C*------------------------------------------------------------------------*
051600000000     C     CARTAB        BEGSR
051700000000     C*--------------------
051800000000     C* TIPI TRAINO
051900000000     C*--------------------
052000000000     C                   Z-ADD     *ZEROS        I                 5 0
052100020906     C                   Z-ADD     1             KTBKUT
052200000000     C                   MOVEL     'TV'          KTBCOD
052300000000     C     KEYTAB        SETLL     TABEL00F
052400000000     C     KEYTAB        READE     TABEL00F                               99
052500000000DO  1C     *IN99         DOWEQ     *OFF
052600000000IF  2C     TBLFLG        IFEQ      *BLANKS
052700000000     C                   MOVEL     TBLUNI        DSTV
052800000000IF  3C     §TVUTC        IFEQ      'S'                                          *DA UTILIZZARE
052900000000     C                   ADD       1             I
053000000000     C                   MOVEL     TBLKEY        TTR(I)                         *TIPO TRAINO
053100000000E   3C                   ENDIF
053200000000E   2C                   ENDIF
053300000000     C     KEYTAB        READE     TABEL00F                               99
053400000000E   1C                   ENDDO
053500000000     C*--------------------
053600000000     C* FILIALI
053700000000     C*--------------------
053800000000     C                   Z-ADD     *ZEROS        I                 5 0
053900000000     C                   READ      AZORG01L                               99
054000000000DO  1C     *IN99         DOWEQ     *OFF
054100000000IF  2C     ORGFVA        IFEQ      *BLANKS
054200000000     C                   ADD       1             I
054300000000     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
054400000000     C                   MOVEL     ORGDIT        ODIT(I)                        *DITTA
054500000000E   2C                   ENDIF
054600000000     C                   READ      AZORG01L                               99
054700000000E   1C                   ENDDO
054800011022     C*--------------------
054900011022     C* DIVISE UTILI
055000011022     C*--------------------
055100011022     C* ANAGRAFICHE (INP)
055200011022     C                   CLEAR                   DGEDAC
055300011022     C                   MOVEL(P)  'GED'         KBECOD
055400011022     C                   MOVEL(P)  'AC'          KBEKE1
055500011022     C     KEYTBE        CHAIN     TNTBE01L
055600011022IF  1C                   IF        %FOUND(TNTBE01L)
055700011022     C                   MOVEL     TBEUNI        DGEDAC
055800011022E   1C                   ENDIF
055900011022IF  1C                   IF        §GEDACDAT >= DATCOR                          *D.RIF su D.LANCIO
056000011022     C                   EVAL      $DIVAC = §GEDACDIP                           *DIVISA PRIMA (ITL)
056100011022X   1C                   ELSE
056200011022     C                   EVAL      $DIVAC = §GEDACDID                           *DIVISA DOPO (EUR)
056300011022E   1C                   ENDIF
056400011022     C* C/ECONOMICO (OUT)
056500011022     C                   CLEAR                   DGEDST
056600011022     C                   MOVEL(P)  'GED'         KBECOD
056700011022     C                   MOVEL(P)  'ST'          KBEKE1
056800011022     C     KEYTBE        CHAIN     TNTBE01L
056900011022IF  1C                   IF        %FOUND(TNTBE01L)
057000011022     C                   MOVEL     TBEUNI        DGEDST
057100011022E   1C                   ENDIF
057200011022IF  1C                   IF        §GEDSTDATE >= DATCOR                         *D.RIF su D.LANCIO
057300011022     C                   EVAL      $DIVSTE = §GEDSTDIPE                         *DIVISA PRIMA (ITL)
057400011022X   1C                   ELSE
057500011022     C                   EVAL      $DIVSTE = §GEDSTDIDE                         *DIVISA DOPO (EUR)
057600011022E   1C                   ENDIF
057700011022     C*
057800000000     C                   ENDSR
057900000000     C*------------------------------------------------------------------------*
058000000000     C* FINSR - OPERAZIONI FINALI
058100000000     C*------------------------------------------------------------------------*
058200000000     C     FINSR         BEGSR
058300000000     C*
058400000000     C* CHIUDE I PROGRAMMA DI UTILITA' RICHIAMATI
058500000000     C                   CLEAR                   DSLV55                         *RECUPERA TERMINAL
058600000000     C                   MOVEL     'C'           D55TLA
058700000000     C                   CALL      'FNLV55R'
058800000000     C                   PARM                    DSLV55
058900000000     C*
059000000000     C                   ENDSR
059100000000     C*------------------------------------------------------------------------*
059200000000     C* *INZSR - IMPOSTAZIONI INIZIALI
059300000000     C*------------------------------------------------------------------------*
059400000000     C     *INZSR        BEGSR
059500000000     C*--------------------
059600000000     C* RICEVIMENTO PARAMETRI
059700000000     C*--------------------
059800000000     C     *ENTRY        PLIST
059900000000     C                   PARM                    KPJBA
060000000000     C                   MOVEL     KPJBU         PARAM
060100000000     C*--------------------
060200000000     C* VARIABILI RIFERITE AL DATA BASE
060300000000     C*--------------------
060400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
060500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
060600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
060700011022     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
060800011022     C     *LIKE         DEFINE    TBEKE1        KBEKE1
060900000000     C     *LIKE         DEFINE    EVDVOC        KVDVOC                         *ECEVD00F
061000000000     C     *LIKE         DEFINE    EVDPRG        KVDPRG
061100000000     C     *LIKE         DEFINE    EVDTTV        KVDTTV
061200000000     C     *LIKE         DEFINE    EVDFT1        KVDFT1
061300000000     C     *LIKE         DEFINE    EVDFT2        KVDFT2
061400000000     C     *LIKE         DEFINE    TSTDTN        KSTDTN                         *TNTST00F
061500000000     C     *LIKE         DEFINE    TSMTRN        KSMTRN                         *TNTSM00F
061600000000     C     *LIKE         DEFINE    TSMDTN        KSMDTN
061700011022     C     *LIKE         DEFINE    TECDTN        KECDTN                         *TETEC00F
061800011022     C     *LIKE         DEFINE    TECTRN        KECTRN
061900011022     C     *LIKE         DEFINE    TECTFP        KECTFP
062000011022     C     *LIKE         DEFINE    TECTFA        KECTFA
062100000000     C     *LIKE         DEFINE    TSMTFP        DEPTFP                         *DEPOSITI
062200000000     C     *LIKE         DEFINE    TSMTFF        DEPTFF
062300090907     C*    *LIKE         DEFINE    EVDITR        DEPITR
062400000000     C     *LIKE         DEFINE    ORGDIT        DEPDI1
062500000000     C     *LIKE         DEFINE    EVDVOC        WVOC                           *DI LAVORO
062600000000     C     *LIKE         DEFINE    EVDPRG        WPRG
062700000000     C     *LIKE         DEFINE    EVDFT1        WFT1
062800000000     C     *LIKE         DEFINE    EVDFT2        WFT2
062900000000     C     *LIKE         DEFINE    D55TFP        WTFP
063000000000     C     *LIKE         DEFINE    D55TFA        WTFA
063100000000     C*--------------------
063200000000     C* CHIAVI DI LETTURA
063300000000     C*--------------------
063400000000     C* LETTURA TABEL00F - PARZIALE
063500000000     C     KEYTAB        KLIST
063600000000     C                   KFLD                    KTBKUT
063700000000     C                   KFLD                    KTBCOD
063800011022     C* LETTURA TNTBE01L - PARZIALE
063900011022     C     KEYTBE        KLIST
064000011022     C                   KFLD                    KBECOD
064100011022     C                   KFLD                    KBEKE1
064200140114     C* LETTURA ECEVD06L - PARZIALE
064300000000     C     KEYEVD        KLIST
064400000000     C                   KFLD                    KVDVOC
064500000000     C                   KFLD                    KVDPRG
064600000000     C                   KFLD                    KVDTTV
064700000000     C                   KFLD                    KVDFT1
064800000000     C                   KFLD                    KVDFT2
064900140114     C* LETTURA ECEVD06L - PARZIALE
065000000000     C     KE2EVD        KLIST
065100000000     C                   KFLD                    KVDVOC
065200000000     C* LETTURA TNTST03L - COMPLETA
065300000000     C     KEYTST        KLIST
065400000000     C                   KFLD                    KSTDTN                         *DATA TRAINO
065500000000     C* LETTURA TNTSM05L - PARZIALE
065600000000     C     KEYTSM        KLIST
065700000000     C                   KFLD                    KSMTRN                         *NUMERO TRAINO
065800000000     C                   KFLD                    KSMDTN                         *DATA TRAINO
065900011022     C* LETTURA TETEC01L - COMPLETA
066000000000     C     KEYTEC        KLIST
066100011022     C                   KFLD                    KECDTN                         *DATA TRAINO
066200011022     C                   KFLD                    KECTRN                         *NUMERO TRAINO
066300011022     C                   KFLD                    KECTFP                         *FILIALE DI PARTENZA
066400011022     C                   KFLD                    KECTFA                         *FILIALE DI ARRIVO
066500011022     C* LETTURA TETEC01L - PARZIALE
066600000000     C     KE2TEC        KLIST
066700011022     C                   KFLD                    KECDTN                         *DATA TRAINO
066800000000     C*--------------------
066900000000     C* VARIABILI NON RIFERITE AL DATA BASE
067000000000     C*--------------------
067100011022     C                   Z-ADD     *ZEROS        N113             11 3          *NUMERICO 11,3
067200000000     C*--------------------
067300000000     C* VARIABILI DI CONTROLLO
067400000000     C*--------------------
067500000000     C                   MOVEL     'N'           $FINE             1            *FINE PGM
067600000000     C                   MOVEL     'N'           $FTSM             1            *FINE CICLO -TNTSM-
067700000000     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' TRAINO
067800000000     C                   MOVEL     'S'           $ITR              1            *FLAG TROVATO
067900011022     c*
068000011022     c* reperimento data corrente
068100011022     C                   TIME                    n14              14 0          *ora (6)+ data (8)
068200011022     C                   MOVE      n14           n8                8 0          *data (8) in g/m/a
068300011022     C                   Z-ADD     n8            g08dat
068400011022     C                   Z-ADD     *zeros        g08inv
068500011022     C                   MOVEL     '0'           g08err
068600011022     c                   CALL      'XSRDA8'
068700011022     c                   PARM                    wlbda8
068800011022     C                   Z-ADD     g08inv        datcor            8 0          *Data corrente a/m/g
068900000000     C*
069000000000     C* CARICA LE TAELLE OCCORRENTI
069100000000     C                   EXSR      CARTAB
069200000000     C*
069300000000     C* REPERISCE LA DATA TRAINO PIU' RECENTE PULITA
069400000000     C     *DTAARA       DEFINE                  TNTL58            8 0
069500000000     C                   IN        TNTL58
069600000000     C                   MOVEL     TNTL58        WDPTST            8 0
069700000000IF  1C     PARDAI        IFGT      WDPTST
069800000000     C                   Z-ADD     PARDAI        WDPTST
069900000000E   1C                   ENDIF
070000000000     C*
070100000000     C* CANCELLA IL WORKFILE
070200000000     C                   EXSR      CANTEC
070300000000     C*
070400000000     C                   ENDSR
