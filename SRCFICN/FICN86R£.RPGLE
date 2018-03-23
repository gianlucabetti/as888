000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011030     H* ficn86R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400931020     H*               RICERCA TARIFFE PADRONCINI
000500000000     H*--------------------------------------------------------------*
000600011030     Fficn86D   CF   E             WORKSTN
000700011030     F                                     SFILE(CN86S01:NRR)
000800011030     Ffifgt01L  IF   E           K DISK
000900931020     FTABEL00F  IF   E           K DISK
001000021203     Ffiapd01L  IF   E           K DISK
001100931105     D TPS             S              1    DIM(50)                              TIPO PRESTAZIONE
001200931105     D DSR             S             15    DIM(50)                              DESCR.TIPO PRESTAZIO
001300931105     D SER             S              1    DIM(12)                              COMODO X TIPO PRESTA
001400931020     D*
001500931020     D PARAM           DS
001600940923     D* CODICE PADRONCINO
001700931105     D  PDR                    1      7  0
001800940923     D* FLAG SIMULAZIONE
001900931105     D  VIDSML                 8      8
002000940923     D* TIPO PRESTAZIONE
002100931105     D  TSR                    9      9
002200940923     D* CODICE TARIFFA
002300931105     D  VIDCTR                11     13  0
002400940923     D* PROGRESSIVO TARIFFA
002500931105     D  VIDPRG                14     16  0
002600940923     D* FLGSEL = 'E' ===> NON E' STATA SELEZIONATA NESSUNA TARIFFA
002700940923     D* FLGSEL = ' ' ===>     E' STATA SELEZIONATA UNA TARIFFA
002800931105     D  FLGSEL                17     17
002900931020     D*
003000940923     D                 DS
003100940923     D  AA                     1      4  0
003200940923     D  MM                     5      6  0
003300940923     D  GG                     7      8  0
003400940923     D  DATA                   1      8  0
003500931020     D KPJBA         E DS
003600931020     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003700931020     I/SPACE 3
003800931020     C*****************************************************************
003900931020     C* RIEPILOGO INDICATORI
004000931020     C*****************************************************************
004100931105     C* 01    - IL TIPO PRESTAZIONE PASSATO E' PIENO
004200931021     C* 02    - SONO IN FASE DI SIMULAZIONE CIOE' VIDSML = 'S'
004300931020     C* 10    - USATO PER ESEGUIRE UNA OPERAZIONE SOLO UNA VOLTA
004400931020     C* 20    - LOKUP SU SCHIERE
004500931020     C* 30    - DI COMODO
004600931020     C* 40    - INDICATORE PER MESSAGGIO DI ERRORE
004700931020     C* 87    - PULIZIA SUBFILE
004800931020     C*****************************************************************
004900931020     C     *ENTRY        PLIST
005000931020     C                   PARM                    KPJBA
005100931020     C                   MOVEL     KPJBU         PARAM
005200931020     C                   Z-ADD     1             CODUT
005300931020     C                   CALL      'X§PARUT'
005400931020     C                   PARM                    UT§DSE
005500931020     C                   MOVEL     RAGUT         RSUT             20
005600931020     C*---------------------------------------------------------------*
005700931020     C* ACCESSO TABEL
005800931020     C     KTAB2         KLIST
005900931020     C                   KFLD                    CODUT
006000931020     C                   KFLD                    COD
006001021203     C* ACCESSO fiapd
006002021203     C     Kapd          KLIST
006003021203     C                   KFLD                    apdtip
006004021203     C                   KFLD                    pdr
006005021203     c                   movel     'A'           apdtip
006100011030     C* ACCESSO fifgt
006200931020     C     KFGT          KLIST
006300931105     C                   KFLD                    PDR
006400931020     C                   KFLD                    VIDSML            1
006500931020     C     KFGT2         KLIST
006600931105     C                   KFLD                    PDR
006700931020     C                   KFLD                    VIDSML
006800931020     C                   KFLD                    TSR
006900931021     C*
007000931105     C* DEFINIZIONE CAMPI
007100940923     C     *LIKE         DEFINE    TBLCOD        COD
007200931021     C     *LIKE         DEFINE    FGTTSR        COMTSR
007300931021     C     *LIKE         DEFINE    FGTDDT        COMDDT
007400931021     C     *LIKE         DEFINE    FGTDST        COMDST
007500931021     C     *LIKE         DEFINE    FGTDUV        COMDUV
007600931021     C*
007700931020     C*---------------------------------------------------------------*
007800931020     C** CARICAMENTO TABELLE
007900931020     C                   EXSR      CARTAB
008000931020     C*
008100931105     C** 01 ON  - IL TIPO PRESTAZIONE PASSATO E' PIENO
008200931020     C     TSR           IFNE      ' '
008300931020     C                   SETON                                        01
008400931020     C                   ENDIF
008500931020     C*
008600931021     C** 02 ON  - FLAG SIMULAZIONE = 'S'
008700931021     C     VIDSML        IFNE      ' '
008800931021     C                   SETON                                        02
008900931021     C                   ENDIF
009000931021     C*
009100931020     C** IMPOSTO CAMPI DEL SFLCONTROL
009200931108     C                   MOVEL     PDR           VIDFIL
009300931020     C                   Z-ADD     PDR           VIDPDR
009400931021     C                   MOVEL     *BLANKS       DESPDR
009500931020     C*
009600931020     C** DECODIFICO CODICE PADRONCINO
009700021203     C     kapd          CHAIN     fiapd000                           30
009800940923     C  N30APDATB        IFNE      ' '
009900940923     C                   SETON                                        30
010000940923     C                   ENDIF
010100931020     C   30              GOTO      FINE
010200940923     C                   MOVEL     APDRSC        DESPDR
010300931020     C*
010400931020     C** PULIZIA SUBFILE
010500931020     C                   EXSR      PULSF
010600931020     C** CARICAMENTO SUBFILE
010700931020     C                   EXSR      CARSF
010800931020     C*
010900931020     C** SE NON CARICA NIENTE ESCO DAL PGM
011000931020     C     NRR           IFEQ      0
011100931020     C                   GOTO      FINE
011200931020     C                   ENDIF
011300931020     C*
011400931020     C                   Z-ADD     1             REC
011500011030     C                   WRITE     cn86Z02
011600931020     C     FORCT         TAG
011700011030     C                   EXFMT     cn86C01
011800931020     C*
011900931020     C** CMD12 - FINE LAVORO
012000931021     C** FLGSEL = 'E' ===> NON E' STATA SELEZIONATA NESSUNA TARIFFA
012100931021     C   KL              MOVEL     'E'           FLGSEL
012200931020     C   KL              GOTO      FINE
012300931020     C*
012400931020     C** CONTROLLO SE E' STATA EFFETTUATA ALMENO UNA SCELTA
012500011030     C                   READC     cn86S01                                30
012600931110     C   30              SETON                                        40
012700931020     C*
012800931110     C     *IN30         IFEQ      *OFF
012900931020     C*
013000940923     C     VIDSCE        IFEQ      '1'
013100931110     C** TIPO PRESTAZIONE
013200931020     C                   MOVEL     VIDTSR        TSR
013300931021     C** FLGSEL = ' ' ===> E' STATA SELEZIONATA UNA TARIFFA
013400931021     C                   MOVEL     ' '           FLGSEL
013500931110     C                   ELSE
013600931110     C                   SETON                                        30
013700931110     C                   ENDIF
013800931110     C                   ENDIF
013900931110     C*
014000931110     C* NESSUNA SCELTA EFFETTUATA: RIEMETTO SUBFILE
014100931110     C   30              GOTO      FORCT
014200931020     C*
014300931020     C     FINE          TAG
014400931020     C                   MOVEL     PARAM         KPJBU
014500931020     C*
014600931020     C                   SETON                                        LR
014700931020     C*
014800931020     C*--- CARICAMENTO TABELLE ---------------------------------------*
014900931020     C     CARTAB        BEGSR
015000931105     C****  TIPI PRESTAZIONE E DESCRIZIONI: TPS / DSR  ****
015100931020     C                   Z-ADD     1             X                 3 0
015200931020     C                   MOVEL     '1Z'          COD
015300931020     C     KTAB2         SETLL     TABEL
015400931020     C     KTAB2         READE     TABEL                                  30
015500931020     C*
015600931020     C     *IN30         DOWEQ     *OFF
015700931020     C     TBLFLG        IFEQ      ' '
015800931020     C                   MOVEL     TBLKEY        TPS(X)
015900931020     C                   MOVEL     TBLUNI        DSR(X)
016000931020     C                   ADD       1             X
016100931020     C                   ENDIF
016200931020     C     KTAB2         READE     TABEL                                  30
016300931020     C                   ENDDO
016400931020     C*
016500931020     C                   ENDSR
016600931020     C*
016700931020     C*--- PULIZIA SUBFILE -------------------------------------------*
016800931020     C     PULSF         BEGSR
016900931020     C                   SETON                                        87
017000011030     C                   WRITE     cn86C01
017100931020     C                   SETOFF                                       87
017200931020     C                   ENDSR
017300931020     C*
017400931020     C*--- CARICAMENTO SUBFILE ---------------------------------------*
017500931020     C     CARSF         BEGSR
017600931020     C                   Z-ADD     0             NRR               4 0
017700931020     C*
017800011030     C  N01KFGT          SETLL     fifgt000
017900011030     C   01KFGT2         SETLL     fifgt000
018000931020     C*
018100011030     C  N01KFGT          READE     fifgt000                               30
018200011030     C   01KFGT2         READE     fifgt000                               30
018300931020     C*
018400931020     C     *IN30         DOWEQ     *OFF
018500931020     C     FGTATB        IFEQ      ' '
018600931020     C*
018700931020     C* SALVO IL CODICE TARIFFA SOLO LA PRIMA VOLTA
018800931020     C  N10              Z-ADD     FGTCTR        SAVCTR            3 0
018900931020     C  N10              MOVEL     FGTTSR        SAVTSR            1
019000931020     C  N10              SETON                                        10
019100931020     C*
019200931020     C* VISUALIZZO SOLO L'ULTIMO PROGRESSIVO DI OGNI CODICE TARIFFA
019300931020     C     FGTCTR        IFNE      SAVCTR
019400931020     C     FGTTSR        ORNE      SAVTSR
019500931020     C                   EXSR      SCRIVI
019600931020     C                   Z-ADD     FGTCTR        SAVCTR
019700931020     C                   MOVEL     FGTTSR        SAVTSR
019800931020     C                   ENDIF
019900931020     C*
020000931020     C* CAMPO SELEZIONE
020100931020     C                   MOVEL     ' '           VIDSCE
020200931105     C* TIPO PRESTAZIONE
020300931021     C                   MOVEL     FGTTSR        COMTSR
020400011030     C* CODICE E PROGRESSIVO TARIFFA divisa
020500931020     C                   MOVEL     FGTCTR        VIDCTR
020600931020     C                   MOVEL     FGTPRG        VIDPRG
020700011030     C                   MOVEL     FGTdiv        VIDdiv
020800931020     C* DATA DECORRENZA / SCADENZA / ULTIMA VARIAZIONE
020900931021     C                   MOVEL     FGTDDT        COMDDT
021000931021     C                   MOVEL     FGTDST        COMDST
021100931021     C                   MOVEL     FGTDUV        COMDUV
021200931020     C* DESCRIZIONE TARIFFA
021300931020     C                   MOVEL     FGTDCV        VIDDCV
021400931108     C* TARIFFA PREFERENZIALE
021500931108     C                   MOVEL     FGTFTP        VIDFTP
021600931108     C*
021700931020     C                   ENDIF
021800931020     C*
021900011030     C  N01KFGT          READE     fifgt000                               30
022000011030     C   01KFGT2         READE     fifgt000                               30
022100931020     C*
022200931020     C* SCRIVO ULTIMA TARIFFA LETTA
022300011112     C*  30              EXSR      SCRIVI
022400011112     C                   EXSR      SCRIVI
022500931020     C                   ENDDO
022600931020     C*
022700931020     C                   ENDSR
022800931020     C*
022900931020     C*--- SCRIVO SUBFILE --------------------------------------------*
023000931020     C     SCRIVI        BEGSR
023100931020     C*
023200931105     C* TIPO PRESTAZIONE
023300931020     C                   CLEAR                   SER
023400931020     C                   MOVEA     COMTSR        SER(1)
023500931020     C                   MOVEA     '-'           SER(2)
023600931020     C                   Z-ADD     1             X
023700931020     C     COMTSR        LOOKUP    TPS(X)                                 20
023800931020     C   20              MOVEA     DSR(X)        SER(3)
023900931020     C   20              MOVEA     SER           VIDTSR
024000931020     C*
024100931020     C* GIRO DATA DECORRENZA TARIFFA
024200940923     C                   Z-ADD     COMDDT        DATA
024300940923     C                   Z-ADD     MM            GGMM              4 0
024400940923     C                   MOVEL     GG            GGMM
024500940923     C                   Z-ADD     AA            VIDDDT
024600940923     C                   MOVEL     GGMM          VIDDDT
024700931020     C* GIRO DATA SCADENZA TARIFFA
024800940923     C                   Z-ADD     COMDST        DATA
024900940923     C                   Z-ADD     MM            GGMM
025000940923     C                   MOVEL     GG            GGMM
025100940923     C                   Z-ADD     AA            VIDDST
025200940923     C                   MOVEL     GGMM          VIDDST
025300931020     C* GIRO DATA ULTIMA VARIAZIONE
025400940923     C                   Z-ADD     COMDUV        DATA
025500940923     C                   Z-ADD     MM            GGMM
025600940923     C                   MOVEL     GG            GGMM
025700940923     C                   Z-ADD     AA            VIDDUV
025800940923     C                   MOVEL     GGMM          VIDDUV
025900931020     C*
026000931020     C                   ADD       1             NRR
026100011030     C                   WRITE     cn86S01
026200931020     C*
026300931020     C                   ENDSR
