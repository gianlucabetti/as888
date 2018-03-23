000100940304     H DECEDIT('0,') DATEDIT(*DMY.)
000200940304?     *--------------------------------------------------------------*
000300950203      * TNSC61R                                                      *
000400940304      *       *-----------------------------------------*            *
000500940304      *           STAMPA/PULIZIA CONTRASSEGNI DI SEDE                *
000600940304      *                CREAZIONE ARCHIVI DI WORK                     *
000700940304      *       *-----------------------------------------*            *
000800940304      *                                                              *
000900940304?     *--------------------------------------------------------------*
001000950203      * 70 -Lettura di  TNCSV                                        *
001100950203      * 80 -Lettura di  TNCSB                                        *
001200950203      * 81 -Chain su CNACO/TNCSA                                     *
001300950203      * 82 -Lettura FNLBL                                            *
001400940307?     *--------------------------------------------------------------*
001500950203     FFNLBL01L  IF   E           K DISK
001600940304?     *----------
001700950203     FFNLBL02L  IF   E           K DISK
001800950203     F                                     RENAME(FNLBL000:FNLBL2)
001900940308?     *----------
002000950203     FTNCSA01L  IF   E           K DISK
002100940304?     *----------
002200950203     FTNCSB05L  IF   E           K DISK
002300940304?     *----------
002400950203     FTNCSV01L  IF   E           K DISK
002500940304?     *----------
002600950203     FTNCPB00F  O  A E             DISK
002700940304?     *----------
002800950203     FTNCPV00F  O  A E             DISK
002900940304?     *--------------------------------------------------------------*
003000940304?     *  DS                                                          *
003100940304?     *--------------------------------------------------------------*
003200940304     D KPJBA         E DS
003300940304      *
003400950203     D TNSC60        E DS                  EXTNAME(TNSC60DS)
003500980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003600980127     D CNACO         E DS                  EXTNAME(CNACO00F)
003700980127     D CNIND         E DS                  EXTNAME(CNIND00F)
003800980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003900980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
004000940304      *
004100940307     D PARUT         E DS                  EXTNAME(UT§DSE0F)
004200940304      *
004300950203     D TNCSB         E DS                  EXTNAME(TNCSB00F)
004400940304      *
004500950203     D TNCPB         E DS                  EXTNAME(TNCPB00F)
004600940304      *
004700950203     D TNCSV         E DS                  EXTNAME(TNCSV00F)
004800940304      *
004900950203     D TNCPV         E DS                  EXTNAME(TNCPV00F)
005000940304      *---------------------------------------------------------------*
005100940304      *   C I C L O      P R I N C I P A L E                          *
005200940304      *---------------------------------------------------------------*
005300940308     C*  Posizionamento iniziale x data
005400950623     C     KEYCSB        SETLL     TNCSB05L
005500950623     C                   READ      TNCSB05L                               80
005600950623     C                   Z-ADD     CSBMGS        DATDSP            8 0
005700950623     C                   MOVEL     CSBAAS        DATDSP
005800940308     C*
005900950203     C     DATDSP        DOWLE     D60SDA
006000950203     C     DATDSP        ANDGE     D60SDD
006100940308     C     *IN80         ANDEQ     '0'
006200031024     c* memorizzo solo se non archiviato precedentemente
006300031024     C     CSBFIN        ifne      'S'
006400950623     C*
006500950623     C     CSBCDI        IFNE      SAVCDI
006600950623     C                   MOVEL     CSBCDI        SAVCDI
006700950623     C                   MOVE      CSBCDI        APPOG             4 0
006800950623     C*  Se codice cliente no generico e record buono chain su CNACO
006900950623     C     APPOG         IFNE      9999
007000950623     C     APPOG         ANDNE     8888
007100950623     C                   MOVEL     *BLANKS       ACORAG
007200980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
007300980127     C                   CLEAR                   TIBS69
007400980127     C                   CLEAR                   CNACO
007500980127     C                   CLEAR                   CNIND
007600980127     C                   CLEAR                   CNCLP
007700980127     C                   CLEAR                   FNCLS
007800980127     C                   MOVEL     ' '           I69TLA
007900980127     C                   MOVEL     KNSIF         I69SIF
008000980127     C                   Z-ADD     CSBCDI        I69KAC
008100980127     C                   CALL      'TIBS69R'
008200980127     C                   PARM                    TIBS69
008300980127     C                   PARM                    CNACO
008400980127     C                   PARM                    CNIND
008500980127     C                   PARM                    CNCLP
008600980127     C                   PARM                    FNCLS
008700980127     C     O69ERR        IFEQ      *BLANK
008800950623     C                   MOVEL     ACORAG        CPBRSM
008900950623     C                   END
009000980127     C                   END
009100950623     C                   END
009200940308     C*  Controllo se contrassegno esaminato è da cancellare
009300040517     C     CSBFTR        IFNE      ' '
009400040517     C     CSBDTR        ORNE      0
009500040517     C     CSBSTA        OREQ      9
009600940308     C     CSBDDP        IFNE      0
009700960918     C     CSBSTA        ANDEQ     0
009800960923     C     CSBDDC        ANDGT     0
009900940308     C     CSBSTA        OREQ      9
010000940308     C*  Se erimento dati anagrafici
010100940308     C                   EXSR      REPRSM
010200940308     C*  Loop x ricerca bolla mamma
010300940308     C                   EXSR      REDLBL
010400940308     C*  Loop su archivio variazioni
010500940308     C                   EXSR      GESCPV
010600940308     C                   END
010700940513     C                   END
010800031024     C                   END
010900940308     C*  Nuova lettura
011000950203     C                   READ      TNCSB05L                               80
011100950203     C                   Z-ADD     CSBMGS        DATDSP
011200950203     C                   MOVEL     CSBAAS        DATDSP
011300940308     C                   END
011400980127     C*
011500980127     C                   CLEAR                   TIBS69
011600980127     C                   MOVEL     'C'           I69TLA
011700980127     C                   CALL      'TIBS69R'
011800980127     C                   PARM                    TIBS69
011900980127     C                   PARM                    CNACO
012000980127     C                   PARM                    CNIND
012100980127     C                   PARM                    CNCLP
012200980127     C                   PARM                    FNCLS
012300940308     C*
012400940304     C* Fine pgm.
012500940304     C                   SETON                                        LR
012600940304      *---------------------------------------------------------------*
012700940304      *   REPRSM: Reperimanto dati anagrafici                         *
012800940304      *---------------------------------------------------------------*
012900940304     C     REPRSM        BEGSR
013000940304     C*
013100950203     C*  Imposto campi TNCPB da TNCSB
013200950203     C                   MOVEL     TNCSB         TNCPB
013300940304     C*  Imposto chiave bolla
013400940304     C                   Z-ADD     CSBLNP        CSWLNP
013500940304     C                   Z-ADD     CSBAAS        CSWAAS
013600940310     C                   Z-ADD     CSBNRS        CSWNRS
013700940310     C                   Z-ADD     CSBNSP        CSWNSP
013800950203     C*  Se codice cliente generico reperisco dati da TNCSA
013900940304     C     APPOG         IFEQ      9999
014000940304     C     APPOG         OREQ      8888
014100940308     C                   MOVEL     *BLANKS       CSARSD
014200950203     C     KEYCSA        CHAIN     TNCSA01L                           81
014300940304     C                   MOVEL     CSARSD        CPBRSM
014400940304     C                   END
014500940304     C*
014600940304     C                   ENDSR
014700940304      *---------------------------------------------------------------*
014800940304      *   REDLBL: Reperimanto dati bolla mamma                        *
014900940304      *---------------------------------------------------------------*
015000940304     C     REDLBL        BEGSR
015100940304     C*
015200940308     C*  Azzero campi di riferimento x bolla legata
015300940308     C                   Z-ADD     0             CPBLLP
015400940308     C                   Z-ADD     0             CPBLAA
015500940308     C                   Z-ADD     0             CPBLNS
015600940308     C                   Z-ADD     0             CPBLSP
015700940304     C*  Imposto chiave x posizionamento su fllbl
015800940304     C                   Z-ADD     CSBLNP        LBWLNP
015900940304     C                   Z-ADD     CSBAAS        LBWAAS
016000940311     C                   Z-ADD     CSBNRS        LBWNRS
016100940311     C                   Z-ADD     CSBNSP        LBWNSP
016200940308     C*  Controllo se la bolla esaminata è una mamma
016300950203     C     KEYLBL        CHAIN     FNLBL01L                           82
016400940304     C     *IN82         IFEQ      '0'
016500940308     C*  Imposto campi bolla Legata
016600940308     C                   Z-ADD     LBLLPP        CPBLLP
016700940308     C                   Z-ADD     LBLAAP        CPBLAA
016800940308     C                   Z-ADD     LBLNRP        CPBLNS
016900940308     C                   Z-ADD     LBLNSP        CPBLSP
017000940308     C                   END
017100940308     C*  Controllo se la bolla esaminata è una figlia
017200940308     C     *IN82         IFEQ      '1'
017300940308     C     CSBSTA        OREQ      9
017400950203     C     KEYLBL        CHAIN     FNLBL02L                           82
017500940308     C     *IN82         IFEQ      '0'
017600940308     C*  Imposto campi bolla Legata
017700940308     C                   Z-ADD     LBLLPN        CPBLLP
017800940308     C                   Z-ADD     LBLAAN        CPBLAA
017900940308     C                   Z-ADD     LBLNRN        CPBLNS
018000940308     C                   Z-ADD     LBLNSN        CPBLSP
018100940308     C                   END
018200940308     C                   END
018300940304     C*----------------------------------------------------*
018400950203     C                   WRITE     TNCPB000
018500940304     C*----------------------------------------------------*
018600940304     C                   ENDSR
018700940304      *---------------------------------------------------------------*
018800940304      *   GESCPV: Gestione variazioni bolla                           *
018900940304      *---------------------------------------------------------------*
019000940304     C     GESCPV        BEGSR
019100940304     C*
019200940304     C* Posizionamento iniziale
019300950203     C     KEYCSV        CHAIN     TNCSV01L                           70
019400940307     C     *IN70         DOWEQ     '0'
019500950203     C*  Imposto campi TNCPV da TNCSV
019600950203     C                   MOVEL     TNCSV         TNCPV
019700950203     C                   WRITE     TNCPV000
019800940304     C*  Lettura successiva
019900950203     C     KEYCSV        READE     TNCSV01L                               70
020000940304     C                   END
020100940304     C*----------------------------------------------------*
020200940304     C                   ENDSR
020300940307      *----------------------------------------------------*
020400940307      *   *INZSR: Operazioni iniziali                      *
020500940307      *----------------------------------------------------*
020600940307     C     *INZSR        BEGSR
020700940307     C*
020800940307     C     *ENTRY        PLIST
020900940307     C                   PARM                    KPJBA
021000950203     C                   MOVEL     KPJBU         TNSC60
021100940307     C*----------------------------------------------------*
021200940307     C* Call a XPARUT
021300940307     C                   Z-ADD     1             CODUT
021400940307     C                   CALL      'X§PARUT'
021500940307     C                   PARM                    PARUT
021600940307     C*----------------------------------------------------*
021700940307     C* Definizione chiavi
021800940307     C     KEYCSB        KLIST
021900950203     C                   KFLD                    CSWAAS
022000950203     C                   KFLD                    CSWMGS
022100940307     C*
022200940307     C     KEYCSA        KLIST
022300940307     C                   KFLD                    CSWAAS
022400940307     C                   KFLD                    CSWLNP
022500940307     C                   KFLD                    CSWNRS
022600940307     C                   KFLD                    CSWNSP
022700940307     C*
022800940307     C     KEYCSV        KLIST
022900950203     C                   KFLD                    CSWAAS
023000950203     C                   KFLD                    CSWLNP
023100940307     C                   KFLD                    CSWNRS
023200940307     C                   KFLD                    CSWNSP
023300940307     C*
023400940307     C     KEYLBL        KLIST
023500940307     C                   KFLD                    LBWAAS
023600940307     C                   KFLD                    LBWLNP
023700940307     C                   KFLD                    LBWNRS
023800940307     C                   KFLD                    LBWNSP
023900940307     C*----------------------------------------------------*
024000940307     C* Definizione chiavi
024100950203     C     *LIKE         DEFINE    CSBMGS        CSWMGS
024200940307     C     *LIKE         DEFINE    CSBAAS        CSWAAS
024300940307     C     *LIKE         DEFINE    CSBLNP        CSWLNP
024400940307     C     *LIKE         DEFINE    CSBNRS        CSWNRS
024500940307     C     *LIKE         DEFINE    CSBNSP        CSWNSP
024600940307     C     *LIKE         DEFINE    LBLAAN        LBWAAS
024700940307     C     *LIKE         DEFINE    LBLLPN        LBWLNP
024800940307     C     *LIKE         DEFINE    LBLNRN        LBWNRS
024900940307     C     *LIKE         DEFINE    LBLNSN        LBWNSP
025000940307     C     *LIKE         DEFINE    CSBCDI        SAVCDI
025100940307     C*----------------------------------------------------*
025200940307     C* Inizilizzazione variabili
025300940307     C                   Z-ADD     0             SAVCDI
025400950203     C                   MOVEL     D60SDD        CSWAAS
025500950203     C                   Z-ADD     D60SDD        CSWMGS
025600950203     C                   Z-ADD     0             DATDSP
025700940307     C*----------------------------------------------------*
025800940307     C                   ENDSR
025900940307     C*----------------------------------------------------*
