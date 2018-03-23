000100940308     H DECEDIT('0,') DATEDIT(*DMY.)
000200940308?     *--------------------------------------------------------------*
000300950203      * TNSC63R3                                                     *
000400940308      *       *-----------------------------------------*            *
000500940308      *           STAMPA/PULIZIA CONTRASSEGNI DI SEDE                *
000600940308      *             ELIMINAZIONE RECORD ERRATI                       *
000700940308      *       *-----------------------------------------*            *
000800940308      *                                                              *
000900940308?     *--------------------------------------------------------------*
001000950203     FTNCSV01L  UF   E           K DISK
001100940308     F*--------
001200950203     FTNCSA01L  UF   E           K DISK
001300940308     F*--------
001400950203     FTNCSB03L  IF   E           K DISK
001500940308?     *--------------------------------------------------------------*
001600940308?     *  DS                                                          *
001700940308?     *--------------------------------------------------------------*
001800940308     D KPJBA         E DS
001900940308      *
002000950203     D TNSC60        E DS                  EXTNAME(TNSC60DS)
002100940308      *---------------------------------------------------------------*
002200940308      *   C I C L O      P R I N C I P A L E                          *
002300940308      *---------------------------------------------------------------*
002400940308      *  Cancello record da anagrfico
002500940308     C                   EXSR      DLTCSA
002600940308      *  Cancello record da variazioni
002700940308     C                   EXSR      DLTCSV
002800940308      *  Fine pgm
002900940308     C                   SETON                                        LR
003000940308      *----------------------------------------------------*
003100940308      *   DLTCSA: Routinte di cancellazione errori da CSA  *
003200940308      *----------------------------------------------------*
003300940308     C     DLTCSA        BEGSR
003400940308      *----------------------------------------------------*
003500940308      *  Eseguo posizionamento iniziale su archivio anagrafico
003600940308      *  iniziando a leggere dall'anno della data iniziale
003700950203     C                   MOVEL     D60SDD        CSWAAS
003800950203     C                   MOVEL     D60SDD        ANNINZ            4 0
003900950203     C                   MOVEL     D60SDA        ANNFIN            4 0
004000950203     C     CSWAAS        SETLL     TNCSA01L
004100950203     C                   READ      TNCSA01L                               80
004200940308      *  continuo a fino a fine file o fino a che l'anno di
004300940308      *  spedizione non risulta superiore a quello della data
004400940308      *  massima richiesta
004500940308     C     *IN80         DOWEQ     '0'
004600940310     C     CSAAAS        ANDLE     ANNFIN
004700940308     C                   Z-ADD     CSAAAS        CSWAAS
004800940308     C                   Z-ADD     CSALNP        CSWLNP
004900940308     C                   Z-ADD     CSANRS        CSWNRS
005000940308     C                   Z-ADD     CSANSP        CSWNSP
005100940308      *  controllo se esiste record corrispondente su BLCSB
005200950203     C     KEYCSB        CHAIN     TNCSB03L                           71
005300940308      *  se non esiste cancello record da BLCSA
005400940308     C     *IN71         IFEQ      '1'
005500950203     C                   DELETE    TNCSA000
005600940308     C                   END
005700940308      *  lettura successiva
005800950203     C                   READ      TNCSA01L                               80
005900940308     C                   END
006000940308     C*----------------------------------------------------*
006100940308     C                   ENDSR
006200940308      *----------------------------------------------------*
006300940308      *   DLTCSV: Routinte di cancellazione errori da CSV  *
006400940308      *----------------------------------------------------*
006500940308     C     DLTCSV        BEGSR
006600940308      *----------------------------------------------------*
006700940308     C                   SETOFF                                       80
006800950203     C                   MOVEL     D60SDD        CSWAAS
006900950203     C                   MOVEL     D60SDD        ANNINZ            4 0
007000950203     C                   MOVEL     D60SDA        ANNFIN            4 0
007100950203     C     CSWAAS        SETLL     TNCSV01L
007200031024     C                   READ      TNCSV01L                               80
007300950203      *  continuo a fino a fine file o fino a che l'anno di
007400950203      *  spedizione non risulta superiore a quello della data
007500950203      *  massima richiesta
007600940308     C     *IN80         DOWEQ     '0'
007700950203     C     CSVAAS        ANDLE     ANNFIN
007800940308      *  Test rottura spedizione
007900940308     C                   EXSR      GESSPE
008000940308      *  Posizionamento su spedizione successiva
008100950203     C     KEYCSB        SETGT     TNCSV01L
008200950203     C                   READ      TNCSV01L                               80
008300940308     C                   END
008400940308      *----------------------------------------------------*
008500940308     C                   ENDSR
008600940308      *----------------------------------------------------*
008700940308      *   GESSPE: Gestione spedizione                      *
008800940308      *----------------------------------------------------*
008900940308     C     GESSPE        BEGSR
009000940308      *----------------------------------------------------*
009100940308     C                   Z-ADD     CSVAAS        CSWAAS
009200940308     C                   Z-ADD     CSVLNP        CSWLNP
009300940308     C                   Z-ADD     CSVNRS        CSWNRS
009400940308     C                   Z-ADD     CSVNSP        CSWNSP
009500940310     C*----------------------------------------------------*
009600940308      *  Controllo se la data della spedizione esaminata
009700940308      *  rientra nei limiti richiesti
009800950203     C                   Z-ADD     CSBMGS        DATDSP            8 0
009900950203     C                   MOVEL     CSBAAS        DATDSP
010000950203     C     DATDSP        IFGE      D60SDD
010100950203     C     DATDSP        ANDLE     D60SDA
010200940310     C*----------------------------------------------------*
010300940308      *  controllo se esiste record corrispondente su BLCSB
010400950203     C     KEYCSB        CHAIN     TNCSB03L                           71
010500940308      *  se non esiste cancello record da BLCSA
010600940310     C     *IN71         IFEQ      '1'
010700940310     C                   SETOFF                                       72
010800940310     C     *IN72         DOWEQ     '0'
010900950203     C     KEYCSB        DELETE    TNCSV000                           72
011000940308     C                   END
011100940308     C                   END
011200940308     C*----------------------------------------------------*
011300940310     C                   END
011400940310     C*----------------------------------------------------*
011500940308     C                   ENDSR
011600940308      *----------------------------------------------------*
011700940308      *   *INZSR: Operazioni iniziali                      *
011800940308      *----------------------------------------------------*
011900940308     C     *INZSR        BEGSR
012000940308     C*
012100940308     C     *ENTRY        PLIST
012200940308     C                   PARM                    KPJBA
012300950203     C                   MOVEL     KPJBU         TNSC60
012400940308     C*----------------------------------------------------*
012500940308     C* Definizione chiavi
012600940308     C     KEYCSB        KLIST
012700950203     C                   KFLD                    CSWAAS
012800940308     C                   KFLD                    CSWLNP
012900940308     C                   KFLD                    CSWNRS
013000940308     C                   KFLD                    CSWNSP
013100940308     C*----------------------------------------------------*
013200940308     C* Definizione chiavi
013300940308     C     *LIKE         DEFINE    CSBAAS        CSWAAS
013400940308     C     *LIKE         DEFINE    CSBLNP        CSWLNP
013500940308     C     *LIKE         DEFINE    CSBLNP        SAVLNP
013600940308     C     *LIKE         DEFINE    CSBNRS        CSWNRS
013700940308     C     *LIKE         DEFINE    CSBNSP        CSWNSP
013800940308     C*----------------------------------------------------*
013900940308     C* Inizializzazione variabili
014000940308     C                   Z-ADD     0             SAVLNP
014100940308     C*----------------------------------------------------*
014200940308     C                   ENDSR
014300940308     C*----------------------------------------------------*
