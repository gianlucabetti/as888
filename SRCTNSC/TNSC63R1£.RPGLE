000100940308     H DECEDIT('0,') DATEDIT(*DMY.)
000200940308?     *--------------------------------------------------------------*
000300940308      * TRAS63R1                                                     *
000400940308      *       *-----------------------------------------*            *
000500940308      *           STAMPA/PULIZIA CONTRASSEGNI DI SEDE                *
000600940308      *                PULIZIA ARCHIVIO BOLLE                        *
000700940308      *       *-----------------------------------------*            *
000800940308      *                                                              *
000900940308?     *--------------------------------------------------------------*
001000950203     FTNCPB00F  IP   E           K DISK
001100940308?     *----------
001200950203     FTNCSB03L  UF   E           K DISK
001300940308?     *----------
001400950203     FTNCSA01L  UF   E           K DISK
001500980105?     *----------
001600980105     FTNCSG01L  UF   E           K DISK
001700031023?     *----------
001800031023     FTITAS30C  IF   E           K DISK
001900940308?     *--------------------------------------------------------------*
002000940308?     *  DS                                                          *
002100940308?     *--------------------------------------------------------------*
002200940308     D KPJBA         E DS
002300940308      *
002400950203     D TNSC60        E DS                  EXTNAME(TNSC60DS)
002500940308      *---------------------------------------------------------------*
002600940308      *   C I C L O      P R I N C I P A L E                          *
002700940308      *---------------------------------------------------------------*
002800950203     C*  Leggo i record di TNCPB e per ognuno di essi, nel caso in cui
002900940308     C*  il contrassegno sia stato pagato controllo se è stato trasmesso
003000940308     C*  se si cancello la bolla e l'eventuale anagrafica collegata.
003100940308     C*  Se il contrassegno è stato annullato eseguo comunqe la cancellaz.
003200940308     C     CPBDDP        IFNE      0
003300940308     C     CPBFTR        ANDNE     ' '
003400940308     C     CPBSTA        OREQ      9
003500940308     C                   Z-ADD     CPBLNP        CSWLNP
003600940308     C                   Z-ADD     CPBAAS        CSWAAS
003700940308     C                   Z-ADD     CPBNRS        CSWNRS
003800940308     C                   Z-ADD     CPBNSP        CSWNSP
003900031024     C* prima di cancellare controllo che non esista più la bolla su TITAS*
004000031023     C                   SETOFF                                       40
004100031023     C     KEYCSB        SETLL     TITAS30C                               40
004200031023     C     KEYCSB        CHAIN     TNCSB03L                           70
004300031023     C     *IN40         IFEQ      '0'
004400950203     C  N70              DELETE    TNCSB000
004500980105     C     KEYCSA        CHAIN     TNCSA01L                           70
004600950203     C  N70              DELETE    TNCSA000
004700980105     C     KEYCSG        CHAIN     TNCSG01L                           70
004800980105     C  N70              DELETE    TNCSG000
004900031024     C* se trovo la bolla metto il flag CSBFIN = 'S' x indicare che il
005000031024     C* contrassegno è già stato archiviato su ottico e quindi non lo
005100031024     C* deve più fare
005200031023     C                   ELSE
005300031023     C                   MOVEL     'S'           CSBFIN
005400031023     C  N70              UPDATE    TNCSB000
005500940308     C                   END
005600031023     C                   END
005700940308      *----------------------------------------------------*
005800940308      *   *INZSR: Operazioni iniziali                      *
005900940308      *----------------------------------------------------*
006000940308     C     *INZSR        BEGSR
006100940308     C*
006200940308     C     *ENTRY        PLIST
006300940308     C                   PARM                    KPJBA
006400950203     C                   MOVEL     KPJBU         TNSC60
006500940308     C*----------------------------------------------------*
006600940308     C* Definizione chiavi
006700940308     C     KEYCSB        KLIST
006800950203     C                   KFLD                    CSWAAS
006900940308     C                   KFLD                    CSWLNP
007000940308     C                   KFLD                    CSWNRS
007100940308     C                   KFLD                    CSWNSP
007200940308     C*
007300940308     C     KEYCSA        KLIST
007400940308     C                   KFLD                    CSWAAS
007500940308     C                   KFLD                    CSWLNP
007600940308     C                   KFLD                    CSWNRS
007700940308     C                   KFLD                    CSWNSP
007800980105     C     KEYCSG        KLIST
007900980105     C                   KFLD                    CSWAAS
008000980105     C                   KFLD                    CSWLNP
008100980105     C                   KFLD                    CSWNRS
008200980105     C                   KFLD                    CSWNSP
008300940308     C*----------------------------------------------------*
008400940308     C* Definizione chiavi
008500940308     C     *LIKE         DEFINE    CSBAAS        CSWAAS
008600940308     C     *LIKE         DEFINE    CSBLNP        CSWLNP
008700940308     C     *LIKE         DEFINE    CSBNRS        CSWNRS
008800940308     C     *LIKE         DEFINE    CSBNSP        CSWNSP
008900940308     C*----------------------------------------------------*
009000940308     C                   ENDSR
009100940308     C*----------------------------------------------------*
