000100940308     H DECEDIT('0,') DATEDIT(*DMY.)
000200940308?     *--------------------------------------------------------------*
000300950203      * TNSC63R1                                                     *
000400940308      *       *-----------------------------------------*            *
000500940308      *           STAMPA/PULIZIA CONTRASSEGNI DI SEDE                *
000600940308      *                PULIZIA ARCHIVIO BOLLE                        *
000700940308      *       *-----------------------------------------*            *
000800940308      *                                                              *
000900940308?     *--------------------------------------------------------------*
001000950203     FTNCPV01L  IP   E           K DISK
001100940308?     *----------
001200950203     FTNCSV01L  UF   E           K DISK
001300031023?     *----------
001400031023     FTITAS30C  IF   E           K DISK
001500940308?     *--------------------------------------------------------------*
001600940308?     *  DS                                                          *
001700940308?     *--------------------------------------------------------------*
001800940308     D KPJBA         E DS
001900940308      *
002000950203     D TNSC60        E DS                  EXTNAME(TNSC60DS)
002100940308      *---------------------------------------------------------------*
002200940308      *   C I C L O      P R I N C I P A L E                          *
002300940308      *---------------------------------------------------------------*
002400950203     C*  Leggo i record di TNCPV e per ognuno di essi procede alla
002500950203     C*  cancellazione della variazione corrispondente in TNCSV
002600940308     C                   Z-ADD     CPVLNP        CSWLNP
002700940308     C                   Z-ADD     CPVAAS        CSWAAS
002800940308     C                   Z-ADD     CPVNRS        CSWNRS
002900940308     C                   Z-ADD     CPVNSP        CSWNSP
003000940308     C                   Z-ADD     CPVDTV        CSWDTV
003100940308     C                   Z-ADD     CPVORV        CSWORV
003200031024     C* prima di cancellare controllo che non esista più la bolla su TITAS*
003300031023     C                   SETOFF                                       40
003400031023     C     KEYCSB        SETLL     TITAS30C                               40
003500031023     C     *IN40         IFEQ      '0'
003600950203     C     KEYCSV        CHAIN     TNCSV01L                           70
003700950203     C  N70              DELETE    TNCSV000
003800031023     C                   END
003900940308      *----------------------------------------------------*
004000940308      *   *INZSR: Operazioni iniziali                      *
004100940308      *----------------------------------------------------*
004200940308     C     *INZSR        BEGSR
004300940308     C*
004400940308     C     *ENTRY        PLIST
004500940308     C                   PARM                    KPJBA
004600950203     C                   MOVEL     KPJBU         TNSC60
004700940308     C*----------------------------------------------------*
004800940308     C* Definizione chiavi
004900940308     C     KEYCSV        KLIST
005000950203     C                   KFLD                    CSWAAS
005100940308     C                   KFLD                    CSWLNP
005200940308     C                   KFLD                    CSWNRS
005300940308     C                   KFLD                    CSWNSP
005400940308     C                   KFLD                    CSWDTV
005500940308     C                   KFLD                    CSWORV
005600031023     C     KEYCSB        KLIST
005700031023     C                   KFLD                    CSWAAS
005800031023     C                   KFLD                    CSWLNP
005900031023     C                   KFLD                    CSWNRS
006000031023     C                   KFLD                    CSWNSP
006100940308     C*----------------------------------------------------*
006200940308     C* Definizione chiavi
006300940308     C     *LIKE         DEFINE    CSVAAS        CSWAAS
006400940308     C     *LIKE         DEFINE    CSVLNP        CSWLNP
006500940308     C     *LIKE         DEFINE    CSVNRS        CSWNRS
006600940308     C     *LIKE         DEFINE    CSVNSP        CSWNSP
006700940308     C     *LIKE         DEFINE    CSVDTV        CSWDTV
006800940308     C     *LIKE         DEFINE    CSVORV        CSWORV
006900940308     C*----------------------------------------------------*
007000940308     C                   ENDSR
007100940308     C*----------------------------------------------------*
