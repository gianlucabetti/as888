000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200940610     H*F UDATER *----------------------------------------------------*
000300970530     H*                                                              *
000400970530     H*       SCRIVO MOTIVO MANCATA CONSEGNA                         *
000500900418     H*--------------------------------------------------------------*
000600060214     FFiAR401L  UF A E           K DISK    USROPN
000700970530     F                                     COMMIT
000800940912     D*--------------------------------------------------------------*
000900900418     D KPJBA         E DS
001000970530     D FNLR53        E DS                  EXTNAME(FNLR53DS3)
001100940920     I*---------------------------------------------------------------*
001200970530     C     *ENTRY        PLIST
001300970530     C                   PARM                    KPJBA
001400970530     C                   MOVEL     KPJBU         FNLR53
001500970530     C*
001600060214     C                   OPEN      FiAR401L                             16
001700970530     C   16              GOTO      FINE                                         OPEN vado a fine
001800970530     C*
001900970530     C*  Se non ci sono errori aggiorno campi subfile
002000970530     C                   Z-ADD     D53AAS        KAAS
002100970530     C                   Z-ADD     D53LNP        KLNP
002200970530     C                   Z-ADD     D53NRS        KNRS
002300970530     C                   Z-ADD     D53NSP        KNSP
002400970530     C                   MOVE      D53TR1        KTRC
002500060214     C     KAR4          CHAIN     FiAR401L                           94
002600970530     C     '°':' '       XLATE     D53NT1        AR4NOT
002700970530     C                   MOVE      *BLANKS       AR4FTR
002800970530     C                   MOVE      *ZEROS        AR4DTR
002900970530     C     *IN94         IFEQ      '1'
003000970530     C                   MOVE      D53TR1        AR4TRC
003100970530     C                   Z-ADD     KLNP          AR4LNP
003200970530     C                   Z-ADD     KAAS          AR4AAS
003300970530     C                   Z-ADD     KNRS          AR4NRS
003400970530     C                   Z-ADD     KNSP          AR4NSP
003500060214     C                   WRITE     FiAR4000
003600970530     C                   ELSE
003700060214     C                   UPDATE    FiAR4000
003800970530     C                   END
003900970530     C*  Scrivo seoconda nota
004000970530     C     D53NT2        IFNE      *BLANKS
004100970530     C                   MOVE      D53TR2        KTRC
004200060214     C     KAR4          CHAIN     FiAR401L                           94
004300970530     C     '°':' '       XLATE     D53NT2        AR4NOT
004400970530     C                   MOVE      *BLANKS       AR4FTR
004500970530     C                   MOVE      *ZEROS        AR4DTR
004600970530     C     *IN94         IFEQ      '1'
004700970530     C                   MOVE      D53TR2        AR4TRC
004800970530     C                   Z-ADD     KLNP          AR4LNP
004900970530     C                   Z-ADD     KAAS          AR4AAS
005000970530     C                   Z-ADD     KNRS          AR4NRS
005100970530     C                   Z-ADD     KNSP          AR4NSP
005200060214     C                   WRITE     FiAR4000
005300970530     C                   ELSE
005400060214     C                   UPDATE    FiAR4000
005500970530     C                   END
005600970530     C*
005700970530     C                   END
005800970530     C*
005900970530     C                   COMMIT
006000970530     C*
006100000407     C     FINE          TAG
006200970530     C                   SETON                                        LR
006300940913     C*------------------------------------------------------------*
006400940913     C*  Operazioni iniziali
006500940913     C*------------------------------------------------------------*
006600940913     C     *INZSR        BEGSR
006700940913     C*
006800970530     C* CHIAVE X FNAR4
006900970530     C     KAR4          KLIST
007000970530     C                   KFLD                    KAAS
007100970530     C                   KFLD                    KLNP
007200970530     C                   KFLD                    KNRS
007300970530     C                   KFLD                    KNSP
007400970530     C                   KFLD                    KTRC
007500940913     C*---------------------------------------------------------------*
007600940913     C*  Like DEFN
007700970530     C     *LIKE         DEFINE    AR4AAS        KAAS
007800970530     C     *LIKE         DEFINE    AR4LNP        KLNP
007900970530     C     *LIKE         DEFINE    AR4NRS        KNRS
008000970530     C     *LIKE         DEFINE    AR4NSP        KNSP
008100970530     C     *LIKE         DEFINE    AR4TRC        KTRC
008200940913     C*---------------------------------------------------------------*
008300940913     C                   ENDSR
008400970530     C*---------------------------------------------------------------*
