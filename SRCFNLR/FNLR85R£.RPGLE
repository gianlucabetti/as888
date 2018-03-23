000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941212     H* FNLR85R *----------------------------------------------------*
000300920324     H*        - PULIZIA BOLLE TRANSITO
000400000000     H*--------------------------------------------------------------*
000500031128     FFNBTP15L  UF   E           K DISK
000600031128     FFNBTT11L  UF   E           K DISK
000700920325     D*
000800920325     D PARAM           DS
000900941212     D  COMDPT                25     32  0
001000920324     D                 DS
001100941212     D  BTPAAS                 1      4  0
001200941212     D  BTPMGS                 5      8  0
001300941212     D  BTPDSP                 1      8  0
001400031128     d
001500031128     d wflp            s                   Like(BTPFLP)
001600031128     d
001700000000     D KPJBA         E DS
001800000000     I/SPACE 3
001900000000     C*---------------------------------------------------------------*
002000000000     C     *ENTRY        PLIST
002100000000     C                   PARM                    KPJBA
002200900409     C                   MOVEL     KPJBU         PARAM
002300920205     C*---------------------------------------------------------------*
002400031128     C* ACCESSO FNBTP15L
002500031128     C     KBTP1         KLIST
002600031128     C                   KFLD                    Wflp
002700031128     C                   KFLD                    WDUT
002800031128     C     KBTP2         KLIST
002900031128     C                   KFLD                    Wflp
003000031128     C                   KFLD                    WDUT
003100031128     C                   KFLD                    WDET
003200941212     C* ACCESSO FNBTT01L
003300920324     C     KBTT          KLIST
003400031128     C                   KFLD                    BTPFLP
003500031128     C                   KFLD                    BTPAAS
003600941212     C                   KFLD                    BTPLNP
003700941212     C                   KFLD                    BTPNRS
003800941212     C                   KFLD                    BTPNSP
003900941212     C*
004000941212     C* DEFINIZIONE CAMPI
004100941212     C     *LIKE         DEFINE    BTPDUT        WDUT
004200941212     C     *LIKE         DEFINE    BTPDET        WDET
004300941212     C*---------------------------------------------------------------*
004400031128     c* Per ogni p.o. di transito elaboro i 2 cicli
004500031128     c                   z-add     0             wflp
004600031128     c     wflp          setgt     fnbtp000
004700031128     c                   read      fnbtp000                               32
004800031128    0c                   dow       not *in32
004900031128     c                   z-add     btpflp        wflp
005000941212     C***
005100941212     C** 1°  C I C L O : ELABORO SOLO I RECORD CON BTPDUT > 0 E DELETO
005200941212     C**                 QUELLI CHE HANNO LA DATA USCITA DAL TRANSITO
005300941212     C**                 MINORE O UGUALE ALLA DATA PULIZIA
005400941212     C***
005500941212     C                   Z-ADD     1             WDUT
005600031128     C     kbtp1         SETGT     FNBTP000
005700031128     C     wflp          READE     FNBTP000                               30
005800920306     C*
005900941212    1C     *IN30         DOWEQ     *OFF
006000941212    2C     BTPDUT        IFLE      COMDPT
006100920324     C                   EXSR      PULIZ
006200941213     C                   ELSE
006300941213     C* SE DATA USCITA DAL TRANSITO > DATA PULIZIA ESCO
006400941213     C                   SETON                                        30
006500941212    2C                   ENDIF
006600941212     C*
006700031128     C  N30wflp          READE     FNBTP000                               30
006800941212    1C                   ENDDO
006900941212     C***
007000941212     C** 2°  C I C L O : ELABORO SOLO I RECORD CON BTPDUT E BTPDET = 0
007100941212     C**                 E DELETO QUELLI CHE HANNO LA DATA SPEDIZIONE
007200941212     C**                 MINORE O UGUALE ALLA DATA PULIZIA
007300941212     C***
007400941212     C                   CLEAR                   WDUT
007500941212     C                   CLEAR                   WDET
007600031128     C     KBTP2         SETLL     FNBTP000
007700031128     C     KBTP2         READE     FNBTP000                               30
007800920325     C*
007900941212    1C     *IN30         DOWEQ     *OFF
008000941212    2C     BTPDSP        IFLE      COMDPT
008100920325     C                   EXSR      PULIZ
008200941212    2C                   ENDIF
008300920325     C*
008400031128     C     KBTP2         READE     FNBTP000                               30
008500941212    1C                   ENDDO
008600031128     c
008700031128     c* Leggo nuovo p.o. transito
008800031128     c     wflp          setgt     fnbtp000
008900031128     c                   read      fnbtp000                               32
009000031128    0C                   ENDDO
009100920205     C*
009200920205     C                   SETON                                        LR
009300920205     C*
009400941212     C*--- PULIZIA FILE TRANSITI E FILE COLLEGATI --------------------*
009500920324     C     PULIZ         BEGSR
009600941212     C**
009700941212     C* DETTAGLIO COLLI
009800941212     C**
009900941212     C     KBTT          SETLL     FNBTT000
010000941212     C     KBTT          READE     FNBTT000                               31
010100920324     C*
010200941212    1C     *IN31         DOWEQ     *OFF
010300941212     C                   DELETE    FNBTT000
010400941212     C     KBTT          READE     FNBTT000                               31
010500941212    1C                   ENDDO
010600920324     C**
010700941212     C* CANCELLO LA BOLLA TRANSITO
010800920324     C**
010900941212     C                   DELETE    FNBTP000
011000920324     C*
011100920306     C                   ENDSR
