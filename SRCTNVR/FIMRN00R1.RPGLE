000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020729     FRNFIM00F  IF   E             DISK
000300020729     FECFIM30C  UF   E           K DISK
000400020729     IECFIM000      01
000500020729     IECFIMP00      02
000600020729     C     *LIKE         DEFINE    FIMAAS        KIMAAS
000700020729     C     *LIKE         DEFINE    FIMLNP        KIMLNP
000800020729     C     *LIKE         DEFINE    FIMNRS        KIMNRS
000900020729     C     *LIKE         DEFINE    FIMNSP        KIMNSP
001000020729     C     *LIKE         DEFINE    FIMTBL        KIMTBL
001100020729     C     KEYFIM        KLIST
001200020729     C                   KFLD                    KIMAAS
001300020729     C                   KFLD                    KIMLNP
001400020729     C                   KFLD                    KIMNRS
001500020729     C                   KFLD                    KIMNSP
001600020729     C                   KFLD                    KIMTBL
001700020729     C                   READ      RNFIM00F                               99
001800020729DO  1C                   DOW       NOT *in99
001900020729     C                   Z-ADD     RNMAAS        KIMAAS
002000020729     C                   Z-ADD     RNMLNP        KIMLNP
002100020729     C                   Z-ADD     RNMNRS        KIMNRS
002200020729     C                   Z-ADD     RNMNSP        KIMNSP
002300020729     C                   MOVEL     RNMTBL        KIMTBL
002400020729     C                   SETOFF                                       0102
002500020729     C     KEYFIM        SETLL     ECFIM30C
002600020729     C     KEYFIM        READE     ECFIM30C                               98
002700020729DO  2C                   DOW       NOT *in98
002800020729IF  3C                   IF        *IN01
002900020729     C                   DELETE    ECFIM000
003000020729E   3C                   ENDIF
003100020729IF  3C                   IF        *IN02
003200020729     C                   DELETE    ECFIMP00
003300020729E   3C                   ENDIF
003400020729     C                   SETOFF                                       0102
003500020729     C     KEYFIM        READE     ECFIM30C                               98
003600020729E   2C                   ENDDO
003700020729     C                   READ      RNFIM00F                               99
003800020729E   1C                   ENDDO
003900020726     C*
004000020716     C                   SETON                                            LR
