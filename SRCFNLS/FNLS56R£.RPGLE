000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS56R *----------------------------------------------------*
000300910205     H*    PREPARA TABELLA FILIALI/ANOMALIE PER P.C.                 *
000400000000     H*--------------------------------------------------------------*
000500900509     FAZORG01L  IF   E           K DISK
000600910205     FTABEL00F  IF   E           K DISK
000700900509     FFLDSP00F  O    E             DISK
000800900509     D KPJBA         E DS
000900040625     D DS3E          E DS
001000000000     C*---------------------------------------------------------------*
001100000000     C     *ENTRY        PLIST
001200000000     C                   PARM                    KPJBA
001300910205     C* ACCESSO   TABEL                                    *
001400910205     C     KTAB          KLIST
001500910205     C                   KFLD                    CODUT             1 0
001600910205     C                   KFLD                    COD               2
001700910205     C                   Z-ADD     1             CODUT
001800910205     C                   MOVEL     '3E'          COD
001900000000     C*---------------------------------------------------------------*
002000910205     C* FILIALI
002100910205     C*---------------------------------------------------------------*
002200910205     C*
002300910205     C     *LOVAL        SETLL     AZORG
002400910205     C                   DO        *HIVAL
002500910205     C                   READ      AZORG                                  58
002600910205     C  N58              DO
002700910205     C     ORGFVA        IFNE      '*'
002800920928     C     ORGFAG        ANDNE     'V'
002900910205     C                   MOVEL     ORGFIL        DSPCLA
003000910205     C                   MOVEL     ORGDES        DSPDLA
003100910205     C                   WRITE     FLDSP
003200910205     C                   END
003300910205     C                   END
003400910205     C  N58              END
003500910205     C*---------------------------------------------------------------*
003600910205     C* ANOMALIE
003700910205     C*---------------------------------------------------------------*
003800910205     C*
003900910205     C     KTAB          SETLL     TABEL
004000910205     C                   DO        *HIVAL
004100910205     C     KTAB          READE     TABEL                                  59
004200910205     C  N59              DO
004300910205     C     TBLFLG        IFNE      '*'
004400040625     c                   movel     tbluni        ds3e
004500040625     c                   if        §3euti<>'N'
004600910205     C                   MOVE      *BLANK        DSPCLA
004700910212     C                   MOVEL     TBLKEY        §AA1              1
004800910212     C                   MOVE      §AA1          DSPCLA
004900120711     C                   MOVEL     §3ede12       DSPDLA
005000910205     C                   WRITE     FLDSP
005100910205     C                   END
005200040625     C                   END
005300910205     C                   END
005400910205     C  N59              END
005500910205     C*
005600000000     C                   SETON                                        LR
