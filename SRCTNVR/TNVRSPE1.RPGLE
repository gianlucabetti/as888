000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200001024      *
000300990312     FFNSPE01L  UF   E           K DISK
000400001024     FTABEL00F  IF   E           K DISK
000500990312      *
000600001024     D FILA            S              3    DIM(29)                              FIL TBLUNI    TE
000700001024     D FIL             S              3  0 DIM(999)                             FIL.MECCAN
000800001024      *
000900001024     D xx              S              3  0
001000001024     D jj              S              4  0
001100001024     D KUTE            S                   like(TBLKUt)  inz(1)
001200001024     D KCOD            S                   like(TBLCOD)  inz('£3')
001300990312      *----------------------------------------------------
001400001024      *
001500001024      * CARICO £3
001600001024     C     KTAB          KLIST
001700001024     C                   KFLD                    KUTE
001800001024     C                   KFLD                    KCOD
001900001024      *
002000001024     C                   clear                   jj
002100001024     C     KTAB          SETLL     TABEL
002200001024     C     KTAB          READE     TABEL
002300001024      *
002400001024     C                   dow       NOT %eof(tabel00f)
002500001024     C                   if        TBLKEY > *ZEROS
002600001024      *
002700001024     C                   MOVEA     TBLUNI        FILA
002800001024     C                   do        29            XX
002900001024     C                   if        FILA(xx) > *ZERO
003000001024     C                   ADD       1             jj
003100001024     C                   MOVEL     FILA(xx)      FIL(jj)                        SECONDARIA
003200001024     C                   endif
003300001024     C                   enddo
003400001024      *
003500001024     C                   endif
003600001024     C     KTAB          READE     TABEL
003700001024     C                   enddo
003800001024      *
003900990312      ******
004000990312      ****  ARCHIVIO LUOGHI, SFLEGGO QUELLI DI COMPETENZA
004100990312      ******
004200990312     C     'L'           SETLL     FNSPE000
004300001024     C     'L'           READE     FNSPE000
004400990312      *
004500001024    1C                   DOW       NOT %EOF(FNSPE01L)
004600990312      *
004700990312     C                   MOVEL     SPECLI        POCLI             3 0
004800001024     C     POCLI         LOOKUP    FIL                                    32
004900001024    2C                   IF         *IN32 = *ON
005000001024     C                   clear                   SPEDTR
005100001024     C                   clear                   SPEFTR
005200990312     C                   UPDATE    FNSPE000
005300961014    2C                   ENDIF
005400990312      *
005500001024     C     'L'           READE     FNSPE000
005600960828    1C                   ENDDO
005700001024      *
005800001024     C                   eval      *inlr = *on
