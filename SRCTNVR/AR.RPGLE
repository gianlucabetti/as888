000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020213     FAP        O    E             PRINTER OFLIND(*IN01)
000300000000     C*------------------------------------------------------------------------*
000400000000     C*?MAIN LINE
000500000000     C*------------------------------------------------------------------------*
000600000000     C*
000700020213     C                   EVAL      BARCODE =
000800020213     C                             '~(s3p0h48v7,14,21,28b7,14,21,28s24700T01234'
000900020213     c                             + '567890123456789'
001000020213     C     1             DO        100
001100020213     C                   WRITE     SE60SK                                       *SALTO
001200020213     C     1             DO        35
001300020213     C                   WRITE     SE60DE
001400020213     C                   ENDDO
001500020213     C                   WRITE     SE60SP                                       *SPAZIO
001600020213     C                   WRITE     SE60BA
001700020213     C                   ENDDO
001800020213     C*
001900020213     c                   seton                                        lr
