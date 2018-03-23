000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020716     FFIMRN     IF   E           K DISK
000300020716     F                                     RENAME(ECFIMP00:FIMRNP00)
000400020716     FECFIM30C  UF A E           K DISK
000500020716     C     KEYFIM        KLIST
000600020716     C                   KFLD                    FIMAAS
000700020716     C                   KFLD                    FIMLNP
000800020716     C                   KFLD                    FIMNRS
000900020716     C                   KFLD                    FIMNSP
001000020716     C                   KFLD                    FIMTBL
001100020716     C                   READ      FIMRN                                  99
001200020716DO  1C                   DOW       NOT *in99
001300020716     C     KEYFIM        CHAIN     ECFIM30C                           98
001400020716IF  2C                   IF        *IN98
001500020716     C                   WRITE     ECFIMP00
001600020716E   2C                   ENDIF
001700020716     C                   READ      FIMRN                                  99
001800020716E   1C                   ENDDO
001900020716     C                   SETON                                            LR
