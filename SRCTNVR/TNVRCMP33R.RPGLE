000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*------------------------------------------------------------------------*
000300000000     F* DATA BASE
000400000000     F*------------------------------------------------------------------------*
000500010829     FECCED30C  UF   E           K DISK
000600010829     FECCET30C  UF   E           K DISK
000700010808     D*------------------------------------------------------------------------*
000800000000     C*------------------------------------------------------------------------*
000900000000     C* MAIN LINE
001000000000     C*------------------------------------------------------------------------*
001100010808     C*
001200010829     C                   EXSR      EXECNV
001300010829     C*
001400010808     C                   SETON                                            LR
001500010808     C*------------------------------------------------------------------------*
001600010829     C* EXECNV - ESEGUE OPERAZIONI DI CONVERSIONE
001700010808     C*------------------------------------------------------------------------*
001800010829     C     EXECNV        BEGSR
001900010808     C*
002000010829     C     *loval        SETLL     ECCET30C
002100010829     C                   READ      ECCET30C
002200010808     C*
002300010922DO  1C                   DOW       not %eof(ECCET30C)
002400010829     C* Inizializzo i flag di wrk
002500010829     C                   Z-ADD     *zeros        flgC033           1 0
002600010829     C                   Z-ADD     *zeros        flgUNIQ           1 0
002700010808     C*
002800010829     C* Per ogni bolla di ECCET00F scorre tutti i record correlati su ECCED00F
002900010921     C     KEYCED        CHAIN     ECCED30C                           98
003000010921     C*
003100010922DO  2C                   DOW       not *in98
003200010829     C* Incremento il contatore dei record di dettaglio di ogni bolla
003300010829     C                   ADD       1             flgUNIQ
003400010829     C* Verifico che esista la Comp. 033 nei dettagli x bolla
003500010922IF  3C                   IF        CEDCMP = 033
003600010829     C                   Z-ADD     1             flgC033
003700010829     C                   Z-ADD     CEDIMP        CETSES
003800010829     C                   DELETE    ECCED30C
003900010922E   3C                   ENDIF
004000010829     C* Leggo successivo record di dettaglio x bolla
004100010921     C     KEYCED        READE     ECCED30C                               98
004200010829     C*
004300010922E   2C                   ENDDO
004400010921     C*
004500010829     C* Effettuo considerazioni su quanto ricavato dal dettaglio bolle
004600010922IF  2C                   IF        flgC033 = 1
004700010922IF  3C                   IF        flgUNIQ = 1
004800010829     C                   MOVEL     'N'           CETDET
004900010922E   3C                   ENDIF
005000010829     C                   UPDATE    ECCET000
005100010922E   2C                   ENDIF
005200010808     C*
005300010829     C                   READ      ECCET30C
005400010808     C*
005500010922E   1C                   ENDDO
005600010808     C*
005700010808     C                   ENDSR
005800010808     C*------------------------------------------------------------------------*
005900000000     C* *INZSR - OPERAZIONI INIZIALI
006000000000     C*------------------------------------------------------------------------*
006100000000     C     *INZSR        BEGSR
006200991117     C*
006300010829     C* DEFINIZIONE CHIAVI - ECCED30C
006400010829     C     KEYCED        KLIST
006500010829     C                   KFLD                    CETAAS
006600010829     C                   KFLD                    CETLNP
006700010829     C                   KFLD                    CETNRS
006800010829     C                   KFLD                    CETNSP
006900010829     C                   KFLD                    CETTBL
007000010829     C*
007100010829     C                   ENDSR
