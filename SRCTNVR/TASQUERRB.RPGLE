000100020618     H DECEDIT('0,') DATEDIT(*DMY.)
000200020618     F*------------------------------------------------------------------------*
000300020618     F* DATA BASE
000400020618     F*------------------------------------------------------------------------*
000500020618     Fquerr     IF   E             DISK
000600020618     Ftitas30c  UF   E           K DISK
000700020618     F                                     IGNORE(TITAS000)
000800020618     F                                     IGNORE(TITASP00)
000900020618     C*------------------------------------------------------------------------*
001000020618     C* MAIN LINE
001100020618     C*------------------------------------------------------------------------*
001200020618     C     *LIKE         DEFINE    TasAas        kasAas
001300020618     C     *LIKE         DEFINE    TasLnp        kasLnp
001400020618     C     *LIKE         DEFINE    TasNrs        kasNrs
001500020618     C     *LIKE         DEFINE    TasNsp        kasNsp
001600020618     C     *LIKE         DEFINE    TasTbl        kasTbl
001700020618     c*
001800020618     C     keytas        KLIST
001900020618     C                   KFLD                    kasAas
002000020618     C                   KFLD                    kasLnp
002100020618     C                   KFLD                    kasNrs
002200020618     C                   KFLD                    kasNsp
002300020618     C                   KFLD                    kasTbl
002400020618     C*
002500020618     C                   READ      querr
002600020618do  1C                   DOW       not %eof(querr)
002700020618     C                   EVAL      kasaas = queaas
002800020618     C                   EVAL      kaslnp = quelnp
002900020618     C                   EVAL      kasnrs = quenrs
003000020618     C                   EVAL      kasnsp = quensp
003100020618     C                   EVAL      kastbl = quetbl
003200020618     C     keytas        CHAIN     titas30c
003300020618if  2C                   IF        %found(titas30c)
003400020618     C                   MOVEL     'W'           tasll2
003500020618     C                   UPDATE    titas010
003600020618e   2C                   ENDIF
003700020618     c*
003800020618     C                   READ      querr
003900020618E   1C                   ENDDO
004000020618     c*
004100020618     C                   SETON                                            LR
