000100020618     H DECEDIT('0,') DATEDIT(*DMY.)
000200020618     F*------------------------------------------------------------------------*
000300020618     F* DATA BASE
000400020618     F*------------------------------------------------------------------------*
000500020618     Ftitasque  IF   E             DISK
000600020618     Fquerr     UF A E             DISK
000700020618     Ftitas30c  IF   E           K DISK
000800020618     D                 DS
000900020618     D  DSAASN                 1      3P 0
001000020618     D  DSAASA                 1      3
001100020618     D                 DS
001200020618     D  DSLNPN                 1      2P 0
001300020618     D  DSLNPA                 1      2
001400020618     D                 DS
001500020618     D  DSNRSN                 1      2P 0
001600020618     D  DSNRSA                 1      2
001700020618     D                 DS
001800020618     D  DSNSPN                 1      4P 0
001900020618     D  DSNSPA                 1      4
002000020618     ITITAS000      01
002100020618     ITITAS010      02
002200020618     ITITASP00      03
002300020618     C*------------------------------------------------------------------------*
002400020618     C* MAIN LINE
002500020618     C*------------------------------------------------------------------------*
002600020618     C     *LIKE         DEFINE    TasAas        kasAas
002700020618     C     *LIKE         DEFINE    TasLnp        kasLnp
002800020618     C     *LIKE         DEFINE    TasNrs        kasNrs
002900020618     C     *LIKE         DEFINE    TasNsp        kasNsp
003000020618     C     *LIKE         DEFINE    TasTbl        kasTbl
003100020618     c*
003200020618     C     keytas        KLIST
003300020618     C                   KFLD                    kasAas
003400020618     C                   KFLD                    kasLnp
003500020618     C                   KFLD                    kasNrs
003600020618     C                   KFLD                    kasNsp
003700020618     C                   KFLD                    kasTbl
003800020618     c                   CLEAR                   dstbla            2
003900020618     C*
004000020618     C                   READ      titasque
004100020618do  1C                   DOW       not %eof(titasque)
004200020618if  2C                   IF        queflg = '2'
004300020618     C                   EVAL      dstbla = %subst(quedta: 603: 2)
004400020618     C                   EVAL      dslnpa = %subst(quedta: 605: 2)
004500020618     C                   EVAL      dsaasa = %subst(quedta: 609: 3)
004600020618     C                   EVAL      dsnrsa = %subst(quedta: 612: 2)
004700020618     C                   EVAL      dsnspa = %subst(quedta: 614: 4)
004800020618     c                   Z-ADD     dsaasn        kasaas
004900020618     c                   Z-ADD     dslnpn        kaslnp
005000020618     c                   Z-ADD     dsnrsn        kasnrs
005100020618     c                   Z-ADD     dsnspn        kasnsp
005200020618     c                   MOVEL     dstbla        kastbl
005300020618     C                   SETOFF                                       010203
005400020618if  3C     keytas        CHAIN(N)  titas30c
005500020618     C                   IF        %found(titas30c) AND
005600020618     C                             *in02 = *on
005700020618     C                   EVAL      queaas = TasAas
005800020618     C                   EVAL      quelnp = TasLnp
005900020618     C                   EVAL      quenrs = TasNrs
006000020618     C                   EVAL      quensp = TasNsp
006100020618     C                   EVAL      quetbl = TasTbl
006200020618     C                   WRITE     querr00
006300020618e   3C                   ENDIF
006400020618e   2C                   ENDIF
006500020618     c*
006600020618     C                   READ      titasque
006700020618E   1C                   ENDDO
006800020618     C*
006900020618     c                   SETON                                        lr
