000100980323     H DECEDIT('0,') DATEDIT(*DMY.)
000200000809      *------------------------------------------------------------------------*
000300020829      * CREA NUOVA KEY X TABELLA 5D                                            *
000400000809      *------------------------------------------------------------------------*
000500020829     FTABEL00F  UF   E           K DISK
000501021107     d fil             s              3
000600000809      *
000601020829     C     *ENTRY        PLIST
000602021107     C                   PARM                    fil
000604020829     C                   z-add     1             tblkut
000605020829     C                   eval      tblcod = '5D'
000606020829     C                   eval      tblkey = '1'
000607020829     C     Ktab          KLIST
000608020829     C                   KFLD                    tblkut
000609020829     C                   KFLD                    tblcod
000610020829     C                   KFLD                    tblkey
000612020829     c*
000700020829     C     ktab          chain     Tabel00f
000701020829     c                   if        %found(tabel00f)
000702020829     c                   movel(p)  fil           tblkey
001200020829     C                   UPDATE    tabel
001300020829     C                   END
002200020829      *
002300990108     C                   SETON                                        LR
