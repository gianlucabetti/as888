000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200960530     H DECEDIT('0,') DATEDIT(*DMY/)
000300090626      *                                                                     *
000400050831      *---------------------------------------------------------------------------
000500130514     D RIFDUCROS     E DS
000600130514     D DUC_ARB       E DS
000700130513     D  campoF3                            like(F3)
000800090626      *---------------------------------------------------------------------------
000900090626     C     *ENTRY        PLIST
001000130514     C                   PARM                    RIFER_0          10            CNI
001100130514     C                   PARM                    RIFER_1          15            CU
001200130514     C                   PARM                    RIFER_2          15            AGE
001300130514     C                   PARM                    RIFER_3           1            TIPO
001400090626      *
001500130513     C                   SETON                                        LR
001600130513     c                   move      *blank        campoF3
001700130514     c                   move      *blank        rifer_2
001800130513      *
001900130513     C/EXEC SQL
002000130513     C+ Select F2, F3
002100130513     C+               into :rifer_2, :campoF3
002200130513     C+        FROM DUC_ARB
002300130513     C+        WHERE F1 = :rifer_1
002400130513     C+ fetch first row only
002500130513     C/END-EXEC
002600130513     c                   movel     campoF3       rifer_3
002700130513      *
002800130514      * se deve controllare fra quelle multiple
002900130514     c                   if        rifer_3 <>'U'
003000130514      *
003100130514     C/EXEC SQL
003200130514     C+ Select BRT_LIVRAISON
003300130514     C+               into :rifer_2
003400130514     C+        FROM
003500130514     C+             RIFDUCROS
003600130514     C+        WHERE
003700130514     C+              REFMORY = :rifer_0  and
003800130514     C+        BRT_REFERENCE = :rifer_1
003900130514     C+ fetch first row only
004000130514     C/END-EXEC
004100130514      *
004200130514     c                   endif
004300130514      *
004400130513     c                   return
004500090626      *------------------------------------------------------------
