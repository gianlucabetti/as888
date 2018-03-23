000100040205     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040205
000300040205     fWFMRL00F  uP   E             DISK
000400040206     fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
000500040206     FPRTF198   O    F  198        PRINTER OFLIND(*INOG)
000600040204
000700040206     d wdtda$          s               d   datfmt(*iso)
000800040206     d wdtal$          s               d   datfmt(*iso)
000900040205
001000040205     D KPJBA         E DS
001100030926
001200030205     C*---------------------------------------------------------------*
001300040206
001400040206     c                   If        Tasdrt = 0
001500040206     c   of              except    testa_Q
001600040206     c                   except    detta_Q
001700040206     c                   add       1             CONTA_Q           6 0
001800040206     c                   ELSE
001900040206     c     *iso          movel     tasdrt        wdtda$
002000040206     c                   subdur    3:*d          wdtda$
002100040206     c     *iso          movel     wdtda$        Wdtda             8 0
002200040206     c     *iso          movel     tasdrt        wdtal$
002300040206     c                   adddur    3:*d          wdtal$
002400040206     c     *iso          movel     wdtal$        Wdtal             8 0
002500040206
002600040206     c                   if        exmdum < wdtda  or exmdum > wdtal
002700040206     c   og              except    testa_P
002800040206     c                   except    detta_P
002900040206     c                   add       1             CONTA_P           6 0
003000040206     c                   endif
003100040206     c                   endif
003200040205
003300040206     CLR                 except    fine_p
003400040206     CLR                 except    fine_q
003500040205      **-------------------------------------------------------------**
003600040204     C     *INZSR        BEGSR
003700040205
003800040206     c                   except    testa_P
003900040206     c                   except    testa_Q
004000040204      *
004100040204     C                   ENDSR
004200040206      **-------------------------------------------------------------**
004300040206     OQsysprt   E            testa_q          02
004400040206     O                                         + 20 'RECORD EXCEL MERLONI NON T-
004500040206     O                                              ROVATO SU TITAS'
004600040206     OQsysprt   E            detta_q     1
004700040206     O                       exmdum
004800040206     O                       exmrm1            +  2
004900040206     O                       exmddt            +  2
005000040206     O                       exmloc            +  2
005100040206     OQsysprt   E            fine_q      2
005200040206     O                       conta_q           +  2
005300040206      *-----------------------------
005400040206     Oprtf198   E            testa_p          02
005500040206     O                                         + 30 'RECORD EXCEL MERLONI TROVA-
005600040206     O                                              TO SU TITAS - date da verif-
005700040206     O                                              care'
005800040206     Oprtf198   E            detta_p     1
005900040206     O                       exmdum
006000040206     O                       exmrm1            +  2
006100040206     O                       exmddt            +  2
006200040206     O                       exmnco            +  2
006300040206     O                       exmloc            +  2
006400040206     O                       TASaas            +  4
006500040206     O                       TASlnp            +  2
006600040206     O                       TASnrs            +  2
006700040206     O                       TASnsp            +  2
006800040206     O                       TASdrt            +  2
006900040206     O                       TASncl            +  2
007000040206     O                       TASlod            +  2
007100040206     O                       TASprd            +  2
007200040206     OPrtf198   E            fine_p      2
007300040206     O                       conta_p           +  2
