000100010706     fTNCPO08L  UF   E           K DISK
000200010620     FTNCPS11L  IF   E           K DISK
000300010620     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000400981125
000500010620     d KCMM            s                   like(CPOCMM)   inz(1)
000600010706
000700010706     D CNCR80        E DS
000800010706     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000900010706     D TIBS50        E DS                  EXTNAME(TIBS50DS)
001000010706     D KPJBA         E DS
001100010706
001200010706     D TRUL06DS      E DS
001300010706     D  LIN                    1     90  0    DIM(30)
001400010706
001500010706
001600010706      * CARICO TABELLA £1
001700010706     C                   Z-ADD     1             CODUT
001800010706     C                   CALL      'X§PARUT'
001900010706     C                   PARM                    UT§DSE
002000010706     C                   MOVEL     RAGUT         RSUT             20
002100010706     C                   MOVEL     REC80         CNCR80
002200010706      *
002300010706     C                   CLEAR                   TRUL06DS
002400010706     C                   MOVE      '£1'          D06COD
002500010706     C                   MOVEL     SIMFEL        D06KEY
002600010706     C                   MOVEL(P)  TRUL06DS      KPJBU
002700010706     C                   CALL      'TRUL06R'
002800010706     C                   PARM                    KPJBA
002900010706     C                   MOVEL     KPJBU         TRUL06DS
003000010620
003100010620     c                   except    testa
003200010620     c                   eval      *inof = *off
003300010620
003400010620     c     Kcmm          setll     tncpo000
003500010620     c                   read      tncpo000
003600981125
003700010620     c                   dow       not %eof(tncpo08l)
003800010620
003900010706     C     CPOFLT        LOOKUP    LIN                                    99
004000010706     C                   IF        *IN99 = *ON
004100010706
004200010620     C     CPOCPO        SETLL     TNCPS000
004300010620     c                   IF        not %equal
004400010706     c   of              except    testa
004500010706     c   of              eval      *inof = *off
004600010706     c                   except    detta
004700010706     c                   CLEAR                   CPOFTR
004800010706     c                   CLEAR                   CPOCMM
004900010706     c                   UPDATE    TNCPO000
005000981125     c                   ENDIF
005100010620
005200010706     c                   ENDIF
005300010706
005400010620     c                   read      tncpo000
005500010620     c                   ENDDO
005600010620
005700010620     c                   eval      *inlr = *on
005800010706      *-------------------------------------------------
005900010706     OQSYSPRT   E            TESTA            02
006000010706     O                                         + 20 'POTENZIALI SENZA AZIONI'
006100010706     O                                         +  1 'CON COMMERCIALE'
006200010706     O                                         + 15 '*TNVRPOT01*'
006300010706     O                       UDATE         Y   + 15
006400010706     O                                          128 'Pag.'
006500010706     O                       PAGE          Z    132
006600010706     O          E            DETTA       1
006700010706     O                       CPOatb            +  1
006800010706     O                       CPOCPO            +  5
006900010706     O                       cporag            +  2
007000010706     O                       CPOFLS            +  5
007100010706     O                       cpoclt            +  2
007200010706     O                       cpoflt            +  5
007300010706     O                       cpocmm            +  5
