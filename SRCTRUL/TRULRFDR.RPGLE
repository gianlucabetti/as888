000100030403     D RtvFD01         S                   LIKE(QDBQ25)
000200020618     D RtvFD02         S              9B 0 INZ(%SIZE(RtvFD01))
000300020618     D RtvFD03         S             20
000400020618     D RtvFD04         S              8    INZ('FILD0100')
000500030403     D RtvFD05         S             20    INZ(*BLANKS)
000600020618     D RtvFD06         S             10
000700020618     D RtvFD07         S              1    INZ('1')
000800020618     D RtvFD08         S             10    INZ('*LCL')
000900020618     D RtvFD09         S             10    INZ('*INT')
001000030403
001100030403     D TRULRFDDS     E DS
001200020618
001300020618      /COPY QSYSINC/QRPGLESRC,QDBRTVFD
001400020618      /COPY QSYSINC/QRPGLESRC,QUSEC
001500020618
001600030403     C*
001700030403     C     *ENTRY        PLIST
001800030403     C                   PARM                    TRULRFDDS
001900030403     C*
002000030403     C                   EVAL      RtvFD05 = RFDFIL + RFDLIB
002100030403
002200020618     C                   CLEAR                   QUSEC
002300020618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
002400020618
002500020618     C                   CALL      'QDBRTVFD'
002600030403     C     QDBQ25        PARM                    RtvFD01
002700020618     C                   PARM                    RtvFD02
002800020618     C                   PARM                    RtvFD03
002900020618     C                   PARM                    RtvFD04
003000020618     C                   PARM                    RtvFD05
003100020618     C                   PARM                    RtvFD06
003200020618     C                   PARM                    RtvFD07
003300020618     C                   PARM                    RtvFD08
003400020618     C                   PARM                    RtvFD09
003500020618     C                   PARM                    QUSEC
003600030403
003700030403     C*
003800030403     C* Se nn ritornata la lunghezza del file significa che l'API nn è stata in grado di trovare il FILE
003900030403     C* o la LIBRERIA => restituisco errore in chiamata
004000030403     C                   IF        QUSEI <> *BLANKS
004100030403     C                   EVAL      RFDERR = QUSEI
004200030403     C                   ELSE
004300030403     C                   EVAL      RFDHDR = QDBQ25
004400030403     C                   ENDIF
004500020618
004600020618     C                   EVAL      *INLR = *ON
