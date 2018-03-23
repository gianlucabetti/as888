000100980402     H DECEDIT('0,') DATEDIT(*DMY.)
000200980402     F* LANCIO TEST ETICHETTE
000300010725     D DSBLb         E DS
000400010802     D  §TC                  112    201  0
000500010802     D                                     DIM(30)
000600010802     C     *ENTRY        PLIST
000700010802     C                   PARM                    nompgm           10
000800010802     C* INSERISCO DATI PROVA
000900010802     C                   CLEAR                   DSBLb
001000010802     C                   Z-ADD     12345         §LBNFV
001100010802     C                   Z-ADD     1234567       §LBNSP
001200010802     C                   Z-ADD     160591        §LBDAT
001300010802     C                   Z-ADD     12345         §LBNBR
001400010802     C                   MOVEL     'ROSSI MA'    §LBCON
001500010802     C                   MOVE      'RIO    '     §LBCON
001600010802     C                   MOVEL     'FRANCESC'    §LBVET
001700010802     C                   MOVE      'HELLI  '     §LBVET
001800010802     C                   MOVE      '001'         §LBLNP
001900010802     C                   MOVE      *BLANKS       §LBDLP
002000010802     C                   MOVEL     'BOLOGNA'     §LBDLP
002100010802     C                   MOVE      '078'         §LBLNA
002200010802     C                   MOVE      *BLANKS       §LBDLA
002300010802     C                   MOVEL     'SALERNO'     §LBDLA
002400010802     C                   MOVE      *BLANKS       §LBSCA
002500010802     C                   MOVEL     'NAPOLI'      §LBSCA
002600010802     C                   MOVE      *BLANKS       §LBINO
002700010802     C                   MOVEL     'ROMA   '     §LBINO
002800010802     C                   MOVEL     'X'           §LBARR
002900010802     C                   MOVE      999           §TC
003000010802     C                   MOVE      001           §TC(1)
003100010802     C                   MOVE      028           §TC(2)
003200010802     C                   MOVE      063           §TC(3)
003300010802     C                   MOVE      067           §TC(4)
003400010802     C                   MOVE      015           §TC(5)
003500010802     C                   MOVE      031           §TC(6)
003600010802     C                   MOVE      999           §TC(14)
003700010802     C                   MOVE      999           §TC(22)
003800010802     C                   MOVE      999           §TC(30)
003900010802      *
004000990323     C                   DO        1
004100010802     C                   CALL      nompgm
004200010731     C                   PARM                    DSBLb
004300990316     C                   ENDDO
004400980210     C                   SETON                                        LR
004500980402     C*
