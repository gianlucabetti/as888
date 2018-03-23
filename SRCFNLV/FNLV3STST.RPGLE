000100980402     H DECEDIT('0,') DATEDIT(*DMY.)
000200980402     F* LANCIO TEST ETICHETTE
000300010725     D DSBLb         E DS
000400010802      *
000500010802     C     *ENTRY        PLIST
000600010802     C                   PARM                    nompgm           10
000700010802     C* INSERISCO DATI PROVA
000800010802     C                   CLEAR                   DSBLb
000900010802     C                   Z-ADD     12345         §LBNFV
001000010802     C                   Z-ADD     1234567       §LBNSP
001100010802     C                   Z-ADD     160591        §LBDAT
001200010802     C                   Z-ADD     12345         §LBNBR
001300010802     C                   MOVEL     'ROSSI MA'    §LBCON
001400010802     C                   MOVE      'RIO    '     §LBCON
001500010802     C                   MOVEL     'FRANCESC'    §LBVET
001600010802     C                   MOVE      'HELLI  '     §LBVET
001700010802     C                   MOVE      '001'         §LBLNP
001800010802     C                   MOVE      *BLANKS       §LBDLP
001900010802     C                   MOVEL     'BOLOGNA'     §LBDLP
002000010802     C                   MOVE      '078'         §LBLNA
002100010802     C                   MOVE      *BLANKS       §LBDLA
002200010802     C                   MOVEL     'SALERNO'     §LBDLA
002300010802     C                   MOVE      *BLANKS       §LBSCA
002400010802     C                   MOVEL     'NAPOLI'      §LBSCA
002500010802     C                   MOVE      *BLANKS       §LBINO
002600010802     C                   MOVEL     'ROMA   '     §LBINO
002700010802     C                   MOVEL     'X'           §LBARR
002800990323     C                   DO        1
002900010802     C                   CALL      nompgm
003000010731     C                   PARM                    DSBLb
003100990316     C                   ENDDO
003200980210     C                   SETON                                        LR
003300980402     C*
