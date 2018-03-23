000100980402     H DECEDIT('0,') DATEDIT(*DMY.)
000200980402     F* LANCIO TEST ETICHETTE
000300010726     D dslv25        E DS                  extname(FNLV25DS)
000400010802     C     *ENTRY        PLIST
000500010802     C                   PARM                    nompgm           10
000600010802     C* INSERISCO DATI PROVA
000700010802     C                   CLEAR                   DSlv25
000800010802     C                   Z-ADD     1231234       D25PDR
000900010802     C                   MOVEL     'ROSSI ..'    W25RSC           25
001000010802     C                   MOVE      'MARIO'       W25RSC
001100010802     C                   Z-ADD     67890         D25NBL
001200011203     C                   Z-ADD     39095         D25NFV
001300010802     C                   Z-ADD     11011995      D25DDC
001400010802     C                   MOVE      *BLANK        D25NOT
001500010802     C                   MOVEL     'NO......'    D25NOT
001600010802     C                   MOVE      '......TE'    D25NOT
001700990323     C                   DO        1
001800010802     C                   CALL      nompgm
001900010726     C                   PARM                    DSlv25
002000990316     C                   ENDDO
002100980210     C                   SETON                                        LR
002200980402     C*
