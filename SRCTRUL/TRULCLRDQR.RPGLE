000100020516     H*****************************************************************
000200020517
000300020517     D/COPY QSYSINC/QRPGLESRC,QUSEC
000400020516
000500930617     C     *ENTRY        PLIST
000600930617     C                   PARM                    DATAQ            10
000700930617     C                   PARM                    LIB              10
000800940328     C*
000900940328     C* RICEVE RECORD DA DATAQ
001000930617     C                   EXSR      RCVDTA
001100940328     C*
001200930617     C                   SETON                                        LR
001300940328     C*-------------------------------------------------------------------
001400940328     C*? RCVDTA - RICEVE RECORD DALLA DATAQ
001500940328     C*-------------------------------------------------------------------
001600930617     C     RCVDTA        BEGSR
001700020517     C*
001800020517     C                   EVAL      QUSBPRV = %size(QUSEC)
001900940328     C*
002000940328     C* RICEVE RECORD DALLA DATAQ
002100020517     C                   CALL(e)   'QRCVDTAQ'
002200930617     C                   PARM                    DATAQ
002300930617     C                   PARM                    LIB
002400050526     C                   PARM      0             LEN               5 0
002500050526     C                   PARM                    RCVDATA       32000
002600020516     C                   PARM      0             WAIT              5 0
002700020517     C                   PARM      *blanks       KEYORD            2
002800020517     C                   PARM      *zeros        LENKEYDATA        3 0
002900020517     C                   PARM      *blanks       KEYDATA           1
003000020517     C                   PARM      *zeros        LENSNDINFO        3 0
003100020517     C                   PARM                    SNDINFO          36
003200050526     C                   PARM      '*YES'        RMVMSG           10
003300050526     C                   PARM      0             LENDATARCV        5 0
003400020517     C                   PARM                    QUSEC
003500020516     C*
003600020516     C                   ENDSR
