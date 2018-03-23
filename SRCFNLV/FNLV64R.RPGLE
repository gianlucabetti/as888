000100010125     FWFRCQ12L  UF   E           K DISK
000200990507     FAZORG01L  IF   E           K DISK
000300990507     D KPJBA         E DS
000400990507     C*
000500990507     C     *ENTRY        PLIST
000600990507     C                   PARM                    KPJBA
000700990507     C*
000800990507     C     *LIKE         DEFINE    ORGFIL        SAVFIL
000900990507     C                   MOVE      *ZEROS        SAVFIL
001000990507     C*
001100010125     C     *LOVAL        SETLL     WFRCQ12L
001200010125     C                   READ      WFRCQ12L                               30
001300990507    1C     *IN30         DOWEQ     *OFF
001400990507     C* CHAINO AZORG SOLO A CAMBIO P.O.
001500990507     C     RCQFIL        IFNE      SAVFIL
001600990507     C     RCQFIL        CHAIN     AZORG01L                           31
001700990507     C                   Z-ADD     RCQFIL        SAVFIL
001800990507     C                   END
001900990507     C*
002000990507    3C     *IN31         IFEQ      *OFF
002100990507     C     ORGFL3        ANDNE     RCQDIV
002200990507     C     *IN31         OREQ      *OFF
002300990507     C     ORGCAR        ANDNE     RCQARE
002400990507     C                   MOVEL     ORGFL3        RCQDIV
002500990507     C                   Z-ADD     ORGCAR        RCQARE
002600990507     C                   EXCEPT    AGGRCQ
002700990507    3C                   END
002800010125     C                   READ      WFRCQ12L                               30
002900990507    1C                   ENDDO
003000990507     C                   SETON                                        LR
003100990507     OWFRCQ000  E            AGGRCQ
003200990507     O                       RCQDIV
003300990507     O                       RCQARE
