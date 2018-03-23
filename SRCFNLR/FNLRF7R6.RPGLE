000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000700950523     D                 DS
000800950523     D  §CB                    1      6  0
000900950523     D                                     DIM(6)                               CODICE CON CONTR.
001000950523     D  §CA                    1      6
001100950523     D                                     DIM(6)                               CODICE CON CONTR.
001200950523     D                 DS
001300950523     D  §2                     1      2  0
001400950523     D                                     DIM(2)                               COMODO
001500950523     D  §2A                    1      2
001600950523     D                                     DIM(2)                               COMODO
001700950525     D                 DS
001800950525     D  §C                     1      5  0
001900950523     D                                     DIM(5)                               CODICE S/CONTR.
002000950525     D  WNFV                   1      5
002100050426     D fnlr59ds      E DS
002101050426     D kpjba         E DS
002900910521     C     *ENTRY        PLIST
003000050426     C                   PARM                    kpjba
003001050426     c                   movel(p)  kpjbu         fnlr59ds
007100050426     C                   MOVE      D59ndd        WNFV              5
007200950523     C                   EXSR      M10
007300950523     C                   MOVEA     §CA           §C6A              6
007400050426     C                   MOVE      §C6A          d59bcd
007401050426     c                   movel(p)  fnlr59ds      kpjbu
007402050426     c                   seton                                        lr
008900950523     C*****************************************************
009000950523     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
009100950523     C*****************************************************
009200950523     C     M10           BEGSR
009300950523     C                   MOVE      *ZERO         §TT               3 0
009400950523     C                   MOVE      *ZERO         §TT2              3 0
009500950523     C                   MOVE      *ZERO         §1                1 0
009600950523     C                   Z-ADD     6             §S                2 0
009700950523     C                   DO        5             §N                2 0
009800950523     C                   SUB       1             §S
009900950523     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
010000950523     C                   MOVE      §C(§N)        §CB(§N)
010100950523     C     §N            DIV       2             §CTR              3 0
010200950523     C                   MVR                     §RST              3 010
010300950523     C     *IN(10)       IFEQ      '1'
010400950523     C     §C(§S)        MULT      2             §N2               2 0
010500950523     C                   MOVE      §N2           §A2               2
010600950523     C                   MOVEA     §A2           §2A
010700950523     C                   XFOOT     §2            §CTR
010800950523     C                   ELSE
010900950523     C                   Z-ADD     §C(§S)        §CTR
011000950523     C                   END
011100950523     C                   ADD       §CTR          §TT
011200950523     C                   END
011300950523     C*
011400950523     C     §TT           ADD       9             §TT2
011500950523     C                   MOVE      0             §TT2
011600950523     C     §TT2          SUB       §TT           §1
011700950523     C*
011800950523     C                   MOVE      §1            §CB(6)
011900950523     C                   ENDSR
