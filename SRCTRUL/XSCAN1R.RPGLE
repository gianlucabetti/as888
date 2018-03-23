000100011116     H DECEDIT('0,') DATEDIT(*dmy.)  DATFMT(*ISO)
000200011116
000300011116     fWSCAN00F  O  A E             DISK
000400011116
000500011116     d data6           s              6
000600011116     d data60          s              6  0
000700020610     d SCAN£           s              5U 0
000800011116
000900011116     d                 DS
001000011116     d  g1                     1      1
001100011116     d  g2                     2      2
001200011116     d  m1                     4      4
001300011116     d  m2                     5      5
001400011116     d  a1                     7      7
001500011116     d  a2                     8      8
001600011116     d  DATA                   1     10
001700981020
001800011116     c     *entry        plist
001900011116     c                   parm                    DATAUM           10
002000011116     c                   parm                    FILE             10
002100011116     c                   parm                    LIBR             10
002200011116     c                   parm                    MEMBRO           10
002300011116     c                   parm                    TIPOM            10
002400011116     c                   parm                    TESTO            50
002500011116
002600020610      *  Elaboro solo se il membro non contiene "£"
002700020610     c                   eval      SCAN£ = %scan('£':MEMBRO)
002800020610     c                   IF        SCAN£ = *zeros
002900011116      *  DATAUM è una data espressa "gg/mm/aa"
003000011116     c                   movel     dataum        data
003100011116     C                   IF        g1 >= '0' and g2 >= '0' and
003200011116     C                             m1 >= '0' and m2 >= '0' and
003300011116     C                             a1 >= '0' and a2 >= '0'
003400011116     c                   eval      data6 = g1 + g2 + m1+ m2 + a1 + a2
003500011116     c                   movel     data6         data60
003600011116     c     *dmy          movel     data60        WSCDUM
003700011116     c                   ELSE
003800011116     c                   clear                   WSCDUM
003900011116     c                   ENDIF
004000011116
004100011116     c                   time                    WSCDSC
004200011116     c                   movel     FILE          WSCFIL
004300011116     c                   movel     LIBR          WSCLIB
004400011116     c                   movel     MEMBRO        WSCMEM
004500011116     c                   movel     TIPOM         WSCTIP
004600011116     c                   movel     TESTO         WSCTXT
004700011116
004800011116     C                   WRITE     WSCAN000
004900011116
005000020610     c                   ENDIF
005100020610
005200011116     C                   eval      *inLR = *ON
