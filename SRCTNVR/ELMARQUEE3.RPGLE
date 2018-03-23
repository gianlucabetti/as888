000100050330     FMarquee3d CF   E             WORKSTN
000200050330     F                                     MAXDEV(*FILE)
000300030219
000400010413     C                   DOU       *INKC
000500030219
000600050330     C                   movel     'I'           Fld001
000700050330     C                   movel     'A'           Fld002
000800030219
000900050330     C                   Write     P1
001000050330     C                   Write     T1
001100050330     C                   Write     T3
001200050330     C                   Write     s4
001300050330     C                   Read(E)   Marquee3d
001400050330     C**                 exfmt     s4
001500050330
001600030219
001700030219     C                   IF        *INKC
001800030219     C                   LEAVE
001900030219     C                   ENDIF
002000050330     c                   enddo
002100030219
002200030219
002300010413     C                   EVAL      *INLR = *ON
