000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500050302      *       AGGIORNA ORDINAMENTO IN TARIFFE                         *
000600031105      *                                                               *
000700031105      *****************************************************************
000800050302     FTITAD00L  UF   E           K DISK    usropn
000900050302     FTITPD01L  UF   E           K DISK    usropn
001000050302     FTIOFD00L  UF   E           K DISK    usropn  prefix(o)
001100050302     F                                     rename(TITAD000:TIOFD000)
001200050302     FTIOPD01L  UF   E           K DISK    usropn  prefix(o)
001300050302     F                                     rename(TITPD000:TIOPD000)
001400031105
001500050302      *
001600050302      * ELABORO TIOFD
001700050302     c                   OPEN      TIOFD00L
001800050302     c     *loval        setll     TIOFD00L
001900050302     c                   read      TIOFD000
002000050302     c                   DOW       not %EOF(TIOFD00L)
002100050317    1C                   IF        %subst(otadorp:3:1) = 'W'
002200050317     c                   select
002300050317    2c                   when      %subst(otadorp:4:1) = '1'
002400050317     C                   EVAL      %subst(otadorp:4:1) = 'A'
002500050302     c                   UPDATE    tiOFD000
002600050317    2c                   when      %subst(otadorp:4:1) = '2'
002700050317     C                   EVAL      %subst(otadorp:4:1) = 'B'
002800050317     c                   UPDATE    tiOFD000
002900050317    2c                   when      %subst(otadorp:4:1) = '3'
003000050317     C                   EVAL      %subst(otadorp:4:1) = 'C'
003100050317     c                   UPDATE    tiOFD000
003200050317    2c                   when      %subst(otadorp:4:1) = '4'
003300050317     C                   EVAL      %subst(otadorp:4:1) = 'D'
003400050317     c                   UPDATE    tiOFD000
003500050317    2c                   when      %subst(otadorp:4:1) = '5'
003600050317     C                   EVAL      %subst(otadorp:4:1) = 'E'
003700050317     c                   UPDATE    tiOFD000
003800050317    2c                   when      %subst(otadorp:4:1) = '6'
003900050317     C                   EVAL      %subst(otadorp:4:1) = 'F'
004000050317     c                   UPDATE    tiOFD000
004100050317    2c                   when      %subst(otadorp:4:1) = '7'
004200050317     C                   EVAL      %subst(otadorp:4:1) = 'G'
004300050317     c                   UPDATE    tiOFD000
004400050317    2c                   when      %subst(otadorp:4:1) = '8'
004500050317     C                   EVAL      %subst(otadorp:4:1) = 'H'
004600050317     c                   UPDATE    tiOFD000
004700050317    1c                   Endsl
004800050317    2c                   ENDIF
004900050302     c                   read      tiOFD000
005000050302     c                   ENDDO
005100050302     c                   CLOSE     TIOFD00L
005200050302      *
005300050302      * ELABORO TIOPD
005400050302     c                   OPEN      TIOPD01L
005500050302     c     *loval        setll     TIOPD01L
005600050302     c                   read      tiopd000
005700050302     c                   DOW       not %EOF(TIOPD01L)
005800050317    1C                   IF        %subst(otpdorp:3:1) = 'W'
005900050317     c                   select
006000050317    2c                   when      %subst(otpdorp:4:1) = '1'
006100050317     C                   EVAL      %subst(otpdorp:4:1) = 'A'
006200050302     c                   UPDATE    tiopd000
006300050317    2c                   when      %subst(otpdorp:4:1) = '2'
006400050317     C                   EVAL      %subst(otpdorp:4:1) = 'B'
006500050317     c                   UPDATE    tiopd000
006600050317    2c                   when      %subst(otpdorp:4:1) = '3'
006700050317     C                   EVAL      %subst(otpdorp:4:1) = 'C'
006800050317     c                   UPDATE    tiopd000
006900050317    2c                   when      %subst(otpdorp:4:1) = '4'
007000050317     C                   EVAL      %subst(otpdorp:4:1) = 'D'
007100050317     c                   UPDATE    tiopd000
007200050317    2c                   when      %subst(otpdorp:4:1) = '5'
007300050317     C                   EVAL      %subst(otpdorp:4:1) = 'E'
007400050317     c                   UPDATE    tiopd000
007500050317    2c                   when      %subst(otpdorp:4:1) = '6'
007600050317     C                   EVAL      %subst(otpdorp:4:1) = 'F'
007700050317     c                   UPDATE    tiopd000
007800050317    2c                   when      %subst(otpdorp:4:1) = '7'
007900050317     C                   EVAL      %subst(otpdorp:4:1) = 'G'
008000050317     c                   UPDATE    tiopd000
008100050317    2c                   when      %subst(otpdorp:4:1) = '8'
008200050317     C                   EVAL      %subst(otpdorp:4:1) = 'H'
008300050317     c                   UPDATE    tiopd000
008400050317    2c                   endsl
008500050317    1c                   ENDIF
008600050302     c                   read      tiopd000
008700050302     c                   ENDDO
008800050302     c                   CLOSE     TIOPD01L
008900050302
009000031105      *
009100050302      * ELABORO TITAD
009200050302     c                   OPEN      TITAD00L
009300050302     c     *loval        setll     TITAD00L
009400050302     c                   read      titad000
009500050302     c                   DOW       not %EOF(TITAD00L)
009600050317    1C                   IF        %subst(tadorp:3:1) = 'W'
009700050317     c                   select
009800050317    2c                   when      %subst(tadorp:4:1) = '1'
009900050317     C                   EVAL      %subst(tadorp:4:1) = 'A'
010000050302     c                   UPDATE    titad000
010100050317    2c                   when      %subst(tadorp:4:1) = '2'
010200050317     C                   EVAL      %subst(tadorp:4:1) = 'B'
010300050317     c                   UPDATE    titad000
010400050317    2c                   when      %subst(tadorp:4:1) = '3'
010500050317     C                   EVAL      %subst(tadorp:4:1) = 'C'
010600050317     c                   UPDATE    titad000
010700050317    2c                   when      %subst(tadorp:4:1) = '4'
010800050317     C                   EVAL      %subst(tadorp:4:1) = 'D'
010900050317     c                   UPDATE    titad000
011000050317    2c                   when      %subst(tadorp:4:1) = '5'
011100050317     C                   EVAL      %subst(tadorp:4:1) = 'E'
011200050317     c                   UPDATE    titad000
011300050317    2c                   when      %subst(tadorp:4:1) = '6'
011400050317     C                   EVAL      %subst(tadorp:4:1) = 'F'
011500050317     c                   UPDATE    titad000
011600050317    2c                   when      %subst(tadorp:4:1) = '7'
011700050317     C                   EVAL      %subst(tadorp:4:1) = 'G'
011800050317     c                   UPDATE    titad000
011900050317    2c                   when      %subst(tadorp:4:1) = '8'
012000050317     C                   EVAL      %subst(tadorp:4:1) = 'H'
012100050317     c                   UPDATE    titad000
012200050317    2c                   endsl
012300050317    1c                   ENDIF
012400050302     c                   read      titad000
012500050302     c                   ENDDO
012600050302     c                   CLOSE     TITAD00L
012700050302      *
012800050302      * ELABORO TITPD
012900050302     c                   OPEN      TITPD01L
013000050302     c     *loval        setll     TITPD000
013100050302     c                   read      titpd000
013200050302     c                   DOW       not %EOF(TITPD01L)
013300050317    1C                   IF        %subst(tpdorp:3:1) = 'W'
013400050317     c                   select
013500050317    2c                   when      %subst(tpdorp:4:1) = '1'
013600050317     C                   EVAL      %subst(tpdorp:4:1) = 'A'
013700050302     c                   UPDATE    titpd000
013800050317    2c                   when      %subst(tpdorp:4:1) = '2'
013900050317     C                   EVAL      %subst(tpdorp:4:1) = 'B'
014000050317     c                   UPDATE    titpd000
014100050317    2c                   when      %subst(tpdorp:4:1) = '3'
014200050317     C                   EVAL      %subst(tpdorp:4:1) = 'C'
014300050317     c                   UPDATE    titpd000
014400050317    2c                   when      %subst(tpdorp:4:1) = '4'
014500050317     C                   EVAL      %subst(tpdorp:4:1) = 'D'
014600050317     c                   UPDATE    titpd000
014700050317    2c                   when      %subst(tpdorp:4:1) = '5'
014800050317     C                   EVAL      %subst(tpdorp:4:1) = 'E'
014900050317     c                   UPDATE    titpd000
015000050317    2c                   when      %subst(tpdorp:4:1) = '6'
015100050317     C                   EVAL      %subst(tpdorp:4:1) = 'F'
015200050317     c                   UPDATE    titpd000
015300050317    2c                   when      %subst(tpdorp:4:1) = '7'
015400050317     C                   EVAL      %subst(tpdorp:4:1) = 'G'
015500050317     c                   UPDATE    titpd000
015600050317    2c                   when      %subst(tpdorp:4:1) = '8'
015700050317     C                   EVAL      %subst(tpdorp:4:1) = 'H'
015800050317     c                   UPDATE    titpd000
015900050317    2c                   endsl
016000050317    1c                   ENDIF
016100050302     c                   read      titpd000
016200050302     c                   ENDDO
016300050302     c                   CLOSE     TITPD01L
016400050302
016500050302     C                   EVAL      *inlr = *on
