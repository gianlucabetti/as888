000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050201     ftitas30c  if   e           k disk
000400050201     FSYSPRT    O    F  132        PRINTER OFLIND(*INOF)
000500050124      *
000600050202     dfigcpds        e ds                  extname(figcp00f)
000700050202     dbollads          ds
000800050202     d aas                                 like(gcpaas)
000900050202     d lnp                                 like(gcplnp)
001000050202     d nrs                                 like(gcpnrs)
001100050202     d nsp                                 like(gcpnsp)
001200050124     d
001300000607      *--------------------------------------------------------------------
001400050202     c* GIACENZA IN PARTENZA
001500050202      *--------------------------------------------------------------------
001600050202     C/EXEC SQL
001700050203     C+ DECLARE A1 CURSOR FOR SELECT * FROM FILTRA201/figcp00f exception
001800050203     C+ join FILTRA201/FNBLP00F ON gcpaas=blpaas and gcplnp=blplnp and
001900050202     C+ gcpnrs=blpnrs and gcpnsp=blpnsp
002000050202     C/END-EXEC
002100050202
002200050202     C/EXEC SQL
002300050202     C+ OPEN A1
002400050202     C/END-EXEC
002500050202
002600050202     C                   do        *hival
002700050202
002800050202     C/EXEC SQL
002900050202     C+ FETCH NEXT FROM A1 INTO :figcpds
003000050202     C/END-EXEC
003100050202
003200050202     C                   select
003300050202     C                   when      sqlcod = 100
003400050202     c                   leave
003500050202     C                   when      sqlcod = 0
003600050202     c                   seton                                        20
003700050202     C                   exsr      stampa
003800050202     C                   other
003900050202     C                   exsr      sqlerror
004000050202     C                   endsl
004100050202     C*
004200050202     C                   enddo
004300050202
004400050202     C/EXEC SQL
004500050202     C+ CLOSE A1
004600050202     C/END-EXEC
004700050202      *--------------------------------------------------------------------
004800050202     c* GIACENZA IN ARRIVO
004900050202      *--------------------------------------------------------------------
005000050202     C/EXEC SQL
005100050203     C+ DECLARE B1 CURSOR FOR SELECT * FROM FILTRA201/figca00f exception
005200050203     C+ join FILTRA201/fnarb00f on gcpaas=arbaas and gcplnp=arblnp and
005300050202     C+ gcpnrs=arbnrs and gcpnsp=arbnsp
005400050202     C/END-EXEC
005500050202
005600050202     C/EXEC SQL
005700050202     C+ OPEN B1
005800050202     C/END-EXEC
005900050202
006000050202     C                   do        *hival
006100050202
006200050202     C/EXEC SQL
006300050202     C+ FETCH NEXT FROM B1 INTO :figcpds
006400050202     C/END-EXEC
006500050202
006600050202     C                   select
006700050202     C                   when      sqlcod = 100
006800050202     c                   leave
006900050202     C                   when      sqlcod = 0
007000050202     c                   seton                                        21
007100050202     C                   exsr      stampa
007200050202     C                   other
007300050202     C                   exsr      sqlerror
007400050202     C                   endsl
007500050202     C*
007600050202     C                   enddo
007700050202
007800050202     C/EXEC SQL
007900050202     C+ CLOSE B1
008000050202     C/END-EXEC
008100050202      *--------------------------------------------------------------------
008200050202     c* GIACENZA IN SEDE
008300050202      *--------------------------------------------------------------------
008400050202     C/EXEC SQL
008500050202     C+ DECLARE C1 CURSOR FOR SELECT GCPAAS, GCPLNP, GCPNRS, GCPNSP FROM
008600050203     C+ GAITRAGRU/figcp00f GROUP BY GCPAAS, GCPLNP, GCPNRS, GCPNSP ORDER
008700050202     C+ BY GCPAAS, GCPLNP, GCPNRS, GCPNSP
008800050202     C/END-EXEC
008900050202
009000050202     C/EXEC SQL
009100050202     C+ OPEN c1
009200050202     C/END-EXEC
009300050202
009400050202     C                   do        *hival
009500050202
009600050202     C/EXEC SQL
009700050202     C+ FETCH NEXT FROM c1 INTO :bollads
009800050202     C/END-EXEC
009900050202
010000050202     C                   select
010100050202     C                   when      sqlcod = 100
010200050202     c                   leave
010300050202     C                   when      sqlcod = 0
010400050202     c* verifico se ho la bolla in sede
010500050202     c     ktitas        setll     titas30c                               10
010600050202     c                   if        not *in10
010700050202     c                   seton                                        22
010800050202     c                   eval      gcpaas = aas
010900050202     c                   eval      gcplnp = lnp
011000050202     c                   eval      gcpnrs = nrs
011100050202     c                   eval      gcpnsp = nsp
011200050202     C                   exsr      stampa
011300050202     c                   end
011400050202     C                   other
011500050202     C                   exsr      sqlerror
011600050202     C                   endsl
011700050202     C*
011800050202     C                   enddo
011900050202
012000050202     C/EXEC SQL
012100050202     C+ CLOSE C1
012200050202     C/END-EXEC
012300000607
012400000607     c                   seton                                        lr
012500040510     c
012600050202      *-----------------------------------------------
012700050202     C     sqlerror      begsr
012800050202      *-----------------------------------------------
012900050202     C*
013000050202     C                   call      'X66CHGJOB'
013100050202     C                   seton                                        lr
013200050202     C*
013300050202     C                   endsr
013400050114      *-------------------------------------------------
013500050114     c     *inzsr        begsr
013600050114      *-------------------------------------------------
013700050117
013800050201     c     ktitas        klist
013900050202     c                   kfld                    aas
014000050202     c                   kfld                    lnp
014100050202     c                   kfld                    nrs
014200050202     c                   kfld                    nsp
014300050201     c                   seton                                        of
014400050114     c                   endsr
014500050201      *-------------------------------------------------
014600050201     c     stampa        begsr
014700050201      *-------------------------------------------------
014800050201     c   of              except    testa
014900050201     c                   eval      *inof = '0'
015000050201     c                   except    riga
015100050202     c                   setoff                                       202122
015200050201     c                   endsr
015300050201     OSYSPRT    E            TESTa            01
015400050202     o                                           10 'TNVRTGCPR3'
015500050202     O                                           65 'PRECONTROLLO CONGRUENZA'
015600050202     O                                           +1 'GIACENZE CON BOLLE'
015700050201     O                       PAGE1         Z    132
015800050201     O          E            RIGA       01
015900050202     O                                           22 'Spedizione presente in'
016000050202     O               20                          39 'FIGCP00F filiale'
016100050202     O               21                          39 'FIGCA00F filiale'
016200050202     O               22                          39 'FIGCP00F sede'
016300050202     O                                           +1 'ma non in'
016400050202     O               20                          58 'FNBLP00F'
016500050202     O               21                          58 'FNARB00F'
016600050202     O               22                          58 'TITAS30C'
016700050201     O                       gcpaas              +1
016800050201     O                       gcplnp              +1
016900050201     O                       gcpnrs        z     +1
017000050201     O                       gcpnsp        z     +1
