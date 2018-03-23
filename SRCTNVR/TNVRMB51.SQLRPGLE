000100080520     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200040511
000300040511      *------------------------------------------------------------------------*
000400131001      * Elabora ORM immessi 30 minuti
000500040511      *------------------------------------------------------------------------*
000600080710
000700080710     fwfstat1   uf a e           k disk
000800040511
000900040511      *------------------------------------------------------------------------*
001000040511      *   S C H I E R E
001100040511      *------------------------------------------------------------------------*
001200040511
001300040511      *------------------------------------------------------------------------*
001400040511      *   V A R I A B I L I
001500040511      *------------------------------------------------------------------------*
001600080710     d kwforad         s                   like(wformoaod)
001700080710     d kwforaa         s                   like(wformoaoa)
001800040511
001900040511      *------------------------------------------------------------------------*
002000040511      *   D S   I N T E R N E / E S T E R N E
002100040511      *------------------------------------------------------------------------*
002200080710     d fnormds       e ds                  extname(fnorm00f)
002300080520     d kpjba         e ds
002400080523
002500080523     d parm            ds
002600080523     d  datada                 1      8  0
002700080710     d  dataal                 9     16  0
002800040511
002900040511      *------------------------------------------------------------------------*
003000040511      *  RIEPILOGO INDICATORI
003100040511      *------------------------------------------------------------------------*
003200040511
003300040511      *------------------------------------------------------------------------*
003400080520
003500080520     c     *entry        plist
003600080520     c                   parm                    kpjba
003700080523
003800080523     c                   eval      parm = kpjbu
003900080710
004000080710     c     kwfstat       klist
004100080710     c                   kfld                    ormpoe
004200080710     c                   kfld                    ormdao
004300080710     c                   kfld                    kwforad
004400050428
004500080710     c/EXEC SQL
004600080710     c+ DECLARE B1 CURSOR FOR SELECT fnorm00f.*
004700080710     c+ FROM fnorm00f
004800080710     c+ WHERE ormdao between :datada and :dataal
004900090609     c+ and ormpoe in(028, 059)
005000080710     c+ order by ormdao, ormoao, ormpoe
005100080710     c/END-EXEC
005200080520
005300080710     c/EXEC SQL
005400080710     c+ OPEN B1
005500080710     c/END-EXEC
005600080520
005700080520     c                   do        *hival
005800080520
005900080710     c/EXEC SQL
006000080710     c+ FETCH NEXT FROM B1 INTO :fnormds
006100080710     c/END-EXEC
006200080520
006300080520     c                   if        sqlcod = 100 or
006400080520     c                             sqlcod < 0
006500080520     c                   leave
006600080520     c                   endif
006700080520
006800080710     c                   exsr      sr_elabora
006900080520
007000080520     c                   enddo
007100080520
007200080710     c/EXEC SQL
007300080710     c+ CLOSE B1
007400080710     c/END-EXEC
007500080520
007600080520     c                   eval      *inlr = *on
007700080520
007800080520      *------------------------------------------------------------------------*
007900080710      *? elabora
008000080520      *------------------------------------------------------------------------*
008100080710     c     sr_elabora    begsr
008200080520
008300080710     c                   select
008400080710     c                   when      ormoao >= 000100 and ormoao < 003100
008500080710     c                   eval      kwforad = 000100
008600080710     c                   eval      kwforaa = 003000
008700080710     c                   when      ormoao >= 003100 and ormoao < 010100
008800080710     c                   eval      kwforad = 003100
008900080710     c                   eval      kwforaa = 010000
009000080710     c                   when      ormoao >= 010100 and ormoao < 013100
009100080710     c                   eval      kwforad = 010100
009200080710     c                   eval      kwforaa = 013000
009300080710     c                   when      ormoao >= 013100 and ormoao < 020100
009400080710     c                   eval      kwforad = 013100
009500080710     c                   eval      kwforaa = 020000
009600080710     c                   when      ormoao >= 020100 and ormoao < 023100
009700080710     c                   eval      kwforad = 020100
009800080710     c                   eval      kwforaa = 023000
009900080710     c                   when      ormoao >= 023100 and ormoao < 030100
010000080710     c                   eval      kwforad = 023100
010100080710     c                   eval      kwforaa = 030000
010200080710     c                   when      ormoao >= 030100 and ormoao < 033100
010300080710     c                   eval      kwforad = 030100
010400080710     c                   eval      kwforaa = 033000
010500080710     c                   when      ormoao >= 033100 and ormoao < 040100
010600080710     c                   eval      kwforad = 033100
010700080710     c                   eval      kwforaa = 040000
010800080710     c                   when      ormoao >= 040100 and ormoao < 043100
010900080710     c                   eval      kwforad = 040100
011000080710     c                   eval      kwforaa = 043000
011100080710     c                   when      ormoao >= 043100 and ormoao < 050100
011200080710     c                   eval      kwforad = 043100
011300080710     c                   eval      kwforaa = 050000
011400080710     c                   when      ormoao >= 050100 and ormoao < 053100
011500080710     c                   eval      kwforad = 050100
011600080710     c                   eval      kwforaa = 053000
011700080710     c                   when      ormoao >= 053100 and ormoao < 060100
011800080710     c                   eval      kwforad = 053100
011900080710     c                   eval      kwforaa = 060000
012000080710     c                   when      ormoao >= 060100 and ormoao < 063100
012100080710     c                   eval      kwforad = 060100
012200080710     c                   eval      kwforaa = 063000
012300080710     c                   when      ormoao >= 063100 and ormoao < 070100
012400080710     c                   eval      kwforad = 063100
012500080710     c                   eval      kwforaa = 070000
012600080710     c                   when      ormoao >= 070100 and ormoao < 073100
012700080710     c                   eval      kwforad = 070100
012800080710     c                   eval      kwforaa = 073000
012900080710     c                   when      ormoao >= 073100 and ormoao < 080100
013000080710     c                   eval      kwforad = 073100
013100080710     c                   eval      kwforaa = 080000
013200080710     c                   when      ormoao >= 080100 and ormoao < 083100
013300080710     c                   eval      kwforad = 080100
013400080710     c                   eval      kwforaa = 083000
013500080710     c                   when      ormoao >= 083100 and ormoao < 090100
013600080710     c                   eval      kwforad = 083100
013700080710     c                   eval      kwforaa = 090000
013800080710     c                   when      ormoao >= 090100 and ormoao < 093100
013900080710     c                   eval      kwforad = 090100
014000080710     c                   eval      kwforaa = 093000
014100080710     c                   when      ormoao >= 093100 and ormoao < 100100
014200080710     c                   eval      kwforad = 093100
014300080710     c                   eval      kwforaa = 100000
014400080710     c                   when      ormoao >= 100100 and ormoao < 103100
014500080710     c                   eval      kwforad = 100100
014600080710     c                   eval      kwforaa = 103000
014700080710     c                   when      ormoao >= 103100 and ormoao < 110100
014800080710     c                   eval      kwforad = 103100
014900080710     c                   eval      kwforaa = 110000
015000080710     c                   when      ormoao >= 110100 and ormoao < 113100
015100080710     c                   eval      kwforad = 110100
015200080710     c                   eval      kwforaa = 113000
015300080710     c                   when      ormoao >= 113100 and ormoao < 120100
015400080710     c                   eval      kwforad = 113100
015500080710     c                   eval      kwforaa = 120000
015600080710     c                   when      ormoao >= 120100 and ormoao < 123100
015700080710     c                   eval      kwforad = 120100
015800080710     c                   eval      kwforaa = 123000
015900080710     c                   when      ormoao >= 123100 and ormoao < 130100
016000080710     c                   eval      kwforad = 123100
016100080710     c                   eval      kwforaa = 130000
016200080710     c                   when      ormoao >= 130100 and ormoao < 133100
016300080710     c                   eval      kwforad = 130100
016400080710     c                   eval      kwforaa = 133000
016500080710     c                   when      ormoao >= 133100 and ormoao < 140100
016600080710     c                   eval      kwforad = 133100
016700080710     c                   eval      kwforaa = 140000
016800080710     c                   when      ormoao >= 140100 and ormoao < 143100
016900080710     c                   eval      kwforad = 140100
017000080710     c                   eval      kwforaa = 143000
017100080710     c                   when      ormoao >= 143100 and ormoao < 150100
017200080710     c                   eval      kwforad = 143100
017300080710     c                   eval      kwforaa = 150000
017400080710     c                   when      ormoao >= 150100 and ormoao < 153100
017500080710     c                   eval      kwforad = 150100
017600080710     c                   eval      kwforaa = 153000
017700080710     c                   when      ormoao >= 153100 and ormoao < 160100
017800080710     c                   eval      kwforad = 153100
017900080710     c                   eval      kwforaa = 160000
018000080710     c                   when      ormoao >= 160100 and ormoao < 163100
018100080710     c                   eval      kwforad = 160100
018200080710     c                   eval      kwforaa = 163000
018300080710     c                   when      ormoao >= 163100 and ormoao < 170100
018400080710     c                   eval      kwforad = 163100
018500080710     c                   eval      kwforaa = 170000
018600080710     c                   when      ormoao >= 170100 and ormoao < 173100
018700080710     c                   eval      kwforad = 170100
018800080710     c                   eval      kwforaa = 173000
018900080710     c                   when      ormoao >= 173100 and ormoao < 180100
019000080710     c                   eval      kwforad = 173100
019100080710     c                   eval      kwforaa = 180000
019200080710     c                   when      ormoao >= 180100 and ormoao < 183100
019300080710     c                   eval      kwforad = 180100
019400080710     c                   eval      kwforaa = 183000
019500080710     c                   when      ormoao >= 183100 and ormoao < 190100
019600080710     c                   eval      kwforad = 183100
019700080710     c                   eval      kwforaa = 190000
019800080710     c                   when      ormoao >= 190100 and ormoao < 193100
019900080710     c                   eval      kwforad = 190100
020000080710     c                   eval      kwforaa = 193000
020100080710     c                   when      ormoao >= 193100 and ormoao < 200100
020200080710     c                   eval      kwforad = 193100
020300080710     c                   eval      kwforaa = 200000
020400080710     c                   when      ormoao >= 200100 and ormoao < 203100
020500080710     c                   eval      kwforad = 200100
020600080710     c                   eval      kwforaa = 203000
020700080710     c                   when      ormoao >= 203100 and ormoao < 210100
020800080710     c                   eval      kwforad = 203100
020900080710     c                   eval      kwforaa = 210000
021000080710     c                   when      ormoao >= 210100 and ormoao < 213100
021100080710     c                   eval      kwforad = 210100
021200080710     c                   eval      kwforaa = 213000
021300080710     c                   when      ormoao >= 213100 and ormoao < 220100
021400080710     c                   eval      kwforad = 213100
021500080710     c                   eval      kwforaa = 220000
021600080710     c                   when      ormoao >= 220100 and ormoao < 223100
021700080710     c                   eval      kwforad = 220100
021800080710     c                   eval      kwforaa = 223000
021900080710     c                   when      ormoao >= 223100 and ormoao < 230100
022000080710     c                   eval      kwforad = 223100
022100080710     c                   eval      kwforaa = 230000
022200080710     c                   when      ormoao >= 230100 and ormoao < 233100
022300080710     c                   eval      kwforad = 230100
022400080710     c                   eval      kwforaa = 233000
022500080710     c                   when      ormoao >= 233100 and ormoao < 000100
022600080710     c                   eval      kwforad = 233100
022700080710     c                   eval      kwforaa = 000000
022800080710     c                   endsl
022900080710
023000080710     c     kwfstat       chain     wfstat1
023100080710     c                   if        %found (wfstat1)
023200080710     c                   exsr      sr_aggiorna
023300080710     c                   else
023400080710     c                   exsr      sr_scrive
023500080710     c                   endif
023600080520
023700080520     c                   endsr
023800080710
023900080710      *------------------------------------------------------------------------*
024000080710      *? aggiorna
024100080710      *------------------------------------------------------------------------*
024200080710     c     sr_aggiorna   begsr
024300080710
024400080710     c                   select
024500080710     c                   when      ormtco = 'M'
024600080710     c                   add       1             wformtel
024700080710     c                   when      ormtco = 'I'
024800080710     c                   add       1             wformint
024900080710     c                   when      ormtco = 'F'
025000080710     c                   add       1             wformfile
025100080710     c                   when      ormtco = 'S'
025200080710     c                   add       1             wformfis
025300080710     c                   endsl
025400080710
025500080710     c                   update    wfstatorm
025600080710
025700080710     c                   endsr
025800080710
025900080710      *------------------------------------------------------------------------*
026000080710      *? scrive
026100080710      *------------------------------------------------------------------------*
026200080710     c     sr_scrive     begsr
026300080710
026400080710     c                   clear                   wfstatorm
026500080710
026600080710     c                   eval      wformpoe = ormpoe
026700080710     c                   eval      wformdao = ormdao
026800080710     c                   eval      wformoaod = kwforad
026900080710     c                   eval      wformoaoa = kwforaa
027000080710
027100080710     c                   select
027200080710     c                   when      ormtco = 'M'
027300080710     c                   add       1             wformtel
027400080710     c                   when      ormtco = 'I'
027500080710     c                   add       1             wformint
027600080710     c                   when      ormtco = 'F'
027700080710     c                   add       1             wformfile
027800080710     c                   when      ormtco = 'S'
027900080710     c                   add       1             wformfis
028000080710     c                   endsl
028100080710
028200080710     c                   write     wfstatorm
028300080710
028400080710     c                   endsr
