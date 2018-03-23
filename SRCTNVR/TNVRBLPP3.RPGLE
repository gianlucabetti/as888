000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300080401     fFNarb01l  iF   E           K DISK
000400080401     fFNblp01l  iF   E           K DISK
000500080212     ftitas30c  IF   E           K DISK
000600080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700080320     d
000800980611      * ---------------------------------------------------------
000900080408     c     *entry        plist
001000080408     c                   parm                    dataspe           4
001100080408     c
001200080212     C     Kblp          KLIST
001300080401     C                   KFLD                    arbAAS
001400080401     C                   KFLD                    arbLNP
001500080401     C                   KFLD                    arbNRS
001600080401     C                   KFLD                    arbNSP
001700041222     C
001800080212     c                   clear                   kaas              4 0
001900080320     c                   clear                   x                 4 0
002000080320     c                   clear                   y                 4 0
002100080408     c                   if        %parms=1 and dataspe>*zeros
002200080408     c                   movel     dataspe       wspe              4 0
002300080408     c                   else
002400080408     c                   clear                   wspe
002500080408     c                   endif
002600080212
002700080212     C                   EVAL      KAAS=2008
002800041222     C
002900080320     c     kaas          SETLL     fnarb000                           30
003000080401     c     kaas          READe     fnarb01l
003100041222     C
003200080320    1C                   DOW       NOT %EOF(fnarb01l)
003300080408     c
003400080408     c                   if        (wspe>0 and arbmgs>=wspe) or
003500080408     c                              wspe=0
003600080401
003700080401     c                   clear                   tas               1
003800080401     c                   clear                   blp               1
003900080401     c                   setoff                                       0102
004000080401     c
004100080401     c     kblp          chain     fnblp01l
004200080401     C     Kblp          chain     titas30c
004300080401     c                   if        not %found(fnblp01l)
004400080401     c                   eval      blp='N'
004500080401     c                   endif
004600080401     c                   if        not %found(titas30c)
004700080401     c                   eval      tas='N'
004800080401     c                   endif
004900080401     c
005000080401     c                   select
005100080401     c                   when      blp=' ' and tas=' '
005200080401     c                   if        tasvlc<>arbvlc or arbvlc<>blpvlc
005300080401     c                   seton                                        0102
005400080401     c                   except    stampa3
005500080401     c                   endif
005600080401     c                   when      blp='N' and tas=' '
005700080401     c                   if        arbvlc<>tasvlc
005800080401     c                   seton                                        02
005900080401     c                   except    stampa3
006000080401     c                   endif
006100080401     c                   when      blp=' ' and tas='N'
006200080401     c                   if        arbvlc<>blpvlc
006300080401     c                   seton                                        01
006400080401     c                   except    stampa3
006500080401     c                   endif
006600080401     c                   endsl
006700080401     c
006800080401     c                   setoff                                       0102
006900080401     c                   select
007000080401     c                   when      blp=' ' and tas=' '
007100080401     c                   if        taspkc<>arbpkc or arbpkc<>blppkc
007200080401     c                   seton                                        0102
007300080401     c                   except    stampa2
007400080401     c                   endif
007500080401     c                   when      blp='N' and tas=' '
007600080401     c                   if        arbpkc<>taspkc
007700080401     c                   seton                                        02
007800080401     c                   except    stampa2
007900080401     c                   endif
008000080401     c                   when      blp=' ' and tas='N'
008100080401     c                   if        arbpkc<>blppkc
008200080401     c                   seton                                        01
008300080401     c                   except    stampa2
008400080401     c                   endif
008500080401     c                   endsl
008600080408     c                   endif
008700080320     c
008800080401     c     kaas          READe     fnarb01l
008900080320    1c                   enddo
009000080320     c
009100080212     c                   seton                                        lr
009200080401     OQSYSPRT   E            stampa3     1
009300080401     O                       arbaas           +   1
009400080401     O                       arblnp           +   1
009500080401     O                       arbnrs        z  +   1
009600080401     O                       arbnsp        2  +   1
009700080408     O                       arblna           +   1
009800080402     O                       arbmgs           +   1 '  /  '
009900080401     O                                        +   1 'ARBVLC'
010000080401     O                       arbvlc        4  +   1
010100080401     O                       arbncr        z  +   1
010200080402     O               01                       +   3 'BLPVLC'
010300080401     O               01      blpvlc        4  +   1
010400080401     O               01      blpncr        z  +   1
010500080402     O               02                       +   3 'TASVLC'
010600080401     O               02      TASvlc        4  +   1
010700080401     O               02      TASncr        z  +   1
010800080401     OQSYSPRT   E            stampa2     1
010900080401     O                       arbaas           +   1
011000080401     O                       arblnp           +   1
011100080401     O                       arbnrs        z  +   1
011200080401     O                       arbnsp        2  +   1
011300080408     O                       arblna           +   1
011400080402     O                       arbmgs           +   1 '  /  '
011500080401     O                                        +   1 'ARBPKC'
011600080401     O                       arbpkc        4  +   1
011700080401     O                       arbncp        z  +   1
011800080401     O               01                       +   1 'BLPPKC'
011900080401     O               01      blppkc        4  +   1
012000080401     O               01      blpncp        z  +   1
012100080401     O               02                       +   1 'TASPKC'
012200080401     O               02      TASpkc        4  +   1
012300080401     O               02      TASncp        z  +   1
