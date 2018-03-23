000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300080401     fFNarb01l  iF   E           K DISK
000400080401     fFNblp01l  iF   E           K DISK
000500080212     ftitas30c  IF   E           K DISK
000600080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700080320     d
000800980611      * ---------------------------------------------------------
000900080212     C     Kblp          KLIST
001000080401     C                   KFLD                    arbAAS
001100080401     C                   KFLD                    arbLNP
001200080401     C                   KFLD                    arbNRS
001300080401     C                   KFLD                    arbNSP
001400041222     C
001500080212     c                   clear                   kaas              4 0
001600080320     c                   clear                   x                 4 0
001700080320     c                   clear                   y                 4 0
001800080212
001900080212     C                   EVAL      KAAS=2008
002000041222     C
002100080320     c     kaas          SETLL     fnarb000                           30
002200080401     c     kaas          READe     fnarb01l
002300041222     C
002400080320    1C                   DOW       NOT %EOF(fnarb01l)
002500080401
002600080401     c                   clear                   tas               1
002700080401     c                   clear                   blp               1
002800080401     c                   setoff                                       0102
002900080401     c
003000080401     c     kblp          chain     fnblp01l
003100080401     C     Kblp          chain     titas30c
003200080401     c                   if        not %found(fnblp01l)
003300080401     c                   eval      blp='N'
003400080401     c                   endif
003500080401     c                   if        not %found(titas30c)
003600080401     c                   eval      tas='N'
003700080401     c                   endif
003800080401     c
003900080401     c                   select
004000080401     c                   when      blp=' ' and tas=' '
004100080401     c                   if        tasvlc<>arbvlc or arbvlc<>blpvlc
004200080401     c                   seton                                        0102
004300080401     c                   except    stampa3
004400080401     c                   endif
004500080401     c                   when      blp='N' and tas=' '
004600080401     c                   if        arbvlc<>tasvlc
004700080401     c                   seton                                        02
004800080401     c                   except    stampa3
004900080401     c                   endif
005000080401     c                   when      blp=' ' and tas='N'
005100080401     c                   if        arbvlc<>blpvlc
005200080401     c                   seton                                        01
005300080401     c                   except    stampa3
005400080401     c                   endif
005500080401     c                   endsl
005600080401     c
005700080401     c                   setoff                                       0102
005800080401     c                   select
005900080401     c                   when      blp=' ' and tas=' '
006000080401     c                   if        taspkc<>arbpkc or arbpkc<>blppkc
006100080401     c                   seton                                        0102
006200080401     c                   except    stampa2
006300080401     c                   endif
006400080401     c                   when      blp='N' and tas=' '
006500080401     c                   if        arbpkc<>taspkc
006600080401     c                   seton                                        02
006700080401     c                   except    stampa2
006800080401     c                   endif
006900080401     c                   when      blp=' ' and tas='N'
007000080401     c                   if        arbpkc<>blppkc
007100080401     c                   seton                                        01
007200080401     c                   except    stampa2
007300080401     c                   endif
007400080401     c                   endsl
007500080320     c
007600080401     c     kaas          READe     fnarb01l
007700080320    1c                   enddo
007800080320     c
007900080212     c                   seton                                        lr
008000080401     OQSYSPRT   E            stampa3     1
008100080401     O                       arbaas           +   1
008200080401     O                       arblnp           +   1
008300080401     O                       arbnrs        z  +   1
008400080401     O                       arbnsp        2  +   1
008500080401     O                                        +   1 'ARBVLC'
008600080401     O                       arbvlc        4  +   1
008700080401     O                       arbncr        z  +   1
008800080401     O               01                       +   1 'BLPVLC'
008900080401     O               01      blpvlc        4  +   1
009000080401     O               01      blpncr        z  +   1
009100080401     O               02                       +   1 'TASVLC'
009200080401     O               02      TASvlc        4  +   1
009300080401     O               02      TASncr        z  +   1
009400080401     OQSYSPRT   E            stampa2     1
009500080401     O                       arbaas           +   1
009600080401     O                       arblnp           +   1
009700080401     O                       arbnrs        z  +   1
009800080401     O                       arbnsp        2  +   1
009900080401     O                                        +   1 'ARBPKC'
010000080401     O                       arbpkc        4  +   1
010100080401     O                       arbncp        z  +   1
010200080401     O               01                       +   1 'BLPPKC'
010300080401     O               01      blppkc        4  +   1
010400080401     O               01      blpncp        z  +   1
010500080401     O               02                       +   1 'TASPKC'
010600080401     O               02      TASpkc        4  +   1
010700080401     O               02      TASncp        z  +   1
