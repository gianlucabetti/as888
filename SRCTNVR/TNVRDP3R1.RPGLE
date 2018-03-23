000100000103     H DECEDIT('0,') DATEDIT(*DMY/)
000200000103      *                                                                     *****
000300151202     FTIDP301L  IF   E           K DISK
000400151202     Ffnblp01L  IF   E           K DISK
000500151202     Ffnblt01L  IF   E           K DISK
000600000103      *
000700151202     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
000800000103      *----------------------------------------------------*
000900151202     D                 DS
001000151202     D  dp3aas                 1      4  0
001100151202     d  dp3lnp                 5      7  0
001200151202     d  dp3nrs                 8      9  0
001300151202     d  dp3nsp                10     16  0
001400151202     d  dp3sped                1     16  0
001500151203     D                 DS
001600151203     D  savaas                 1      4  0
001700151203     d  savlnp                 5      7  0
001800151203     d  savnrs                 8      9  0
001900151203     d  savnsp                10     16  0
002000151203     d  savsped                1     16  0 inz
002100151202     d  kaas           s                   like(dp3aas)
002200151203     d  totale         s              7  0
002300151203     d  erra           s              7  0
002400000103      *----------------------------------------------------*
002500151202
002600000103
002700151202     c                   eval      kaas=2015
002800151202     c     kaas          setll     tidp301l
002900151202     c                   read      tidp301l
003000151202     c                   dow       not %eof(tidp301l)
003100151202     c                   if        dp3lnp>190 and dp3lnp<199 or
003200151202     c                             dp3lnp=291  or dp3lnp=292
003300151202     c                   if        dp3dev>20151122
003400151202     c
003500151202     c                   if        dp3sped<>savsped
003600151203     c                   if        scan02 >0 and scan10>0
003700151203     c                   eval      totale=totale+1
003800151203     c                   endif
003900151202     c                   if        scan02<scan10 and scan02>0
004000151202     c* Verifico se spuntato subito in arrivo ( lo fanno per i locali)
004100151202     c     kblp          klist
004200151203     c                   kfld                    savaas
004300151203     c                   kfld                    savlnp
004400151203     c                   kfld                    savnrs
004500151203     c                   kfld                    savnsp
004600151203     c                   clear                   blpdse
004700151202     C     KBLP          CHAIN     FNBLP01L
004800151202     c                   except    err
004900151203     c                   eval      erra  =erra  +1
005000151202     c                   endif
005100151202     c                   clear                   scan10           12 0
005200151202     c                   clear                   scan02           12 0
005300151202     c                   eval      savsped=dp3sped
005400151202     c                   endif
005500151202
005600151202     c                   if        dp3evd='10 '
005700151202     c                   if        scan10=0
005800151202     c                   eval      scan10=dp3dev*10000+dp3hev
005900151202     c                   else
006000151202     c                   except    err10
006100151202     c                   endif
006200151202     c                   endif
006300151202     c                   if        dp3evd='02 '
006400151202     c                   if        scan02=0
006500151202     c                   eval      scan02=dp3dev*10000+dp3hev
006600151202     c                   else
006700151202     c                   except    err02
006800151202     c                   endif
006900151202     c                   endif
007000151202     c                   endif
007100151202     c                   endif
007200151202     c
007300151202     c                   read      tidp301l
007400151202     c                   enddo
007500151202
007600151203     c                   if        scan02 >0 and scan10>0
007700151203     c                   eval      totale=totale+1
007800151203     c                   endif
007900151202     c                   if        scan02<scan10 and scan02>0
008000151203     C     KBLP          CHAIN     FNBLP01L
008100151203     c                   except    err
008200151203     c                   eval      erra  =erra  +1
008300151203     c                   endif
008400151202     c
008500151203     c                   except    tot
008600151202     c                   seton                                        lr
008700151202     c
008800000103      *-------------------------------------------------------------------------
008900151202     Oqsysprt   E            err         1
009000151202     o                                              'scan02<scan10'
009100151202     o                       savsped       z  +   1
009200151202     O                       SCAN10           +   1
009300151202     O                       SCAN02           +   1
009400151202     O                       blpdse           +   1
009500151203     O                       blpduc           +   1
009600151203     O                       blpTFP           +   1
009700151203     O                       blpTFA           +   1
009800151202     Oqsysprt   E            err10       1
009900151202     o                                              'scan10 DOPPIO'
010000151202     o                       savsped       z  +   1
010100151202     O                       SCAN10           +   1
010200151202     O                       DP3DEV        Z  +   1
010300151202     O                       DP3HEV        Z  +   1
010400151202     Oqsysprt   E            err02       1
010500151202     o                                              'scan02 DOPPIO'
010600151202     o                       savsped       z  +   1
010700151202     O                       SCAN02           +   1
010800151202     O                       DP3DEV        Z  +   1
010900151202     O                       DP3HEV        Z  +   1
011000151203     Oqsysprt   E            tot         1
011100151203     o                                              'totali scan 10/02 da 22/11'
011200151203     o                       totale        2  +   1
011300151203     o                                        +   1 'errato'
011400151203     o                       erra          2  +   1
