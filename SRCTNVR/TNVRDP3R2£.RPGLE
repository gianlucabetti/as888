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
001500151202     d  savsped        s                   like(dp3sped)
001600151202     d  kaas           s                   like(dp3aas)
001700000103      *----------------------------------------------------*
001800151202
001900000103
002000151202     c                   eval      kaas=2015
002100151202     c     kaas          setll     tidp301l
002200151202     c                   read      tidp301l
002300151202     c                   dow       not %eof(tidp301l)
002400151202     c                   if        dp3lnp>190 and dp3lnp<199 or
002500151202     c                             dp3lnp=291  or dp3lnp=292
002600151202     c                   if        dp3dev>20151122
002700151202     c
002800151202     c                   if        dp3sped<>savsped
002900151202     c                   if        scan02<scan10 and scan02>0
003000151202     c* Verifico se spuntato subito in arrivo ( lo fanno per i locali)
003100151202     c     kblp          klist
003200151202     c                   kfld                    dp3aas
003300151202     c                   kfld                    dp3lnp
003400151202     c                   kfld                    dp3nrs
003500151202     c                   kfld                    dp3nsp
003600151202     C     KBLP          CHAIN     FNBLP01L
003700151202     c                   except    err
003800151202     c                   endif
003900151202     c                   clear                   scan10           12 0
004000151202     c                   clear                   scan02           12 0
004100151202     c                   eval      savsped=dp3sped
004200151202     c                   endif
004300151202
004400151202     c                   if        dp3evd='10 '
004500151202     c                   if        scan10=0
004600151202     c                   eval      scan10=dp3dev*10000+dp3hev
004700151202     c                   else
004800151202     c                   except    err10
004900151202     c                   endif
005000151202     c                   endif
005100151202     c                   if        dp3evd='02 '
005200151202     c                   if        scan02=0
005300151202     c                   eval      scan02=dp3dev*10000+dp3hev
005400151202     c                   else
005500151202     c                   except    err02
005600151202     c                   endif
005700151202     c                   endif
005800151202     c                   endif
005900151202     c                   endif
006000151202     c
006100151202     c                   read      tidp301l
006200151202     c                   enddo
006300151202
006400151202     c                   if        scan02<scan10 and scan02>0
006500151202     c                   except    err
006600151202     c                   endif
006700151202     c
006800151202     c                   seton                                        lr
006900151202     c
007000000103      *-------------------------------------------------------------------------
007100151202     Oqsysprt   E            err         1
007200151202     o                                              'scan02<scan10'
007300151202     o                       savsped       z  +   1
007400151202     O                       SCAN10           +   1
007500151202     O                       SCAN02           +   1
007600151202     O                       blpdse           +   1
007700151202     Oqsysprt   E            err10       1
007800151202     o                                              'scan10 DOPPIO'
007900151202     o                       savsped       z  +   1
008000151202     O                       SCAN10           +   1
008100151202     O                       DP3DEV        Z  +   1
008200151202     O                       DP3HEV        Z  +   1
008300151202     Oqsysprt   E            err02       1
008400151202     o                                              'scan02 DOPPIO'
008500151202     o                       savsped       z  +   1
008600151202     O                       SCAN02           +   1
008700151202     O                       DP3DEV        Z  +   1
008800151202     O                       DP3HEV        Z  +   1
