000100000103     H DECEDIT('0,') DATEDIT(*DMY/)
000200000103      *                                                                     *****
000300151202     FTIDP301L  IF   E           K DISK
000400151202     Ffnblp01L  IF   E           K DISK
000500000103      *
000600151202     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
000700000103      *----------------------------------------------------*
000800151202     D                 DS
000900151202     D  dp3aas                 1      4  0
001000151202     d  dp3lnp                 5      7  0
001100151202     d  dp3nrs                 8      9  0
001200151202     d  dp3nsp                10     16  0
001300151202     d  dp3sped                1     16  0
001400151203     D                 DS
001500151203     D  savaas                 1      4  0
001600151203     d  savlnp                 5      7  0
001700151203     d  savnrs                 8      9  0
001800151203     d  savnsp                10     16  0
001900151203     d  savsped                1     16  0 inz
002000151202     d  kaas           s                   like(dp3aas)
002100151203     d  totale         s              7  0
002200151203     d  erra           s              7  0
002300000103      *----------------------------------------------------*
002400151202
002500000103
002600151202     c                   eval      kaas=2015
002700151202     c     kaas          setll     tidp301l
002800151202     c                   read      tidp301l
002900151202     c                   dow       not %eof(tidp301l)
003000151202     c                   if        dp3lnp>190 and dp3lnp<199 or
003100151202     c                             dp3lnp=291  or dp3lnp=292
003200160118     c                   if        dp3dev>20141122
003300151202     c
003400151202     c                   if        dp3sped<>savsped
003500151203     c                   if        scan02 >0 and scan03>0
003600151203     c                   eval      totale=totale+1
003700151203     c                   endif
003800151203     c                   if        scan03<scan02 and scan03>0
003900151202     c* Verifico se spuntato subito in arrivo ( lo fanno per i locali)
004000151202     c     kblp          klist
004100151203     c                   kfld                    savaas
004200151203     c                   kfld                    savlnp
004300151203     c                   kfld                    savnrs
004400151203     c                   kfld                    savnsp
004500151203     c                   clear                   blpdse
004600151202     C     KBLP          CHAIN     FNBLP01L
004700151202     c                   except    err
004800151203     c                   eval      erra  =erra  +1
004900151202     c                   endif
005000151203     c                   clear                   scan03           12 0
005100151202     c                   clear                   scan02           12 0
005200151202     c                   eval      savsped=dp3sped
005300151202     c                   endif
005400151202
005500151203     c                   if        dp3evd='03 '
005600151203     c                   if        scan03=0
005700151203     c                   eval      scan03=dp3dev*10000+dp3hev
005800151202     c                   endif
005900151202     c                   endif
006000151202     c                   if        dp3evd='02 '
006100151202     c                   if        scan02=0
006200151202     c                   eval      scan02=dp3dev*10000+dp3hev
006300151202     c                   else
006400151202     c                   except    err02
006500151202     c                   endif
006600151202     c                   endif
006700151202     c                   endif
006800151202     c                   endif
006900151202     c
007000151202     c                   read      tidp301l
007100151202     c                   enddo
007200151202
007300151203     c                   if        scan02 >0 and scan03>0
007400151203     c                   eval      totale=totale+1
007500151203     c                   endif
007600151203     c                   if        scan03<scan02 and scan03>0
007700151203     C     KBLP          CHAIN     FNBLP01L
007800151203     c                   except    err
007900151203     c                   eval      erra  =erra  +1
008000151203     c                   endif
008100151202     c
008200151203     c                   except    tot
008300151202     c                   seton                                        lr
008400151202     c
008500000103      *-------------------------------------------------------------------------
008600151202     Oqsysprt   E            err         1
008700151203     o                                              'scan03<scan02'
008800151202     o                       savsped       z  +   1
008900151203     O                       SCAN02           +   1
009000151203     O                       SCAN03           +   1
009100151202     O                       blpdse           +   1
009200151203     O                       blpduc           +   1
009300151203     O                       blpTFP           +   1
009400151203     O                       blpTFA           +   1
009500151202     Oqsysprt   E            err10       1
009600151202     o                                              'scan10 DOPPIO'
009700151202     o                       savsped       z  +   1
009800151203     O                       SCAN03           +   1
009900151202     O                       DP3DEV        Z  +   1
010000151202     O                       DP3HEV        Z  +   1
010100151202     Oqsysprt   E            err02       1
010200151202     o                                              'scan02 DOPPIO'
010300151202     o                       savsped       z  +   1
010400151202     O                       SCAN02           +   1
010500151202     O                       DP3DEV        Z  +   1
010600151202     O                       DP3HEV        Z  +   1
010700151203     Oqsysprt   E            tot         1
010800151203     o                                              'totali scan 02/03 dal23/11'
010900151203     o                       totale        2  +   1
011000151203     o                                        +   1 'errato'
011100151203     o                       erra          2  +   1
