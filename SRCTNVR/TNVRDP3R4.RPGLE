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
003500151203     c                   if        scan03 >0 and scan13>0
003600151203     c                   eval      totale=totale+1
003700151203     c                   endif
003800151203     c                   if        scan13<scan03 and scan13>0
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
005100151203     c                   clear                   scan13           12 0
005200151202     c                   eval      savsped=dp3sped
005300151202     c                   endif
005400151202
005500151203     c                   if        dp3evd='03 '
005600151203     c                   if        scan03=0
005700151203     c                   eval      scan03=dp3dev*10000+dp3hev
005800151203     c**                 else
005900151203     c**                 except    err03
006000151202     c                   endif
006100151202     c                   endif
006200151203     c                   if        dp3evd='13 '
006300151203     c                   if        scan13=0
006400151203     c                   eval      scan13=dp3dev*10000+dp3hev
006500151202     c                   else
006600151203     c                   except    err13
006700151202     c                   endif
006800151202     c                   endif
006900151202     c                   endif
007000151202     c                   endif
007100151202     c
007200151202     c                   read      tidp301l
007300151202     c                   enddo
007400151202
007500151203     c                   if        scan03 >0 and scan13>0
007600151203     c                   eval      totale=totale+1
007700151203     c                   endif
007800151203     c                   if        scan13<scan03 and scan13>0
007900151203     C     KBLP          CHAIN     FNBLP01L
008000151203     c                   except    err
008100151203     c                   eval      erra  =erra  +1
008200151203     c                   endif
008300151202     c
008400151203     c                   except    tot
008500151202     c                   seton                                        lr
008600151202     c
008700000103      *-------------------------------------------------------------------------
008800151202     Oqsysprt   E            err         1
008900151203     o                                              'scan13<scan03'
009000151202     o                       savsped       z  +   1
009100151203     O                       SCAN03           +   1
009200151203     O                       SCAN13           +   1
009300151202     O                       blpdse           +   1
009400151203     O                       blpduc           +   1
009500151203     O                       blpTFP           +   1
009600151203     O                       blpTFA           +   1
009700151203     Oqsysprt   E            err13       1
009800151203     o                                              'scan13 DOPPIO'
009900151202     o                       savsped       z  +   1
010000151203     O                       SCAN13           +   1
010100151202     O                       DP3DEV        Z  +   1
010200151202     O                       DP3HEV        Z  +   1
010300151203     Oqsysprt   E            err03       1
010400151203     o                                              'scan03 DOPPIO'
010500151202     o                       savsped       z  +   1
010600151203     O                       SCAN03           +   1
010700151202     O                       DP3DEV        Z  +   1
010800151202     O                       DP3HEV        Z  +   1
010900151203     Oqsysprt   E            tot         1
011000151203     o                                              'totali scan 03/13 dal23/11'
011100151203     o                       totale        2  +   1
011200151203     o                                        +   1 'errato'
011300151203     o                       erra          2  +   1
