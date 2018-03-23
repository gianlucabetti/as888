000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300070913     FFNblp79L  iF   E           K DISK
000400070913     Ffnagb00f  o  a E             DISK
000500021214      *--------------------------------------------------
000600070913     c     kblp          klist
000700070913     c                   kfld                    tfp               3 0
000800070913     c                   kfld                    nfv               5 0
000900070913     c                   kfld                    lna               3 0
001000070913     c                   z-add     001           tfp
001100070913     c                   z-add     10849         nfv
001200070913     c                   z-add     066           lna
001300070913     c
001400070913     c     kblp          setll     fnblp79l
001500070913     c     kblp          reade     fnblp79l
001600051110
001700070913    1c                   DOW       not %EOF(FNblp79l)
001800070913     c                   eval      agbaas=blpaas
001900070913     c                   eval      agblnp=blplnp
002000070913     c                   eval      agbnrs=blpnrs
002100070913     c                   eval      agbnsp=blpnsp
002200070913     c                   eval      agbagb='DT'
002300070913     c                   eval      agbtbo='P'
002400070913     c                   write     fnagb000                             99
002500070913     c                   eval      agbtbo='A'
002600070913     c                   write     fnagb000                             99
002700070913     c     kblp          reade     fnblp79l
002800051110     c                   enddo
002900021216      *
003000021214     C                   eval       *inlr = *on
