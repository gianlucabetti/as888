000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300080404     FFNblp01L  uF   E           K DISK
000400080404     FFNarb01L  uF   E           K DISK
000500080404     FFiar404L  iF   E           K DISK
000600080404     FFiar401L  uF   E           K DISK    prefix(d_) rename(fiar4000:fiar4001)
000700021214      *--------------------------------------------------
000800080404     c     kar4          klist
000900080404     c                   kfld                    trc               1
001000080404     c                   kfld                    note14           14
001100080404     c     kar4DEL       klist
001200080404     c                   kfld                    ar4aas
001300080404     c                   kfld                    ar4lnp
001400080404     c                   kfld                    ar4nrs
001500080404     c                   kfld                    ar4nsp
001600080404     c     kar4DELM      klist
001700080404     c                   kfld                    ar4aas
001800080404     c                   kfld                    ar4lnp
001900080404     c                   kfld                    ar4nrs
002000080404     c                   kfld                    ar4nsp
002100080404     c                   kfld                    TRC
002200080404     c
002300080404     c                   movel     'M'           TRC
002400080404     c                   eval      note14='12500009333300'
002500080404     c     kar4          setll     fiar404l
002600080404     c     kar4          reade     fiar404l
002700080404     c
002800080404    1c                   DOW       not %EOF(Fiar404l)
002900080404
003000080404     c     kar4del       chain     fnblp01l
003100080404     c     kar4del       chain     fnarb01l
003200080404     c                   if        %found(fnblp01l)  and %found(fnarb01l)
003300080404     c                   move      blpmgs        blpdrt
003400080404     c                   update    fnblp000
003500080404
003600080404     c                   if        arbdcm>0
003700080404     c                   clear                   arbdti
003800080404     c                   clear                   arbhti
003900080404     c                   movel     'S'           arbfbs
004000080404     c                   update    fnarb000
004100080404     c                   endif
004200080404     c
004300080404     c     kar4delM      delete    fiar401l
004400080404     c                   eval      recagg=recagg+1
004500080404     c                   endif
004600080404     c
004700080404     c     kar4          reade     fiar404l
004800080404     c                   enddo
004900070913     c
005000080404     c                   dsply                   recagg            8 0
005100021216      *
005200021214     C                   eval       *inlr = *on
