000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300080404     Ftitas30c  uF   E           K DISK
000400080404     Ftita432c  iF   E           K DISK
000500080404     Ftita430c  uF   E           K DISK    rename(tita4000:tita4001)
000600080404     F                                     rename(tita4p00:tita4p01) prefix(D_)
000700080404     d note14          s             14
000800080404     ITITAS000      01
000900080404     ITITAS010      02
001000080404     ITITASP00      03
001100021214      *--------------------------------------------------
001200080404     c     kar4DEL       klist
001300080404     c                   kfld                    ta4aas
001400080404     c                   kfld                    ta4lnp
001500080404     c                   kfld                    ta4nrs
001600080404     c                   kfld                    ta4nsp
001700080404     c     kar4DELM      klist
001800080404     c                   kfld                    ta4aas
001900080404     c                   kfld                    ta4lnp
002000080404     c                   kfld                    ta4nrs
002100080404     c                   kfld                    ta4nsp
002200080404     c                   kfld                    TRC               1
002300080404     c
002400080404     c                   eval      trc='M'
002500080404     c                   eval      note14='12500009333300'
002600080404     c     note14        setll     tita432c
002700080404     c     note14        reade     tita432c
002800080404     c
002900080404    1c                   DOW       not %EOF(tita432c)
003000080404
003100080404     c                   setoff                                       010203
003200080404     c     kar4del       chain     titas30c
003300080404     c                   if        %found(titas30c)
003400080404     c                   move      tasmgs        tasdrt
003500080404     c   01              update    titas000
003600080404
003700080404     c
003800080404     c     kar4delM      delete    tita430c
003900080404     c                   eval      recagg=recagg+1
004000080404     c                   endif
004100080404     c
004200080404     c     note14        reade     tita432c
004300080404     c                   enddo
004400070913     c
004500080404     c                   dsply                   recagg            8 0
004600021216      *
004700021214     C                   eval       *inlr = *on
