000100020412     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020529      *
000300060920      *--------------------------------------------------------------*
000400020412
000500080730     fwvacerr   if   e             disk    rename(wvacerr:wvac)
000600080730     ffnblp01l  uf   e           k disk
000700020412
000800080730     d fnvacds       e ds                  extname(fnvac00t)
000900020529
001000080604     c*
001100080604     c     kblp          klist
001200080730     c                   kfld                    VACAAS
001300080730     c                   kfld                    VACLNP
001400080730     c                   kfld                    VACNRS
001500080730     c                   kfld                    VACNSP
001600080604      *
001700080730     c     *start        setll     Wvacerr
001800080730     c                   do        *hival
001900080730     c                   read      wvacerr
002000080730     c                   if        %eof(wvacerr)
002100080730     c                   leave
002200080730     c                   endif
002300080730     c                   clear                   fnvacds
002400080730     c                   movel     vgddta        fnvacds
002500020529
002600080730     c     kblp          chain     fnblp01l
002700080730     c                   if        %found(fnblp01l)
002800080730     C                   MOVEL     *blank        BLPFT3
002900080730     C                   MOVEL     *zeros        BLPDT3
003000080730     c                   update    fnblp000
003100060921     c                   endif
003200080730
003300060921     c                   enddo
003400060920     c
003500080730     c                   seton                                        lr
003600080603      *
