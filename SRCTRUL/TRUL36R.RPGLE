000100040226     h Decedit('0,') Datedit(*ymd.) Option(*Nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040226      * C A L C O L O   V O L U M E  -  Inserimento misure colli
000500040225      *------------------------------------------------------------------------*
000600040225
000700040226     fTrul36d   cf   e             Workstn
000800010116
000900040226     d TotVlm          s              8  3
001000040225
001100001006      *------------------------------------------------------------------------*
001200040225
001300040226     c                   Eval      w1cqta = 1
001400040225
001500040225do  1c                   Do        *Hival
001600040226     c                   Exfmt     Ul36w01
001700040225     c                   Eval      *In28 = *Off
001800040225
001900040225     c   kl              Leave
002000040225
002100040226     c                   Eval(h)   totvlm = w1cbas * w1cpro * w1calt / 1000000
002200040226     c                             * w1cqta
002300040225
002400040225     c                   If        totvlm > 99,999
002500040225     c                   Eval      *In28 = *On
002600040225     c                   Eval      w1cmsg = 'ATTENZIONE!! Volume troppo alto. +
002700040225     c                                       Ricalcolare.'
002800040225     c                   Iter
002900040225     c                   EndIf
003000040225
003100040225     c                   Leave
003200040225
003300040225     c                   EndDo
003400010202
003500040225     c                   Eval      *Inlr = *On
003600040226      *------------------------------------------------------------------------*
003700040226      * *INZSR - OPERAZIONI INIZIALI
003800040226      *------------------------------------------------------------------------*
003900040226     c     *Inzsr        BegSr
004000040226
004100040226     c     *Entry        Plist
004200040226     c                   Parm                    TotVlm
004300040226
004400040226     c                   EndSr
