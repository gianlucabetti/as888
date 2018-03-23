000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040225      *------------------------------------------------------------------------*
000300070207     fAZCEP01l  uf   e           k Disk
000400001006      *------------------------------------------------------------------------*
000500070207     D* DS PER TISI95R - CONTROLLO DI CAP
000600070207     D tisi95ds      E DS
000700070207     c* Leggo tutto azcep  e pulisco cepflo
000800070207     c     *loval        setll     azcep01l
000900070207     c                   read      azcep01l
001000040226
001100070207    1c                   dow       not %eof(azcep01l)
001200070207     c                   clear                   cepflo
001300070207     c                   if        cepfil=0
001400070207     c* chiamo tisi95
001500070207     C*
001600070207     C                   CLEAR                   TISI95DS
001700070207     C                   EVAL      I95TCN = '7'
001800070207     C                   EVAL      I95DAT = cepdev
001900070207     C                   EVAL      I95NAR = cepnar
002000070207     C                   EVAL      I95PRV = cepprv
002100070207     C                   EVAL      I95CAP = cepcap
002200070207     C                   EVAL      I95LOC = ceploc
002300070207     C                   CALL      'TISI95R'
002400070207     C                   PARM                    TISI95DS
002500070207     c* Se c'e' la località, memorizzo seobsoleta
002600070207    2c                   if        ceploc<>*blanks
002700070223     c                   if        o95lf1<>' '
002800070223     c                   eval      cepflo=o95lf1+%editc(o95lna: 'X')
002900070223     c                   endif
003000070223     c
003100070223     c***                movel     o95lf1        cepflo
003200070207     c                   else
003300070223     c***                movel     o95cf1        cepflo
003400070223     c                   if        o95cf1<>' '
003500070223     c                   eval      cepflo=o95cf1+%editc(o95lna: 'X')
003600070207    2c                   endif
003700070223    2c                   endif
003800070207    2c                   endif
003900070207     c
004000070207     c                   update    azcep000
004100040225
004200070207     c                   Read      azcep01l
004300040415    1c                   EndDo
004400070207     c                   seton                                        lr
