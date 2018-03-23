000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040415      * Dati errati in bolla  - DUT
000500040225      *------------------------------------------------------------------------*
000600111108     fTitas30c  if   e           k Disk    ignore(titasp00)
000700111110     ftncsb03l  if   e           k Disk
000800111107     fFiar500r  if   e             Disk    rename(fiar5000:fiar50rr)
000900040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000111107      *
001100111107     d dtasflo       e ds
001200111107     d dar5gen       e ds
001300040302
001400040415     iTitas000      01
001500040415     iTitas010      02
001600040225
001700001006      *------------------------------------------------------------------------*
001800111107     C     Ktas          KLIST
001900111107     C                   KFLD                    ar5aas
002000111107     C                   KFLD                    ar5lnp
002100111107     C                   KFLD                    ar5nrs
002200111107     C                   KFLD                    ar5nsp
002300111107     c
002400040225
002500111110     c                   except    errorT
002600111110     c
002700111107     c                   Read      Fiar500r
002800040226
002900111107    1c                   dow       not %eof(fiar500r)
003000111107     c* solo record GEN
003100111107     c                   if        ar5trd='GEN'
003200111107     c                   eval      dar5gen=ar5uni
003300111108     c* Se presete CDI stampo
003400111108     c                   if        §ar5cdi>*zeros
003500111110     c     ktas          chain     tncsb03l
003600111110     c
003700111110     c* stampo se CDI diverso
003800111110     c                   movel     csbcdi        w007              7
003900111110     c                   if        §ar5cdi<>w007
004000111108     c                   except    errore
004100111108     c                   add       1             totcdi            9 0
004200040420    4c                   endif
004300111110     c
004400111110    4c                   endif
004500111107     c                   endif
004600111107     c
004700111107     c
004800111107     c                   Read      Fiar500r
004900040415    1c                   EndDo
005000111107     c                   except    totale
005100010202
005200111107     c
005300040225     c                   Eval      *Inlr = *On
005400111107     c*--------------------------------------------
005500111110     OQSYSPRT   E            errort      1
005600111110     O                                         +  1 'Presente INT.Assegno'
005700111110     O                                         +  1 'da bolla'
005800111110     OQSYSPRT   E            errore      1
005900111110     O                       ar5aas        z   +  0
006000111107     O                       ar5lnp        z   +  1
006100111107     O                       ar5nrs        z   +  1
006200111107     O                       ar5nsp        2   +  1
006300111107     O                       ar5trd            +  1
006400111110     o                                         +  1 'Tipo inc da bolla'
006500111110     O                       §ar5tici          +  1
006600111110     o                                         +  1 'Int asssegno ORM'
006700111108     O                       §ar5cdi           +  1
006800111110     o                                         +  1 'Int asssegno'
006900111110     O                       csbcdi            +  1
007000111110     o                                         +  1 'Data inc'
007100111110     O                       csbdri            +  1 '    /  /  '
007200111110     o                                         +  1 'Data pag'
007300111110     O                       csbddp            +  1 '    /  /  '
007400111110     o
007500111107     o          e            totale      1
007600111108     o                       totcdi        2   +  1
