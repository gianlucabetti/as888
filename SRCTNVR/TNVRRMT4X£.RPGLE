000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040415      * Dati errati in bolla  - DUT
000500040225      *------------------------------------------------------------------------*
000600040423     ffiar530c  if   e           k Disk
000700040423     ftitas00f  if   e             Disk
000800040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900040302
001000040415     d dAr5TRS       e ds
001100001006      *------------------------------------------------------------------------*
001200040415     C     Ktas          KLIST
001300040423     C                   KFLD                    tasaas
001400040423     C                   KFLD                    taslnp
001500040423     C                   KFLD                    tasnrs
001600040423     C                   KFLD                    tasnsp
001700040423     C                   KFLD                    wtrd              3
001800040423     c                   eval      wtrd='TRS'
001900040225
002000040423     c                   Read      titas00f
002100040226
002200040423    1c                   dow       not %eof(titas00f)
002300040423    c
002400040423    2c                   if        tasflp=28 or tasfl2=28
002500040423     c     ktas          chain     fiar530c                           31
002600040423    3c                   if        *in31
002700040423     c                   except    errore1
002800040423   x3c                   else
002900040415     c*  tipo record TRS
003000040415     c                   movel     ar5uni        dar5trs
003100040415     c
003200040423    4c                   if        tasflp=28 and §ar5det=20040416
003300040420     c                   add       tasncl        totncl            9 0
003400040423     c                   add       tasncl        totnc1            9 0
003500040423    5c                   if        §ar5flp<>028
003600040423     c                   except    errore
003700040422     c                   add       tasncl        ERRncl            9 0
003800040422    5c                   endif
003900040420    4c                   endif
004000040423     c
004100040423    4c                   if        tasfl2=28 and §ar5de2=20040416
004200040420     c                   add       tasncl        totncl            9 0
004300040423     c                   add       tasncl        totnc2            9 0
004400040423    5c                   if        §ar5fl2<>028
004500040420     c                   except    errore2
004600040420     c                   add       tasncl        ERRncl            9 0
004700040420    5c                   endif
004800040420    4c                   endif
004900040423    3c                   endif
005000040415     c
005100040415    2c                   endif
005200040225
005300040423     c                   Read      titas00f
005400040415    1c                   EndDo
005500010202
005600040415     c                   except    totale
005700040225     c                   Eval      *Inlr = *On
005800040420     OQSYSPRT   E            totale      2
005900040423     O                                         +  1 'Totale colli'
006000040420     O                       totncl        2   +  2
006100040423     O                                         +  1 'Transito 1'
006200040423     O                       totnc1        2   +  2
006300040423     O                                         +  1 'Transito 2'
006400040423     O                       totnc2        2   +  2
006500040423     O                                         +  1 'di cui ERRATI'
006600040423     O                       ERRncl        2   +  2
006700040420     OQSYSPRT   E            errore      1
006800040420     O                                         +  1 'Errore transito:TAS'
006900040420     O                       tasflp            +  1
007000040420     O                                         +  1 'AR5'
007100040420     O                       §ar5flp           +  1
007200040420     O                                         +  1 'Bolla'
007300040420     O                       tasAAS            +  1
007400040420     O                       tasLNP            +  1
007500040420     O                       tasNRS            +  1
007600040420     O                       tasNSP            +  1
007700040420     OQSYSPRT   E            errore2     1
007800040420     O                                         +  1 'Errore2transito:TAS'
007900040420     O                       tasfl2            +  1
008000040420     O                                         +  1 'AR5'
008100040420     O                       §ar5fl2           +  1
008200040420     O                                         +  1 'Bolla'
008300040420     O                       tasAAS            +  1
008400040420     O                       tasLNP            +  1
008500040420     O                       tasNRS            +  1
008600040420     O                       tasNSP            +  1
008700040423     OQSYSPRT   E            errore1     1
008800040423     O                                         +  1 'Errore: manca recTRS'
008900040423     O                       tasflp            +  1
009000040423     O                       tasfl2            +  1
009100040423     O                                         +  1 'Bolla'
009200040423     O                       tasAAS            +  1
009300040423     O                       tasLNP            +  1
009400040423     O                       tasNRS            +  1
009500040423     O                       tasNSP            +  1
009600040423     O                       tasmgs            +  1 '  /  '
