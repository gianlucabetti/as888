000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040415      * Dati errati in bolla  - DUT
000500040225      *------------------------------------------------------------------------*
000600040423     ffnbtp19l  if   e           k Disk
000700040423     ffiar530c  if   e           k Disk
000800040423     ftitas30c  if   e           k Disk
000900040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000040302
001100040415     d dAr5TRS       e ds
001200001006      *------------------------------------------------------------------------*
001300040423     C     Kbtp          KLIST
001400040423     C                   KFLD                    wflp              3 0
001500040423     C                   KFLD                    wdut              8 0
001600040423     c                   eval      wflp=28
001700040423     c                   eval      wdut=20040420
001800040423     C     Ktas          KLIST
001900040423     C                   KFLD                    btpaas
002000040423     C                   KFLD                    btplnp
002100040423     C                   KFLD                    btpnrs
002200040423     C                   KFLD                    btpnsp
002300040423     C     Kar5          KLIST
002400040423     C                   KFLD                    btpaas
002500040423     C                   KFLD                    btplnp
002600040423     C                   KFLD                    btpnrs
002700040423     C                   KFLD                    btpnsp
002800040423     C                   KFLD                    wtrd              3
002900040423     c                   eval      wtrd='TRS'
003000040225
003100040423     c     kbtp          setll     fnbtp19l
003200040423     c     kbtp          Reade     fnbtp19l
003300040423    0c                   dow       not %eof(fnbtp19l)
003400040423     c     ktas          chain     titas30c
003500040226
003600040423    1c                   if        not %found(titas30c)
003700040423     c                   except    errore1
003800040423     c                   add       btpncl        toterr1           7 0
003900040423   x1c                   else
004000040423    c
004100040423    2c                   if        tasflp<>28 and tasfl2<>28
004200040423     c                   except    errore2
004300040423     c                   add       btpncl        toterr2           7 0
004400040423   x2c                   else
004500040423     c     kar5          chain     fiar530c                           31
004600040423    3c                   if        *in31
004700040423     c                   except    errore3
004800040423     c                   add       btpncl        toterr3           7 0
004900040423   x3c                   else
005000040415     c*  tipo record TRS
005100040415     c                   movel     ar5uni        dar5trs
005200040415     c
005300040423    4c                   if        tasflp=28
005400040423    5c                   if        §ar5det=20040420
005500040423     c                   add       tasncl        totncl            7 0
005600040423     c                   add       tasncl        totnc1            7 0
005700040423     c                   else
005800040423     c                   except    errore4
005900040423     c                   add       btpncl        toterr4           7 0
006000040423    5c                   endif
006100040423    4c                   endif
006200040423     c
006300040423     c
006400040423    4c                   if        tasfl2=28
006500040423    5c                   if        §ar5de2=20040420
006600040423     c                   add       tasncl        totncl            7 0
006700040423     c                   add       tasncl        totnc2            7 0
006800040423     c                   else
006900040423     c                   except    errore4
007000040423     c                   add       btpncl        toterr4           7 0
007100040420    5c                   endif
007200040420    4c                   endif
007300040423    3c                   endif
007400040415     c
007500040415    2c                   endif
007600040423    2c                   endif
007700040225
007800040423     c     kbtp          Reade     fnbtp19l
007900040423    0c                   EndDo
008000010202
008100040415     c                   except    totale
008200040225     c                   Eval      *Inlr = *On
008300040420     OQSYSPRT   E            totale      2
008400040423     O                                              'Totale colli ok'
008500040420     O                       totncl        2   +  2
008600040423     O                                         +  1 'Transito 1'
008700040423     O                       totnc1        2   +  2
008800040423     O                                         +  1 'Transito 2'
008900040423     O                       totnc2        2   +  2
009000040423     OQSYSPRT   E            totale      4
009100040423     O                                              'Errori: NO TITAS'
009200040423     O                       toterr1       2   +  1
009300040423     O                                         +  1 'NO Transito 28'
009400040423     O                       toterr2       2   +  1
009500040423     O                                         +  1 'NO AR5'
009600040423     O                       toterr3       2   +  1
009700040423     O                                         +  1 'NO DATA GIUSTA'
009800040423     O                       toterr4       2   +  1
009900040423     o
010000040423     OQSYSPRT   E            errore1     1
010100040423     O                                              'Non trovato TITAS'
010200040423     O                       btpAAS            +  1
010300040423     O                       btpLNP            +  1
010400040423     O                       btpNRS            +  1
010500040423     O                       btpNSP        z   +  1
010600040423     O                       btpmgs            +  1 '  /  '
010700040420     OQSYSPRT   E            errore2     1
010800040423     O                                              'Su TAS non c''é tra'
010900040423     O                                              'nsito 28'
011000040423     O                       tasflp            +  1
011100040423     O                       tasfl2            +  1
011200040420     O                                         +  1 'Bolla'
011300040420     O                       tasAAS            +  1
011400040420     O                       tasLNP            +  1
011500040420     O                       tasNRS            +  1
011600040423     O                       tasNSP        z   +  1
011700040423     O                       tasmgs            +  1 '  /  '
011800040423     OQSYSPRT   E            errore3     1
011900040423     O                                              'Manca AR5'
012000040423     O                       tasflp            +  1
012100040423     O                       tasfl2            +  1
012200040423     O                                         +  1 'Bolla'
012300040423     O                       tasAAS            +  1
012400040423     O                       tasLNP            +  1
012500040423     O                       tasNRS            +  1
012600040423     O                       tasNSP            +  1
012700040423     O                       tasmgs            +  1 '  /  '
012800040423     OQSYSPRT   E            errore4     1
012900040423     O                                              'Transito 28 non in'
013000040423     O                                              'data 20/4'
013100040423     O                       tasflp            +  1
013200040423     O                       §ar5det           +  1 '    /  /  '
013300040423     O                       tasfl2            +  1
013400040423     O                       §ar5de2           +  1 '    /  /  '
013500040423     O                                         +  1 'Bolla'
013600040423     O                       tasAAS            +  1
013700040423     O                       tasLNP            +  1
013800040423     O                       tasNRS            +  1
013900040423     O                       tasNSP            +  1
014000040423     O                       tasmgs            +  1 '  /  '
