000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040225      *------------------------------------------------------------------------*
000300040420     fTitas00f  uf   e             Disk
000400040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500001006      *------------------------------------------------------------------------*
000600040415     C     *entry        plist
000700040415     c                   parm                    aGGIORNA          1
000800040225
000900040420     c                   Read      titas000
001000040226
001100040420    1c                   dow       not %eof(titas00f)
001200040420     c                   if        tasflp=0 and tasfl2>0
001300040420     c                   except    errore
001400040420     c                   add       1             totspe            9 0
001500040420     c                   if        aggiorna='S'
001600040420     c                   eval      tasflp=tasfl2
001700040420     c                   eval      tasfl2=0
001800040420     c                   update    titas000
001900040420     c                   endif
002000040420     c                   endif
002100040225
002200040420     c                   Read      titas00f
002300040415    1c                   EndDo
002400010202
002500040415     c                   except    totale
002600040225     c                   Eval      *Inlr = *On
002700040420     OQSYSPRT   E            totale      2
002800040420     O                                         +  1 'totale'
002900040420     O                       totspe        2   +  2
003000040420     OQSYSPRT   E            errore      1
003100040420     O                                         +  1 'Errore TASFLP'
003200040420     O                       tasflp            +  1
003300040420     O                                         +  1 'FL2'
003400040420     O                       tasfl2            +  1
003500040420     O                                         +  1 'Bolla'
003600040420     O                       tasAAS            +  1
003700040420     O                       tasLNP            +  1
003800040420     O                       tasNRS            +  1
003900040420     O                       tasNSP            +  1
