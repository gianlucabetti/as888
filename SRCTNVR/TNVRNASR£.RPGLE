000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040415      * Dati errati in bolla  - DUT
000500040225      *------------------------------------------------------------------------*
000700111107     fTitas30c  uf   e           k Disk    ignore(titasp00)
000900111107     fFiar500r  if   e             Disk    rename(fiar5000:fiar50rr)
001000040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100111107      *
001400111107     d dtasflo       e ds
001401111107     d dar5gen       e ds
001500040302
001600040415     iTitas000      01
001700040415     iTitas010      02
001900040225
002000001006      *------------------------------------------------------------------------*
002900111107     C     Ktas          KLIST
003000111107     C                   KFLD                    ar5aas
003100111107     C                   KFLD                    ar5lnp
003200111107     C                   KFLD                    ar5nrs
003300111107     C                   KFLD                    ar5nsp
003400111107     c
005100040225
005200111107     c                   Read      Fiar500r
005300040226
005400111107    1c                   dow       not %eof(fiar500r)
005401111107     c* solo record GEN
005402111107     c                   if        ar5trd='GEN'
005501111107     c                   eval      dar5gen=ar5uni
005502111107     c* solo colli di valore
005503111107     c                   if        §ar5bva='V'
007600111107     c                   exsr      aggtitas
007700040420    4c                   endif
007701111107     c                   endif
007900111107     c
008700111107     c
010400111107     c                   Read      Fiar500r
010500040415    1c                   EndDo
010501111107     c                   except    totale
010600010202
010800111107     c
010900040225     c                   Eval      *Inlr = *On
011000111107     c*--------------------------------------------
011100111107     c     AggTITAS      BEGSR
011200111107     c
011400111107     c                   setoff                                       0102
011500111107     c     ktas          chain     titas30c
011600111107     c                   if        %found(titas30c)
011700111107     c                   Eval      dtasflo = TasFlo
011801111107     c                   Eval      §FloTba = §Ar5Bva
011900111107     c                   Eval      TasFlo = dtasflo
012000111107     c   01              update    titas000
012001111107     c   02              update    titas010
012002111107     c                   add       1             totgenV           9 0
012003111107     c                   else
012004111107     c                   except    erroreN
012100111107     c                   endif
016100111107     c                   ENDSR
016102111107
016200111107     OQSYSPRT   E            erroreN     1
016300111107     O                                         +  1 'Titas non trovato '
016400111107     O                       ar5aas        z   +  1
016500111107     O                       ar5lnp        z   +  1
016600111107     O                       ar5nrs        z   +  1
016700111107     O                       ar5nsp        2   +  1
016800111107     O                       ar5trd            +  1
016900111107     o          e            totale      1
017000111107     o                       totgenv       2   +  1
