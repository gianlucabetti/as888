000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400150305      * CREA TITA400 RECORD "X" PER LOCALITÀ NORMALIZZATA
000500040225      *------------------------------------------------------------------------*
000700150305     ftitas26c  if   e             Disk
000701150305     f                                     ignore(titas000)
000702150305     ftita430c  if a e           k Disk
000703150305     d kloc            s                   like(TASLOD)
000704150305     d tisi95ds      e ds
000900040302
001100001006      *------------------------------------------------------------------------*
001200150305     C     Kta4          KLIST
001300040423     C                   KFLD                    tasaas
001400040423     C                   KFLD                    taslnp
001500040423     C                   KFLD                    tasnrs
001600040423     C                   KFLD                    tasnsp
001700150305     C                   KFLD                    wtrd              1
001800150305     c                   eval      wtrd='X'
001900040225
002000150305     c                   Read      titas26c
002100040226
002200150305    1c                   dow       not %eof(titas26c)
002300040423    c
002301150305   1ac                   if        (tasdcm=0 and tasaas>=2014) or tasaas=2015
002303150305
002500150305     c     kta4          chain     tita430c
002600150305    2c                   if        not %found(tita430c)
002601150305     c                   exsr      norm_loc
002602150305    3c                   if        kloc<>*blanks and kloc<>taslod
002603150305     c                   clear                   tita4000
002604150305     c                   eval      ta4aas=tasaas
002605150305     c                   eval      ta4lnp=taslnp
002606150305     c                   eval      ta4nrs=tasnrs
002607150305     c                   eval      ta4nsp=tasnsp
002608150305     c                   eval      ta4trc=wtrd
002609150305     c                   eval      ta4not=kloc
002610150305     c                   write     tita4000
002611150305    3c                   endif
004900150305    2c                   endif
004901150305
004902150305   1ac                   endif
005000040415     c
005200040225
005300150305     c                   Read      titas26c
005400040415    1c                   EndDo
005500010202
005700040225     c                   Eval      *Inlr = *On
005800150305     C**************************************************************************
005900150305     c* Normalizzo la località
006000150305     C**************************************************************************
006100150305     C     Norm_loc      BEGSR
006101150305     c                   clear                   kloc
006200150305     c                   clear                   tisi95ds
006300150305     C                   MOVEL     '3'           I95TCN
006400150305     c                   eval      i95loc=taslod
006500150305     c                   eval      i95cap=tascad
006600150305     c                   eval      i95nar=tasnzd
006700150305     c                   eval      i95dat=tasaas*10000+tasmgs
006800150305     C                   call      'TISI95R'
006900150305     C                   parm                    tisi95ds
007000150305     C*
007100150305     c
007200150305     C                   IF        O95ERR= ' '
007300150305     c                   eval      kloc=o95loc
007600150305     c                   endif
008600150305     c
008700150305     C                   ENDSR
