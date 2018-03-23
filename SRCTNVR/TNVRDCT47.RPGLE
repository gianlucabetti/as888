000100050201     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200050201
000300050427     fFndct01l  if   e           k Disk
000400050427     fFndcf01l  if   e           K Disk
000500051114     fCaapertef o    e             Disk    rename(caapertef:caap)
000600050427     f                                     prefix(w)
000700050427
000800151123     d kdctaac         s                   like(dctaac) inz(2011)
000900050201
001000050201      *------------------------------------------------------------------------*
001100050201
001200050427     c     kfndct        klist
001300050427     c                   kfld                    dctaac
001400050427     c                   kfld                    dctfil
001500050427     c                   kfld                    dctnca
001600050310
001700050427      * Leggo le ca
001800051114     c     kdctaac       Setll     Fndct01l
001900051114do  1c                   Do        *Hival
002000051114     c     kdctaac       Reade     Fndct01l
002100050310      * Fine file
002200050427     c                   If        %Eof(Fndct01l)
002300050310     c                   Leave
002400050310     c                   EndIf
002500050427
002600050427      * solo le ca aperte
002700050427     c                   If        dctdch > *Zeros
002800050427     c                   Iter
002900050427     c                   EndIf
003000050427
003100050427     c                   clear                   caap
003200050427
003300050427     c                   Eval      wdctaac = dctaac
003400050427     c                   Eval      wdctfil = dctfil
003500050427     c                   Eval      wdctnca = dctnca
003600050427     c                   Eval      wdctaas = dctaas
003700050427     c                   Eval      wdctlnp = dctlnp
003800050427     c                   Eval      wdctnrs = dctnrs
003900050427     c                   Eval      wdctnsp = dctnsp
004000050427     c                   Eval      wdctfca = dctfca
004100050427     c                   Eval      wdctipv = dctipv
004200051114     c                   Eval      wdctgfc = dctgfc
004300051114     c                   Eval      wdctfpr = dctfpr
004400051114     c                   Eval      wdcttad = dcttad
004500050427
004600050427      * cerco la data ultima fase
004700050427     c     kfndct        Setgt     fndcf01l
004800050427     c     kfndct        Readpe    fndcf01l
004900050427     c                   Eval      wdcfdfc = dcfdfc
005000050427
005100050427     c                   Write     caap
005200050310
005300050310     c                   EndDo
005400050310
005500050310     c                   Eval      *InLr = *On
