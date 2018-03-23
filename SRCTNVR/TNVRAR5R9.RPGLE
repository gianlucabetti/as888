000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040415      * Dati errati in bolla  - DUT
000500040225      *------------------------------------------------------------------------*
000600040420     fTitas30c  if   e           k Disk
000700040420     fFiar500f  if   e             Disk
000800040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900040302
001000040415     d dAr5TRS       e ds
001100040415
001200040415     iTitas000      01
001300040415     iTitas010      02
001400040415     iTitasp00      03
001500040225
001600001006      *------------------------------------------------------------------------*
001700040415     C     *entry        plist
001800040415     c                   parm                    aGGIORNA          1
001900040415     C     Ktas          KLIST
002000040415     C                   KFLD                    ar5aas
002100040415     C                   KFLD                    ar5lnp
002200040415     C                   KFLD                    ar5nrs
002300040415     C                   KFLD                    ar5nsp
002400040225
002500040415     c                   Read      Fiar500f
002600040226
002700040415    1c                   dow       not %eof(fiar500f)
002800040415     c                   clear                   wsiwrite          1
002900040415     c*  tipo record TRS
003000040415    2c                   if        ar5trd='TRS'
003100040415     C
003200040415     c                   movel     ar5uni        dar5trs
003300040415     c
003400040427    4c                   if        §ar5flp>0 or §ar5fl2>0
003500040420     c     ktas          chain     titas30c                           30
003600040427    5c                   if        tasflp<>§ar5flp  or
003700040427    5c                             tasfl2<>§ar5fl2
003800040427     c                   except    errore
003900040420    4c                   endif
004000040420    4c                   endif
004100040415
004200040415    2c                   endif
004300040225
004400040415     c                   Read      Fiar500f
004500040415    1c                   EndDo
004600010202
004700040225     c                   Eval      *Inlr = *On
004800040420     OQSYSPRT   E            errore      1
004900040420     O                                         +  1 'Errore transito:TAS'
005000040420     O                       tasflp            +  1
005100040427     O                       tasfl2            +  1
005200040420     O                                         +  1 'AR5'
005300040420     O                       §ar5flp           +  1
005400040427     O                       §ar5fl2           +  1
005500040420     O                                         +  1 'Bolla'
005600040420     O                       tasAAS            +  1
005700040420     O                       tasLNP            +  1
005800040420     O                       tasNRS            +  1
005900040420     O                       tasNSP            +  1
