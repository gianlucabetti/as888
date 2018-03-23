000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040415      * Dati errati in bolla  - DUT
000500040225      *------------------------------------------------------------------------*
000600040420     fTitas30c  if   e           k Disk
000700111027     fFiar500f  if   e             Disk    rename(fiar5000:fiar5fis)
000800111027     fFiar531c  uf   e           k Disk    prefix (T_)
000900040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000040302
001100040415     d dAr5TRS       e ds
001200040415
001300040415     iTitas000      01
001400040415     iTitas010      02
001500040415     iTitasp00      03
001600040225
001700001006      *------------------------------------------------------------------------*
001800040415     C     *entry        plist
001900040415     c                   parm                    aGGIORNA          1
002000040415     C     Ktas          KLIST
002100040415     C                   KFLD                    ar5aas
002200040415     C                   KFLD                    ar5lnp
002300040415     C                   KFLD                    ar5nrs
002400040415     C                   KFLD                    ar5nsp
002500111027     C     Kar5          KLIST
002600111027     C                   KFLD                    ar5aas
002700111027     C                   KFLD                    ar5lnp
002800111027     C                   KFLD                    ar5nrs
002900111027     C                   KFLD                    ar5nsp
003000111027     C                   KFLD                    ar5trd
003100111027     C                   KFLD                    ar5prg
003200040225
003300040415     c                   Read      Fiar500f
003400040226
003500040415    1c                   dow       not %eof(fiar500f)
003600040415     c*  tipo record TRS
003700040415    2c                   if        ar5trd='TRS'
003800040415     C
003900040415     c                   movel     ar5uni        dar5trs
004000111027     c     ktas          chain     titas30c                           30
004100111027     c                   if        not *in30 and taslna=44
004200111027     c                             and tasdbr=20111026
004300111027     c
004400040427     c                   except    errore
004500111027     c                   add       1             conta             5 0
004600111027     c
004700111027     c                   if        Aggiorna='S'
004800111027     c     kar5          chain     fiar531c
004900111027     c                   eval      %subst(t_ar5uni:1:19)='0000000000000000000'
005000111027     c                   update    fiar5000
005100040415    2c                   endif
005200111027    2c                   endif
005300111027    2c                   endif
005400040225
005500040415     c                   Read      Fiar500f
005600040415    1c                   EndDo
005700010202
005800111027     c                   except    fine
005900040225     c                   Eval      *Inlr = *On
006000040420     OQSYSPRT   E            errore      1
006100040420     O                                         +  1 'Errore transito:TAS'
006200040420     O                       §ar5flp           +  1
006300111027     O                       §ar5det           +  1
006400111027     O                       §ar5dut           +  1
006500040427     O                       §ar5fl2           +  1
006600040420     O                                         +  1 'Bolla'
006700111027     O                       tasAAS        z   +  1
006800111027     O                       tasLNP        z   +  1
006900111027     O                       tasNRS        z   +  1
007000111027     O                       tasNSP        z   +  1
007100111027     O                       taslna            +  1
007200111027     O                       tasmgs            +  1 '  /  '
007300111027     OQSYSPRT   E            Fine        1
007400111027     O                                         +  1 'totale'
007500111027     O                       conta         2   +  1
