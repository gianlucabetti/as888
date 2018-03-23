000100930713     H*---------------------------------------------------------------*
000200070301     H* CREA PROFILI PER PDA DA PROFILI PST*                          *
000300930713     H*---------------------------------------------------------------*
000400070301     FAZUTE01L  if   e           k disk
000500070301     Fazorg01l  if   e           k disk
000600930713     C*
000700070301     D OG143         E DS
000800070301     d Kprf            s             10    inz('PST       ')
000900070301     c     kprf          setll     azute01l
001000070301     c                   do        *hival
001100070301     c                   read      azute01l
001200070301
001300070301     c                   if        %eof(azute01l) or %subst(uteute:1:3)<>
001400070301     c                             'PST'
001500070301     c                   leave
001600070301     c                   endif
001700070301     c
001800070301     c* se profilo di p.o. estero scarto
001900070301     c                   clear                   og143
002000070301     c     utefil        chain     azorg01l
002100070301     c                   if        %found(azorg01l)
002200070301     c                   movel     orgde3        og143
002300070301     c                   endif
002400070301     c                   if        §ogntw='EEX' or §ogntw='DPD' or
002500070301     c                             §ogntw='FED'
002600070301     c                   iter
002700070301     c                   endif
002800070301
002900070301     c                   eval      parpo=%subst(uteute:4:3)
003000070301     c                   call      'FIMS88C'
003100070301     c                   parm                    parpo             3
003200080806     c                   parm      'P'           partipo           1
003300080806
003400080806     c                   eval      parpo=%subst(uteute:4:3)
003500080806     c                   call      'FIMS88C'
003600080806     c                   parm                    parpo             3
003700080806     c                   parm      'A'           partipo           1
003800070301     c
003900070301     c                   enddo
004000070301     c                   seton                                        lr
