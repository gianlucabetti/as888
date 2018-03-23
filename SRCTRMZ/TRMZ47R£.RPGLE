000100021219     H DECEDIT('0,') DATEDIT(*YMD.)
000200020606     f********************************************************************
000300070727     f* il pgm reperisce la società e la matricola del dipendente
000400070727     f* dell'autista passato in una certa data
000500020606     f********************************************************************
000600070727     Fgpdan01l  if   E           K DISK
001600070727     d trmz47ds      e ds
001601070727     d kpjba         e ds
001700021219     c*
001800021219     c     *entry        plist
001900070727     c                   parm                    kpjba
001901070727     c                   eval      trmz47ds = kpjbu
002100070727     c                   clear                   d47cogo
002200070727     c                   clear                   d47nomo
002201121119     c                   clear                   d47erro
003700030115     c* leggo l'anagrafica attuale
003900070727     c     d47mat        chain     gpdan01l
004000070727     c                   if        %found(gpdan01l)
004100070727     c                   eval      d47cogo = dancog
004200070727     c                   eval      d47nomo = dannom
004201070802     c                   eval      d47dlio = dandli
004202070727     c                   else
004203121119     c*                  eval      d47erro = '1'
005000070727     c                   end
005001070727     c                   eval      kpjbu = trmz47ds
012200070129
014100021219     c                   seton                                        lr
