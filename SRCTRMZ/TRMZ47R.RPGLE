000100021219     H DECEDIT('0,') DATEDIT(*YMD.)
000200020606     f********************************************************************
000300070727     f* il pgm reperisce la società e la matricola del dipendente
000400070727     f* dell'autista passato in una certa data
000500020606     f********************************************************************
000600130131     Fazdan01l  if   E           K DISK
001600070727     d trmz47ds      e ds
001601070727     d kpjba         e ds
001700021219     c*
001800021219     c     *entry        plist
001900070727     c                   parm                    kpjba
001901070727     c                   eval      trmz47ds = kpjbu
002100070727     c                   clear                   d47cogo
002200070727     c                   clear                   d47nomo
002201121119     c                   clear                   d47erro
002202130204     c     kdan          klist
002203130204     c                   kfld                    d47soc
002204130204     c                   kfld                    d47mat
003700030115     c* leggo l'anagrafica attuale
003900130204     c     kdan          chain     azdan01l
004000130131     c                   if        %found(azdan01l)
004100070727     c                   eval      d47cogo = dancog
004200070727     c                   eval      d47nomo = dannom
004201130131     c                   move      dandli        d47dlio
004202070727     c                   else
004203121119     c*                  eval      d47erro = '1'
005000070727     c                   end
005001130204     c                   if        d47dlio = 00010101
005002130204     c                   clear                   d47dlio
005003130204     c                   endif
005004070727     c                   eval      kpjbu = trmz47ds
012200070129
014100021219     c                   seton                                        lr
