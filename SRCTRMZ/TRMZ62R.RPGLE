000100021219     H DECEDIT('0,') DATEDIT(*YMD.)
000200020606     f********************************************************************
000300070730     f* il pgm reperisce l'autista e la targa dell'auto ad una certa data
000400070730     f* passando società, dipendente e data
000500020606     f********************************************************************
000600070727     Ftabel00f  if   E           K DISK
000700070727     Fmzxnh01l  if   E           K DISK
001600070727     d trmz62ds      e ds
001601070727     d dsxn          e ds
001700021219     c*
001800021219     c     *entry        plist
001900070727     c                   parm                    trmz62ds
002100070730     c                   clear                   d62soco
002101070730     c                   clear                   d62mato
002300070730     c                   clear                   d62erro
002400021219     c*
002500070727     C     Ktab          KLIST                                                  *
002600070727     C                   KFLD                    tblkut                         *
002700070727     C                   KFLD                    tblcod                         *
002701070727     C                   KFLD                    tblkey                         *
002702070727     c                   eval      tblkut = 1
002703070727     c                   eval      tblcod = 'XN'
002704070727     c                   eval      tblkey = d62cau
002800021219     c*
002900070727     C     Kxnh          KLIST                                                  *
003000070727     C                   KFLD                    d62cau                         *
003200070727     C                   KFLD                    d62dta                         *
003700030115     c* leggo l'anagrafica attuale
003900070727     c     ktab          chain     tabel00f
004000070727     c                   if        %found(tabel00f)
004001070727     c                   eval      dsxn = tbluni
004002070731     c                   if        %subst(tbluni: 36: 1) <> ' '
004100070727     c                   eval      d62soco = §xnsoc
004200070727     c                   eval      d62mato = §xnmat
004201070802     c                   else
004202070802     c                   eval      d62erro = '1'
004203070802     c                   seton                                        lr
004204070802     c                   return
004205070731     c                   end
004206070802     c                   else
004207070802     c                   eval      d62erro = '1'
004208070802     c                   seton                                        lr
004209070802     c                   return
005000070727     c                   end
010300070727     c* verifico lo storico
010500070727     c     kxnh          setll     mzxnh01l
010600030205     c                   do        *hival
010700070727     c     d62cau        reade     mzxnh01l
010800070727     c                   if        %eof(mzxnh01l)
010900030205     c                   leave
011000030205     c                   end
011100070727     c                   if        xnhdta >=D62DTA
011700070727     c                   eval      d62soco = xnhsoc
011701070727     c                   eval      d62mato = xnhmat
011702070727     c                   leave
012000021219     c                   end
012100030205     c                   enddo
012200070129
014100021219     c                   seton                                        lr
