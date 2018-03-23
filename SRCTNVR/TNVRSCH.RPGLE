000100020403     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200000921
000300000921      *------------------------------------------------------------------------*
000400000921      *                                                                        *
000500020403      *          SCRIVE TABELLA SCH DA TABELLA 3J                              *
000600000921      *                                                                        *
000700000921      *------------------------------------------------------------------------*
000800000921
000900020403     ftabel00f  if   e           k disk
001000020403     ftntbe01l  if a e           k disk
001100020403
001200020403     d ds3j          e ds
001300020403     d dsch          e ds
001400001009
001500020403     c     ktabel        klist
001600020403     c                   kfld                    codut             1 0
001700020403     c                   kfld                    kcod              2
001800020403
001900020403     c     ktntbe        klist
002000020403     c                   kfld                    cod               3
002100020403     c                   kfld                    ke1              15
002200000921
002300020403     c                   eval      kcod = '3J'
002400020403     c                   eval      codut = 1
002500010709
002600020403     c     ktabel        setll     tabel00f
002700020403     c                   do        *hival
002800020403     c     ktabel        reade     tabel00f
002900010709
003000020403     c                   if        %eof
003100020403     c                   leave
003200020403     c                   endif
003300020403     c                   movel     tbluni        ds3j
003400010709
003500020403     c                   eval      cod = 'SCH'
003600020403     c                   movel     tblkey        ke1
003700020403     c                   clear                   tntbe000
003800020403     c                   clear                   dsch
003900020403     c     ktntbe        chain     tntbe01l
004000020403     c                   if        not %found(tntbe01l)
004100020403     c                   eval      tbecod = cod
004200020403     c                   eval      tbeke1 = ke1
004300020403     c                   eval      d§schdes = §3jdes
004400020403     c                   eval      d§schese = §3jese
004500020403     c                   eval      d§schor1 = §3jor1
004600020403     c                   eval      d§schor2 = §3jor2
004700020403     c                   eval      d§schor3 = §3jor3
004800020403     c                   eval      d§schor4 = §3jor4
004900020403     c                   eval      d§schor5 = §3jor5
005000020403     c                   eval      d§schor6 = §3jor6
005100020403     c                   eval      d§schor7 = §3jor7
005200020403     c                   eval      d§schor8 = §3jor8
005300020403     c                   eval      d§schor9 = §3jor9
005400020403     c                   eval      d§schor10 = §3jor0
005500020403     c                   movel     dsch          tbeuni
005600020403     c                   write     tntbe000
005700020403     c                   endif
005800020403
005900020403     c                   enddo
006000010709
006100020403     c                   eval      *inlr = *on
006200000921
