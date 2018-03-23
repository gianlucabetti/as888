000100080923     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) Option(*nodebugio)
000200020318     ftntbe01l  if   e           k disk
000300020318     d kcod            s                   like(tbecod)
000400020318     d key1            s                   like(tbeke1)
000500020318     d key2            s                   like(tbeke2)
000600000607      *--------------------------------------------------------------------
000700000607     c
000800111216     c     *entry        plist
000900111216     c                   parm                    percorso         50
001000111216     c                   clear                   percorso
001100080922     c                   eval      *inlr = *on
001200020318
001300020318     c     ktbe          klist
001400020318     c                   kfld                    kcod
001500020318     c                   kfld                    key1
001600020318     c                   kfld                    key2
001700111216     c                   eval      kcod = 'YZH'
001800111216     c                   eval      key1 = '*AUTOMEZZI'
001900111216     c                   eval      key2 = '*FROMSTMF'
002000111216     c     ktbe          chain     tntbe01l
002100111216     c                   if        %found(tntbe01l)
002200111216     c                   eval      percorso = tbeuni
002300080919     c                   endif
002400111216     c                   return
