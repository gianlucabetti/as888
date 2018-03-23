000100080923     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) Option(*nodebugio)
000200020318     ftntbe01l  if   e           k disk
000300090109     D ficn05ds      e ds
000400080922     D dgpi          e ds
000500020318     d kcod            s                   like(tbecod)
000600020318     d key1            s                   like(tbeke1)
000700020318     d key2            s                   like(tbeke2)
000800000607      *--------------------------------------------------------------------
000900000607     c
001000000607     c     *entry        plist
001100090109     c                   parm                    ficn05ds
001200080922     c                   eval      *inlr = *on
001300111124     c                   clear                   dgpi
001400111124     c                   eval      O05GPI = dgpi
001500111124     c                   return
001600020318
001700020318     c     ktbe          klist
001800020318     c                   kfld                    kcod
001900020318     c                   kfld                    key1
002000020318     c                   kfld                    key2
002100080919     c                   eval      kcod = 'GPI'
002200080922      * reperimento prezzi di riferimento
002300080922     c                   exsr      repprz
002400080922
002500080922     c                   return
002600080922     c
002700080922      *_________________________________________________________________
002800080922     c     repprz        begsr
002900080922      *_________________________________________________________________
003000080919      *cerco prezzo di ingresso carburante
003100080919     c                   eval      key1 = '1'
003200080919     c                   eval      key2 = *blank
003300020318     c     ktbe          chain     tntbe01l                           99
003400020318     c                   if        not *in99
003500090109     c                   clear                   o05err
003600090109     c                   clear                   o05msg
003700090109     c                   movel     tbeuni        o05gpi
003800090109     c                   else
003900090109     c                   eval      o05msg = 'Non trovato record tabella'
004000090109     c                   clear                   o05gpi
004100090109     c                   eval      o05err = 'E'
004200080919     c                   endif
004300090109     c                   endsr
