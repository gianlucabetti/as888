000100080923     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) Option(*nodebugio)
000200020318     ftntbe01l  if   e           k disk
000400090109     D ficn05ds      e ds
000600080922     D dgpi          e ds
000800020318     d kcod            s                   like(tbecod)
000900020318     d key1            s                   like(tbeke1)
001000020318     d key2            s                   like(tbeke2)
001500000607      *--------------------------------------------------------------------
001600000607     c
001700000607     c     *entry        plist
001800090109     c                   parm                    ficn05ds
001900080922     c                   eval      *inlr = *on
002000020318
002100020318     c     ktbe          klist
002200020318     c                   kfld                    kcod
002300020318     c                   kfld                    key1
002400020318     c                   kfld                    key2
002500080919     c                   eval      kcod = 'GPI'
003000080922      * reperimento prezzi di riferimento
003100080922     c                   exsr      repprz
004000080922
004100080922     c                   return
004200080922     c
004300080922      *_________________________________________________________________
004400080922     c     repprz        begsr
004500080922      *_________________________________________________________________
004600080919      *cerco prezzo di ingresso carburante
004700080919     c                   eval      key1 = '1'
004800080919     c                   eval      key2 = *blank
004900020318     c     ktbe          chain     tntbe01l                           99
005000020318     c                   if        not *in99
005001090109     c                   clear                   o05err
005002090109     c                   clear                   o05msg
005003090109     c                   movel     tbeuni        o05gpi
005004090109     c                   else
005005090109     c                   eval      o05msg = 'Non trovato record tabella'
005006090109     c                   clear                   o05gpi
005007090109     c                   eval      o05err = 'E'
005300080919     c                   endif
005400090109     c                   endsr
