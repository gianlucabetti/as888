000100080923     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) Option(*nodebugio)
000200020318     ftntbe01l  if   e           k disk
000400090407     D ficn08ds      e ds
000600090407     D dtma          e ds
000601090407     D dtmd          e ds
000602090407     D dtmt          e ds
000800020318     d kcod            s                   like(tbecod)
000900020318     d key1            s                   like(tbeke1)
001000020318     d key2            s                   like(tbeke2)
001500000607      *--------------------------------------------------------------------
001600000607     c
001700000607     c     *entry        plist
001800090407     c                   parm                    ficn08ds
001900080922     c                   eval      *inlr = *on
001901090407     c                   clear                   o08err
001902090407     c                   clear                   o08msg
001903090407     c                   clear                   o08tc
001904090407     c                   clear                   o08ts
002100020318     c     ktbe          klist
002200020318     c                   kfld                    kcod
002300020318     c                   kfld                    key1
002401090407     c                   select
002402090407     c                   when      i08tip = 'A'
002500090407     c                   eval      kcod = 'TMA'
002501090407     c                   when      i08tip = 'D'
002502090407     c                   eval      kcod = 'TMD'
002503090407     c                   when      i08tip = 'T'
002504090407     c                   eval      kcod = 'TMT'
002505090407     c                   endsl
002506090407     c                   movel     i08data       key1
003000080922      * reperimento prezzi di riferimento
003100080922     c                   exsr      repprz
004000080922
004100080922     c                   return
004200080922     c
004300080922      *_________________________________________________________________
004400080922     c     repprz        begsr
004500080922      *_________________________________________________________________
004600080919      *cerco prezzo di ingresso carburante
004900090407     c     ktbe          setgt     tntbe01l                           99
004901090407     c                   do        *hival
004902090407     c     kcod          readpe    tntbe01l
004903090407     c                   if        %eof(tntbe01l)
004904090407     c                   leave
004905090407     c                   end
004906090407     c* AUTISTI cons/rit
004907090407     c                   select
004908090407     c                   when      i08tip = 'A'
004909090407     c                   movel     tbeuni        dtma
004910090407     c* cerco il range giusto
004911090407     c                   if        i08mcp>= §TMADA and i08mcp<= §TMAA
004912090407     c                   eval      o08tc = §TMATC
004913090407     c                   eval      o08ts = §TMATs
004914090407     c                   leave
004915090407     c                   end
004916090407     c* AFF/DEFL
004917090407     c                   when      i08tip = 'D'
004918090407     c                   movel     tbeuni        dtmd
004919090407     c* cerco il range giusto
004920090407     c                   if        i08mcp>= §TMDDA and i08mcp<= §TMDA
004921090407     c                   eval      o08tc = §TMDTC
004922090407     c                   eval      o08ts = §TMDTs
004923090407     c                   leave
004924090407     c                   end
004925090407     c* TRAZIONISTI
004926090407     c                   when      i08tip = 'T'
004927090407     c                   movel     tbeuni        dtmt
004928090407     c* cerco il range giusto
004929090407     c                   if        i08mcp>= §TMTDA and i08mcp<= §TMTA
004930090407     c                   eval      o08tc = §TMTTC
004931090407     c                   eval      o08ts = §TMTTs
004932090407     c                   leave
004933090407     c                   end
004934090407     c                   endsl
005004090407     c                   enddo
005005090407     c                   if        o08tc = 0 and o08ts = 0
005006090407     c                   eval      o08msg = 'Non trovato record tabella'
005008090407     c                   eval      o08err = 'E'
005300080919     c                   endif
005400090109     c                   endsr
