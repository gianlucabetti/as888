000100940224     H*--------------------------------------------------------------*
000200060421     Fazorg01L  UF   E           k DISK
000201060421     d og143         e ds
000500900131     C*---------------------------------------------------------------*
001700030205      *
001800030515     c                   do        *hival
001900060421     c                   read      azorg01l
002000030515     c* fine file
002100060421     c                   if        %eoF(azorg01l)
002200030515     c                   leave
002300030515     c                   end
002400030205     C*
002401060421     c                   movel     orgde3        og143
002402060424     c                   clear                   trasmette         1
002403060421     C*
002404060421     C*  hub   --> da 3 a 4 su altro sottocampo hb1
002405060421     C*  depot --> da 3 a 4 su altro sottocampo dp1
002406060421     C*
002407060426     c                   if        §oghub <> *blank and §oghub <> '000'
002408060421     c                   move      *zeros        §oghb1
002409060421     c                   move      §oghub        §oghb1
002410060424     c                   move      'S'           trasmette
002411060426     c                   else
002412060426     c                   move      *blank        §oghb1
002413060426     c                   end
002414060421     C*
002415060421     c                   if        §ogdpt <> *blank
002416060421     c                   move      *zeros        §ogdp1
002417060421     c                   move      §ogdpt        §ogdp1
002418060424     c                   move      'S'           trasmette
002419060421     c                   end
002420060421     C*
002421060421     c                   movel     og143         orgde3
002422060421     C*
002423060424     c                   if        trasmette = 'S'
002424060424     c                   clear                   orgftr
002800060421     c                   update    azorg
002801060424     c                   end
003000041122     C*
003100030205     c                   enddo
003200000616     C*
003300000000     C                   SETON                                        LR
