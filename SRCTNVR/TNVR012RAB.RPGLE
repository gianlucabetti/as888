000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300060203      * Il pgm deve accodare i records non presenti già sulla libreria
000500060203      *  di arrivo.  Qualora il record fosse già presente, viene
000900060203      * parcheggiato sui files XX per il controllo finale.
001500060125      *****************************************************************
001501060203     fWxftt00f  if   e             disk    rename(fiftt000:WXFTT000)
001502060203     fWxftd00f  if   e             disk    rename(fiftd000:WXFTD000)
001503060203     fWxfce00f  if   e             disk    rename(fifce000:WXFCE000)
001504060203     fWxfre00f  if   e             disk    rename(fifre000:WXFRE000)
001505060203     F*---------------------------------------------------------------------------------
002500060203     fWxfttxxf  o  a e             disk    rename(fiftt000:WXFTTxxx)
002501060203     fWxftdxxf  o  a e             disk    rename(fiftd000:WXFTDxxx)
002502060203     fWxfcexxf  o  a e             disk    rename(fifce000:WXFCExxx)
002503060203     fWxfrexxf  o  a e             disk    rename(fifre000:WXFRExxx)
003000030429     F*---------------------------------------------------------------------------------
003302060203     ffiftt01l  if a e           k disk
003303060203     ffiftd05l  if a e           k disk
003304060203     ffifce01l  if a e           k disk
003305060203     ffifre02l  if a e           k disk
007800020205     c**********************************************************************
007801060203      * ?  Testate
007802060203     c                   read      wxftt00f
007803060203     c                   dow       not %EoF(wxftt00f)
007804060203     c*
007805060203     c     kftt          chain     fiftt01l
007806060203     c                   if        %Found(fiftt01l)
007807060203     c                   write     wxfttxxx
007808060203     c                   else
007809060203     c                   write     fiftt000
007810060203     c                   end
007811060203     c*
007812060203     c                   read      wxftt00f
007813060203     c                   enddo
009701060125     c*
009702060203      * ?  Dettagli
009703060203     c                   read      wxftd00f
009704060203     c                   dow       not %EoF(wxftd00f)
009705060203     c*
009706060203     c     kftd          chain     fiftd05l
009707060203     c                   if        %Found(fiftd05l)
009708060203     c                   write     wxftdxxx
009709060203     c                   else
009710060203     c                   write     fiftd000
009711060203     c                   end
009712060203     c*
009713060203     c                   read      wxftd00f
009714060203     c                   enddo
009715060203     c*
009716060203      * ?  Costi
009717060203     c                   read      wxfce00f
009718060203     c                   dow       not %EoF(wxfce00f)
009719060203     c*
009720060203     c     kfce          chain     fifce01l
009721060203     c                   if        %Found(fifce01l)
009722060203     c                   write     wxfcexxx
009723060203     c                   else
009724060203     c                   write     fifce000
009725060203     c                   end
009726060203     c*
009727060203     c                   read      wxfce00f
009728060203     c                   enddo
009729060203      * ?  Rettifiche
009730060203     c                   read      wxfre00f
009731060203     c                   dow       not %EoF(wxfre00f)
009732060203     c*
009733060203     c     kfre          chain     fifre02l
009734060203     c                   if        %Found(fifre02l)
009735060203     c                   write     wxfrexxx
009736060203     c                   else
009737060203     c                   write     fifre000
009738060203     c                   end
009739060203     c*
009740060203     c                   read      wxfre00f
009741060203     c                   enddo
009742060203     c*
015100000607     c                   seton                                        lr
031500020205     C**-------------------------------------------------------------**
034900000607     c     *inzsr        begsr
034901060125     C**-------------------------------------------------------------**
035200000607     c     *entry        plist
035300060127     c                   parm                    UNILIB           10
035301060127     c                   parm                    GRULIB           10
035302060127     c                   parm                    FILLIB           10
035303060127      *----------*
035304060203     c     kftt          klist
035305060127     c                   kfld                    fttpdr
035306060127     c                   kfld                    ftttsr
035307060127     c                   kfld                    fttndc
035308060127     c                   kfld                    fttddc
035309060203      *----------*
035310060203     c     kftd          klist
035311060203     c                   kfld                    ftdpdr
035312060203     c                   kfld                    ftdtsr
035313060203     c                   kfld                    ftdndc
035314060203     c                   kfld                    ftdddc
035315060203     c                   kfld                    ftdaas
035316060203     c                   kfld                    ftdlnp
035317060203     c                   kfld                    ftdnrs
035318060203     c                   kfld                    ftdnsp
037600030429      *----------*
040600060127     c     kfce          klist
040601060127     c                   kfld                    FceFGS
040700060203     c                   kfld                    FcePDR
040701060203     c                   kfld                    FceTSR
040703060203     c                   kfld                    FceNDC
040704060203     c                   kfld                    FceDDC
040705060203     c                   kfld                    FceAAS
040706060203     c                   kfld                    FceLNP
040707060203     c                   kfld                    FceNRS
040708060203     c                   kfld                    FceNSP
040709060203     c                   kfld                    FceCCE
040710060127      *----------*
040711060127     c     kfre          klist
040712060127     c                   kfld                    fretip
040713060203     c                   kfld                    frepdr
040714060203     c                   kfld                    freddc
040715060203     c                   kfld                    fretsr
040716060203     c                   kfld                    frecre
040717060203     c                   kfld                    frepno
041001060127      *----------*
041400020402     c                   endsr
043804060125     O*------------------------------------------------------*
