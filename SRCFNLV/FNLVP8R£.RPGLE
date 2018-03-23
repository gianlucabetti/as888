000100070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000101080328     h dftactgrp(*no) actgrp(*caller)
000200070209
000300070209      *------------------------------------------------------------------------*
000400080328      *               sfl dettaglio colli
000600070209      *
000700070209      *------------------------------------------------------------------------*
000800070209
001300080328     ffnlvp3d   cf   e             workstn
001400070209
004000070216      * ? V A R I A B I L I ?
004100070531     d dataeur         s               d   datfmt(*eur)
004101080328     d dataiso         s               d
008900080328     d kpjba         e ds
008901080328     d fiprdcedds    e ds
010300070209      *------------------------------------------------------------------------*
010301080327     c     *entry        plist
010303080328     c                   parm                    kpjba
010304080328     c                   parm                    plgdati         250
010305080328     c                   parm                    esitol            1
082400080328     c                   eval      fiprdcedds= plgdati
082401080327     c                   exsr      sr_pulvid
082500070508      * filiale distinta
082501080327     c                   monitor
082600080328     c                   move      §cedfgs       v3fgs
082601080327     c                   on-error
082602080327     c                   endmon
082700070508      * numero distinta
082701080327     c                   monitor
082800080328     c                   move      §cedndc       v3ndc
082801080327     c                   on-error
082802080327     c                   endmon
082900070514      * data/ora evento
082901080328     c                   clear                   errore            1
082902080327     c                   monitor
083000080328     c                   movel     §ceddtorin    v3data
083001080327     c                   on-error
083002080328     c                   eval      errore = '1'
083003080327     c                   endmon
083004080328     c                   if        errore = ' '
083005080328     c                   move      v3data        dataiso
083006080328     c                   move      dataiso       dataeur
083007080328     c                   move      dataeur       v3data
083008080328     c                   end
083009080327     c                   monitor
083100080328     c                   move      §ceddtorin    v3ora
083101080327     c                   on-error
083102080327     c                   endmon
083103080327      * spedizione
083104080327     c                   monitor
083105080328     c                   move      §cedaas       v3aas
083106080327     c                   on-error
083107080327     c                   endmon
083108080327     c                   monitor
083109080328     c                   move      §cedlnp       v3lnp
083110080327     c                   on-error
083111080327     c                   endmon
083112080327     c                   monitor
083113080328     c                   move      §cednrs       v3nrs
083114080327     c                   on-error
083115080327     c                   endmon
083116080327     c                   monitor
083117080328     c                   move      §cednsp       v3nsp
083118080327     c                   on-error
083119080327     c                   endmon
083120080328      * segnacollo
083121080328     c                   monitor
083122080328     c                   move      §cedfls       v3fls
083123080328     c                   on-error
083124080328     c                   endmon
083125080328     c                   monitor
083126080328     c                   move      §cedlna       v3lna
083127080328     c                   on-error
083128080328     c                   endmon
083129080328     c                   monitor
083130080328     c                   move      §cednrser     v3nrser
083131080328     c                   on-error
083132080328     c                   endmon
083133080328     c                   monitor
083134080328     c                   move      §cednsc       v3nsc
083135080328     c                   on-error
083136080328     c                   endmon
083137080328      * spunta consegna
083138080327     c                   monitor
083139080328     c                   movel     §cedspucon    v3spucon
083140080327     c                   on-error
083141080327     c                   endmon
083157080328     c* spunta al rientro
083158080327     c                   monitor
083159080328     c                   movel     §CEdspuspc    v3spuspc
083160080327     c                   on-error
083161080327     c                   endmon
083162080328     c* anomalia
083163080327     c                   monitor
083164080328     c                   movel     §CEdcancon    v3cancon
083165080327     c                   on-error
083166080327     c                   endmon
083167080328     c* inserito da spc
083168080327     c                   monitor
083169080328     c                   movel     §CEdflgspc    v3flgspc
083170080327     c                   on-error
083171080327     c                   endmon
094600080328     c                   exfmt     lvp4d06
094700070508
094800070508      * f3=fine
094900070508     c                   if        *inkc
095000080327     c                   eval      esitol='1'
095001080327     c                   seton                                        lr
095002080327     c                   return
095100070508     c                   endif
095200070508
095300070508      * f12=ritorno
095400070508     c                   if        *inkl
095401080327     c                   eval      esitol='2'
095403080327     c                   seton                                        lr
095404080327     c                   return
095600070508     c                   endif
095700080327      *------------------------------------------------------------------------*
095800080327      *? PULIZIA DELLA VIDEATA  ?
095900080327      *------------------------------------------------------------------------*
096000080327     c     sr_pulvid     begsr
096100080327
097900080327     c                   clear                   v3fgs
098000080327     c                   clear                   v3ndc
098100080327     c                   clear                   v3data
098200080327     c                   clear                   v3ora
098300080327
098501080327     c                   clear                   v3aas
098505080327     c                   clear                   v3lnp
098509080327     c                   clear                   v3nrs
098513080327     c                   clear                   v3nsp
098514080328     c                   clear                   v3fls
098515080328     c                   clear                   v3lna
098516080328     c                   clear                   v3nrser
098517080328     c                   clear                   v3nsc
098518080327     c                   clear                   v3data
098588080328     c                   clear                   v3cancon
098589080328     c                   clear                   v3spuspc
098590080328     c                   clear                   v3spucon
098591080328     c                   clear                   v3flgspc
100700080327
100800080327     c                   endsr
100900080327
