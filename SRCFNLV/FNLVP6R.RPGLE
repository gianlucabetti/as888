000100070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000101080328     h dftactgrp(*no) actgrp(*caller)
000200070209
000300070209      *------------------------------------------------------------------------*
000400080327      *               Record CRI
000600070209      *
000700070209      *------------------------------------------------------------------------*
000800070209
001300080327     ffnlvp3d   cf   e             workstn
001400070209
004000070216      * ? V A R I A B I L I ?
004100070531     d dataeur         s               d   datfmt(*eur)
004101080328     d dataiso         s               d
008900080328     d kpjba         e ds
008901080328     d fiprdcrids    e ds
010300070209      *------------------------------------------------------------------------*
010301080327     c     *entry        plist
010303080328     c                   parm                    kpjba
010304080328     c                   parm                    plgdati         250
010305080327     c                   parm                    esitol            1
082400080327     c                   eval      fiprdcrids= plgdati
082401080327     c                   exsr      sr_pulvid
082500070508      * filiale distinta
082501080327     c                   monitor
082600080327     c                   move      §crifgs       v3fgs
082601080327     c                   on-error
082602080327     c                   endmon
082700070508      * numero distinta
082701080327     c                   monitor
082800080327     c                   move      §crindc       v3ndc
082801080327     c                   on-error
082802080327     c                   endmon
082900070514      * data/ora evento
082901080328     c                   clear                   errore            1
082902080327     c                   monitor
083000080327     c                   movel     §cridtorin    v3data
083001080327     c                   on-error
083002080328     c                   eval      errore = '1'
083003080327     c                   endmon
083004080328     c                   if        errore = ' '
083005080328     c                   move      v3data        dataiso
083006080328     c                   move      dataiso       dataeur
083007080328     c                   move      dataeur       v3data
083008080328     c                   end
083009080327     c                   monitor
083100080327     c                   move      §cridtorin    v3ora
083101080327     c                   on-error
083102080327     c                   endmon
083103080327     c                   monitor
083104080327     c                   move      §CRINRBANR    v3nrbanr
083105080327     c                   on-error
083106080327     c                   endmon
083107080327     c                   monitor
083108080327     c                   move      §CRINRCOLR    v3nrcolr
083109080327     c                   on-error
083110080327     c                   endmon
083111080512     C                   EVAL      V3SPCCOD= §CRISPCCOD
085800070508
094600080327     c                   exfmt     lvp4d04
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
098400080327     c                   clear                   v3nrbanr
098500080327     c                   clear                   v3nrcolr
100700080327
100800080327     c                   endsr
100900080327
