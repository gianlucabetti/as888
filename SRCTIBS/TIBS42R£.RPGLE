000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000201130930     D kpjba         e ds
000400130930     D ficn70ds1     e ds
000801130930     D ddatiute      e ds
000802130930     D azuteds       e ds                  extname(AZUTE00F)
000803130930     D dute01        e ds
000804130930     D uteautds      e ds
000807130930       // -?Caricamento filiali gestite dall'utente?
000808130930     d TRUL31ds      e ds                  inz
000809130930     d   $POg                 10    759    inz   dim(250)
001800000607      *--------------------------------------------------------------------
001900000607     c
002000000607     c     *entry        plist
002100130930     c                   parm                    ficn70ds1
002101130930     c     *dtaara       define    §azute        azuteds
002102130930     c     *dtaara       define    §datiute      ddatiute
002103130930     C                   in(E)     *dtaara
002104130930     C                   IF        %Error  or  RSUT = *blanks
002105130930     C                   call      'TIBS34R'
002106130930     C                   parm                    Tibs34Ds
002107130930     C                   in        *dtaara
002108130930     c                   movel     utefaf        dute01
002109130930     c                   movel     §utedsaut     uteautds
002110130930     c                   if        §uteautor = 'NO'
002111130930     c                   move      *all'N'       o70uni
002112130930     c                   exsr      uscita
002113130930     c                   endif
002114130930      * Reperimento dei P.O. gestibili dall'utente
002115130930     c                   clear                   TRUL31DS
002116130930     c                   eval      I31abi = §uteautor
002117130930     c                   eval      I31cdi = DUTdis
002118130930     c                   eval      I31car = DUTare
002119130930     c                   eval      I31cpo = DUTpou
002120130930     c                   call      'TRUL31R'
002121130930     c                   parm                    KPJBA
002122130930     c                   parm                    TRUL31DS
002123130930     c                   if        o31pog > *zeros
002124130930     c                   movea     o31pog        $pog
002127130930     c                   else
002128130930     c                   move      *all'N'       o70uni
002129130930     c                   exsr      uscita
002132130930     c                   endif
002133130930     c     i70pog        lookup    $pog                                   99
002134130930     c                   if        *in99
002135130930     c                   movel     uteautds      o70uni
002136130930     c                   else
002137130930     c                   move      *all'N'       o70uni
002138130930     c                   exsr      uscita
002139130930     c                   endif
002140130930     c
002141130930     c                   endif
002148130930
002801130930     c*-------------------------------------------------------
002802130930     c     uscita        begsr
002803130930      *--------------------------------------------------------
002804130930     c                   eval      *inLR = *on
002900130930     c                   endsr
