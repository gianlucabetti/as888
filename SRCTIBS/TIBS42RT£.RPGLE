000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000201130930     D kpjba         e ds
000400131001     D tibs42ds      e ds
000401131001     D tibs34ds      e ds
000801130930     D ddatiute      e ds
000802130930     D azuteds       e ds                  extname(AZUTE00F)
000803130930     D dute01        e ds
000804130930     D uteautds      e ds
000807130930       // -?Caricamento filiali gestite dall'utente?
000808130930     d TRUL31ds      e ds                  inz
000809131001     d $POg            s              3    inz   dim(250)
001800000607      *--------------------------------------------------------------------
001900000607     c
002000000607     c     *entry        plist
002100131001     c                   parm                    tibs42ds
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
002111131001     c                   move      *all'N'       o42uni
002112131001     c                   move      'E'           o42err
002113131001     c                   eval      o42errmsg = 'Profilo non abilitato alla -
002114131001     c                             filiale '
002115130930     c                   exsr      uscita
002116130930     c                   endif
002117130930      * Reperimento dei P.O. gestibili dall'utente
002118130930     c                   clear                   TRUL31DS
002119130930     c                   eval      I31abi = §uteautor
002120130930     c                   eval      I31cdi = DUTdis
002121130930     c                   eval      I31car = DUTare
002122130930     c                   eval      I31cpo = DUTpou
002123130930     c                   call      'TRUL31R'
002124130930     c                   parm                    KPJBA
002125130930     c                   parm                    TRUL31DS
002126130930     c                   if        o31pog > *zeros
002127130930     c                   movea     o31pog        $pog
002128130930     c                   else
002129131001     c                   move      *all'N'       o42uni
002130131001     c                   move      'E'           o42err
002131131001     c                   eval      o42errmsg = 'Profilo non abilitato alla -
002132131001     c                             filiale '
002133130930     c                   exsr      uscita
002134130930     c                   endif
002135131001     c                   move      i42pge        pgealfa           3
002136131001     c     pgealfa       lookup    $pog                                   99
002137130930     c                   if        *in99
002138131001     c                   movel     uteautds      o42uni
002139130930     c                   else
002140131001     c                   move      *all'N'       o42uni
002141131001     c                   move      'E'           o42err
002142131001     c                   eval      o42errmsg = 'Profilo non abilitato alla -
002143131001     c                             filiale '
002144130930     c                   exsr      uscita
002145130930     c                   endif
002146130930     c
002147130930     c                   endif
002148130930
002801130930     c*-------------------------------------------------------
002802130930     c     uscita        begsr
002803130930      *--------------------------------------------------------
002804130930     c                   eval      *inLR = *on
002900130930     c                   endsr
