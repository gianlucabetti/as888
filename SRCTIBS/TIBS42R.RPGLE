000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000201130930     D kpjba         e ds
000400131001     D tibs42ds      e ds
000401131001     D tibs34ds      e ds                  inz
000801131001     D ddatiute      e ds                  inz
000802131001     D azuteds       e ds                  extname(AZUTE00F) inz
000803131001     D dute01        e ds                  inz
000804131001     D uteautds      e ds                  inz
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
002108131001     c                   ENDIF
002109131014     c*                  if        utefil = 046 and
002110131014     c*                            %subst(uteute:1:3) = 'EDP'
002111131014     c*                  move      *all'S'       o42uni
002112131014     c*                  exsr      uscita
002113131014     c*                  endif
002114131003      *
002115130930     c                   movel     utefaf        dute01
002116130930     c                   movel     §utedsaut     uteautds
002117130930     c                   if        §uteautor = 'NO'
002118131001     c                   move      *all'N'       o42uni
002119131001     c                   move      'E'           o42err
002120131001     c                   eval      o42errmsg = 'Profilo non abilitato alla -
002121131001     c                             filiale '
002122130930     c                   exsr      uscita
002123130930     c                   endif
002124130930      * Reperimento dei P.O. gestibili dall'utente
002125130930     c                   clear                   TRUL31DS
002126130930     c                   eval      I31abi = §uteautor
002127130930     c                   eval      I31cdi = DUTdis
002128130930     c                   eval      I31car = DUTare
002129130930     c                   eval      I31cpo = DUTpou
002130130930     c                   call      'TRUL31R'
002131130930     c                   parm                    KPJBA
002132130930     c                   parm                    TRUL31DS
002133130930     c                   if        o31pog > *zeros
002134130930     c                   movea     o31pog        $pog
002135130930     c                   else
002136131001     c                   move      *all'N'       o42uni
002137131001     c                   move      'E'           o42err
002138131001     c                   eval      o42errmsg = 'Profilo non abilitato alla -
002139131001     c                             filiale '
002140130930     c                   exsr      uscita
002141130930     c                   endif
002142131001     c                   move      i42pge        pgealfa           3
002143131001     c     pgealfa       lookup    $pog                                   99
002144130930     c                   if        *in99
002145131001     c                   movel     uteautds      o42uni
002146131003     c                   exsr      uscita
002147130930     c                   else
002148131001     c                   move      *all'N'       o42uni
002149131001     c                   move      'E'           o42err
002150131001     c                   eval      o42errmsg = 'Profilo non abilitato alla -
002151131001     c                             filiale '
002152130930     c                   exsr      uscita
002153130930     c                   endif
002154130930
002801130930     c*-------------------------------------------------------
002802130930     c     uscita        begsr
002803130930      *--------------------------------------------------------
002804130930     c                   eval      *inLR = *on
002805131003     c                   return
002900130930     c                   endsr
