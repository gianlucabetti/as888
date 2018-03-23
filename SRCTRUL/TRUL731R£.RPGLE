000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200011011      *
000300090520      * ------------------------------------------------------------------------------------
000400090520      *  Routine reperimento e verifica limiti per tipo servizio
000500090520      * ------------------------------------------------------------------------------------
000600011011      *
000700090520      *  Il programma ricerca i limiti in base al tipo servizio passato e li confronta con
000800090520      *  in valori ricevuti in input
000801090520      *  impostando il flag corrispondente = '1' nel caso di superamento dei limiti previsti.
000803090520      *  Se non trovata tabella per il tipo servizio viene restituito errore.
000900011030
001400090520     d dlts          e ds
001500090520     d trul73ds      e ds
001600011029     d tibs02DS      e ds
001800011030     d kpjba         e ds
002600011030
002700011217     d xx              s              3  0
002800011217     d yy              s              3  0
003100011019
003200011019      *----------------------------------------------------------------
003300011019
003400011029     c     *entry        plist
003500090520     c                   parm                    trul73ds
004000011019
004100090520     c                   clear                   o73lclm
004200090520     c                   clear                   O73FCLM
004300090520     c                   clear                   O73LCLF
004400090520     c                   clear                   O73FCLF
004500090520     c                   clear                   O73LKGM
004600090520     c                   clear                   O73FKGM
004700090520     c                   clear                   O73LKGF
004800090520     c                   clear                   O73FKGF
004900090520     c                   clear                   O73LMCM
005000090520     c                   clear                   O73FMCM
005001090520     c                   clear                   O73LMCF
005002090520     c                   clear                   O73FMCF
005003090520     c                   clear                   O73ERR
005004090520     c                   clear                   O73MSG
005900011031
006000011031      * Controllo validità tipo lancio
006100090520     c                   if        i73tla <> 'C' and i73tla <> 'L' and
006200090520     c                             i73tla <> *blanks
006300090520     c                   clear                   i73tla
006400011031     c                   endif
006500011019
006600011019      * Se Tipo lancio 'C' --> solo chiusura file
006700090520     c                   if        i73tla <> 'C'
006800011029     c                   exsr      elabora
006900011029     c                   endif
007000011011
007100090520     c                   if        i73tla <> *blanks
007200011031     c                   eval      *inlr = *on
007300011029     c                   else
007400011031     c                   eval      *inrt = *on
007500011029     c                   endif
007600011011
007700011011      *----------------------------------------------------
007800011019      * ELABORAZIONE
007900011011      *----------------------------------------------------
008000011029     c     elabora       begsr
008100011106
014300090520      * aggancio la tabella LTS
014500090520     c                   clear                   dlts
014600011030     c                   clear                   tibs02ds
014700011031     c                   movel     'C'           t02mod
014800011029     c                   movel     knsif         t02sif
014900090520     c                   movel     'LTS'         t02cod
015000090520     c                   movel(p)  i73tsp        t02ke1
015100011029     c                   call      'TIBS02R'
015200011030     c                   parm                    kpjba
015300011030     c                   parm                    tibs02ds
015400011030     c                   if        t02err = *blanks
015500090520     c                   movel     t02uni        dlts
015700020123     c                   else
015701090610     c* errore se tabella non trovata o annullata
015801090520     c                   eval      o73err='E'
015802090520     c                   eval      o73msg='Non trovata tabella per il tipo serv-
015803090520     c                             izio richiesto'
016000090520     c                   leavesr
016200020123     c                   endif
016300011030
018400090520     c                   z-add     §LTSLCLM      o73lclm
018500090520     c                   z-add     §LTSLCLF      o73lclf
018600090520     c                   z-add     §LTSLPKM      o73lkgm
018700090520     c                   z-add     §LTSLPKF      o73lkgf
018800090520     c                   z-add     §LTSLMCM      o73lmcm
018900090520     c                   z-add     §LTSLMCF      o73lmcf
018901090520
019000090520      * controllo i valori
019001090520      * . .  Colli
019400090520     c                   if        i73ncl > o73lclm
019500090520     c                   eval      o73fclm = '1'
019600011030     c                   endif
019700090520     c                   if        i73ncl > o73lclf
019800090520     c                   eval      o73fclf = '1'
019900011030     c                   endif
019901090520      * . .  peso kg
019902090520     c                   if        i73psk > o73lkgm
019903090520     c                   eval      o73fkgm = '1'
019904090520     c                   endif
019905090520     c                   if        i73psk > o73lkgf
019906090520     c                   eval      o73fkgf = '1'
019907090520     c                   endif
019908090520      * . .  volume mc
019909090520     c                   if        i73vmc > o73lmcm
019910090520     c                   eval      o73fmcm = '1'
019911090520     c                   endif
019912090520     c                   if        i73vmc > o73lmcf
019913090520     c                   eval      o73fmcf = '1'
019914090520     c                   endif
021000011029
021100011029     c                   endsr
