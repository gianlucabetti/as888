000100071205     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000200061227
000300061227      *------------------------------------------------------------------------*
000400140207      *  DRIVER di aggiornamento del FIPro10F del record di fase "OK"
000500061227      *
000600080627      *    Interroga se la bolla è da Visionare perchè ci sono delle INFO
000700140207      *     quindi se è presente il record "OK"
000800080627      *    e se è da flaggare come già Visionata.
000900061227      *
001000061227      *------------------------------------------------------------------------*
001100140207     Ffipro11l  iF   E           K DISK    infds(prods)
001200140207     f                                     rename(fipro100:fipro11)
001300140207     FfiPro10f  uF   E             DISK
001400061227
001500061227      *------------------------------------------------------------------------*
001600061227      *  RIEPILOGO INDICATORI
001700061227      *------------------------------------------------------------------------*
001800071205
001900061227      *------------------------------------------------------------------------*
002000080627     d kpjba         e ds
002100140207     d fior82ds      e ds
002200080627
002300140207     d prods           ds
002400140207     d  pro_nrr              397    400b 0
002500080627      *
002600080627     d DataEora        ds
002700080627     d Data                           8  0
002800080627     d Ora                            6  0
002900080627      *
003000140207     d nrrpro          s              9  0
003100140207     d protrdOK        s                   like(proTRD)
003200140207     d                                     inz('OK ')
003300140207     d fiproOKds     e ds
003400061227
003500061227     d wlbdat          ds
003600061227     d  g02dat                 1      8  0
003700061227     d  g02inv                 9     16  0
003800061227     d  g02err                17     17
003900061227     d  g02tgi                18     22  0
004000080227      * ?_______________________________________________________________________
004100080627      *   MAIN program
004200080627      * ?_______________________________________________________________________
004300080627      *
004400080627      *  Aggancia il record del "COK"
004500140207     C                   z-add     c82POE        proPOE
004600140207     c                   z-add     c82NSR        proNSR
004700140207     C                   z-add     c82NOR        proNOR
004800140207     C                   z-add     c82NRV        proNRV
004900140207     C                   z-add     c82fgs        profgs
005000140207     C                   z-add     c82ndc        prondc
005100140207     C                   z-add     c82codaut     procodaut
005200080627      * Aggancia x Bolla
005300080627      *     il record del "COK"
005400140207     C     Kpro_11L      chain     fipro11L
005500080627      *
005600140207     c                   if        %Found(fipro11L)
005700080627      * carica la DS
005800140207     c                   eval      fiproOKds = proDATI
005900080627      *
006000080627      *     ?   se è da visionare --> ossia da avere presente    ?
006100140207     c                   If        §proFLGASS <>' ' and
006200140207     c                             §proFLGVIS = *blank
006300080627      *
006400080627      *  imposta sempre il flag di ritorno
006500080627      *   da restituire al chiamante
006600140207     c******             eval      c82daVisio = 'S'
006700140207     c                   eval      c82daVisio = §proFLGASS
006800140207     c                   eval      c82NOTASS  = §proNOTASS
006900140210     c                   eval      c82dtorino = prodatora
007000080627      *
007100080627      *  Aggiorna il record come visionato
007200140207     c                   if        c82AggVis = 'S'
007300080627      *
007400140207     c                   z-add     pro_nrr       nrrpro
007500140207     C     nrrpro        chain     fipro10F
007600080627      *
007700140207     c                   if        %Found(fipro10F)
007800140207     c                   eval      fiproOKds = proDATI
007900140207     c                   move      'S'           §proFLGVIS
008000140207     c                   eval      proDATI   = fiproOKds
008100140210     c                   move      DataEora      proDatora
008300140207     c                   update    fipro100                             99
008400140207     c   99              eval      c82AggVis = 'N'
008500140207     c   99              EVAL      c82MSG = 'Il record non è stato aggiornato'
008600140207     c   99              move      '1'           c82ERR
008700080627     c                   end
008800080627      *
008900080627     c                   end
009000080627      *
009100080627     c                   endIf
009200080627      *
009300080627     c                   ELSE
009400140207     c                   EVAL      c82MSG = 'Non trovato il record su pro'
009500140207     c                   move      '1'           c82ERR
009600080627     c                   end
009700080627      *
009800080311     c     fine          tag
009900140210     c                   eval      kpjbu =  fior82ds
010000061227     c                   eval      *inlr = *on
010100080227      * ?_____________________________________________________
010200061227     c     *inzsr        begsr
010300080227      * ?_____________________________________________________
010400061227     c     *entry        plist
010500061227     c                   parm                    kpjba
010600061227
010700140210     c                   clear                   fior82ds
010800140210     c                   eval      fior82ds = kpjbu
010900070124
011000080627     c                   eval      Data  = *DATE
011100080627     c                   time                    ora
011200070122      * Richiamato da preparazione dati di massa
011300070115
011400140207     c                   clear                   c82err
011500140207     c                   clear                   c82msg
011600140207     c                   clear                   c82daVisio
011700140207     c                   clear                   c82NOTASS
011800080627     C*
011900140207     C     Kpro_11L      KLIST
012000140207     C                   KFLD                    profgs
012100140207     C                   KFLD                    prondc
012101140207     C                   KFLD                    protrdok
012200140207     C                   KFLD                    procodaut
012400140207     C                   KFLD                    proPOE
012500140207     C                   KFLD                    proNSR
012600140207     C                   KFLD                    proNOR
012700140207     C                   KFLD                    proNRV
012800061227
012900061227     c                   endsr
013000080627      * ?_____________________________________________________
