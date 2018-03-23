000100071205     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000200061227
000300061227      *------------------------------------------------------------------------*
000400080627      *  DRIVER di aggiornamento del FIPCT00F del record di fase "COK"
000500061227      *
000600080627      *    Interroga se la bolla è da Visionare perchè ci sono delle INFO
000700080627      *     quindi se è presente il record "COK"
000800080627      *    e se è da flaggare come già Visionata.
000900061227      *
001000061227      *------------------------------------------------------------------------*
001100080805     Ffipct02l  iF   E           K DISK    infds(pctds)
001200080805     f                                     rename(fipct000:fipct02)
001300080627     FfiPCT00f  uF   E             DISK
001400061227
001500061227      *------------------------------------------------------------------------*
001600061227      *  RIEPILOGO INDICATORI
001700061227      *------------------------------------------------------------------------*
001800071205
001900061227      *------------------------------------------------------------------------*
002000080627     d kpjba         e ds
002100080627     d fidg45ds      e ds
002200080627
002300080627     d pctds           ds
002400080627     d  pct_nrr              397    400b 0
002500080627      *
002600080627     d DataEora        ds
002700080627     d Data                           8  0
002800080627     d Ora                            6  0
002900080627      *
003000080627     d nrrPCT          s              9  0
003100080627     d pcttrdCOK       s                   like(pctTRD)
003200080627     d                                     inz('COK')
003300080627     d fipctCOKds    e ds
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
004500080627     C                   z-add     c45aas        pctAAS
004600080627     C                   z-add     c45lnp        pctLNP
004700080627     C                   z-add     c45nrs        pctNRS
004800080627     C                   z-add     c45nsp        pctNSP
004900080805     C                   z-add     c45fgs        pctfgs
005000080805     C                   z-add     c45ndc        pctndc
005100080805     C                   z-add     c45codaut     pctcodaut
005200080627      * Aggancia x Bolla
005300080627      *     il record del "COK"
005400080805     C     Kpct_02L      chain     fiPCT02L
005500080627      *
005600080805     c                   if        %Found(fiPCT02L)
005700080627      * carica la DS
005800080627     c                   eval      fiPCTcOKds = pctDATI
005900080627      *
006000080627      *     ?   se è da visionare --> ossia da avere presente    ?
006100101103     c                   If        §PCTFLGASS <>' ' and
006200080627     c                             §PCTFLGVIS = *blank
006300080627      *
006400080627      *  imposta sempre il flag di ritorno
006500080627      *   da restituire al chiamante
006600101126     c******             eval      c45daVisio = 'S'
006700101126     c                   eval      c45daVisio = §PCTFLGASS
006800101126     c                   eval      c45NOTASS  = §PCTNOTASS
006900080805     c                   eval      c45dtorino = pctdtorin
007000080627      *
007100080627      *  Aggiorna il record come visionato
007200080627     c                   if        c45AggVis = 'S'
007300080627      *
007400080627     c                   z-add     pct_nrr       nrrPCT
007500080627     C     nrrPCT        chain     fiPCT00F
007600080627      *
007700080627     c                   if        %Found(fiPCT00F)
007800080627     c                   eval      fiPCTcOKds = pctDATI
007900080627     c                   move      'S'           §PCTFLGVIS
008000080627     c                   eval      pctDATI   = fiPCTcOKds
008100080627     c                   move      DataEora      pctDTORIN
008200080627     c                   eval      pctPDAAS4 = 'A'
008300080627     c                   update    fiPCT000                             99
008400080627     c   99              eval      c45AggVis = 'N'
008500080627     c   99              EVAL      c45MSG = 'Il record non è stato aggiornato'
008600080627     c   99              move      '1'           c45ERR
008700080627     c                   end
008800080627      *
008900080627     c                   end
009000080627      *
009100080627     c                   endIf
009200080627      *
009300080627     c                   ELSE
009400080627     c                   EVAL      c45MSG = 'Non trovato il record su PCT'
009500080627     c                   move      '1'           c45ERR
009600080627     c                   end
009700080627      *
009800080311     c     fine          tag
009900080627     c                   eval      kpjbu =  fidg45ds
010000061227     c                   eval      *inlr = *on
010100080227      * ?_____________________________________________________
010200061227     c     *inzsr        begsr
010300080227      * ?_____________________________________________________
010400061227     c     *entry        plist
010500061227     c                   parm                    kpjba
010600061227
010700080627     c                   clear                   fidg45ds
010800080627     c                   eval      fidg45ds = kpjbu
010900070124
011000080627     c                   eval      Data  = *DATE
011100080627     c                   time                    ora
011200070122      * Richiamato da preparazione dati di massa
011300070115
011400080627     c                   clear                   c45err
011500080627     c                   clear                   c45msg
011600080627     c                   clear                   c45daVisio
011700101126     c                   clear                   c45NOTASS
011800080627     C*
011900080805     C     Kpct_02L      KLIST
012000080805     C                   KFLD                    pctfgs
012100080805     C                   KFLD                    pctndc
012200080805     C                   KFLD                    pctcodaut
012300080805     C                   KFLD                    pcttrdCOK
012400080627     C                   KFLD                    pctAAS
012500080627     C                   KFLD                    pctLNP
012600080627     C                   KFLD                    pctNRS
012700080627     C                   KFLD                    pctNSP
012800061227
012900061227     c                   endsr
013000080627      * ?_____________________________________________________
