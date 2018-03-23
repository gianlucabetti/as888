000100110324     /*PRM tgtrls(*current)
000200110324     /*END
000300070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400080328     h dftactgrp(*no) actgrp(*caller)
000500070209
000600070209      *------------------------------------------------------------------------*
000700080328      *               sfl dettaglio colli
000800070209      *
000900070209      *------------------------------------------------------------------------*
001000070209
001100110324     fTITAS30C  if   e           k disk    usropn
001200110324     f                                     extdesc('GAITRAGRPS/TITAS30C')
001300080328     ffnlvp3d   cf   e             workstn
001400070209
001500070216      * ? V A R I A B I L I ?
001600070531     d dataeur         s               d   datfmt(*eur)
001700080328     d dataiso         s               d
001800110324     d W3Ctipappl      s              1    inz
001900110324     d $GiaRicev       s               n   inz
002000110324     d wTBL            s                   like(TAStbl)  inz
002100110324
002200110324      * ? D S   I N T E R N E / E S T E R N E ?
002300080328     d kpjba         e ds
002400170719     d FIPRDCSADS    e ds
002401170719      **
002500110324     d TNSB51ds      e ds                  inz
002600110324      /copy gaitrasrc/srcProtoPI,FNLR36R
002700070209      *------------------------------------------------------------------------*
002800080327     c     *entry        plist
002900080328     c                   parm                    kpjba
003000080328     c                   parm                    plgdati         250
003100080328     c                   parm                    esitol            1
003101170719
003200170719     c                   eval      FIPRDCSADS=plgdati
003201170719
003300110324     c                   if        Not  $GiaRicev
003400110324     c                   movel     kpjbu         W3Ctipappl
003500110324     c                   eval      $GiaRicev = *on
003600110324     c                   endif
003601170719
003700080327     c                   exsr      sr_pulvid
003800070508      * filiale distinta
003900080327     c                   monitor
004000170719     c                   move      §csafgs       v3fgs
004100080327     c                   on-error
004200080327     c                   endmon
004300070508      * numero distinta
004400080327     c                   monitor
004500170719     c                   move      §csandc       v3ndc
004600080327     c                   on-error
004700080327     c                   endmon
004800070514      * data/ora evento
004900080328     c                   clear                   errore            1
005000080327     c                   monitor
005100170719     c                   movel     §csadtorin    v3data
005200080327     c                   on-error
005300080328     c                   eval      errore = '1'
005400080327     c                   endmon
005500080328     c                   if        errore = ' '
005600080328     c                   move      v3data        dataiso
005700080328     c                   move      dataiso       dataeur
005800080328     c                   move      dataeur       v3data
005900080328     c                   end
006000080327     c                   monitor
006100170719     c                   move      §csadtorin    v3ora
006200080327     c                   on-error
006300080327     c                   endmon
006400080327      * spedizione
006500080327     c                   monitor
006600170719     c                   move      §csaaas       v3aas
006700080327     c                   on-error
006800080327     c                   endmon
006900080327     c                   monitor
007000170719     c                   move      §csalnp       v3lnp
007100080327     c                   on-error
007200080327     c                   endmon
007300080327     c                   monitor
007400170719     c                   move      §csanrs       v3nrs
007500080327     c                   on-error
007600080327     c                   endmon
007700080327     c                   monitor
007800170719     c                   move      §csansp       v3nsp
007900080327     c                   on-error
008000080327     c                   endmon
008100080328      * segnacollo
008200080328     c                   monitor
008300170719     c                   move      §csafls       v3fls
008400080328     c                   on-error
008500080328     c                   endmon
008600080328     c                   monitor
008700170719     c                   move      §csalna       v3lna
008800080328     c                   on-error
008900080328     c                   endmon
009000080328     c                   monitor
009100170719     c                   move      §csanrser     v3nrser
009200080328     c                   on-error
009300080328     c                   endmon
009400080328     c                   monitor
009500170719     c                   move      §csansc       v3nsc
009600080328     c                   on-error
009700080328     c                   endmon
009800170719      * spunta
009900080327     c                   monitor
010000170719     c                   movel     §CSASPUAUT    v3spuaut
010100080327     c                   on-error
010200080327     c                   endmon
010300170719     c* anomalia
010900080327     c                   monitor
011000170719     c                   movel     §CSACANCON    v3cancon
011100080327     c                   on-error
011200080327     c                   endmon
011300080328     c* inserito da spc
011400080327     c                   monitor
011500170719     c                   movel     §CSASPCCOD    v3spccod
011600080327     c                   on-error
011700080327     c                   endmon
011701170719
011800170719     c                   exfmt     lvp4d20
011900070508
012000070508      * f3=fine
012100070508     c                   if        *inkc
012200080327     c                   eval      esitol='1'
012300080327     c                   seton                                        lr
012400080327     c                   return
012500070508     c                   endif
012600110324
012700110324      *?F9=Visualizzazione Bolla?
012800110324if  1c                   if        *inKI
012900110324if  2c                   IF        %subst(KNSIF:1:3) = 'GAI'
013000110324if  3c                   if        wTBL = *blank
013100110324     c                   exsr      sr_GetTBL
013200110324e   3c                   endif
013300110324     c                   clear                   TNSB51ds
013400110324     c                   eval      I51aas = V3aas
013500110324     c                   eval      I51lnp = V3lnp
013600110324     c                   eval      I51nrs = V3nrs
013700110324     c                   eval      I51nsp = V3nsp
013800110324     c                   eval      I51tbl = wTBL
013900110324     c                   eval      kpjbu  = TNSB51ds
014000110324     c                   call      'TNSB51R'
014100110324     c                   parm                    kpjba
014200110324     c                   eval      TNSB51ds = kpjbu
014300110324if  3c                   if        O51f03 = *on
014400110324     c                   eval      EsitoL = *on
014500110324e   3c                   endif
014600110324x   2c                   ELSE
014700110324     c                   clear                   FNLR36ds
014800110324     c                   eval      FNLR36ds.LR36aas = V3aas
014900110324     c                   eval      FNLR36ds.LR36lnp = V3lnp
015000110324     c                   eval      FNLR36ds.LR36nrs = V3nrs
015100110324     c                   eval      FNLR36ds.LR36nsp = V3nsp
015200110324     c                   eval      FNLR36ds.LR36flg = '1'
015300110324     c                   eval      kpjbu  = FNLR36ds
015400110324     c                   call      'FNLR36R'
015500110324     c                   parm                    kpjba
015600110324     c                   eval      FNLR36ds = kpjbu
015700110324if  3c                   if        FNLR36ds.LR36f03  = *on
015800110324if  4c                   if        O51f03 = *on
015900110324     c                   eval      EsitoL = *on
016000110324e   4c                   endif
016100110324e   3c                   endif
016200110324     c                   eval      *inLR  = *on
016300110324     c                   return
016400110324e   2c                   ENDIF
016500110324e   1c                   endif
016600070508
016700070508      * f12=ritorno
016800070508     c                   if        *inkl
016900080327     c                   eval      esitol='2'
017000080327     c                   seton                                        lr
017100080327     c                   return
017200070508     c                   endif
017300080327      *------------------------------------------------------------------------*
017400080327      *? PULIZIA DELLA VIDEATA  ?
017500080327      *------------------------------------------------------------------------*
017600080327     c     sr_pulvid     begsr
017700080327
017800080327     c                   clear                   v3fgs
017900080327     c                   clear                   v3ndc
018000080327     c                   clear                   v3data
018100080327     c                   clear                   v3ora
018200080327
018300080327     c                   clear                   v3aas
018400080327     c                   clear                   v3lnp
018500080327     c                   clear                   v3nrs
018600080327     c                   clear                   v3nsp
018700080328     c                   clear                   v3fls
018800080328     c                   clear                   v3lna
018900080328     c                   clear                   v3nrser
019000080328     c                   clear                   v3nsc
019100080327     c                   clear                   v3data
019101170719
019102170719
019200080328     c                   clear                   v3cancon
019300170719     c                   clear                   v3spuaut
019500170719     c                   clear                   v3spccod
019600110324
019700110324      * -?Abilitazione F9=Interrogazione Spedizione?
019800110324     c                   eval      *in09 = (W3Ctipappl = 'C')
019900080327
020000080327     c                   endsr
020100110324
020200110324      /free
020300110324
020400110324       //--------------------------------------------------------------
020500110324       //?Reperimento "Tipo Bolla" per bolla di sede.                  ?
020600110324       //--------------------------------------------------------------
020700110324       BEGSR  sr_GetTBL;
020800110324
020900110324         if  Not  %open(TITAS30C);
021000110324           open  TITAS30C;
021100110324         endif;
021200110324
021300110324         chain  ( V3aas : V3lnp : V3nrs : V3nsp )  TITAS30C;
021400110324
021500110324         if  %found(TITAS30C);
021600110324           wTBL = TAStbl;
021700110324         endif;
021800110324
021900110324       ENDSR;
022000110324
022100110324      /end-free
