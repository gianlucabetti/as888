000100110324     /*PRM tgtrls(*current)
000200110324     /*END
000300070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400080328     h dftactgrp(*no) actgrp(*caller)
000500070209
000600070209      *------------------------------------------------------------------------*
000700080328      *               Record CET
000800070209      *
000900070209      *------------------------------------------------------------------------*
001000070209
001100110323     fTITAS30C  if   e           k disk    usropn
001200110324     f                                     extdesc('GAITRAGRPS/TITAS30C')
001300080327     ffnlvp3d   cf   e             workstn
001400070209
001500070216      * ? V A R I A B I L I ?
001600070531     d dataeur         s               d   datfmt(*eur)
001700080328     d dataiso         s               d
001800110322     d W3Ctipappl      s              1    inz
001900110323     d $GiaRicev       s               n   inz
002000110323     d wTBL            s                   like(TAStbl)  inz
002100110322
002200110322      * ? D S   I N T E R N E / E S T E R N E ?
002300080328     d kpjba         e ds
002400080328     d fiprdcetds    e ds
002500080331     d fnlvp2ds      e ds
002600110322     d TNSB51ds      e ds                  inz
002700110322      /copy gaitrasrc/srcProtoPI,FNLR36R
002800070209      *------------------------------------------------------------------------*
002900080327     c     *entry        plist
003000080328     c                   parm                    kpjba
003100080328     c                   parm                    plgdati         250
003200080327     c                   parm                    esitol            1
003300080327     c                   eval      fiprdcetds= plgdati
003400110323     c                   if        Not  $GiaRicev
003500110322     c                   movel     kpjbu         W3Ctipappl
003600110323     c                   eval      $GiaRicev = *on
003700110323     c                   endif
003800080327     c                   exsr      sr_pulvid
003900070508      * filiale distinta
004000080327     c                   monitor
004100080327     c                   move      呃etfgs       v3fgs
004200080327     c                   on-error
004300080327     c                   endmon
004400070508      * numero distinta
004500080327     c                   monitor
004600080327     c                   move      呃etndc       v3ndc
004700080327     c                   on-error
004800080327     c                   endmon
004900070514      * data/ora evento
005000080328     c                   clear                   errore            1
005100080327     c                   monitor
005200080327     c                   movel     呃etdtorin    v3data
005300080327     c                   on-error
005400080328     c                   eval      errore = '1'
005500080327     c                   endmon
005600080328     c                   if        errore = ' '
005700080328     c                   move      v3data        dataiso
005800080328     c                   move      dataiso       dataeur
005900080328     c                   move      dataeur       v3data
006000080328     c                   end
006100080327     c                   monitor
006200080327     c                   move      呃etdtorin    v3ora
006300080327     c                   on-error
006400080327     c                   endmon
006500080327      * spedizione
006600080327     c                   monitor
006700080327     c                   move      呃etaas       v3aas
006800080327     c                   on-error
006900080327     c                   endmon
007000080327     c                   monitor
007100080327     c                   move      呃etlnp       v3lnp
007200080327     c                   on-error
007300080327     c                   endmon
007400080327     c                   monitor
007500080327     c                   move      呃etnrs       v3nrs
007600080327     c                   on-error
007700080327     c                   endmon
007800080327     c                   monitor
007900080327     c                   move      呃etnsp       v3nsp
008000080327     c                   on-error
008100080327     c                   endmon
008200080327      * codice mancata consegna
008300080327     c                   monitor
008400080328     c                   movel     呃etcmc       v3cmc
008500080327     c                   on-error
008600080327     c                   endmon
008700080327      * data/ora mancata consegna
008800080328     c                   clear                   errore            1
008900080327     c                   monitor
009000080327     c                   movel     低ETDATA      v3datacon
009100080327     c                   on-error
009200080328     c                   eval      errore = '1'
009300080327     c                   endmon
009400080328     c                   if        errore = ' '
009500080328     c                   move      v3datacon     dataiso
009600080328     c                   move      dataiso       dataeur
009700080328     c                   move      dataeur       v3datacon
009800080328     c                   end
009900080327     c                   monitor
010000080327     c                   move      低ETORA       v3oracon
010100080327     c                   on-error
010200080327     c                   endmon
010300080327      * data/ora consegna richiesta
010400080328     c                   clear                   errore            1
010500080327     c                   monitor
010600080327     c                   movel     呃etdcr       v3dcr
010700080327     c                   on-error
010800080328     c                   eval      errore = '1'
010900080327     c                   endmon
011000080328     c                   if        errore = ' '
011100080328     c                   move      v3dcr         dataiso
011200080328     c                   move      dataiso       dataeur
011300080328     c                   move      dataeur       v3dcr
011400080328     c                   end
011500080327     c                   monitor
011600080327     c                   move      呃ethcr       v3hcr
011700080327     c                   on-error
011800080327     c                   endmon
011900080327     c* note
012000080327     c                   monitor
012100080328     c                   movel     低ETNOTAUT    v3notaut
012200080327     c                   on-error
012300080327     c                   endmon
012400080327     c* incassi
012500080327     c                   monitor
012600080328     c                   movel     低ETINCCAS    v3inccas
012700080327     c                   on-error
012800080327     c                   endmon
012900080327     c                   monitor
013000080328     c                   movel     低ETINCFAT    v3incfat
013100080327     c                   on-error
013200080327     c                   endmon
013300080327     c* cod. fiscale e part. iva
013400080327     c                   monitor
013500080328     c                   movel     低ETCODPIV    v3codpiv
013600080327     c                   on-error
013700080327     c                   endmon
013800080327     c                   monitor
013900080328     c                   movel     低ETCODISO    v3codiso
014000080327     c                   on-error
014100080327     c                   endmon
014200080327     c                   monitor
014300080328     c                   movel     低ETCODFIS    v3codfis
014400080327     c                   on-error
014500080327     c                   endmon
014600080327     c* firma
014700080327     c                   monitor
014800080328     c                   movel     低ETNOMFIR    v3nomfir
014900080327     c                   on-error
015000080327     c                   endmon
015100080327     c* riserva
015200080327     c                   monitor
015300080328     c                   movel     低ETRISERV    v3riserv
015400080327     c                   on-error
015500080327     c                   endmon
015600080327     c* consegna particolare
015700080327     c                   monitor
015800080328     c                   movel     低ETTC1       v3tc1
015900080327     c                   on-error
016000080327     c                   endmon
016100080327     c* bancali resi
016200080327     c                   monitor
016300080327     c                   move      低ETNRBANR    v3nrbanr
016400080327     c                   on-error
016500080327     c                   endmon
016600080327     c* inserito da spc
016700080327     c                   monitor
016800080328     c                   movel     低ETFLGSPC    v3flgspc
016900080327     c                   on-error
017000080327     c                   endmon
017100101027     c* Firma su ldv
017200101027     c                   monitor
017300101102     c                   movel     低ETldvfir    v3flgfir
017400101027     c                   on-error
017500101027     c                   endmon
017501120511      * coordinate geostazionarie
017502120511     c                   monitor
017503120511     c                   move      呃etxlo       v3xlo
017504120511     c                   move      呃etyla       v3yla
017505140121     c                   move      呃etritdoc    v3ritdoc
017506120511     c                   on-error
017507120511     c                   endmon
017600080327     c                   exfmt     lvp4d05
017700070508
017800070508      * f3=fine
017900070508     c                   if        *inkc
018000080327     c                   eval      esitol='1'
018100080327     c                   seton                                        lr
018200080327     c                   return
018300070508     c                   endif
018400110322
018500110322      *?F9=Visualizzazione Bolla?
018600110322if  1c                   if        *inKI
018700110322if  2c                   IF        %subst(KNSIF:1:3) = 'GAI'
018800110323if  3c                   if        wTBL = *blank
018900110323     c                   exsr      sr_GetTBL
019000110323e   3c                   endif
019100110322     c                   clear                   TNSB51ds
019200110322     c                   eval      I51aas = V3aas
019300110322     c                   eval      I51lnp = V3lnp
019400110322     c                   eval      I51nrs = V3nrs
019500110322     c                   eval      I51nsp = V3nsp
019600110323     c                   eval      I51tbl = wTBL
019700110322     c                   eval      kpjbu  = TNSB51ds
019800110322     c                   call      'TNSB51R'
019900110322     c                   parm                    kpjba
020000110322     c                   eval      TNSB51ds = kpjbu
020100110322if  3c                   if        O51f03 = *on
020200110322     c                   eval      EsitoL = *on
020300110322e   3c                   endif
020400110322x   2c                   ELSE
020500110322     c                   clear                   FNLR36ds
020600110322     c                   eval      FNLR36ds.LR36aas = V3aas
020700110322     c                   eval      FNLR36ds.LR36lnp = V3lnp
020800110322     c                   eval      FNLR36ds.LR36nrs = V3nrs
020900110322     c                   eval      FNLR36ds.LR36nsp = V3nsp
021000110322     c                   eval      FNLR36ds.LR36flg = '1'
021100110322     c                   eval      kpjbu  = FNLR36ds
021200110322     c                   call      'FNLR36R'
021300110322     c                   parm                    kpjba
021400110322     c                   eval      FNLR36ds = kpjbu
021500110322if  3c                   if        FNLR36ds.LR36f03  = *on
021600110322if  4c                   if        O51f03 = *on
021700110322     c                   eval      EsitoL = *on
021800110322e   4c                   endif
021900110322e   3c                   endif
022000110322     c                   eval      *inLR  = *on
022100110322     c                   return
022200110322e   2c                   ENDIF
022300110322e   1c                   endif
022400070508
022500070508      * f12=ritorno
022600070508     c                   if        *inkl
022700080327     c                   eval      esitol='2'
022800080327     c                   seton                                        lr
022900080327     c                   return
023000070508     c                   endif
023100080328      * f7=sfl dettaglio segnacolli
023200080328     c                   if        *inkg
023300080331     c                   clear                   fnlvp2ds
023400080331     c                   eval      ds2tiporcd ='CED'
023500090122     c                   z-add     低ETFGS       ds2fgs
023600090122     c                   eval      ds2ndc = %subst(fiprdcetds: 4: 6)
023700090122     c                   eval      ds2iddoc = %subst(fiprdcetds: 10: 16)
023800080331     c                   movel(p)  fnlvp2ds      kpjbu
023900080328     c                   call      'FNLVP2R'
024000080328     c                   parm                    kpjba
024100080331     c                   movel     kpjbu         fnlvp2ds
024101140206     c                   eval      *inLR  = *on
024200080328      * f3=fine
024300080331     c                   if        ds2esito <> ' '
024400080331     c                   eval      esitol =ds2esito
024500080328     c                   seton                                        lr
024600080328     c                   return
024700080328     c                   endif
024800080328
024900080328     c                   endif
025000080327      *------------------------------------------------------------------------*
025100080327      *? PULIZIA DELLA VIDEATA  ?
025200080327      *------------------------------------------------------------------------*
025300080327     c     sr_pulvid     begsr
025400080327
025500080327     c                   clear                   v3fgs
025600080327     c                   clear                   v3ndc
025700080327     c                   clear                   v3data
025800080327     c                   clear                   v3ora
025900080327
026000080327     c                   clear                   v3aas
026100080327     c                   clear                   v3lnp
026200080327     c                   clear                   v3nrs
026300080327     c                   clear                   v3nsp
026400080327     c                   clear                   v3data
026500080327     c                   clear                   v3datacon
026600080327     c                   clear                   v3oracon
026700080327     c                   clear                   v3dcr
026800080327     c                   clear                   v3hcr
026900080327     c                   clear                   v3notaut
027000080327     c                   clear                   v3inccas
027100080327     c                   clear                   v3incfat
027200080327     c                   clear                   v3codpiv
027300080327     c                   clear                   v3codiso
027400080327     c                   clear                   v3codfis
027500080327     c                   clear                   v3nomfir
027600080327     c                   clear                   v3riserv
027700080327     c                   clear                   v3nrbanr
027800080327     c                   clear                   v3tc1
027900080327     c                   clear                   v3flgspc
027901120511     c                   clear                   v3xlo
027902140121     c                   clear                   v3ritdoc
028000110322
028100110322      * -?Abilitazione F9=Interrogazione Spedizione?
028200110322     c                   eval      *in09 = (W3Ctipappl = 'C')
028300080327
028400080327     c                   endsr
028500110323
028600110323      /free
028700110323
028800110323       //--------------------------------------------------------------
028900110323       //?Reperimento "Tipo Bolla" per bolla di sede.                  ?
029000110323       //--------------------------------------------------------------
029100110323       BEGSR  sr_GetTBL;
029200110323
029300110323         if  Not  %open(TITAS30C);
029400110323           open  TITAS30C;
029500110323         endif;
029600110323
029700110323         chain  ( V3aas : V3lnp : V3nrs : V3nsp )  TITAS30C;
029800110323
029900110323         if  %found(TITAS30C);
030000110323           wTBL = TAStbl;
030100110323         endif;
030200110323
030300110323       ENDSR;
030400110323
030500110323      /end-free
