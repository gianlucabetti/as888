000100110324     /*PRM tgtrls(*current)
000200110324     /*END
000300070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400080328     h dftactgrp(*no) actgrp(*caller)
000500070209      *------------------------------------------------------------------------*
000600080327      *               Record COK
000700070209      *
000800070209      *------------------------------------------------------------------------*
000900070209
001000110324     fTITAS30C  if   e           k disk    usropn
001100110324     f                                     extdesc('GAITRAGRPS/TITAS30C')
001200110324     ftntbe01l  if   e           k disk
001300080327     ffnlvp3d   cf   e             workstn
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
002400080328     d fiprdcokds    e ds
002500101103     d dass          e ds
002600110324     d TNSB51ds      e ds                  inz
002700110324      /copy gaitrasrc/srcProtoPI,FNLR36R
002800070209      *------------------------------------------------------------------------*
002900080327     c     *entry        plist
003000080328     c                   parm                    kpjba
003100080328     c                   parm                    plgdati         250
003200080327     c                   parm                    esitol            1
003300080327     c                   eval      fiprdcokds= plgdati
003400110324     c                   if        Not  $GiaRicev
003500110324     c                   movel     kpjbu         W3Ctipappl
003600110324     c                   eval      $GiaRicev = *on
003700110324     c                   endif
003800080327     c                   exsr      sr_pulvid
003900101103     C     KTBE          KLIST
004000101103     C                   KFLD                    tbecod
004100101103     C                   KFLD                    tbeke1
004200070508      * filiale distinta
004300080327     c                   monitor
004400080327     c                   move      §cokfgs       v3fgs
004500080327     c                   on-error
004600080327     c                   endmon
004700070508      * numero distinta
004800080327     c                   monitor
004900080327     c                   move      §cokndc       v3ndc
005000080327     c                   on-error
005100080327     c                   endmon
005200070514      * data/ora evento
005300080328     c                   clear                   errore            1
005400080327     c                   monitor
005500080327     c                   movel     §cokdatora    v3data
005600080327     c                   on-error
005700080328     c                   eval      errore = '1'
005800080327     c                   endmon
005900080328     c                   if        errore = ' '
006000080328     c                   move      v3data        dataiso
006100080328     c                   move      dataiso       dataeur
006200080328     c                   move      dataeur       v3data
006300080328     c                   end
006400080327     c                   monitor
006500080327     c                   move      §cokdatora    v3ora
006600080327     c                   on-error
006700080327     c                   endmon
006800080327     c*
006900080327     c                   monitor
007000080327     c                   move      §cokaas       v3aas
007100080327     c                   on-error
007200080327     c                   endmon
007300080327     c                   monitor
007400080327     c                   move      §coklnp       v3lnp
007500080327     c                   on-error
007600080327     c                   endmon
007700080327     c                   monitor
007800080327     c                   move      §coknrs       v3nrs
007900080327     c                   on-error
008000080327     c                   endmon
008100080327     c                   monitor
008200080327     c                   move      §coknsp       v3nsp
008300080327     c                   on-error
008400080327     c                   endmon
008500070508      * richiesta assistenza
008600070514     c                   select
008700080327     c                   when      §cokflgass = 'S'
008800070508     c                   eval      v3okflgass = 'SI'
008900080327     c                   when      §cokflgass = *blanks
009000070508     c                   eval      v3okflgass = 'NO'
009100070514     c                   other
009200080327     c                   eval      v3okflgass = §cokflgass
009300101103     c                   movel(p)  v3okflgass    tbeke1
009400101201     c                   movel     'ASA'         tbecod
009500101103     c     ktbe          chain     tntbe01l
009600101103     c                   if        %found(tntbe01l)
009700101103     c                   movel(p)  tbeuni        dass
009800101103     c                   movel     §assdes       v3okflgass
009900101103     c                   else
010000101103     c                   clear                   dass
010100101103     c                   clear                   v3okflgass
010200101103     c                   end
010300070514     c                   endsl
010400101103     c                   eval      v3oknotass = §coknotass
010500080221      * id riga
010600080327     c                   monitor
010700080327     c                   move      §cokidrow     v3okidrow
010800080327     c                   on-error
010900080327     c                   endmon
011000070508
011100080327     c                   exfmt     lvp4d02
011200070508
011300070508      * f3=fine
011400070508     c                   if        *inkc
011500080327     c                   eval      esitol='1'
011600080327     c                   seton                                        lr
011700080327     c                   return
011800070508     c                   endif
011900110324
012000110324      *?F9=Visualizzazione Bolla?
012100110324if  1c                   if        *inKI
012200110324if  2c                   IF        %subst(KNSIF:1:3) = 'GAI'
012300110324if  3c                   if        wTBL = *blank
012400110324     c                   exsr      sr_GetTBL
012500110324e   3c                   endif
012600110324     c                   clear                   TNSB51ds
012700110324     c                   eval      I51aas = V3aas
012800110324     c                   eval      I51lnp = V3lnp
012900110324     c                   eval      I51nrs = V3nrs
013000110324     c                   eval      I51nsp = V3nsp
013100110324     c                   eval      I51tbl = wTBL
013200110324     c                   eval      kpjbu  = TNSB51ds
013300110324     c                   call      'TNSB51R'
013400110324     c                   parm                    kpjba
013500110324     c                   eval      TNSB51ds = kpjbu
013600110324if  3c                   if        O51f03 = *on
013700110324     c                   eval      EsitoL = *on
013800110324e   3c                   endif
013900110324x   2c                   ELSE
014000110324     c                   clear                   FNLR36ds
014100110324     c                   eval      FNLR36ds.LR36aas = V3aas
014200110324     c                   eval      FNLR36ds.LR36lnp = V3lnp
014300110324     c                   eval      FNLR36ds.LR36nrs = V3nrs
014400110324     c                   eval      FNLR36ds.LR36nsp = V3nsp
014500110324     c                   eval      FNLR36ds.LR36flg = '1'
014600110324     c                   eval      kpjbu  = FNLR36ds
014700110324     c                   call      'FNLR36R'
014800110324     c                   parm                    kpjba
014900110324     c                   eval      FNLR36ds = kpjbu
015000110324if  3c                   if        FNLR36ds.LR36f03  = *on
015100110324if  4c                   if        O51f03 = *on
015200110324     c                   eval      EsitoL = *on
015300110324e   4c                   endif
015400110324e   3c                   endif
015500110324     c                   eval      *inLR  = *on
015600110324     c                   return
015700110324e   2c                   ENDIF
015800110324e   1c                   endif
015900070508
016000070508      * f12=ritorno
016100070508     c                   if        *inkl
016200080327     c                   eval      esitol='2'
016300080327     c                   seton                                        lr
016400080327     c                   return
016500070508     c                   endif
016600080327      *------------------------------------------------------------------------*
016700080327      *? PULIZIA DELLA VIDEATA  ?
016800080327      *------------------------------------------------------------------------*
016900080327     c     sr_pulvid     begsr
017000080327
017100080327     c                   clear                   v3fgs
017200080327     c                   clear                   v3ndc
017300080327     c                   clear                   v3data
017400080327     c                   clear                   v3ora
017500080327
017600080327     c                   clear                   v3aas
017700080327     c                   clear                   v3lnp
017800080327     c                   clear                   v3nrs
017900080327     c                   clear                   v3nsp
018000080327
018100080327     c                   clear                   v3okidrow
018200110324
018300110324      * -?Abilitazione F9=Interrogazione Spedizione?
018400110324     c                   eval      *in09 = (W3Ctipappl = 'C')
018500080327
018600080327     c                   endsr
018700080327
018800110324      /free
018900110324
019000110324       //--------------------------------------------------------------
019100110324       //?Reperimento "Tipo Bolla" per bolla di sede.                  ?
019200110324       //--------------------------------------------------------------
019300110324       BEGSR  sr_GetTBL;
019400110324
019500110324         if  Not  %open(TITAS30C);
019600110324           open  TITAS30C;
019700110324         endif;
019800110324
019900110324         chain  ( V3aas : V3lnp : V3nrs : V3nsp )  TITAS30C;
020000110324
020100110324         if  %found(TITAS30C);
020200110324           wTBL = TAStbl;
020300110324         endif;
020400110324
020500110324       ENDSR;
020600110324
020700110324      /end-free
