000100110324     /*PRM tgtrls(*current)
000200110324     /*END
000300070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400080328     h dftactgrp(*no) actgrp(*caller)
000500070209      *------------------------------------------------------------------------*
000600131129      *               Record COR
000700070209      *
000800070209      *------------------------------------------------------------------------*
000900070209
001000110324     fTITAS30C  if   e           k disk    usropn
001100110324     f                                     extdesc('GAITRAGRPS/TITAS30C')
001200110324     ftntbe01l  if   e           k disk
001300131211     ffnlvp3d   cf   e             workstn
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
002400131129     d fiprdcords    e ds
002600110324     d TNSB51ds      e ds                  inz
002700110324      /copy gaitrasrc/srcProtoPI,FNLR36R
002800070209      *------------------------------------------------------------------------*
002900080327     c     *entry        plist
003000080328     c                   parm                    kpjba
003100080328     c                   parm                    plgdati         250
003200080327     c                   parm                    esitol            1
003300131129     c                   eval      fiprdcords= plgdati
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
004400131129     c                   move      §corfgs       v1fgs
004500080327     c                   on-error
004600080327     c                   endmon
004700070508      * numero distinta
004800080327     c                   monitor
004900131129     c                   move      §corndc       v1ndc
005000080327     c                   on-error
005100080327     c                   endmon
005200070514      * data/ora evento
005300080328     c                   clear                   errore            1
005400080327     c                   monitor
005500131129     c                   movel     §cordatora    v1data
005600080327     c                   on-error
005700080328     c                   eval      errore = '1'
005800080327     c                   endmon
005900080328     c                   if        errore = ' '
006000131129     c                   move      v1data        dataiso
006100080328     c                   move      dataiso       dataeur
006200131129     c                   move      dataeur       v1data
006300080328     c                   end
006400080327     c                   monitor
006500131129     c                   move      §cordatora    v1ora
006600080327     c                   on-error
006700080327     c                   endmon
006800080327     c*
006900080327     c                   monitor
007000131129     c                   move      §coraas       v1aas
007100080327     c                   on-error
007200080327     c                   endmon
007300080327     c                   monitor
007400131129     c                   move      §corlnp       v1lnp
007500080327     c                   on-error
007600080327     c                   endmon
007700080327     c                   monitor
007800131129     c                   move      §cornrs       v1nrs
007900080327     c                   on-error
008000080327     c                   endmon
008100080327     c                   monitor
008200131129     c                   move      §cornsp       v1nsp
008300080327     c                   on-error
008400080327     c                   endmon
008401131211     c                   monitor
008402131211     c                   move      §cororasti    v1orasti
008403131211     c                   on-error
008404131211     c                   endmon
008405131211     c                   monitor
008406131211     c                   move      §corsort      v1sort
008407131211     c                   on-error
008408131211     c                   endmon
011100131211     c                   exfmt     lvp4d07
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
012700131129     c                   eval      I51aas = V1aas
012800131129     c                   eval      I51lnp = V1lnp
012900131129     c                   eval      I51nrs = V1nrs
013000131129     c                   eval      I51nsp = V1nsp
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
014100131129     c                   eval      FNLR36ds.LR36aas = V1aas
014200131129     c                   eval      FNLR36ds.LR36lnp = V1lnp
014300131129     c                   eval      FNLR36ds.LR36nrs = V1nrs
014400131129     c                   eval      FNLR36ds.LR36nsp = V1nsp
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
017100131129     c                   clear                   v1fgs
017200131129     c                   clear                   v1ndc
017300131129     c                   clear                   v1data
017400131129     c                   clear                   v1ora
017500080327
017600131129     c                   clear                   v1aas
017700131129     c                   clear                   v1lnp
017800131129     c                   clear                   v1nrs
017900131129     c                   clear                   v1nsp
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
019900131129         chain  ( V1aas : V1lnp : V1nrs : V1nsp )  TITAS30C;
020000110324
020100110324         if  %found(TITAS30C);
020200110324           wTBL = TAStbl;
020300110324         endif;
020400110324
020500110324       ENDSR;
020600110324
020700110324      /end-free
