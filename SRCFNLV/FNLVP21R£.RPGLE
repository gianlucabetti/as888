000100110324     /*PRM tgtrls(*current)
000200110324     /*END
000300170719     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400080328     h dftactgrp(*no) actgrp(*caller)
000500070209
000600070209      *------------------------------------------------------------------------*
000700170719      *               sfl dettaglio RIE
000900070209      *------------------------------------------------------------------------*
001100170719     ffnlvp3d   cf   e             workstn
001400070209
001500070216      * ? V A R I A B I L I ?
001600070531     d dataeur         s               d   datfmt(*eur)
001700080328     d dataiso         s               d
001800110324     d W3Ctipappl      s              1    inz
001900110324     d $GiaRicev       s               n   inz
002000170719
002200110324      * ? D S   I N T E R N E / E S T E R N E ?
002300080328     d kpjba         e ds
002400170719     d fiprdRIEds    e ds
002500110324     d TNSB51ds      e ds                  inz
002600110324      /copy gaitrasrc/srcProtoPI,FNLR36R
002700070209      *------------------------------------------------------------------------*
002800080327     c     *entry        plist
002900080328     c                   parm                    kpjba
003000080328     c                   parm                    plgdati         250
003100080328     c                   parm                    esitol            1
003101170719      **
003200170719     c                   eval      fiprdRIEds= plgdati
003201170719
003300110324     c                   if        Not  $GiaRicev
003400110324     c                   movel     kpjbu         W3Ctipappl
003500110324     c                   eval      $GiaRicev = *on
003600110324     c                   endif
003601170719
003700080327     c                   exsr      sr_pulvid
003800070508      * filiale distinta
003900080327     c                   monitor
004000170719     c                   move      §RIEfgs       v3fgs
004100080327     c                   on-error
004200080327     c                   endmon
004300070508      * numero distinta
004400080327     c                   monitor
004500170719     c                   move      §RIEndc       v3ndc
004600080327     c                   on-error
004700080327     c                   endmon
004800070514      * data/ora evento
004900080328     c                   clear                   errore            1
005000080327     c                   monitor
005100170719     c                   movel     §RIEdtorin    v3data
005200080327     c                   on-error
005300080328     c                   eval      errore = '1'
005400080327     c                   endmon
005500080328     c                   if        errore = ' '
005600080328     c                   move      v3data        dataiso
005700080328     c                   move      dataiso       dataeur
005800080328     c                   move      dataeur       v3data
005900080328     c                   end
006000080327     c                   monitor
006100170719     c                   move      §RIEdtorin    v3ora
006200080327     c                   on-error
006300080327     c                   endmon
006301170719      *  inserito da spc
006303170719     c                   monitor
006304170719     c                   movel     §RIESPCCOD    v3spccod
006306170719     c                   on-error
006307170719     c                   endmon
006308170719      *-------------
006400170719      * LONGITUDINE
006500080327     c                   monitor
006600170719     c                   move      §RIEXLO       v3XLO
006700080327     c                   on-error
006800080327     c                   endmon
006801170719      * LATITUDINE
006900080327     c                   monitor
007000170719     c                   move      §RIEYLA       v3YLA
007100080327     c                   on-error
007200080327     c                   endmon
007201170719      *-------------
007202170719      * FORZA CHIUSURA
007203170719     c                   monitor
007204170719     c                   move      §RIEFORZA     v3FORZA
007205170719     c                   on-error
007206170719     c                   endmon
007207170719      *-------------
007208170719      * TOTALI
007209170719     c                   monitor
007210170719     c                   move      §RIENCLNC     v3NCLNC
007211170719     c                   on-error
007212170719     c                   endmon
007213170719     c                   monitor
007214170719     c                   move      §RIENCLR      v3NCLR
007215170719     c                   on-error
007216170719     c                   endmon
007217170719     c                   monitor
007218170719     c                   move      §RIENBNR      v3NBNR
007219170719     c                   on-error
007220170719     c                   endmon
007221170719     c                   monitor
007222170719     c                   move      §RIENCLNS     v3NCLNS
007223170719     c                   on-error
007224170719     c                   endmon
007225170719      *-------------
011800170719     c                   exfmt     lvp4d21
012000070508      * f3=fine
012100070508     c                   if        *inkc
012200080327     c                   eval      esitol='1'
012300080327     c                   seton                                        lr
012400080327     c                   return
012500070508     c                   endif
012600110324
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
018200170719     c                   clear                   v3spccod
018201170719      **
018300170719     c                   clear                   v3xlo
018400170719     c                   clear                   v3yla
018401170719
020000170719     c                   endsr
