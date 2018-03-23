000100151029       //==============================================================
000200151103       //?TNSS11R - Statistica spedizioni fino a 30 Kg                 ?
000300151030       //?          per clienti codificati e non (BATCH)               ?
000400151029       //==============================================================
000500151029
000600151029       //--------------------------------------------------------------
000700151029       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800151029       //--------------------------------------------------------------
000900151029
001000151029     /*PRM dbgview(*source)
001100151029     /*END
001200151029
001300151029       //--------------------------------------------------------------
001400151029       //?Specifiche di controllo.                                     ?
001500151029       //--------------------------------------------------------------
001600151029
001700151029     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800151029     h alwnull(*inputonly)
001900151029     h dftactgrp(*no)
002000170317     h bnddir('TRUL')
002100151029
002200151029       //--------------------------------------------------------------
002300151029       //?Dichiarazione file.                                          ?
002400151029       //--------------------------------------------------------------
002500151029
002600151029       // -?Printer File?
002700151029     fTNSS11P   o    e             printer
002800151029     f                                     oflind(*in25)
002900151029
003000151029       //--------------------------------------------------------------
003100151029       //?Definizione costanti.                                        ?
003200151029       //--------------------------------------------------------------
003300151029
003400151029       // -?Peso limite?
003500151030     d c_PesoMax       c                   const('30.0')
003600151030
003700151030       // -?Estratti Totali per Clienti NON codificati?
003800151030     d c_Tip_NOcod     c                   const('1NOC')
003900151030       // -?Estratti Totali per Clienti codificati?
004000151030     d c_Tip_SIcod     c                   const('2COD')
004100170317
004200170317       // -?Tipo Servizio "Priority"?
004300170317     d c_TSE_Priority  c                   const('E')
004400170317       // -?Tipo Servizio "H 10:30"?
004500170317     d c_TSE_H1030     c                   const('H')
004600151030
004700151030       // -?Estratti Totali per Clienti?
004800151030     d c_Naz_ALL       c                   const('***')
004900151030       // -?Estratti Totali per Clienti Esteri?
005000151030     d c_Naz_EST       c                   const('EST')
005100151030       // -?Estratti Totali per Clienti Italia?
005200151030     d c_Naz_ITA       c                   const('ITA')
005300151030
005400151030       // -?Testate in Stampa?
005500151030     d c_Txt_NOcod     c                   const('CLIENTI NON CODIFICATI')
005600151030     d c_Txt_SIcod     c                   const('CLIENTI CODIFICATI')
005700151029
005800151029       //--------------------------------------------------------------
005900151029       //?Definizione schiere.                                         ?
006000151029       //--------------------------------------------------------------
006100151029
006200151029
006300151029       //--------------------------------------------------------------
006400151029       //?Definizione aree dati.                                       ?
006500151029       //--------------------------------------------------------------
006600151029
006700151029       // -?Dati utente?
006800151029     d §AzUte        e ds                  extname(AZUTE00F)
006900151029     d                                     dtaara
007000151029     d §DatiUte      e ds                  extname(dDatiUte)
007100151029     d                                     dtaara
007200151029
007300151029       //--------------------------------------------------------------
007400151029       //?Definizione strutture dati.                                  ?
007500151029       //--------------------------------------------------------------
007600151029
007700151029       // -?Status ds?
007800151029     d Status         sds
007900151029     d   SDSpgmName      *proc
008000151029
008100151029       // -?Parametri ricevuti?
008200151029     d KPJBA         e ds
008300151029     d TNSS11ds      e ds                  inz
008400170317
008500170317       // -?Tab. "5E" = Tipi Servizio?
008600170317     d ds5E_E        e ds                  extname( ds5E )
008700170317     d                                     inz   qualified
008800170317     d ds5E_H        e ds                  extname( ds5E )
008900170317     d                                     inz   qualified
009000151029
009100151030       // -?Dati estratti via SQL?
009200151030     d wSQL_ds         ds                  inz   qualified
009300151030     d   totTIP                       4    inz
009400170317     d   totSEQ                       1    inz
009500151030     d   totNAZ                       3    inz
009600151030     d   totNSP                      13  0 inz
009700151030     d   totNCL                      15  0 inz
009800151030     d   totPKF                      15  1 inz
009900151030     d   totVLF                      15  3 inz
010000151030     d   totIMV                      19  3 inz
010100151030     d SaveSQL_ds      ds                  inz   qualified
010200151030     d   totTIP                       4    inz
010300170317     d   totSEQ                       1    inz
010400151030     d   totNAZ                       3    inz
010500151030     d   totNSP                      13  0 inz
010600151030     d   totNCL                      15  0 inz
010700151030     d   totPKF                      15  1 inz
010800151030     d   totVLF                      15  3 inz
010900151030     d   totIMV                      19  3 inz
011000151029
011100151029       // -?Schiera per "scomposizione" stringa SQL in stampa?
011200151030     d*// ds_Temp         ds          5000    inz
011300151030     d*//   sk_Temp               1   5000    inz   dim(50)
011400151029
011500151029       // -?Data/Ora attuali?
011600151029     d wTime_ds        ds                  inz
011700151029     d   wDate                        8s 0 inz
011800151029     d   wTime                        6s 0 inz
011900151029
012000151029       //--------------------------------------------------------------
012100151029       //?Definizione variabili globali.                               ?
012200151029       //--------------------------------------------------------------
012300151029
012400151029       // -?Flags booleani?
012500151029     d $EoF            s               n   inz
012600151029
012700151029       // -?Indici di schiera / Contatori?
012800151029     d xx              s              5  0 inz
012900151029
013000151029       // -?Stringhe SQL da eseguire?
013100170317     d wSQL            s          32740    inz   varying
013200151030       // -?Nome File da cui estrarre i dati?
013300151030     d wFile           s             10    inz
013400170317
013500170317       // -?"Tipo Dati" estratti (Clienti Codificati/Non_Codificati)?
013600170317     d wSaveTIP        s                   like(wSQL_ds.totTIP)
013700170317     d                                     inz
013800170320       // -?"Nazionalità" estratti (Totale "***" / Estero "EST")?
013900170320     d wSaveNAZ        s                   like(wSQL_ds.totNAZ)
014000170320     d                                     inz
014100151029
014200151029       // -?Data in formato *EUR?
014300151029     d wDate_Eur       s               d   inz   datfmt(*EUR)
014400151029
014500151029       //--------------------------------------------------------------
014600151029       //?Definizione prototipi procedure.                             ?
014700151029       //--------------------------------------------------------------
014800151029
014900151029       // -?Reperimento dati utente?
015000151029     d TIBS34ds      e ds                  inz
015100151029      /copy gaitrasrc/srcProtoPR,TIBS34R
015200170317
015300170317       // -?Reperimento dati tabelle?
015400170317      /copy gaitrasrc/srcProtoPI,TRULTAB
015500170317      /copy gaitrasrc/srcProtoPR,TRULTAB
015600151030
015700151030       // -?Caricamento filiali x Estero?
015800151030     d TRUL31ds      e ds                  inz
015900151030     d   sk_POest             10    759    inz   dim(250)
016000151030      /copy gaitrasrc/srcProtoPR,TRUL31R
016100151029
016200151029       // -?Parametri API QCAPCMD (Process Commands)?
016300151030     d Qcmd            s           2048    inz   varying
016400151029      /copy qSysInc/qRpgleSrc,QCAPCMD
016500151029       // -?API QCAPCMD (Process Commands)?
016600151029      /copy gaitrasrc/srcProtoPR,QCAPCMD
016700151029
016800151029       // -?Parametri gestione errori API.?
016900151029      /copy qsysinc/qrpglesrc,QUSEC
017000151029
017100151029       //--------------------------------------------------------------
017200151029       //?Definizione key-list.                                        ?
017300151029       //--------------------------------------------------------------
017400151029
017500151029
017600151029       //--------------------------------------------------------------
017700151029       //?Riepilogo indicatori utilizzati.                             ?
017800151029       //--------------------------------------------------------------
017900151029
018000151029
018100151029       //--------------------------------------------------------------
018200151029       //?M A I N - L I N E                                            ?
018300151029       //--------------------------------------------------------------
018400151029
018500151029     c     *Entry        plist
018600151029     c                   parm                    KPJBA
018700151029
018800151029      /free
018900151029
019000151029       // -?Operazioni iniziali?
019100151029       exsr  sr_RoutInz;
019200151029
019300151030       // -?Preparazione stringa SQL per estrazione dati?
019400151030       exsr  sr_PrepSQL;
019500151030
019600151030       // -?Apertura Cursore?
019700151030       exsr  sr_OpenCursor;
019800151030
019900151030       // -?Ciclo di elaborazione dati ITALIA?
020000151030       DoW  $EoF = *off;
020100151030         exsr  sr_ReadCursor;
020200151030       EndDo;
020300151030
020400151030       // -?Chiusura Cursore?
020500151030       exsr  sr_CloseCursor;
020600151029
020700151029       // -?Operazioni finali?
020800151029       exsr sr_RoutEnd;
020900151029
021000151029       //--------------------------------------------------------------
021100151029       //?Operazioni iniziali.                                         ?
021200151029       //--------------------------------------------------------------
021300151029       BEGSR  sr_RoutInz;
021400151029
021500151029         // -?Impostazione opzioni per SQL?
021600151029         exec sql   set  option  DynUsrPrf = *Owner,
021700151029                                 CloSqlCsr = *EndMod;
021800151029
021900151029         // -?Impostazione chiusura?
022000151029         *inLR = *on;
022100151029
022200151029         // -?Reperimento dati job?
022300151029         exsr  sr_DatiJob;
022400151029
022500151029         // -?Reperimento parametri?
022600151029         TNSS11ds = kpjbu;
022700151029
022800151029         // -?Reperimento data e ora attuali?
022900151029         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
023000151030
023100151030         // -?Caricamento filiali x Estero?
023200151030         clear  TRUL31ds;
023300151030         I31abi = 'EE';
023400151030         trul31r ( KPJBA : TRUL31ds );
023500170317
023600170317         // -?Reperimento dati per Tipo Servizio "E" (Priority)?
023700170317         clear  ds5E_E;
023800170317         if  getTabella ( c_Tabel : '5E' : 'E' : *omit :
023900170317                          *omit : *omit :
024000170317                          *omit : *omit :
024100170317                          *omit : *omit : *omit : *omit :
024200170317                          *omit : *omit : *omit : *omit :
024300170317                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
024400170317                          = *zero      AND
024500170317               ds_TNTBE.TBEatb = *blank;
024600170317           ds5E_E = ds_TNTBE.TBEuni;
024700170317         endif;
024800170317         // -?Reperimento dati per Tipo Servizio "H" (H 10:30)?
024900170317         clear  ds5E_H;
025000170317         if  getTabella ( c_Tabel : '5E' : 'H' : *omit :
025100170317                          *omit : *omit :
025200170317                          *omit : *omit :
025300170317                          *omit : *omit : *omit : *omit :
025400170317                          *omit : *omit : *omit : *omit :
025500170317                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
025600170317                          = *zero      AND
025700170317               ds_TNTBE.TBEatb = *blank;
025800170317           ds5E_H = ds_TNTBE.TBEuni;
025900170317         endif;
026000151029
026100151029         // -?Impostazione campi di stampa in testata?
026200151029         T1Cpgm = SDSpgmName;
026300151118         wDate_Eur = %date( wDate : *iso );
026400151118         T1Cdta    = %dec( wDate_Eur );
026500151029         T1Cora = wTime;
026600151029
026700151029         wDate_Eur = %date( SS11dsi : *iso );
026800151029         T1Cdsi    = %dec( wDate_Eur );
026900151029         wDate_Eur = %date( SS11dsf : *iso );
027000151029         T1Cdsf    = %dec( wDate_Eur );
027100151029
027200151029         // -?Impostazione condizione di OverFlow (per stampare testata)?
027300151029         *in25  = *on;
027400151029
027500151029       ENDSR;
027600151029
027700151029       //--------------------------------------------------------------
027800151029       //?Reperimento Dati del job (Utente/Operativi).                 ?
027900151029       //--------------------------------------------------------------
028000151029       BEGSR  sr_DatiJob;
028100151029
028200151029         in(e)  §AzUte;
028300151029         if  NOT  %error;
028400151029           in(e)  §DatiUte;
028500151029         endif;
028600151029         if  %error  or  RSut = *blank;
028700151029           tibs34r ( tibs34ds );
028800151029           in  §AzUte;
028900151029           in  §DatiUte;
029000151029         endif;
029100151029
029200151029       ENDSR;
029300151029
029400151029       //--------------------------------------------------------------
029500151029       //?Preparazione stringa SQL (per l'estrazione da TITAS).        ?
029600151029       //--------------------------------------------------------------
029700151029       BEGSR  sr_PrepSQL;
029800151029
029900151030         // -?Impostazione iniziale della stringa SQL?
030000151030         //  ?(per ciascuno dei 3 file TITAS*)?
030100151030         wSQL =
030200151030            'with TITAS  as (';
030300151030
030400151030         For  xx = 1  To  3;
030500151030
030600151030           select;
030700151030             when  xx = 1;
030800151030               wFile = 'TITAS00F';
030900151030             when  xx = 2;
031000151030               wFile = 'TITAS10F';
031100151030             when  xx = 3;
031200151030               wFile = 'TITASP0F';
031300151030           endsl;
031400151030
031500151030           exsr  sr_PrepSQL_1;
031600151030
031700151030         EndFor;
031800151030
031900170320         // -?Impostazione finale della stringa SQL:?
032000170320         //   ·?Totale generale spedizioni?
032100170320         //    ?ord. Tipo (Codificato - NON Codificato) / "1" / "***"?
032200151030         wSQL += ' ) +
032300151030
032400170317           select wTIP, ''1'' as wSEQ, ''' + c_Naz_ALL + ''' as wNAZ, +
032500151030                  sum(wNSP), sum(wNCL), sum(wPKF), sum(wVLF), sum(wIMV) +
032600151030             from TITAS +
032700170317            group by wTIP +
032800151030
032900170320            UNION';
033000151030
033100170320         //   ·?Totale spedizioni Estero?
033200170320         //    ?ord. Tipo (Codificato - NON Codificato) / "1" / "EST"?
033300170320         wSQL +=
033400170320         ' select wTIP, ''1'' as wSEQ, ''' + c_Naz_EST + ''' as wNAZ, +
033500151030                  sum(wNSP), sum(wNCL), sum(wPKF), sum(wVLF), sum(wIMV) +
033600151030             from TITAS +
033700151030            where tasLNP in (';
033800151030
033900151030         xx = 1;
034000151030         DoW  sk_POest(xx) > *zero;
034100151030           if  xx > 1;
034200151030             wSQL += ', ';
034300151030           endif;
034400151030           wSQL += sk_POest(xx);
034500151030           xx   += 1;
034600151030         EndDo;
034700151030
034800151030         wSQL +=  ') +
034900151030               or tasLNA in (';
035000151030
035100151030         xx = 1;
035200151030         DoW  sk_POest(xx) > *zero;
035300151030           if  xx > 1;
035400151030             wSQL += ', ';
035500151030           endif;
035600151030           wSQL += sk_POest(xx);
035700151030           xx   += 1;
035800151030         EndDo;
035900170317
036000170317         wSQL +=  ') +
036100170317            group by wTIP +
036200170317
036300170317
036400170320            UNION';
036500170317
036600170320         //   ·?Totale generale sped. con Tipo Servizio "E" (Priority)?
036700170320         //    ?ord. Tipo (Codificato - NON Codificato) / "2" / "***"?
036800170320         wSQL +=
036900170320         ' select wTIP, ''2'' as wSEQ, ''' + c_Naz_ALL + ''' as wNAZ, +
037000170317                  sum(wNSP), sum(wNCL), sum(wPKF), sum(wVLF), sum(wIMV) +
037100170317             from TITAS +
037200170317            where tasTSP = ''' + c_TSE_Priority + ''' +
037300170317            group by wTIP +
037400170317
037500170320            UNION';
037600170317
037700170320         //   ·?Totale Estero sped. con Tipo Servizio "E" (Priority)?
037800170320         //    ?ord. Tipo (Codificato - NON Codificato) / "2" / "EST"?
037900170320         wSQL +=
038000170320         ' select wTIP, ''2'' as wSEQ, ''' + c_Naz_EST + ''' as wNAZ, +
038100170317                  sum(wNSP), sum(wNCL), sum(wPKF), sum(wVLF), sum(wIMV) +
038200170317             from TITAS +
038300170317            where tasTSP = ''' + c_TSE_Priority + ''' +
038400170317              and ( tasLNP in (';
038500170317
038600170317         xx = 1;
038700170317         DoW  sk_POest(xx) > *zero;
038800170317           if  xx > 1;
038900170317             wSQL += ', ';
039000170317           endif;
039100170317           wSQL += sk_POest(xx);
039200170317           xx   += 1;
039300170317         EndDo;
039400170317
039500170317         wSQL +=  ') +
039600170317               or tasLNA in (';
039700170317
039800170317         xx = 1;
039900170317         DoW  sk_POest(xx) > *zero;
040000170317           if  xx > 1;
040100170317             wSQL += ', ';
040200170317           endif;
040300170317           wSQL += sk_POest(xx);
040400170317           xx   += 1;
040500170317         EndDo;
040600151030
040700170317         wSQL +=  ') ) +
040800170317            group by wTIP +
040900170317
041000170317
041100170320            UNION';
041200170317
041300170320         //   ·?Totale generale sped. con Tipo Servizio "H" (H 10:30)?
041400170320         //    ?ord. Tipo (Codificato - NON Codificato) / "3" / "***"?
041500170320         wSQL +=
041600170320         ' select wTIP, ''3'' as wSEQ, ''' + c_Naz_ALL + ''' as wNAZ, +
041700170317                  sum(wNSP), sum(wNCL), sum(wPKF), sum(wVLF), sum(wIMV) +
041800170317             from TITAS +
041900170317            where tasTSP = ''' + c_TSE_H1030 + ''' +
042000170317            group by wTIP +
042100170317
042200170320            UNION';
042300170317
042400170320         //   ·?Totale Estero sped. con Tipo Servizio "H" (H 10:30)?
042500170320         //    ?ord. Tipo (Codificato - NON Codificato) / "3" / "EST"?
042600170320         wSQL +=
042700170320         ' select wTIP, ''3'' as wSEQ, ''' + c_Naz_EST + ''' as wNAZ, +
042800170317                  sum(wNSP), sum(wNCL), sum(wPKF), sum(wVLF), sum(wIMV) +
042900170317             from TITAS +
043000170317            where tasTSP = ''' + c_TSE_H1030 + ''' +
043100170317              and ( tasLNP in (';
043200170317
043300170317         xx = 1;
043400170317         DoW  sk_POest(xx) > *zero;
043500170317           if  xx > 1;
043600170317             wSQL += ', ';
043700170317           endif;
043800170317           wSQL += sk_POest(xx);
043900170317           xx   += 1;
044000170317         EndDo;
044100170317
044200170317         wSQL +=  ') +
044300170317               or tasLNA in (';
044400170317
044500170317         xx = 1;
044600170317         DoW  sk_POest(xx) > *zero;
044700170317           if  xx > 1;
044800170317             wSQL += ', ';
044900170317           endif;
045000170317           wSQL += sk_POest(xx);
045100170317           xx   += 1;
045200170317         EndDo;
045300170317
045400170317         wSQL +=  ') ) +
045500170317            group by wTIP +
045600170317
045700151030
045800170317            order by wTIP, wSEQ, wNAZ +
045900151030
046000151030              for fetch only';
046100151030
046200151030       ENDSR;
046300151030
046400151030       //--------------------------------------------------------------
046500151030       //?Impostazione iniziale della stringa SQL per ciascuno dei 3   ?
046600170320       //?  file TITAS*.                                               ?
046700151030       //--------------------------------------------------------------
046800151030       BEGSR  sr_PrepSQL_1;
046900151030
047000151030         if  xx > 1;
047100151030           wSQL += ' UNION';
047200151030         endif;
047300151030
047400170320         // -?Sub-Totale spedizioni per Clienti NON Codificati?
047500170320         //  ?per Tipo cliente (Codificato / NON Codificato),?
047600170320         //      ?Tipo Spedizione,?
047700170320         //      ?Linea di Partenza e?
047800170320         //      ?Linea di Arrivo?
047900151030         wSQL +=
048000170316         ' select ''' + c_Tip_NOcod + ''' as wTIP, +
048100170317                  tasTSP, tasLNP, tasLNA, +
048200170316                  count(*) as wNSP, +
048300151030                  sum(tasNCL) as wNCL, sum(tasPKF) as wPKF, +
048400151030                  sum(tasVLF) as wVLF, sum(tasIMV) as wIMV +
048500151030
048600151030             from ' + %trim( wFile ) +
048700151030
048800151030          ' where ( ( tasAAS * 10000 ) + tasMGS ) between ' +
048900151030                  %trim( %editc( SS11dsi : '3' ) ) + ' and ' +
049000151030                  %trim( %editc( SS11dsf : '3' ) ) +
049100151030            ' and tasNFT >  0 +
049200151030              and tasPKF <= ' + c_PesoMax +
049300151030            ' and substr( digits( tasKSC ), 4, 4 ) in +
049400151030                  ( ''8888'', ''9999'' ) +
049500151030
049600170317            group by tasTSP, tasLNP, tasLNA +
049700151030
049800170320            UNION';
049900151030
050000170320         // -?Sub-Totale spedizioni per Clienti Codificati?
050100170320         //  ?per Tipo cliente (Codificato / NON Codificato),?
050200170320         //      ?Tipo Spedizione,?
050300170320         //      ?Linea di Partenza e?
050400170320         //      ?Linea di Arrivo?
050500170320         wSQL +=
050600170320         ' select ''' + c_Tip_SIcod + ''' as wTIP, +
050700170317                  tasTSP, tasLNP, tasLNA, +
050800151030                  count(*) as wNSP, +
050900151030                  sum(tasNCL) as wNCL, sum(tasPKF) as wPKF, +
051000151030                  sum(tasVLF) as wVLF, sum(tasIMV) as wIMV +
051100151030
051200151030             from ' + %trim( wFile ) +
051300151030
051400151030          ' where ( ( tasAAS * 10000 ) + tasMGS ) between ' +
051500151030                  %trim( %editc( SS11dsi : '3' ) ) + ' and ' +
051600151030                  %trim( %editc( SS11dsf : '3' ) ) +
051700151030            ' and tasNFT >  0 +
051800151030              and tasPKF <= ' + c_PesoMax +
051900151030            ' and substr( digits( tasKSC ), 4, 4 ) NOT in +
052000151030                  ( ''8888'', ''9999'' ) +
052100151030
052200170317            group by tasTSP, tasLNP, tasLNA';
052300151029
052400151029       ENDSR;
052500151029
052600151029       //--------------------------------------------------------------
052700151029       //?Apertura cursore.                                            ?
052800151029       //--------------------------------------------------------------
052900151029       BEGSR  sr_OpenCursor;
053000151029
053100151029         $EoF = *off;
053200151029
053300151029         // -?Dichiarazione cursore?
053400151030         exec sql   prepare S1   from :wSQL;
053500151030         exec sql   declare C1   cursor for S1;
053600151029
053700151029         if  SQLcode < *zero;
053800151029           $EoF = *on;
053900151029           exsr  sr_PrintERR;
054000170317           leavesr;
054100151029         endif;
054200151029
054300151029         // -?Apertura del cursore?
054400151029         exec sql   open C1;
054500151029
054600151029         if  SQLcode < *zero;
054700151029           $EoF = *on;
054800151029           exsr  sr_PrintERR;
054900170317           leavesr;
055000151029         endif;
055100151029
055200151029       ENDSR;
055300151029
055400151029       //--------------------------------------------------------------
055500151029       //?Chiusura cursore.                                            ?
055600151029       //--------------------------------------------------------------
055700151029       BEGSR  sr_CloseCursor;
055800151029
055900151029         // -?Chiusura del cursore?
056000151029         exec sql   close C1;
056100151029
056200151029       ENDSR;
056300151029
056400151029       //--------------------------------------------------------------
056500151029       //?Lettura cursore 1.                                           ?
056600151029       //--------------------------------------------------------------
056700151029       BEGSR  sr_ReadCursor;
056800151029
056900151029         clear  wSQL_ds;
057000151029
057100151029         exec sql   fetch next   from C1   into :wSQL_ds;
057200151029
057300151030         Select;
057400151030
057500151030           // -?E.o.F.?
057600151030           When  SQLcode = 100;
057700151029             $EoF = *on;
057800170320             if  wSaveNAZ =  c_Naz_ALL;
057900170320               exsr  sr_SubTot0;
058000170320             endif;
058100151030             write  SS11end;
058200151030
058300151030           // -?Errore?
058400151030           When  SQLcode < *zero;
058500151029             $EoF = *on;
058600151029             exsr  sr_PrintERR;
058700151030
058800151030           // -?Elaborazione Totale?
058900151030           Other;
059000170317             // -?Totale estratto (Generale o Estero)?
059100170317             exsr  sr_Stampa;
059200170317             // -?Totale Italia?
059300170317             if  wSQL_ds.totNAZ = c_Naz_ITA;
059400170317               exsr  sr_Stampa;
059500170317             endif;
059600151030
059700151030         EndSl;
059800151029
059900151029       ENDSR;
060000151029
060100151029       //--------------------------------------------------------------
060200151029       //?Stampa singola riga di totale.                               ?
060300151029       //--------------------------------------------------------------
060400151029       BEGSR  sr_Stampa;
060500151030
060600170320         // -?Stampa eventuali totali precedenti a zero?
060700170320         //  ?(Estero e Italia)?
060800170320         if  wSaveNAZ =  c_Naz_ALL  and
060900170320             wSaveNAZ =  wSQL_ds.totNAZ;
061000170320           exsr  sr_SubTot0;
061100170320         endif;
061200170320         wSaveNAZ = wSQL_ds.totNAZ;
061300170320
061400170317         // -?Salto Pagina:?
061500170317         //  ?Stampa 1ª Testata?
061600170317         if  wSQL_ds.totTIP <> wSaveTIP;
061700170320           if  wSaveTIP <> *blank;
061800170317             write  SS11seg;
061900170317           endif;
062000170317           wSaveTIP = wSQL_ds.totTIP;
062100170317           *in25    = *on;
062200170317         endif;
062300151030         if  *in25;
062400151030           write  SS11nwpg;
062500170317           write  SS11txt1;
062600151030           *in25 = *off;
062700151030         endif;
062800151029
062900170317         // -?Stampa 2ª Testata?
063000170317         If  wSQL_ds.totNAZ = c_Naz_ALL  and
063100170317             wSQL_ds.totSEQ = '1';
063200151030
063300151030           clear  T1Dtxt;
063400151030           Select;
063500151030             // -?Elaborazione Clienti NON codificati?
063600151030             When  wSQL_ds.totTIP = c_Tip_NOcod;
063700151030               xx = ( %size(T1Dtxt) - %size(c_Txt_NOcod) ) / 2;
063800151103               %subst( T1Dtxt : xx + 1 ) = c_Txt_NOcod;
063900151030             // -?Elaborazione Clienti codificati?
064000151030             When  wSQL_ds.totTIP = c_Tip_SIcod;
064100151030               xx = ( %size(T1Dtxt) - %size(c_Txt_SIcod) ) / 2;
064200151103               %subst( T1Dtxt : xx + 1 ) = c_Txt_SIcod;
064300151030           EndSl;
064400151030
064500170317           write  SS11txt2;
064600151030
064700151029         EndIf;
064800170317
064900170317         // -?Stampa 3ª Testata?
065000170317         If  wSQL_ds.totNAZ = c_Naz_ALL  and
065100170317            (wSQL_ds.totSEQ = '2'        or
065200170317             wSQL_ds.totSEQ = '3');
065300170317
065400170317           clear  T1Dtxt3;
065500170317           Select;
065600170317             // -?Elaborazione Spedizioni "E" = Priority?
065700170317             When  wSQL_ds.totSEQ = '2';
065800170317               T1Dtxt3 = 'SOLO SPEDIZIONI CON TIPO SERVIZIO "E" - ' +
065900170317                         ds5E_E.§5Edes;
066000170317             // -?Elaborazione Spedizioni "H" = H 10:30?
066100170317             When  wSQL_ds.totSEQ = '3';
066200170317               T1Dtxt3 = 'SOLO SPEDIZIONI CON TIPO SERVIZIO "H" - ' +
066300170317                         ds5E_H.§5Edes;
066400170317           EndSl;
066500170317
066600170317           write  SS11txt3;
066700170317
066800170317         EndIf;
066900170317
067000170317         // -?Stampa 4ª Testata?
067100170317         If  wSQL_ds.totNAZ = c_Naz_ALL;
067200170317           write  SS11txt4;
067300170317         EndIf;
067400151030
067500151029
067600151029         // -?Impostazione dati in stampa?
067700151030         clear  SS11det;
067800151030
067900151030         Select;
068000151030           // -?Elaborazione dati Totali?
068100151030           When  wSQL_ds.totNAZ = c_Naz_ALL;
068200151030             D1Dtot   = 'Totale';
068300151030           // -?Elaborazione dati Clienti Estero?
068400151030           When  wSQL_ds.totNAZ = c_Naz_EST;
068500151030             D1Dtot   = 'Estero';
068600151030           // -?Elaborazione dati Clienti Italia?
068700151030           When  wSQL_ds.totNAZ = c_Naz_ITA;
068800151030             D1Dtot   = 'Italia';
068900151030         EndSl;
069000151030
069100151030         D1Dnsp   = wSQL_ds.totNSP;
069200151030         D1Dncl   = wSQL_ds.totNCL;
069300151030         D1Dpkf   = wSQL_ds.totPKF;
069400151030         D1Dvlf   = wSQL_ds.totVLF;
069500151030         D1Dimv   = wSQL_ds.totIMV;
069600151029
069700151030
069800151029         // -?Stampa del singolo Totale?
069900151030         write  SS11det;
070000151030
070100151030
070200151030         // -?Impostazioni finali?
070300151030         Select;
070400151030
070500151030           // -?Elaborati dati Totali?
070600151030           //  ?=> salvataggio dati appena elaborati?
070700151030           When  wSQL_ds.totNAZ = c_Naz_ALL;
070800151030             SaveSQL_ds = wSQL_ds;
070900151030
071000151030           // -?Elaborati dati Estero?
071100151030           //  ?=> impostazione dati Italia?
071200151030           When  wSQL_ds.totNAZ = c_Naz_EST;
071300151030             wSQL_ds.totTIP = SaveSQL_ds.totTIP;
071400170317             wSQL_ds.totSEQ = SaveSQL_ds.totSEQ;
071500151030             wSQL_ds.totNAZ = c_Naz_ITA;
071600151030             wSQL_ds.totNSP = SaveSQL_ds.totNSP - wSQL_ds.totNSP;
071700151030             wSQL_ds.totNCL = SaveSQL_ds.totNCL - wSQL_ds.totNCL;
071800151030             wSQL_ds.totPKF = SaveSQL_ds.totPKF - wSQL_ds.totPKF;
071900151030             wSQL_ds.totVLF = SaveSQL_ds.totVLF - wSQL_ds.totVLF;
072000151030             wSQL_ds.totIMV = SaveSQL_ds.totIMV - wSQL_ds.totIMV;
072100151030
072200151030           // -?Elaborati dati Italia?
072300151030           //  ?=> pulizia dati salvati (Totale)?
072400151030           When  wSQL_ds.totNAZ = c_Naz_ITA;
072500151030             clear  SaveSQL_ds;
072600151030
072700151030         EndSl;
072800151029
072900151029       ENDSR;
073000170320
073100170320       //--------------------------------------------------------------
073200170320       //?Stampa del sub-totale Estero a zero e di quello Italia.      ?
073300170320       //--------------------------------------------------------------
073400170320       BEGSR  sr_SubTot0;
073500170320
073600170320         // -?Stampa sub-totale Estero (a zero)?
073700170320         clear  SS11det;
073800170320         D1Dtot = 'Estero';
073900170320
074000170320         write  SS11det;
074100170320
074200170320         // -?Stampa sub-totale Italia (uguale a quello generale)?
074300170320         clear  SS11det;
074400170320         D1Dtot = 'Italia';
074500170320         D1Dnsp = SaveSQL_ds.totNSP;
074600170320         D1Dncl = SaveSQL_ds.totNCL;
074700170320         D1Dpkf = SaveSQL_ds.totPKF;
074800170320         D1Dvlf = SaveSQL_ds.totVLF;
074900170320         D1Dimv = SaveSQL_ds.totIMV;
075000170320
075100170320         write  SS11det;
075200170320
075300170320       ENDSR;
075400151029
075500151029       //--------------------------------------------------------------
075600170320       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
075700151029       //--------------------------------------------------------------
075800151029       BEGSR  sr_PrintERR;
075900151029
076000151029         // -?Stampa del Dump?
076100151029         Dump(A);
076200151029
076300151029         // -?Stampa del Job-Log?
076400151029         Qcmd = 'DSPJOBLOG job(*) output(*print)';
076500151029         exsr  sr_ExecCmd;
076600151029
076700151029       ENDSR;
076800151029
076900151029       //--------------------------------------------------------------
077000151029       //?Operazioni finali.                                           ?
077100151029       //--------------------------------------------------------------
077200151029       BEGSR  sr_RoutEnd;
077300151029
077400151029         // -?Uscita dal *pgm?
077500151029         return;
077600151029
077700151029       ENDSR;
077800151029
077900151029       //--------------------------------------------------------------
078000151029       //?Esecuzione del comando (già impostato).                      ?
078100151029       //--------------------------------------------------------------
078200151029       BEGSR  sr_ExecCmd;
078300151029
078400151029         clear Qcap0100;
078500151029         Qcabcsdh = *off;
078600151029         Qcapa    = *off;
078700151029         Qcacmdss = *off;
078800151029         Qcaerved = *allX'00';
078900151029
079000151029         clear Qusec;
079100151029         Qusbprv  = %size(Qusec);
079200151029
079300151029         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
079400151029                           %size(Qcap0100) : 'CPOP0100' : *omit :
079500151029                           0 : 0 : Qusec);
079600151029
079700151029         //if  Qusei <> *blank;
079800151029         //  ...;
079900151029         //endif;
080000151029
080100151029       ENDSR;
080200151029
080300151029      /end-free
