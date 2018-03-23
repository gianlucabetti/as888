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
002000151029
002100151029       //--------------------------------------------------------------
002200151029       //?Dichiarazione file.                                          ?
002300151029       //--------------------------------------------------------------
002400151029
002500151029       // -?Printer File?
002600151029     fTNSS11P   o    e             printer
002700151029     f                                     oflind(*in25)
002800151029
002900151029       //--------------------------------------------------------------
003000151029       //?Definizione costanti.                                        ?
003100151029       //--------------------------------------------------------------
003200151029
003300151029       // -?Peso limite?
003400151030     d c_PesoMax       c                   const('30.0')
003500151030
003600151030       // -?Estratti Totali per Clienti NON codificati?
003700151030     d c_Tip_NOcod     c                   const('1NOC')
003800151030       // -?Estratti Totali per Clienti codificati?
003900151030     d c_Tip_SIcod     c                   const('2COD')
004000151030
004100151030       // -?Estratti Totali per Clienti?
004200151030     d c_Naz_ALL       c                   const('***')
004300151030       // -?Estratti Totali per Clienti Esteri?
004400151030     d c_Naz_EST       c                   const('EST')
004500151030       // -?Estratti Totali per Clienti Italia?
004600151030     d c_Naz_ITA       c                   const('ITA')
004700151030
004800151030       // -?Testate in Stampa?
004900151030     d c_Txt_NOcod     c                   const('CLIENTI NON CODIFICATI')
005000151030     d c_Txt_SIcod     c                   const('CLIENTI CODIFICATI')
005100151029
005200151029       //--------------------------------------------------------------
005300151029       //?Definizione schiere.                                         ?
005400151029       //--------------------------------------------------------------
005500151029
005600151029
005700151029       //--------------------------------------------------------------
005800151029       //?Definizione aree dati.                                       ?
005900151029       //--------------------------------------------------------------
006000151029
006100151029       // -?Dati utente?
006200151029     d §AzUte        e ds                  extname(AZUTE00F)
006300151029     d                                     dtaara
006400151029     d §DatiUte      e ds                  extname(dDatiUte)
006500151029     d                                     dtaara
006600151029
006700151029       //--------------------------------------------------------------
006800151029       //?Definizione strutture dati.                                  ?
006900151029       //--------------------------------------------------------------
007000151029
007100151029       // -?Status ds?
007200151029     d Status         sds
007300151029     d   SDSpgmName      *proc
007400151029
007500151029       // -?Parametri ricevuti?
007600151029     d KPJBA         e ds
007700151029     d TNSS11ds      e ds                  inz
007800151029
007900151030       // -?Dati estratti via SQL?
008000151030     d wSQL_ds         ds                  inz   qualified
008100151030     d   totTIP                       4    inz
008200151030     d   totNAZ                       3    inz
008300151030     d   totNSP                      13  0 inz
008400151030     d   totNCL                      15  0 inz
008500151030     d   totPKF                      15  1 inz
008600151030     d   totVLF                      15  3 inz
008700151030     d   totIMV                      19  3 inz
008800151030     d SaveSQL_ds      ds                  inz   qualified
008900151030     d   totTIP                       4    inz
009000151030     d   totNAZ                       3    inz
009100151030     d   totNSP                      13  0 inz
009200151030     d   totNCL                      15  0 inz
009300151030     d   totPKF                      15  1 inz
009400151030     d   totVLF                      15  3 inz
009500151030     d   totIMV                      19  3 inz
009600151029
009700151029       // -?Schiera per "scomposizione" stringa SQL in stampa?
009800151030     d*// ds_Temp         ds          5000    inz
009900151030     d*//   sk_Temp               1   5000    inz   dim(50)
010000151029
010100151029       // -?Data/Ora attuali?
010200151029     d wTime_ds        ds                  inz
010300151029     d   wDate                        8s 0 inz
010400151029     d   wTime                        6s 0 inz
010500151029
010600151029       //--------------------------------------------------------------
010700151029       //?Definizione variabili globali.                               ?
010800151029       //--------------------------------------------------------------
010900151029
011000151029       // -?Flags booleani?
011100151029     d $EoF            s               n   inz
011200151029
011300151029       // -?Indici di schiera / Contatori?
011400151029     d xx              s              5  0 inz
011500151029
011600151029       // -?Stringhe SQL da eseguire?
011700151030     d*// wSQL            s          32767a   inz   varying
011800151030     d wSQL            s           9999    inz   varying
011900151030       // -?Nome File da cui estrarre i dati?
012000151030     d wFile           s             10    inz
012100151029
012200151029       // -?Data in formato *EUR?
012300151029     d wDate_Eur       s               d   inz   datfmt(*EUR)
012400151029
012500151029       //--------------------------------------------------------------
012600151029       //?Definizione prototipi procedure.                             ?
012700151029       //--------------------------------------------------------------
012800151029
012900151029       // -?Reperimento dati utente?
013000151029     d TIBS34ds      e ds                  inz
013100151029      /copy gaitrasrc/srcProtoPR,TIBS34R
013200151030
013300151030       // -?Caricamento filiali x Estero?
013400151030     d TRUL31ds      e ds                  inz
013500151030     d   sk_POest             10    759    inz   dim(250)
013600151030      /copy gaitrasrc/srcProtoPR,TRUL31R
013700151029
013800151029       // -?Parametri API QCAPCMD (Process Commands)?
013900151030     d Qcmd            s           2048    inz   varying
014000151029      /copy qSysInc/qRpgleSrc,QCAPCMD
014100151029       // -?API QCAPCMD (Process Commands)?
014200151029      /copy gaitrasrc/srcProtoPR,QCAPCMD
014300151029
014400151029       // -?Parametri gestione errori API.?
014500151029      /copy qsysinc/qrpglesrc,QUSEC
014600151029
014700151029       //--------------------------------------------------------------
014800151029       //?Definizione key-list.                                        ?
014900151029       //--------------------------------------------------------------
015000151029
015100151029
015200151029       //--------------------------------------------------------------
015300151029       //?Riepilogo indicatori utilizzati.                             ?
015400151029       //--------------------------------------------------------------
015500151029
015600151029
015700151029       //--------------------------------------------------------------
015800151029       //?M A I N - L I N E                                            ?
015900151029       //--------------------------------------------------------------
016000151029
016100151029     c     *Entry        plist
016200151029     c                   parm                    KPJBA
016300151029
016400151029      /free
016500151029
016600151029       // -?Operazioni iniziali?
016700151029       exsr  sr_RoutInz;
016800151029
016900151030       // -?Preparazione stringa SQL per estrazione dati?
017000151030       exsr  sr_PrepSQL;
017100151030
017200151030       // -?Apertura Cursore?
017300151030       exsr  sr_OpenCursor;
017400151030
017500151030       // -?Ciclo di elaborazione dati ITALIA?
017600151030       DoW  $EoF = *off;
017700151030         exsr  sr_ReadCursor;
017800151030       EndDo;
017900151030
018000151030       // -?Chiusura Cursore?
018100151030       exsr  sr_CloseCursor;
018200151029
018300151029       // -?Operazioni finali?
018400151029       exsr sr_RoutEnd;
018500151029
018600151029       //--------------------------------------------------------------
018700151029       //?Operazioni iniziali.                                         ?
018800151029       //--------------------------------------------------------------
018900151029       BEGSR  sr_RoutInz;
019000151029
019100151029         // -?Impostazione opzioni per SQL?
019200151029         exec sql   set  option  DynUsrPrf = *Owner,
019300151029                                 CloSqlCsr = *EndMod;
019400151029
019500151029         // -?Impostazione chiusura?
019600151029         *inLR = *on;
019700151029
019800151029         // -?Reperimento dati job?
019900151029         exsr  sr_DatiJob;
020000151029
020100151029         // -?Reperimento parametri?
020200151029         TNSS11ds = kpjbu;
020300151029
020400151029         // -?Reperimento data e ora attuali?
020500151029         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
020600151030
020700151030         // -?Caricamento filiali x Estero?
020800151030         clear  TRUL31ds;
020900151030         I31abi = 'EE';
021000151030         trul31r ( KPJBA : TRUL31ds );
021100151029
021200151029         // -?Impostazione campi di stampa in testata?
021300151029         T1Cpgm = SDSpgmName;
021400151118         wDate_Eur = %date( wDate : *iso );
021500151118         T1Cdta    = %dec( wDate_Eur );
021600151029         T1Cora = wTime;
021700151029
021800151029         wDate_Eur = %date( SS11dsi : *iso );
021900151029         T1Cdsi    = %dec( wDate_Eur );
022000151029         wDate_Eur = %date( SS11dsf : *iso );
022100151029         T1Cdsf    = %dec( wDate_Eur );
022200151029
022300151029         // -?Impostazione condizione di OverFlow (per stampare testata)?
022400151029         *in25  = *on;
022500151029
022600151029       ENDSR;
022700151029
022800151029       //--------------------------------------------------------------
022900151029       //?Reperimento Dati del job (Utente/Operativi).                 ?
023000151029       //--------------------------------------------------------------
023100151029       BEGSR  sr_DatiJob;
023200151029
023300151029         in(e)  §AzUte;
023400151029         if  NOT  %error;
023500151029           in(e)  §DatiUte;
023600151029         endif;
023700151029         if  %error  or  RSut = *blank;
023800151029           tibs34r ( tibs34ds );
023900151029           in  §AzUte;
024000151029           in  §DatiUte;
024100151029         endif;
024200151029
024300151029       ENDSR;
024400151029
024500151029       //--------------------------------------------------------------
024600151029       //?Preparazione stringa SQL (per l'estrazione da TITAS).        ?
024700151029       //--------------------------------------------------------------
024800151029       BEGSR  sr_PrepSQL;
024900151029
025000151030         // -?Impostazione iniziale della stringa SQL?
025100151030         //  ?(per ciascuno dei 3 file TITAS*)?
025200151030         wSQL =
025300151030            'with TITAS  as (';
025400151030
025500151030         For  xx = 1  To  3;
025600151030
025700151030           select;
025800151030             when  xx = 1;
025900151030               wFile = 'TITAS00F';
026000151030             when  xx = 2;
026100151030               wFile = 'TITAS10F';
026200151030             when  xx = 3;
026300151030               wFile = 'TITASP0F';
026400151030           endsl;
026500151030
026600151030           exsr  sr_PrepSQL_1;
026700151030
026800151030         EndFor;
026900151030
027000151030         // -?Impostazione finale della stringa SQL?
027100151030         wSQL += ' ) +
027200151030
027300151030           select wTIP, ''' + c_Naz_ALL + ''' as wNAZ, +
027400151030                  sum(wNSP), sum(wNCL), sum(wPKF), sum(wVLF), sum(wIMV) +
027500151030             from TITAS +
027600151030            group by wTIP +
027700151030
027800151030            UNION +
027900151030
028000151030           select wTIP, ''' + c_Naz_EST + ''' as wNAZ, +
028100151030                  sum(wNSP), sum(wNCL), sum(wPKF), sum(wVLF), sum(wIMV) +
028200151030             from TITAS +
028300151030            where tasLNP in (';
028400151030
028500151030         xx = 1;
028600151030         DoW  sk_POest(xx) > *zero;
028700151030           if  xx > 1;
028800151030             wSQL += ', ';
028900151030           endif;
029000151030           wSQL += sk_POest(xx);
029100151030           xx   += 1;
029200151030         EndDo;
029300151030
029400151030         wSQL +=  ') +
029500151030               or tasLNA in (';
029600151030
029700151030         xx = 1;
029800151030         DoW  sk_POest(xx) > *zero;
029900151030           if  xx > 1;
030000151030             wSQL += ', ';
030100151030           endif;
030200151030           wSQL += sk_POest(xx);
030300151030           xx   += 1;
030400151030         EndDo;
030500151030
030600151030         wSQL +=  ') +
030700151030            group by wTIP +
030800151030
030900151030            order by wTIP +
031000151030
031100151030              for fetch only';
031200151030
031300151030       ENDSR;
031400151030
031500151030       //--------------------------------------------------------------
031600151030       //?Impostazione iniziale della stringa SQL per ciascuno dei 3   ?
031700151030       //?  file TITAS*                                                ?
031800151030       //--------------------------------------------------------------
031900151030       BEGSR  sr_PrepSQL_1;
032000151030
032100151030         if  xx > 1;
032200151030           wSQL += ' UNION';
032300151030         endif;
032400151030
032500151030         wSQL +=
032600151030         ' select ''' + c_Tip_NOcod + ''' as wTIP, tasLNP, tasLNA, +
032700151030                  count(*) as wNSP, +
032800151030                  sum(tasNCL) as wNCL, sum(tasPKF) as wPKF, +
032900151030                  sum(tasVLF) as wVLF, sum(tasIMV) as wIMV +
033000151030
033100151030             from ' + %trim( wFile ) +
033200151030
033300151030          ' where ( ( tasAAS * 10000 ) + tasMGS ) between ' +
033400151030                  %trim( %editc( SS11dsi : '3' ) ) + ' and ' +
033500151030                  %trim( %editc( SS11dsf : '3' ) ) +
033600151030            ' and tasNFT >  0 +
033700151030              and tasPKF <= ' + c_PesoMax +
033800151030            ' and substr( digits( tasKSC ), 4, 4 ) in +
033900151030                  ( ''8888'', ''9999'' ) +
034000151030
034100151030            group by tasLNP, tasLNA +
034200151030
034300151030            UNION +
034400151030
034500151030           select ''' + c_Tip_SIcod + ''' as wTIP, tasLNP, tasLNA, +
034600151030                  count(*) as wNSP, +
034700151030                  sum(tasNCL) as wNCL, sum(tasPKF) as wPKF, +
034800151030                  sum(tasVLF) as wVLF, sum(tasIMV) as wIMV +
034900151030
035000151030             from ' + %trim( wFile ) +
035100151030
035200151030          ' where ( ( tasAAS * 10000 ) + tasMGS ) between ' +
035300151030                  %trim( %editc( SS11dsi : '3' ) ) + ' and ' +
035400151030                  %trim( %editc( SS11dsf : '3' ) ) +
035500151030            ' and tasNFT >  0 +
035600151030              and tasPKF <= ' + c_PesoMax +
035700151030            ' and substr( digits( tasKSC ), 4, 4 ) NOT in +
035800151030                  ( ''8888'', ''9999'' ) +
035900151030
036000151030            group by tasLNP, tasLNA';
036100151029
036200151029       ENDSR;
036300151029
036400151029       //--------------------------------------------------------------
036500151029       //?Apertura cursore.                                            ?
036600151029       //--------------------------------------------------------------
036700151029       BEGSR  sr_OpenCursor;
036800151029
036900151029         $EoF = *off;
037000151029
037100151029         // -?Dichiarazione cursore?
037200151030         exec sql   prepare S1   from :wSQL;
037300151030         exec sql   declare C1   cursor for S1;
037400151029
037500151029         if  SQLcode < *zero;
037600151029           $EoF = *on;
037700151029           exsr  sr_PrintERR;
037800151029         endif;
037900151029
038000151029         // -?Apertura del cursore?
038100151029         exec sql   open C1;
038200151029
038300151029         if  SQLcode < *zero;
038400151029           $EoF = *on;
038500151029           exsr  sr_PrintERR;
038600151029         endif;
038700151029
038800151029       ENDSR;
038900151029
039000151029       //--------------------------------------------------------------
039100151029       //?Chiusura cursore.                                            ?
039200151029       //--------------------------------------------------------------
039300151029       BEGSR  sr_CloseCursor;
039400151029
039500151029         // -?Chiusura del cursore?
039600151029         exec sql   close C1;
039700151029
039800151029       ENDSR;
039900151029
040000151029       //--------------------------------------------------------------
040100151029       //?Lettura cursore 1.                                           ?
040200151029       //--------------------------------------------------------------
040300151029       BEGSR  sr_ReadCursor;
040400151029
040500151029         clear  wSQL_ds;
040600151029
040700151029         exec sql   fetch next   from C1   into :wSQL_ds;
040800151029
040900151030         Select;
041000151030
041100151030           // -?E.o.F.?
041200151030           When  SQLcode = 100;
041300151029             $EoF = *on;
041400151030             write  SS11end;
041500151030
041600151030           // -?Errore?
041700151030           When  SQLcode < *zero;
041800151029             $EoF = *on;
041900151029             exsr  sr_PrintERR;
042000151030
042100151030           // -?Elaborazione Totale?
042200151030           Other;
042300151030             // -?Totale estratto (Generale o Estero)?
042400151029             exsr  sr_Stampa;
042500151030             // -?Totale Italia?
042600151030             if  wSQL_ds.totNAZ = c_Naz_ITA;
042700151030               exsr  sr_Stampa;
042800151030             endif;
042900151030
043000151030         EndSl;
043100151029
043200151029       ENDSR;
043300151029
043400151029       //--------------------------------------------------------------
043500151029       //?Stampa singola riga di totale.                               ?
043600151029       //--------------------------------------------------------------
043700151029       BEGSR  sr_Stampa;
043800151030
043900151030         // -?Salto Pagina?
044000151030         if  *in25;
044100151030           write  SS11nwpg;
044200151030           write  SS11Txt1;
044300151030           *in25 = *off;
044400151030         endif;
044500151029
044600151030         // -?Stampa Testata?
044700151030         If  wSQL_ds.totNAZ = c_Naz_ALL;
044800151030
044900151030           clear  T1Dtxt;
045000151030           Select;
045100151030             // -?Elaborazione Clienti NON codificati?
045200151030             When  wSQL_ds.totTIP = c_Tip_NOcod;
045300151030               xx = ( %size(T1Dtxt) - %size(c_Txt_NOcod) ) / 2;
045400151103               %subst( T1Dtxt : xx + 1 ) = c_Txt_NOcod;
045500151030             // -?Elaborazione Clienti codificati?
045600151030             When  wSQL_ds.totTIP = c_Tip_SIcod;
045700151030               xx = ( %size(T1Dtxt) - %size(c_Txt_SIcod) ) / 2;
045800151103               %subst( T1Dtxt : xx + 1 ) = c_Txt_SIcod;
045900151030           EndSl;
046000151030
046100151030           write  SS11Txt2;
046200151030
046300151029         EndIf;
046400151030
046500151029
046600151029         // -?Impostazione dati in stampa?
046700151030         clear  SS11det;
046800151030
046900151030         Select;
047000151030           // -?Elaborazione dati Totali?
047100151030           When  wSQL_ds.totNAZ = c_Naz_ALL;
047200151030             D1Dtot   = 'Totale';
047300151030           // -?Elaborazione dati Clienti Estero?
047400151030           When  wSQL_ds.totNAZ = c_Naz_EST;
047500151030             D1Dtot   = 'Estero';
047600151030           // -?Elaborazione dati Clienti Italia?
047700151030           When  wSQL_ds.totNAZ = c_Naz_ITA;
047800151030             D1Dtot   = 'Italia';
047900151030         EndSl;
048000151030
048100151030         D1Dnsp   = wSQL_ds.totNSP;
048200151030         D1Dncl   = wSQL_ds.totNCL;
048300151030         D1Dpkf   = wSQL_ds.totPKF;
048400151030         D1Dvlf   = wSQL_ds.totVLF;
048500151030         D1Dimv   = wSQL_ds.totIMV;
048600151029
048700151030
048800151029         // -?Stampa del singolo Totale?
048900151030         write  SS11det;
049000151030
049100151030
049200151030         // -?Impostazioni finali?
049300151030         Select;
049400151030
049500151030           // -?Elaborati dati Totali?
049600151030           //  ?=> salvataggio dati appena elaborati?
049700151030           When  wSQL_ds.totNAZ = c_Naz_ALL;
049800151030             SaveSQL_ds = wSQL_ds;
049900151030
050000151030           // -?Elaborati dati Estero?
050100151030           //  ?=> impostazione dati Italia?
050200151030           When  wSQL_ds.totNAZ = c_Naz_EST;
050300151030             wSQL_ds.totTIP = SaveSQL_ds.totTIP;
050400151030             wSQL_ds.totNAZ = c_Naz_ITA;
050500151030             wSQL_ds.totNSP = SaveSQL_ds.totNSP - wSQL_ds.totNSP;
050600151030             wSQL_ds.totNCL = SaveSQL_ds.totNCL - wSQL_ds.totNCL;
050700151030             wSQL_ds.totPKF = SaveSQL_ds.totPKF - wSQL_ds.totPKF;
050800151030             wSQL_ds.totVLF = SaveSQL_ds.totVLF - wSQL_ds.totVLF;
050900151030             wSQL_ds.totIMV = SaveSQL_ds.totIMV - wSQL_ds.totIMV;
051000151030
051100151030           // -?Elaborati dati Italia?
051200151030           //  ?=> pulizia dati salvati (Totale)?
051300151030           When  wSQL_ds.totNAZ = c_Naz_ITA;
051400151030             clear  SaveSQL_ds;
051500151030
051600151030         EndSl;
051700151029
051800151029       ENDSR;
051900151029
052000151029       //--------------------------------------------------------------
052100151029       //?Stampa segnalazione dell'errore rilevato via SQL             ?
052200151029       //--------------------------------------------------------------
052300151029       BEGSR  sr_PrintERR;
052400151029
052500151029         // -?Stampa del Dump?
052600151029         Dump(A);
052700151029
052800151029         // -?Stampa del Job-Log?
052900151029         Qcmd = 'DSPJOBLOG job(*) output(*print)';
053000151029         exsr  sr_ExecCmd;
053100151029
053200151029       ENDSR;
053300151029
053400151029       //--------------------------------------------------------------
053500151029       //?Operazioni finali.                                           ?
053600151029       //--------------------------------------------------------------
053700151029       BEGSR  sr_RoutEnd;
053800151029
053900151029         // -?Uscita dal *pgm?
054000151029         return;
054100151029
054200151029       ENDSR;
054300151029
054400151029       //--------------------------------------------------------------
054500151029       //?Esecuzione del comando (già impostato).                      ?
054600151029       //--------------------------------------------------------------
054700151029       BEGSR  sr_ExecCmd;
054800151029
054900151029         clear Qcap0100;
055000151029         Qcabcsdh = *off;
055100151029         Qcapa    = *off;
055200151029         Qcacmdss = *off;
055300151029         Qcaerved = *allX'00';
055400151029
055500151029         clear Qusec;
055600151029         Qusbprv  = %size(Qusec);
055700151029
055800151029         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
055900151029                           %size(Qcap0100) : 'CPOP0100' : *omit :
056000151029                           0 : 0 : Qusec);
056100151029
056200151029         //if  Qusei <> *blank;
056300151029         //  ...;
056400151029         //endif;
056500151029
056600151029       ENDSR;
056700151029
056800151029      /end-free
