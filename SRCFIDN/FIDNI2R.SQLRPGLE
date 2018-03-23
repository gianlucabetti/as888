000100150724       //==============================================================
000200150724       //?Elenco Spediz. Giornaliere con Imp.d'Assicurare eccessivo BCH?
000300150724       //==============================================================
000400150724
000500150724       //--------------------------------------------------------------
000600150724       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150724       //--------------------------------------------------------------
000800150724
000900150724     /*PRM dbgview(*source)
001000150724     /*END
001100150724
001200150724       //--------------------------------------------------------------
001300150724       //?Specifiche di controllo.                                     ?
001400150724       //--------------------------------------------------------------
001500150724
001600150724     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150724     h dftactgrp(*no)
001800150727     h bnddir('UBRTVNETA')
001900150727     h alwnull(*inputonly)
002000150724
002100150724       //--------------------------------------------------------------
002200150724       //?Dichiarazione file.                                          ?
002300150724       //--------------------------------------------------------------
002400150724
002500150728       // -?Stampa unica (dettaglio & totali)?
002600150727     fFIDNI2P   o    e             printer
002700150724     f                                     oflind(*in25)
002800150724
002900150724       // -?Stampa segnalazioni di errore?
003000150724     fQSYSPRT   o    f  132        printer usropn
003100150724     f                                     oflind(*inOF)
003200150724
003300150724       //--------------------------------------------------------------
003400150724       //?Definizione costanti.                                        ?
003500150724       //--------------------------------------------------------------
003600150724
003700150727       // -?Importo Limite: 100.000,00 €?
003800150727     d c_ImportoLimite...
003900150727     d                 c                   const('100000')
004000150727
004100150724       // -?Comandi da eseguire?
004200150724     d c_OvrPrtF       c                   const('OVRPRTF +
004300150724     d                                            file(QSYSPRT) +
004400150727     d                                            usrdta(''FIDNI2+
004500150724     d                                                     *ERR'')')
004600150724     d c_DltOvr        c                   const('DLTOVR +
004700150724     d                                            file(QSYSPRT)')
004800150724
004900150724       //--------------------------------------------------------------
005000150724       //?Definizione schiere.                                         ?
005100150724       //--------------------------------------------------------------
005200150724
005300150724
005400150724       //--------------------------------------------------------------
005500150724       //?Definizione aree dati.                                       ?
005600150724       //--------------------------------------------------------------
005700150724
005800150724       // -?Dati utente?
005900150724     d §AzUte        e ds                  extname(AZUTE00F)
006000150724     d                                     dtaara
006100150724     d §DatiUte      e ds                  extname(dDatiUte)
006200150724     d                                     dtaara
006300150724
006400150724       //--------------------------------------------------------------
006500150724       //?Definizione strutture dati.                                  ?
006600150724       //--------------------------------------------------------------
006700150724
006800150724       // -?Status ds?
006900150724     d Status         sds
007000150724     d   SDSpgmName      *proc
007100150724
007200150724       // -?Parametri ricevuti?
007300150724     d KPJBA         e ds
007400150727     d FIDNI2ds      e ds                  inz
007500150724
007600150727       // -?Dati estratti via SQL per totale?
007700150727     d TITAS00F      e ds                  based(nullPtr)
007800150727     d CNACO00F      e ds                  based(nullPtr)
007900150727     d WTIAEds         ds                  inz   qualified
008000150727     d   TOTccm                            inz   like(TASksc)
008100150724     d   TOTrsm                            inz   like(ACOrag)
008200150724     d   TOTrsd                            inz   like(TASrsd)
008300150724     d   TOTcad                            inz   like(TAScad)
008400150727     d   TOTdsp                            inz   like(TASdrt)
008500150724     d   TOTias                            inz   like(TASias)
008600150727     d   TOTnum                            inz   like(TASnsp)
008700150728     d SavTIAEds       ds                  inz   likeds(WTIAEds)
008800150727
008900150727       // -?Dati estratti via SQL per dettaglio?
009000150727     d WDIAEds         ds                  inz   qualified
009100150727     d   DETaas                            inz   like(TASaas)
009200150727     d   DETlnp                            inz   like(TASlnp)
009300150727     d   DETnrs                            inz   like(TASnrs)
009400150727     d   DETnsp                            inz   like(TASnsp)
009500150727     d   DETdcm                            inz   like(TASdcm)
009600150727     d   DETias                            inz   like(TASias)
009700150727
009800150727       // -?Schiera per "scomposizione" stringa SQL in stampa?
009900150727     d ds_Temp         ds         10000    inz
010000150727     d   sk_Temp               1  10000    inz   dim(100)
010100150724
010200150724       //--------------------------------------------------------------
010300150724       //?Definizione variabili globali.                               ?
010400150724       //--------------------------------------------------------------
010500150724
010600150724       // -?Flags booleani?
010700150724     d $EoF            s               n   inz
010800150724
010900150724       // -?Indici di schiera / Contatori?
011000150727     d xx              s              5  0 inz
011100150724
011200150724       // -?Stringhe SQL da eseguire?
011300150727     d wSQL            s          32740    inz   varying
011400150724
011500150724       // -?Campi di comodo?
011600150724     d wTime           s              6  0 inz
011700150724     d wDate           s              8  0 inz
011800150724     d wDate_Iso       s               d   inz  datfmt(*iso)
011900150724     d wDate_Eur       s               d   inz  datfmt(*eur)
012000150724
012100150724       //--------------------------------------------------------------
012200150724       //?Definizione prototipi procedure.                             ?
012300150724       //--------------------------------------------------------------
012400150724
012500150724       // -?Reperimento NETA sistema AS/400 corrente?
012600150724     d currSysNeta     s              8a   inz
012700150724      /copy gaitrasrc/srcProtoPR,UBRTVNETA
012800150724
012900150724       // -?Reperimento dati utente?
013000150724     d TIBS34ds      e ds                  inz
013100150724      /copy gaitrasrc/srcProtoPR,TIBS34R
013200150724
013300150724       // -?Parametri API QCAPCMD (Process Commands)?
013400150724     d Qcmd            s           2048    inz  varying
013500150724      /copy qSysInc/qRpgleSrc,QCAPCMD
013600150724       // -?API QCAPCMD (Process Commands)?
013700150724      /copy gaitrasrc/srcProtoPR,QCAPCMD
013800150724
013900150724       // -?Parametri gestione errori API.?
014000150724      /copy qsysinc/qrpglesrc,QUSEC
014100150724
014200150724       //--------------------------------------------------------------
014300150724       //?Definizione key-list.                                        ?
014400150724       //--------------------------------------------------------------
014500150724
014600150724
014700150724       //--------------------------------------------------------------
014800150724       //?Riepilogo indicatori utilizzati.                             ?
014900150724       //--------------------------------------------------------------
015000150728       // *in25     - OverFlow in FIDNI2P
015100150728       // *inOF     - OverFlow in QSYSPRT
015200150728       //--------------------------------------------------------------
015300150724
015400150724       //--------------------------------------------------------------
015500150724       //?M A I N - L I N E                                            ?
015600150724       //--------------------------------------------------------------
015700150724
015800150724     c     *Entry        plist
015900150724     c                   parm                    KPJBA
016000150724
016100150724      /free
016200150724
016300150724       // -?Operazioni iniziali?
016400150724       exsr  sr_RoutInz;
016500150724
016600150728       // -?Preparazione stringa SQL per l'estrazione dei dati per?
016700150727       //  ?Mittente/Destinatario/Data spedizione?
016800150728       exsr  sr_PrepSQL;
016900150724
017000150728       // -?Ciclo di elaborazione?
017100150724       reset  $EoF;
017200150728       exsr  sr_OpenCursor;
017300150724
017400150724       DoW  $EoF = *off;
017500150728         exsr  sr_ReadCursor;
017600150724       EndDo;
017700150724
017800150728       exsr  sr_CloseCursor;
017900150724
018000150724       // -?Operazioni finali?
018100150724       exsr sr_RoutEnd;
018200150724
018300150724       //--------------------------------------------------------------
018400150724       //?Operazioni iniziali.                                         ?
018500150724       //--------------------------------------------------------------
018600150724       BEGSR  sr_RoutInz;
018700150724
018800150724         // -?Impostazione opzioni per SQL?
018900150724         exec sql   set  option  DynUsrPrf = *Owner,
019000150724                                 CloSqlCsr = *EndMod;
019100150724
019200150724         // -?Impostazione chiusura?
019300150724         *inLR = *on;
019400150724
019500150724         // -?Reperimento dati job?
019600150724         exsr  sr_DatiJob;
019700150724
019800150724         // -?Reperimento data odierna in formato aaaammgg?
019900150724         wDate_Iso = %date();
020000150724         wDate_Eur = wDate_Iso;
020100150724         wDate = %dec( wDate_Eur );
020200150724
020300150724         // -?Reperimento orario?
020400150724         wTime = %dec( %time() );
020500150724
020600150724         // -?Reperimento parametri?
020700150727         FIDNI2ds = kpjbu;
020800150724
020900150724         // -?Verifica del sistema AS/400 corrente?
021000150724         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
021100150724           exsr  sr_RoutEnd;
021200150724         endif;
021300150724
021400150724         // -?Impostazione campi di stampa in testata?
021500150724         T1Cpgm = SDSpgmName;
021600150724         T1Cdta = wDate;
021700150724         T1Cora = wTime;
021800150724
021900150728         // -?Parametri di stampa?
022000150727         wDate_Eur = %date( DNI2dsi : *iso );
022100150728         PI2dsi    = %dec( wDate_Eur );
022200150727         wDate_Eur = %date( DNI2dsf : *iso );
022300150728         PI2dsf    = %dec( wDate_Eur );
022400150727
022500150727         // -?Stampa testate?
022600150727         write  dnI2txt;
022700150727         *in25  = *off;
022800150724
022900150724       ENDSR;
023000150724
023100150724       //--------------------------------------------------------------
023200150724       //?Reperimento Dati del job (Utente/Operativi).                 ?
023300150724       //--------------------------------------------------------------
023400150724       BEGSR  sr_DatiJob;
023500150724
023600150724         in(e) §AzUte;
023700150724         if NOT %error;
023800150724           in(e) §DatiUte;
023900150724         endif;
024000150724         if %error or RSut = *blank;
024100150724           tibs34r ( tibs34ds );
024200150724           in §AzUte;
024300150724           in §DatiUte;
024400150724         endif;
024500150724
024600150724       ENDSR;
024700150724
024800150724       //--------------------------------------------------------------
024900150728       //?Preparazione stringa SQL per estrazione dati.                ?
025000150724       //--------------------------------------------------------------
025100150728       BEGSR  sr_PrepSQL;
025200150724
025300150727         wSQL = 'with +
025400150724
025500150727                   TITASP as ( select TASksc, TASrsd, TAScad, TASias, +
025600150727                                      TASaas, TASmgs, TASlnp, TASnrs, TASnsp, +
025700150727                                      TASdcm +
025800150727                                 from TITASP0F +
025900150727                                where TAStbl = ''F1'' +
026000150727                                  and TASias > 0 +
026100150727                                  and TASfcm = '' '' +
026200150727                                  and ( (TASaas * 10000) + TASmgs ) between ' +
026300150727                                      %trimL( %editc( DNI2dsi : '3' ) ) +
026400150727                                      ' and ' +
026500150727                                      %trimL( %editc( DNI2dsf : '3' ) ) +
026600150728                           ' order by TASaas, TASmgs, TASksc, TAScad ), +
026700150727
026800150727                   TITAS1 as ( select TASksc, TASrsd, TAScad, TASias, +
026900150727                                      TASaas, TASmgs, TASlnp, TASnrs, TASnsp, +
027000150727                                      TASdcm +
027100150727                                 from TITAS10F +
027200150727                                where TAStbl = ''F1'' +
027300150727                                  and TASias > 0 +
027400150727                                  and TASfcm = '' '' +
027500150727                                  and ( (TASaas * 10000) + TASmgs ) between ' +
027600150727                                      %trimL( %editc( DNI2dsi : '3' ) ) +
027700150727                                      ' and ' +
027800150727                                      %trimL( %editc( DNI2dsf : '3' ) ) +
027900150728                           ' order by TASaas, TASmgs, TASksc, TAScad ), +
028000150727
028100150727                   TITAS0 as ( select TASksc, TASrsd, TAScad, TASias, +
028200150727                                      TASaas, TASmgs, TASlnp, TASnrs, TASnsp, +
028300150727                                      TASdcm +
028400150727                                 from TITAS00F +
028500150727                                where TAStbl = ''F1'' +
028600150727                                  and TASias > 0 +
028700150727                                  and TASfcm = '' '' +
028800150727                                  and ( (TASaas * 10000) + TASmgs ) between ' +
028900150727                                      %trimL( %editc( DNI2dsi : '3' ) ) +
029000150727                                      ' and ' +
029100150727                                      %trimL( %editc( DNI2dsf : '3' ) ) +
029200150728                           ' order by TASaas, TASmgs, TASksc, TAScad ), +
029300150728
029400150728                   TITAS  as ( select * from TITASP +
029500150728                                union +
029600150728                               select * from TITAS1 +
029700150728                                union +
029800150728                               select * from TITAS0 ), +
029900150727
030000150728                   TOTALI as ( select TASksc, ACOrag, TASrsd, TAScad, +
030100150728                                      int( TASaas * 10000 + TASmgs ) +
030200150728                                         as DataSped, +
030300150728                                      sum(TASias), count(*) +
030400150728                                 from TITAS left outer join CNACO00F +
030500150728                                   on TASksc = ACOksc +
030600150728                                where ACOkcc = ' +
030700150728                                      %trimL( %editc( DUTkci : '3' ) ) +
030800150728                                ' and ACOkut = 1 +
030900150728                             group by TASaas, TASmgs, TASksc, +
031000150728                                      ACOrag, TASrsd, TAScad +
031100150728                               having sum(TASias) > ' + c_ImportoLimite +
031200150728                           ' order by TASaas, TASmgs, TASksc, +
031300150728                                      ACOrag, TASrsd, TAScad ) +
031400150728
031500150728                  select TITAS.TASaas, TITAS.TASlnp, TITAS.TASnrs, +
031600150728                         TITAS.TASnsp, TITAS.TASdcm, TITAS.TASias, +
031700150728                         TOTALI.* +
031800150728
031900150728                    from TITAS inner join TOTALI +
032000150728                      on TOTALI.DataSped = ( ( TITAS.TASaas * 10000 ) +
032100150728                                             + TITAS.TASmgs ) +
032200150728                     and TOTALI.TASksc = TITAS.TASksc +
032300150728                     and TOTALI.TASrsd = TITAS.TASrsd +
032400150728                     and TOTALI.TAScad = TITAS.TAScad +
032500150728
032600150728                   order by TITAS.TASaas, TITAS.TASmgs, TITAS.TASksc, +
032700150728                            TITAS.TASrsd, TITAS.TAScad, +
032800150728                            TITAS.TASlnp, TITAS.TASnrs, TITAS.TASnsp +
032900150727
033000150728                     for fetch only';
033100150724
033200150724       ENDSR;
033300150724
033400150724       //--------------------------------------------------------------
033500150728       //?Apertura cursore.                                            ?
033600150724       //--------------------------------------------------------------
033700150728       BEGSR  sr_OpenCursor;
033800150724
033900150724         // -?Dichiarazione cursore?
034000150728         exec sql   prepare S1   from :wSQL;
034100150724         exec sql   declare C1   cursor for S1;
034200150724
034300150724         if  SQLcode < *zero;
034400150724           $EoF = *on;
034500150724           exsr  sr_PrintERR;
034600150724         endif;
034700150724
034800150724         // -?Apertura del cursore?
034900150724         exec sql   open C1;
035000150724
035100150724         if  SQLcode < *zero;
035200150724           $EoF = *on;
035300150724           exsr  sr_PrintERR;
035400150724         endif;
035500150724
035600150724       ENDSR;
035700150724
035800150724       //--------------------------------------------------------------
035900150728       //?Chiusura cursore.                                            ?
036000150724       //--------------------------------------------------------------
036100150728       BEGSR  sr_CloseCursor;
036200150724
036300150724         // -?Chiusura del cursore?
036400150724         exec sql   close C1;
036500150724
036600150724       ENDSR;
036700150724
036800150724       //--------------------------------------------------------------
036900150728       //?Lettura cursore.                                             ?
037000150724       //--------------------------------------------------------------
037100150728       BEGSR  sr_ReadCursor;
037200150724
037300150727         clear  WTIAEds;
037400150728         clear  WDIAEds;
037500150724
037600150728         exec sql   fetch next   from C1   into :WDIAEds, :WTIAEds;
037700150724
037800150724         select;
037900150724           when  SQLcode = 100;
038000150724             $EoF = *on;
038100150724           when  SQLcode < *zero;
038200150724             $EoF = *on;
038300150724             exsr  sr_PrintERR;
038400150724           other;
038500150728             exsr  sr_Stampa;
038600150724         endsl;
038700150724
038800150724       ENDSR;
038900150727
039000150727       //--------------------------------------------------------------
039100150728       //?Stampa dati estratti.                                        ?
039200150727       //--------------------------------------------------------------
039300150728       BEGSR  sr_Stampa;
039400150728
039500150727         // -?Stampa totale per Mittente/Destinatario/Data?
039600150728         If  SavTIAEds.TOTccm <> *zero            and
039700150728             SavTIAEds.TOTrsd <> *blank           and
039800150728             SavTIAEds.TOTcad <> *blank           and
039900150728             SavTIAEds.TOTdsp <> *zero            and
040000150728             ( WTIAEds.TOTccm <> SavTIAEds.TOTccm  or
040100150728               WTIAEds.TOTrsd <> SavTIAEds.TOTrsd  or
040200150728               WTIAEds.TOTcad <> SavTIAEds.TOTcad  or
040300150728               WTIAEds.TOTdsp <> SavTIAEds.TOTdsp );
040400150728           exsr  sr_StampaTOT;
040500150728         EndIf;
040600150728
040700150728         // -?Stampa dettaglio (singola spedizione)?
040800150728         exsr  sr_StampaDET;
040900150728
041000150728       ENDSR;
041100150727
041200150727       //--------------------------------------------------------------
041300150727       //?Stampa totale per Mittente/Destinatario/Data spedizione      ?
041400150727       //?con Importo da Assicurare > 100.000,00 €                     ?
041500150727       //--------------------------------------------------------------
041600150727       BEGSR  sr_StampaTOT;
041700150727
041800150727         // -?Impostazione campi totale?
041900150727         clear  dnI2tot;
042000150728         TI2ias = SavTIAEds.TOTias;
042100150728         TI2tsp = SavTIAEds.TOTnum;
042200150727
042300150727         // -?Stampa totale?
042400150727         write dnI2tot;
042500150727
042600150727       ENDSR;
042700150724
042800150724       //--------------------------------------------------------------
042900150728       //?Stampa singola spedizione di quelle giornaliere              ?
043000150728       //?con Importo da Assicurare > 100.000,00 €                     ?
043100150724       //--------------------------------------------------------------
043200150727       BEGSR  sr_StampaDET;
043300150727
043400150727         // -?Impostazione campi dettaglio?
043500150727         clear  dnI2det;
043600150728         If  WTIAEds.TOTccm <> SavTIAEds.TOTccm  or
043700150728             WTIAEds.TOTrsd <> SavTIAEds.TOTrsd  or
043800150728             WTIAEds.TOTcad <> SavTIAEds.TOTcad  or
043900150728             WTIAEds.TOTdsp <> SavTIAEds.TOTdsp;
044000150728           DI2ccm = WTIAEds.TOTccm;
044100150728           DI2rsm = WTIAEds.TOTrsm;
044200150728           DI2rsd = WTIAEds.TOTrsd;
044300150728           DI2cad = WTIAEds.TOTcad;
044400150728           Monitor;
044500150728             wDate_Eur = %date( WTIAEds.TOTdsp : *iso );
044600150728             DI2dsp    = %dec( wDate_Eur );
044700150728           //On-Error  112;
044800150728           On-Error;
044900150728             clear  DI2dsp;
045000150728           EndMon;
045100150728         EndIf;
045200150728         DI2ias = WDIAEds.DETias;
045300150728         DI2lnp = WDIAEds.DETlnp;
045400150728         DI2nrs = WDIAEds.DETnrs;
045500150728         DI2nsp = WDIAEds.DETnsp;
045600150727         Monitor;
045700150727           wDate_Eur = %date( WDIAEds.DETdcm : *iso );
045800150728           DI2dcm    = %dec( wDate_Eur );
045900150728         //On-Error  112;
046000150727         On-Error;
046100150728           clear  DI2dcm;
046200150727         EndMon;
046300150727
046400150727         // -?Stampa testata?
046500150728         if  *in25;
046600150728           write  dnI2seg;
046700150728           write  dnI2txt;
046800150728           *in25 = *off;
046900150727         endif;
047000150727
047100150727         // -?Stampa dettaglio?
047200150727         write  dnI2det;
047300150728
047400150728         // -?Memorizzazione nuovo "gruppo" di spedizioni?
047500150728         //  ?(per controllo rottura)?
047600150728         SavTIAEds = WTIAEds;
047700150724
047800150724       ENDSR;
047900150724
048000150724       //--------------------------------------------------------------
048100150724       //?Stampa segnalazione dell'errore rilevato via SQL             ?
048200150724       //--------------------------------------------------------------
048300150724       BEGSR  sr_PrintERR;
048400150724
048500150724         // -?Stampa del Dump?
048600150724         Dump(A);
048700150724
048800150724         // -?Stampa del Job-Log?
048900150724         Qcmd = 'DSPJOBLOG job(*) output(*print)';
049000150724         exsr  sr_ExecCmd;
049100150728
049200150728         // -?Stampa segnalazione nel file di stampa?
049300150728         EI2txt = 'Rilevato e stampato errore. Avvisare il CED.';
049400150728         write  DNI2err;
049500150724
049600150724         // -?Apertura del printer-file?
049700150724         if  Not %open(QSYSPRT);
049800150724           Qcmd = c_OvrPrtF;
049900150724           exsr  sr_ExecCmd;
050000150724           open  QSYSPRT;
050100150724           except  PRTtxt;
050200150724         endif;
050300150724
050400150724
050500150727         // -?Stampa dell'errore SQL (e chiusura del *pgm)?
050600150727         clear  ds_Temp;
050700150728         ds_Temp = wSQL;
050800150727         if  *inOF;
050900150727           except  PRTtxt;
051000150727           *inOF = *off;
051100150727         endif;
051200150727         except  PRTerr1;
051300150727         *inOF = *off;
051400150727         For  xx = 1  To  %elem(sk_Temp);
051500150727           if  sk_Temp(xx) = *blank;
051600150727             leave;
051700150727           endif;
051800150727           if  *inOF;
051900150727             except  PRTtxt;
052000150727             *inOF = *off;
052100150727           endif;
052200150727           except  PRTerr2;
052300150727         EndFor;
052400150727
052500150727         // -?Chiusura del programma (DOPO)?
052600150727         exsr  sr_RoutEnd;
052700150724
052800150724       ENDSR;
052900150724
053000150724       //--------------------------------------------------------------
053100150724       //?Operazioni finali.                                           ?
053200150724       //--------------------------------------------------------------
053300150724       BEGSR  sr_RoutEnd;
053400150727
053500150727         // -?Fine stampe?
053600150728         If  SavTIAEds.TOTccm <> *zero            and
053700150728             SavTIAEds.TOTrsd <> *blank           and
053800150728             SavTIAEds.TOTcad <> *blank           and
053900150728             SavTIAEds.TOTdsp <> *zero;
054000150728           exsr  sr_StampaTOT;
054100150728         EndIf;
054200150727         write  dnI2end;
054300150724
054400150727         // -?Chiusura printer-file Errori?
054500150724         if  %open(QSYSPRT);
054600150724           close  QSYSPRT;
054700150724           Qcmd = c_DltOvr;
054800150724           exsr  sr_ExecCmd;
054900150724         endif;
055000150724
055100150724         // -?Uscita dal *pgm?
055200150724         return;
055300150724
055400150724       ENDSR;
055500150724
055600150724       //--------------------------------------------------------------
055700150724       //?Esecuzione del comando (già impostato).                      ?
055800150724       //--------------------------------------------------------------
055900150724       BEGSR  sr_ExecCmd;
056000150724
056100150724         clear Qcap0100;
056200150724         Qcabcsdh = *off;
056300150724         Qcapa    = *off;
056400150724         Qcacmdss = *off;
056500150724         Qcaerved = *allX'00';
056600150724
056700150724         clear Qusec;
056800150724         Qusbprv  = %size(Qusec);
056900150724
057000150724         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
057100150724                           %size(Qcap0100) : 'CPOP0100' : *omit :
057200150724                           0 : 0 : Qusec);
057300150724
057400150724         //if  Qusei <> *blank;
057500150724         //  ...;
057600150724         //endif;
057700150724
057800150724       ENDSR;
057900150724
058000150724      /end-free
058100150724
058200150724       //--------------------------------------------------------------
058300150724       //?S P O O L - F I L E S                                        ?
058400150724       //--------------------------------------------------------------
058500150724
058600150724     oQSYSPRT   e            PRTtxt            1
058700150724     o                       RSUT
058800150727     o                                        +   5 'ERRORI RILEVATI I-
058900150727     o                                              N FASE DI STAMPA E-
059000150727     o                                              LENCO SPEDIZIONI'
059100150727     o                       SDSpgmName       +   5
059200150724     o                       wDate         y  +   3
059300150724     o          e            PRTtxt      0
059400150727     o                                        +  25 'ERRORI RILEVATI I-
059500150727     o                                              N FASE DI STAMPA E-
059600150727     o                                              LENCO SPEDIZIONI'
059700150724     o          e            PRTtxt      1
059800150724     o                       KNSIF
059900150724     o                       KNMUS            +   1
060000150727     o                                        +   4 'GIORNALIERE CON I-
060100150727     o                                              MPORTO DA ASSICURA-
060200150727     o                                              RE >100.000 EURO'
060300150727     o                                        +   5 'Pag.'
060400150724     o                       Page          z  +   0
060500150727     o                       wTime            +   5 '0 :  :  '
060600150727     o          e            PRTtxt      0
060700150727     o                                        +  25 'GIORNALIERE CON I-
060800150727     o                                              MPORTO DA ASSICURA-
060900150727     o                                              RE >100.000 EURO'
061000150724      *
061100150724     o          e            PRTerr1     2
061200150724     o                                              'RILEVATO ERRORE: +
061300150724     o                                               SQLCODE'
061400150724     o                       SQLcode       k  +   1
061500150724     o                                        +   1 'SQLSTATE'
061600150724     o                       SQLstate         +   1
061700150724     o                                        +   1 'DURANTE L''ESECUZ-
061800150724     o                                              IONE DEL SEGUENTE -
061900150724     o                                              COMANDO SQL:'
062000150724     o          e            PRTerr1     0  1
062100150724     o                                              'RILEVATO ERRORE: +
062200150724     o                                               SQLCODE'
062300150724     o                       SQLcode       k  +   1
062400150724     o                                        +   1 'SQLSTATE'
062500150724     o                       SQLstate         +   1
062600150724     o          e            PRTerr2     1
062700150727     o                       sk_Temp(xx)      +   3
