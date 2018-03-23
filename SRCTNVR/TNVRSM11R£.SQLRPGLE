000100110203       //==============================================================
000200110929       //?TNVRSM011R - Verifica sovrapposizioni range segnacolli in    ?
000300110914       //?             tabelle "3CP" e "3EW" (o solo "3C" - non parz.).?
000400110914       //?>>>>>>>>>>>>>>>>>>>> SQL lungo eterno!!! <<<<<<<<<<<<<<<<<<<<?
000500110203       //==============================================================
000600110203
000700110203       //--------------------------------------------------------------
000800110203       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900110203       //--------------------------------------------------------------
001000110203
001100110203     /*PRM  dbgview(*source)
001200110203     /*END
001300110203
001400110203       //--------------------------------------------------------------
001500110203       //?Specifiche di controllo.                                     ?
001600110203       //--------------------------------------------------------------
001700110203
001800110203     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900110914     h dftactgrp(*no) bnddir('UBBNDDIR':'TRUL')
002000110203     h alwnull(*inputonly)
002100110203
002200110203       //--------------------------------------------------------------
002300110203       //?Dichiarazione file.                                          ?
002400110203       //--------------------------------------------------------------
002500110203
002600110203       // -?Printer File?
002700110209     fQSYSPRT   o    f  132        printer usropn oflind(*inOF)
002800110203
002900110203       //--------------------------------------------------------------
003000110203       //?Definizione costanti.                                        ?
003100110203       //--------------------------------------------------------------
003200110203
003300110203     d c_OvrPrtF       c                   const('OVRPRTF +
003400110203     d                                            file(QSYSPRT) +
003500110203     d                                            pagesize(66 132 *rowcol) +
003600110203     d                                            lpi(6) cpi(10) +
003700110203     d                                            usrdta(''SovrRngScl'')')
003800110203     d c_DltOvr        c                   const('DLTOVR +
003900110203     d                                            file(QSYSPRT)')
004000110203
004100110203       //--------------------------------------------------------------
004200110203       //?Definizione schiere.                                         ?
004300110203       //--------------------------------------------------------------
004400110203
004500110913       // -?Note in stampa?
004600110913     d $Note1          s                   like(s_Note)  dim(3)  inz
004700110913     d $Note2          s                   like(s_Note)  dim(3)  inz
004800110203
004900110203       //--------------------------------------------------------------
005000110203       //?Definizione aree dati.                                       ?
005100110203       //--------------------------------------------------------------
005200110203
005300110203
005400110203       //--------------------------------------------------------------
005500110203       //?Definizione strutture dati.                                  ?
005600110203       //--------------------------------------------------------------
005700110203
005800110203       // -?Status?
005900110203     d Status         sds
006000110203     d  SDSpgm           *proc
006100110203     d  SDSusr               254    263
006200110203
006300110203       // -?Dati estratti via SQL?
006400110203     d wSQLrecC1       ds                  inz  qualified
006500110203     d   sql_Filiale                  3    inz
006600110203     d   sql_Cliente                  4    inz
006700110203     d   sql_Serie                    2  0 inz
006800110203     d   sql_SgnCl_I                  7  0 inz
006900110203     d   sql_SgnCl_F                  7  0 inz
007000110914     d   sql_ACOrag                  48    inz
007100110914     d   sql_ACOabl                   1    inz
007200110914     d   sql_DtUltSpe                 8  0 inz
007300110203
007400110203     d SavSQLrec       ds                  inz  likeds(wSQLrecC1)
007500110203
007600110203       // -?Tab. "3C"?
007700110203     d ds3Ca         e ds                  inz  extname(ds3C)
007800110203     d                                          qualified
007900110203     d ds3Cb         e ds                  inz  extname(ds3C)
008000110203     d                                          qualified
008100110203
008200110203       //--------------------------------------------------------------
008300110203       //?Definizione variabili globali.                               ?
008400110203       //--------------------------------------------------------------
008500110913
008600110913       // -?Indici di schiera?
008700110913     d xx              s              3  0 inz
008800110913     d xy              s              3  0 inz
008900110913     d yy              s              3  0 inz
009000110914
009100110914       // -?Nome libreria del file di sede?
009200110914     d wLibrSede       s             10    inz
009300110203
009400110203       // -?Stringa SQL da eseguire?
009500110914     d wSQL_C1         s           5000    inz  Varying
009600110203
009700110203       // -?Orario?
009800110203     d w_Time          s             10    inz
009900110203
010000110203       // -?Campi di stampa?
010100110913     d s_Filiale       s                   like(wSQLrecC1.sql_Filiale)
010200110203     d                                     inz
010300110913     d s_Cliente       s                   like(wSQLrecC1.sql_Cliente)
010400110203     d                                     inz
010500110913     d s_Serie         s                   like(wSQLrecC1.sql_Serie)
010600110203     d                                     inz
010700110913     d s_SgnCl_I       s                   like(wSQLrecC1.sql_SgnCl_I)
010800110203     d                                     inz
010900110913     d s_SgnCl_F       s                   like(wSQLrecC1.sql_SgnCl_F)
011000110203     d                                     inz
011100110914     d s_BloAnn        s              9    inz
011200110913     d s_RagSoc        s             23    inz
011300110913     d s_Padre         s                   like(ds3Ca.§3Ccks)
011400110209     d                                     inz
011500110914     d s_DtUltSpe      s              8  0 inz
011600110913     d s_Note          s             42    inz
011700110203     d w_TotSovrapp    s              5  0 inz
011800110914
011900110914       // -?Campi di comodo?
012000110914     d wDate_EUR       s               d   datfmt(*eur)  inz
012100110914     d wNRS            s                   like(ds3Ca.§3Cnrs)
012200110914     d                                     inz
012300110203
012400110203       //--------------------------------------------------------------
012500110203       //?Definizione procedure usate.                                 ?
012600110203       //--------------------------------------------------------------
012700110203
012800110203       // -?Reperimento dati tabelle?
012900110203      /copy gaitrasrc/srcProtoPI,TRULTAB
013000110203      /copy gaitrasrc/srcProtoPR,TRULTAB
013100110203
013200110203       // -?Parametri API QCAPCMD (Process Commands)?
013300110203     d Qcmd            s           1024    inz  varying
013400110203      /copy qSysInc/qRpgleSrc,QCAPCMD
013500110203      /copy gaitrasrc/srcProtoPR,QCAPCMD
013600110203
013700110203       // -?Parametri gestione errori API.?
013800110203      /copy qSysInc/qRpgleSrc,QUSEC
013900110914
014000110914       // -?Reperimento NETA sistema AS/400 corrente?
014100110914     d currSysNeta     s              8a   inz
014200110914      /copy gaitrasrc/srcProtoPr,UBRTVNETA
014300110203
014400110203       //--------------------------------------------------------------
014500110203       //?Definizione key-list.                                        ?
014600110203       //--------------------------------------------------------------
014700110203
014800110203
014900110203       //--------------------------------------------------------------
015000110203       //?Riepilogo indicatori.                                        ?
015100110203       //--------------------------------------------------------------
015200110203
015300110203
015400110203       //--------------------------------------------------------------
015500110203       //?M A I N - L I N E                                            ?
015600110203       //--------------------------------------------------------------
015700110203
015800110203      /free
015900110203
016000110203       // -?Operazioni iniziali?
016100110203       exsr  sr_RoutInz;
016200110203
016300110203       // -?Estrazione loghi non utilizzati?
016400110203       DoU  sqlCode <> *zero;
016500110203         exsr  sr_ReadCursor;
016600110203       EndDo;
016700110203
016800110203       // -?Operazioni finali?
016900110203       exsr  sr_RoutEnd;
017000110203
017100110203       //--------------------------------------------------------------
017200110203       //?Operazioni iniziali.                                         ?
017300110203       //--------------------------------------------------------------
017400110203       BEGSR sr_RoutInz;
017500110203
017600110203         *inLR = *on;
017700110203
017800110203         // -?Impostazione opzioni per SQL?
017900110203         exec SQL   set option   DynUsrPrf = *owner,
018000110203                                 CloSqlCsr = *endmod;
018100110914
018200110914         // -?Verifica del sistema AS/400 corrente?
018300110914         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
018400110914           exsr  sr_RoutEnd;
018500110914         endif;
018600110203
018700110203         // -?Creazione stringa SQL da eseguire?
018800110203         wSQL_C1 = 'WITH +
018900110913
019000110203                    Tab_3CP as +
019100110203                    (select substr(TBEke1, 1, 3) as Filiale, +
019200110203                            substr(TBEke1, 4, 4) as Cliente, +
019300110203                            substr(TBEke2, 1, 2) as Serie, +
019400110203                            substr(TBEke2, 3, 7) as SgnCl_i, +
019500110203                            substr(TBEuni, 1, 7) as SgnCl_f +
019600110203                       from TNTBE00F +
019700110315                      where TBEcod = ''3CP'' +
019800110315                        and TBEatb = '' ''), +
019900110913
020000110203                    Tab_3EW as +
020100110203                    (select substr(TBEke1,  2, 3) as Filiale, +
020200110203                            substr(TBEke1,  5, 4) as Cliente, +
020300110203                            substr(TBEuni,  7, 2) as Serie, +
020400110203                            substr(TBEuni,  9, 7) as SgnCl_i, +
020500110203                            substr(TBEuni, 16, 7) as SgnCl_f +
020600110203                       from TNTBE00F +
020700110315                      where TBEcod = ''3EW'' +
020800110913                        and TBEatb = '' ''), +
020900110913
021000110914                     Tab_3C as +
021100110913                    (select substr(TBLkey, 1, 3) as Filiale, +
021200110913                            substr(TBLkey, 4, 4) as Cliente, +
021300110913                            substr(TBLuni, 1, 2) as Serie, +
021400110913                                               1 as SgnCl_i, +
021500110913                                         9999999 as SgnCl_f +
021600110913                       from TABEL00F +
021700110913                      where TBLcod = ''3C'' +
021800110913                        and TBLflg = '' '' +
021900110913                        and substr(TBLuni, 1, 2) > ''00'' +
022000110913                        and substr(TBLkey, 1, 7) not in +
022100110913                            (select Tab_3CP.Filiale concat Tab_3CP.Cliente +
022200110913                               from Tab_3CP +
022300110913                              union +
022400110913                             select Tab_3EW.Filiale concat Tab_3EW.Cliente +
022500110914                               from Tab_3EW)),';
022600110914
022700110914         wSQL_C1 += ' CNACO as +
022800110914                    (select ACOksc, ACOrag, ACOabl +
022900110914                       from CNACO00F +
023000110914                      where ACOkcc = ''0151'' +
023100110914                      order by ACOksc), +
023200110914
023300110914                      TITAS as +
023400110914                    (select TASksc, max((TASaas * 10000) + TASmgs) as TASaamgs +
023500110914                       from TITAS00F +
023600110914                      group by TASksc +
023700110914                      union +
023800110914                     select TASksc, max((TASaas * 10000) + TASmgs) as TASaamgs +
023900110914                       from TITAS10F +
024000110914                      group by TASksc +
024100110914                      union +
024200110914                     select TASksc, max((TASaas * 10000) + TASmgs) as TASaamgs +
024300110914                       from TITASP0F +
024400110914                      group by TASksc +
024500110914                      order by TASksc)';
024600110914
024700110914         wSQL_C1 += ' +
024800110914                    select Tab_3C.*, CNACO.ACOrag, CNACO.ACOabl, +
024900110914                           int(max(TITAS.TASaamgs)) as DtUltSped +
025000110914                      from Tab_3C left outer join CNACO +
025100110914                        on int(Tab_3C.Filiale concat Tab_3C.Cliente) = +
025200110914                           CNACO.ACOksc +
025300110914                                  left outer join TITAS +
025400110914                        on int(Tab_3C.Filiale concat Tab_3C.Cliente) = +
025500110914                           TITAS.TASksc +
025600110914                     group by Filiale, Serie, SgnCl_i, SgnCl_f, Cliente, +
025700110914                              CNACO.ACOrag, CNACO.ACOabl +
025800110913
025900110914                    union';
026000110913
026100110914         wSQL_C1 += ' +
026200110914                    select Tab_3CP.*, CNACO.ACOrag, CNACO.ACOabl, +
026300110914                           int(max(TITAS.TASaamgs)) as DtUltSped +
026400110914                      from Tab_3CP left outer join CNACO +
026500110914                        on int(Tab_3CP.Filiale concat Tab_3CP.Cliente) = +
026600110914                               CNACO.ACOksc +
026700110914                                  left outer join TITAS +
026800110914                        on int(Tab_3CP.Filiale concat Tab_3CP.Cliente) = +
026900110914                               TITAS.TASksc +
027000110914                     group by Filiale, Serie, SgnCl_i, SgnCl_f, Cliente, +
027100110914                              CNACO.ACOrag, CNACO.ACOabl';
027200110913
027300110914         wSQL_C1 += ' +
027400110203                    union +
027500110913
027600110914                    select Tab_3EW.*, CNACO.ACOrag, CNACO.ACOabl, +
027700110914                           int(max(TITAS.TASaamgs)) as DtUltSped +
027800110914                      from Tab_3EW left outer join CNACO +
027900110914                        on int(Tab_3EW.Filiale concat Tab_3EW.Cliente) = +
028000110914                               CNACO.ACOksc +
028100110914                                  left outer join TITAS +
028200110914                        on int(Tab_3EW.Filiale concat Tab_3EW.Cliente) = +
028300110914                               TITAS.TASksc +
028400110914                     group by Filiale, Serie, SgnCl_i, SgnCl_f, Cliente, +
028500110914                              CNACO.ACOrag, CNACO.ACOabl';
028600110913
028700110914         wSQL_C1 += ' +
028800110203                     order by Filiale, Serie, SgnCl_i, SgnCl_f, +
028900110203                              Cliente';
029000110203
029100110203         // -?Apertura cursore?
029200110203         exsr  sr_OpenCursor;
029300110203
029400110203       ENDSR;
029500110203
029600110203       //--------------------------------------------------------------
029700110203       //?Esecuzione del comando (già impostato).                      ?
029800110203       //--------------------------------------------------------------
029900110203       BEGSR  sr_ExecCmd;
030000110203
030100110203         clear Qcap0100;
030200110203         Qcabcsdh = *off;
030300110203         Qcapa    = *off;
030400110203         Qcacmdss = *off;
030500110203         Qcaerved = *allX'00';
030600110203
030700110203         clear Qusec;
030800110203         Qusbprv  = %size(Qusec);
030900110203
031000110203         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
031100110203                           %size(Qcap0100) : 'CPOP0100' : *omit :
031200110203                           0 : 0 : Qusec);
031300110203
031400110203         if  Qusei <> *blank;
031500110203           exsr  sr_PrintErr;
031600110203         endif;
031700110203
031800110203       ENDSR;
031900110203
032000110203       //--------------------------------------------------------------
032100110203       //?Stampa segnalazione dell'errore rilevato.                    ?
032200110203       //--------------------------------------------------------------
032300110203       BEGSR  sr_PrintErr;
032400110203
032500110203         // -?Stampa del Dump?
032600110203         Dump(A);
032700110203
032800110203         // -?Stampa del Job-Log?
032900110203         Qcmd = 'DSPJOBLOG job(*) output(*print)';
033000110203         exsr  sr_ExecCmd;
033100110203
033200110203         // -?Chiusura del programma?
033300110203         exsr  sr_RoutEnd;
033400110203
033500110203       ENDSR;
033600110203
033700110203       //--------------------------------------------------------------
033800110203       //?Apertura cursore C1.                                         ?
033900110203       //--------------------------------------------------------------
034000110203       BEGSR  sr_OpenCursor;
034100110203
034200110203         // -?Dichiarazione del cursore?
034300110203         exec sql  prepare S1  from :wSQL_C1;
034400110203         exec sql  declare C1  cursor for S1;
034500110203
034600110203         // -?Apertura del cursore?
034700110203         exec sql   open C1;
034800110203
034900110203       ENDSR;
035000110203
035100110203       //--------------------------------------------------------------
035200110203       //?Lettura cursore C1.                                          ?
035300110203       //--------------------------------------------------------------
035400110203       BEGSR  sr_ReadCursor;
035500110203
035600110203         clear  wSQLrecC1;
035700110203
035800110203         exec SQL  fetch next  from C1  into :wSQLrecC1;
035900110203
036000110203         select;
036100110203           // -?Fine lettura?
036200110203           when  sqlCode = 100;
036300110203             exsr  sr_RoutEnd;
036400110203           // -?Errore?
036500110203           when  sqlCode < *zero;
036600110203             exsr  sr_PrintErr;
036700110203           // -?Elaborazione?
036800110203           other;
036900110203             exsr  sr_ElabRec;
037000110203             SavSQLrec = wSQLrecC1;
037100110203         endsl;
037200110203
037300110203       ENDSR;
037400110203
037500110203       //--------------------------------------------------------------
037600110203       //?Chiusura cursore C1.                                         ?
037700110203       //--------------------------------------------------------------
037800110203       BEGSR  sr_CloseCursor;
037900110203
038000110203         // -?Chiusura del cursore?
038100110203         exec sql   close C1;
038200110203
038300110203       ENDSR;
038400110203
038500110203       //--------------------------------------------------------------
038600110203       //?Elaborazione singolo record estratto.                        ?
038700110203       //--------------------------------------------------------------
038800110203       BEGSR  sr_ElabRec;
038900110203
039000110203         select;
039100110203
039200110203           // -?Controllo all'interno della stessa filiale?
039300110203           //                      ?e della stessa serie?
039400110203           when  wSQLrecC1.sql_Filiale <> SavSQLrec.sql_Filiale  or
039500110203                 wSQLrecC1.sql_Serie   <> SavSQLrec.sql_Serie;
039600110203             leavesr;
039700110203
039800110203           // -?Controllo sovrapposizione range?
039900110203           when  (wSQLrecC1.sql_SgnCl_I < SavSQLrec.sql_SgnCl_I   and
040000110203                  wSQLrecC1.sql_SgnCl_F < SavSQLrec.sql_SgnCl_I)  or
040100110203                 (wSQLrecC1.sql_SgnCl_I > SavSQLrec.sql_SgnCl_F   and
040200110203                  wSQLrecC1.sql_SgnCl_F > SavSQLrec.sql_SgnCl_F);
040300110203             leavesr;
040400110203
040500110203         endsl;
040600110913
040700110913         clear  xx;
040800110913         clear  yy;
040900110913         clear  $Note1;
041000110913         clear  $Note2;
041100110914         clear  ds3Ca;
041200110914         clear  ds3Cb;
041300110203
041400110913         // -?Reperimento dati 1° cliente da tab. "3C"?
041500110913         If  getTabella ( c_Tabel : '3C' : SavSQLrec.sql_Filiale +
041600110203                                           SavSQLrec.sql_Cliente :
041700110203                          *omit : *omit : *omit :
041800110203                          *omit : *omit :
041900110203                          *omit : *omit : *omit : *omit :
042000110203                          *omit : *omit : *omit : *omit :
042100110203                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
042200110203                        < *zero       OR
042300110203             ds_TNTBE.TBEatb <> *blank;
042400110203
042500110203           // -?Stampa segnalazione cliente non in "3C"?
042600110913           xx += 1;
042700110913           $Note1(xx) = 'Cliente ' + SavSQLrec.sql_Filiale +
042800110913                                     SavSQLrec.sql_Cliente +
042900110913                        ' non trovato in tab. "3C"';
043000110914           //exsr  sr_Stampa;
043100110203
043200110913         EndIf;
043300110203
043400110913         ds3Ca = ds_TNTBE.TBEuni;
043500110913
043600110913         // -?Reperimento "3C" x padre 1° cliente?
043700110913         If  %editc( ds3Ca.§3Ccks : 'X' ) <>
043800110913               SavSQLrec.sql_Filiale + SavSQLrec.sql_Cliente;
043900110913
044000110913           if  getTabella ( c_Tabel : '3C' : %editc( ds3Ca.§3Ccks : 'X' ) :
044100110913                            *omit : *omit : *omit :
044200110913                            *omit : *omit :
044300110913                            *omit : *omit : *omit : *omit :
044400110913                            *omit : *omit : *omit : *omit :
044500110913                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
044600110913                          < *zero       OR
044700110913               ds_TNTBE.TBEatb <> *blank;
044800110913
044900110913             // -?Stampa segnalazione unificante non in "3C"?
045000110913             xx += 1;
045100110913             $Note1(xx) = 'Unificante ' + %editc( ds3Ca.§3Ccks : 'X' ) +
045200110913                          ' non trovato in tab."3C"';
045300110914             //exsr  sr_Stampa;
045400110913
045500110913           endif;
045600110913
045700110913
045800110913         EndIf;
045900110913
046000110913         // -?Reperimento dati 2° cliente da tab. "3C"?
046100110913         If  getTabella ( c_Tabel : '3C' : wSQLrecC1.sql_Filiale +
046200110913                                           wSQLrecC1.sql_Cliente :
046300110913                          *omit : *omit : *omit :
046400110913                          *omit : *omit :
046500110913                          *omit : *omit : *omit : *omit :
046600110913                          *omit : *omit : *omit : *omit :
046700110913                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
046800110913                        < *zero       OR
046900110913             ds_TNTBE.TBEatb <> *blank;
047000110913
047100110913           // -?Stampa segnalazione cliente non in "3C"?
047200110913           yy += 1;
047300110913           $Note2(yy) = 'Cliente ' + wSQLrecC1.sql_Filiale +
047400110913                                     wSQLrecC1.sql_Cliente +
047500110913                        ' non trovato in tab. "3C"';
047600110914           //exsr  sr_Stampa;
047700110913
047800110913         EndIf;
047900110913
048000110913         ds3Cb = ds_TNTBE.TBEuni;
048100110913
048200110913         // -?Reperimento "3C" x padre 2° cliente?
048300110913         If  %editc( ds3Cb.§3Ccks : 'X' ) <>
048400110913               wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente;
048500110913
048600110913           if  getTabella ( c_Tabel : '3C' : %editc( ds3Ca.§3Ccks : 'X' ) :
048700110913                            *omit : *omit : *omit :
048800110913                            *omit : *omit :
048900110913                            *omit : *omit : *omit : *omit :
049000110913                            *omit : *omit : *omit : *omit :
049100110913                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
049200110913                          < *zero       OR
049300110913               ds_TNTBE.TBEatb <> *blank;
049400110913
049500110913             // -?Stampa segnalazione unificante non in "3C"?
049600110913             yy += 1;
049700110913             $Note2(yy) = 'Unificante ' + %editc( ds3Ca.§3Ccks : 'X' ) +
049800110913                          ' non trovato in tab."3C"';
049900110914             //exsr  sr_Stampa;
050000110913
050100110913           endif;
050200110913
050300110913         EndIf;
050400110913
050500110913         // -?Stampa segnalazione range sovrapposto &?
050600110913         //                      ?padre non corrispondente?
050700110914         If  (%editc( ds3Cb.§3Ccks : 'X' ) <>
050800110914                SavSQLrec.sql_Filiale + SavSQLrec.sql_Cliente  and
050900110914                      ds3Ca.§3Ccks         <> ds3Cb.§3Ccks)    OR
051000110914             xx > *zero   or   yy > *zero;
051100110913
051200110913           exsr  sr_Stampa;
051300110913
051400110913         EndIf;
051500110203
051600110203       ENDSR;
051700110203
051800110203       //--------------------------------------------------------------
051900110203       //?Stampa segnalazione segmento di pagina.                      ?
052000110203       //--------------------------------------------------------------
052100110203       BEGSR  sr_Stampa;
052200110203
052300110203         // -?Test OverFlow e stampa testata?
052400110203         exsr  sr_TestOF;
052500110203
052600110204         // -?Stampa 1ª riga di dettaglio?
052700110913         s_Filiale = SavSQLrec.sql_Filiale;
052800110913         s_Cliente = SavSQLrec.sql_Cliente;
052900110913         s_Serie   = SavSQLrec.sql_Serie;
053000110913         s_SgnCl_I = SavSQLrec.sql_SgnCl_I;
053100110913         s_SgnCl_F = SavSQLrec.sql_SgnCl_F;
053200110914         s_RagSoc  = SavSQLrec.sql_ACOrag;
053300110913         s_Padre   = ds3Ca.§3Ccks;
053400110913         clear  s_BloAnn;
053500110914         if  SavSQLrec.sql_ACOabl =  '8';
053600110914           s_BloAnn = 'BLOCCATO ';
053700110914         endif;
053800110914         if  SavSQLrec.sql_DtUltSpe > *zero;
053900110914           wDate_Eur = %date( SavSQLrec.sql_DtUltSpe : *iso );
054000110914           s_DtUltSpe = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
054100110914                          ( %subdt(wDate_Eur : *months) * 10000 ) +
054200110914                          ( %subdt(wDate_Eur : *years) ) );
054300110914         endif;
054400110913         clear  s_Note;
054500110914         clear  xy;
054600110913         if  xx > *zero;
054700110913           xy = 1;
054800110913           s_Note = $Note1(xy);
054900110913         endif;
055000110913         except  PRTdet;
055100110913         DoW  xy < xx;
055200110913           xy += 1;
055300110913           s_Note = $Note1(xy);
055400110913           except  PRTnot;
055500110913         EndDo;
055600110204
055700110204         // -?Stampa 2ª riga di dettaglio?
055800110913         s_Filiale = wSQLrecC1.sql_Filiale;
055900110913         s_Cliente = wSQLrecC1.sql_Cliente;
056000110913         s_Serie   = wSQLrecC1.sql_Serie;
056100110913         s_SgnCl_I = wSQLrecC1.sql_SgnCl_I;
056200110913         s_SgnCl_F = wSQLrecC1.sql_SgnCl_F;
056300110914         s_RagSoc  = wSQLrecC1.sql_ACOrag;
056400110913         s_Padre   = ds3Cb.§3Ccks;
056500110913         clear  s_BloAnn;
056600110914         if  wSQLrecC1.sql_ACOabl =  '8';
056700110914           s_BloAnn = 'BLOCCATO ';
056800110914         endif;
056900110914         if  wSQLrecC1.sql_DtUltSpe > *zero;
057000110914           wDate_Eur = %date( wSQLrecC1.sql_DtUltSpe : *iso );
057100110914           s_DtUltSpe = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
057200110914                          ( %subdt(wDate_Eur : *months) * 10000 ) +
057300110914                          ( %subdt(wDate_Eur : *years) ) );
057400110914         endif;
057500110913         clear  s_Note;
057600110914         clear  xy;
057700110913         if  yy > *zero;
057800110913           xy = 1;
057900110913           s_Note = $Note2(xy);
058000110913         endif;
058100110913         except  PRTdet;
058200110913         DoW  xy < yy;
058300110913           xy += 1;
058400110913           s_Note = $Note2(xy);
058500110913           except  PRTnot;
058600110913         EndDo;
058700110203
058800110203         except  PRTspace;
058900110203
059000110203         // -?Incremento n° totale sovrapposizioni?
059100110203         w_TotSovrapp += 1;
059200110203
059300110203       ENDSR;
059400110203
059500110203       //--------------------------------------------------------------
059600110203       //?Test OverFlow in stampa.                                     ?
059700110203       //--------------------------------------------------------------
059800110203       BEGSR  sr_TestOF;
059900110203
060000110203         // -?Apertura file di stampa?
060100110203         if  not %open (QSYSPRT);
060200110203           Qcmd = c_OvrPrtF;
060300110203           exsr  sr_ExecCmd;
060400110203           open  QSYSPRT;
060500110203           *inOF = *on;
060600110203         endif;
060700110203
060800110203         // -?Test OverFlow e stampa testata?
060900110203         if  *inOF;
061000110203           w_Time = %char( %time() );
061100110203           except PRTtxt;
061200110203           *inOF  = *off;
061300110203         endif;
061400110203
061500110203       ENDSR;
061600110203
061700110203       //--------------------------------------------------------------
061800110203       //?Operazioni finali.                                           ?
061900110203       //--------------------------------------------------------------
062000110203       BEGSR  sr_RoutEnd;
062100110203
062200110203         // -?Chiusura cursore?
062300110203         exsr  sr_CloseCursor;
062400110203
062500110203         // -?Stampa "fine-lista"?
062600110203         exsr  sr_TestOF;
062700110203         except  PRTend;
062800110203
062900110203         // -?Chiusura printer-file?
063000110203         close  QSYSPRT;
063100110203
063200110203         // -?Cancellazione override?
063300110203         Qcmd = c_DltOvr;
063400110203         exsr  sr_ExecCmd;
063500110204
063600110204         // -?Chiusura applicazioni?
063700110204         cloTabella();
063800110203
063900110203         // -?Uscita?
064000110203         return;
064100110203
064200110203       ENDSR;
064300110203
064400110203      /end-free
064500110203
064600110203       //--------------------------------------------------------------
064700110203       //?P R I N T E R - F I L E S                                    ?
064800110203       //--------------------------------------------------------------
064900110203
065000110203     oQSYSPRT   e            PRTtxt            2
065100110913     o                                              'ELENCO SOVRAPPOSI-
065200110913     o                                              Z. RANGE SEGNACOLL-
065300110913     o                                              I IN TAB. "3CP"/"3-
065400110913     o                                              EW"/"3C"'
065500110203     o                       SDSpgm           +   5
065600110203     o                       SDSusr           +   5
065700110913     o                       *date         y  +   3
065800110203     o                       w_Time           +   5
065900110203     o                                        +   5 'Pag.'
066000110203     o                       Page          z  +   0
066100110203     o          e            PRTtxt      0  0
066200110203     o                                              'ELENCO SOVRAPPOSI-
066300110913     o                                              Z. RANGE SEGNACOLL-
066400110913     o                                              I IN TAB. "3CP"/"3-
066500110913     o                                              EW"/"3C"'
066600110203     o          e            PRTtxt      2
066700110914     o                                              'Fil'
066800110914     o                                        +   1 'Cliente'
066900110914     o                                        +  22 '  Padre'
067000110914     o                                        +   2 'Sr'
067100110914     o                                        +   2 '  SgnCl_I'
067200110914     o                                        +   2 '  SgnCl_F'
067300110914     o                                        +   2 'Cl.Bloc. '
067400110914     o                                        +   1 'Dt.Ult.Sp.'
067500110203     o          e            PRTtxt      0  0
067600110914     o                                              'Fil'
067700110914     o                                        +   1 'Cliente'
067800110914     o                                        +  22 '  Padre'
067900110914     o                                        +   2 'Sr'
068000110914     o                                        +   2 '  SgnCl_I'
068100110914     o                                        +   2 '  SgnCl_F'
068200110914     o                                        +   2 'Cl.Bloc. '
068300110914     o                                        +   1 'Dt.Ult.Sp.'
068400110203     o          e            PRTtxt      1
068500110914     o                                              '==='
068600110914     o                                        +   1 '======='
068700110914     o                                        +  22 '======='
068800110914     o                                        +   2 '=='
068900110914     o                                        +   2 '========='
069000110914     o                                        +   2 '========='
069100110914     o                                        +   2 '======== '
069200110914     o                                        +   1 '=========='
069300110203     o          e            PRTtxt      0  1
069400110914     o                                              '==='
069500110914     o                                        +   1 '======='
069600110914     o                                        +  22 '======='
069700110914     o                                        +   2 '=='
069800110914     o                                        +   2 '========='
069900110914     o                                        +   2 '========='
070000110914     o                                        +   2 '======== '
070100110914     o                                        +   1 '=========='
070200110203      *
070300110913     o          e            PRTdet      1
070400110914     o                       s_Filiale
070500110914     o                       s_Cliente        +   1
070600110914     o                       s_RagSoc         +   1
070700110914     o                       s_Padre          +   1
070800110914     o                       s_Serie       1  +   2
070900110914     o                       s_SgnCl_I     1  +   2
071000110914     o                       s_SgnCl_F     1  +   2
071100110914     o                       s_BloAnn         +   2
071200110914     o                       s_DtUltSpe       +   1 '  /  /    '
071300110913     o                       s_Note           +   2
071400110913      *
071500110913     o          e            PRTnot      1
071600110914     o                                              '   '
071700110914     o                                        +   1 '       '
071800110914     o                                        +  23 '       '
071900110914     o                                        +   1 '  '
072000110914     o                                        +   1 '         '
072100110914     o                                        +   1 '         '
072200110914     o                                        +   1 '        '
072300110914     o                                        +   1 '          '
072400110913     o                       s_Note           +   2
072500110203      *
072600110203     o          e            PRTspace    1
072700110203      *
072800110203     o          e            PRTend      2
072900110203     o                                        +  10 '***  Fine Lista  +
073000110203     o                                               ***'
073100110203     o                                        +   3 'Tot. sovrapposizioni = '
073200110203     o                       w_TotSovrapp  k  +   1
073300110203     o          e            PRTend      0  0
073400110203     o                                        +  10 '***  Fine Lista  +
073500110203     o                                               ***'
