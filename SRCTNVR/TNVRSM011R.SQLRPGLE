000100110203       //==============================================================
000200111215       //?TNVRSM011R - Verifica sovrapposizioni range segnacolli in    ?
000300110914       //?             tabelle "3CP" e "3EW" (o solo "3C" - non parz.).?
000400110203       //==============================================================
000500110203
000600110203       //--------------------------------------------------------------
000700110203       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800110203       //--------------------------------------------------------------
000900110203
001000110203     /*PRM  dbgview(*source)
001100110914     /*CMD  ovrdbf file(TITAS33C) tofile(GAITRAGRPS/TITAS33C) +
001200110914     /*CMD         ovrscope(*calllvl)
001300110914     /*END  dltovr file(TITAS33C) lvl(*)
001400110203     /*END
001500110203
001600110203       //--------------------------------------------------------------
001700110203       //?Specifiche di controllo.                                     ?
001800110203       //--------------------------------------------------------------
001900110203
002000110203     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002100110914     h dftactgrp(*no) bnddir('UBBNDDIR':'TRUL')
002200110203     h alwnull(*inputonly)
002300110203
002400110203       //--------------------------------------------------------------
002500110203       //?Dichiarazione file.                                          ?
002600110203       //--------------------------------------------------------------
002700110914
002800110914       // -?Bolle Sede?
002900110914     fTITAS33C  if   e           k disk    usropn
003000110914     f                                     extfile(wLibFileSede)
003100110203
003200110203       // -?Printer File?
003300110209     fQSYSPRT   o    f  132        printer usropn oflind(*inOF)
003400110203
003500110203       //--------------------------------------------------------------
003600110203       //?Definizione costanti.                                        ?
003700110203       //--------------------------------------------------------------
003800110203
003900110203     d c_OvrPrtF       c                   const('OVRPRTF +
004000110203     d                                            file(QSYSPRT) +
004100110203     d                                            pagesize(66 132 *rowcol) +
004200110203     d                                            lpi(6) cpi(10) +
004300110203     d                                            usrdta(''SovrRngScl'')')
004400110203     d c_DltOvr        c                   const('DLTOVR +
004500110203     d                                            file(QSYSPRT)')
004600110203
004700110203       //--------------------------------------------------------------
004800110203       //?Definizione schiere.                                         ?
004900110203       //--------------------------------------------------------------
005000110203
005100110913       // -?Note in stampa?
005200110913     d $Note1          s                   like(s_Note)  dim(3)  inz
005300110913     d $Note2          s                   like(s_Note)  dim(3)  inz
005400110203
005500110203       //--------------------------------------------------------------
005600110203       //?Definizione aree dati.                                       ?
005700110203       //--------------------------------------------------------------
005800110203
005900110203
006000110203       //--------------------------------------------------------------
006100110203       //?Definizione strutture dati.                                  ?
006200110203       //--------------------------------------------------------------
006300110203
006400110203       // -?Status?
006500110203     d Status         sds
006600110203     d  SDSpgm           *proc
006700110203     d  SDSusr               254    263
006800110203
006900110203       // -?Dati estratti via SQL?
007000110203     d wSQLrecC1       ds                  inz  qualified
007100110203     d   sql_Filiale                  3    inz
007200110203     d   sql_Cliente                  4    inz
007300111215     d   sql_Padre                    7    inz
007400110203     d   sql_Serie                    2  0 inz
007500110203     d   sql_SgnCl_I                  7  0 inz
007600110203     d   sql_SgnCl_F                  7  0 inz
007700110203
007800110203     d SavSQLrec       ds                  inz  likeds(wSQLrecC1)
007900110203
008000110203       // -?Tab. "3C"?
008100110203     d ds3Ca         e ds                  inz  extname(ds3C)
008200110203     d                                          qualified
008300110203     d ds3Cb         e ds                  inz  extname(ds3C)
008400110203     d                                          qualified
008500110203
008600110203       //--------------------------------------------------------------
008700110203       //?Definizione variabili globali.                               ?
008800110203       //--------------------------------------------------------------
008900110913
009000110913       // -?Indici di schiera?
009100110913     d xx              s              3  0 inz
009200110913     d xy              s              3  0 inz
009300110913     d yy              s              3  0 inz
009400110914
009500110914       // -?Nome libreria del file di sede?
009600110914     d wLibFileSede    s             21    inz
009700110203
009800110203       // -?Stringa SQL da eseguire?
009900110916     d wSQL_C1         s           5000    inz  Varying
010000110203
010100110203       // -?Orario?
010200110203     d w_Time          s             10    inz
010300110203
010400110203       // -?Campi di stampa?
010500110913     d s_Filiale       s                   like(wSQLrecC1.sql_Filiale)
010600110203     d                                     inz
010700110913     d s_Cliente       s                   like(wSQLrecC1.sql_Cliente)
010800110203     d                                     inz
010900110913     d s_Serie         s                   like(wSQLrecC1.sql_Serie)
011000110203     d                                     inz
011100110913     d s_SgnCl_I       s                   like(wSQLrecC1.sql_SgnCl_I)
011200110203     d                                     inz
011300110913     d s_SgnCl_F       s                   like(wSQLrecC1.sql_SgnCl_F)
011400110203     d                                     inz
011500110914     d s_BloAnn        s              9    inz
011600110913     d s_RagSoc        s             23    inz
011700110913     d s_Padre         s                   like(ds3Ca.§3Ccks)
011800110209     d                                     inz
011900110914     d s_DtUltSpe      s              8  0 inz
012000110913     d s_Note          s             42    inz
012100110203     d w_TotSovrapp    s              5  0 inz
012200110914
012300110914       // -?Campi di comodo?
012400110914     d wDate_EUR       s               d   datfmt(*eur)  inz
012500110914     d wNRS            s                   like(ds3Ca.§3Cnrs)
012600110914     d                                     inz
012700110203
012800110203       //--------------------------------------------------------------
012900110203       //?Definizione procedure usate.                                 ?
013000110203       //--------------------------------------------------------------
013100110203
013200110203       // -?Reperimento dati tabelle?
013300110203      /copy gaitrasrc/srcProtoPI,TRULTAB
013400110203      /copy gaitrasrc/srcProtoPR,TRULTAB
013500110204
013600110204       // -?Controllo/Decodifica cliente?
013700110204      /copy gaitrasrc/srcProtoPI,TIBS69R
013800110204      /copy gaitrasrc/srcProtoPR,TIBS69R
013900110203
014000110203       // -?Parametri API QCAPCMD (Process Commands)?
014100110203     d Qcmd            s           1024    inz  varying
014200110203      /copy qSysInc/qRpgleSrc,QCAPCMD
014300110203      /copy gaitrasrc/srcProtoPR,QCAPCMD
014400110203
014500110203       // -?Parametri gestione errori API.?
014600110203      /copy qSysInc/qRpgleSrc,QUSEC
014700110914
014800110914       // -?Reperimento NETA sistema AS/400 corrente?
014900110914     d currSysNeta     s              8a   inz
015000110914      /copy gaitrasrc/srcProtoPr,UBRTVNETA
015100110203
015200110203       //--------------------------------------------------------------
015300110203       //?Definizione key-list.                                        ?
015400110203       //--------------------------------------------------------------
015500110203
015600110914       // -?File TITAS33C?
015700110914     d k03titas33    e ds                  extname(TITAS33C : *key)
015800110914     d                                     prefix(k_)
015900110914     d                                     inz
016000110203
016100110203       //--------------------------------------------------------------
016200110203       //?Riepilogo indicatori.                                        ?
016300110203       //--------------------------------------------------------------
016400110203
016500110203
016600110203       //--------------------------------------------------------------
016700110203       //?M A I N - L I N E                                            ?
016800110203       //--------------------------------------------------------------
016900110203
017000110203      /free
017100110203
017200110203       // -?Operazioni iniziali?
017300110203       exsr  sr_RoutInz;
017400110203
017500110203       // -?Estrazione loghi non utilizzati?
017600110203       DoU  sqlCode <> *zero;
017700110203         exsr  sr_ReadCursor;
017800110203       EndDo;
017900110203
018000110203       // -?Operazioni finali?
018100110203       exsr  sr_RoutEnd;
018200110203
018300110203       //--------------------------------------------------------------
018400110203       //?Operazioni iniziali.                                         ?
018500110203       //--------------------------------------------------------------
018600110203       BEGSR sr_RoutInz;
018700110203
018800110203         *inLR = *on;
018900110203
019000110203         // -?Impostazione opzioni per SQL?
019100110203         exec SQL   set option   DynUsrPrf = *owner,
019200110203                                 CloSqlCsr = *endmod;
019300110914
019400110914         // -?Verifica del sistema AS/400 corrente?
019500110914         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
019600110914           exsr  sr_RoutEnd;
019700110914         endif;
019800110203
019900110203         // -?Creazione stringa SQL da eseguire?
020000110203         wSQL_C1 = 'WITH +
020100110913
020200110203                    Tab_3CP as +
020300111215                    (select substr(TBEke1,  1, 3) as Filiale, +
020400111215                            substr(TBEke1,  4, 4) as Cliente, +
020500111215                            substr(TBLuni, 79, 7) as Padre, +
020600111215                            substr(TBEke2,  1, 2) as Serie, +
020700111215                            substr(TBEke2,  3, 7) as SgnCl_i, +
020800111215                            substr(TBEuni,  1, 7) as SgnCl_f +
020900111215                       from TNTBE00F inner join TABEL00F +
021000111215                         on TBEcod = ''3CP'' and TBLcod = ''3C'' +
021100111215                        and TBEatb = '' '' and TBLflg = '' '' +
021200111215                        and TBEke1 = TBLkey), +
021300110913
021400110203                    Tab_3EW as +
021500110203                    (select substr(TBEke1,  2, 3) as Filiale, +
021600110203                            substr(TBEke1,  5, 4) as Cliente, +
021700111215                            substr(TBLuni, 79, 7) as Padre, +
021800110203                            substr(TBEuni,  7, 2) as Serie, +
021900110203                            substr(TBEuni,  9, 7) as SgnCl_i, +
022000110203                            substr(TBEuni, 16, 7) as SgnCl_f +
022100111215                       from TNTBE00F inner join TABEL00F +
022200111215                         on TBEcod = ''3EW'' and TBLcod = ''3C'' +
022300111215                        and TBEatb = '' '' and TBLflg = '' '' +
022400111215                        and TBEke1 = ''0'' concat TBLkey),' ;
022500111215
022600111215         wSQL_C1 += ' +
022700111215                     Tab_3C as +
022800111215                    (select substr(TBLkey,  1, 3) as Filiale, +
022900111215                            substr(TBLkey,  4, 4) as Cliente, +
023000111215                            substr(TBLuni, 79, 7) as Padre, +
023100111215                            substr(TBLuni,  1, 2) as Serie, +
023200111215                                                1 as SgnCl_i, +
023300111215                                          9999999 as SgnCl_f +
023400110913                       from TABEL00F +
023500110913                      where TBLcod = ''3C'' +
023600110913                        and TBLflg = '' '' +
023700111215                        and substr(TBLuni,  1, 2) > ''00'' +
023800111215                        and substr(TBLuni, 44, 5) <> ''ESWEB'' +
023900111215                        and substr(TBLkey,  1, 7) not in +
024000110913                            (select Tab_3CP.Filiale concat Tab_3CP.Cliente +
024100111215                               from Tab_3CP))';
024200111215
024300111215         wSQL_C1 += ' +
024400111215                     select * +
024500111215                       from Tab_3C +
024600111215
024700111215                      union +
024800111215
024900111215                     select * +
025000111215                       from Tab_3CP +
025100111215
025200111215                      union +
025300111215
025400111215                     select * +
025500111215                       from Tab_3EW +
025600110913
025700110203                     order by Filiale, Serie, SgnCl_i, SgnCl_f, +
025800111215                              Padre, Cliente';
025900110203
026000110203         // -?Apertura cursore?
026100110203         exsr  sr_OpenCursor;
026200110914
026300110914         // -?Apertura file TITAS33C?
026400110914         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
026500110914           wLibFileSede = 'GAITRAGRU/TITAS33C';
026600110914         else ;
026700110914           wLibFileSede = 'GAITRAGRPS/TITAS33C';
026800110914         endif;
026900110914         open  TITAS33C;
027000110203
027100110203       ENDSR;
027200110203
027300110203       //--------------------------------------------------------------
027400110203       //?Esecuzione del comando (già impostato).                      ?
027500110203       //--------------------------------------------------------------
027600110203       BEGSR  sr_ExecCmd;
027700110203
027800110203         clear Qcap0100;
027900110203         Qcabcsdh = *off;
028000110203         Qcapa    = *off;
028100110203         Qcacmdss = *off;
028200110203         Qcaerved = *allX'00';
028300110203
028400110203         clear Qusec;
028500110203         Qusbprv  = %size(Qusec);
028600110203
028700110203         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
028800110203                           %size(Qcap0100) : 'CPOP0100' : *omit :
028900110203                           0 : 0 : Qusec);
029000110203
029100110203         if  Qusei <> *blank;
029200110203           exsr  sr_PrintErr;
029300110203         endif;
029400110203
029500110203       ENDSR;
029600110203
029700110203       //--------------------------------------------------------------
029800110203       //?Stampa segnalazione dell'errore rilevato.                    ?
029900110203       //--------------------------------------------------------------
030000110203       BEGSR  sr_PrintErr;
030100110203
030200110203         // -?Stampa del Dump?
030300110203         Dump(A);
030400110203
030500110203         // -?Stampa del Job-Log?
030600110203         Qcmd = 'DSPJOBLOG job(*) output(*print)';
030700110203         exsr  sr_ExecCmd;
030800110203
030900110203         // -?Chiusura del programma?
031000110203         exsr  sr_RoutEnd;
031100110203
031200110203       ENDSR;
031300110203
031400110203       //--------------------------------------------------------------
031500110203       //?Apertura cursore C1.                                         ?
031600110203       //--------------------------------------------------------------
031700110203       BEGSR  sr_OpenCursor;
031800110203
031900110203         // -?Dichiarazione del cursore?
032000110203         exec sql  prepare S1  from :wSQL_C1;
032100110203         exec sql  declare C1  cursor for S1;
032200110203
032300110203         // -?Apertura del cursore?
032400110203         exec sql   open C1;
032500110203
032600110203       ENDSR;
032700110203
032800110203       //--------------------------------------------------------------
032900110203       //?Lettura cursore C1.                                          ?
033000110203       //--------------------------------------------------------------
033100110203       BEGSR  sr_ReadCursor;
033200110203
033300110203         clear  wSQLrecC1;
033400110203
033500110203         exec SQL  fetch next  from C1  into :wSQLrecC1;
033600110203
033700110203         select;
033800110203           // -?Fine lettura?
033900110203           when  sqlCode = 100;
034000110203             exsr  sr_RoutEnd;
034100110203           // -?Errore?
034200110203           when  sqlCode < *zero;
034300110203             exsr  sr_PrintErr;
034400110203           // -?Elaborazione?
034500110203           other;
034600110203             exsr  sr_ElabRec;
034700110203             SavSQLrec = wSQLrecC1;
034800110203         endsl;
034900110203
035000110203       ENDSR;
035100110203
035200110203       //--------------------------------------------------------------
035300110203       //?Chiusura cursore C1.                                         ?
035400110203       //--------------------------------------------------------------
035500110203       BEGSR  sr_CloseCursor;
035600110203
035700110203         // -?Chiusura del cursore?
035800110203         exec sql   close C1;
035900110203
036000110203       ENDSR;
036100110203
036200110203       //--------------------------------------------------------------
036300110203       //?Elaborazione singolo record estratto.                        ?
036400110203       //--------------------------------------------------------------
036500110203       BEGSR  sr_ElabRec;
036600110203
036700110203         select;
036800110203
036900110203           // -?Controllo all'interno della stessa filiale?
037000110203           //                      ?e della stessa serie?
037100110203           when  wSQLrecC1.sql_Filiale <> SavSQLrec.sql_Filiale  or
037200110203                 wSQLrecC1.sql_Serie   <> SavSQLrec.sql_Serie;
037300110203             leavesr;
037400110203
037500110203           // -?Controllo sovrapposizione range?
037600110203           when  (wSQLrecC1.sql_SgnCl_I < SavSQLrec.sql_SgnCl_I   and
037700110203                  wSQLrecC1.sql_SgnCl_F < SavSQLrec.sql_SgnCl_I)  or
037800110203                 (wSQLrecC1.sql_SgnCl_I > SavSQLrec.sql_SgnCl_F   and
037900110203                  wSQLrecC1.sql_SgnCl_F > SavSQLrec.sql_SgnCl_F);
038000110203             leavesr;
038100110203
038200110203         endsl;
038300110913
038400110913         clear  xx;
038500110913         clear  yy;
038600110913         clear  $Note1;
038700110913         clear  $Note2;
038800110914         clear  ds3Ca;
038900110914         clear  ds3Cb;
039000110203
039100110913         // -?Reperimento dati 1° cliente da tab. "3C"?
039200110913         If  getTabella ( c_Tabel : '3C' : SavSQLrec.sql_Filiale +
039300110203                                           SavSQLrec.sql_Cliente :
039400110203                          *omit : *omit : *omit :
039500110203                          *omit : *omit :
039600110203                          *omit : *omit : *omit : *omit :
039700110203                          *omit : *omit : *omit : *omit :
039800110203                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
039900110203                        < *zero       OR
040000110203             ds_TNTBE.TBEatb <> *blank;
040100110203
040200110203           // -?Stampa segnalazione cliente non in "3C"?
040300110913           xx += 1;
040400110913           $Note1(xx) = 'Cliente ' + SavSQLrec.sql_Filiale +
040500110913                                     SavSQLrec.sql_Cliente +
040600110913                        ' non trovato in tab. "3C"';
040700110914           //exsr  sr_Stampa;
040800110203
040900110913         EndIf;
041000110203
041100110913         ds3Ca = ds_TNTBE.TBEuni;
041200110913
041300110913         // -?Reperimento "3C" x padre 1° cliente?
041400110913         If  %editc( ds3Ca.§3Ccks : 'X' ) <>
041500110913               SavSQLrec.sql_Filiale + SavSQLrec.sql_Cliente;
041600110913
041700110913           if  getTabella ( c_Tabel : '3C' : %editc( ds3Ca.§3Ccks : 'X' ) :
041800110913                            *omit : *omit : *omit :
041900110913                            *omit : *omit :
042000110913                            *omit : *omit : *omit : *omit :
042100110913                            *omit : *omit : *omit : *omit :
042200110913                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
042300110913                          < *zero       OR
042400110913               ds_TNTBE.TBEatb <> *blank;
042500110913
042600110913             // -?Stampa segnalazione unificante non in "3C"?
042700110913             xx += 1;
042800110913             $Note1(xx) = 'Unificante ' + %editc( ds3Ca.§3Ccks : 'X' ) +
042900110913                          ' non trovato in tab."3C"';
043000110914             //exsr  sr_Stampa;
043100110913
043200110913           endif;
043300110913
043400110913         EndIf;
043500110913
043600110913         // -?Reperimento dati 2° cliente da tab. "3C"?
043700110913         If  getTabella ( c_Tabel : '3C' : wSQLrecC1.sql_Filiale +
043800110913                                           wSQLrecC1.sql_Cliente :
043900110913                          *omit : *omit : *omit :
044000110913                          *omit : *omit :
044100110913                          *omit : *omit : *omit : *omit :
044200110913                          *omit : *omit : *omit : *omit :
044300110913                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
044400110913                        < *zero       OR
044500110913             ds_TNTBE.TBEatb <> *blank;
044600110913
044700110913           // -?Stampa segnalazione cliente non in "3C"?
044800110913           yy += 1;
044900110913           $Note2(yy) = 'Cliente ' + wSQLrecC1.sql_Filiale +
045000110913                                     wSQLrecC1.sql_Cliente +
045100110913                        ' non trovato in tab. "3C"';
045200110914           //exsr  sr_Stampa;
045300110913
045400110913         EndIf;
045500110913
045600110913         ds3Cb = ds_TNTBE.TBEuni;
045700110913
045800110913         // -?Reperimento "3C" x padre 2° cliente?
045900110913         If  %editc( ds3Cb.§3Ccks : 'X' ) <>
046000110913               wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente;
046100110913
046200110916           if  getTabella ( c_Tabel : '3C' : %editc( ds3Cb.§3Ccks : 'X' ) :
046300110913                            *omit : *omit : *omit :
046400110913                            *omit : *omit :
046500110913                            *omit : *omit : *omit : *omit :
046600110913                            *omit : *omit : *omit : *omit :
046700110913                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
046800110913                          < *zero       OR
046900110913               ds_TNTBE.TBEatb <> *blank;
047000110913
047100110913             // -?Stampa segnalazione unificante non in "3C"?
047200110913             yy += 1;
047300110916             $Note2(yy) = 'Unificante ' + %editc( ds3Cb.§3Ccks : 'X' ) +
047400110913                          ' non trovato in tab."3C"';
047500110914             //exsr  sr_Stampa;
047600110913
047700110913           endif;
047800110913
047900110913         EndIf;
048000110913
048100110913         // -?Stampa segnalazione range sovrapposto &?
048200110913         //                      ?padre non corrispondente?
048300110914         If  (%editc( ds3Cb.§3Ccks : 'X' ) <>
048400110914                SavSQLrec.sql_Filiale + SavSQLrec.sql_Cliente  and
048500110914                      ds3Ca.§3Ccks         <> ds3Cb.§3Ccks)    OR
048600110914             xx > *zero   or   yy > *zero;
048700110913
048800110913           exsr  sr_Stampa;
048900110913
049000110913         EndIf;
049100110203
049200110203       ENDSR;
049300110203
049400110203       //--------------------------------------------------------------
049500110203       //?Stampa segnalazione segmento di pagina.                      ?
049600110203       //--------------------------------------------------------------
049700110203       BEGSR  sr_Stampa;
049800110203
049900110203         // -?Test OverFlow e stampa testata?
050000110203         exsr  sr_TestOF;
050100110204
050200110204         // -?Decodifica 1° cliente?
050300110204         clear  TIBS69ds;
050400110204         //I69sif = knsif;
050500110204         I69kcc = 151;
050600110204         I69kac = %int( SavSQLrec.sql_Filiale + SavSQLrec.sql_Cliente );
050700110204         tibs69r( tibs69ds :
050800110204                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
050900110914
051000110914         // -?Reperimento data ultima spedizione con questa serie?
051100110914         //  ?per il 1° cliente?
051200110914         wNRS = %int(SavSQLrec.sql_Serie);
051300110914         exsr  sr_GetLastSpedDta;
051400110203
051500110204         // -?Stampa 1ª riga di dettaglio?
051600110913         s_Filiale = SavSQLrec.sql_Filiale;
051700110913         s_Cliente = SavSQLrec.sql_Cliente;
051800110913         s_Serie   = SavSQLrec.sql_Serie;
051900110913         s_SgnCl_I = SavSQLrec.sql_SgnCl_I;
052000110913         s_SgnCl_F = SavSQLrec.sql_SgnCl_F;
052100110913         s_RagSoc  = ACOrag;
052200110913         s_Padre   = ds3Ca.§3Ccks;
052300110913         clear  s_BloAnn;
052400110204         select;
052500110204           when  ACOflg <> *blank;
052600110914             s_BloAnn = 'ANNULLATO';
052700110204           when  ACOabl =  '8';
052800110914             s_BloAnn = 'BLOCCATO ';
052900110204         endsl;
053000110914         if  s_DtUltSpe > *zero;
053100110914           wDate_Eur = %date( s_DtUltSpe : *iso );
053200110914           s_DtUltSpe = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
053300110914                          ( %subdt(wDate_Eur : *months) * 10000 ) +
053400110914                          ( %subdt(wDate_Eur : *years) ) );
053500110914         endif;
053600110913         clear  s_Note;
053700110914         clear  xy;
053800110913         if  xx > *zero;
053900110913           xy = 1;
054000110913           s_Note = $Note1(xy);
054100110913         endif;
054200110913         except  PRTdet;
054300110913         DoW  xy < xx;
054400110913           xy += 1;
054500110913           s_Note = $Note1(xy);
054600110913           except  PRTnot;
054700110913         EndDo;
054800110204
054900110204         // -?Decodifica 2° cliente?
055000110204         clear  TIBS69ds;
055100110204         //I69sif = knsif;
055200110204         I69kcc = 151;
055300110204         I69kac = %int( wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente );
055400110204         tibs69r( tibs69ds :
055500110204                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
055600110914
055700110914         // -?Reperimento data ultima spedizione con questa serie?
055800110914         //  ?per il 2° cliente?
055900110914         wNRS = %int(wSQLrecC1.sql_Serie);
056000110914         exsr  sr_GetLastSpedDta;
056100110204
056200110204         // -?Stampa 2ª riga di dettaglio?
056300110913         s_Filiale = wSQLrecC1.sql_Filiale;
056400110913         s_Cliente = wSQLrecC1.sql_Cliente;
056500110913         s_Serie   = wSQLrecC1.sql_Serie;
056600110913         s_SgnCl_I = wSQLrecC1.sql_SgnCl_I;
056700110913         s_SgnCl_F = wSQLrecC1.sql_SgnCl_F;
056800110913         s_RagSoc  = ACOrag;
056900110913         s_Padre   = ds3Cb.§3Ccks;
057000110913         clear  s_BloAnn;
057100110204         select;
057200110204           when  ACOflg <> *blank;
057300110914             s_BloAnn = 'ANNULLATO';
057400110204           when  ACOabl =  '8';
057500110914             s_BloAnn = 'BLOCCATO ';
057600110204         endsl;
057700110914         if  s_DtUltSpe > *zero;
057800110914           wDate_Eur = %date( s_DtUltSpe : *iso );
057900110914           s_DtUltSpe = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
058000110914                          ( %subdt(wDate_Eur : *months) * 10000 ) +
058100110914                          ( %subdt(wDate_Eur : *years) ) );
058200110914         endif;
058300110913         clear  s_Note;
058400110914         clear  xy;
058500110913         if  yy > *zero;
058600110913           xy = 1;
058700110913           s_Note = $Note2(xy);
058800110913         endif;
058900110913         except  PRTdet;
059000110913         DoW  xy < yy;
059100110913           xy += 1;
059200110913           s_Note = $Note2(xy);
059300110913           except  PRTnot;
059400110913         EndDo;
059500110203
059600110203         except  PRTspace;
059700110203
059800110203         // -?Incremento n° totale sovrapposizioni?
059900110203         w_TotSovrapp += 1;
060000110203
060100110203       ENDSR;
060200110914
060300110914       //--------------------------------------------------------------
060400110914       //?Reperimento data ultima spedizione per il cliente.           ?
060500110914       //--------------------------------------------------------------
060600110914       BEGSR  sr_GetLastSpedDta;
060700110914
060800110914         clear  s_DtUltSpe;
060900110914         k_TASccm = I69kac;
061000110914         k_TASaas = *year;
061100110914         k_TASmgs = *hival;
061200110914         setll   %kds(k03titas33)  TITAS33C;
061300110914         readPe  %kds(k03titas33 : 1)  TITAS33C;
061400110914
061500110914do  1    DoW  not %eof(TITAS33C);
061600110914
061700110914sel 2      select;
061800110914             // -?Spedizione senza segnacolli: da scartare?
061900110914w   2        when  TASncd <= *zeros  and  TASfns <> 'S';
062000110914             // -?Spedizione con serie diversa: da scartare?
062100110914w   2        when  TASnrs <> wNRS;
062200110914             // -?Ultima spedizione valida?
062300110914x   2        other;
062400110914               s_DtUltSpe = (TASaas * 10000) + TASmgs;
062500110914               leave;
062600110914e   2      endsl;
062700110914
062800110914           readPe  %kds(k03titas33 : 1)  TITAS33C;
062900110914
063000110914e   1    EndDo;
063100110914
063200110914       ENDSR;
063300110203
063400110203       //--------------------------------------------------------------
063500110203       //?Test OverFlow in stampa.                                     ?
063600110203       //--------------------------------------------------------------
063700110203       BEGSR  sr_TestOF;
063800110203
063900110203         // -?Apertura file di stampa?
064000110203         if  not %open (QSYSPRT);
064100110203           Qcmd = c_OvrPrtF;
064200110203           exsr  sr_ExecCmd;
064300110203           open  QSYSPRT;
064400110203           *inOF = *on;
064500110203         endif;
064600110203
064700110203         // -?Test OverFlow e stampa testata?
064800110203         if  *inOF;
064900110203           w_Time = %char( %time() );
065000110203           except PRTtxt;
065100110203           *inOF  = *off;
065200110203         endif;
065300110203
065400110203       ENDSR;
065500110203
065600110203       //--------------------------------------------------------------
065700110203       //?Operazioni finali.                                           ?
065800110203       //--------------------------------------------------------------
065900110203       BEGSR  sr_RoutEnd;
066000110203
066100110203         // -?Chiusura cursore?
066200110203         exsr  sr_CloseCursor;
066300110203
066400110203         // -?Stampa "fine-lista"?
066500110203         exsr  sr_TestOF;
066600110203         except  PRTend;
066700110203
066800110203         // -?Chiusura printer-file?
066900110203         close  QSYSPRT;
067000110203
067100110203         // -?Cancellazione override?
067200110203         Qcmd = c_DltOvr;
067300110203         exsr  sr_ExecCmd;
067400110204
067500110204         // -?Chiusura applicazioni?
067600110204         cloTabella();
067700110204         clear  TIBS69ds;
067800110204         I69tla = 'C';
067900110204         tibs69r( tibs69ds :
068000110204                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
068100110203
068200110203         // -?Uscita?
068300110203         return;
068400110203
068500110203       ENDSR;
068600110203
068700110203      /end-free
068800110203
068900110203       //--------------------------------------------------------------
069000110203       //?P R I N T E R - F I L E S                                    ?
069100110203       //--------------------------------------------------------------
069200110203
069300110203     oQSYSPRT   e            PRTtxt            2
069400110913     o                                              'ELENCO SOVRAPPOSI-
069500110913     o                                              Z. RANGE SEGNACOLL-
069600110913     o                                              I IN TAB. "3CP"/"3-
069700110913     o                                              EW"/"3C"'
069800110203     o                       SDSpgm           +   5
069900110203     o                       SDSusr           +   5
070000110913     o                       *date         y  +   3
070100110203     o                       w_Time           +   5
070200110203     o                                        +   5 'Pag.'
070300110203     o                       Page          z  +   0
070400110203     o          e            PRTtxt      0  0
070500110203     o                                              'ELENCO SOVRAPPOSI-
070600110913     o                                              Z. RANGE SEGNACOLL-
070700110913     o                                              I IN TAB. "3CP"/"3-
070800110913     o                                              EW"/"3C"'
070900110203     o          e            PRTtxt      2
071000110914     o                                              'Fil'
071100110914     o                                        +   1 'Cliente'
071200110914     o                                        +  22 '  Padre'
071300110914     o                                        +   2 'Sr'
071400110914     o                                        +   2 '  SgnCl_I'
071500110914     o                                        +   2 '  SgnCl_F'
071600110914     o                                        +   2 'Cl.Bloc. '
071700110914     o                                        +   1 'Dt.Ult.Sp.'
071800110203     o          e            PRTtxt      0  0
071900110914     o                                              'Fil'
072000110914     o                                        +   1 'Cliente'
072100110914     o                                        +  22 '  Padre'
072200110914     o                                        +   2 'Sr'
072300110914     o                                        +   2 '  SgnCl_I'
072400110914     o                                        +   2 '  SgnCl_F'
072500110914     o                                        +   2 'Cl.Bloc. '
072600110914     o                                        +   1 'Dt.Ult.Sp.'
072700110203     o          e            PRTtxt      1
072800110914     o                                              '==='
072900110914     o                                        +   1 '======='
073000110914     o                                        +  22 '======='
073100110914     o                                        +   2 '=='
073200110914     o                                        +   2 '========='
073300110914     o                                        +   2 '========='
073400110914     o                                        +   2 '======== '
073500110914     o                                        +   1 '=========='
073600110203     o          e            PRTtxt      0  1
073700110914     o                                              '==='
073800110914     o                                        +   1 '======='
073900110914     o                                        +  22 '======='
074000110914     o                                        +   2 '=='
074100110914     o                                        +   2 '========='
074200110914     o                                        +   2 '========='
074300110914     o                                        +   2 '======== '
074400110914     o                                        +   1 '=========='
074500110203      *
074600110913     o          e            PRTdet      1
074700110914     o                       s_Filiale
074800110914     o                       s_Cliente        +   1
074900110914     o                       s_RagSoc         +   1
075000110914     o                       s_Padre          +   1
075100110914     o                       s_Serie       1  +   2
075200110914     o                       s_SgnCl_I     1  +   2
075300110914     o                       s_SgnCl_F     1  +   2
075400110914     o                       s_BloAnn         +   2
075500110914     o                       s_DtUltSpe       +   1 '  /  /    '
075600110913     o                       s_Note           +   2
075700110913      *
075800110913     o          e            PRTnot      1
075900110914     o                                              '   '
076000110914     o                                        +   1 '       '
076100110914     o                                        +  23 '       '
076200110914     o                                        +   1 '  '
076300110914     o                                        +   1 '         '
076400110914     o                                        +   1 '         '
076500110914     o                                        +   1 '        '
076600110914     o                                        +   1 '          '
076700110913     o                       s_Note           +   2
076800110203      *
076900110203     o          e            PRTspace    1
077000110203      *
077100110203     o          e            PRTend      2
077200110203     o                                        +  10 '***  Fine Lista  +
077300110203     o                                               ***'
077400110203     o                                        +   3 'Tot. sovrapposizioni = '
077500110203     o                       w_TotSovrapp  k  +   1
077600110203     o          e            PRTend      0  0
077700110203     o                                        +  10 '***  Fine Lista  +
077800110203     o                                               ***'
