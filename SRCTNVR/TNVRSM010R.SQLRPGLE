000100110203       //==============================================================
000200110203       //?TNVRSM010R - Verifica sovrapposizioni range segnacolli in    ?
000300110203       //?             tabelle "3CP" e "3EW".                          ?
000400110203       //==============================================================
000500110203
000600110203       //--------------------------------------------------------------
000700110203       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800110203       //--------------------------------------------------------------
000900110203
001000110203     /*PRM  dbgview(*source)
001100110916     /*CMD  ovrdbf file(TITAS33C) tofile(GAITRAGRPS/TITAS33C) +
001200110916     /*CMD         ovrscope(*calllvl)
001300110916     /*END  dltovr file(TITAS33C) lvl(*)
001400110203     /*END
001500110203
001600110203       //--------------------------------------------------------------
001700110203       //?Specifiche di controllo.                                     ?
001800110203       //--------------------------------------------------------------
001900110203
002000110203     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002100110916     h dftactgrp(*no) bnddir('UBBNDDIR':'TRUL')
002200110203     h alwnull(*inputonly)
002300110203
002400110203       //--------------------------------------------------------------
002500110203       //?Dichiarazione file.                                          ?
002600110203       //--------------------------------------------------------------
002700110916
002800110916       // -?Bolle Sede?
002900110916     fTITAS33C  if   e           k disk    usropn
003000110916     f                                     extfile(wLibFileSede)
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
005100110916       // -?Note in stampa?
005200110916     d $Note1          s                   like(s_Note)  dim(3)  inz
005300110916     d $Note2          s                   like(s_Note)  dim(3)  inz
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
007300110203     d   sql_Serie                    2  0 inz
007400110203     d   sql_SgnCl_I                  7  0 inz
007500110203     d   sql_SgnCl_F                  7  0 inz
007600110203
007700110203     d SavSQLrec       ds                  inz  likeds(wSQLrecC1)
007800110203
007900110203       // -?Tab. "3C"?
008000110203     d ds3Ca         e ds                  inz  extname(ds3C)
008100110203     d                                          qualified
008200110203     d ds3Cb         e ds                  inz  extname(ds3C)
008300110203     d                                          qualified
008400110203
008500110203       //--------------------------------------------------------------
008600110203       //?Definizione variabili globali.                               ?
008700110203       //--------------------------------------------------------------
008800110916
008900110916       // -?Indici di schiera?
009000110916     d xx              s              3  0 inz
009100110916     d xy              s              3  0 inz
009200110916     d yy              s              3  0 inz
009300110916
009400110916       // -?Nome libreria del file di sede?
009500110916     d wLibFileSede    s             21    inz
009600110203
009700110203       // -?Stringa SQL da eseguire?
009800110916     d wSQL_C1         s           2048    Varying        inz
009900110203
010000110203       // -?Orario?
010100110203     d w_Time          s             10    inz
010200110203
010300110203       // -?Campi di stampa?
010400110203     d s_Filiale       s                   like(wSQLrecC1.sql_Filiale)
010500110203     d                                     inz
010600110203     d s_Cliente       s                   like(wSQLrecC1.sql_Cliente)
010700110203     d                                     inz
010800110203     d s_Serie         s                   like(wSQLrecC1.sql_Serie)
010900110203     d                                     inz
011000110203     d s_SgnCl_I       s                   like(wSQLrecC1.sql_SgnCl_I)
011100110203     d                                     inz
011200110203     d s_SgnCl_F       s                   like(wSQLrecC1.sql_SgnCl_F)
011300110203     d                                     inz
011400110916     d s_BloAnn        s              9    inz
011500110209     d s_RagSoc        s             23    inz
011600110209     d s_Padre         s                   like(ds3Ca.§3Ccks)
011700110209     d                                     inz
011800110916     d s_DtUltSpe      s              8  0 inz
011900110916     d s_Note          s             42    inz
012000110203     d w_TotSovrapp    s              5  0 inz
012100110916
012200110916       // -?Campi di comodo?
012300110916     d wDate_EUR       s               d   datfmt(*eur)  inz
012400110916     d wNRS            s                   like(ds3Ca.§3Cnrs)
012500110916     d                                     inz
012600110203
012700110203       //--------------------------------------------------------------
012800110203       //?Definizione procedure usate.                                 ?
012900110203       //--------------------------------------------------------------
013000110203
013100110203       // -?Reperimento dati tabelle?
013200110203      /copy gaitrasrc/srcProtoPI,TRULTAB
013300110203      /copy gaitrasrc/srcProtoPR,TRULTAB
013400110204
013500110204       // -?Controllo/Decodifica cliente?
013600110204      /copy gaitrasrc/srcProtoPI,TIBS69R
013700110204      /copy gaitrasrc/srcProtoPR,TIBS69R
013800110203
013900110203       // -?Parametri API QCAPCMD (Process Commands)?
014000110203     d Qcmd            s           1024    inz  varying
014100110203      /copy qSysInc/qRpgleSrc,QCAPCMD
014200110203      /copy gaitrasrc/srcProtoPR,QCAPCMD
014300110203
014400110203       // -?Parametri gestione errori API.?
014500110203      /copy qSysInc/qRpgleSrc,QUSEC
014600110916
014700110916       // -?Reperimento NETA sistema AS/400 corrente?
014800110916     d currSysNeta     s              8a   inz
014900110916      /copy gaitrasrc/srcProtoPr,UBRTVNETA
015000110203
015100110203       //--------------------------------------------------------------
015200110203       //?Definizione key-list.                                        ?
015300110203       //--------------------------------------------------------------
015400110203
015500110916       // -?File TITAS33C?
015600110916     d k03titas33    e ds                  extname(TITAS33C : *key)
015700110916     d                                     prefix(k_)
015800110916     d                                     inz
015900110203
016000110203       //--------------------------------------------------------------
016100110203       //?Riepilogo indicatori.                                        ?
016200110203       //--------------------------------------------------------------
016300110203
016400110203
016500110203       //--------------------------------------------------------------
016600110203       //?M A I N - L I N E                                            ?
016700110203       //--------------------------------------------------------------
016800110203
016900110203      /free
017000110203
017100110203       // -?Operazioni iniziali?
017200110203       exsr  sr_RoutInz;
017300110203
017400110203       // -?Estrazione loghi non utilizzati?
017500110203       DoU  sqlCode <> *zero;
017600110203         exsr  sr_ReadCursor;
017700110203       EndDo;
017800110203
017900110203       // -?Operazioni finali?
018000110203       exsr  sr_RoutEnd;
018100110203
018200110203       //--------------------------------------------------------------
018300110203       //?Operazioni iniziali.                                         ?
018400110203       //--------------------------------------------------------------
018500110203       BEGSR sr_RoutInz;
018600110203
018700110203         *inLR = *on;
018800110203
018900110203         // -?Impostazione opzioni per SQL?
019000110203         exec SQL   set option   DynUsrPrf = *owner,
019100110203                                 CloSqlCsr = *endmod;
019200110916
019300110916         // -?Verifica del sistema AS/400 corrente?
019400110916         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
019500110916           exsr  sr_RoutEnd;
019600110916         endif;
019700110203
019800110203         // -?Creazione stringa SQL da eseguire?
019900110203         wSQL_C1 = 'WITH +
020000110203                    Tab_3CP as +
020100110203                    (select substr(TBEke1, 1, 3) as Filiale, +
020200110203                            substr(TBEke1, 4, 4) as Cliente, +
020300110203                            substr(TBEke2, 1, 2) as Serie, +
020400110203                            substr(TBEke2, 3, 7) as SgnCl_i, +
020500110203                            substr(TBEuni, 1, 7) as SgnCl_f +
020600110203                       from TNTBE00F +
020700110315                      where TBEcod = ''3CP'' +
020800110315                        and TBEatb = '' ''), +
020900110203                    Tab_3EW as +
021000110203                    (select substr(TBEke1,  2, 3) as Filiale, +
021100110203                            substr(TBEke1,  5, 4) as Cliente, +
021200110203                            substr(TBEuni,  7, 2) as Serie, +
021300110203                            substr(TBEuni,  9, 7) as SgnCl_i, +
021400110203                            substr(TBEuni, 16, 7) as SgnCl_f +
021500110203                       from TNTBE00F +
021600110315                      where TBEcod = ''3EW'' +
021700110315                        and TBEatb = '' '') +
021800110203                    select * +
021900110203                      from Tab_3CP +
022000110203                    union +
022100110203                    select * +
022200110203                      from Tab_3EW +
022300110203                     order by Filiale, Serie, SgnCl_i, SgnCl_f, +
022400110203                              Cliente';
022500110203
022600110203         // -?Apertura cursore?
022700110203         exsr  sr_OpenCursor;
022800110916
022900110916         // -?Apertura file TITAS33C?
023000110916         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
023100110916           wLibFileSede = 'GAITRAGRU/TITAS33C';
023200110916         else ;
023300110916           wLibFileSede = 'GAITRAGRPS/TITAS33C';
023400110916         endif;
023500110916         open  TITAS33C;
023600110203
023700110203       ENDSR;
023800110203
023900110203       //--------------------------------------------------------------
024000110203       //?Esecuzione del comando (già impostato).                      ?
024100110203       //--------------------------------------------------------------
024200110203       BEGSR  sr_ExecCmd;
024300110203
024400110203         clear Qcap0100;
024500110203         Qcabcsdh = *off;
024600110203         Qcapa    = *off;
024700110203         Qcacmdss = *off;
024800110203         Qcaerved = *allX'00';
024900110203
025000110203         clear Qusec;
025100110203         Qusbprv  = %size(Qusec);
025200110203
025300110203         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
025400110203                           %size(Qcap0100) : 'CPOP0100' : *omit :
025500110203                           0 : 0 : Qusec);
025600110203
025700110203         if  Qusei <> *blank;
025800110203           exsr  sr_PrintErr;
025900110203         endif;
026000110203
026100110203       ENDSR;
026200110203
026300110203       //--------------------------------------------------------------
026400110203       //?Stampa segnalazione dell'errore rilevato.                    ?
026500110203       //--------------------------------------------------------------
026600110203       BEGSR  sr_PrintErr;
026700110203
026800110203         // -?Stampa del Dump?
026900110203         Dump(A);
027000110203
027100110203         // -?Stampa del Job-Log?
027200110203         Qcmd = 'DSPJOBLOG job(*) output(*print)';
027300110203         exsr  sr_ExecCmd;
027400110203
027500110203         // -?Chiusura del programma?
027600110203         exsr  sr_RoutEnd;
027700110203
027800110203       ENDSR;
027900110203
028000110203       //--------------------------------------------------------------
028100110203       //?Apertura cursore C1.                                         ?
028200110203       //--------------------------------------------------------------
028300110203       BEGSR  sr_OpenCursor;
028400110203
028500110203         // -?Dichiarazione del cursore?
028600110203         exec sql  prepare S1  from :wSQL_C1;
028700110203         exec sql  declare C1  cursor for S1;
028800110203
028900110203         // -?Apertura del cursore?
029000110203         exec sql   open C1;
029100110203
029200110203       ENDSR;
029300110203
029400110203       //--------------------------------------------------------------
029500110203       //?Lettura cursore C1.                                          ?
029600110203       //--------------------------------------------------------------
029700110203       BEGSR  sr_ReadCursor;
029800110203
029900110203         clear  wSQLrecC1;
030000110203
030100110203         exec SQL  fetch next  from C1  into :wSQLrecC1;
030200110203
030300110203         select;
030400110203           // -?Fine lettura?
030500110203           when  sqlCode = 100;
030600110203             exsr  sr_RoutEnd;
030700110203           // -?Errore?
030800110203           when  sqlCode < *zero;
030900110203             exsr  sr_PrintErr;
031000110203           // -?Elaborazione?
031100110203           other;
031200110203             exsr  sr_ElabRec;
031300110203             SavSQLrec = wSQLrecC1;
031400110203         endsl;
031500110203
031600110203       ENDSR;
031700110203
031800110203       //--------------------------------------------------------------
031900110203       //?Chiusura cursore C1.                                         ?
032000110203       //--------------------------------------------------------------
032100110203       BEGSR  sr_CloseCursor;
032200110203
032300110203         // -?Chiusura del cursore?
032400110203         exec sql   close C1;
032500110203
032600110203       ENDSR;
032700110203
032800110203       //--------------------------------------------------------------
032900110203       //?Elaborazione singolo record estratto.                        ?
033000110203       //--------------------------------------------------------------
033100110203       BEGSR  sr_ElabRec;
033200110203
033300110203         select;
033400110203
033500110203           // -?Controllo all'interno della stessa filiale?
033600110203           //                      ?e della stessa serie?
033700110203           when  wSQLrecC1.sql_Filiale <> SavSQLrec.sql_Filiale  or
033800110203                 wSQLrecC1.sql_Serie   <> SavSQLrec.sql_Serie;
033900110203             leavesr;
034000110203
034100110203           // -?Controllo sovrapposizione range?
034200110203           when  (wSQLrecC1.sql_SgnCl_I < SavSQLrec.sql_SgnCl_I   and
034300110203                  wSQLrecC1.sql_SgnCl_F < SavSQLrec.sql_SgnCl_I)  or
034400110203                 (wSQLrecC1.sql_SgnCl_I > SavSQLrec.sql_SgnCl_F   and
034500110203                  wSQLrecC1.sql_SgnCl_F > SavSQLrec.sql_SgnCl_F);
034600110203             leavesr;
034700110203
034800110203         endsl;
034900110916
035000110916         clear  xx;
035100110916         clear  yy;
035200110916         clear  $Note1;
035300110916         clear  $Note2;
035400110916         clear  ds3Ca;
035500110916         clear  ds3Cb;
035600110203
035700110916         // -?Reperimento dati 1° cliente da tab. "3C"?
035800110916         If  getTabella ( c_Tabel : '3C' : SavSQLrec.sql_Filiale +
035900110203                                           SavSQLrec.sql_Cliente :
036000110203                          *omit : *omit : *omit :
036100110203                          *omit : *omit :
036200110203                          *omit : *omit : *omit : *omit :
036300110203                          *omit : *omit : *omit : *omit :
036400110203                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
036500110203                        < *zero       OR
036600110203             ds_TNTBE.TBEatb <> *blank;
036700110203
036800110203           // -?Stampa segnalazione cliente non in "3C"?
036900110916           xx += 1;
037000110916           $Note1(xx) = 'Cliente ' + SavSQLrec.sql_Filiale +
037100110916                                     SavSQLrec.sql_Cliente +
037200110916                        ' non trovato in tab. "3C"';
037300110916           //exsr  sr_Stampa;
037400110203
037500110916         EndIf;
037600110203
037700110916         ds3Ca = ds_TNTBE.TBEuni;
037800110203
037900110916         // -?Reperimento "3C" x padre 1° cliente?
038000110916         If  %editc( ds3Ca.§3Ccks : 'X' ) <>
038100110916               SavSQLrec.sql_Filiale + SavSQLrec.sql_Cliente;
038200110203
038300110916           // -?2° Reperimento padre da tab. "3C"?
038400110916           if  getTabella ( c_Tabel : '3C' : %editc( ds3Ca.§3Ccks : 'X' ) :
038500110203                              *omit : *omit : *omit :
038600110203                              *omit : *omit :
038700110203                              *omit : *omit : *omit : *omit :
038800110203                              *omit : *omit : *omit : *omit :
038900110203                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
039000110203                            < *zero       OR
039100110916               ds_TNTBE.TBEatb <> *blank;
039200110203
039300110916             // -?Stampa segnalazione unificante non in "3C"?
039400110916             xx += 1;
039500110916             $Note1(xx) = 'Unificante ' + %editc( ds3Ca.§3Ccks : 'X' ) +
039600110916                          ' non trovato in tab."3C"';
039700110916             //exsr  sr_Stampa;
039800110203
039900110916           endif;
040000110916
040100110916         EndIf;
040200110916
040300110916         // -?Reperimento dati 2° cliente da tab. "3C"?
040400110916         If  getTabella ( c_Tabel : '3C' : wSQLrecC1.sql_Filiale +
040500110916                                           wSQLrecC1.sql_Cliente :
040600110916                          *omit : *omit : *omit :
040700110916                          *omit : *omit :
040800110916                          *omit : *omit : *omit : *omit :
040900110916                          *omit : *omit : *omit : *omit :
041000110916                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
041100110916                        < *zero       OR
041200110916             ds_TNTBE.TBEatb <> *blank;
041300110916
041400110916           // -?Stampa segnalazione cliente non in "3C"?
041500110916           yy += 1;
041600110916           $Note2(yy) = 'Cliente ' + wSQLrecC1.sql_Filiale +
041700110916                                     wSQLrecC1.sql_Cliente +
041800110916                        ' non trovato in tab. "3C"';
041900110916           //exsr  sr_Stampa;
042000110916
042100110916         EndIf;
042200110203
042300110916         ds3Cb = ds_TNTBE.TBEuni;
042400110203
042500110916         // -?Reperimento "3C" x padre 2° cliente?
042600110916         If  %editc( ds3Cb.§3Ccks : 'X' ) <>
042700110916               wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente;
042800110916
042900110916           if  getTabella ( c_Tabel : '3C' : %editc( ds3Cb.§3Ccks : 'X' ) :
043000110916                            *omit : *omit : *omit :
043100110916                            *omit : *omit :
043200110916                            *omit : *omit : *omit : *omit :
043300110916                            *omit : *omit : *omit : *omit :
043400110916                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
043500110916                          < *zero       OR
043600110916               ds_TNTBE.TBEatb <> *blank;
043700110916
043800110916             // -?Stampa segnalazione unificante non in "3C"?
043900110916             yy += 1;
044000110916             $Note2(yy) = 'Unificante ' + %editc( ds3Cb.§3Ccks : 'X' ) +
044100110916                          ' non trovato in tab."3C"';
044200110916             //exsr  sr_Stampa;
044300110916
044400110916           endif;
044500110916
044600110916         EndIf;
044700110916
044800110916         // -?Stampa segnalazione range sovrapposto &?
044900110916         //                      ?padre non corrispondente?
045000110916         If  (%editc( ds3Cb.§3Ccks : 'X' ) <>
045100110916                SavSQLrec.sql_Filiale + SavSQLrec.sql_Cliente  and
045200110916                      ds3Ca.§3Ccks         <> ds3Cb.§3Ccks)    OR
045300110916             xx > *zero   or   yy > *zero;
045400110916
045500110916           exsr  sr_Stampa;
045600110916
045700110916         EndIf;
045800110203
045900110203       ENDSR;
046000110203
046100110203       //--------------------------------------------------------------
046200110203       //?Stampa segnalazione segmento di pagina.                      ?
046300110203       //--------------------------------------------------------------
046400110203       BEGSR  sr_Stampa;
046500110203
046600110203         // -?Test OverFlow e stampa testata?
046700110203         exsr  sr_TestOF;
046800110204
046900110204         // -?Decodifica 1° cliente?
047000110204         clear  TIBS69ds;
047100110204         //I69sif = knsif;
047200110204         I69kcc = 151;
047300110204         I69kac = %int( SavSQLrec.sql_Filiale + SavSQLrec.sql_Cliente );
047400110204         tibs69r( tibs69ds :
047500110204                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
047600110916
047700110916         // -?Reperimento data ultima spedizione con questa serie?
047800110916         //  ?per il 1° cliente?
047900110916         wNRS = %int(SavSQLrec.sql_Serie);
048000110916         exsr  sr_GetLastSpedDta;
048100110203
048200110204         // -?Stampa 1ª riga di dettaglio?
048300110203         s_Filiale = SavSQLrec.sql_Filiale;
048400110203         s_Cliente = SavSQLrec.sql_Cliente;
048500110203         s_Serie   = SavSQLrec.sql_Serie;
048600110203         s_SgnCl_I = SavSQLrec.sql_SgnCl_I;
048700110203         s_SgnCl_F = SavSQLrec.sql_SgnCl_F;
048800110209         s_RagSoc  = ACOrag;
048900110209         s_Padre   = ds3Ca.§3Ccks;
049000110204         clear  s_BloAnn;
049100110204         select;
049200110204           when  ACOflg <> *blank;
049300110204             s_BloAnn = 'ANNULLATO ';
049400110204           when  ACOabl =  '8';
049500110204             s_BloAnn = 'BLOCCATO  ';
049600110204         endsl;
049700110916         if  s_DtUltSpe > *zero;
049800110916           wDate_Eur = %date( s_DtUltSpe : *iso );
049900110916           s_DtUltSpe = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
050000110916                          ( %subdt(wDate_Eur : *months) * 10000 ) +
050100110916                          ( %subdt(wDate_Eur : *years) ) );
050200110916         endif;
050300110916         clear  s_Note;
050400110916         clear  xy;
050500110916         if  xx > *zero;
050600110916           xy = 1;
050700110916           s_Note = $Note1(xy);
050800110916         endif;
050900110916         except  PRTdet;
051000110916         DoW  xy < xx;
051100110916           xy += 1;
051200110916           s_Note = $Note1(xy);
051300110916           except  PRTnot;
051400110916         EndDo;
051500110204
051600110204         // -?Decodifica 2° cliente?
051700110204         clear  TIBS69ds;
051800110204         //I69sif = knsif;
051900110204         I69kcc = 151;
052000110204         I69kac = %int( wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente );
052100110204         tibs69r( tibs69ds :
052200110204                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
052300110916
052400110916         // -?Reperimento data ultima spedizione con questa serie?
052500110916         //  ?per il 2° cliente?
052600110916         wNRS = %int(wSQLrecC1.sql_Serie);
052700110916         exsr  sr_GetLastSpedDta;
052800110204
052900110204         // -?Stampa 2ª riga di dettaglio?
053000110203         s_Filiale = wSQLrecC1.sql_Filiale;
053100110203         s_Cliente = wSQLrecC1.sql_Cliente;
053200110203         s_Serie   = wSQLrecC1.sql_Serie;
053300110203         s_SgnCl_I = wSQLrecC1.sql_SgnCl_I;
053400110203         s_SgnCl_F = wSQLrecC1.sql_SgnCl_F;
053500110209         s_RagSoc  = ACOrag;
053600110209         s_Padre   = ds3Cb.§3Ccks;
053700110204         clear  s_BloAnn;
053800110204         select;
053900110204           when  ACOflg <> *blank;
054000110204             s_BloAnn = 'ANNULLATO ';
054100110204           when  ACOabl =  '8';
054200110204             s_BloAnn = 'BLOCCATO  ';
054300110204         endsl;
054400110916         if  s_DtUltSpe > *zero;
054500110916           wDate_Eur = %date( s_DtUltSpe : *iso );
054600110916           s_DtUltSpe = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
054700110916                          ( %subdt(wDate_Eur : *months) * 10000 ) +
054800110916                          ( %subdt(wDate_Eur : *years) ) );
054900110916         endif;
055000110916         clear  s_Note;
055100110916         clear  xy;
055200110916         if  yy > *zero;
055300110916           xy = 1;
055400110916           s_Note = $Note2(xy);
055500110916         endif;
055600110916         except  PRTdet;
055700110916         DoW  xy < yy;
055800110916           xy += 1;
055900110916           s_Note = $Note2(xy);
056000110916           except  PRTnot;
056100110916         EndDo;
056200110203
056300110203         except  PRTspace;
056400110203
056500110203         // -?Incremento n° totale sovrapposizioni?
056600110203         w_TotSovrapp += 1;
056700110203
056800110203       ENDSR;
056900110916
057000110916       //--------------------------------------------------------------
057100110916       //?Reperimento data ultima spedizione per il cliente.           ?
057200110916       //--------------------------------------------------------------
057300110916       BEGSR  sr_GetLastSpedDta;
057400110916
057500110916         clear  s_DtUltSpe;
057600110916         k_TASccm = I69kac;
057700110916         k_TASaas = *year;
057800110916         k_TASmgs = *hival;
057900110916         setll   %kds(k03titas33)  TITAS33C;
058000110916         readPe  %kds(k03titas33 : 1)  TITAS33C;
058100110916
058200110916do  1    DoW  not %eof(TITAS33C);
058300110916
058400110916sel 2      select;
058500110916             // -?Spedizione senza segnacolli: da scartare?
058600110916w   2        when  TASncd <= *zeros  and  TASfns <> 'S';
058700110916             // -?Spedizione con serie diversa: da scartare?
058800110916w   2        when  TASnrs <> wNRS;
058900110916             // -?Ultima spedizione valida?
059000110916x   2        other;
059100110916               s_DtUltSpe = (TASaas * 10000) + TASmgs;
059200110916               leave;
059300110916e   2      endsl;
059400110916
059500110916           readPe  %kds(k03titas33 : 1)  TITAS33C;
059600110916
059700110916e   1    EndDo;
059800110916
059900110916       ENDSR;
060000110203
060100110203       //--------------------------------------------------------------
060200110203       //?Test OverFlow in stampa.                                     ?
060300110203       //--------------------------------------------------------------
060400110203       BEGSR  sr_TestOF;
060500110203
060600110203         // -?Apertura file di stampa?
060700110203         if  not %open (QSYSPRT);
060800110203           Qcmd = c_OvrPrtF;
060900110203           exsr  sr_ExecCmd;
061000110203           open  QSYSPRT;
061100110203           *inOF = *on;
061200110203         endif;
061300110203
061400110203         // -?Test OverFlow e stampa testata?
061500110203         if  *inOF;
061600110203           w_Time = %char( %time() );
061700110203           except PRTtxt;
061800110203           *inOF  = *off;
061900110203         endif;
062000110203
062100110203       ENDSR;
062200110203
062300110203       //--------------------------------------------------------------
062400110203       //?Operazioni finali.                                           ?
062500110203       //--------------------------------------------------------------
062600110203       BEGSR  sr_RoutEnd;
062700110203
062800110203         // -?Chiusura cursore?
062900110203         exsr  sr_CloseCursor;
063000110203
063100110203         // -?Stampa "fine-lista"?
063200110203         exsr  sr_TestOF;
063300110203         except  PRTend;
063400110203
063500110203         // -?Chiusura printer-file?
063600110203         close  QSYSPRT;
063700110203
063800110203         // -?Cancellazione override?
063900110203         Qcmd = c_DltOvr;
064000110203         exsr  sr_ExecCmd;
064100110204
064200110204         // -?Chiusura applicazioni?
064300110204         cloTabella();
064400110204         clear  TIBS69ds;
064500110204         I69tla = 'C';
064600110204         tibs69r( tibs69ds :
064700110204                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
064800110203
064900110203         // -?Uscita?
065000110203         return;
065100110203
065200110203       ENDSR;
065300110203
065400110203      /end-free
065500110203
065600110203       //--------------------------------------------------------------
065700110203       //?P R I N T E R - F I L E S                                    ?
065800110203       //--------------------------------------------------------------
065900110203
066000110203     oQSYSPRT   e            PRTtxt            2
066100110203     o                                              'ELENCO SOVRAPPOSI-
066200110203     o                                              ZIONI RANGE SEGNAC-
066300110203     o                                              OLLI IN TAB. "3CP"-
066400110203     o                                              /"3EW"'
066500110203     o                       SDSpgm           +   5
066600110203     o                       SDSusr           +   5
066700110916     o                       *date         y  +   5
066800110203     o                       w_Time           +   5
066900110203     o                                        +   5 'Pag.'
067000110203     o                       Page          z  +   0
067100110203     o          e            PRTtxt      0  0
067200110203     o                                              'ELENCO SOVRAPPOSI-
067300110203     o                                              ZIONI RANGE SEGNAC-
067400110203     o                                              OLLI IN TAB. "3CP"-
067500110203     o                                              /"3EW"'
067600110203     o          e            PRTtxt      2
067700110916     o                                              'Fil'
067800110916     o                                        +   1 'Cliente'
067900110916     o                                        +  22 '  Padre'
068000110916     o                                        +   2 'Sr'
068100110916     o                                        +   2 '  SgnCl_I'
068200110916     o                                        +   2 '  SgnCl_F'
068300110916     o                                        +   2 'Cl.Bloc. '
068400110916     o                                        +   1 'Dt.Ult.Sp.'
068500110203     o          e            PRTtxt      0  0
068600110916     o                                              'Fil'
068700110916     o                                        +   1 'Cliente'
068800110916     o                                        +  22 '  Padre'
068900110916     o                                        +   2 'Sr'
069000110916     o                                        +   2 '  SgnCl_I'
069100110916     o                                        +   2 '  SgnCl_F'
069200110916     o                                        +   2 'Cl.Bloc. '
069300110916     o                                        +   1 'Dt.Ult.Sp.'
069400110203     o          e            PRTtxt      1
069500110916     o                                              '==='
069600110916     o                                        +   1 '======='
069700110916     o                                        +  22 '======='
069800110916     o                                        +   2 '=='
069900110916     o                                        +   2 '========='
070000110916     o                                        +   2 '========='
070100110916     o                                        +   2 '======== '
070200110916     o                                        +   1 '=========='
070300110203     o          e            PRTtxt      0  1
070400110916     o                                              '==='
070500110916     o                                        +   1 '======='
070600110916     o                                        +  22 '======='
070700110916     o                                        +   2 '=='
070800110916     o                                        +   2 '========='
070900110916     o                                        +   2 '========='
071000110916     o                                        +   2 '======== '
071100110916     o                                        +   1 '=========='
071200110203      *
071300110203     o          e            PRTdet      1
071400110916     o                       s_Filiale
071500110916     o                       s_Cliente        +   1
071600110916     o                       s_RagSoc         +   1
071700110916     o                       s_Padre          +   1
071800110916     o                       s_Serie       1  +   2
071900110916     o                       s_SgnCl_I     1  +   2
072000110916     o                       s_SgnCl_F     1  +   2
072100110916     o                       s_BloAnn         +   2
072200110916     o                       s_DtUltSpe       +   1 '  /  /    '
072300110204     o                       s_Note           +   2
072400110916      *
072500110916     o          e            PRTnot      1
072600110916     o                                              '   '
072700110916     o                                        +   1 '       '
072800110916     o                                        +  23 '       '
072900110916     o                                        +   1 '  '
073000110916     o                                        +   1 '         '
073100110916     o                                        +   1 '         '
073200110916     o                                        +   1 '        '
073300110916     o                                        +   1 '          '
073400110916     o                       s_Note           +   2
073500110203      *
073600110203     o          e            PRTspace    1
073700110203      *
073800110203     o          e            PRTend      2
073900110203     o                                        +  10 '***  Fine Lista  +
074000110203     o                                               ***'
074100110203     o                                        +   3 'Tot. sovrapposizioni = '
074200110203     o                       w_TotSovrapp  k  +   1
074300110203     o          e            PRTend      0  0
074400110203     o                                        +  10 '***  Fine Lista  +
074500110203     o                                               ***'
